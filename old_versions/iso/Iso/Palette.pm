#$Id: HexCanvas.pm 103 2010-04-26 13:06:53Z ikm $

use strict;

use Wx qw[:everything];
use Log::Log4perl qw(get_logger);
use List::MoreUtils;
use Storable qw(dclone);

my $log = get_logger;

package Iso::Palette;
use base qw(Wx::Control Class::Accessor::Fast);
use Wx qw[:everything];
use Math::Trig;
use Data::Dumper;
use Iso::BigPalette;

__PACKAGE__->mk_accessors(qw(frame brushes palette highlight_pen top left right saved_color big_palette next_change));

# bitmap flags to describe the nature of a mouse event
our (
    $ME_LEFT_BUTTON,                    # 0001
    $ME_RIGHT_BUTTON,                   # 0002
    $ME_MIDDLE_BUTTON,                  # 0004
    $ME_BUTTON_DOWN,                    # 0008
    $ME_BUTTON_UP,                      # 0010
    $ME_WHEEL_FORWARD,                  # 0020
    $ME_WHEEL_BACK,                     # 0040
    $ME_SHIFT_IS_DOWN,                  # 0080
    $ME_CTRL_IS_DOWN,                   # 0100
    $ME_ALT_IS_DOWN,                    # 0200
    $ME_MANUAL_SELECT_BUILD_HEIGHT,     # 0400
    $ME_MANUAL_IDLE,                    # 0800
    $ME_DOUBLE_CLICK,                   # 1000
) = map { 2 ** $_ } (0 .. 15);

my $map;

my $tool_width = 24;
my $tool_height = 24;

################################################################################
# Constructor.
sub new { #{{{1

    my $nbr_colors = 32;

    my( $class, $parent, $id, $pos, $size) = @_;

    $log->info("palette parent $parent");

    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = [ $tool_width, $tool_height * $nbr_colors - $nbr_colors + 1 ]      unless defined $size;

    my $self = $class->SUPER::new( $parent, $id, $pos, $size);

    $self->big_palette(Iso::BigPalette->new( $self ));

    $self->brushes($self->big_palette->brushes);

    # the palette is the list of brush indexes currently displayed in the control
    $self->palette([ 0 .. 31 ]);

    $self->highlight_pen(Wx::Pen->new(wxRED, 3, wxSOLID));

    $self->top( 1 );
    $self->left( 2 );
    $self->right( 3 );

    # non-ctrl accelerators not working in wxPanel?
    main::assign_event_handler($self, 'EVT_CHAR',
        sub {
            my ($listbox, $event) = @_;
#            $log->debug("keycode " . $event->GetKeyCode);

            if ($event->GetKeyCode == ord('q')) {
                wxTheApp->GetTopWindow->Destroy;
            }
        });

    Wx::Event::EVT_PAINT($self, \&repaint_palette);

    Wx::Event::EVT_MOUSE_EVENTS( $self,
        sub { 
            my ($self, $event) = @_;

            my $event_flags = 0;
            $event_flags |= $ME_LEFT_BUTTON if $event->Button(wxMOUSE_BTN_LEFT);
            $event_flags |= $ME_RIGHT_BUTTON if $event->Button(wxMOUSE_BTN_RIGHT);
            $event_flags |= $ME_MIDDLE_BUTTON if $event->Button(wxMOUSE_BTN_MIDDLE);
            $event_flags |= $ME_BUTTON_DOWN if $event->ButtonDown;
            $event_flags |= $ME_BUTTON_UP if $event->ButtonUp;
            $event_flags |= $ME_WHEEL_FORWARD if $event->GetWheelRotation > 0;
            $event_flags |= $ME_WHEEL_BACK if $event->GetWheelRotation < 0;
            $event_flags |= $ME_SHIFT_IS_DOWN if $event->ShiftDown;
            $event_flags |= $ME_CTRL_IS_DOWN if $event->ControlDown;
            $event_flags |= $ME_ALT_IS_DOWN if $event->AltDown;
            $event_flags |= $ME_DOUBLE_CLICK if $event->ButtonDClick;

            $self->mouse_event_handler($event_flags, $event->GetX, $event->GetY);

            # have to skip to get EVT_LEAVE_WINDOW as well
            $event->Skip;
        }
    );

    $self->SetBackgroundStyle(wxBG_STYLE_CUSTOM);
    $self->frame($parent->GetParent);
    $self->saved_color(undef);

    return $self;
}

#*******************************************************************************
sub mouse_event_handler { #{{{1
    my ($self, $event_flags, $device_x, $device_y) = @_;

    my  $refresh;

    # y coords seem off by 2?
    $device_y -= 2;
    $device_y = 0 if $device_y < 0;

    my $index = int($device_y / ($tool_height - 1));

    if ($event_flags & $ME_BUTTON_DOWN && $event_flags & $ME_LEFT_BUTTON) {

        if ($event_flags & $ME_SHIFT_IS_DOWN) {

            # replace a color with another

            # do we have the first color already, ie the one to be switched?
            if (defined $self->saved_color) {

                # this is the second stage of the color switch
                my $first_color = $self->saved_color;
                $self->saved_color(undef);

                # same color selected, just quit
                if ($index == $first_color) {
                    return;
                }

                # restrict to current view?
                my $answer = $self->frame->display_dialog(
                    message => 'Restrict color change to current view?',
                    title => 'Question',
                    style => wxYES_NO,
                );

                $self->frame->canvas->replace_color($first_color, $index, $answer == wxID_YES);
            }
            else {

                $self->saved_color($index);
            }

        }
#        elsif ($event_flags & $ME_CTRL_IS_DOWN) {
#
#            # Change a color in the palette
#
#            my $data = Wx::ColourData->new;
#            $data->SetColour(Wx::Colour->new('#' . $self->colors->[$index]));
#            my $dialog = Wx::ColourDialog->new( $self->frame, $data );
#
#            if ($dialog->ShowModal == wxID_OK ) {
#                my $data = $dialog->GetColourData;
#                my $colour = $data->GetColour;
#                $self->colors->[$index] = sprintf("%02x%02x%02x", $colour->Red, $colour->Green, $colour->Blue );
#                $self->make_brushes;
#                $refresh = 1;
#                $self->frame->canvas->Refresh;
#            }
#
#            $dialog->Destroy;
#        }
        else {
            my $old_left = $self->left;
            $self->left($index);
            $refresh = $self->left != $old_left;
        }
    }

    if ($event_flags & $ME_BUTTON_DOWN && $event_flags & $ME_MIDDLE_BUTTON) {
        if ($event_flags & $ME_SHIFT_IS_DOWN) {
            my ($left, $top) = $self->GetPositionXY;
            $self->big_palette->Move( $left + 30, $top + 24 );
            $self->big_palette->Popup;
            $self->next_change($index);
        }
        else {
            my $old_top = $self->top;
            $self->top($index);
            $refresh = $self->top != $old_top;
        }
    }

    if ($event_flags & $ME_BUTTON_DOWN && $event_flags & $ME_RIGHT_BUTTON) {
        my $old_right = $self->right;
        $self->right($index);
        $refresh = $self->right != $old_right;
    }

    $self->Refresh if $refresh;

    return;
}

#*******************************************************************************
sub repaint_palette { #{{{1
    my ($self) = @_;

    my $dc = Wx::AutoBufferedPaintDC->new( $self );
    my $frame = $self->frame;

    $dc->Clear;

#    unless ($self->brushes) {
#
#        my @brushes;
#
#        for my $color (@{ $self->colors }) {
#            push @brushes, Wx::Brush->new("#$color", wxSOLID);
#        }
#
#        $self->brushes(\@brushes);
#
#        $self->highlight_pen(Wx::Pen->new(wxRED, 3, wxSOLID));
#
#    }

    my $box_width = $tool_width;
    my $box_height = $tool_height;
    my $box_top = 0;

    my $index = 0;
    for my $brush_index (@{ $self->palette }) {
        my $brush = $self->brushes->[$brush_index];
        $dc->SetBrush($brush);
        $dc->DrawRectangle(0,$box_top,$box_width, $box_height);
        $box_top += $box_height - 1;
        $index++;
    }

    # paint top, left, right markers; these aren't adjusted to tool size
    my $top_points = [
        Wx::Point->new( 0, 5 ),
        Wx::Point->new( 9, 0 ),
        Wx::Point->new( 18, 5 ),
        Wx::Point->new( 9, 10 ),
    ];

    my $left_points = [
        Wx::Point->new( 0, 5 ),
        Wx::Point->new( 9, 10 ),
        Wx::Point->new( 9, 20 ),
        Wx::Point->new( 0, 15 ),
    ];

    my $right_points = [
        Wx::Point->new( 9, 10 ),
        Wx::Point->new( 18, 5 ),
        Wx::Point->new( 18, 15 ),
        Wx::Point->new( 9, 20 ),
    ];

    $dc->SetPen(wxBLACK_PEN);
    $dc->SetBrush(wxRED_BRUSH);
    $dc->DrawPolygon($top_points, 3, $self->top * ($box_height - 1) + 1);

    $dc->DrawPolygon($left_points, 3, $self->left * ($box_height - 1) + 1);
    $dc->DrawPolygon($right_points, 3, $self->right * ($box_height - 1) + 1);

    return;
}

################################################################################

#sub get_brush { #{{{1
#    my ($self, $index) = @_;
#
#    # default to final colour
#    if (! defined $index) {
#        $index = $#{ $self->brushes };
#    }
#
#    return $self->brushes->[ $index ];
#}

################################################################################

sub count { #{{{1
    my ($self) = @_;

    return scalar @{ $self->palette };
}

################################################################################

sub get_shape_brush { #{{{1
    my ($self, $shape) = @_;

    return $self->palette->[ $self->shape_position($shape) ];
}

################################################################################
sub shape_position {
    my ($self, $shape) = @_;

    my $shape_position = {
        L => $self->left,
        T => $self->top,
        R => $self->right,
    };

    return $shape_position->{$shape};
}

################################################################################

sub big_palette_selection { #{{{1
    my ($self, $selection) = @_;

    $log->info("big_palette_selection $selection");

    # the selected color is moved to $next_change of the palette and $next_change
    # is incremented (if possible)
    splice @{ $self->palette }, $self->next_change, 1, $selection;
    $self->next_change($self->next_change + 1) if $self->next_change < 31;
#    delete $self->palette->[32];
#
#    # current selections stay the same if possible
#    $self->left($self->left + 1) if $self->left < 31;
#    $self->top($self->top + 1) if $self->top < 31;
#    $self->right($self->right + 1) if $self->right < 31;

    $self->Refresh;

    return;
}

################################################################################

1;
