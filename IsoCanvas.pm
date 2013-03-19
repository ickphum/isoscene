#$Id: IsoCanvas.pm 169 2013-03-02 05:27:05Z ikm $

use strict;

use Wx qw[:everything];
use Log::Log4perl qw(get_logger);
use List::MoreUtils;
use Storable qw(dclone);

my $log = get_logger;

package IsoCanvas;
use base qw(Wx::Panel Class::Accessor::Fast);
use Wx qw[:everything];
use Math::Trig;
use Data::Dumper;
use POSIX qw(floor);
use List::Util qw(min max);
use Time::Piece;
use Time::HiRes qw(gettimeofday tv_interval);
use Math::Geometry::Planar;

# accessors & globals {{{1

__PACKAGE__->mk_accessors(qw(frame dragging last_device_x last_device_y
    x_grid_size y_grid_size control_gradient
    device_width device_height
    min_x_grid min_y_grid max_x_grid max_y_grid
    paint_shape
    background_brush bg_line_pen tile_line_pen 
    floodfill
    current_location current_grid_key
    area_start area_tiles deleted_tile select_toggle selected_tile

    cursor_multiplier_x cursor_multiplier_y action_cursor

    _scene palette palette_index brush_index

    paste_list

    tile_cache
    ));

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

# y offsets are expressed in units of half the y grid size, eg 0 = 0, 1 = ygs/2, 2 = ygs, 3 = ygs * 1.5.
# (no other values are valid except 0,1,2,3).
# Build an array to hold these values so we don't calculate them for each tile.
my @Y_offsets;

our ($SH_LEFT, $SH_TOP, $SH_RIGHT, $SH_TRIANGLE_LEFT, $SH_TRIANGLE_RIGHT) = qw(L T R TL TR);

# map shape to the triangle facing that anchors that shape
my %Shape_facing = (
    $SH_LEFT => $SH_RIGHT,
    $SH_TOP => $SH_LEFT,
    $SH_RIGHT => $SH_RIGHT,
    $SH_TRIANGLE_LEFT => $SH_LEFT,
    $SH_TRIANGLE_RIGHT => $SH_RIGHT,
);

# this data structure encodes how we check for overlapping shapes
# in order to either change to a triangle or abandon the paint.
# For each shape to be painted, we have two alternatives, ie the 
# two triangles that make up the shape (expressed as offset and shape).
# For each triangle, we have three possible clashing tiles 
# (expressed as offset and shape) that would force that triangle
# to be used. We have to check both triangles each time; if they are
# both required, the tile cannot be painted.
my %Shape_clashes = (
    L => [
        [ 1,0,1, $SH_TRIANGLE_LEFT, 1, 
            [
                [  0, 0, 0, $SH_RIGHT, ],
                [  1,-1, 0, $SH_TOP, ],
                [  0, 0, 0, $SH_TRIANGLE_RIGHT, ],
            ],
        ],
        [ 0,0,0, $SH_TRIANGLE_RIGHT, 2,
            [
                [  1, 0, 1, $SH_RIGHT, ],
                [  1, 0, 1, $SH_TOP, ],
                [  1, 0, 1, $SH_TRIANGLE_LEFT, ],
            ],
        ],
    ],
    T => [
        [ -1,1,0, $SH_TRIANGLE_RIGHT, 1, 
            [
                [  0, 0, 0, $SH_RIGHT, ],
                [ -1, 0,-1, $SH_LEFT, ],
                [  0, 0, 0, $SH_TRIANGLE_LEFT, ],
            ],
        ],
        [ 0,0,0, $SH_TRIANGLE_LEFT, 2,
            [
                [ -1, 1, 0, $SH_RIGHT, ],
                [ -1, 1, 0, $SH_LEFT, ],
                [ -1, 1, 0, $SH_TRIANGLE_RIGHT, ],
            ],
        ],
    ],
    R => [
        [ 0,0,0, $SH_TRIANGLE_RIGHT, 1, 
            [
                [ -1, 0,-1, $SH_LEFT, ],
                [  0, 0, 0, $SH_TOP, ],
                [  0, 0, 0, $SH_TRIANGLE_LEFT, ],
            ],
        ],
        [ 0,0,0, $SH_TRIANGLE_LEFT, 2,
            [
                [  0, 0, 0, $SH_LEFT, ],
                [  1,-1, 0, $SH_TOP, ],
                [  0, 0, 0, $SH_TRIANGLE_RIGHT, ],
            ],
        ],
    ],
    TL => [

        # shift_flag set to 3 so any clash stops the paint; can't
        # paint half a triangle.
        [ 0,0,0, $SH_TRIANGLE_RIGHT, 3, 
            [
                [ -1, 0,-1, $SH_LEFT, ],
                [  0, 0, 0, $SH_TOP, ],
                [  0, 0, 0, $SH_RIGHT, ],
            ],
        ],
    ],
    TR => [
        [ 0,0,0, $SH_TRIANGLE_LEFT, 3,
            [
                [  0, 0, 0, $SH_LEFT, ],
                [  0, 0, 0, $SH_RIGHT, ],
                [  1,-1, 0, $SH_TOP, ],
                [  0, 0, 0, $SH_TRIANGLE_RIGHT, ],
            ],
        ],
    ],
);

# extent of each shape relative to its anchor, in multiples
# of the relevant grid size.
my $Shape_extent = {
    L => {
        min_y => 0,
        max_x => 1,
        max_y => 1.5,
    },
    T => {
        min_y => 0.5,
        max_x => 2,
        max_y => 0.5,
    },
    R => {
        min_y => 0.5,
        max_x => 1,
        max_y => 1,
    },
    TL => {
        min_y => 0.5,
        max_x => 1,
        max_y => 0.5,
    },
    TR => {
        min_y => 0,
        max_x => 1,
        max_y => 1,
    },
};

# This structure defines how to draw each type of tile; it's loaded
# once the grid size is defined.
my $Tile_shape;

################################################################################
# Constructor.
sub new { #{{{1

    my( $class, $parent, $scene, $id, $pos, $size) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;

    my $self = $class->SUPER::new( $parent, $id, $pos, $size, 0);

    $self->frame($parent);

    $self->scene($scene);
    my $app = wxTheApp;

    # create a table relating action to a cursor; don't worry that the 3
    # erase actions and the 3 select actions could share cursors.
    $self->action_cursor({});
    for my $action (@IsoFrame::ACTIONS) {

        # skip the transitory colour change actions
        next if $action =~ /\A(?:$IsoFrame::AC_SHADE|$IsoFrame::AC_LIGHTEN|$IsoFrame::AC_DARKEN)\z/;

        my $bitmap = $action =~ /erase/
            ? 'erase'
            : $action =~ /select/
                ? 'select'
                : $action;
        $log->logdie("no bitmap for $bitmap") unless $app->bitmap->{$bitmap};
        my $cursor_image = $app->bitmap->{$bitmap}->ConvertToImage;
        $cursor_image->SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_X, 0);
        $cursor_image->SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_Y, $cursor_image->GetHeight - 1);
        $self->action_cursor->{$action} = Wx::Cursor->new($cursor_image);
    }

    # add move as a pseudo-action as it has a separate cursor
    $self->action_cursor->{move} = Wx::Cursor->new($app->bitmap->{move_on}->ConvertToImage);

    $self->set_cursor;
    $self->cursor_multiplier_x(1);
    $self->cursor_multiplier_y(1);

    Wx::Event::EVT_SIZE($self, \&find_logical_size);
    Wx::Event::EVT_PAINT($self, \&repaint_canvas);

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
#            $log->debug(sprintf("event_flags = %04X", $event_flags));

            $self->mouse_event_handler($event_flags, $event->GetX, $event->GetY);

            # have to skip to get EVT_LEAVE_WINDOW as well
            $event->Skip;
        }
    );

    $self->calculate_grid_dims(120);

    $self->SetBackgroundStyle(wxBG_STYLE_CUSTOM);

    return $self;
}

################################################################################
sub scene { #{{{1
    my ($self, $scene) = @_;

    if ($scene) {
        $self->_scene($scene);

        $self->palette([]);
        $self->palette_index({});
        for my $i ( 0 .. $#{ $scene->palette } ) {
            my $color = $scene->palette->[$i];
            push @{ $self->palette }, Wx::Brush->new( Wx::Colour->new($color), wxBRUSHSTYLE_SOLID);
            $self->palette_index->{$color} = $i;
        }

        $self->background_brush(Wx::Brush->new(Wx::Colour->new($scene->background_rgb), wxBRUSHSTYLE_SOLID));
        $self->bg_line_pen(Wx::Pen->new(Wx::Colour->new($scene->bg_line_rgb), 1, wxPENSTYLE_SOLID));
        $self->tile_line_pen(Wx::Pen->new(Wx::Colour->new($scene->tile_line_rgb), 1, wxPENSTYLE_SOLID));

        $self->frame->cube_brush({
            L => Wx::Brush->new(Wx::Colour->new($scene->left_rgb), wxBRUSHSTYLE_SOLID),
            T => Wx::Brush->new(Wx::Colour->new($scene->top_rgb), wxBRUSHSTYLE_SOLID),
            R => Wx::Brush->new(Wx::Colour->new($scene->right_rgb), wxBRUSHSTYLE_SOLID),
        });

        $self->area_tiles([]);
        $self->deleted_tile({});
        $self->selected_tile({});
        $self->tile_cache(0);
    }

    return $self->_scene;
}

################################################################################
sub set_cursor { #{{{1
    my ($self) = @_;

    my $frame = $self->frame;
    $self->SetCursor($frame->mode eq $IsoFrame::MO_MOVE
        ? $self->action_cursor->{move}
        : $self->action_cursor->{$frame->action});

    return;
}

################################################################################
sub find_triangle_coords { #{{{1
    my ($self, $logical_x, $logical_y) = @_;

    my $min_x = floor($logical_x / $self->x_grid_size);
    my $min_y = floor($logical_y / $self->y_grid_size);

    my $min_x_is_even = $min_x % 2 == 0;

    my @key = (undef, ($min_y + 0.5) * $self->y_grid_size);
    $key[0] = $min_x_is_even 
        ? ($min_x + 1 ) * $self->x_grid_size
        : $min_x * $self->x_grid_size;

    my $point_gradient = ($key[1] - $logical_y)/($key[0] - $logical_x);

    my @anchor = ($min_x * $self->x_grid_size, undef);
    my $facing;

    if (abs($point_gradient) < $self->control_gradient) {
        if ($min_x_is_even) {
            $anchor[1] = $min_y;
            $facing = $SH_RIGHT;
        }
        else {
            $anchor[1] = $min_y + 0.5;
            $facing = $SH_LEFT;
        }
    }
    else {
        if ($point_gradient > 0) {
            if ($min_x_is_even) {
                $anchor[1] = $min_y;
                $facing = $SH_LEFT;
            }
            else {
                $anchor[1] = $min_y + 0.5;
                $facing = $SH_RIGHT;
            }
        }
        else {
            if ($min_x_is_even) {
                $anchor[1] = $min_y + 1;
                $facing = $SH_LEFT;
            }
            else {
                $anchor[1] = $min_y - 0.5;
                $facing = $SH_RIGHT;
            }
        }
    }

    my $top = $min_x;
    my $left = $anchor[1] - $top / 2;
    my $right = $left + $top;

    $anchor[1] *= $self->y_grid_size;

#    $log->info("pg $point_gradient, cg $control_gradient, min_x $min_x, min_y $min_y, key @key, anchor @anchor, facing $facing");
    $log->debug("find_triangle_coords $left, $top, $right, $facing");

#    @stash = ($min_x, $min_y, @key, @anchor, $facing);

    return ($left, $top, $right, $facing);
}

################################################################################
sub mouse_event_handler { #{{{1
    my ($self, $event_flags, $device_x, $device_y) = @_;

    # $log->info(sprintf "mouse event: %08x", $event_flags);
    # my $start_mouse = [ gettimeofday ];

    my $refresh;
    my $frame = $self->frame;
    my $mode = $frame->mode;
    my $side = $frame->current_side;
    my $action = $frame->action;

    my ($logical_x, $logical_y) = ( int(($device_x - $self->scene->origin_x) / $self->scene->scale),
        int(($device_y - $self->scene->origin_y) / $self->scene->scale));

    my ($left, $top, $right, $facing) = $self->find_triangle_coords($logical_x, $logical_y);
    $self->current_location([ $left, $top, $right, $facing ]);
    my $grid_key = "${facing}_${left}_${top}_${right}";
    if ($grid_key ne ($self->current_grid_key || '')) {
        $refresh = 1;
        $self->current_grid_key($grid_key);
    }
    elsif ($event_flags == 0 && $mode ne $IsoFrame::MO_MOVE ) {
        
        # we haven't changed triangle, we're not moving the offset
        # and we haven't pressed a button; bail
        return;
    }

    if ($event_flags & $ME_LEFT_BUTTON) {

        # in move mode, left button down/up toggles dragging flag
        if ($mode eq $IsoFrame::MO_MOVE) {
            $self->dragging($event_flags & $ME_BUTTON_DOWN);
            $log->debug("dragging now? " . $self->dragging);
            return;
        }

        # in paint, sample, select or erase action, left button down/up toggles the action
        if ($event_flags & $ME_BUTTON_DOWN) {
            $refresh = 1;
            $self->paint_shape($side);
            $self->area_start( $mode eq $IsoFrame::MO_AREA
                ? [ $left, $top, $right, $facing ]
                : undef);

            if ($action eq $IsoFrame::AC_PAINT) {

                # set the current brush index, adding the color to the palette if we need to
                my $palette_index = $self->get_palette_index($frame->cube_brush->{$side}->GetColour->GetAsString(wxC2S_HTML_SYNTAX));
                $self->brush_index($palette_index);
            }
            elsif ($action =~ /select/) {
                $log->debug("lmb down select");

                # the state of the initially selected tile dictates whether this down-[move]-up
                # sequence selects or deselects.
                if (my $tile = $self->find_tile($grid_key)) {
                    $self->select_toggle(! $tile->{selected});
                    $log->debug("select start from tile: flag " . $self->select_toggle);
                }
                else {
                    $self->select_toggle(1);
                    $log->debug("select start from no tile: flag " . $self->select_toggle);
                }
            }
            elsif ($action eq $IsoFrame::AC_PASTE) {

                # look for the left-down on paste so we can filter out the end of the
                # activation double-click from the popup menu
                $self->paint_shape(2);
            }
        }
        elsif ($event_flags & $ME_BUTTON_UP) {

            if ($action eq $IsoFrame::AC_PASTE && $self->paint_shape == 1) {
                
                # ignore this click, it's the end of the activation double-click from
                # the paste popup menu
                return;
            }

            $self->paint_shape(0);

            if ($action eq $IsoFrame::AC_SAMPLE) {
                $frame->action($IsoFrame::AC_PAINT);
                $self->set_cursor;
                $frame->update_scene_color($side);
                $frame->Refresh;
            }

            if ($action eq $IsoFrame::AC_IMPORT) {
                $self->import_bitmap($side);
                $frame->action($IsoFrame::AC_PAINT);
                $self->set_cursor;
                $frame->Refresh;
            }

            if ($action eq $IsoFrame::AC_PASTE) {
                $log->debug("do paste");
                $frame->action($IsoFrame::AC_PAINT);
                $self->set_cursor;
                $self->paste_list(undef);
                $self->add_undo_action($IsoFrame::AC_PAINT, [ keys %{ $self->selected_tile } ]);
                push @{ $self->tile_cache }, keys %{ $self->selected_tile };
                $self->selected_tile({});
                $frame->Refresh;
            }

            if ($mode eq $IsoFrame::MO_AREA) {
                if ($action eq $IsoFrame::AC_PAINT) {
                    $self->add_undo_action($IsoFrame::AC_PAINT, [ keys %{ $self->selected_tile } ]);
                    push @{ $self->tile_cache }, keys %{ $self->selected_tile };
                    $self->selected_tile({});
                }
                elsif ($action =~ /erase/) {

                    my @deleted_tiles = keys %{ $self->deleted_tile };
                    $log->debug("deleted_tiles @deleted_tiles");
                    $self->add_undo_action($IsoFrame::AC_ERASE, \@deleted_tiles);
                    for my $key (@deleted_tiles) {
                        delete $self->scene->grid->{$key};
                        $refresh = 1;
                    }
                    $self->deleted_tile({});
                }
                elsif ($action =~ /select/) {

                    my @selected_tiles = keys %{ $self->selected_tile };
                    $log->debug("selected_tiles @selected_tiles");
                    for my $key (@selected_tiles) {
                        $self->scene->grid->{$key}->{selected} = $self->select_toggle;
                        $refresh = 1;
                    }
                    $self->selected_tile({});
                }
                $self->area_start( undef );
            }
        }
    }

    # note that paint_shape will be true during paste but won't be a real shape; it's just
    # true to pass this test.
    if (my $paint_shape = $self->paint_shape) {

        if ($self->area_start) {

            # we have to define areas to a triangle of the same facing that we started with,
            # since only similar triangles have relatable coords. Note; this is unrelated
            # to the anchor triangle for a particular shape. You can start in either half
            # of the tile (since find_tile copes with that initial mapping) but you then have to
            # keep painting to that half. The mapping from one facing to the other (and back, we need both)
            # does depend on the current shape.
            my ($start_left, $start_top, $start_right, $start_facing) = @{ $self->area_start };
            if ($facing ne $start_facing) {

                # both triangles in SH_RIGHT triangles have the same address, just switching 
                # the facing is all we need do.
                if ($paint_shape ne $SH_RIGHT) {

                    # this hash is keyed by shape, then facing, and the values are lists of adjustments
                    # for the coord list. The facing key here is the one of the current tile, and
                    # the adjustments will make the coords point to the same tile once the facing is changed.
                    my $refacing_adjustments = {
                        $SH_LEFT => {
                            L => [ -1, 0, -1 ],
                            R => [ 1, 0, 1 ],
                        },
                        $SH_TOP => {
                            L => [ -1, 1, 0 ],
                            R => [ 1, -1, 0 ],
                        },
                    };

                    my $adjustments = $refacing_adjustments->{ $paint_shape }->{ $facing };
                    $left += $adjustments->[0];
                    $top += $adjustments->[1];
                    $right += $adjustments->[2];
                }

                $facing = $start_facing;
            }
        }

        if ($action =~ /erase/) {
            if ($self->area_start) {

                # clear tiles from previous area clear location
                $self->deleted_tile({});

                # note that erase-all doesn't work easily in area mode; the tiles erased
                # will depend on the facing of the original tile clicked.
                $refresh = $self->mark_area($self->deleted_tile, $left, $top, $right, $facing, $paint_shape);
            }
            else {
                if (my $tile = $self->find_tile($grid_key)) {
                    $log->debug("found tile " . Dumper($tile));

                    if ($action eq $IsoFrame::AC_ERASE_ALL
                        || $tile->{shape} =~ /T[LR]/
                        || ($action eq $IsoFrame::AC_ERASE && $tile->{shape} eq $paint_shape)
                        || ($action eq $IsoFrame::AC_ERASE_OTHERS && $tile->{shape} ne $paint_shape))
                    {

                        # the tile we delete will not have the same grid_key that we 
                        # passed in if we clicked in the "wrong" half.
                        $grid_key = join('_', @{ $tile }{qw( facing left top right )});

                        $self->add_undo_action($IsoFrame::AC_ERASE, $self->scene->grid->{$grid_key});
                        delete $self->scene->grid->{$grid_key};
                        $refresh = 1;
                    }
                }
            }
        }
        elsif ($action eq $IsoFrame::AC_PAINT) {

            if ($self->area_start) {

                # clear tiles from previous area paint
                for my $key (keys %{ $self->selected_tile } ) {
                    delete $self->scene->grid->{$key};
                }
                $self->selected_tile({});

                $refresh = $self->mark_area($self->selected_tile, $left, $top, $right, $facing, $paint_shape);
            }
            else {

                # let paint_tile do all the transforms and clash checking
                if (my $grid_key = $self->paint_tile($left, $top, $right, $facing, $paint_shape)) {
                    $refresh = 1;
                    $self->add_undo_action($IsoFrame::AC_PAINT, $self->scene->grid->{$grid_key});
                }
            }
        }
        elsif ($action eq $IsoFrame::AC_PASTE) {

            # clear tiles from previous refresh
            for my $key (keys %{ $self->selected_tile } ) {
                delete $self->scene->grid->{$key};
            }
            $self->selected_tile({});

            for my $tile ( @{ $self->paste_list } ) {
                if (my $grid_key = $self->paint_tile($left + $tile->{left}, $top + $tile->{top}, $right + $tile->{right},
                    $tile->{facing}, $tile->{shape}, $tile->{brush_index}))
                {
                    $refresh = 1;
                    $self->selected_tile->{$grid_key} = 1;
                }
            }
        }
        elsif ($action =~ /select/) {
            if ($self->area_start) {

                # clear tiles from previous area
                $self->selected_tile({});

                $refresh = $self->mark_area($self->selected_tile, $left, $top, $right, $facing, $paint_shape);
            }
            elsif (my $tile = $self->find_tile($grid_key)) {
                $log->debug("tile: " . Dumper($tile));
                if ($action eq $IsoFrame::AC_SELECT_ALL
                    || $tile->{shape} =~ /T[LR]/
                    || ($action eq $IsoFrame::AC_SELECT && $tile->{shape} eq $paint_shape)
                    || ($action eq $IsoFrame::AC_SELECT_OTHERS && $tile->{shape} ne $paint_shape))
                {
                    $tile->{selected} = $self->select_toggle;
                    $refresh = 1;
                }
            }
        }
        elsif ($action eq $IsoFrame::AC_SAMPLE) {
            if (my $tile = $self->find_tile($grid_key)) {
                my $brush = $frame->cube_brush->{ $side };
                $brush->SetColour( $self->palette->[$tile->{brush_index}]->GetColour );
                $frame->Refresh;
            }
        }
    }

    # wheel zooms in/out
    if ($event_flags & ($ME_WHEEL_FORWARD | $ME_WHEEL_BACK)) {
        my $scale = $self->scene->scale;
        if ($event_flags & $ME_WHEEL_FORWARD) {

            # reset to int 1 when coming back up from fractional end of range
            $scale *= 2 if $scale < 4;
            $scale = 1 if ($scale > 0.9 && $scale < 1.1);
        }
        elsif ($event_flags & $ME_WHEEL_BACK) {
            $scale /= 2 if $scale > 0.05;
        }
        $self->scene->scale($scale);
        $self->scene->origin_x($device_x - $scale * $logical_x);
        $self->scene->origin_y($device_y - $scale * $logical_y);
        $self->tile_cache(0);
        $refresh = 1;
    }

    # motion while dragging moves origin
    if (defined $device_x && $self->dragging) {
        $self->move_origin($device_x, $device_y);
        $refresh = 1;
    }

#    $log->debug("refresh from mouse_event_handler") if $refresh;
    $self->Refresh if $refresh;

    $self->last_device_x($device_x);
    $self->last_device_y($device_y);

    # $log->info(sprintf "mouse: finished after %.6f", tv_interval($start_mouse));

    return;
}

################################################################################
sub get_palette_index { #{{{1
    my ($self, $colour_str) = @_;

    unless (defined $self->palette_index->{$colour_str}) {
        push @{ $self->palette }, Wx::Brush->new(Wx::Colour->new($colour_str), wxBRUSHSTYLE_SOLID);
        push @{ $self->scene->palette }, $colour_str;
        $self->palette_index->{$colour_str} = $#{ $self->palette };
        $log->debug("added color $colour_str to palette");
    }

    return $self->palette_index->{$colour_str};
}

################################################################################
sub find_tile { #{{{1
    my ($self, $grid_key) = @_;

    my $tile;

    # look for an instant hit
    unless ($tile = $self->scene->grid->{$grid_key}) {
        $log->debug("not at $grid_key");

        # have to pull apart the key to check other halves
        $log->logdie("bad grid key $grid_key") unless $grid_key =~ /\A([A-Z]+)_(-?\d+)_(-?\d+)_(-?\d+)\z/;
        my ($facing, $left, $top, $right) = ($1,$2,$3,$4);

        if ($facing eq $SH_LEFT) {
            
            # check for a right facing tile on the same point (anchoring a SH_RIGHT)
            $grid_key =~ s/L/R/;
            $log->debug("check $grid_key");
            $tile = $self->scene->grid->{$grid_key};
            if (! $tile || $tile->{shape} ne $SH_RIGHT) {
                $log->debug("not a R tile at $grid_key");

                # move up one and check for a right facing tile anchoring a SH_LEFT
                $left--;
                $right--;
                $tile = $self->scene->grid->{ "R_${left}_${top}_${right}" };
                $log->debug("tile at R_${left}_${top}_${right} ?" . Dumper($tile));
            }
        }
        else {

            # left up, top down and check for a left facing tile anchoring a SH_TOP
            $left++;
            $top--;
            $tile = $self->scene->grid->{ "L_${left}_${top}_${right}" };
            $log->debug("tile at L_${left}_${top}_${right} ?" . Dumper($tile));
        }
    }

    return $tile;
}

################################################################################
sub mark_area { #{{{1
    my ($self, $marked_hash, $left, $top, $right, $facing, $shape) = @_;

    my ($start_left, $start_top, $start_right, $start_facing) = @{ $self->area_start };

    $log->debug("mark_area $shape from $start_left, $start_top, $start_right, $start_facing to $left, $top, $right, $facing");

    my @start = ($start_left, $start_top, $start_right);
    my @current = ($left, $top, $right);

    my %shape_axes = (
        L => [ 0,1,2,1,1 ],
        T => [ 0,2,1,-1,1 ],
        R => [ 1,2,0,-1,1 ],
    );
    my @axes = @{ $shape_axes{$shape} };
    my @mins = (List::Util::min($start[$axes[0]], $current[$axes[0]]), List::Util::min($start[$axes[1]], $current[$axes[1]]));
    my @maxes = (List::Util::max($start[$axes[0]], $current[$axes[0]]), List::Util::max($start[$axes[1]], $current[$axes[1]]));

    my $axis_1_coeff = pop @axes;
    my $axis_0_coeff = pop @axes;

    my $action = $self->frame->action;

    my @point;
    for my $coord_0 ($mins[0] .. $maxes[0]) {
        for my $coord_1 ($mins[1] .. $maxes[1]) {

            $point[ $axes[0] ] = $coord_0;
            $point[ $axes[1] ] = $coord_1;
            $point[ $axes[2] ] = $axis_0_coeff * $coord_0 + $axis_1_coeff * $coord_1; 

            if ($action eq $IsoFrame::AC_PAINT) {
                if (my $key = $self->paint_tile(@point, $facing, $shape)) {
                    $marked_hash->{$key} = 1;
                }
            }
            else {

                my $grid_key = "${facing}_$point[0]_$point[1]_$point[2]";
                if (my $tile = $self->find_tile ( $grid_key )) {

                    # select according to action mode and shape
                    if (($action eq $IsoFrame::AC_ERASE_ALL || $action eq $IsoFrame::AC_SELECT_ALL)
                        || (($action eq $IsoFrame::AC_ERASE || $action eq $IsoFrame::AC_SELECT) && $tile->{shape} eq $shape)
                        || (($action eq $IsoFrame::AC_ERASE_OTHERS || $action eq $IsoFrame::AC_SELECT_OTHERS) && $tile->{shape} ne $shape))
                    {

                        # mark this tile for the next refresh; the final set of marked tiles, ie
                        # when the mouse button is released, will be acted on.

                        # we may have found a tile with a different grid key than which we searched for,
                        # due to facing; mark the key of the tile we found.
                        $grid_key = join('_', @{ $tile }{qw( facing left top right )});
                        $marked_hash->{$grid_key} = 1;
                    }
                }
            }
        }
    }

    return 1;
}

################################################################################
sub paint_tile { #{{{1
    my ($self, $left, $top, $right, $facing, $shape, $brush_index) = @_;
    $log->debug("paint_tile @_");

    # check all args are defined
    $log->logconfess("bad paint : " . Dumper(\@_)) unless (scalar @_ == scalar grep { defined $_ } @_);
    $log->logconfess("bad shape $shape") unless $shape =~ /[LTR]/;

    # change the grid key according to the shape if we're in the wrong triangle;
    if ($shape eq $SH_LEFT && $facing eq $SH_LEFT) {
        $left--;
        $right--;
    }
    elsif ($shape eq $SH_TOP && $facing eq $SH_RIGHT) {
        $left++;
        $top--;
    }

    # just ensure the facing is right, don't bother checking it
    $facing = $Shape_facing{$shape};

    # check for the same shape in the same place; we will just change the color of that tile
    # if we find it.
    my $grid_key = "${facing}_${left}_${top}_${right}";
    if (my $tile = $self->scene->grid->{$grid_key}) {
        if ($tile->{shape} eq $shape) {
            $log->debug("duplicate tile, change color TODO");
            return 0;
        }
    }

    # check for existing tiles that overlap.
    my $shifts = 0;
    for my $shift_point ( @{ $Shape_clashes{$shape} } ){
        $log->debug("shift_point for $shape " . Dumper($shift_point));
        my ($left_shift_offset, $top_shift_offset, $right_shift_offset, $new_shape, $shift_flag, $clash_list ) = @{ $shift_point };
        for my $clash ( @{ $clash_list } ) {
            $log->debug("clash " . Dumper($clash));
            my ($left_clash_offset, $top_clash_offset, $right_clash_offset, $clash_shape) = @{ $clash };

            my $grid_key = $Shape_facing{$clash_shape}
                . '_' . ($left + $left_clash_offset)
                . '_' . ($top + $top_clash_offset)
                . '_' . ($right + $right_clash_offset);

            if (my $tile = $self->scene->grid->{$grid_key}) {
                if ($tile->{shape} eq $clash_shape) {
                    $shifts += $shift_flag;
                    last;
                }
            }
        }
    }

    # check what clashes we found
    if ($shifts == 1 || $shifts == 2) {

        # clash with one point only, so shift to a new shape as required
        my ($left_shift_offset, $top_shift_offset, $right_shift_offset, $new_shape, $shift_flag ) = @{ $Shape_clashes{$shape}->[$shifts - 1] };

        $left += $left_shift_offset;
        $top += $top_shift_offset;
        $right += $right_shift_offset;
        $shape = $new_shape;
        $facing = $Shape_facing{$shape};
    }
    elsif ($shifts == 3) {
        $log->debug("clashes with existing cell(s)");
        return 0;
    }

    $grid_key = "${facing}_${left}_${top}_${right}";
    my $tile = {
        shape => $shape,
        brush_index => defined $brush_index ? $brush_index : $self->brush_index,
        left => $left,
        top => $top,
        right => $right,
        facing => $facing,
    };

    $log->debug("paint_tile $shape at $left, $top, $right, $facing");

    $self->scene->grid->{$grid_key} = $tile;

    # add to cache on creation unless we're painting an area or pasting,
    # in which case the selected_tile hash is used in addition to the cache.
    my $action = $self->frame->action;
    unless ($action eq $IsoFrame::AC_PASTE || ($action eq $IsoFrame::AC_PAINT && $self->area_start)) {
        $log->debug("add to tile_cache");
        push @{ $self->tile_cache }, $grid_key;
    }

    return $grid_key;
}

################################################################################
# perform a clipboard operation
sub clipboard_operation { #{{{1
    my ($self, $operation, $data) = @_;

    $log->debug("clipboard_operation: $operation");

    if ($operation eq 'paste') {

        if (my $number_clipboard_items = @{ $self->scene->clipboard }) {

            $self->frame->action($IsoFrame::AC_PASTE);
            $self->set_cursor;

            # use the index specified in $data or if undef, the most recent clipboard object
            $self->paste_list($self->scene->clipboard->[ defined $data ? $data : 0]->{tiles});

            # a true value here tells mouse_event_handler to paint the pasted tiles on each mouse event.
            # we change it from 1 to 2 on left-down, which means the next left-up is the paste position.
            # this is so paste operations started from the popup list don't immediately terminate
            # when the activating double-click on the list is over the canvas, and so sends a left-up.
            $self->paint_shape(1);

            $self->frame->Refresh;
        }
    }
    else {

        # copy or cut, so first step is to copy selected tiles to clipboard
        my ($min_x, $min_y, $max_x, $max_y);
        my @clipboard_tiles = ();
        my @selected_keys = ();

        while (my ($key, $tile) = each %{ $self->scene->grid }) {

            if ($tile->{selected}) {
                $log->debug("copy tile at $key to clipboard");

                # create a copy to put on the clipboard, we don't want the extra fields added 
                # to the scene copy.
                my $clipboard_tile = { %{ $tile } };

                # we need the tile color, shape and the relative position; the anchor
                # will be the tile nearest the centre, as calculated from the rectangular
                # area occupied by the tiles.
                my $x = $clipboard_tile->{x} = $tile->{top} * $self->x_grid_size;
                my $y = $clipboard_tile->{y} = ($tile->{left} + $tile->{top} / 2) * $self->y_grid_size;

                # the position found above is the anchor; find the true extents of the tile
                # to get an accurate area. (This makes a surprisingly large difference for small sets.)
                # note that the anchor's x coord is already the minimum x coord for all shapes.
                my $extent = $Shape_extent->{ $tile->{shape} };
                my $tile_min_x = $x;
                my $tile_min_y = $y - $extent->{min_y} * $self->y_grid_size;
                my $tile_max_x = $x + $extent->{max_x} * $self->x_grid_size;
                my $tile_max_y = $y + $extent->{max_y} * $self->y_grid_size;

                $min_x = $tile_min_x if ! defined $min_x || $tile_min_x < $min_x;
                $min_y = $tile_min_y if ! defined $min_y || $tile_min_y < $min_y;
                $max_x = $tile_max_x if ! defined $max_x || $tile_max_x > $max_x;
                $max_y = $tile_max_y if ! defined $max_y || $tile_max_y > $max_y;

                push @clipboard_tiles, $clipboard_tile;
                push @selected_keys, $key if $operation eq 'cut';

            }
        }

        # bail if no selections
        return unless @clipboard_tiles;

        # we've got the clipboard tiles in the list, we can do the cut now if req.
        if ($operation eq 'cut') {
            $self->add_undo_action($IsoFrame::AC_ERASE, \@selected_keys);
            for my $key (@selected_keys) {
                delete $self->scene->grid->{$key};
            }
        }

        # find the centre of the region and then the tile that's closest to it
        my $centre = [ $min_x + ($max_x - $min_x) / 2, $min_y + ($max_y - $min_y) / 2 ];
        my $centre_tile;
        my $min_distance;
        for my $tile (@clipboard_tiles) {
            my $distance = Math::Geometry::Planar::SegmentLength([$centre, [ $tile->{x}, $tile->{y} ]]);
            if (! defined $min_distance || $distance < $min_distance) {
                $min_distance = $distance;
                $centre_tile = $tile;
            }
        }
        unless ($centre_tile) {
            $log->logwarn("didn't find a centre tile?");
            return;
        }

        # make centre_tile a copy so we can refer to it after the actual
        # centre tile gets adjusted to 0,0,0.
        $centre_tile = { %{ $centre_tile } };

        # adjust the clipboard tile coords to be offsets from centre tile
        for my $tile (@clipboard_tiles) {
            for my $coord (qw(left top right)) {
                $tile->{$coord} -= $centre_tile->{$coord};
            }

            # no need for these attributes anymore
            delete $tile->{$_} foreach qw(x y selected);
        }

        # create thumbnail file

        # create a 1024x1024 bitmap for good detail; we'll rescale before saving
        my ($width, $height) = (1024, 1024);
        my $clip_width = ($max_x - $min_x) * 1.1;
        my $clip_height = ($max_y - $min_y) * 1.1;

        # find the scale factors for logical to device width, then use the smaller so we fit
        # both dimensions in.
        my $x_scale = $width / $clip_width;
        my $y_scale = $height / $clip_height;
        my $thumb_scale = min($x_scale, $y_scale);
        $log->debug("thumb_scale from $x_scale, $y_scale : $thumb_scale");

        # create a bitmap to hold the thumbed image
        my $thumb_bm = Wx::Bitmap->new($width, $height, 32);
        $log->debug("bitmap ok? : " . $thumb_bm->IsOk);

        # create a dc to draw the image into and link the bitmap to it
        my $thumb_dc = Wx::MemoryDC->new();
        $thumb_dc->SelectObject($thumb_bm);
        $log->debug("memory dc ok? : " . $thumb_dc->IsOk);

        $thumb_dc->SetUserScale($thumb_scale, $thumb_scale);

        # set device origin to top left of image, which will be centred around 0,0,0
        # since we're drawing with adjusted clipboard coords
        my $device_origin_x = ($clip_width / 2) * $thumb_scale;
        my $device_origin_y = ($clip_height / 2) * $thumb_scale;
        $thumb_dc->SetDeviceOrigin(max($device_origin_x, $device_origin_y), max($device_origin_x, $device_origin_y));

        # draw the scene into the memory dc
        $thumb_dc->SetBackground(wxWHITE_BRUSH);
        $thumb_dc->Clear;

        $thumb_dc->SetPen(wxWHITE_PEN);

        # it's tempting to try to use draw_scene to draw into the dc, but there's
        # quite a lot of stuff we don't want, and not much we do, so just copy for now.
        my $palette = $self->palette;

        # to precisely centre the shape in the image, adjust by the distance from
        # the centre tile anchor to the centre
        my $x_adjustment = $min_x + (($max_x - $min_x) / 2) - $centre_tile->{x};
        my $y_adjustment = $min_y + (($max_y - $min_y) / 2) - $centre_tile->{y};

        for my $tile (@clipboard_tiles) {

            $thumb_dc->SetBrush($palette->[ $tile->{brush_index} ]);

            my $shape = $Tile_shape->{ $tile->{shape} }
                or $log->logdie("bad tile shape $tile->{shape}");

            $thumb_dc->DrawPolygon(
                $shape->{polygon_points}, 
                $tile->{top} * $self->x_grid_size - $x_adjustment,
                ($tile->{left} + $tile->{top} / 2) * $self->y_grid_size - $y_adjustment,
            );
        }

        my $item_name = $self->scene->filename;
        $item_name .= localtime->strftime("_%Y%m%d_%H%M%S");

        my $thumb_image = $thumb_bm->ConvertToImage;
        $thumb_image->Rescale(64,64,wxIMAGE_QUALITY_HIGH);
        $thumb_image->SaveFile("clipboard/$item_name.png", wxBITMAP_TYPE_PNG);

        unshift @{ $self->scene->clipboard }, {
            tiles => \@clipboard_tiles,
            name => $item_name,
        };
        # $log->info("scene cb " . Dumper($self->scene->clipboard));

    }

    $self->Refresh unless $operation eq 'copy';

    return;
}

################################################################################
sub calculate_grid_dims { #{{{1
    my ($self, $side) = @_;

    $self->x_grid_size( ( cos (deg2rad(30)) * ($side / 1)) );
    $self->y_grid_size( $side );
    $self->control_gradient( ($self->y_grid_size / 2) / $self->x_grid_size);

    # y offsets are expressed in units of half the y grid size, eg 0 = 0, 1 = ygs/2, 2 = ygs, 3 = ygs * 1.5.
    # (no other values are valid except 0,1,2,3).
    # Build an array to hold these values so we don't calculate them for each tile.
    @Y_offsets = (0, $self->y_grid_size / 2, $self->y_grid_size, $self->y_grid_size * 1.5 ); 

    # shapes are converted into Wx::Point lists for use by DrawPolygon below
    $Tile_shape = {
        L => {
            points => [
                [ 0, 0 ],
                [ $self->x_grid_size, $self->y_grid_size / 2 ],
                [ $self->x_grid_size, $self->y_grid_size * 1.5 ],
                [ 0, $self->y_grid_size ],
            ],
        },
        T => {
            points => [
                [ 0, 0 ],
                [ $self->x_grid_size, $self->y_grid_size / 2 ],
                [ $self->x_grid_size * 2, 0 ],
                [ $self->x_grid_size, - $self->y_grid_size / 2 ],
            ],
        },
        R => {
            points => [
                [ 0, 0 ],
                [ 0, $self->y_grid_size ],
                [ $self->x_grid_size, $self->y_grid_size / 2 ],
                [ $self->x_grid_size, - $self->y_grid_size / 2 ],
            ],
        },
        TR => {
            points => [
                [ 0, 0 ],
                [ $self->x_grid_size, $self->y_grid_size / 2 ],
                [ 0, $self->y_grid_size ],
            ],
        },
        TL => {
            points => [
                [ 0, 0 ],
                [ $self->x_grid_size, - $self->y_grid_size / 2 ],
                [ $self->x_grid_size, $self->y_grid_size / 2 ],
            ],
        },
    };

    # convert point lists to Wx::Point objects which DrawPolygon needs
    for my $shape (values %{ $Tile_shape }) {
        $shape->{polygon_points} = [];
        for my $point (@{ $shape->{points} }) {
            push @{ $shape->{polygon_points} }, Wx::Point->new(@{ $point });
        }
    }

    # we loaded the tick bitmaps in the App constructor; they need to be resized
    # to suit the grid. Sadly this involves knowing how big the reference grid lines
    # were. For square ticks it was 220.
    my $tick_scale = $side / 220;
    my $app = wxTheApp;
    for my $side ($IsoFrame::SI_LEFT, $IsoFrame::SI_TOP, $IsoFrame::SI_RIGHT, 'TL', 'TR') {
        my $image = $app->bitmap->{"tick_$side"}->ConvertToImage;
        $image->Rescale($image->GetWidth * $tick_scale, $image->GetHeight * $tick_scale, wxIMAGE_QUALITY_HIGH);
        $app->bitmap->{"tick_$side"} = Wx::Bitmap->new($image);
    }

    $log->debug(sprintf "xgs %s, ygs %s", $self->x_grid_size, $self->y_grid_size);

    return;
}

################################################################################
# Handle window resize; suprisingly little we can do here, since changes of
# scale or offset must trigger repaint.
sub find_logical_size { #{{{1
    my ($self) = @_;

    my ($width, $height) = $self->GetSizeWH;

    $self->device_width($width);
    $self->device_height($height);
    $self->tile_cache(0);

    return;
}

################################################################################
sub find_grid_extents { #{{{1
    my ($self, $dc) = @_;

    $dc->SetUserScale($self->scene->scale, $self->scene->scale);
    $dc->SetDeviceOrigin($self->scene->origin_x, $self->scene->origin_y);

    # we draw the grid in logical coords; we need to know what the logical extents of the canvas are.
    my ($min_x, $max_x) = ( $dc->DeviceToLogicalX(0), $dc->DeviceToLogicalX($self->device_width));
    my ($min_y, $max_y) = ( $dc->DeviceToLogicalY(0), $dc->DeviceToLogicalY($self->device_height));

    # calculate the grid coordinates for an area slightly larger than the window.
    # We force the X coord to an even line (same as the origin) to cope with the uneven nature of the X axis.
    my $min_x_grid = int($min_x / ($self->x_grid_size * 2)) * 2 - 2;
    my $min_y_grid = int($min_y / ($self->y_grid_size)) - 2;
    my $max_x_grid = int($max_x / ($self->x_grid_size * 2)) * 2 + 2;
    my $max_y_grid = int($max_y / ($self->y_grid_size)) + 1;
    my $count = $max_x_grid - $min_x_grid;

    $log->debug("logical screen $min_x,$min_y to $max_x,$max_y grid bounds $min_x_grid,$min_y_grid to $max_x_grid,$max_y_grid");

    $dc->SetPen($self->bg_line_pen);

    # slopes
    for my $i (0 .. $count * 2) {
        $dc->DrawLine($min_x_grid * $self->x_grid_size, ($min_y_grid + $i) * $self->y_grid_size, $max_x_grid * $self->x_grid_size, ($min_y_grid + $i - $count / 2) * $self->y_grid_size);
        $dc->DrawLine($max_x_grid * $self->x_grid_size, ($min_y_grid + $i) * $self->y_grid_size, $min_x_grid * $self->x_grid_size, ($min_y_grid + $i - $count / 2) * $self->y_grid_size);
    }

    # verticals
    for my $i (0 .. $count) {
        $dc->DrawLine(($min_x_grid + $i) * $self->x_grid_size, $min_y_grid * $self->y_grid_size, ($min_x_grid + $i) * $self->x_grid_size, $max_y_grid * $self->y_grid_size);
    }

    $log->debug("screen logical grid coords 2: $min_x_grid,$min_y_grid to $max_x_grid,$max_y_grid");

    # record extents to filter repainting
    $self->min_x_grid($min_x_grid);
    $self->min_y_grid($min_y_grid);
    $self->max_x_grid($max_x_grid);
    $self->max_y_grid($max_y_grid);

    return;
}

################################################################################
sub repaint_canvas { #{{{1
    my ($self) = @_;

    my $dc = Wx::PaintDC->new( $self );
    my $frame = $self->frame;

    my $start_refresh = [ gettimeofday ];

    $dc->SetBackground($self->background_brush);
    $dc->Clear;

    # $log->info(sprintf "refresh: cleared after %.6f", tv_interval($start_refresh));

    $self->find_grid_extents($dc);
    # $log->info(sprintf "refresh: find_ge after %.6f", tv_interval($start_refresh));

    $self->draw_scene($dc);
    # $log->info(sprintf "refresh: drw_scn after %.6f", tv_interval($start_refresh));

#    $dc->SetUserScale(1,1);
#    $dc->SetDeviceOrigin(0,0);
#
#    $dc->DrawBitmap( $self->bitmap->{cube}, 100, 100, 1);

    return;
}

################################################################################
sub draw_scene { #{{{1
    my ($self, $dc, %param) = @_;

    my $frame = $self->frame;
    my $current_brush = 0;

    # my $start_draw = [ gettimeofday ];

#    $dc->DrawLine(0, 0, 0, $self->y_grid_size);
#    $dc->DrawLine(0, 0, $self->x_grid_size, $self->y_grid_size / 2);
#    $dc->DrawLine($self->x_grid_size, $self->y_grid_size / 2, 0, $self->y_grid_size);

#    for my $left (-5 .. 5) {
#        for my $top (-5 .. 5) {
#            my $x = $top * $self->x_grid_size;
#            my $y = ($left + $top / 2) * $self->y_grid_size;
#            my $right = $left + $top;
#            $dc->DrawText("$left,$top,$right", $x,$y);
#        }
#    }

#    my @key;
#    my @anchor;
#    (my $min_x, my $min_y, $key[0], $key[1], $anchor[0], $anchor[1], my $facing) = @stash;
#    if (defined $min_x) {
##        $log->info("min_x $min_x, min_y $min_y, key @key, anchor @anchor, facing $facing");
##        $dc->DrawRectangle($min_x * $self->x_grid_size, $min_y * $self->y_grid_size, $self->x_grid_size, $self->y_grid_size);
##        $dc->SetPen(wxRED_PEN);
##        $dc->DrawCircle(@key, 20);
#        $dc->SetPen(wxGREEN_PEN);
#        $dc->SetBrush(wxTRANSPARENT_BRUSH);
#        $dc->DrawCircle(@anchor, 20);
#    }

#    return;

    if ($param{tb_transparent_rbn}) {
        $dc->SetPen(wxTRANSPARENT_PEN);
    }
    elsif ($param{tb_normal_rbn}) {
        $dc->SetPen($self->tile_line_pen);
    }

    my $palette = $self->palette;

    # use the previous list of painted keys (if we haven't moved or scaled)
    # or use the whole scene
    my $tile_keys = $self->tile_cache 
        || [ keys %{ $self->scene->grid } ];
#        || [ map { join('_', @{ $_ }{qw( facing left top right )}) } values %{ $self->scene->grid } ];
    my @new_cache = ();
    my %tick_offset = (
        $IsoFrame::SI_LEFT  => [ 5, 55 ],
        $IsoFrame::SI_TOP   => [ 53, -27 ],
        $IsoFrame::SI_RIGHT => [ 5, -20 ],
        TL                  => [ 28, -22, ],
        TR                  => [ 5, 42, ],
    );
    my $bitmap = wxTheApp->bitmap;
    my $select_toggle = $self->select_toggle;
    my $action = $self->frame->action;
    my $doing_select = $action =~ /select/;

    # if we're in progress while painting an area or pasting (ie before the closing mouse up event),
    # also check the keys from the selected_tile hash. Make this a separate pass so we don't add 
    # these keys to the cache if we're creating it.
    my @passes = (0);
    if ($action eq $IsoFrame::AC_PASTE || ($action eq $IsoFrame::AC_PAINT && $self->area_start)) {
        push @passes, 1;
    }

    for my $pass (@passes) {

        # The ?: operator short-circuits so we don't get the selected_tile keys unless we need them.
        for my $grid_key ($pass == 0 ? @{ $tile_keys } : keys %{ $self->selected_tile }) {

            my $tile = $self->scene->grid->{$grid_key};

            # skip this stuff for temporary tiles
            if ($pass == 0) {

                # filter deleted items when deleting area
                next if $self->deleted_tile->{$grid_key};

                # individual deleted tiles will still be in the key list but won't be found
                next unless $tile;

                # if we don't have a tile cache, filter by visible location and
                # build the new cache
                unless ($self->tile_cache) {
                    next if $tile->{top} < ($self->min_x_grid - 1) || $tile->{top} > $self->max_x_grid;
                    my $y_grid = ($tile->{left} + $tile->{right}) / 2;
                    next if $y_grid < $self->min_y_grid || $y_grid > $self->max_y_grid;

                    push @new_cache, $grid_key;
                }
            }

    #        $log->debug(sprintf "draw $tile->{shape} $grid_key at %f, %f",
    #            $tile->{top} * $self->x_grid_size,
    #            ($tile->{left} + $tile->{top} / 2) * $self->y_grid_size);

            $dc->SetBrush($palette->[ $tile->{brush_index} ]);

            if ($param{tb_matching_tile_rbn}) {
                $dc->SetPen(Wx::Pen->new($dc->GetBrush->GetColour,1,wxPENSTYLE_SOLID));
            }

            my $shape = $Tile_shape->{ $tile->{shape} }
                or $log->logdie("bad tile shape $tile->{shape}");

            $dc->DrawPolygon(
                $shape->{polygon_points}, 
                $tile->{top} * $self->x_grid_size,
                ($tile->{left} + $tile->{top} / 2) * $self->y_grid_size,
            );

            # draw selected images, including temporary selections (or deselections) during area select
            my $part_of_area_select = defined $self->selected_tile->{$grid_key} && $action =~ /select/;
            if (($tile->{selected} && ($select_toggle || ! $part_of_area_select || ! $doing_select))
                || ($select_toggle && $part_of_area_select) )
            {
                $dc->DrawBitmap($bitmap->{"tick_$tile->{shape}"},
                    $tile->{top} * $self->x_grid_size + $tick_offset{$tile->{shape}}->[0],
                    ($tile->{left} + $tile->{top} / 2) * $self->y_grid_size + $tick_offset{$tile->{shape}}->[1],
                    1,
                );
            }
        }
    }

    # $log->info(sprintf "draw_scene: painted tiles after %.6f", tv_interval($start_draw));

    unless ($self->tile_cache) {
        $log->debug("built new cache with $#new_cache+1 tiles");
        $self->tile_cache(\@new_cache);
    }

    # draw current location unless we're exporting; just check for any key from export_options
    if ((my $current_location = $self->current_location) && ! defined $param{a4_rbn}) {
        my ($left, $top, $right, $facing) = @{ $current_location };

        # here, we can equate side with shape
        my $shape = $self->frame->current_side;

        # change the grid key according to the shape if we're in the wrong triangle;
        if ($shape eq $SH_LEFT && $facing eq $SH_LEFT) {
            $left--;
            $right--;
        }
        elsif ($shape eq $SH_TOP && $facing eq $SH_RIGHT) {
            $left++;
            $top--;
        }

        if (my $paste_list = $self->paste_list) {

#            for my $tile ( @{ $paste_list } ) {
#
#                $dc->SetBrush($palette->[ $tile->{brush_index} ]);
#
#                my $shape = $Tile_shape->{ $tile->{shape} }
#                    or $log->logdie("bad tile shape $tile->{shape}");
#
#                # coords in paste tiles are offsets from current location
#                my $adjusted_top = $top + $tile->{top};
#                $dc->DrawPolygon(
#                    $shape->{polygon_points}, 
#                    $adjusted_top * $self->x_grid_size,
#                    ($left + $tile->{left} + $adjusted_top / 2) * $self->y_grid_size,
#                );
#            }
        }
        else {

            my $polygon_points = [];
            my $tile_shape = $Tile_shape->{$shape};

            if ($self->cursor_multiplier_x > 1 || $self->cursor_multiplier_y > 1) {
                my $shape_multiplier = {
                    $SH_LEFT => [
                        [ 1,1 ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_x ],
                        [ $self->cursor_multiplier_x, ($self->cursor_multiplier_y * 2 + $self->cursor_multiplier_x) / 3 ],
                        [ 1, $self->cursor_multiplier_y, ],
                    ],
                    $SH_TOP => [
                        [ 1,1 ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_x, ],
                        [ ($self->cursor_multiplier_x + $self->cursor_multiplier_y) / 2, 0 ],
                        [ $self->cursor_multiplier_y, $self->cursor_multiplier_y ],
                    ],
                    $SH_RIGHT => [
                        [ 1,1 ],
                        [ 1, $self->cursor_multiplier_y, ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_y * 2 - $self->cursor_multiplier_x ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_x ],
                    ],
                };
                my $multipliers = $shape_multiplier->{ $shape };
                for my $i (0 .. 3) {

                    # there's one corner in one shape that needs something more; top shape, 3rd corner
                    # (ie right hand corner) has a Y coord of 0, so multiplying it by anything doesn't
                    # work. Just calculate it directly for that case.
                    my $third_top_y_coord = $shape eq $SH_TOP && $i == 2
                        ? ($self->cursor_multiplier_x - $self->cursor_multiplier_y) * ($self->y_grid_size / 2)
                        : 0;

                    push @{ $polygon_points }, Wx::Point->new(
                        $tile_shape->{points}->[$i]->[0] * $multipliers->[$i]->[0],
                        $tile_shape->{points}->[$i]->[1] * $multipliers->[$i]->[1] + $third_top_y_coord,
                    );
                }
            }
            else {

                for my $point (@{ $tile_shape->{points} }) {
                    push @{ $polygon_points }, Wx::Point->new( @{ $point } );
                }
            }

            $dc->SetBrush(wxTRANSPARENT_BRUSH);
            $dc->SetPen(Wx::Pen->new(wxRED,4,wxPENSTYLE_SOLID));
            $dc->DrawPolygon(
                $polygon_points, 
                $top * $self->x_grid_size,
                ($left + $top / 2) * $self->y_grid_size,
            );
        }
    }
    # $log->info(sprintf "draw_scene: finished      after %.6f", tv_interval($start_draw));

    return;
}

################################################################################
sub export_scene { #{{{1
    my ($self) = @_;

    $log->info("export_scene");

    my $busy = Wx::BusyCursor->new;
    $self->SetCursor(wxHOURGLASS_CURSOR);
    $self->Refresh;
    $self->Update;

    my $dc = Wx::PaintDC->new( $self );
    my $frame = $self->frame;

    # we need to know what the logical extents of the scene are.

    # find extents in terms of grid keys
    my ($min_x_grid, $min_y_grid, $max_x_grid, $max_y_grid);
    for my $tile (values %{ $self->scene->grid }) {

        my $tile_y_grid = ($tile->{left} + $tile->{right}) / 2;
        unless (defined $min_x_grid) {
            $min_x_grid = $tile->{top};
            $max_x_grid = $tile->{top};
            $min_y_grid = $tile_y_grid;
            $max_y_grid = $tile_y_grid;
        }

        $min_x_grid = min($min_x_grid, $tile->{top});
        $max_x_grid = max($max_x_grid, $tile->{top});
        $min_y_grid = min($min_y_grid, $tile_y_grid);
        $max_y_grid = max($max_y_grid, $tile_y_grid);
    }
    $log->debug("grid key extents: $min_x_grid,$min_y_grid to $max_x_grid,$max_y_grid");

    # add margin so it's consistent wrt number of empty grid rows/cols
    # around image
    $min_x_grid -= 2;
    $min_y_grid -= 2;
    $max_x_grid += 2;
    $max_y_grid += 2;

    $self->min_x_grid($min_x_grid);
    $self->min_y_grid($min_y_grid);
    $self->max_x_grid($max_x_grid);
    $self->max_y_grid($max_y_grid);

    # extents in logical coords
    my $min_x = $min_x_grid * $self->x_grid_size;
    my $max_x = $max_x_grid * $self->x_grid_size;
    my $min_y = $min_y_grid * $self->y_grid_size;
    my $max_y = $max_y_grid * $self->y_grid_size;

    my $export_option = $self->scene->export_options;

    # find bitmap size
    my ($width, $height); 
    if ($export_option->{pixels_per_tile_rbn}) {
        my $pixels_per_tile = $export_option->{pixels_per_tile_sld};
        ($width, $height) = (($max_x_grid - $min_x_grid) * $pixels_per_tile, ($max_y_grid - $min_y_grid) * $pixels_per_tile);
    }
    else {

        # doc sizes in inches, in portrait
        my $sheet_size_inches = {
            a3_rbn => [ 11.69, 16.54 ],
            a4_rbn => [ 8.27, 11.69 ],
            a5_rbn => [ 5.83, 8.27 ],
        };

        my ($selected_sheet_rbn) = grep { $export_option->{$_} } qw(a3_rbn a4_rbn a5_rbn);

        my @dims = @{ $sheet_size_inches->{ $selected_sheet_rbn } };
        @dims = reverse @dims if $export_option->{landscape_chb};

        my $dpi = $export_option->{dpi_600_rbn} ? 600 : 300;

        ($width, $height) = (int($dims[0] * $dpi), int($dims[1] * $dpi));
    }

    $log->info("top left $min_x,$min_y to bottom right $max_x,$max_y, bitmap dims $width x $height");

    # find the scale factors for logical to device width, then use the smaller so we fit
    # both dimensions in.
    my $x_scale = $width / ($max_x - $min_x);
    my $y_scale = $height / ($max_y - $min_y);
    my $export_scale = min($x_scale, $y_scale);
    $log->debug("export_scale from $x_scale, $y_scale : $export_scale");

    # create a bitmap to hold the exported image
    my $export_bm = Wx::Bitmap->new($width, $height, 24);
    $log->debug("bitmap ok? : " . $export_bm->IsOk);

    # create a dc to draw the image into and link the bitmap to it
    my $export_dc = Wx::MemoryDC->new();
    $export_dc->SelectObject($export_bm);
    $log->debug("memory dc ok? : " . $export_dc->IsOk);

    $export_dc->SetUserScale($export_scale, $export_scale);

    # set device origin to top left
    my $device_origin_x = -$min_x * $export_scale;
    my $device_origin_y = -$min_y * $export_scale;
    $export_dc->SetDeviceOrigin($device_origin_x, $device_origin_y);

    # draw the scene into the memory dc
    $export_dc->SetBackground(wxTRANSPARENT_BRUSH);
    $export_dc->Clear;

    $self->draw_scene($export_dc, %{ $export_option });

    # convert to an image for alpha channel addition
    my $export_image = $export_bm->ConvertToImage;
    $export_image->SetMask(1);
    $export_image->InitAlpha();

    # save the image to file
    my $name = $self->scene->filename;
    my ($extension, $type) = $export_option->{jpeg_rbn}
        ? ('jpg', wxBITMAP_TYPE_JPEG)
        : ('png', wxBITMAP_TYPE_PNG);
    $export_image->SaveFile("$name.$extension", $type);

    $log->debug("export done");
    $self->SetCursor(wxCROSS_CURSOR);

    return;
}

################################################################################
# action is whatever we've just done, data is either a tile or a list of grid keys.
sub add_undo_action { #{{{1
    my ($self, $action, $data) = @_;

    # we want a reference to a list of tiles.
    my $undo_data = [ ((ref $data) eq 'ARRAY') 
        ? map { $self->scene->grid->{$_} } @{ $data } 
        : $data
    ];

    my $new_action = [ $action, $undo_data ];

    # Are there any items on the redo stack?
    if (my $new_branch_node = pop @{ $self->scene->redo_stack }) {

        $log->info("create branch then add new action");
        
        # the thing called new_branch_node may already be a branch node, or it may be a simple action.
        # If the latter, make it into a branch node first, then we can add the new action generically.
        if ((ref $new_branch_node) ne 'HASH') {

            # turn this simple item into a branch item pointing to the top item from the redo stack, 
            # then we can add the new branch.
            $new_branch_node = {
                branches => [ $new_branch_node, ],
                current_branch => 0,
            };
        }

        # data for the current branch is an action; turn this into a list of actions from the redo stack plus the existing current action
        # from the branch node.
        my $current_branch = $new_branch_node->{current_branch};
        $new_branch_node->{branches}->[ $current_branch ] = [ @{ $self->scene->redo_stack }, $new_branch_node->{branches}->[$current_branch],  ];

        # add the new branch (containing the new undo action) and make it current
        push @{ $new_branch_node->{branches} }, $new_action;
        $new_branch_node->{current_branch} = $#{ $new_branch_node->{branches} };

        push @{ $self->scene->undo_stack }, $new_branch_node;

        # clear the redo stack now we've saved it to the branch
        $self->scene->redo_stack([]);

        $log->info("new_branch_node: " . Dumper($new_branch_node, $self->scene->undo_stack));
    }
    else {
        push @{ $self->scene->undo_stack }, $new_action;
    }

    return;
}

################################################################################
sub undo_or_redo { #{{{1
    my ($self, $redo) = @_;

    # if we didn't get an explicit type, it's a timer and we check the flag set on LEFT_DOWN.
    unless (defined $redo) {
        $redo = $self->frame->undo_or_redo_flag;
    }

    # undo and redo are conceptually the same; they take an action off a stack, either paint it or erase it 
    # depending on the action type and then put it on another stack. They only differ in the choice of stack
    # and action.
    my ($source_stack, $target_stack, $erase_action, $paint_action) = $redo
        ? ($self->scene->redo_stack, $self->scene->undo_stack, $IsoFrame::AC_ERASE, $IsoFrame::AC_PAINT)  
        : ($self->scene->undo_stack, $self->scene->redo_stack, $IsoFrame::AC_PAINT, $IsoFrame::AC_ERASE);

    return unless my $next_action = pop @{ $source_stack };

    push @{ $target_stack }, $next_action;

    # Ok, make the change to the scene.
    # Turn $next_action into a real action if it's a branch node
    if ((ref $next_action) eq 'HASH') {
        $log->debug("get real action from branch node");
        $next_action = $next_action->{branches}->[ $next_action->{current_branch} ];
    }

    my ($action, $tiles) = @{ $next_action };
    if ($action eq $erase_action) {
        for my $tile ( @{ $tiles } ) {
            my $grid_key = "$tile->{facing}_$tile->{left}_$tile->{top}_$tile->{right}";
            delete $self->scene->grid->{$grid_key};
        }
    }
    elsif ($action eq $paint_action) {
        for my $tile ( @{ $tiles } ) {
            my $grid_key = "$tile->{facing}_$tile->{left}_$tile->{top}_$tile->{right}";
            $self->scene->grid->{$grid_key} = $tile;
            push @{ $self->tile_cache }, $grid_key;
        }
    }

    # if the top action on the redo stack is a branch node, indicate this on the button
    my $top_redo_index = $#{ $self->scene->redo_stack };
    my $redo_tooltip = ($top_redo_index + 1) . " actions.";
    my $redo_button_bitmap = 'redo';
    if ( $top_redo_index >= 0 && (ref $self->scene->redo_stack->[$top_redo_index]) eq 'HASH' ) {
        $redo_button_bitmap = 'branch_redo';
        $redo_tooltip .= ' ' . scalar @{ $self->scene->redo_stack->[$top_redo_index]->{branches} } . ' branches available.';
    }
    
    IsoApp::set_button_bitmap($self->frame->misc_btn->{redo}, $redo_button_bitmap);

    $self->frame->misc_btn->{'undo'}->SetToolTip(scalar @{ $self->scene->undo_stack } . " actions.");
    $self->frame->misc_btn->{'redo'}->SetToolTip($redo_tooltip);

    $self->Refresh;

    return;
}

################################################################################
sub move_origin { #{{{1
    my ($self, $device_x, $device_y) = @_;

    $self->scene->origin_x ( $self->scene->origin_x - ($self->last_device_x - $device_x));
    $self->scene->origin_y ( $self->scene->origin_y - ($self->last_device_y - $device_y));
    $log->debug(sprintf("origin %d, %d, origin mod grid %d, %d", $self->scene->origin_x, $self->scene->origin_y, $self->scene->origin_x % $self->x_grid_size, $self->scene->origin_y % $self->y_grid_size));

    $self->tile_cache(0);

    return;
}

################################################################################
sub import_bitmap { #{{{1
    my ($self, $shape) = @_;

    $log->debug("import file " . $self->frame->import_file . " on side $shape at " . join(',', @{ $self->current_location }));

    my $image = Wx::Image->new($self->frame->import_file, wxBITMAP_TYPE_ANY);

    my ($left, $top, $right, $facing) = @{ $self->current_location };

    # change the grid key according to the shape if we're in the wrong triangle;
    if ($shape eq $SH_LEFT && $facing eq $SH_LEFT) {
        $left--;
        $right--;
    }
    elsif ($shape eq $SH_TOP && $facing eq $SH_RIGHT) {
        $left++;
        $top--;
    }

    my ($width, $height) = ($image->GetWidth, $image->GetHeight);
    my @imported_tiles = ();

    # sometimes small images have their alpha channel turned into a mask; this turns it back
    $image->InitAlpha;

    my $has_alpha = $image->HasAlpha;

    # express the way the coords change per image row and column for each
    # side; this is the current side in the cube, nothing to do with the selected tile.
    my @ltr_coords = ($left, $top, $right);
    my $side_changes = {
        $SH_LEFT => {
            x_changes => [ 0, 1, 1 ],
            y_changes => [ 1, -$width, -($width-1) ],
        },
        $SH_TOP => {
            x_changes => [ 0, 1, 1 ],
            y_changes => [ -1, -($width-1), -$width ],
        },
        $SH_RIGHT => {
            x_changes => [ -1, 1, 0 ],
            y_changes => [ $width+1, -$width, 1 ],
        },
    };
    my $changes = $side_changes->{$shape};

    for my $y (0 .. $height - 1) {
        for my $x (0 .. $width - 1) {
    
            # by virtue of how we anchoring the extended cursor displayed for import,
            # the selected tile is at the bottom of the image for TOP imports so we 
            # have to reverse the y sequence.
            my $image_y = $shape eq $SH_TOP
                ? $height - 1 - $y
                : $y;

            # skip wholly transparent pixels; we aren't doing anything useful with
            # alpha values yet.
            if (! $has_alpha || (my $alpha = $image->GetAlpha($x, $image_y))) {

                my ($red, $green, $blue) = ($image->GetRed($x,$image_y), 
                    $image->GetGreen($x,$image_y),
                    $image->GetBlue($x,$image_y));

                my $colour_str = sprintf("#%02X%02X%02X", $red, $green, $blue);
                $self->brush_index($self->get_palette_index($colour_str));
                if (my $key = $self->paint_tile(@ltr_coords, $facing, $shape)) {
                    push @imported_tiles, $key;
                }
            }

            foreach my $i (0 .. 2) {
                $ltr_coords[$i] += $changes->{x_changes}->[$i];
            }
        }
        foreach my $i (0 .. 2) {
            $ltr_coords[$i] += $changes->{y_changes}->[$i];
        }
    }

    $self->add_undo_action($IsoFrame::AC_PAINT, \@imported_tiles);
    push @{ $self->tile_cache }, \@imported_tiles;

    $self->cursor_multiplier_x(1);
    $self->cursor_multiplier_y(1);

    return;
}

################################################################################
sub set_selection_for_all { #{{{1
    my ($self, $flag) = @_;

    for my $tile (values %{ $self->scene->grid }) {
        $tile->{selected} = $flag;
    }
    $self->Refresh;

    return;
}

################################################################################
sub select_visible { #{{{1
    my ($self) = @_;

    for my $key (@{ $self->tile_cache }) {
        $self->scene->grid->{$key}->{selected} = 1;
    }
    $self->Refresh;

    return;
}

################################################################################

# TODO {{{1

# compression of data files

# undo stack to include selection

# import of bitmap 
#  * currently only works for top plane

# line width / brush shape

# floodfill

# list of previous color combinations

# we have a conventional undo/redo arrangement, where you can move
# back and forward in past actions but as soon as you do anything
# except redo (having wound back), the redo stack is cleared. What if
# the point where you restarted became a fork, and you could later go back
# and return down the original path. What would the interface look like?
# A custom dialog would be required...

# once branching undo was implemented, you could jump back/forward to a branch point.
# This could be a story-telling tool; a scene could contain multiple paths, and you
# could choose which path to display via a script.

# undo/redo multiple actions at once, eg one press of the undo button goes back 10 steps.

# you could create checkpoints in the undo log and then quickly undo everything back to the most recent
# checkpoint. Branch points (above) become automatic checkpoints.

# Still on undo, display the stack sizes in the undo/redo buttons. See undo_or_redo for a partial attempt.
# Later; I mocked this up and I don't think it's worth the ugliness, particularly as we're planning
# to have a more sophisticated branching structure, for which stack sizes aren't as relevant.

# layers

# textures; this may be against the spirit of the program, and may be prohibitively slow.

1;
