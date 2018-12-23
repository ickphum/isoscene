package Iso::BigPalette;

use strict;
use base qw(Wx::PlPopupTransientWindow Class::Accessor::Fast);

use Wx qw(wxSOLID :brush :colour :pen);
use Wx::Event qw(EVT_PAINT);
use Graphics::Color::HSV;
use Graphics::Color::RGB;
use Log::Log4perl qw(get_logger);
use Data::Dumper;

# present a 32x32 (1024) grid of color cells.
# if we step through 60 hues (0 - 354 by sixes) and have a 4x4 saturation/value combination grid for each hue,
# we use 960 colors, which leaves 64 for custom colors.
# For the SV grid, we'll use 100/80/60/40, since below 40 the colors are very washed out/dark resp.

__PACKAGE__->mk_accessors(qw(brushes colors top left right colors saturations values parent));

my $GRID_SIZE = 24;
my $CELLS_PER_ROW = 60;
my $log = get_logger;

sub new {
    my( $class, @args ) = @_;
    $log->info("new BigPalette @args");
    my $self = $class->SUPER::new( @args );

    $self->parent($args[0]);

    $self->saturations( [ 1, 0.8, 0.55, 0.4, ]);
    $self->values ([ 1, 0.75, 0.55, 0.35, 0.25]);

    # if we use a qw() list here we get a warning about "possibly using comments in a qw list",
    # and I can't be bothered working out how to kill that warning and nothing else.
    # These are the fixed colors at the start of the palette for legacy support.
    $self->colors( [
        '#ffffff',
        '#ab75bf',
        '#8222a5',
        '#ffb671',
        '#e89723',
        '#cf5809',
        '#973c00',
        '#3a9516',
        '#25600e',
        '#fffe74',
        '#ffff00',
        '#ff4600',
        '#ff4a4a',
        '#ff0000',
        '#c50000',
        '#830000',
        '#70ff70',
        '#23c523',
        '#006300',
        '#003500',
        '#7efcff',
        '#43befd',
        '#295cdc',
        '#0000ff',
        '#eeeeee',
        '#cccccc',
        '#aaaaaa',
        '#888888',
        '#666666',
        '#444444',
        '#222222',
        '#000000',
    ]);

    # we've got 32 custom colors (at the moment); we're showing rows of 60, so we can have 28 more custom colors,
    # then a row of grayscales.

    # fill up the remainder of the custom row of 60 with gray
    for my $i (1 .. ($CELLS_PER_ROW - scalar @{ $self->colors })) {
        push @{ $self->colors }, '#aaaaaa';
    }

    # grayscale row
    for my $i (0 .. $CELLS_PER_ROW - 1) {
        push @{ $self->colors }, sprintf('#%02x%02x%02x', $i * 4, $i * 4, $i * 4);
    }

    # generate palette and add hex strings to color list
    my $saturations = $self->saturations;
    my $values = $self->values;
    for my $saturation_index (0 .. $#{ $saturations }) {
        for my $value_index (0 .. $#{ $values }) {
            for my $hue_index (0 .. $CELLS_PER_ROW - 1) {

                my $hsv = Graphics::Color::HSV->new({
                    hue => $hue_index * 6,
                    saturation => $saturations->[$saturation_index],
                    value => $values->[$value_index],
                });

                push @{ $self->colors }, $hsv->to_rgb->as_css_hex;
            }
        }
    }
    $log->info("colors; made this many : " . scalar @{ $self->colors });
    $log->debug("colors: " . Dumper($self->colors));

    # generate brushes
    $self->brushes([]);
    for my $cell (0 .. $#{ $self->colors }) {
        $self->brushes->[$cell] = Wx::Brush->new( $self->colors->[$cell], wxSOLID);
    }

    EVT_PAINT( $self, \&on_paint );

    $self->SetSize( $CELLS_PER_ROW * ($GRID_SIZE - 1) + 1, (2 + (scalar @{ $saturations } * scalar @{ $values })) * ($GRID_SIZE - 1) + 1);
    return $self;
}

#sub Popup {
#    my ($self) = @_;
#
#    Wx::PlPopupTransientWindow::Popup($self);
#
#    return;
#}

sub ProcessLeftDown {
    my ($self, $event) = @_;

    my ($x,$y) = ($event->GetX, $event->GetY);

    my $index = int($y / ($GRID_SIZE - 1)) * $CELLS_PER_ROW + int($x / ($GRID_SIZE - 1));
    $log->debug("ProcessLeftDown $x,$y = $index");
    $self->parent->big_palette_selection($index);
    $self->Dismiss unless $event->ShiftDown;
    return 1;
}

sub on_paint {
    my( $self, $event ) = @_;
    my $dc = Wx::PaintDC->new( $self );

    $dc->SetPen( wxBLACK_PEN );

    my $colors = $self->colors;
    for my $cell (0 .. $#{ $colors }) {

        # one wide column
        my $box_left = ($cell % $CELLS_PER_ROW) * ($GRID_SIZE - 1);
        my $box_top = int($cell / $CELLS_PER_ROW) * ($GRID_SIZE - 1);
        $log->debug("draw $cell at $box_left, $box_top");

        $dc->SetBrush($self->brushes->[$cell]);
        $dc->DrawRectangle($box_left,$box_top,$GRID_SIZE,$GRID_SIZE);
    }

    return;
}

1;
