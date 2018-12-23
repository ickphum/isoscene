#$Id: IsoCanvas.pm 118 2010-12-30 13:04:55Z ikm $

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
# use Math::Bezier;
# use Math::CatmullRom;
use Data::Dumper;
use POSIX qw(floor);
use List::Util qw(min max);
use Time::Piece;

__PACKAGE__->mk_accessors(qw(frame dragging last_device_x last_device_y
    scale origin_x origin_y
    x_grid_size y_grid_size
    paint_shape
    background_brush bg_line_pen tile_line_pen
    created_grid_keys
    previous_paint
    floodfill
    select_area
    current_tile
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

################################################################################
# Constructor.
sub new { #{{{1

    my( $class, $parent, $id, $pos, $size) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = [ 200, 200 ]      unless defined $size;

    my $self = $class->SUPER::new( $parent, $id, $pos, $size);

    # non-ctrl accelerators not working in wxPanel?
    main::assign_event_handler($self, 'EVT_CHAR',
        sub {
            my ($listbox, $event) = @_;
            $log->debug("keycode " . $event->GetKeyCode);

            if ($event->GetKeyCode == ord('Q')) {
                $self->frame->Destroy;
            }

            if ($event->GetKeyCode == ord('a')) {
                $self->select_area($self->select_area ? 0 : 1);
                $self->frame->set_tool($IsoFrame::ID_SELECTAREA, $self->select_area);
            }

            if ($event->GetKeyCode == ord('f')) {
                $self->floodfill($self->floodfill ? 0 : 1);
                $self->frame->set_tool($IsoFrame::ID_FLOODFILL, $self->floodfill);
            }

            if ($event->GetKeyCode == ord('C')) {
                $self->frame->start_new_scene;
            }

            if ($event->GetKeyCode == ord('r')) {

                # replace palette color; find the tile under the cursor and replace
                # that shape's current color with the color of the tile.
                if (my $tile = $self->current_tile) {
                    $log->info("current tile : " . Dumper($tile));
                    if ((length $tile->{shape}) == 1) {
                        my $palette = $self->frame->palette;
                        my $shape_pos = $palette->shape_position($tile->{shape});
                        $palette->next_change($shape_pos);
                        $palette->big_palette_selection($tile->{brush_index});
                    }
                }
            }

            return;
        });

    $self->scale(1);
    $self->origin_x(0);
    $self->origin_y(0);
    $self->previous_paint({});

    $self->SetCursor(wxCROSS_CURSOR);

    Wx::Event::EVT_PAINT($self, \&repaint_canvas);
    Wx::Event::EVT_SIZE($self, \&repaint_canvas);

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
    $self->background_brush(wxLIGHT_GREY_BRUSH);
    $self->bg_line_pen(wxWHITE_PEN);
    $self->tile_line_pen(wxLIGHT_GREY_PEN);
    $self->floodfill(0);
    $self->select_area(0);

    # undo stack
    $self->created_grid_keys([]);

    $self->frame($parent);

    return $self;
}

#*******************************************************************************
sub find_triangle_coords { #{{{1
    my ($self, $logical_x, $logical_y) = @_;

    my $y_grid = floor($logical_y / $self->y_grid_size);
    my $major_column = floor($logical_x / $self->x_grid_size);

    my ($x,$y);

    my $control_gradient = ($self->y_grid_size / 2) / $self->x_grid_size;

    if ($major_column % 2 == 0) {

        # the $logical_x test is to stop div/0
        my $point_to_top_gradient = $logical_x ? ($logical_y - ($y_grid * $self->y_grid_size)) / ($logical_x - ($major_column * $self->x_grid_size)) : 1000000;
        my $point_to_bottom_gradient = $logical_x ? ($logical_y - (($y_grid + 1) * $self->y_grid_size)) / ($logical_x - ($major_column * $self->x_grid_size)) : 1000000;

        if ($point_to_top_gradient < $control_gradient) {

            # we're in the bottom of the second triangle in the above row
            $log->debug("bottom second above");
            $x = $major_column * 2 + 1;
            $y = $y_grid - 1;
        }
        else {

            $control_gradient *= -1;

            if ($point_to_bottom_gradient > $control_gradient) {

                # we're in the top of the second triangle in the current row
                $log->debug("top second current");
                $x = $major_column * 2 + 1;
                $y = $y_grid;
            }
            else {

                # we're in the first triangle in the current row
                $log->debug("first current");
                $x = $major_column * 2;
                $y = $y_grid;
            }
        }
    }
    else {

        # the $logical_x test is to stop div/0
        my $point_to_top_gradient = $logical_x ? ($logical_y - ($y_grid * $self->y_grid_size)) / ($logical_x - (($major_column + 1) * $self->x_grid_size)) : 1000000;
        my $point_to_bottom_gradient = $logical_x ? ($logical_y - (($y_grid + 1) * $self->y_grid_size)) / ($logical_x - (($major_column + 1) * $self->x_grid_size)) : 1000000;
        $control_gradient *= -1;

        # this is the same logic as above but with reversed tests, outcomes, etc; 
        # too lazy to find the general solution.
        if ($point_to_top_gradient > $control_gradient) {

            # we're in the bottom of the second triangle in the above row
            $log->debug("bottom first above");
            $x = $major_column * 2;
            $y = $y_grid - 1;
        }
        else {

            $control_gradient *= -1;

            if ($point_to_bottom_gradient < $control_gradient) {

                # we're in the top of the second triangle in the current row
                $log->debug("top first current");
                $x = $major_column * 2;
                $y = $y_grid;
            }
            else {

                # we're in the first triangle in the current row
                $log->debug("last current");
                $x = $major_column * 2 + 1;
                $y = $y_grid;
            }
        }
    }

    $log->debug("x = $x, y = $y");

    return ($x,$y);
}

#*******************************************************************************
sub mouse_event_handler { #{{{1
    my ($self, $event_flags, $device_x, $device_y) = @_;

    # $log->debug(sprintf "mouse event: %08x", $event_flags);

    my $refresh;

    my ($logical_x, $logical_y) = ( int(($device_x - $self->origin_x) / $self->scale),
        int(($device_y - $self->origin_y) / $self->scale));

    # shift-ctrled middle button down/up toggles dragging flag
    if ($event_flags & $ME_MIDDLE_BUTTON && $event_flags & $ME_SHIFT_IS_DOWN && $event_flags & $ME_CTRL_IS_DOWN) {
        $self->dragging($event_flags & $ME_BUTTON_DOWN);
        $log->debug("dragging now? " . $self->dragging);
        return;
    }

    my ($x,$y) = $self->find_triangle_coords($logical_x, $logical_y);
    my $grid_key = "${x}_${y}";

    # mouse down starts paint mode
    if ($event_flags & $ME_BUTTON_DOWN) {
        $self->paint_shape($event_flags & $ME_LEFT_BUTTON
                    ? 'L'
                    : $event_flags & $ME_RIGHT_BUTTON
                        ? 'R'
                        : 'T');
    }
    elsif ($event_flags & $ME_BUTTON_UP) {
        $self->paint_shape(0);
    }

    my $title = "IsoScene";
    if (my $file = $self->frame->last_save_name) {
        $title .= " - $file";
    }
    $title .= " @ $x,$y";
    my $existing_tile = $self->frame->scene->{$grid_key};
    if ($existing_tile) {
        $title .= " = $existing_tile->{shape}$existing_tile->{brush_index}";
    }
    $self->current_tile($existing_tile);
    $self->frame->SetTitle($title);

    if ($self->paint_shape) {

        if ($event_flags & $ME_CTRL_IS_DOWN) {
            delete $self->frame->scene->{$grid_key};
            $refresh = 1;
        }
        elsif ($self->floodfill) {

            $self->do_floodfill($x,$y);

            # floodfill is a one-time thing; stop painting now and turn off flag and button
            $self->paint_shape(0);
            $self->floodfill(0);
            $self->frame->set_tool($IsoFrame::ID_FLOODFILL, 0);

            $refresh = 1;
        }
        elsif ($self->select_area) {

            # no triangles when doing areas, tops must start in odd columns and lefts/rights must not.
            my $valid = ! ($self->paint_shape eq 'T' xor $x % 2);
            if ($valid) {
                $self->do_area($x,$y);
                $self->select_area(0);
                $self->frame->set_tool($IsoFrame::ID_SELECTAREA, 0);
                $self->previous_paint->{ $self->paint_shape } = { x => $x, y => $y, };

                $refresh = 1;
            }
        }
        else {

            # colour always comes from button clicked
            my $brush_shape = $self->paint_shape;

            # shift-click to get a triangle; still want the button choice for colour
            my $shape = ($event_flags & $ME_SHIFT_IS_DOWN)
                ? $x % 2
                    ? 'TL'
                    : 'TR'
                : $brush_shape;

            my $brush_index = $self->frame->palette->get_shape_brush($brush_shape);

            # is this already set?
            if ($existing_tile) {
                if ($existing_tile->{shape} eq $shape && $existing_tile->{brush_index} == $brush_index) {
                    return;
                }
                else {
                    delete $self->frame->scene->{$grid_key};
                }
            }

            # triangles are always valid, tops must start in odd columns and lefts/rights must not.
            my $valid = $shape =~ /T./ || ! ($shape eq 'T' xor $x % 2);

            if ($valid) {
                $self->paint_tile($x, $y, $shape, $brush_shape);
                $self->previous_paint->{ $self->paint_shape } = { x => $x, y => $y, };
                $refresh = 1;
            }
        }

    }

    # wheel zooms in/out
    if ($event_flags & ($ME_WHEEL_FORWARD | $ME_WHEEL_BACK)) {
        my $scale = $self->scale;
        if ($event_flags & $ME_WHEEL_FORWARD) {

            # reset to int 1 when coming back up from fractional end of range
            $scale *= 2;
            $scale = 1 if ($scale > 0.9 && $scale < 1.1);
        }
        elsif ($event_flags & $ME_WHEEL_BACK) {
            $scale /= 2 if $scale > 0.05;
        }
        $self->scale($scale);
        $self->origin_x($device_x - $self->scale * $logical_x);
        $self->origin_y($device_y - $self->scale * $logical_y);
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

    return;
}

#*******************************************************************************
sub do_area { #{{{1
    my ($self, $x, $y) = @_;
    my $grid_key = "${x}_${y}";

    # since we're not doing triangles, paint_shape dictates color and shape
    my $shape = $self->paint_shape;

    # find previous paint for this shape
    return unless my $previous_paint = $self->previous_paint->{ $shape };
    my ($px, $py) = @{$previous_paint}{qw(x y)};

    # no-op to paint to yourself
    return if $x == $px && $y == $py;

    $log->info("area corners are $x,$y, previous $px, $py, shape $shape");

    if ($shape =~ /[LR]/) {

        # we want to paint the area in a series of vertical stripes;
        # to do this we need the top left corner, the width and the height.

        # the point clicked in the left-most column defines the left side but
        # not necessarily the top corner. To find if it does, we must translate the
        # other point up the diagonal line to the same x coordinate and see what the y
        # coord has become. We can skip this if they're equal.
        my (@top_left, @bottom_right, $height);
        if ($x == $px) {
            $log->info("Xs equal");
            $top_left[0] = $bottom_right[0] = $x;
            ($top_left[1], $bottom_right[1]) = $y > $py ? ($py, $y) : ($y, $py);
            $height = $bottom_right[1] - $top_left[1];
        }
        else {

            $log->info("calc top_left/bottom_right");
            # find left and right points
            my (@left, @right);
            if ($x < $px) {
                @left = ($x,$y);
                @right = ($px,$py);
            }
            else {
                @left = ($px,$py);
                @right = ($x,$y);
            }

            # move right to left x coord in major column steps,
            # and by default change y coord by 1 for every 2 steps.

            # these are valid x coords for a L or R tile and therefore must be
            # even, so the difference is also even and can be evenly divided.
            my $steps = ($right[0] - $left[0]) / 2;
            my $y_delta = int($steps/2);

            # if the right corner started in an even major column, moving
            # a single major column will decrement y. If there are not an even number of
            # steps this will not average out and the y delta will be under by 1.
            if ((($right[0] / 2) % 2 xor $shape eq 'L') && $steps % 2) {
                $y_delta++;
            }

            # y change is different sign for R shape
            $y_delta *= -1 if $shape eq 'R';

            my @shifted_right = ($left[0], $right[1] - $y_delta);
            $log->info("y_delta = $y_delta, shifted_right = @shifted_right");

            # we can now compare the y coords of the left point and the
            # shifted right point; if the left is less, left and right are
            # top_left and bottom_right, otherwise we inc left's y by the same dec and 
            # top_left/bottom_right are shifted right and left resp.
            if ($left[1] <= $shifted_right[1]) {
                @top_left = @left;
                @bottom_right = @right;
            }
            else {
                @top_left = @shifted_right;
                @bottom_right = ($right[0], $left[1] + $y_delta);
            }
            $height = abs($left[1] - $shifted_right[1]);
        }
        
        $log->info("top_left = @top_left, bottom_right = @bottom_right, height $height");

        # draw the area
        my ($paint_x, $paint_y) = @top_left;
        my $inc = $shape eq 'L' ? 1 : -1;
        while ($paint_x <= $bottom_right[0]) {
            $log->info("paint_x $paint_x");
            for my $row (0 .. $height) {
                $log->info("paint_y $paint_y, row $row");
                $self->paint_tile($paint_x, $paint_y + $row, $shape, $shape);
            }

            # adjust y for the next column; depends on major column state and shape
            $paint_y += $inc if (($paint_x / 2) % 2) xor $shape eq 'R';

            $paint_x += 2;
        }
    }
    elsif ($shape eq 'T') {
        
        # things are more complicated for T tiles since we have no vertical edge
        # to shift to; we are projecting diagonally from both corners and trying
        # to find the intersection.

        # The basic approach is this:
        #   Identify and deal with the simple cases where (column) or (row AND major column) are equal.
        #   Find the top point.
        #   By comparing the angle from the top point to the lower point,
        #       identify the sides which project toward the lower point.
        #   Project each of these sides the required amount, recognising this
        #       by the angle of the projected point to the second point.
        #   We now have the two side dimensions we need; proceed along one side projecting
        #       the length of the other side.

        # In each case, we're trying to find these variables, which will drive the common
        # drawing code. We will start at the anchor, move as dictated by major_dir and major_len,
        # and draw lines as dictated by minor_dir and minor_len.
        my ($anchor, $major_dir, $major_len, $minor_dir, $minor_len);

        my $major_column = floor($x/2);
        my $prev_major_column = floor($px/2);
        my $major_column_type = ($major_column % 2) ? 'odd' : 'even';
        my $prev_major_column_type = ($prev_major_column % 2) ? 'odd' : 'even';

        #   Identify and deal with the simple cases where (column) or (row AND major column) are equal.
        if ($x == $px) {

            # two points in the same column, so top and bottom corners of a diamond. Anchor at top
            # corner, draw SE & SW same distance as height.
            $anchor = $y < $py ? [$x,$y] : [$px, $py];
            $major_dir = 'SW';
            $minor_dir = 'SE';
            $major_len = $minor_len = abs ($y - $py);
        }
        elsif ($y == $py && $major_column_type eq $prev_major_column_type) {

            # two points in the same row; left and right corners of a diamond. Anchor at left.
            $anchor = $x < $px ? [$x,$y] : [$px, $py];
            $major_dir = 'NE';
            $minor_dir = 'SE';
            $major_len = $minor_len = (abs ($x - $px) / 4);
        }
        else {

            # Find the top point; not trivial if the points have the same y coord but
            # different major columns.
            my (@top, @bottom);
            if (($y == $py && $major_column_type eq 'odd') || $y < $py) {
                @top = ($x,$y);
                @bottom = ($px,$py);
            }
            else {
                @top = ($px,$py);
                @bottom = ($x,$y);
            }
            $log->info("top @top, bottom @bottom");

            # we need the actual points the tiles will be drawn at; the easiest way to get them is to
            # paint the points (the previous one is already done), since the offset calculation
            # is done in paint_tile().
            $self->paint_tile($x, $y, $shape, $shape);

            my $gradient = $self->find_gradient_between_tiles($x,$y,$px,$py);

            my $control_gradient = ($self->y_grid_size / 2) / $self->x_grid_size;
            my @directions = abs($gradient) >= $control_gradient
                ? qw(SW SE)
                : $gradient > 0
                    ? qw(NW SW)
                    : qw(SE NE);
            $log->info("top = @top, bottom = @bottom, g = $gradient, G = $control_gradient, directions = @directions");

            # Project each of these sides the required amount, recognising this
            # by the angle of the projected point to the second point.
            my @lengths;
            for my $direction (@directions) {
                my @points = $self->diagonal_shift(\@top, $direction, \@bottom);
                if (scalar @points == 0) {

                    # if we get back an empty list, it means the bottom point 
                    # was on a true diagonal and we've painted all the correct points
                    # while discovering that, so we're done.
                    return;
                }

                # the points list includes the anchor so the lengths are -1
                $log->info("points going $direction: " . scalar @points);
                push @lengths, $#points;
            }

            # now we can assign major & minor directions and lengths for the paint routine
            ($major_dir, $minor_dir, $major_len, $minor_len) = (@directions, @lengths);
            $anchor = \@top;
        }

        $log->info("anchor $anchor->[0], $anchor->[1], major: $major_dir, $major_len, minor: $minor_dir, $minor_len");

        #   We now have the two side dimensions we need; proceed along major side projecting
        #       the length of the minor side.
        for my $major_point ( $self->diagonal_shift($anchor, $major_dir, $major_len) ) {

            $log->info("major point: $major_point->[0], $major_point->[1]");
            for my $minor_point ($self->diagonal_shift($major_point, $minor_dir, $minor_len) ) {

                # the anchor is included in the shifted points so paint at the second level
                $self->paint_tile( @{ $minor_point }, $shape, $shape);
            }
        }

    }
    else {
        $log->logdie("bad shape $shape");
    }

    return;
}

#*******************************************************************************
# The assumption is that the two pairs refer to coords of the same shape, since
# the gradient returned is that between the anchor points, and different shapes of
# tile have different anchor points.
sub find_gradient_between_tiles { #{{{1
    my ($self,$x1,$y1,$x2,$y2) = @_;

    my $tile1 = $self->frame->scene->{"${x1}_${y1}"}
        or $log->logconfess("no tile at $x1, $y1");
    my $tile2 = $self->frame->scene->{"${x2}_${y2}"}
        or $log->logconfess("no tile at $x2, $y2");

    # die for now
    $log->logconfess("different tile shape : " . Dumper($tile1, $tile2)) unless $tile1->{shape} eq $tile2->{shape};

    my ($anchor1_x, $anchor1_y) = (
        $tile1->{major_column} * $self->x_grid_size + $tile1->{offsets}->[0],
        $tile1->{y} * $self->y_grid_size + $Y_offsets[ $tile1->{offsets}->[1] ]);

    my ($anchor2_x, $anchor2_y) = (
        $tile2->{major_column} * $self->x_grid_size + $tile2->{offsets}->[0],
        $tile2->{y} * $self->y_grid_size + $Y_offsets[ $tile2->{offsets}->[1] ]);

    $log->info("1: x,y $x1,$y1 major_column $tile1->{major_column}, anchor $anchor1_x,$anchor1_y");
    $log->info("2: x,y $x2,$y2 major_column $tile2->{major_column}, anchor $anchor2_x,$anchor2_y");

    # Invert the y dimension to get "natural" values, ie cartesian not window.
    # Order of points is irrelevant.
    # Return a arbitrary big value for vertical lines.
    return ($anchor1_x - $anchor2_x)
        ? -($anchor1_y - $anchor2_y) / ($anchor1_x - $anchor2_x)
        : $anchor1_y < $anchor2_y 
            ? 100000000000000
            : -100000000000000;
}

#*******************************************************************************
# return the set of tile coords found by shifting the anchor tile in the specified direction
# by either a number of tiles or until a target is reached. $anchor is an arrayref, we return a list of arrayrefs.
sub diagonal_shift { #{{{1
    my ($self, $anchor, $dir, $target) = @_;

    $log->info("diagonal_shift: anchor $anchor->[0], $anchor->[1], dir $dir, target $target");

    my $control_gradient = ($self->y_grid_size / 2) / $self->x_grid_size;

    my ($anchor_tile, $simple_tests);
    if (ref $target) {
        $anchor_tile = $self->frame->scene->{"$anchor->[0]_$anchor->[1]"}
            or $log->logconfess("no anchor tile at $anchor->[0] $anchor->[1]");

        # the end conditions may differ depending on the initial gradient
        # between anchor & target.
        my $target_tile = $self->frame->scene->{"$target->[0]_$target->[1]"}
            or $log->logconfess("no target tile at $target->[0] $target->[1]");
        my $gradient = $self->find_gradient_between_tiles(@{ $anchor }, @{ $target });
        $simple_tests = abs($gradient) < $control_gradient ? 1 : 0;
        $log->info("initial gradient $gradient, simple_tests $simple_tests");
    }

    my @points = ($anchor);
    my $index = 1;
    while (1) {
        my $prev_point = $points[$#points];
        my $prev_major_column_odd = floor($prev_point->[0]/2) % 2;
        my $next_point = [ 
            $prev_point->[0]
                + ($dir =~ /E/
                    ? 2
                    : -2),
            $prev_point->[1]
                + ( ! $prev_major_column_odd 
                    ? $dir =~ /S/
                        ? 1
                        : 0
                    : $dir =~ /N/
                        ? -1
                        : 0),
        ];
        $log->info("prev_point $prev_point->[0], $prev_point->[1], prev_major_column_odd $prev_major_column_odd, next_point $next_point->[0], $next_point->[1]");

        if (ref $target) {

            # easy finish condition; if we've reached the target (which means the target
            # was on a true diagonal from the anchor):
            # a) we're done, and
            # b) no other filling need be done
            if ($next_point->[0] == $target->[0] && $next_point->[1] == $target->[1]) {
                @points = ();
                last;
            }

            # paint the tile to obtain precise location, so we can find the gradient to the target.
            $self->paint_tile(@{ $next_point }, $anchor_tile->{shape}, $anchor_tile->{shape});

            my $gradient = $self->find_gradient_between_tiles(@{ $next_point }, @{ $target });
            $log->info("gradient is $gradient from $target->[0], $target->[1] to $next_point->[0]_$next_point->[1]");

            if (($dir eq 'NE' && $gradient < -$control_gradient)
                || ($dir eq 'NW' && $gradient > $control_gradient)
                || ($dir eq 'SE' && (! $simple_tests && $gradient >= 0 && $gradient < $control_gradient)
                    || ($simple_tests && $gradient > $control_gradient))
                || ($dir eq 'SW' && (! $simple_tests && $gradient <= 0 && $gradient > -$control_gradient)
                    || ($simple_tests && $gradient < -$control_gradient)))
            {
                $log->info("remove failing tile and stop");

                # the gradient has gone over the limit so we've gone past the target cell, 
                # so remove the tile we just painted and quit.
                delete $self->frame->scene->{"$next_point->[0]_$next_point->[1]"};

                last;
            }

#            last if $index > 10;

        }
        else {
            last if $index > $target;
        }
        $index++;

        push @points, $next_point;
    }

    $log->info("diagonal_shift anchor $anchor->[0], $anchor->[1], dir $dir, target $target returns: " . Dumper(\@points));
    return @points;
}

#*******************************************************************************
sub do_floodfill { #{{{1
    my ($self, $x, $y) = @_;
    my $grid_key = "${x}_${y}";

    $log->info("flood around $x,$y with " . $self->paint_shape);

    # since we're not doing triangles, paint_shape dictates color and shape
    my $shape = $self->paint_shape;

    # is this a valid location for the shape? This test is copied from below
    # with the triangle test dropped. We're assuming that all neighbours
    # are similarly valid, which they will be if our code for finding neighbours
    # is correct.
    if ( ! ($shape eq 'T' xor $x % 2) ) {

        # prime the flood list with this location
        my @flood_locations = ( [ $x,$y ] );

        # we need to track locations we've already checked
        my %checked_grid_key;

        # shortcut to scene hash
        my $scene = $self->frame->scene;

        # we just painted this locations; all other locations will be ignored
        # if they're already painted, so clear this location so it fits the pattern.
        delete $scene->{$grid_key};

        # offsets to other triangle in shape, keyed by shape & MC type
        my %other_triangle_offsets = (
            T => {
                even => [ 1, 0 ],
                odd => [ 1, 0 ],
            },
            L => {
                even => [ 1, 0 ],
                odd => [ 1, 1 ],
            },
            R => {
                even => [ 1, -1 ],
                odd => [ 1, 0 ],
            },
        );

        # offsets to neighbouring shapes, keyed by shape & MC type.
        # Each combination has a list of 4 offset pairs; I haven't tried to find any patterns here.
        my %neighbour_offsets = (
            T => {
                even => [ [ -2, 0 ], [ -2, 1 ], [ 2, 0 ], [ 2, 1 ], ],
                odd => [ [ -2, 0 ], [ -2, -1 ], [ 2, 0 ], [ 2, -1 ], ],
            },
            L => {
                even => [ [ 0, 1 ], [ 0, -1 ], [ 2, 0 ], [ -2, -1 ], ],
                odd => [ [ 0, 1 ], [ 0, -1 ], [ -2, 0 ], [ 2, 1 ], ],
            },
            R => {
                even => [ [ 0, 1 ], [ 0, -1 ], [ -2, 0 ], [ 2, -1 ], ],
                odd => [ [ 0, 1 ], [ 0, -1 ], [ 2, 0 ], [ -2, 1 ], ],
            },
        );

        # insurance...
        my $count = 0;
        my $flood_limit = 500;

        while (@flood_locations && $count < $flood_limit) {
            my $location = shift @flood_locations;
            my ($this_x, $this_y) = @{ $location };

            my $grid_key = "${this_x}_${this_y}";

            # we do the checks here in two parts so that we eliminate locations quickly where possible,
            # ie by checking hashes.
            
            # is location unchecked & unpainted (both triangles)? A location can be added twice, hence
            # can be checked and in the list.
            $log->debug("first check for $grid_key : checked " . ($checked_grid_key{$grid_key} ? 1 : 0) . ', painted ' . ($scene->{$grid_key} ? 1 : 0) );
            unless ($checked_grid_key{$grid_key} || $scene->{$grid_key}) {

                # is other triangle also unpainted?
                my $major_column_type = (floor($this_x/2) % 2) ? 'odd' : 'even';
                my @offsets = @{ $other_triangle_offsets{$shape}->{$major_column_type} };
                my $other_triangle_grid_key = sprintf "%d_%d", $this_x + $offsets[0], $this_y + $offsets[1];
                $log->debug("second check at $other_triangle_grid_key from @offsets : " . ($scene->{$other_triangle_grid_key} ? 1 : 0));
                unless ($scene->{$other_triangle_grid_key}) {

                    # paint
                    $self->paint_tile($this_x, $this_y, $shape, $shape);
                    $log->debug("flood paint at $this_x, $this_y");

                    # mark as checked
                    $checked_grid_key{$grid_key} = 1;

                    # add unchecked neighbours to flood list
                    foreach my $neighbour ( @{ $neighbour_offsets{$shape}->{$major_column_type} } ) {
                        my ($x_offset, $y_offset) = @{ $neighbour };
                        push @flood_locations, [ $this_x + $x_offset, $this_y + $y_offset ];
                    }

                    $count++;
                }
            }
        }
        $log->info("flood capped at $flood_limit") if $count == $flood_limit;
            
    }
    else {
        $log->info("flood from invalid location for shape");
    }

    return; 
} 

#*******************************************************************************
sub paint_tile { #{{{1
    my ($self, $x, $y, $tile_shape, $brush_shape) = @_;

    # check all args are defined
    $log->logconfess("bad paint : " . Dumper(\@_)) unless (scalar @_ == scalar grep { defined $_ } @_);

    my $grid_key = "${x}_${y}";
    my $tile = {
        shape => $tile_shape,
        x => $x,
        major_column => floor($x / 2),
        y => $y,
        brush_index => $self->frame->palette->get_shape_brush($brush_shape),
        offsets => [ 0, ],
    };

    $log->debug("paint_tile $tile_shape at $x,$y");

    # y offset is dependent on major column type and shape; it's expressed in 
    # units of half the Y grid size.
    my $major_column_type = $tile->{major_column} % 2 ? 'odd' : 'even';
    my $y_offset = {
        L => {
            even => 0,
            odd => 1
        },
        R => {
            even => 2,
            odd => 3
        },
        T => {
            even => 2,
            odd => 1,
        },
        TR => {
            even => 0,
            odd => 1,
        },
        TL => {
            even => 2,
            odd => 1,
        },
    };
    $tile->{offsets}->[1] = $y_offset->{$tile_shape}->{$major_column_type};
    $self->frame->scene->{$grid_key} = $tile;
    push @{ $self->created_grid_keys }, $grid_key;

    return;
}
#*******************************************************************************
sub calculate_grid_dims { #{{{1
    my ($self, $side) = @_;

    $self->x_grid_size( ( cos (deg2rad(30)) * ($side / 1)) );
    $self->y_grid_size( $side );

    # y offsets are expressed in units of half the y grid size, eg 0 = 0, 1 = ygs/2, 2 = ygs, 3 = ygs * 1.5.
    # (no other values are valid except 0,1,2,3).
    # Build an array to hold these values so we don't calculate them for each tile.
    @Y_offsets = (0, $self->y_grid_size / 2, $self->y_grid_size, $self->y_grid_size * 1.5 ); 

    $log->debug(sprintf "xgs %s, ygs %s", $self->x_grid_size, $self->y_grid_size);

    return;
}

#*******************************************************************************
sub find_grid_extents { #{{{1
    my ($self, $dc, $minimal) = @_;
    $dc->SetUserScale($self->scale, $self->scale);
    $dc->SetDeviceOrigin($self->origin_x, $self->origin_y);

    # we draw the grid in logical coords; we need to know what the logical extents of the canvas are.
    my ($width, $height) = $self->GetSizeWH;
    my ($min_x, $max_x) = ( $dc->DeviceToLogicalX(0), $dc->DeviceToLogicalX($width));
    my ($min_y, $max_y) = ( $dc->DeviceToLogicalY(0), $dc->DeviceToLogicalY($height));
#    $log->info("screen logical coords: $min_x,$min_y to $max_x,$max_y");

    # move mins & maxes to grid lines in one of two ways
    if ($minimal) {

        # fill inside edges
        $min_x += ($self->x_grid_size - ($min_x % $self->x_grid_size));
        $min_y += ($self->y_grid_size - ($min_y % $self->y_grid_size));
        $max_x -= $max_x % $self->x_grid_size;
        $max_y -= $max_y % $self->y_grid_size;
    }
    else {

        # fill to/past edges
        $min_x = (int($min_x / $self->x_grid_size) - 1) * $self->x_grid_size;
        $min_y -= $min_y % $self->y_grid_size + $self->y_grid_size;
        $max_x = (int($max_x / $self->x_grid_size) + 1) * $self->x_grid_size;
        $max_y += ($self->y_grid_size - ($max_y % $self->y_grid_size));
    }

    # what indexes can we display?
    my ($min_x_index, $max_x_index) = ($min_x / $self->x_grid_size * 2, $max_x / $self->x_grid_size * 2);
    my ($min_y_index, $max_y_index) = ($min_y / $self->y_grid_size, $max_y / $self->y_grid_size);
    $log->debug("index extents: $min_x_index, $min_y_index to $max_x_index, $max_x_index");

    return ($min_x, $max_x, $min_y, $max_y, $min_x_index, $max_x_index, $min_y_index, $max_y_index);
}

#*******************************************************************************
sub repaint_canvas { #{{{1
    my ($self) = @_;

    my $dc = Wx::AutoBufferedPaintDC->new( $self );
    my $frame = $self->frame;

    $dc->SetBackground($self->background_brush);
    $dc->Clear;

#    $log->debug("refresh");

    my ($min_x, $max_x, $min_y, $max_y, $min_x_index, $max_x_index, $min_y_index, $max_y_index) = $self->find_grid_extents($dc);

    $self->draw_scene($dc, $min_x, $min_y, $max_x, $max_y, $min_x_index, $max_x_index, $min_y_index, $max_y_index);

    return;
}

#*******************************************************************************
sub draw_scene { #{{{1
    my ($self, $dc, $min_x, $min_y, $max_x, $max_y, $min_x_index, $max_x_index, $min_y_index, $max_y_index, $no_bg_lines) = @_;

    my $frame = $self->frame;
    my $current_brush = 0;
    $dc->SetPen($self->bg_line_pen);

    my $rise_fall = ($max_x - $min_x) * tan(deg2rad(30));
    $log->debug("min $min_x, $min_y  max $max_x, $max_y rf $rise_fall");

    my ($x,$y) = ($min_x,$min_y - (int($rise_fall / $self->y_grid_size) + 1) * $self->y_grid_size);
    if ((int($min_x / $self->x_grid_size) % 2) == 1) {
        $y -= $self->y_grid_size >> 1;
        $log->debug("adjust y to $y");
    }

    unless ($no_bg_lines) {

        while (($y - $rise_fall) < $max_y) {

            $dc->DrawLine($min_x, $y, $max_x, $y + $rise_fall);
            $dc->DrawLine($min_x, $y, $max_x, $y - $rise_fall);

            $y += $self->y_grid_size;
        }

        while ($x < $max_x) {

            $dc->DrawLine($x, $min_y, $x, $max_y);

            $x += $self->x_grid_size;
        }
    }

    # shapes are converted in Wx::Point lists for use by DrawPolygon on first use.
    my $tile_shape = {
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
                [ $self->x_grid_size, - $self->y_grid_size / 2 ],
                [ $self->x_grid_size * 2, 0 ],
                [ $self->x_grid_size, $self->y_grid_size / 2 ],
            ],
        },
        R => {
            points => [
                [ 0, 0 ],
                [ 0, - $self->y_grid_size ],
                [ $self->x_grid_size, - $self->y_grid_size * 1.5 ],
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

    $dc->SetPen($self->tile_line_pen);
    my $brush_list = $frame->palette->brushes;
    my $color_palette = $frame->palette->palette;

    for my $tile (values %{ $self->frame->scene }) {

        next if defined $min_x_index && $tile->{x} < $min_x_index;
        next if defined $max_x_index && $tile->{x} > $max_x_index;
        next if defined $min_y_index && $tile->{y} < $min_y_index;
        next if defined $max_y_index && $tile->{y} > $max_y_index;

#        $dc->SetBrush($frame->palette->brushes->[ $frame->palette->palette->[ $tile->{brush_index} ] ]);
#        $dc->SetBrush($brush_list->[ $color_palette->[ $tile->{brush_index} ] ]);
        $dc->SetBrush($brush_list->[ $tile->{brush_index} ]);

        my $shape = $tile_shape->{ $tile->{shape} }
            or $log->logdie("bad tile shape $tile->{shape}");

        unless ($shape->{polygon_points}) {
            $shape->{polygon_points} = [];
            for my $point (@{ $shape->{points} }) {
                push @{ $shape->{polygon_points} }, Wx::Point->new(@{ $point });
            }
        }

        $dc->DrawPolygon(
            $shape->{polygon_points}, 
            $tile->{major_column} * $self->x_grid_size + $tile->{offsets}->[0],
            $tile->{y} * $self->y_grid_size + $Y_offsets[ $tile->{offsets}->[1] ],
        );
    }

#    my $label_offset = {
#        0 => [ 3, 30 ],
#        1 => [ $self->x_grid_size - 40, $self->y_grid_size / 2 + 30 ],
#        2 => [ 3, $self->y_grid_size / 2 + 30 ],
#        3 => [ $self->x_grid_size - 40, 30 ],
#    };
#
#    for $x (-10 .. 10) {
#        my $major_column = floor($x / 2);
#        my $offset = $label_offset->{ $x % 4 };
#        my $major_column_type = (floor($x/2) % 2) ? 'odd' : 'even';
#        for $y (-10 .. 10) {
#            $dc->DrawText("$x,$y", $major_column * $self->x_grid_size + $offset->[0], $y * $self->y_grid_size + $offset->[1]);
#            $dc->DrawText("$major_column_type", $major_column * $self->x_grid_size + $offset->[0], $y * $self->y_grid_size + $offset->[1] + $self->y_grid_size * 0.2);
#        }
#    }

    return;
}

#*******************************************************************************
sub export_scene { #{{{1
    my ($self) = @_;

    my $dc = Wx::AutoBufferedPaintDC->new( $self );
    my $frame = $self->frame;

    # we need to know what the logical extents of the scene are.

    # find extents in terms of grid keys
    my ($min_x_grid, $min_y_grid, $max_x_grid, $max_y_grid);
    for my $grid_key (keys %{ $self->frame->scene }) {
        if ($grid_key =~ /(.+)_(.+)/) {
            my ($x,$y) = ($1,$2);

            unless (defined $min_x_grid) {
                $min_x_grid = $x;
                $max_x_grid = $x;
                $min_y_grid = $y;
                $max_y_grid = $y;
            }

            $min_x_grid = min($min_x_grid, $x);
            $max_x_grid = max($max_x_grid, $x);
            $min_y_grid = min($min_y_grid, $y);
            $max_y_grid = max($max_y_grid, $y);
        }
        else {
            $log->logdie("bad grid_key: $grid_key");
        }
    }
    $log->info("grid key extents: $min_x_grid,$min_y_grid to $max_x_grid,$max_y_grid");

    # add margin so it's consistent wrt number of empty grid rows/cols
    # around image
    $min_x_grid -= 2;
    $min_y_grid -= 2;
    $max_x_grid += 2;
    $max_y_grid += 2;

    # extents in logical coords; recall that each pair of grid columns overlaps
    my $min_x = $min_x_grid * $self->x_grid_size / 2;
    my $max_x = $max_x_grid * $self->x_grid_size / 2;
    my $min_y = $min_y_grid * $self->y_grid_size;
    my $max_y = $max_y_grid * $self->y_grid_size;

    # create a bitmap for A4 600 dpi
    my ($width, $height) = (4800, 6600);

    $log->info("export: image size $width x $height, $min_x,$min_y to $max_x, $max_y");

    # find the scale factors for logical to device width, then use the
    my $x_scale = $width / ($max_x - $min_x);
    my $y_scale = $height / ($max_y - $min_y);
    my $export_scale = min($x_scale, $y_scale);
    $log->info("export_scale from $x_scale, $y_scale : $export_scale");

    # create a bitmap to hold the exported image
    my $export_bm = Wx::Bitmap->new($width, $height, 24);
    $log->info("bitmap ok? : " . $export_bm->IsOk);

    # create a dc to draw the image into and link the bitmap to it
    my $export_dc = Wx::MemoryDC->new();
    $export_dc->SelectObject($export_bm);
    $log->info("memory dc ok? : " . $export_dc->IsOk);

    $export_dc->SetUserScale($export_scale, $export_scale);

    # set device origin to top left
    my $device_origin_x = -($min_x/($max_x - $min_x) * 4800);
    my $device_origin_y = -($min_y/($max_y - $min_y) * 6600);
    $export_dc->SetDeviceOrigin($device_origin_x, $device_origin_y);

    # draw the scene into the memory dc
    $export_dc->SetBackground(wxWHITE_BRUSH);
    $export_dc->Clear;

    $self->draw_scene($export_dc, $min_x, $min_y, $max_x, $max_y, undef, undef, undef, undef, 1);

   # save the bitmap to file
    (my $name = $self->frame->last_save_name || 'new') =~ s/\.isc//;
    $export_bm->SaveFile("$name.png", wxBITMAP_TYPE_PNG);
#    $export_bm->SaveFile("test.jpg", wxBITMAP_TYPE_JPEG);

    $log->info("export done");

    return;
}

#*******************************************************************************
sub reset_scale { #{{{1
    my ($self) = @_;

    $self->scale(1);
    $self->origin_x(0);
    $self->origin_y(0);
    $self->Refresh;

    return;
}

#*******************************************************************************
sub undo_last_create { #{{{1
    my ($self) = @_;

    if (my $grid_key = pop @{ $self->created_grid_keys }) {
        delete $self->frame->scene->{$grid_key};
        $self->Refresh;
    }

    return;
}

#*******************************************************************************
sub move_origin { #{{{1
    my ($self, $device_x, $device_y) = @_;

    $self->origin_x ( $self->origin_x - ($self->last_device_x - $device_x));
    $self->origin_y ( $self->origin_y - ($self->last_device_y - $device_y));

    return;
}

#*******************************************************************************
sub change_background { #{{{1
    my ($self, $rgb) = @_;

    unless ($rgb) {

        my $data = Wx::ColourData->new;
        $data->SetChooseFull( 1 );
        $data->SetColour($self->background_brush->GetColour);

        my $dialog = Wx::ColourDialog->new( $self, $data );

        if ($dialog->ShowModal != wxID_CANCEL) {
            my $data = $dialog->GetColourData;
            my $colour = $data->GetColour;
            $rgb = sprintf "#%02x%02x%02x", $colour->Red, $colour->Green, $colour->Blue;
        }

        $dialog->Destroy;
    }

    return unless $rgb;

    $self->background_brush(Wx::Brush->new("$rgb", wxSOLID));
    $self->Refresh;

    return;
}

#*******************************************************************************
sub change_bg_line_colour { #{{{1
    my ($self, $rgb) = @_;

    if (my $pen = $self->get_pen_color($rgb, $self->bg_line_pen)) {
        $self->bg_line_pen($pen);
        $self->Refresh;
    }

    return;
}

#*******************************************************************************
sub change_tile_line_colour { #{{{1
    my ($self, $rgb) = @_;

    if (my $pen = $self->get_pen_color($rgb, $self->tile_line_pen)) {
        $self->tile_line_pen($pen);
        $self->Refresh;
    }

    return;
}

#*******************************************************************************
sub get_pen_color { #{{{1
    my ($self, $rgb, $pen) = @_;

    unless ($rgb) {

        my $data = Wx::ColourData->new;
        $data->SetChooseFull( 1 );
        $data->SetColour($pen->GetColour);

        my $dialog = Wx::ColourDialog->new( $self, $data );

        if ($dialog->ShowModal != wxID_CANCEL) {
            my $data = $dialog->GetColourData;
            my $colour = $data->GetColour;
            $rgb = sprintf "#%02x%02x%02x", $colour->Red, $colour->Green, $colour->Blue;
        }

        $dialog->Destroy;
    }

    return $rgb ? Wx::Pen->new("$rgb", 1, wxSOLID) : undef;
}

#*******************************************************************************
sub replace_color { #{{{1
    my ($self, $old_color, $new_color, $current_view_only) = @_;

    $log->info("replace_color: $old_color, $new_color, $current_view_only");

    my $dc = Wx::AutoBufferedPaintDC->new( $self );

    my ($min_x, $max_x, $min_y, $max_y, $min_x_index, $max_x_index, $min_y_index, $max_y_index) = $self->find_grid_extents($dc, 1);

    for my $tile (values %{ $self->frame->scene }) {

        next unless $tile->{brush_index} == $old_color;

        if ($current_view_only) {
            next unless $tile->{x} >= $min_x_index && $tile->{x} <= $max_x_index
                && $tile->{y} >= $min_y_index && $tile->{y} <= $max_y_index;
        }

        $tile->{brush_index} = $new_color;
    }

    $self->Refresh;

    return;
}

################################################################################

1;
