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
    x_grid_size y_grid_size
    scale origin_x origin_y
    device_width device_height
    min_x_grid min_y_grid max_x_grid max_y_grid
    paint_shape
    background_brush bg_line_pen tile_line_pen
    created_grid_keys
    floodfill
    current_tile
    select_area area_start area_tiles deleted_tile
    bitmap
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

# map shape to the triangle facing that anchors that shape
my %Shape_facing = (
    L => 'R',
    T => 'L',
    R => 'R',
    TL => 'L',
    TR => 'R',
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
        [ 1,0,1, 'TL', 1, 
            [
                [  0, 0, 0, 'R', ],
                [  1,-1, 0, 'T', ],
                [  0, 0, 0, 'TR', ],
            ],
        ],
        [ 0,0,0, 'TR', 2,
            [
                [  1, 0, 1, 'R', ],
                [  1, 0, 1, 'T', ],
                [  1, 0, 1, 'TL', ],
            ],
        ],
    ],
    T => [
        [ -1,1,0, 'TR', 1, 
            [
                [  0, 0, 0, 'R', ],
                [ -1, 0,-1, 'L', ],
                [  0, 0, 0, 'TL', ],
            ],
        ],
        [ 0,0,0, 'TL', 2,
            [
                [ -1, 1, 0, 'R', ],
                [ -1, 1, 0, 'L', ],
                [ -1, 1, 0, 'TR', ],
            ],
        ],
    ],
    R => [
        [ 0,0,0, 'TR', 1, 
            [
                [ -1, 0,-1, 'L', ],
                [  0, 0, 0, 'T', ],
                [  0, 0, 0, 'TL', ],
            ],
        ],
        [ 0,0,0, 'TL', 2,
            [
                [  0, 0, 0, 'L', ],
                [  1,-1, 0, 'T', ],
                [  0, 0, 0, 'TR', ],
            ],
        ],
    ],
    TL => [

        # shift_flag set to 3 so any clash stops the paint; can't
        # paint half a triangle.
        [ 0,0,0, 'TR', 3, 
            [
                [ -1, 0,-1, 'L', ],
                [  0, 0, 0, 'T', ],
                [  0, 0, 0, 'R', ],
            ],
        ],
    ],
    TR => [
        [ 0,0,0, 'TL', 3,
            [
                [  0, 0, 0, 'L', ],
                [  1,-1, 0, 'T', ],
                [  0, 0, 0, 'TR', ],
            ],
        ],
    ],
);

my @stash;

################################################################################
# Constructor.
sub new { #{{{1

    my( $class, $parent, $id, $pos, $size) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = [ 200, 200 ]      unless defined $size;

    my $self = $class->SUPER::new( $parent, $id, $pos, $size, 0);

$self->SetBackgroundStyle(1);
$log->info("bs1 = " . $self->GetBackgroundStyle);

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

    $self->SetCursor(wxCROSS_CURSOR);

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

    $self->SetBackgroundStyle(2);
    $self->background_brush(wxLIGHT_GREY_BRUSH);
    $self->bg_line_pen(wxWHITE_PEN);
    $self->tile_line_pen(wxLIGHT_GREY_PEN);
    $self->floodfill(0);

    $self->select_area(0);
    $self->area_tiles([]);
    $self->deleted_tile({});

    # undo stack
    $self->created_grid_keys([]);

    $self->frame($parent);

    return $self;
}

#*******************************************************************************
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
    my $control_gradient = ($self->y_grid_size / 2) / $self->x_grid_size;

    my @anchor = ($min_x * $self->x_grid_size, undef);
    my $type;

    if (abs($point_gradient) < $control_gradient) {
        if ($min_x_is_even) {
#            $log->info("a");
            $anchor[1] = $min_y;
            $type = 'R';
        }
        else {
#            $log->info("b");
            $anchor[1] = $min_y + 0.5;
            $type = 'L';
        }
    }
    else {
        if ($point_gradient > 0) {
            if ($min_x_is_even) {
#            $log->info("c");
                $anchor[1] = $min_y;
                $type = 'L';
            }
            else {
#            $log->info("d");
                $anchor[1] = $min_y + 0.5;
                $type = 'R';
            }
        }
        else {
            if ($min_x_is_even) {
#            $log->info("e");
                $anchor[1] = $min_y + 1;
                $type = 'L';
            }
            else {
#            $log->info("f");
                $anchor[1] = $min_y - 0.5;
                $type = 'R';
            }
        }
    }

    my $top = $min_x;
    my $left = $anchor[1] - $top / 2;
    my $right = $left + $top;

    $anchor[1] *= $self->y_grid_size;

#    $log->info("pg $point_gradient, cg $control_gradient, min_x $min_x, min_y $min_y, key @key, anchor @anchor, type $type");
    $log->debug("find_triangle_coords $left, $top, $right, $type");

    @stash = ($min_x, $min_y, @key, @anchor, $type);

    return ($left, $top, $right, $type);
}

#*******************************************************************************
sub mouse_event_handler { #{{{1
    my ($self, $event_flags, $device_x, $device_y) = @_;

    # $log->debug(sprintf "mouse event: %08x", $event_flags);

    my $refresh;

    my ($logical_x, $logical_y) = ( int(($device_x - $self->origin_x) / $self->scale),
        int(($device_y - $self->origin_y) / $self->scale));

    # shift-ctrled middle button down/up toggles dragging flag
    if ($event_flags & $ME_MIDDLE_BUTTON && $event_flags & $ME_SHIFT_IS_DOWN && $event_flags & $ME_ALT_IS_DOWN) {
        $self->dragging($event_flags & $ME_BUTTON_DOWN);
        $log->debug("dragging now? " . $self->dragging);
        return;
    }

#    $refresh = 1;

    my ($left, $top, $right, $facing) = $self->find_triangle_coords($logical_x, $logical_y);

    # mouse down starts paint mode
    if ($event_flags & $ME_BUTTON_DOWN) {
        $refresh = 1;
        $self->paint_shape($event_flags & $ME_LEFT_BUTTON
                    ? 'L'
                    : $event_flags & $ME_RIGHT_BUTTON
                        ? 'R'
                        : 'T');

        $self->area_start( ($event_flags & $ME_SHIFT_IS_DOWN )
            ? [ $left, $top, $right, $facing ]
            : undef);
    }
    elsif ($event_flags & $ME_BUTTON_UP) {
        $self->paint_shape(0);
        $self->area_tiles([]);

        my @deleted_tiles = keys %{ $self->deleted_tile };
        for my $key (@deleted_tiles) {
            delete $self->frame->scene->{$key};
            $refresh = 1;
        }
        $self->deleted_tile({});
    }

    my $title = "IsoScene";
    if (my $file = $self->frame->last_save_name) {
        $title .= " - $file";
    }
#    $self->current_tile($existing_tile);

    if ($self->paint_shape) {

        if ($event_flags & $ME_CTRL_IS_DOWN) {
            if ($self->area_start) {
                my ($start_left, $start_top, $start_right, $start_facing) = @{ $self->area_start };
                if ($start_facing eq $facing) {
                    $refresh = $self->clear_area($left, $top, $right, $facing, $self->paint_shape);
                }
            }
            else {
                my $grid_key = "${facing}_${left}_${top}_${right}";
                delete $self->frame->scene->{$grid_key};
                $refresh = 1;
            }
        }
#        elsif ($self->floodfill) {
#
#            $self->do_floodfill($x,$y);
#
#            # floodfill is a one-time thing; stop painting now and turn off flag and button
#            $self->paint_shape(0);
#            $self->floodfill(0);
#            $self->frame->set_tool($IsoFrame::ID_FLOODFILL, 0);
#
#            $refresh = 1;
#        }
        else {

            # colour always comes from button clicked
            my $brush_shape = $self->paint_shape;

#            # shift-click to get a triangle; still want the button choice for colour
#            my $shape = ($event_flags & $ME_SHIFT_IS_DOWN)
#                ? "T$facing"
#                : $brush_shape;

            if ($self->area_start) {
                my ($start_left, $start_top, $start_right, $start_facing) = @{ $self->area_start };
                if ($start_facing eq $facing) {
                    $refresh = $self->paint_area($left, $top, $right, $facing, $brush_shape);
                }
            }
            else {

                # let paint_tile do all the transforms and clash checking
                $refresh = $self->paint_tile($left, $top, $right, $facing, $brush_shape);
            }
        }
    }

    $self->frame->SetTitle($title);

    # wheel zooms in/out
    if ($event_flags & ($ME_WHEEL_FORWARD | $ME_WHEEL_BACK)) {
        my $scale = $self->scale;
        if ($event_flags & $ME_WHEEL_FORWARD) {

            # reset to int 1 when coming back up from fractional end of range
            $scale *= 2 if $scale < 4;
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
sub paint_area { #{{{1
    my ($self, $left, $top, $right, $facing, $shape) = @_;

    my ($start_left, $start_top, $start_right, $start_facing) = @{ $self->area_start };

    $log->debug("paint_area $shape from $start_left, $start_top, $start_right, $start_facing to $left, $top, $right, $facing");

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

    my $refresh;

    # clear tiles from previous area paint
    for my $key ( @{ $self->area_tiles } ) {
        delete $self->frame->scene->{$key};
        $refresh = 1;
    }
    $self->area_tiles([]);

    my @point;
    for my $coord_0 ($mins[0] .. $maxes[0]) {
        for my $coord_1 ($mins[1] .. $maxes[1]) {

            $point[ $axes[0] ] = $coord_0;
            $point[ $axes[1] ] = $coord_1;
            $point[ $axes[2] ] = $axis_0_coeff * $coord_0 + $axis_1_coeff * $coord_1; 

            $log->debug("paint_area: paint_tile $shape at @point, $facing");
            my $key = $self->paint_tile(@point, $facing, $shape);
            if ($key) {
                push @{ $self->area_tiles }, $key;
                $refresh = 1;
            }
        }
    }

    return $refresh;
}

#*******************************************************************************
sub clear_area { #{{{1
    my ($self, $left, $top, $right, $facing, $shape) = @_;

    my ($start_left, $start_top, $start_right, $start_facing) = @{ $self->area_start };

    $log->debug("clear_area $shape from $start_left, $start_top, $start_right, $start_facing to $left, $top, $right, $facing");

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

    # clear tiles from previous area clear
    $self->deleted_tile({});

    my @point;
    for my $coord_0 ($mins[0] .. $maxes[0]) {
        for my $coord_1 ($mins[1] .. $maxes[1]) {

            $point[ $axes[0] ] = $coord_0;
            $point[ $axes[1] ] = $coord_1;
            $point[ $axes[2] ] = $axis_0_coeff * $coord_0 + $axis_1_coeff * $coord_1; 

            my $grid_key = "${facing}_$point[0]_$point[1]_$point[2]";
            if ($self->frame->scene->{ $grid_key }) {

                # mark this tile as deleted for the next refresh; the final set of deleted tiles, ie
                # when the mouse button is released, will really be deleted.
                $self->deleted_tile->{$grid_key} = 1;
            }
        }
    }

    return 1;
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
    my ($self, $left, $top, $right, $facing, $shape) = @_;
    $log->debug("paint_tile @_");

    # check all args are defined
    $log->logconfess("bad paint : " . Dumper(\@_)) unless (scalar @_ == scalar grep { defined $_ } @_);

    # change the grid key according to the shape if we're in the wrong triangle;
    if ($shape eq 'L' && $facing eq 'L') {
        $left--;
        $right--;
    }
    elsif ($shape eq 'T' && $facing eq 'R') {
        $left++;
        $top--;
    }

    # just ensure the facing is right, don't bother checking it
    $facing = $Shape_facing{$shape};

    # check for the same shape in the same place; we will just change the color of that tile
    # if we find it.
    my $grid_key = "${facing}_${left}_${top}_${right}";
    if (my $tile = $self->frame->scene->{$grid_key}) {
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

            if (my $tile = $self->frame->scene->{$grid_key}) {
                if ($tile->{shape} eq $clash_shape) {
                    $shifts += $shift_flag;
                    last;
                }
            }

        }
    }

    # record the brush index before we change the shape
    my $brush_index = $self->frame->palette->get_shape_brush($shape);

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
        brush_index => $brush_index,
        left => $left,
        top => $top,
        right => $right,
        type => $facing,
    };

    $log->debug("paint_tile $shape at $left, $top, $right, $facing");

    $self->frame->scene->{$grid_key} = $tile;

    push @{ $self->created_grid_keys }, $grid_key;

    return $grid_key;
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
# Handle window resize; suprisingly little we can do here, since changes of
# scale or offset must trigger repaint.
sub find_logical_size { #{{{1
    my ($self) = @_;

    my ($width, $height) = $self->GetSizeWH;

    $self->device_width($width);
    $self->device_height($height);

    return;
}

#*******************************************************************************
sub find_grid_extents { #{{{1
    my ($self, $dc) = @_;

    $dc->SetUserScale($self->scale, $self->scale);
    $dc->SetDeviceOrigin($self->origin_x, $self->origin_y);

    # we draw the grid in logical coords; we need to know what the logical extents of the canvas are.
    my ($min_x, $max_x) = ( $dc->DeviceToLogicalX(0), $dc->DeviceToLogicalX($self->device_width));
    my ($min_y, $max_y) = ( $dc->DeviceToLogicalY(0), $dc->DeviceToLogicalY($self->device_height));

    # calculate the grid coordinates for an area slightly larger than the window.
    # We force the X coord to an even line (same as the origin) to cope with the uneven nature of the X axis.
    my $min_x_grid = int($min_x / ($self->x_grid_size * 2)) * 2 - 2;
    my $min_y_grid = int($min_y / ($self->y_grid_size)) - 1;
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

#*******************************************************************************
sub repaint_canvas { #{{{1
    my ($self) = @_;


$log->info("bs2 = " . $self->GetBackgroundStyle);
    my $dc = Wx::PaintDC->new( $self );
    my $frame = $self->frame;

    $dc->SetBackground($self->background_brush);
    $dc->Clear;

#    $log->debug("refresh");

    $self->find_grid_extents($dc);

    $self->draw_scene($dc);

#    $dc->SetUserScale(1,1);
#    $dc->SetDeviceOrigin(0,0);
#
#    $dc->DrawBitmap( $self->bitmap->{cube}, 100, 100, 1);

    return;
}

#*******************************************************************************
sub draw_scene { #{{{1
    my ($self, $dc, $no_bg_lines) = @_;

    my $frame = $self->frame;
    my $current_brush = 0;

    $dc->DrawLine(0, 0, 0, $self->y_grid_size);
    $dc->DrawLine(0, 0, $self->x_grid_size, $self->y_grid_size / 2);
    $dc->DrawLine($self->x_grid_size, $self->y_grid_size / 2, 0, $self->y_grid_size);

    for my $left (-5 .. 5) {
        for my $top (-5 .. 5) {
            my $x = $top * $self->x_grid_size;
            my $y = ($left + $top / 2) * $self->y_grid_size;
            my $right = $left + $top;
            $dc->DrawText("$left,$top,$right", $x,$y);
        }
    }

    my @key;
    my @anchor;
    (my $min_x, my $min_y, $key[0], $key[1], $anchor[0], $anchor[1], my $type) = @stash;
    if (defined $min_x) {
#        $log->info("min_x $min_x, min_y $min_y, key @key, anchor @anchor, type $type");
#        $dc->DrawRectangle($min_x * $self->x_grid_size, $min_y * $self->y_grid_size, $self->x_grid_size, $self->y_grid_size);
#        $dc->SetPen(wxRED_PEN);
#        $dc->DrawCircle(@key, 20);
        $dc->SetPen(wxGREEN_PEN);
        $dc->SetBrush(wxTRANSPARENT_BRUSH);
        $dc->DrawCircle(@anchor, 20);
    }

#    return;

    $dc->SetPen($self->tile_line_pen);
    my $brush_list = $frame->palette->brushes;
    my $color_palette = $frame->palette->palette;

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

    for my $tile (values %{ $self->frame->scene }) {
        my $grid_key = join('_', @{ $tile }{qw( type left top right )});

        # filter deleted items when deleting area
        next if $self->deleted_tile->{$grid_key};

        # filter by visible location
        next if $tile->{top} < $self->min_x_grid || $tile->{top} > $self->max_x_grid;
        my $y_grid = ($tile->{left} + $tile->{right}) / 2;
        next if $y_grid < $self->min_y_grid || $y_grid > $self->max_y_grid;

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
            $tile->{top} * $self->x_grid_size,
            ($tile->{left} + $tile->{top} / 2) * $self->y_grid_size,
        );

        $log->debug(sprintf "draw $tile->{shape} $grid_key at %f, %f",
            $tile->{top} * $self->x_grid_size,
            ($tile->{left} + $tile->{top} / 2) * $self->y_grid_size);
    }

    return;
}

#*******************************************************************************
sub export_scene { #{{{1
    my ($self) = @_;

    my $busy = new Wx::BusyCursor;

    # my $dc = Wx::AutoBufferedPaintDC->new( $self );
    my $dc = Wx::PaintDC->new( $self );
    my $frame = $self->frame;

    # we need to know what the logical extents of the scene are.

    # find extents in terms of grid keys
    my ($min_x_grid, $min_y_grid, $max_x_grid, $max_y_grid);
    for my $tile (values %{ $self->frame->scene }) {

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
    $log->info("grid key extents: $min_x_grid,$min_y_grid to $max_x_grid,$max_y_grid");

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

    # create a bitmap for A4 600 dpi
    my ($width, $height) = (4800, 6600);

    $log->info("top left $min_x,$min_y to bottom right $max_x,$max_y");

    # find the scale factors for logical to device width, then use the smaller so we fit
    # both dimensions in.
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
    my $device_origin_x = -$min_x * $export_scale;
    my $device_origin_y = -$min_y * $export_scale;
    $export_dc->SetDeviceOrigin($device_origin_x, $device_origin_y);

    # draw the scene into the memory dc
    $export_dc->SetBackground(wxWHITE_BRUSH);
    $export_dc->Clear;

    $self->draw_scene($export_dc, $min_x, $min_y, $max_x, $max_y, undef, undef, undef, undef, 1);

   # save the bitmap to file
    (my $name = $self->frame->last_save_name || 'new') =~ s/\.isc//;
    $export_bm->SaveFile("$name.png", wxBITMAP_TYPE_PNG);

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
    $log->debug(sprintf("origin %d, %d, origin mod grid %d, %d", $self->origin_x, $self->origin_y, $self->origin_x % $self->x_grid_size, $self->origin_y % $self->y_grid_size));

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

    # my $dc = Wx::AutoBufferedPaintDC->new( $self );
    my $dc = Wx::PaintDC->new( $self );

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

# TODO {{{1
# color manipulation
# new controls
# textures

1;
