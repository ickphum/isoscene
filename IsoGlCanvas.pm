#$Id: IsoGlCanvas.pm 169 2013-03-02 05:27:05Z ikm $

use strict;

use Wx qw[:everything];
# use Wx::RegionIterator;
use Log::Log4perl qw(get_logger);
use List::MoreUtils;
use Storable qw(dclone);

my $log = get_logger;

package IsoGlCanvas;
use OpenGL qw(:glconstants :glfunctions :glufunctions GL_FLOAT);
use OpenGL::Image;
use base qw(Wx::GLCanvas Class::Accessor::Fast);
use Wx qw[:everything];
use Math::Trig;
use Data::Dumper;
use POSIX qw(floor ceil);
use List::Util qw(min max);
use Time::Piece;
use Time::HiRes qw(gettimeofday tv_interval);
use Math::Geometry::Planar;

# accessors {{{1

#    min_x_grid min_y_grid max_x_grid max_y_grid
__PACKAGE__->mk_accessors(qw(frame dragging last_device_x last_device_y
    x_grid_size y_grid_size control_gradient
    device_width device_height
    current_view
    paint_shape
    background_color bg_line_color 
    floodfill
    current_location current_grid_key
    area_start area_tiles select_toggle 
    
    transient_key 

    cursor_multiplier_x cursor_multiplier_y action_cursor

    _scene palette palette_index brush_index

    paste_list

    grid transient_grid key_hash visible_is_new

    render_group texture_id init
    ));

# globals {{{1

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
        [ -1,0,-1, $SH_TRIANGLE_LEFT, 1, 
            [
                [  0,  0, 0, $SH_RIGHT, ],
                [  0, -1,-1, $SH_TOP, ],
                [  0,  0, 0, $SH_TRIANGLE_RIGHT, ],
            ],
        ],
        [ 0,0,0, $SH_TRIANGLE_RIGHT, 2,
            [
                [ -1, 0,-1, $SH_RIGHT, ],
                [ -1, 0,-1, $SH_TOP, ],
                [ -1, 0,-1, $SH_TRIANGLE_LEFT, ],
            ],
        ],
    ],
    T => [
        [ 0,1,1, $SH_TRIANGLE_RIGHT, 1, 
            [
                [  0, 0, 0, $SH_RIGHT, ],
                [  1, 0, 1, $SH_LEFT, ],
                [  0, 0, 0, $SH_TRIANGLE_LEFT, ],
            ],
        ],
        [ 0,0,0, $SH_TRIANGLE_LEFT, 2,
            [
                [ 0, 1, 1, $SH_RIGHT, ],
                [ 0, 1, 1, $SH_LEFT, ],
                [ 0, 1, 1, $SH_TRIANGLE_RIGHT, ],
            ],
        ],
    ],
    R => [
        [ 0,0,0, $SH_TRIANGLE_RIGHT, 1, 
            [
                [  1, 0, 1, $SH_LEFT, ],
                [  0, 0, 0, $SH_TOP, ],
                [  0, 0, 0, $SH_TRIANGLE_LEFT, ],
            ],
        ],
        [ 0,0,0, $SH_TRIANGLE_LEFT, 2,
            [
                [  0, 0, 0, $SH_LEFT, ],
                [  0,-1,-1, $SH_TOP, ],
                [  0, 0, 0, $SH_TRIANGLE_RIGHT, ],
            ],
        ],
    ],
    TL => [

        # shift_flag set to 3 so any clash stops the paint; can't
        # paint half a triangle.
        [ 0,0,0, $SH_TRIANGLE_RIGHT, 3, 
            [
                [  1, 0, 1, $SH_LEFT, ],
                [  0, 0, 0, $SH_TOP, ],
                [  0, 0, 0, $SH_RIGHT, ],
                [  0, 0, 0, $SH_TRIANGLE_LEFT, ],
            ],
        ],
    ],
    TR => [
        [ 0,0,0, $SH_TRIANGLE_LEFT, 3,
            [
                [  0, 0, 0, $SH_LEFT, ],
                [  0, 0, 0, $SH_RIGHT, ],
                [  0,-1,-1, $SH_TOP, ],
                [  0, 0, 0, $SH_TRIANGLE_RIGHT, ],
            ],
        ],
    ],
);

# the offsets from the anchor for each shape to the anchors of the other shapes
# when they are painted to form a cube.
my $Cube_side_offset = {
    L => {
        T => [ 0, 0, 0 ],
        R => [ -1, 1, 0 ],
    },
    T => {
        L => [ 0, 0, 0, ],
        R => [ -1, 1, 0 ],
    },
    R => {
        L => [ 1, -1, 0 ],
        T => [ 1, -1, 0 ],
    },
};

# extent of each shape relative to its anchor, in multiples
# of the relevant grid size.
my $Shape_extent = {
    L => {
        min_y => 1.5,
        max_x => 1,
        max_y => 0,
    },
    T => {
        min_y => 0.5,
        max_x => 2,
        max_y => 0.5,
    },
    R => {
        min_y => 1,
        max_x => 1,
        max_y => 0.5,
    },
    TL => {
        min_y => 0.5,
        max_x => 1,
        max_y => 0.5,
    },
    TR => {
        min_y => 1,
        max_x => 1,
        max_y => 0,
    },
};

# This structure defines how to draw each type of tile; it's loaded
# once the grid size is defined.
my $Tile_shape;
my $DC_tile_shape;

# same shapes but scaled and translated for the tick mark area for each shape
my $Tick_shape;

my @Triangles;

################################################################################
# Constructor.
sub new { #{{{1

    my( $class, $parent, $scene, $id, $pos, $size) = @_;
    $parent = undef              unless defined $parent;
    $id     = -1                 unless defined $id;
    $pos    = wxDefaultPosition  unless defined $pos;
    $size   = wxDefaultSize      unless defined $size;

    my $self = $class->SUPER::new( $parent, $id, $pos, $size, 0);

    # if this canvas is in the export frame (so we can resize it to the correct resolution for export),
    # get all the frame information from the real frame.
    $self->frame("$parent" =~ /ScrolledWindow/ ? $parent->GetParent : $parent);

    $self->render_group({});
    for my $group (qw(permanent new transient select)) {
        $self->init_render_group($group);
    }

    $self->transient_grid({});
    $self->key_hash({});

    $self->calculate_grid_dims(57.735);

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

    # add move as pseudo-action as it has a separate cursor
    $self->action_cursor->{move} = Wx::Cursor->new($app->bitmap->{move_on}->ConvertToImage);

    $self->set_cursor;
    $self->cursor_multiplier_x(1);
    $self->cursor_multiplier_y(1);
    $self->last_device_x(0);
    $self->last_device_y(0);

    Wx::Event::EVT_SIZE($self, \&Resize);
    Wx::Event::EVT_PAINT($self,
        sub {
            my $dc = Wx::AutoBufferedPaintDC->new( $self );
            $self->Render( );
        });
#    Wx::Event::EVT_IDLE($self, sub { $self->Refresh; });

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

            $self->mouse_event_handler($event_flags, $event->GetX, $self->GetSize->GetHeight - $event->GetY);

            # have to skip to get EVT_LEAVE_WINDOW as well
            $event->Skip;
        }
    );

    $self->SetBackgroundStyle(wxBG_STYLE_CUSTOM);

    my $dim = 2;
    for my $row (0 .. $dim) {
        for my $col (0 .. $dim) {
            push @Triangles, [ $col, $row ];
        }
    }

    # not working for me...
#    my @vbo = glGenBuffersARB_p(1);
#    $log->info("vbo @vbo");
#    die "unable to allocate VBOs\n" if (!scalar(@vbo) || !$vbo[0]);
#    $self->tile_va->bind(1);
#    glBufferDataARB_p(GL_ARRAY_BUFFER_ARB, $self->tile_va, GL_STATIC_DRAW_ARB);

    return $self;
}

################################################################################

sub InitGL { # {{{1
    my $self = shift;

    return if $self->init;
    return unless $self->GetContext;
    $self->init( 1 );

    my @textures = qw(texture font_trans select font_black);

    my @texture_ids = glGenTextures_p(scalar @textures);
    $self->texture_id({});

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
#    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
    glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
    glEnable( GL_TEXTURE_2D );

    for my $index (0 .. $#textures) {

        my $texture_name = $textures[$index];
        my $texture_path = "images/$texture_name.tga";
        $log->logdie("no texture at $texture_path")
            unless -f $texture_path;
        $log->info("load texture from $texture_path");
        my $img = new OpenGL::Image( source => $texture_path);
        my @image_info = $img->Get(qw(gl_internalformat width height gl_format gl_type));
        $log->info("image_info $texture_name @image_info");
        my $Tex_Pixels = $img->GetArray();

        glBindTexture(GL_TEXTURE_2D, $texture_ids[$index]);
        $self->texture_id->{$texture_name} = $texture_ids[$index];

        # The GLU library helps us build MipMaps for our texture.
        if ((my $gluerr = gluBuild2DMipmaps_c(GL_TEXTURE_2D, @image_info, $Tex_Pixels->ptr()))) {
            $log->logdie("GLULib%s\n" . gluErrorString($gluerr));
        }

    }
    glDisable( GL_TEXTURE_2D );

    # clockwise winding = front face
#    glFrontFace(GL_CW);
#
#    glDepthFunc( GL_LESS );
#    glEnable( GL_DEPTH_TEST );
    glEnable( GL_BLEND );
#    glEnable( GL_NORMALIZE );
#
#    # smooth is the default
##    glShadeModel(GL_FLAT);
#
#    glEnable( GL_LIGHTING );
##    glLightModelfv_p(GL_LIGHT_MODEL_AMBIENT, 1, 1, 1, 1);
#    glEnable(GL_COLOR_MATERIAL);
#    glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
#
#    glLightfv_p(GL_LIGHT0,GL_AMBIENT, 0.3, 0.3, 0.3, 1);
#    glLightfv_p(GL_LIGHT0,GL_DIFFUSE, 0.9, 0.9, 0.9, 1);
#
#    glMaterialfv_p(GL_FRONT, GL_SPECULAR, 1, 1, 1, 1);
#    glMateriali(GL_FRONT, GL_SHININESS, 128);
#
#    glEnable(GL_LIGHT0);
#
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
#    glSelectBuffer_c($self->select_buffer_len, $self->select_buffer_c->ptr());
}

################################################################################

sub GetContext { # {{{1
    my( $self ) = @_;

    if( Wx::wxVERSION >= 2.009 ) {
        return $self->{context} ||= Wx::GLContext->new( $self );
    } else {
        return $self->SUPER::GetContext;
    }
}

################################################################################

sub SetCurrent { # {{{1
    my( $self, $context ) = @_;

    if( Wx::wxVERSION >= 2.009 ) {
        return $self->SUPER::SetCurrent( $context );
    } else {
        return $self->SUPER::SetCurrent;
    }
}

################################################################################

sub Resize { # {{{1
    my( $self, $event ) = @_;

    my ($width, $height) = $self->GetSizeWH;

    $self->device_width($width);
    $self->device_height($height);

    $log->debug("Resize $width $height");

    $event->Skip;

    return unless $self->GetContext;

    $self->SetCurrent( $self->GetContext );
    glViewport( 0, 0, $width, $height );

    $self->calculate_grid_points;

}
################################################################################

sub render_text_string { #{{{1
    my ($self, $group_id, $string, $base, $char_width, $char_height) = @_;

    $char_width ||= 8;
    $char_height ||= 8;

    for my $char ($string =~ /(.)/g) {
        my $ord = ord($char);

        my $row = int($ord / 16);
        my $col = $ord % 16;
        # $log->info("char $char, ord $ord, row $row, col $col");

        # position of bottom left triangle
        $self->add_render_group_data("${group_id}_text", 'vertex',
            $base->[0], $base->[1],
            $base->[0] + $char_width, $base->[1],
            $base->[0], $base->[1] + $char_height);

        # texture for bottom left triangle
        $self->add_render_group_data("${group_id}_text", 'texture',
            $col/16,$row/16, ($col+1)/16,$row/16, $col/16,($row+1)/16);

        # position of top right triangle
        $self->add_render_group_data("${group_id}_text", 'vertex',
            $base->[0] + $char_width, $base->[1],
            $base->[0] + $char_width, $base->[1] + $char_height,
            $base->[0], $base->[1] + $char_height);

        # texture for top right triangle
        $self->add_render_group_data("${group_id}_text", 'texture',
            ($col+1)/16,$row/16, ($col+1)/16,($row+1)/16, $col/16,($row+1)/16);

        # move the base along
        $base->[0] += $char_width;
    }

    return;
}

################################################################################

sub calculate_grid_points { #{{{1
    my ($self) = @_;

    my ($width, $height) = $self->GetSizeWH;
    my $scene = $self->scene;
    my $scale = $scene->scale;
    my $origin_x = $scene->origin_x;
    my $origin_y = $scene->origin_y;

    my $logical_width = $width * $scale;
    my $logical_height = $height * $scale;
    my $logical_origin_x = 0 - $origin_x;
    my $logical_origin_y = 0 - $origin_y;
    my $debug_logging = $log->debug("logical_origin $logical_origin_x, $logical_origin_y");

    # find corner triangles; bottom left to top left, bottom right to top right
    my @corners;
    $corners[0] = [ $self->find_triangle_coords($logical_origin_x, $logical_origin_y) ];
    $corners[1] = [ $self->find_triangle_coords($logical_origin_x, $logical_origin_y + $logical_height) ];
    $corners[2] = [ $self->find_triangle_coords($logical_origin_x + $logical_width, $logical_origin_y) ];
    $corners[3] = [ $self->find_triangle_coords($logical_origin_x + $logical_width, $logical_origin_y + $logical_height) ];

    # we want to know the bottom left corner of a right-shaped tile containing the point; that corner
    # is not the anchor of a right tile, but the sectors do not go by the same system.
    # Whatever facing triangle we found, we want the anchor point immediately below it, due to the
    # anchor placement on the triangles. We don't change the facing, we're just after the left and top coords.
    for my $i (0 .. 3) {
        $corners[$i]->[0]--;
        $corners[$i]->[2]--;
    }
    $log->info("corners: " . Dumper(\@corners));

    # We're using sectors based on R shaped tiles (arbitrarily chosen over L; T would have been less
    # useful since they have no sides parallel with the window sides). This means that L and T tiles
    # on the bottom and right edges respectively may extend past the edges of the sector as made up
    # of R tiles. What this means in practice is that sectors that are just above or to the left of
    # the visible area (and technically out of view) may need to be rendered as well in case such tiles exist.
    # We'll accomplish this by moving the corners on the left edge outward (ie down and left for the
    # bottom corner, up and left for the top corner) so that we'll include any sectors in this
    # DANGER ZONE. We don't care about the corners on the right edge; we actually don't use the L and R
    # coords from those corners at all, we just need the T coord of the top-right sector to terminate
    # the loop which builds the sector list.

    # move bottom left corner down and left
    $corners[0]->[1]--;
    $corners[0]->[2]--;

    # move top left corner up and left
    $corners[0]->[0]++;
    $corners[0]->[1]--;

    # find the sectors for each corner
    my $sector_size = 3;
    my @sectors = map { [ floor($_->[0]/$sector_size), floor($_->[1]/$sector_size) ] } @corners;
    $log->info("sectors: " . Dumper(\@sectors));

    # finding the vertical sector list along each edge is easy, we just fill in the gap between the
    # respective corner pairs. finding the top and bottom edges is not so easy, since the required sectors
    # will exhibit the usual sawtooth shape along these edges, and the top and bottom sawtooth shapes need not be 
    # in sync; both will have two tiles with the same L coord, ie ramping up as T increases, then L drops. However,
    # the points where L drops at top and bottom are not related.
    # What we need to do (for each of top and bottom) is work out where in the sequence we are at the start; we will drop L after the first or
    # second tile, and then every two tiles after that as we go across (increasing T by 1 per step).
    # have to work out the shift from the left corner sectors to the top and bottom sectors in the next column.
    # For each left corner, the next sector will either be left, top+1 or left-1,top+1. Once we know this, we have the 
    # size of both even and odd sector strips and we can assemble the sector list.
    # See sector_edges.jpg for diagrams; the reference line is labelled C.

    # bottom left; there are two reference lines, 1 and 0.5 Y sector dim above the sector corner.
    # Below the 0.5 line we're at stage 0 but we need to drop the corner by 1 dim, below
    # the 1 line we're at stage 1, otherwise stage 0.
    my $top_ref_line = ($sectors[0]->[0] + $sectors[0]->[1] / 2 + 1) * $self->y_grid_size * $sector_size;
    my $bottom_ref_line = ($sectors[0]->[0] + $sectors[0]->[1] / 2 + 0.5) * $self->y_grid_size * $sector_size;
    my $bottom_stage = 0;
    if ($logical_origin_y <= $bottom_ref_line) {
        $sectors[0]->[0]--;
    }
    elsif ($logical_origin_y <= $top_ref_line) {
        $bottom_stage = 1;
    }

    # top left; reference line 0.5 and 1 Y dims above the sector corner. We never change the corner location
    # in this case, but we may have to start at stage -1 so we get 3 initial raises.
    $bottom_ref_line = ($sectors[1]->[0] + $sectors[1]->[1] / 2 + 0.5) * $self->y_grid_size * $sector_size;
    $top_ref_line = ($sectors[1]->[0] + $sectors[1]->[1] / 2 + 1) * $self->y_grid_size * $sector_size;
    my $top_edge = $logical_origin_y + $logical_height;
    my $top_stage = $top_edge < $bottom_ref_line
        ? 1
        : $top_edge < $top_ref_line
            ? 0
            : -1;
    $log->info(sprintf("logical_origin_y %d, logical_height %d, top corner %d, bottom_ref_line %d, top ref line %d",
        $logical_origin_y, $logical_height, $logical_origin_y + $logical_height, $bottom_ref_line, $top_ref_line));
    $log->info("bottom_stage $bottom_stage, top_stage $top_stage");

    # we'll move across the grid, adding all sectors between top and bottom for each 
    # column of sectors. We adjust the top and bottom coords using the stage fields.
    # stop once we've done the final column.
    my $sector_bottom = $sectors[0]->[0];
    my $sector_top = $sectors[1]->[0];
    for my $sector_column ($sectors[0]->[1] .. $sectors[2]->[1]) {

        # $sector_row is actually the left coord of the sector
        for my $sector_row ($sector_bottom .. $sector_top) {
            push @sectors, [ $sector_row, $sector_column ];
#            $log->info("add [ $sector_row, $sector_column ]");
        }

        $bottom_stage++;
        if ($bottom_stage == 2) {
            $bottom_stage = 0;
            $sector_bottom--;
        }

        $top_stage++;
        if ($top_stage == 2) {
            $top_stage = 0;
            $sector_top--;
        }

    }

    $self->init_render_group('grid');

    my @grid_points = ();

    my $x_grid_size = $self->x_grid_size;
    my $y_grid_size = $self->y_grid_size;

    my $min_x_grid = floor($logical_origin_x / ($x_grid_size * 2));
    my $max_x_grid = ceil(($logical_origin_x + $logical_width) / ($x_grid_size * 2));
    my $min_x = $min_x_grid * $x_grid_size * 2;
    my $max_x = $max_x_grid * $x_grid_size * 2;
    $log->debug("min_x $min_x, max_x $max_x") if $debug_logging;

    my $min_y_grid = floor($logical_origin_y / $y_grid_size);
    my $min_y = $min_y_grid * $y_grid_size;
    $log->debug("min_y $min_y") if $debug_logging;

    # Draw vertical lines
    my $next_x_pos = $min_x;
    while ($next_x_pos < $logical_origin_x + $logical_width) {
        push @grid_points, $next_x_pos, $logical_origin_y, $next_x_pos, $logical_origin_y + $logical_height;
        $next_x_pos += $x_grid_size;
    }

    # drop/rise across the width between the x min & max grid lines
    my $y_slope = ($max_x_grid - $min_x_grid) * $y_grid_size;

    # first loop; lines start at min y grid & move up until the line start is
    # above the top of the graph
    my $y_inc = 0;
    my $max_y_grid = $min_y_grid;
    while ($min_y + $y_inc < $logical_origin_y + $logical_height) {

        # right axes
        push @grid_points, $min_x, $min_y + $y_inc, $max_x, $min_y + $y_inc + $y_slope;

        # reverse x coords for left axes
        push @grid_points, $max_x, $min_y + $y_inc, $min_x, $min_y + $y_inc + $y_slope;

        $y_inc += $y_grid_size;
        $max_y_grid++;
    }
    $log->debug("grid : x from $min_x_grid to $max_x_grid, y from $min_y_grid to $max_y_grid") if $debug_logging;

    # second loop; lines start 1 grid below min y grid (we've already drawn the axes
    # starting at min) and move down until the line end is below the bottom of the graph.
    # right & left axes done as above.
    $y_inc = -$y_grid_size;
    while ($min_y + $y_inc + $y_slope > $logical_origin_y) {
        push @grid_points, $min_x, $min_y + $y_inc, $max_x, $min_y + $y_inc + $y_slope;
        push @grid_points, $max_x, $min_y + $y_inc, $min_x, $min_y + $y_inc + $y_slope;
        $y_inc -= $y_grid_size;
    }

    # don't display a grid below a certain point, you couldn't pick a tile at this scale.
    # We still do the calculations because they give us the visible tiles.
    if ($scale < 8) {
        push @{ $self->render_group->{grid}->{vertex}->{data} }, @grid_points;
    }

    # save the grid dimensions for visible cell calculations; these slow down panning
    # so we just calculate them as needed, but there are too many values calculated in here
    # to calculate again.
    $self->current_view({
        logical_origin_x => $logical_origin_x,
        logical_origin_y => $logical_origin_y,
        logical_width => $logical_width,
        min_y_grid => $min_y_grid,
        max_y_grid => $max_y_grid,
    });

    $self->make_render_group_array_objects('grid');

    # whenever we change the grid area, we clear this flag
    $self->visible_is_new(0);

    return;
}

################################################################################
# determine visible tiles.
sub mark_visible_tiles { #{{{1
    my ($self) = @_;

    # test all tiles to see if they're visible.
    # Note that this definition of visible is somewhat loose with respect to the upper and lower
    # boundaries, since behaviour along these lines is tricky to model given the inherent up-and-down
    # pattern and the different tile shapes. We're being overly inclusive.

    $self->key_hash->{visible} = {};

    return unless my $view = $self->current_view;

    # the x grid values are useful for drawing the lines but too vague for visibility. We calculate the 
    # corner triangle coordinates and just use the top.
    my (undef, $leftmost_top, undef, undef) = $self->find_triangle_coords($view->{logical_origin_x}, $view->{logical_origin_y});
    my (undef, $rightmost_top, undef, undef) = $self->find_triangle_coords($view->{logical_origin_x} + $view->{logical_width}, $view->{logical_origin_y});

    # Add 1 to max_y_grid to cope with the up-and-down pattern of the top "row".
    my $max_y_grid = $view->{max_y_grid} + 1;

    for my $tile (values %{ $self->grid }) {
        my $x_grid = $tile->{top} / 2;
        my $y_grid = $tile->{left} + $x_grid;

        # top tiles can start one coord to the left and still be visible
        if ( ($tile->{shape} eq 'T' ? ($tile->{top} + 1 >= $leftmost_top) : ($tile->{top} >= $leftmost_top))
            && $tile->{top} <= $rightmost_top
            && $y_grid >= $view->{min_y_grid}
            && $y_grid <= $view->{max_y_grid})
        {
            my $grid_key = "$tile->{facing}_$tile->{left}_$tile->{top}_$tile->{right}";
            $self->key_hash->{visible}->{$grid_key} = 1;
        }
    }
#    $log->debug("visible: " . Dumper($self->key_hash->{visible}));

    return;
}

################################################################################
sub init_render_group { #{{{1
    my ($self, $group_id) = @_;

    for my $real_group_id ($group_id, "${group_id}_text") {
        for my $array_type (qw(vertex color texture)) {
            $self->render_group->{$real_group_id}->{$array_type}->{data} = [];
            $self->render_group->{$real_group_id}->{number_vertices} = 0;
            delete $self->render_group->{$real_group_id}->{$array_type}->{array};
        }
    }

    return;
}

################################################################################
# Take the raw data added to a render group's lists and build the OpenGL::Array
# objects required for rendering.
sub make_render_group_array_objects { #{{{1
    my ($self, $group_id) = @_;

    my %array_info = (
        vertex => {
            group_size => 2,
            type => GL_FLOAT,
        },
        texture => {
            group_size => 2,
            type => GL_FLOAT,
        },
        color => {
            group_size => 3,
            type => GL_UNSIGNED_BYTE,
        },
    );

    # size from vertex list, textures and colors must match if present
    for my $real_group_id ($group_id, "${group_id}_text") {
        my $number_vertices = scalar @{ $self->render_group->{$real_group_id}->{vertex}->{data} } / $array_info{vertex}->{group_size};
        # $log->debug("made $number_vertices vertices for $real_group_id");

        for my $array_type (qw(vertex color texture)) {
            my $data = $self->render_group->{$real_group_id}->{$array_type}->{data};
            my $info = $array_info{$array_type};
            next unless my $number_values = scalar @{ $data };
            if ($array_type ne 'vertex') {
                my $number_elements = $number_values / $info->{group_size};
                $log->logdie("bad data size for $real_group_id $array_type; found $number_elements, need $number_vertices")
                    if $number_elements != $number_vertices;
                $log->debug("made $number_elements $array_type elements for $real_group_id");
            }
            $self->render_group->{$real_group_id}->{$array_type}->{array} = OpenGL::Array->new_list($info->{type}, @{ $data });
        }

        # save this for the glDrawArrays call
        $self->render_group->{$real_group_id}->{number_vertices} = $number_vertices;
    }

    return;
}

################################################################################
sub add_render_group_data { #{{{1
    my ($self, $real_group_id, $array_type, @data) = @_;

    # $log->info("add_render_group_data: $real_group_id $array_type " . Dumper(\@data));

    push @{ $self->render_group->{$real_group_id}->{$array_type}->{data} }, @data;

    return;
}

################################################################################
sub display_render_group { # {{{1
    my ($self, $real_group_id) = @_;

    my $render_group = $self->render_group->{$real_group_id};
    unless ($render_group) {
        $log->info("render_group $real_group_id doesn't exist");
        return;
    }

    return unless $render_group->{number_vertices};
    # $log->debug("rendering $render_group->{number_vertices} vertices from $real_group_id");

    glVertexPointer_p(2, $render_group->{vertex}->{array});

    if (exists $render_group->{color}->{array}) {
        glEnableClientState(GL_COLOR_ARRAY);
        glColorPointer_p(3, $render_group->{color}->{array});
    }
    elsif (exists $render_group->{texture}->{array}) {
        glEnable( GL_TEXTURE_2D );
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glBindTexture(GL_TEXTURE_2D, $self->texture_id->{ $real_group_id =~ /text/ ? 'font_trans' : 'select'});
        glTexCoordPointer_p(2, $render_group->{texture}->{array});
    }

    glDrawArrays($real_group_id eq 'grid' ? GL_LINES : GL_TRIANGLES, 0, $render_group->{number_vertices});

    if (exists $render_group->{color}->{array}) {
        glDisableClientState(GL_COLOR_ARRAY);
#        glColorPointer_p(3, $render_group->{color}->{array});
    }
    elsif (exists $render_group->{texture}->{array}) {
        glDisable( GL_TEXTURE_2D );
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    }

    return;
}

################################################################################

sub Render { # {{{1
    my ($self, $width, $height) = @_;

    return unless $self->GetContext;
    $self->SetCurrent( $self->GetContext );
    $self->InitGL unless $self->init;

#    my $spacing = 10;
#    $self->init_render_group('permanent');
#    $self->init_render_group('text');
#    for my $triangle (@Triangles) {
#        my ($col, $row) = @{ $triangle };
#
#        # no texture data required here, this array is drawn without texture data
#
#        $self->add_render_group_data('permanent', 'vertex',
#            $col * $spacing, $row * $spacing,
#            $col * $spacing + $spacing / 2, $row * $spacing,
#            $col * $spacing, $row * $spacing + $spacing / 2);
#
#        $self->add_render_group_data('permanent', 'color',
#            $col * 30, $row * 30, 0,
#            $col * 30, $row * 30, 0,
#            $col * 30, $row * 30, 0);
#
##        $self->render_text_string('permanent', "$col,$row", [ $col * $spacing, $row * $spacing ]);
#
#    }
#
#    $self->make_render_group_array_objects('permanent');
#
#    for my $left (-5 .. 5) {
#        for my $top (-5 .. 5) {
#            my $x = $top * $self->x_grid_size;
#            my $y = ($left + $top / 2) * $self->y_grid_size;
#            my $right = $left + $top;
#            $self->render_text_string('permanent', "$left,$top,$right", [ $x,$y ]);
#        }
#    }
#
#    $self->make_render_group_array_objects('text');

    my ($canvas_width, $canvas_height) = $self->GetSizeWH;
    my $render_to_image;

    if (defined $width) {

        glViewport( 0, 0, $width, $height );
        $render_to_image = 1;

        $log->info(sprintf("render to image; origin %.1f,%.1f, output dim $width x $height, scale %.2f, logical dim %.1f x %.1f", 
            $self->scene->origin_x,$self->scene->origin_y,
            $self->scene->scale, 
            $width * $self->scene->scale, 
            $height * $self->scene->scale) );

    }
    else {
        ($width, $height) = ($canvas_width, $canvas_height);
    }

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
#    glOrtho(0, $width * $self->scene->scale, $height * $self->scene->scale, 0, -1, 1);

    glOrtho(0, $width * $self->scene->scale, 0, $height * $self->scene->scale, -1, 1);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef( $self->scene->origin_x,$self->scene->origin_y,0 );

    # Rendering passes & arrays:
    # Grid - vertex (may come before tiles)
    # Tiles; Permanent New Transient
    # Select markers
    # Text; Permanent New Transient
    # Target

    glClearColor(@{ $self->background_color }, 0);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glEnableClientState(GL_VERTEX_ARRAY);

    # display the grid
    unless ($render_to_image) {
        glColor3ub(@{ $self->bg_line_color });
        $self->display_render_group('grid');
    }

    for my $group (qw(permanent new transient)) {
        $self->display_render_group($group);
    }

    $self->display_render_group('select');

    # render text after select so markers don't cover text
    for my $group (qw(permanent new transient)) {
        $self->display_render_group("${group}_text");
    }

    # draw the current target
    glColor4ub(0xff, 0x00, 0x00, 0x3f);
    $self->display_render_group('target');

    glDisableClientState(GL_VERTEX_ARRAY);

#    glBindTexture(GL_TEXTURE_2D, $self->texture_id->{texture});
#    glBegin(GL_TRIANGLES);
#    glTexCoord2f(0,0);
#    glVertex2f(0,0);
#    glTexCoord2f(1,0);
#    glVertex2f(5,0);
#    glTexCoord2f(0,1);
#    glVertex2f(0,5);
#    glEnd;

    glColor3ub(0xff, 0xff, 0xff);
    glBegin(GL_LINES);
    glVertex2f(1,1);
    glVertex2f(1,-1);
    glVertex2f(1,-1);
    glVertex2f(-1,-1);
    glVertex2f(-1,-1);
    glVertex2f(-1,1);
    glVertex2f(-1,1);
    glVertex2f(1,1);
    glEnd;

    if ($render_to_image) {
        glViewport( 0, 0, $canvas_width, $canvas_height );
    }
    else {
        $self->SwapBuffers();
    }

    return;
}

################################################################################
sub rebuild_render_group { #{{{1
    my ($self, $group_id) = @_;

    my $info_logging = $log->info("rebuild_render_group $group_id from " . (caller)[2]);

    my $config = wxTheApp->config;

    $self->init_render_group($group_id);

    my $palette = $self->palette;

    my %group_source = (
        permanent => [
            {
                data => $self->grid,
                exclude => $self->key_hash->{new},
            },
        ],
        new => [
            {
                data => $self->grid,
                include => $self->key_hash->{new},
                exclude => $self->key_hash->{erase},
            },
        ],
        transient => [
            {
                data => $self->transient_grid,
            },
        ],
        select => [
            {
                data => $self->grid,
                include => $self->key_hash->{select},
                exclude => $self->key_hash->{erase},
            },

        ],
    );

    my $sources = $group_source{$group_id}
        or $log->logdie("no sources for group $group_id");

    # while we're doing area select/deselect, keys in transient are also relevant,
    # but how so depends on select_toggle.
    if ($self->frame->action =~ /select/ && $self->frame->area_mode && $group_id eq 'select') {
        if ($self->select_toggle) {

            # we're selecting, so the transient hash is an extra source of selected tiles
            push @{ $sources }, 
                {
                    data => $self->grid,
                    include => $self->key_hash->{transient},
                };
        }
        else {

            # we're unselecting, so the transient hash becomes the exclude array for the
            # current selections.
            $sources->[0]->{exclude} = $self->key_hash->{transient};
        }
    }

    # $log->debug("sources for $group_id : " . Dumper($sources));

    # position of text relative to tile anchor
    my %shape_text_offset = (
        $SH_LEFT            => [ 0, -8 ],
        $SH_TOP             => [ 0, 0 ],
        $SH_RIGHT           => [ 0, -8 ],
        $SH_TRIANGLE_LEFT   => [ 0, 0 ],
        $SH_TRIANGLE_RIGHT  => [ 0, -8 ],
    );

    # shortcut for speed; this count of how many vertices make up a tile is used to add
    # the correct number of color items.
    my %number_shape_vertices;

    for my $shape ( $SH_LEFT, $SH_TOP, $SH_RIGHT, $SH_TRIANGLE_LEFT, $SH_TRIANGLE_RIGHT ) {
        $number_shape_vertices{$shape} = scalar @{ $Tile_shape->{ $shape } };
    }

    my %shape_select_texture_coords = (
        $SH_LEFT            => [ 0,1, 0,0.5, 0.5,1, 0.5,1, 0,0.5, 0.5,0.5 ],
        $SH_TOP             => [ 0.5,0.5, 1,0.5, 0.5,1, 0.5,1, 1,0.5, 1,1 ],
        $SH_RIGHT           => [ 0,0.5, 0,0, 0.5,0, 0.5,0, 0.5,0.5, 0,0.5 ],
        $SH_TRIANGLE_RIGHT  => [ 0.5,0.333, 0.5,0, 0.78,0.167 ],
        $SH_TRIANGLE_LEFT   => [ 0.72,0.333, 1,0.167, 1,0.5 ],
    );

    for my $source ( @{ $sources }) {

        # If there's no include hash, we just use all the keys in data
        my $key_hash = $source->{include} || $source->{data};
        my $data_hash = $source->{data};
        my $exclude_hash = $source->{exclude} || {};

        for my $grid_key (keys %{ $key_hash }) {

            # skip any keys in the exclude hash
            next if $exclude_hash->{$grid_key};

            # $log->debug("add $grid_key in group $group_id");
            my $tile = $data_hash->{$grid_key}
                or $log->logdie("didn't find tile in data array");

    #        $log->debug(sprintf "draw $tile->{shape} $grid_key at %f, %f",
    #            $tile->{top} * $self->x_grid_size,
    #            ($tile->{left} + $tile->{top} / 2) * $self->y_grid_size);

            my $shape = $Tile_shape->{ $tile->{shape} }
                or $log->logdie("bad tile shape $tile->{shape}");

            my @anchor = (
                $tile->{top} * $self->x_grid_size,
                ($tile->{left} + $tile->{top} / 2) * $self->y_grid_size,
            );

            if ($group_id eq 'select') {

                # draw select markers
                my $tick_shape = $Tick_shape->{ $tile->{shape} }
                    or $log->logdie("no tick shape for shape $tile->{shape}");

                my @tick_vertices = ();
                for my $vertex ( @{ $tick_shape } ) {
                    push @tick_vertices, $vertex->[0] + $anchor[0], $vertex->[1] + $anchor[1];
                }

                $self->add_render_group_data($group_id, 'vertex', @tick_vertices);
                $self->add_render_group_data($group_id, 'texture', @{ $shape_select_texture_coords{ $tile->{shape} } });

            }
            else {

                # draw tiles and tile text (into separate arrays, but that's hidden inside render_text_string).
                my @vertices = ();
                for my $vertex ( @{ $shape } ) {
                    push @vertices, $vertex->[0] + $anchor[0], $vertex->[1] + $anchor[1];
                }

                $self->add_render_group_data($group_id, 'vertex', @vertices);

                # all vertices in both triangles share the color
                $self->add_render_group_data($group_id, 'color',
                    (@{ $palette->[ $tile->{brush_index} ] }) x $number_shape_vertices{$tile->{shape}} );

                # text for tile?
                if ($config->display_palette_index || $config->display_color || $config->display_key) {

                    my @strings = (
                        $config->display_palette_index ? $tile->{brush_index} : '',
                        $config->display_color ? $self->scene->palette->[ $tile->{brush_index} ] : '',
                        $config->display_key ? join(':', @{ $tile }{qw(shape left top right)}) : '',
                    );
                    $strings[1] =~ s/#//;

                    my $text_offset = [ @{ $shape_text_offset{ $tile->{shape} } } ];

                    for my $string_index (0,1,2) {
                        next unless length $strings[$string_index];
                        $self->render_text_string($group_id, $strings[$string_index], 
                            [
                                $anchor[0] + $text_offset->[0],
                                $anchor[1] + $text_offset->[1],
                            ]);
                        $text_offset->[1] += 8;
                    }

                }
            }
        }
    }

    $self->make_render_group_array_objects($group_id);

    return;
}

################################################################################
# copy the transient tiles to the real grid
sub preserve_transient_grid { #{{{1
    my ($self) = @_;

#    $log->debug("preserve_transient_grid from " . (caller)[2] . ", data " . Dumper($self->transient_grid));

    # copy tiles from transient_grid to scene and add keys to new
    my ($grid, $transient_grid) = ($self->grid, $self->transient_grid);
    my @transient_keys = keys %{ $transient_grid };
    for my $key (@transient_keys) {
        $log->debug("copy transient $key to new");
        $grid->{ $key } = $transient_grid->{ $key };
        $self->key_hash->{new}->{$key} = 1;
    }

    # since all the undo information needs is keys, we can get them from the transient grid.
    # We have to copy to the real grid first, as that's where add_undo_action will look 
    # for the tile data.
    $self->add_undo_action($IsoFrame::AC_PAINT, \@transient_keys);

    # clear transient grid
    $self->transient_grid({});

    $self->rebuild_render_group('new');
    $self->rebuild_render_group('transient');

    return;
}

################################################################################
# make all new tiles permanent, so we speed up any new painting from now on.
sub make_new_permanent { #{{{1
    my ($self) = @_;

    # all we have to do is empty the new key hash and rebuild
    $self->key_hash->{new} = {};
    $self->rebuild_render_group('new');
    $self->rebuild_render_group('permanent');

    return;
}

################################################################################
# mark all visible tiles as new, so we can render them separately.
sub make_visible_new { #{{{1
    my ($self) = @_;

    # we only need to do this once per visible region; this flag goes false 
    # whenever we zoom or scroll.
    $self->visible_is_new(1);

    $self->mark_visible_tiles;

    for my $key (keys %{ $self->key_hash->{visible} }) {
        $self->key_hash->{new}->{$key} = 1;
    }

    return;
}

################################################################################
# build the array to display the target, ie the standard cursor, the tiles to 
# be pasted or an enlarged cursor if we're importing.
sub make_current_target { #{{{1
    my ($self) = @_;

    $self->init_render_group('target');

    if (my $current_location = $self->current_location) {
        my ($left, $top, $right, $facing) = @{ $current_location };

        # here, we can equate side with shape
        my $shape = $self->frame->current_side;

        # change the grid key according to the shape if we're in the wrong triangle;
        if ($shape eq $SH_LEFT && $facing eq $SH_LEFT) {
            $left++;
            $right++;
        }
        elsif ($shape eq $SH_TOP && $facing eq $SH_RIGHT) {
            $right--;
            $top--;
        }

        my @anchor = (
            $top * $self->x_grid_size,
            ($left + $top / 2) * $self->y_grid_size,
        );

#        if (my $paste_list = $self->paste_list) {
#
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
#        }
#        else {

            my @vertices = ();
            my $tile_shape = $Tile_shape->{$shape};

            if ($self->cursor_multiplier_x > 1 || $self->cursor_multiplier_y > 1) {
                my $shape_multiplier = {
                    $SH_LEFT => [
                        [ 1,1 ],
                        [ 1, $self->cursor_multiplier_y, ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_x ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_x ],
                        [ 1, $self->cursor_multiplier_y, ],
                        [ $self->cursor_multiplier_x, ($self->cursor_multiplier_y * 2 + $self->cursor_multiplier_x) / 3 ],
                    ],
                    $SH_TOP => [
                        [ 1,1 ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_x, ],
                        [ $self->cursor_multiplier_y, $self->cursor_multiplier_y ],
                        [ $self->cursor_multiplier_y, $self->cursor_multiplier_y ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_x, ],
                        [ ($self->cursor_multiplier_x + $self->cursor_multiplier_y) / 2, 0 ],
                    ],
                    $SH_RIGHT => [
                        [ 1,1 ],
                        [ 1, $self->cursor_multiplier_y, ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_x ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_x ],
                        [ $self->cursor_multiplier_x, $self->cursor_multiplier_y * 2 - $self->cursor_multiplier_x ],
                        [ 1,1 ],
                    ],
                };
                my $multipliers = $shape_multiplier->{ $shape };
                for my $i (0 .. 5) {

                    # there's one corner in one shape that needs something more; top shape, far-right corner
                    # has a Y coord of 0, so multiplying it by anything doesn't work. 
                    # Just calculate it directly for that case.
                    my $third_top_y_coord = $shape eq $SH_TOP && $i == 5
                        ? ($self->cursor_multiplier_y - $self->cursor_multiplier_x) * ($self->y_grid_size / 2)
                        : 0;

                    push @vertices,
                        $anchor[0] + $tile_shape->[$i]->[0] * $multipliers->[$i]->[0],
                        $anchor[1] + $tile_shape->[$i]->[1] * $multipliers->[$i]->[1] + $third_top_y_coord;
                }
            }
            else {

                for my $vertex (@{ $tile_shape }) {
                    push @vertices, $anchor[0] + $vertex->[0], $anchor[1] + $vertex->[1];
                }
            }

            $self->add_render_group_data('target', 'vertex', @vertices);
#        }

        $self->make_render_group_array_objects('target');
    }

    return;
}

################################################################################
sub scene { #{{{1
    my ($self, $scene) = @_;

    if ($scene) {
        $self->_scene($scene);

        # make the grid available through an accessor, so this function call isn't 
        # necessary every time we want the grid.
        $self->grid($scene->grid);

        $self->palette([]);
        $self->palette_index({});
        for my $i ( 0 .. $#{ $scene->palette } ) {
            my $colour = Wx::Colour->new(my $colour_str = $scene->palette->[$i]);
            push @{ $self->palette }, [ $colour->Red, $colour->Green, $colour->Blue ];
            $self->palette_index->{$colour_str} = $i;
        }

        my $color = Wx::Colour->new($scene->background_rgb);
        $self->background_color([ $color->Red / 255, $color->Green / 255, $color->Blue / 255 ] );
        $color = Wx::Colour->new($scene->bg_line_rgb);
        $self->bg_line_color([ $color->Red, $color->Green, $color->Blue ] );
        # $self->tile_line_pen(Wx::Pen->new(Wx::Colour->new($scene->tile_line_rgb), 1, wxPENSTYLE_SOLID));

        $self->frame->cube_brush({
            L => Wx::Brush->new(Wx::Colour->new($scene->left_rgb), wxBRUSHSTYLE_SOLID),
            T => Wx::Brush->new(Wx::Colour->new($scene->top_rgb), wxBRUSHSTYLE_SOLID),
            R => Wx::Brush->new(Wx::Colour->new($scene->right_rgb), wxBRUSHSTYLE_SOLID),
        });

        # clear the key hashes
        for my $hash (qw(new select transient erase)) {
            $self->key_hash->{$hash} = {};
        }

        for my $key (keys %{ $scene->grid }) {

            # initialise the select hash with selected tiles
            $self->key_hash->{select}->{$key} = 1 if $scene->grid->{$key}->{selected};

            # clear the new key
            delete $scene->grid->{$key}->{new};
        }

        $self->area_tiles([]);

        for my $array (qw(permanent new transient select)) {
            $self->rebuild_render_group($array);
        }

        # initialise the undo/redo buttons to match the new scene
        $self->set_undo_redo_button_states;
    }

    return $self->_scene;
}

################################################################################
sub set_cursor { #{{{1
    my ($self) = @_;

    my $frame = $self->frame;
    my $cursor = $frame->pan_mode
        ? $self->action_cursor->{move}
        : $self->action_cursor->{$frame->action};
    $log->logconfess("no cursor for action " . $frame->action)
        unless $cursor;
    $self->SetCursor($cursor);

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
            $anchor[1] = $min_y + 1;
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
                $anchor[1] = $min_y + 1.5;
                $facing = $SH_RIGHT;
            }
        }
        else {
            if ($min_x_is_even) {
                $anchor[1] = $min_y + 1;
                $facing = $SH_LEFT;
            }
            else {
                $anchor[1] = $min_y + 0.5;
                $facing = $SH_RIGHT;
            }
        }
    }

    my $top = $min_x;
    my $left = $anchor[1] - $top / 2;
    my $right = $left + $top;

    $anchor[1] *= $self->y_grid_size;

#    $log->info("pg $point_gradient, cg $control_gradient, min_x $min_x, min_y $min_y, key @key, anchor @anchor, facing $facing");
#    $log->info("find_triangle_coords $left, $top, $right, $facing");

#    @stash = ($min_x, $min_y, @key, @anchor, $facing);

    return ($left, $top, $right, $facing);
}

################################################################################
sub mouse_event_handler { #{{{1
    my ($self, $event_flags, $device_x, $device_y) = @_;

    # $log->info(sprintf "mouse event: %08x", $event_flags);
    # my $start_mouse = [ gettimeofday ];

    my $refresh;
    my @refresh_corners;
    my $frame = $self->frame;
    my $area_mode = $frame->area_mode;
    my $pan_mode = $frame->pan_mode;
    my $side = $frame->current_side;
    my $action = $frame->action;
    my $app = wxTheApp;

    # if the autosave timer is running on the short period, restart it; we're not idle yet
    if ($app->autosave_timer->GetInterval == $app->config->autosave_idle_seconds * 1000) {
        $app->autosave_timer->Start($app->config->autosave_idle_seconds * 1000, wxTIMER_ONE_SHOT);
    }

    my ($logical_x, $logical_y) = ( int($device_x * $self->scene->scale - $self->scene->origin_x),
        int($device_y * $self->scene->scale - $self->scene->origin_y));
    # $log->debug("logical_x, logical_y = $logical_x, $logical_y, origin = " . $self->scene->origin_x . ',' . $self->scene->origin_y);

    my ($left, $top, $right, $facing) = $self->find_triangle_coords($logical_x, $logical_y);
    $self->current_location([ $left, $top, $right, $facing ]);
    $self->make_current_target;

    my $grid_key = "${facing}_${left}_${top}_${right}";
    if ($grid_key ne ($self->current_grid_key || '')) {
        $refresh = 1;
        $self->current_grid_key($grid_key);

#        if ($event_flags == 0) {
#            @refresh_corners = ($device_x, $self->last_device_x, $device_y, $self->last_device_y);
#        }
    }
    elsif ($event_flags == 0 && ! $pan_mode ) {
        
        # we haven't changed triangle, we're not moving the offset
        # and we haven't pressed a button; bail
        return;
    }

    if ($event_flags & $ME_LEFT_BUTTON) {

        # in move mode, left button down/up toggles dragging flag
        if ($pan_mode) {
            $self->dragging($event_flags & $ME_BUTTON_DOWN);
            $log->debug("dragging now? " . $self->dragging);
            return;
        }

        # in paint, sample, select or erase action, left button down/up toggles the action
        if ($event_flags & $ME_BUTTON_DOWN) {
            $refresh = 1;
            $self->paint_shape($side);
            $self->area_start( $area_mode
                ? [ $left, $top, $right, $facing ]
                : undef);

            if ($action =~ /$IsoFrame::AC_PAINT|$IsoFrame::AC_SHADE_CUBE/o) {

                # set the current brush index, adding the colour to the palette if we need to
                my $palette_index = $self->get_palette_index($frame->cube_brush->{$side}->GetColour->GetAsString(wxC2S_HTML_SYNTAX));
                $self->brush_index($palette_index);
            }
            elsif ($action =~ /erase/ && $area_mode) {

                # we're starting an area erase; add all visible tiles to new so we only have to refresh that array on every move,
                # not permanent as well.
                unless ($self->visible_is_new) {
                    $self->make_visible_new;
                    $self->rebuild_render_group('permanent');
                    $self->rebuild_render_group('new');
                }
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
                $frame->action($frame->previous_paint_action);
                $self->set_cursor;
                $frame->update_scene_color($side);
                $frame->Refresh;
            }

            if ($action eq $IsoFrame::AC_IMPORT) {
                $self->import_bitmap($side);
                $frame->action($frame->previous_action);
                $self->set_cursor;
                $self->rebuild_render_group('new');
                $frame->Refresh;
            }

            if ($action eq $IsoFrame::AC_PASTE) {
                $log->debug("do paste");

                # after pasting, do we continue pasting or switch to paint
                if ($app->config->repeated_pasting) {
                    $self->paint_shape(1);
                }
                else {
                    $frame->action($frame->previous_action);
                    $self->set_cursor;
                    $self->paste_list(undef);
                }

                $self->preserve_transient_grid;

                # we've changed the action so refresh the frame
                $frame->Refresh;
            }

            if ($area_mode) {

                # clear area size message
                wxTheApp->set_frame_title();

                if ($action =~ /$IsoFrame::AC_PAINT|$IsoFrame::AC_SHADE_CUBE/o) {
                    $self->preserve_transient_grid;
                    $refresh = 1;
                }
                elsif ($action =~ /erase/) {

                    # the erase key hash holds the tiles to be deleted, which are all now marked
                    # as new and rendered as such.
                    my $deleted_keys = [ keys %{ $self->key_hash->{erase} } ];
                    $self->add_undo_action($IsoFrame::AC_ERASE, $deleted_keys);

                    # we don't care about the return flags from remove_tiles, we know we're
                    # rebuilding new.
                    $self->remove_tiles($deleted_keys);
                    $refresh = 1;
                    $self->key_hash->{erase} = {};
                    $self->rebuild_render_group('new');
                    $self->rebuild_render_group('select');
                }
                elsif ($action =~ /select/) {

                    # keys in transient are new selections, copy them to select and mark the tiles
                    my @selected_tiles = keys %{ $self->key_hash->{transient} };
                    $log->debug("selected_tiles @selected_tiles");
                    my $grid = $self->grid;
                    for my $key (@selected_tiles) {
                        if ($self->select_toggle) {
                            $self->key_hash->{select}->{$key} = 1;
                        }
                        else {
                            delete $self->key_hash->{select}->{$key};
                        }
                        $grid->{$key}->{selected} = $self->select_toggle;
                    }
                    $self->rebuild_render_group('select');
                    $refresh = 1;
                    $self->key_hash->{transient} = {};
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
                            L => [ 1, 0, 1 ],
                            R => [ -1, 0, -1 ],
                        },
                        $SH_TOP => {
                            L => [ 0, 1, 1 ],
                            R => [ 0, -1, -1 ],
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

        if ($action eq $IsoFrame::AC_PAINT) {

            if ($self->area_start) {

                # clear tiles from previous area paint
                $self->transient_grid({});

                $refresh = $self->mark_area($self->transient_grid, $left, $top, $right, $facing, $paint_shape);
                $self->rebuild_render_group('transient') if $refresh;
            }
            else {

                # let paint_tile do all the transforms and clash checking
                if (my $grid_key = $self->paint_tile($left, $top, $right, $facing, $paint_shape)) {
                    $self->rebuild_render_group('new');
                    $self->add_undo_action($IsoFrame::AC_PAINT, $self->grid->{$grid_key});
                    $refresh = 1;
                }
            }
        }
        elsif ($action =~ /erase/) {
            if ($self->area_start) {

                # clear tiles from previous area clear location
                $self->key_hash->{erase} = {};

                # we shifted all visible tiles from permanent to new when we started, so now we
                # just display new - erased.
                $refresh = $self->mark_area($self->key_hash->{erase}, $left, $top, $right, $facing, $paint_shape);
                $self->rebuild_render_group('new');
                $self->rebuild_render_group('select');
            }
            else {
                if (my $tile = $self->find_tile($grid_key)) {
#                    $log->debug("found tile " . Dumper($tile));

                    if ($action eq $IsoFrame::AC_ERASE_ALL
                        || $tile->{shape} =~ /T[LR]/
                        || ($action eq $IsoFrame::AC_ERASE && $tile->{shape} eq $paint_shape)
                        || ($action eq $IsoFrame::AC_ERASE_OTHERS && $tile->{shape} ne $paint_shape))
                    {

                        # the tile we delete will not have the same grid_key that we 
                        # passed in if we clicked in the "wrong" half.
                        $grid_key = join('_', @{ $tile }{qw( facing left top right )});

                        $self->add_undo_action($IsoFrame::AC_ERASE, $self->grid->{$grid_key});
                        delete $self->grid->{$grid_key};

                        # no point in checking if it exists in new or select, just try to delete it and record if it was there
                        my $was_new = delete $self->key_hash->{new}->{$grid_key};
                        my $was_selected = delete $self->key_hash->{select}->{$grid_key};

                        # if we delete a single permanent tile, we can't prevent refreshing permanent
                        $self->rebuild_render_group($was_new ? 'new' : 'permanent');
                        $self->rebuild_render_group('select') if $was_selected;
                        $refresh = 1;
                    }
                }
            }
        }
        elsif ($action =~ /select/) {
            if ($self->area_start) {

                # clear tiles from previous area
                $self->key_hash->{transient} = {};

                $refresh = $self->mark_area($self->key_hash->{transient}, $left, $top, $right, $facing, $paint_shape);
                $self->rebuild_render_group('select');
            }
            elsif (my $tile = $self->find_tile($grid_key)) {
#                $log->debug("tile: " . Dumper($tile));
                if ($action eq $IsoFrame::AC_SELECT_ALL
                    || $tile->{shape} =~ /T[LR]/
                    || ($action eq $IsoFrame::AC_SELECT && $tile->{shape} eq $paint_shape)
                    || ($action eq $IsoFrame::AC_SELECT_OTHERS && $tile->{shape} ne $paint_shape))
                {
                    $tile->{selected} = $self->select_toggle;

                    # find the real grid key
                    $grid_key = join('_', @{ $tile }{qw( facing left top right )});
                    if ($self->select_toggle) {
                        $self->key_hash->{select}->{$grid_key} = 1;
                    }
                    else {
                        delete $self->key_hash->{select}->{$grid_key};
                    }
                    $self->rebuild_render_group('select');
                    $refresh = 1;
                }
            }
        }
        elsif ($action eq $IsoFrame::AC_SAMPLE) {
            if (my $tile = $self->find_tile($grid_key)) {
                my $brush = $frame->cube_brush->{ $side };
                $brush->SetColour( Wx::Colour->new(sprintf("#%02X%02X%02X", @{ $self->palette->[$tile->{brush_index}] })));
                $frame->Refresh;
            }
        }
        elsif ($action eq $IsoFrame::AC_SHADE_CUBE) {

            if ($self->area_start) {

                # clear tiles from previous area paint
                $self->transient_grid({});

                $refresh = $self->mark_area($self->transient_grid, $left, $top, $right, $facing, $paint_shape);
                $self->rebuild_render_group('transient') if $refresh;
            }
            else {

                if ($refresh = $self->shade_cube($left, $top, $right, $facing, $paint_shape) ) {
                    $self->rebuild_render_group('new');
                }
            }
        }
        elsif ($action eq $IsoFrame::AC_PASTE) {

            # clear tiles from previous refresh
            $self->transient_grid({});

            # paint tiles from paste list onto the transient_grid
            for my $tile ( @{ $self->paste_list } ) {

                if (my $grid_key = $self->paint_tile($left + $tile->{left}, $top + $tile->{top}, $right + $tile->{right},
                    $tile->{facing}, $tile->{shape}, $tile->{brush_index}))
                {
                    $refresh = 1;
                }
            }
            $self->rebuild_render_group('transient') if $refresh;
        }
    }

    # wheel zooms in/out
    if ($event_flags & ($ME_WHEEL_FORWARD | $ME_WHEEL_BACK)) {
        my $scale = $self->scene->scale;
        my $scale_factor;
        if ($event_flags & $ME_WHEEL_BACK) {

            $scale_factor = 2 if $scale < 16;
            $log->debug("wheel back, scale_factor " . ($scale_factor || 'undef'));
        }
        else {
            $scale_factor = 0.5 if $scale > 0.4;
            $log->debug("wheel forward, scale_factor " . ($scale_factor || 'undef'));
        }
        if (defined $scale_factor) {
            $scale *= $scale_factor;

            # reset to int 1 when coming back up from fractional end of range
            $scale = 1 if ($scale > 0.9 && $scale < 1.1);

            $self->scene->scale($scale);
            $log->debug("scale now $scale");
            $self->scene->origin_x($scale_factor * ($self->scene->origin_x + $logical_x) - $logical_x);
            $self->scene->origin_y($scale_factor * ($self->scene->origin_y + $logical_y) - $logical_y);
            $self->calculate_grid_points;
            $refresh = 1;
        }
    }

    # motion while dragging moves origin
    if (defined $device_x && $self->dragging) {
        $self->move_origin($device_x, $device_y);
        $self->calculate_grid_points;
        $refresh = 1;
    }

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

        # despite the shift to 3 byte colour lists as required by OpenGL, we're still
        # mostly referring to colours via #RRGGBB strings; we can store them in files,
        # we can quickly create brushes from them, we can get them from wxcolour objects, etc.
        # We only create lists in the actual palette, which is what we render from.

        # this is pretty lazy; building a temp colour object to avoid unpacking the string myself
        my $colour = Wx::Colour->new($colour_str);
        push @{ $self->palette }, [ $colour->Red, $colour->Green, $colour->Blue ];
        push @{ $self->scene->palette }, $colour_str;
        $self->palette_index->{$colour_str} = $#{ $self->palette };
        $log->debug("added colour $colour_str to palette");
    }

    return $self->palette_index->{$colour_str};
}

################################################################################
sub find_tile { #{{{1
    my ($self, $grid_key, $data_hash) = @_;

    # we need to look in the transient_grid occasionally, but usually in the scene's grid.
    $data_hash ||= $self->grid;

    my $tile;

    # look for an instant hit
    $log->debug("check instant at $grid_key");
    if ($tile = $data_hash->{$grid_key}) {
        $log->debug("found instant $tile->{shape}");
    }
    else {
        # $log->info("no instant hit at $grid_key");

        # have to pull apart the key to check other halves
        $log->logdie("bad grid key $grid_key") unless $grid_key =~ /\A([A-Z]+)_(-?\d+)_(-?\d+)_(-?\d+)\z/;
        my ($facing, $left, $top, $right) = ($1,$2,$3,$4);

        if ($facing eq $SH_LEFT) {
            
            # check for a right facing tile on the same point (anchoring a SH_RIGHT)
            $grid_key =~ s/L/R/;
            $log->debug("check $grid_key");
            $tile = $data_hash->{$grid_key};
            if (! $tile || $tile->{shape} ne $SH_RIGHT) {
                $log->debug("not a R tile at $grid_key");

                # move up one and check for a right facing tile anchoring a SH_LEFT
                $left++;
                $right++;
                $tile = $data_hash->{ "R_${left}_${top}_${right}" };

                # must be a LEFT tile to be a valid match
                if ($tile && $tile->{shape} ne $SH_LEFT) {
                    $log->debug("found $tile->{shape} instead of L, clear it");
                    $tile = undef;
                }

#                $log->debug("found tile B at R_${left}_${top}_${right} " . Dumper($tile)) if $tile;
            }
            else {
                $log->debug("found tile A at $grid_key");
            }
        }
        else {

            # right down, top down and check for a left facing tile anchoring a SH_TOP
            $right--;
            $top--;
            $tile = $data_hash->{ "L_${left}_${top}_${right}" };

            # must be a TOP tile to be a valid match
            if ($tile && $tile->{shape} ne $SH_TOP) {
                $log->debug("found $tile->{shape} instead of L, clear it");
                $tile = undef;
            }

#            $log->debug("found tile C at L_${left}_${top}_${right} " . Dumper($tile)) if $tile;
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
        L => [ 2,1,0,1,-1 ],
        T => [ 0,2,1,-1,1 ],
        R => [ 0,1,2,1,1 ],
    );
    my @axes = @{ $shape_axes{$shape} };
    my @mins = (List::Util::min($start[$axes[0]], $current[$axes[0]]), List::Util::min($start[$axes[1]], $current[$axes[1]]));
    my @maxes = (List::Util::max($start[$axes[0]], $current[$axes[0]]), List::Util::max($start[$axes[1]], $current[$axes[1]]));
    $log->debug("mins [ @mins ], maxes [ @maxes ]");

    my $axis_1_coeff = pop @axes;
    my $axis_0_coeff = pop @axes;

    # this is how we check for triangles in the other facing once we've established there isn't a full 
    # tile at a location. These offsets may be defined (or at least implied) as part of some other structure
    # if you looked hard enough...
    # The top level key is the current shape, the second level is the facing we're marking with (so
    # we're looking for triangles with the other facing). The list are offsets to the current coord.
    my $shape_triangle_shift = {
        $SH_LEFT => {
            L => [ -1, 0, -1 ],
            R => [ 1, 0, 1 ],
        },
        $SH_TOP => {
            L => [ -1, 1, 0 ],
            R => [ 1, -1, 0 ],
        },
        $SH_RIGHT => {
            L => [ 0, 0, 0 ],
            R => [ 0, 0, 0 ],
        },
    };
    my $other_facing = $facing eq 'L' ? 'R' : 'L';
    my $triangle_shift = $shape_triangle_shift->{ $shape }->{ $facing };

    my $action = $self->frame->action;

    wxTheApp->set_frame_title(($maxes[0] - $mins[0] + 1) . "x" . ($maxes[1] - $mins[1] + 1));

    my @point;
    for my $coord_0 ($mins[0] .. $maxes[0]) {
        for my $coord_1 ($mins[1] .. $maxes[1]) {

            $point[ $axes[0] ] = $coord_0;
            $point[ $axes[1] ] = $coord_1;
            $point[ $axes[2] ] = $axis_0_coeff * $coord_0 + $axis_1_coeff * $coord_1; 

            if ($action =~ /$IsoFrame::AC_PAINT|$IsoFrame::AC_SHADE_CUBE/o) {

                if (my $key = $self->paint_tile(@point, $facing, $shape)) {
#                    $marked_hash->{$key} = 1;
                }

            }
            else {

                # action is select or erase

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

                # if we're matching all shapes, look for triangles with the other facing.
                if ($action eq $IsoFrame::AC_ERASE_ALL || $action eq $IsoFrame::AC_SELECT_ALL) {

                    # triangle handling; we have to check for triangles starting in the
                    # other facing, which isn't being done by find_tile because you need to
                    # know the current shape to know where to extend the search.
                    my @tpoint = ($point[0] + $triangle_shift->[0], $point[1] + $triangle_shift->[1], $point[2] + $triangle_shift->[2]);
                    my $grid_key = "${other_facing}_$tpoint[0]_$tpoint[1]_$tpoint[2]";
                    $log->debug("check triangle key $grid_key");
                    if (my $tile = $self->find_tile ( $grid_key )) {
                        if ($tile->{shape} =~ /T[LR]/) {
                            $grid_key = join('_', @{ $tile }{qw( facing left top right )});
                            $log->debug("triangle key $grid_key found $tile->{shape}");
                            $marked_hash->{$grid_key} = 1;
                        }
                    }

                }
            }
        }
    }

    if ($action eq $IsoFrame::AC_SHADE_CUBE) {
        if ($shape eq $SH_RIGHT) {

            for my $coord_1 ($mins[1] .. $maxes[1]) {
                $point[ $axes[0] ] = $maxes[0];
                $point[ $axes[1] ] = $coord_1;
                $point[ $axes[2] ] = $axis_0_coeff * $maxes[0] + $axis_1_coeff * $coord_1; 
                $self->shade_cube(@point, $facing, $shape);
            }

            for my $coord_0 (reverse $mins[0] .. ($maxes[0]-1)) {
                $point[ $axes[0] ] = $coord_0;
                $point[ $axes[1] ] = $mins[1];
                $point[ $axes[2] ] = $axis_0_coeff * $coord_0 + $axis_1_coeff * $mins[1]; 
                $self->shade_cube(@point, $facing, $shape);
            }

        }
        elsif ($shape eq $SH_LEFT) {
            for my $coord_0 (reverse $mins[0] .. $maxes[0]) {
                $point[ $axes[0] ] = $coord_0;
                $point[ $axes[1] ] = $maxes[1];
                $point[ $axes[2] ] = $axis_0_coeff * $coord_0 + $axis_1_coeff * $maxes[1]; 
                $self->shade_cube(@point, $facing, $shape);
            }

            for my $coord_1 (reverse $mins[1] .. $maxes[1]-1) {
                $point[ $axes[0] ] = $maxes[0];
                $point[ $axes[1] ] = $coord_1;
                $point[ $axes[2] ] = $axis_0_coeff * $maxes[0] + $axis_1_coeff * $coord_1; 
                $self->shade_cube(@point, $facing, $shape);
            }
        }
        elsif ($shape eq $SH_TOP) {
            for my $coord_0 ($mins[0] .. $maxes[0]) {
                $point[ $axes[0] ] = $coord_0;
                $point[ $axes[1] ] = $mins[1];
                $point[ $axes[2] ] = $axis_0_coeff * $coord_0 + $axis_1_coeff * $mins[1]; 
                $self->shade_cube(@point, $facing, $shape);
            }

            for my $coord_1 ($mins[1] + 1 .. $maxes[1]) {
                $point[ $axes[0] ] = $mins[0];
                $point[ $axes[1] ] = $coord_1;
                $point[ $axes[2] ] = $axis_0_coeff * $mins[0] + $axis_1_coeff * $coord_1; 
                $self->shade_cube(@point, $facing, $shape);
            }
        }
    }

    return 1;
}

################################################################################
sub paint_tile { #{{{1
    my ($self, $left, $top, $right, $facing, $shape, $brush_index) = @_;
    my $debug_logging = $log->debug("paint_tile @_");

    # check all args are defined
#    $log->logconfess("bad paint : " . Dumper(\@_)) unless (scalar @_ == scalar grep { defined $_ } @_);
    $log->logconfess("bad shape $shape") unless $shape =~ /[LTR]/;

    # change the grid key according to the shape if we're in the wrong triangle;
    if ($shape eq $SH_LEFT && $facing eq $SH_LEFT) {
        $left++;
        $right++;
    }
    elsif ($shape eq $SH_TOP && $facing eq $SH_RIGHT) {
        $right--;
        $top--;
    }

    # just ensure the facing is right, don't bother checking it
    $facing = $Shape_facing{$shape};

    # check for the same shape in the same place; we will just change the colour of that tile
    # if we find it.
    my $grid_key = "${facing}_${left}_${top}_${right}";
    if (my $tile = $self->grid->{$grid_key}) {
        if ($tile->{shape} eq $shape) {
            $log->debug("duplicate tile, change colour TODO") if $debug_logging;

            if (wxTheApp->config->repaint_same_tile) {
                $tile->{brush_index} = defined $brush_index ? $brush_index : $self->brush_index;
                $log->debug("repaint_same_tile with $tile->{brush_index}") if $debug_logging;

                # if this tile wasn't part of new, make it so (and rebuild permanent)
                $self->key_hash->{new}->{$grid_key} = 1;
                return $grid_key;
            }
            else {
                return 0;
            }
        }
    }

    my $action = $self->frame->action;

    # check for existing tiles that overlap.
    my $doing_shade_area = $action eq $IsoFrame::AC_SHADE_CUBE && $self->frame->area_mode;
    my $shifts = 0;
    $log->logconfess("no clashes for $shape") unless $Shape_clashes{$shape};
    my $grid = $self->grid;
    my $transient_grid = $self->transient_grid;
    for my $shift_point ( @{ $Shape_clashes{$shape} } ){
#        $log->debug("shift_point for $shape " . Dumper($shift_point)) if $debug_logging;
        my (undef, undef, undef, undef, $shift_flag, $clash_list ) = @{ $shift_point };
        for my $clash ( @{ $clash_list } ) {
#            $log->debug("clash " . Dumper($clash)) if $debug_logging;
            my ($left_clash_offset, $top_clash_offset, $right_clash_offset, $clash_shape) = @{ $clash };

            my $grid_key = $Shape_facing{$clash_shape}
                . '_' . ($left + $left_clash_offset)
                . '_' . ($top + $top_clash_offset)
                . '_' . ($right + $right_clash_offset);

            my $tile;
            if ($tile = $grid->{$grid_key}) {
                $log->debug("found a clashing tile by key at $grid_key") if $debug_logging;
                if ($tile->{shape} eq $clash_shape) {
                    $log->debug("clash tile matches shape $clash_shape, add shift_flag $shift_flag") if $debug_logging;
                    $shifts += $shift_flag;
                    last;
                }
            }

            # if we're shading an area, we have to check for clashes in transient_grid as well
            elsif ($doing_shade_area && ($tile = $transient_grid->{$grid_key})) {
                if ($tile->{shape} eq $clash_shape) {
                    $shifts += $shift_flag;
                    last;
                }
            }
        }
    }

    # check what clashes we found
    if ($shifts == 1 || $shifts == 2) {

        # clash with one point only, so shift to a new shape (ie a triangle) as required
        my ($left_shift_offset, $top_shift_offset, $right_shift_offset, $new_shape, $shift_flag ) = @{ $Shape_clashes{$shape}->[$shifts - 1] };

        $left += $left_shift_offset;
        $top += $top_shift_offset;
        $right += $right_shift_offset;
        $shape = $new_shape;
        $facing = $Shape_facing{$shape};
    }
    elsif ($shifts == 3) {
        $log->debug("clashes with existing cell(s)") if $debug_logging;
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
        'new' => 1,
    };

    $log->debug("paint_tile $shape at $left, $top, $right, $facing") if $debug_logging;

    # TODO I think this covers all cases when we should use transient_grid but check it
    if ($self->area_start || $action eq $IsoFrame::AC_PASTE) {
        $transient_grid->{$grid_key} = $tile;
    }
    else {
        $grid->{$grid_key} = $tile;
        $self->key_hash->{new}->{$grid_key} = 1;
    }

    return $grid_key;
}

################################################################################
sub shade_cube { #{{{1
    my ($self, $left, $top, $right, $facing, $paint_shape) = @_;
    $log->debug("shade_cube $left, $top, $right, $facing, $paint_shape");

    my $frame = $self->frame;
    my $refresh;
    my $area_mode = $frame->area_mode;

    # try to find a tile initially; we don't want to use paint_tile as that will paint
    # a triangle if it can, ie in the "other" half of the tile. If we hit anything,
    # we want to use that as the start of a cube. If we're in area mode
    # we should look in transient_grid as well as grid.
    my $tile = $self->find_tile("${facing}_${left}_${top}_${right}", $area_mode ? $self->transient_grid : undef);

    # if we didn't find a tile in transient, check grid as well
    if (! $tile && $area_mode) {
        $tile = $self->find_tile("${facing}_${left}_${top}_${right}");
    }

    # if there was no tile present, paint at this spot and then try the find again;
    # this will result in all 3 sides appearing with one click.
    unless ($tile) {

        # if we're called in area mode, the tile should always exist (it was just painted by
        # mark_area).
        $log->logconfess("painting in shade_cube in area mode") if $area_mode;

        if (my $grid_key = $self->paint_tile($left, $top, $right, $facing, $paint_shape)) {
            $refresh = 1;

            # can't be in area mode here, so painting to the grid, not transient_grid
            $self->add_undo_action($IsoFrame::AC_PAINT, $self->grid->{$grid_key});
            $tile = $self->find_tile("${facing}_${left}_${top}_${right}");
        }
        else {

            # don't see how this could happen; we could neither find nor paint at this location
            $log->logdie("huh? at ${facing}_${left}_${top}_${right}");
        }
    }

    if ($tile) {

#        $log->debug("found a tile to shade_cube " . Dumper($tile));

        # if the tile is a triangle, we try to infer the shape by matching its colour
        # against the current colour.  If we can't, we do nothing.
        my $cube_side;
        if ($tile->{shape} =~ /T[LR]/) {
            for my $side (qw(L T R)) {
                my $palette_index = $self->get_palette_index($frame->cube_brush->{$side}->GetColour->GetAsString(wxC2S_HTML_SYNTAX));
                if ($tile->{brush_index} == $palette_index) {
                    $log->debug("matched cube side $side at $left,$top,$right");
                    $cube_side = $side;

                    # if we're in the "wrong" half, convert the grid key so it points
                    # to the location that would have been used by the tile.
                    my %triangle_delta = (
                        L => {
                            L => [ 1, 0, 1 ],
                        },
                        R => {
                            T => [ 0, -1, -1 ],
                        }
                    );
                    if (my $delta = $triangle_delta{$tile->{facing}}->{$cube_side}) {
                        $left += $delta->[0];
                        $top += $delta->[1];
                        $right += $delta->[2];
                    }
                    last;
                }
            }

        }
        else {
            $cube_side = $tile->{shape};
            $left = $tile->{left};
            $top = $tile->{top};
            $right = $tile->{right};
        }
        if ($cube_side) {
            $log->debug("paint a cube based on side $cube_side, side anchor $left,$top,$right");

            # we have a tile (which gives us a colour), a side shape (ie L, T or R, not a triangle)
            # and the logical anchor for that shape (ie regardless of whether the whole shape is present
            # or just a triangle), from which we can derive the positions of the other sides. 

            # find the shades for the other sides
            my @other_shades = $frame->find_shades($cube_side, $tile->{brush_index});
#            $log->debug("other_shades " . Dumper(\@other_shades));

            for my $other_side ( @{ $IsoFrame::OTHER_SIDES{ $cube_side } } ) {

                # find the anchor for this side
                my $offsets = $Cube_side_offset->{ $cube_side }->{ $other_side };
#                $log->debug("offsets " . Dumper($offsets));

                # convert the shade into a brush index
                my $new_rgb = shift @other_shades;
                my $brush_index = $self->get_palette_index( sprintf("#%02X%02X%02X", @{ $new_rgb }));
                $log->debug("brush_index $brush_index");

                # paint the tile
                if (my $grid_key = $self->paint_tile($left + $offsets->[0], $top + $offsets->[1], $right + $offsets->[2],
                    $other_side eq 'T' ? 'L' : 'R', $other_side, $brush_index)) 
                {
                    # only add undo action if this is a single operation, area ops are only saved when 
                    # they conclude.
                    $self->add_undo_action($IsoFrame::AC_PAINT, $self->grid->{$grid_key})
                        unless $area_mode;
                    $refresh = 1;
                }
            }

        }
    }

    return $refresh;
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

            # when we paste from the list, we want to make that item the most recent one so
            # we can paste it again from the button
            if (defined $data && $data > 0) {
                my $item = splice @{ $self->scene->clipboard }, $data, 1;
                unshift @{ $self->scene->clipboard }, $item;
            }

            # following the above, we're always pasting the first item in the list
            $self->paste_list($self->scene->clipboard->[0]->{tiles});

            # a true value here tells mouse_event_handler to paint the pasted tiles on each mouse event.
            # we change it from 1 to 2 on left-down, which means the next left-up is the paste position.
            # this is so paste operations started from the popup list don't immediately terminate
            # when the activating double-click on the list is over the canvas, and so sends a left-up 
            # (since the popup has disappeared on the second left-down).
            $self->paint_shape(1);

            $self->frame->Refresh;
        }
    }
    else {

        # copy or cut, so first step is to copy selected tiles to clipboard
        my ($min_x, $min_y, $max_x, $max_y);
        my @clipboard_tiles = ();

        for my $tile (map { $self->grid->{$_} } keys %{ $self->key_hash->{select} }) {

            # create a copy to put on the clipboard, we don't want the extra fields added 
            # to the scene copy.
            my $clipboard_tile = { %{ $tile } };

            # we need the tile colour, shape and the relative position; the anchor
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
        }

        # bail if no selections
        return unless @clipboard_tiles;

        # clear the selections; cut tiles are gone, and it's been found to be annoying to have the
        # selections retained after copy.
        if ($operation eq 'copy') {
            map { delete $self->grid->{$_}->{selected} } keys %{ $self->key_hash->{select} };
        }
        $self->key_hash->{select} = {};
        $self->rebuild_render_group('select');

        # we've got the clipboard tiles in the list, we can do the cut now if req.
        if ($operation eq 'cut') {
            my $grid_keys = [ map { "$_->{facing}_$_->{left}_$_->{top}_$_->{right}" } @clipboard_tiles ];

            # must record undo before we remove
            $self->add_undo_action($IsoFrame::AC_ERASE, $grid_keys);

            my ($refresh_permanent,$refresh_new) = $self->remove_tiles($grid_keys);
            $self->rebuild_render_group('permanent') if $refresh_permanent;
            $self->rebuild_render_group('new') if $refresh_new;
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

        my $palette = $self->palette;

        # to precisely centre the shape in the image, adjust by the distance from
        # the centre tile anchor to the centre
        my $x_adjustment = $min_x + (($max_x - $min_x) / 2) - $centre_tile->{x};
        my $y_adjustment = $min_y + (($max_y - $min_y) / 2) - $centre_tile->{y};

        my %brush_cache;
        for my $tile (@clipboard_tiles) {

#            $log->debug("palette at $tile->{brush_index} is " . Dumper($palette->[ $tile->{brush_index} ]));

            # we're still using DC graphics to build the thumbnail; just make brushes as we go
            $brush_cache{ $tile->{brush_index} } ||= Wx::Brush->new( Wx::Colour->new( @{ $palette->[ $tile->{brush_index} ] } ), wxBRUSHSTYLE_SOLID );
            $thumb_dc->SetBrush($brush_cache{ $tile->{brush_index} });

            my $shape = $DC_tile_shape->{ $tile->{shape} }
                or $log->logdie("bad tile shape $tile->{shape}");

            $thumb_dc->DrawPolygon(
                $shape->{polygon_points}, 
                $tile->{top} * $self->x_grid_size - $x_adjustment,
                (- $tile->{top} / 2 - $tile->{left}) * $self->y_grid_size - $y_adjustment,
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

    $self->Refresh;

    return;
}

################################################################################
# remove the specified tiles and return 3 things; the list of keys (required if
# we're called from cut or area erase and need to create an undo action) and
# 2 flags indicating whether permanent and/or new tiles were removed.
sub remove_tiles { #{{{1
    my ($self, $grid_keys) = @_;

    my ($refresh_permanent, $refresh_new);

    for my $grid_key ( @{ $grid_keys } ) {
        $log->debug("remove $grid_key");
        delete $self->grid->{$grid_key};
        if ($self->key_hash->{new}->{$grid_key}) {
            $refresh_new = 1;
            delete $self->key_hash->{new}->{$grid_key};
        }
        else {
            $refresh_permanent = 1;
        }

        # can't be visible or selected once it's deleted...
        delete $self->key_hash->{select}->{$grid_key};
        delete $self->key_hash->{visible}->{$grid_key};
    }

    return $refresh_permanent, $refresh_new;
}

################################################################################
sub calculate_grid_dims { #{{{1
    my ($self, $side) = @_;

    $self->x_grid_size(my $x = ( cos (deg2rad(30)) * ($side / 1)) );
    $self->y_grid_size(my $y = $side );
    $self->control_gradient( ($y / 2) / $x);

    $Tile_shape = {
        L => [
            [ 0, 0 ],
            [ 0, - $y ],
            [ $x, - $y / 2 ],

            [ $x, - $y / 2 ],
            [ 0, - $y ],
            [ $x, - $y * 1.5 ],
        ],
        T => [
            [ 0, 0 ],
            [ $x, - $y / 2 ],
            [ $x, $y / 2 ],

            [ $x, $y / 2 ],
            [ $x, - $y / 2 ],
            [ $x * 2, 0 ],
        ],
        R => [
            [ 0, 0 ],
            [ 0, - $y ],
            [ $x, - $y / 2 ],

            [ $x, - $y / 2 ],
            [ $x, $y / 2 ],
            [ 0, 0 ],
        ],
        TR => [
            [ 0, 0 ],
            [ 0, -$y ],
            [ $x, -$y / 2 ],
        ],
        TL => [
            [ 0, 0 ],
            [ $x, - $y / 2 ],
            [ $x, $y / 2 ],
        ],
    };

    # We still need Wx::Point lists to draw thumbnails for clipboard items;
    # shapes are converted into Wx::Point lists for use by DrawPolygon below.
    $DC_tile_shape = {
        L => {
            points => [
                $Tile_shape->{L}->[0],
                $Tile_shape->{L}->[2],
                $Tile_shape->{L}->[5],
                $Tile_shape->{L}->[4],
            ],
        },
        T => {
            points => [
                $Tile_shape->{T}->[0],
                $Tile_shape->{T}->[1],
                $Tile_shape->{T}->[5],
                $Tile_shape->{T}->[2],
            ],
        },
        R => {
            points => [
                $Tile_shape->{R}->[0],
                $Tile_shape->{R}->[1],
                $Tile_shape->{R}->[2],
                $Tile_shape->{R}->[4],
            ],
        },
        TR => {
            points => [
                $Tile_shape->{TR}->[0],
                $Tile_shape->{TR}->[2],
                $Tile_shape->{TR}->[1],
            ],
        },
        TL => {
            points => [
                $Tile_shape->{TL}->[0],
                $Tile_shape->{TL}->[2],
                $Tile_shape->{TL}->[1],
            ],
        },
    };

    for my $shape (values %{ $DC_tile_shape }) {
        $shape->{polygon_points} = [];
        for my $point (@{ $shape->{points} }) {

            # the openGL shapes need to be converted for DC use so
            # swap Y coord signs
            push @{ $shape->{polygon_points} }, Wx::Point->new($point->[0], - $point->[1]);
        }
    }

    # create the vertices for the tick marks by scaling down the tile shapes;
    # also add an offset to position the tick somewhere suitable.
    my $factor = 2;
    my %tick_offset = (
        L => [ $x * 0.95 * ($factor - 1) / $factor, - $y * 1.05 * ($factor - 1) / ($factor * 2) ],
        T => [ $x * 0.95 * ($factor - 1) * 2 / $factor, 0 ],
        R => [ $x * 0.95 * ($factor - 1) / $factor, $y * 0.85 * ($factor - 1) / ($factor * 2) ],

        # these don't scale with factor, they were done by hand for factor == 2
        TR => [ $x / 40, - $y * 0.46 ],
        TL => [ $x / 2.1, - $y * 0.21 ],
    );

    $Tick_shape = {};
    for my $shape (keys %{ $Tile_shape }) {
        $Tick_shape->{$shape} = [];
        my $offset = $tick_offset{$shape};
        for my $vertex (@{ $Tile_shape->{$shape} }) {
            push @{ $Tick_shape->{$shape} }, 
                [
                    $vertex->[0] / $factor + $offset->[0],
                    $vertex->[1] / $factor + $offset->[1],
                ];
        }
    }

    $log->debug("xgs $x, ygs $y");

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

    my $frame = $self->frame;

    # we need to know what the logical extents of the scene are.

    # find extents in terms of grid keys
    my ($min_x_grid, $min_y_grid, $max_x_grid, $max_y_grid);
    for my $tile (values %{ $self->grid }) {

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

#    $self->min_x_grid($min_x_grid);
#    $self->min_y_grid($min_y_grid);
#    $self->max_x_grid($max_x_grid);
#    $self->max_y_grid($max_y_grid);

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

        # doc sizes in mm, in portrait and allowing for 5mm non-printable area
        my $sheet_size_mm = {
            a2_rbn => [ 410, 584 ],
            a3_rbn => [ 287, 410 ],
            a4_rbn => [ 200, 287 ],
            a5_rbn => [ 138, 200 ],
        };

        my ($selected_sheet_rbn) = grep { $export_option->{$_} } qw(a2_rbn a3_rbn a4_rbn a5_rbn);

        my @dims = @{ $sheet_size_mm->{ $selected_sheet_rbn } };
        @dims = reverse @dims if $export_option->{landscape_chb};

        my $dpi = $export_option->{dpi_600_rbn}
            ? 600
            : $export_option->{dpi_300_rbn}
                ? 300
                : 150;

        ($width, $height) = (int($dims[0] / 25.4 * $dpi ), int($dims[1] / 25.4 * $dpi));
    }

    $log->info("top left $min_x,$min_y to bottom right $max_x,$max_y, bitmap dims $width x $height");

    # find the scale factors for logical to device width, then use the smaller so we fit
    # both dimensions in.
    my $x_scale = ($max_x - $min_x) / $width;
    my $y_scale = ($max_y - $min_y) / $height;
    my $export_scale = max($x_scale, $y_scale);
    $log->info("export_scale from $x_scale, $y_scale : $export_scale");

    # Allocate texture buffer
    my($TextureID_FBO) = glGenTextures_p(1);

    # Allocate FBO frame and render buffers
    my($FrameBufferID) = glGenFramebuffersEXT_p(1);
    my($RenderBufferID) = glGenRenderbuffersEXT_p(1);

    # Bind frame/texture
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, $FrameBufferID);
    glBindTexture(GL_TEXTURE_2D, $TextureID_FBO);

    # Initiate texture
    glTexImage2D_c(GL_TEXTURE_2D, 0, GL_RGBA8, $width, $height, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    # Bind frame/render buffers
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, $TextureID_FBO, 0);
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, $RenderBufferID);
    glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24, $width, $height);
    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, $RenderBufferID);

    # Test status
    my $stat = glCheckFramebufferStatusEXT(GL_RENDERBUFFER_EXT);
    $log->info(sprintf("Status: %04X\n",$stat));

    # Draw to texture
    my $scale = $self->scene->scale;
    my @origin = ($self->scene->origin_x, $self->scene->origin_y);
    $self->scene->scale($export_scale);
    $self->scene->origin_x(- $min_x);
    $self->scene->origin_y(- $min_y);

    $self->Render($width, $height);
#    $self->Render;

    $self->scene->scale($scale);
    $self->scene->origin_x($origin[0]);
    $self->scene->origin_y($origin[1]);

    # we don't swap buffers here; we've rendered the export image into the back buffer, but
    # we never want to see it.
    glReadBuffer(GL_BACK);

    my $image = new OpenGL::Image(width => $width, height => $height);
    my ($fmt, $size) = $image->Get('gl_format', 'gl_type');

    OpenGL::glReadPixels_c(0, 0, $width, $height, $fmt, $size, $image->Ptr());

    my $name = $self->scene->filename . localtime->strftime("_%Y%m%d_%H%M%S") . '.tga';
    $image->Save($name);

    # Release binding
    glBindRenderbufferEXT( GL_RENDERBUFFER_EXT, 0 );
    glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, 0 );

    # Delete frame/render buffers
    # NOTE: Should cache to improve performance
    glDeleteRenderbuffersEXT_p( $RenderBufferID );
    glDeleteFramebuffersEXT_p( $FrameBufferID );

    $log->debug("export done");
    $self->SetCursor(wxCROSS_CURSOR);

    return;
}

################################################################################
# action is whatever we've just done, data is either a tile or a list of grid keys.
sub add_undo_action { #{{{1
    my ($self, $action, $data) = @_;

    # we want a reference to a list of tiles; we've been passed either a list of keys or a single tile reference.
    my $undo_data = [ ((ref $data) eq 'ARRAY') 
        ? map { $self->grid->{$_} } @{ $data } 
        : $data
    ];

    my $new_action = [ $action, $undo_data ];

    # Are there any items on the redo stack?
    if (my $new_branch_node = pop @{ $self->scene->redo_stack }) {

        if ((ref $new_branch_node) eq 'HASH' || wxTheApp->config->automatic_branching) {

            $log->debug("create branch then add new action");
            
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
            $new_branch_node->{branches}->[ $current_branch ] = {
                last_current_at => scalar localtime,
                actions => [ @{ $self->scene->redo_stack }, $new_branch_node->{branches}->[$current_branch],  ],
            };

            # add the new branch (containing the new undo action) and make it current
            push @{ $new_branch_node->{branches} }, $new_action;
            $new_branch_node->{current_branch} = $#{ $new_branch_node->{branches} };

            push @{ $self->scene->undo_stack }, $new_branch_node;
        }
        else {

            # we're not doing automatic branching and the top redo item wasn't a branch node
            # so we're discarding it. Just add the undo action.
            push @{ $self->scene->undo_stack }, $new_action;
        }

        # clear the redo stack now we've saved it to the branch (if we're branching)
        $self->scene->redo_stack([]);

    }
    else {
        push @{ $self->scene->undo_stack }, $new_action;
    }

    $self->set_undo_redo_button_states;

    return;
}

################################################################################
# returns true if the action was possible.
sub undo_or_redo { #{{{1
    my ($self, $redo_flag, $no_refresh, $many) = @_;
    $log->debug("undo_or_redo");

    # if we didn't get an explicit type, it's a timer and we check the flag set on LEFT_DOWN.
    unless (defined $redo_flag) {
        $redo_flag = $self->frame->undo_or_redo_flag;
    }

    # count is 1 or the many_count config item
    my $count = $many ? wxTheApp->config->undo_many_count : 1;

    # undo and redo are conceptually the same; they take an action off a stack, either paint or erase a list of tiles 
    # depending on the action type and then put it on another stack. They only differ in the choice of stack
    # and action.
    my ($source_stack, $target_stack, $erase_action, $paint_action) = $redo_flag
        ? ($self->scene->redo_stack, $self->scene->undo_stack, $IsoFrame::AC_ERASE, $IsoFrame::AC_PAINT)  
        : ($self->scene->undo_stack, $self->scene->redo_stack, $IsoFrame::AC_PAINT, $IsoFrame::AC_ERASE);

    my ($refresh_new, $refresh_permanent);
    while ($count && (my $next_action = pop @{ $source_stack } )) {
        $count--;

        push @{ $target_stack }, $next_action;

        # Ok, make the change to the scene.
        # Turn $next_action into a real action if it's a branch node
        if ((ref $next_action) eq 'HASH') {
            $log->debug("get real action from branch node");
            $next_action = $next_action->{branches}->[ $next_action->{current_branch} ];
        }

        my ($action, $tiles) = @{ $next_action };
        if ($action eq $erase_action) {
            my $grid_keys = [ map { "$_->{facing}_$_->{left}_$_->{top}_$_->{right}" } @{ $tiles } ];
            ($refresh_permanent, $refresh_new) = $self->remove_tiles($grid_keys);
        }
        elsif ($action eq $paint_action) {
            for my $tile ( @{ $tiles } ) {
                my $grid_key = "$tile->{facing}_$tile->{left}_$tile->{top}_$tile->{right}";
                $self->grid->{$grid_key} = $tile;
                $log->debug("undo/redo: paint $grid_key");

                # put all repainted tiles in new
                $refresh_new = 1;
                $self->key_hash->{new}->{$grid_key} = 1;
                $self->key_hash->{select}->{$grid_key} = 1 if $tile->{selected};
            }
        }
    }

    if (($refresh_permanent || $refresh_new) && ! $no_refresh) {
        $self->rebuild_and_refresh($refresh_permanent, $refresh_new);
    }

    # return true if we did any work
    return $refresh_permanent || $refresh_new;
}

################################################################################
sub rebuild_and_refresh { #{{{1
    my ($self, $refresh_permanent, $refresh_new) = @_;

    $self->set_undo_redo_button_states;
    $self->rebuild_render_group('permanent') if $refresh_permanent;
    $self->rebuild_render_group('new') if $refresh_new;
    $self->rebuild_render_group('select');
    $self->Refresh;

    return;
}

################################################################################
sub change_to_branch { #{{{1
    my ($self, $branch_index, $redo_flag) = @_;
    $log->info("change_to_branch branch_index $branch_index, redo_flag $redo_flag");

    # make the selected branch current

    my $branch_node = pop @{ $self->scene->redo_stack };
    return 0 unless (ref $branch_node) eq 'HASH';
    return 0 unless $branch_index <= $#{ $branch_node->{branches} };

    # 1. Turn existing current branch into list of redo_stack + current action
    # data for the current branch is an action; turn this into a list of actions from the redo stack plus the existing current action
    # from the branch node.
    my $current_branch = $branch_node->{current_branch};
    $log->info("current_branch $current_branch");
    $branch_node->{branches}->[ $current_branch ] = {
        last_current_at => scalar localtime,
        actions => [ @{ $self->scene->redo_stack }, $branch_node->{branches}->[$current_branch],  ],
    };

    # 2. Set redo stack to new branch list
    $self->scene->redo_stack( $branch_node->{branches}->[$branch_index]->{actions} );

    # 3. Set current branch index to new value
    $branch_node->{current_branch} = $branch_index;

    # 4. Pop new action stack into new branch attribute in branch node
    $branch_node->{branches}->[ $branch_index ] = pop @{ $self->scene->redo_stack };

    # 5. Push the branch node onto the redo stack
    push @{ $self->scene->redo_stack }, $branch_node;

    if ($redo_flag) {

        # redo all the way along this branch
        $self->redo_all_actions;
    }

    return 1;
}

################################################################################
# We should undo until no harkonnen breathes arrakeen air. And the stack position
# matches the specified position.
# This could probably be handled by the undo-many feature TODO.
sub undo_to_position { #{{{1
    my ($self, $undo_stack_position) = @_;

    if ($#{ $self->scene->undo_stack } < $undo_stack_position) {
        $log->logconfess("current undo stack is smaller than undo_stack_position $undo_stack_position : " . $#{ $self->scene->undo_stack });
    }

    # perform all actions but don't refresh after each one
    while ($#{ $self->scene->undo_stack } > $undo_stack_position) {
        $self->undo_or_redo(0,1);
    }

    # refresh everything
    $self->rebuild_render_group('permanent');
    $self->rebuild_render_group('new');
    $self->rebuild_render_group('select');

    $self->set_undo_redo_button_states;
    $self->Refresh;

    return;
}

################################################################################
# Redo all available actions.
sub redo_all_actions { #{{{1
    my ($self) = @_;

    while (@{ $self->scene->redo_stack }) {
        $self->undo_or_redo(1,1);
    }

    # refresh everything
    $self->rebuild_render_group('permanent');
    $self->rebuild_render_group('new');
    $self->rebuild_render_group('select');

    $self->set_undo_redo_button_states;
    $self->Refresh;

    return;
}

################################################################################
sub set_undo_redo_button_states { #{{{1
    my ($self) = @_;
    $log->debug("set_undo_redo_button_states");

    # if the top action on the redo stack is a branch node, indicate this on the button
    my $top_redo_index = $#{ $self->scene->redo_stack };
    my $redo_tooltip = ($top_redo_index + 1) . " actions.";
    my $redo_button_bitmap = 'redo';
    if ( $top_redo_index >= 0 && (ref $self->scene->redo_stack->[$top_redo_index]) eq 'HASH' ) {
        $redo_button_bitmap = 'branch_redo';
        $redo_tooltip .= ' ' . scalar @{ $self->scene->redo_stack->[$top_redo_index]->{branches} } . ' branches available.';
    }

    IsoApp::set_button_bitmap($self->frame->misc_btn->{'redo'}, $redo_button_bitmap);
    $self->frame->misc_btn->{'redo'}->SetToolTip($redo_tooltip);
    $self->frame->misc_btn->{'redo'}->Enable($top_redo_index >= 0);

    my $undo_count = scalar @{ $self->scene->undo_stack };
    $self->frame->misc_btn->{'undo'}->SetToolTip("$undo_count actions.");
    $self->frame->misc_btn->{'undo'}->Enable($undo_count);

    # if we've hit either end of the undo/redo sequence, stop the timer; if we've
    # come from the button, we're aren't going to get the button up event which usually does this.
    unless ($self->frame->misc_btn->{'redo'}->IsEnabled && $self->frame->misc_btn->{'undo'}->IsEnabled) {
        $self->frame->undo_timer->Stop;
    }

    return 1;
}

################################################################################
sub move_origin { #{{{1
    my ($self, $device_x, $device_y) = @_;

    $self->scene->origin_x ( $self->scene->origin_x - $self->scene->scale * ($self->last_device_x - $device_x));
    $self->scene->origin_y ( $self->scene->origin_y - $self->scene->scale * ($self->last_device_y - $device_y));
    $log->debug(sprintf("origin %d, %d, origin mod grid %d, %d", $self->scene->origin_x, $self->scene->origin_y, $self->scene->origin_x % $self->x_grid_size, $self->scene->origin_y % $self->y_grid_size));

#    $self->tile_cache(0);

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
#    push @{ $self->tile_cache }, \@imported_tiles;

    $self->cursor_multiplier_x(1);
    $self->cursor_multiplier_y(1);

    return;
}

################################################################################
sub set_selection_for_all { #{{{1
    my ($self, $flag) = @_;

    for my $tile (values %{ $self->grid }) {
        $tile->{selected} = $flag;
        if ($flag) {
            $self->key_hash->{select}->{"$tile->{facing}_$tile->{left}_$tile->{top}_$tile->{right}"} = 1;
        }
        else {
            delete $self->key_hash->{select}->{"$tile->{facing}_$tile->{left}_$tile->{top}_$tile->{right}"};
        }
    }
    $self->rebuild_render_group('select');
    $self->Refresh;

    return;
}

################################################################################
sub select_visible { #{{{1
    my ($self) = @_;

    $self->mark_visible_tiles;
    for my $key (keys %{ $self->key_hash->{visible} }) {
        $self->grid->{$key}->{selected} = 1;
        $self->key_hash->{select}->{$key} = 1;
    }
    $self->rebuild_render_group('select');
    $self->Refresh;

    return;
}

################################################################################

# TODO {{{1

# change cursor color when over red tiles

# autohide error messages

# copy/cut clears selection

# single click to fill in empty sides with shades of clicked tile

# undo stack to include selection (?)

# line width / brush shape

# floodfill

# list of previous colour combinations

# undo/redo multiple actions at once, eg one press of the undo button goes back 10 steps.

# layers, ideally with transparency. Much more possible now we've got OpenGL.

# textures; this may be against the spirit of the program, and may be prohibitively slow.

# art ideas;
#   Ravenloft-style maps
#   Cross-sectional scenes, eg mines underground, workings above.
#   Sets of small related objects or scenes. Zodiac, famous buildings, workshop machines

1;
