#$Id: IsoScene.pm 169 2013-03-02 05:27:05Z ikm $
package IsoScene;

use base qw(Class::Accessor::Fast);
use Data::Dumper;
use YAML::XS qw(DumpFile LoadFile);
use Storable qw(dclone);
use Log::Log4perl qw(get_logger);
use Wx qw(wxTheApp);
use File::Basename;

my $log = get_logger;

__PACKAGE__->mk_accessors( qw(
    grid
    origin_x
    origin_y
    scale
    size
    position
    left_rgb
    top_rgb
    right_rgb
    background_rgb
    bg_line_rgb
    tile_line_rgb
    palette
    undo_stack
    redo_stack
    filename
    clipboard
    export_options
));

################################################################################
sub new { # {{{1
    my ( $class, $arg ) = @_;

    if ($arg->{file} && $arg->{file} !~ /\./) {
        $arg->{file} .= ".isc";
    }

    my $app = wxTheApp;
    my $config = $app->config;

    my $scene = $arg->{file}
        ? LoadFile($arg->{file}) # this will crash on bad file and I'm fine with that
        : {
            grid => {},
            origin_x => 0,
            origin_y => 0,
            size => [ $app->frame->GetSizeWH ],
            position => [ $app->frame->GetScreenPosition->x, $app->frame->GetScreenPosition->y ],
            palette => [],
            undo_stack => [],
            redo_stack => [],
            clipboard => [],
            scale => $config->default_scene_scale,
            left_rgb => $config->default_scene_left_rgb,
            top_rgb => $config->default_scene_top_rgb,
            right_rgb => $config->default_scene_right_rgb,
            background_rgb => $config->default_scene_background_rgb,
            bg_line_rgb => $config->default_scene_bg_line_rgb,
            tile_line_rgb => $config->default_scene_tile_line_rgb,
            filename => $config->default_scene_file,
        };

    my $self = $class->SUPER::new($scene);

    # fix old files with full paths in filename
    if ($arg->{file}) {
        $self->filename(File::Basename::basename($self->filename, '.isc'));
    }

    $self->clipboard([]) unless $self->clipboard;

    return $self;
}

################################################################################
sub save { #{{{1
    my ($self, $settings) = @_;

    DumpFile($self->filename . '.isc', $self);

    wxTheApp->config->previous_scene_file($self->filename);
    $log->debug("saved to " . $self->filename . '.isc: ' . Dumper($self->size, $self->position));

    return;
}

################################################################################

1;
