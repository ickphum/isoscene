#$Id: IsoScene.pm 169 2013-03-02 05:27:05Z ikm $
package IsoScene;

use base qw(Class::Accessor::Fast);
use Data::Dumper;
use YAML::XS qw(Dump Load);
use Storable qw(dclone);
use Log::Log4perl qw(get_logger);
use Wx qw(wxTheApp);
use File::Basename;
use IO::Compress::Zip qw(zip $ZipError) ;
use IO::Uncompress::Unzip qw(unzip $UnzipError) ;

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
    tile_border
));

################################################################################
sub new { # {{{1
    my ( $class, $arg ) = @_;

    if ($arg->{file} && $arg->{file} !~ /\./) {
        $arg->{file} .= ".isz";
    }

    my $app = wxTheApp;
    my $config = $app->config;

    my $default_scene = {
        grid => {},
        origin_x => 0,
        origin_y => 0,
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
        tile_border => 'normal',
    };

    my $scene;
    if ($arg->{file}) {

        my $unzipped_yaml;
        unless (unzip $arg->{file} => \$unzipped_yaml) {
            $log->logdie("Error during uncompression of $file: $UnzipError");
            next;
        }

        $scene = Load($unzipped_yaml);
    }
    else {
        $scene = $default_scene;
    }

    # make sure all new keys are added to old scenes loaded from file
    for my $key (keys %{ $default_scene }) {
        unless (defined $scene->{$key}) {
            $scene->{$key} = $default_scene->{$key};
        }
    }

    my $self = $class->SUPER::new($scene);

    # fix old files with full paths in filename
    if ($arg->{file}) {
        $self->filename(File::Basename::basename($self->filename, '.isz'));
    }

    $self->remember_filename if $self->filename;

    $self->clipboard([]) unless $self->clipboard;

    return $self;
}

################################################################################
sub remember_filename { #{{{1
    my ($self) = @_;

    # If this file occurs lower down, remove it
    my $previous_scene_files = wxTheApp->config->previous_scene_files;
    if ((my $index = List::MoreUtils::firstidx { $_ eq $self->filename } @{ $previous_scene_files }) >= 0) {
        splice @{ $previous_scene_files }, $index, 1;
    }

    push @{ $previous_scene_files }, $self->filename;

    return;
}

################################################################################
sub save { #{{{1
    my ($self, $settings) = @_;

    my $yaml_scene = Dump($self);
    zip \$yaml_scene => $self->filename . '.isz', Name => ($self->filename . '.isc')
        or $log->logdie("zip failed: $ZipError");

    $self->remember_filename;

    $log->debug("saved to " . $self->filename . '.isz');

    return;
}

################################################################################

1;
