#$Id: IsoScene.pm 169 2013-03-02 05:27:05Z ikm $
package IsoScene;

use strict;
use warnings;

use base qw(Class::Accessor::Fast);
use Data::Dumper;
use YAML::XS qw(Dump Load);
use Storable qw(dclone);
use Log::Log4perl qw(get_logger);
use Wx qw(wxTheApp);
use File::Basename;
use IO::Compress::Zip qw(zip $ZipError) ;
use IO::Uncompress::Unzip qw(unzip $UnzipError) ;
use File::Slurp qw(read_file write_file);

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
    opengl
));

################################################################################
sub new { # {{{1
    my ( $class, $arg ) = @_;

    my $use_compressed_files = wxTheApp->config->use_compressed_files;
    $log->debug("new IsoScene : " . Dumper($arg));

    if ($arg->{file} && $arg->{file} !~ /\./) {
        $arg->{file} .= $use_compressed_files ? '.isz' : '.isc';
    }
    $log->debug("loading $arg->{file}");

    my $app = wxTheApp;
    my $config = $app->config;

    my $busy = Wx::BusyCursor->new;

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
        opengl => 1,
    };

    my $scene;
    if ($arg->{file} && -f $arg->{file}) {

        # we assigned default extensions above, now respect whatever extension we find; you should
        # be able to either type of file in either mode (compressed or not).

        my $unzipped_yaml;
        if ($arg->{file} =~ /\.isz\z/) {
            $log->info("load compressed file $arg->{file}");
            unless (unzip $arg->{file} => \$unzipped_yaml) {
                $log->logdie("Error during uncompression of $arg->{file} : $UnzipError");
            }
        }
        else {
            $log->info("load YML file $arg->{file}");
            $unzipped_yaml = read_file($arg->{file});
        }

        $scene = Load($unzipped_yaml);

        # we haven't added new keys to old scenes yet (see below) so we can tell opengl
        # files apart.
        unless ($scene->opengl) {
            my $new_grid = {};
            for my $tile (values %{ $scene->grid }) {
                my $left = $tile->{left};
                $tile->{left} = - $tile->{right};
                $tile->{right} = - $left;
                $new_grid->{ "$tile->{facing}_$tile->{left}_$tile->{top}_$tile->{right}" } = $tile;
            }
            $scene->grid($new_grid);
        }

        $log->info("loaded scene with this many tiles: " . scalar keys %{ $scene->grid });
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
    my ($self) = @_;

    my $busy = Wx::BusyCursor->new;

    my $use_compressed_files = wxTheApp->config->use_compressed_files;

    my $yaml_scene = Dump($self);
    
    if ($use_compressed_files) {
        zip \$yaml_scene => $self->filename . '.isz', Name => ($self->filename . '.isc')
            or $log->logdie("zip failed: $ZipError");
        $log->debug("saved to " . $self->filename . '.isz');
    }
    else {
        write_file($self->filename . '.isc', $yaml_scene);
        $log->debug("saved to " . $self->filename . '.isc');
    }

    $self->remember_filename;

    return;
}

################################################################################

1;
