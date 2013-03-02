#$Id: IsoScene.pm 148 2012-10-27 04:01:09Z ikm $
package IsoConfig;

use base qw(Class::Accessor::Fast);
use Data::Dumper;
use YAML::XS qw(DumpFile LoadFile);
use Storable qw(dclone);
use Log::Log4perl qw(get_logger);

my $log = get_logger;

my $CONFIG_FILENAME = 'iso.yml';

__PACKAGE__->mk_accessors( qw(
    previous_scene_file
    autosave_period_seconds
    undo_wait_milliseconds
    undo_repeat_milliseconds
    shade_change
    relative_shades
    default_scene_file
    default_scene_scale
    default_scene_left_rgb
    default_scene_top_rgb
    default_scene_right_rgb
    default_scene_background_rgb
    default_scene_bg_line_rgb
    default_scene_tile_line_rgb
));

################################################################################
sub new { # {{{1
    my( $class, $arg ) = @_;

    my $default_config = {
        autosave_period_seconds => 60,
        undo_wait_milliseconds => 200,
        undo_repeat_milliseconds => 20,
        shade_change => 30,
        relative_shades => "L,1,T,0,R,2",
        default_scene_file => 'Start',
        default_scene_scale => 0.25,
        default_scene_left_rgb => '#ca9c5b',
        default_scene_top_rgb => '#e8ba79',
        default_scene_right_rgb => '#ac7e3d',
        default_scene_background_rgb => '#AAAAAA',
        default_scene_bg_line_rgb => '#B3B3B3',
        default_scene_tile_line_rgb => '#DDDDDD',
    };

    my $config;
    if (-f $CONFIG_FILENAME) {
        $config = LoadFile($CONFIG_FILENAME);

        # make sure all new keys are added to old configs loaded from file
        for my $key (keys %{ $default_config }) {
            unless (defined $config->{$key}) {
                $config->{$key} = $default_config->{$key};
            }
        }
    }
    else {
        $config = $default_config;
    }

    my $self = $class->SUPER::new($config);

    return $self;
}

################################################################################
sub save { #{{{1
    my ($self) = @_;

    $log->debug("save config to $CONFIG_FILENAME, previous scene is " . $self->previous_scene_file);
    DumpFile($CONFIG_FILENAME, $self);

    return;
}

################################################################################

1;
