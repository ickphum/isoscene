#$Id: IsoScene.pm 148 2012-10-27 04:01:09Z ikm $
package IsoConfig;

use strict;
use warnings;

use base qw(Class::Accessor::Fast);
use Data::Dumper;
use YAML::XS qw(DumpFile LoadFile);
use Storable qw(dclone);
use Log::Log4perl qw(get_logger);

my $log = get_logger;

my $CONFIG_FILENAME = 'iso.yml';

__PACKAGE__->mk_accessors( qw(
    previous_scene_files
    autosave_on_exit
    autosave_period_seconds
    autosave_idle_seconds
    use_compressed_files
    use_binary_files
    script_delay_milliseconds
    undo_wait_milliseconds
    undo_repeat_milliseconds
    undo_many_count
    undo_includes_view
    repeated_pasting
    repaint_same_tile
    automatic_branching
    shade_change
    darken_lighten_change
    relative_shades
    display_palette_index
    display_color
    display_key
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
        previous_scene_files => [],
        autosave_on_exit => 1,
        autosave_period_seconds => 60,
        autosave_idle_seconds => 2,
        use_compressed_files => 1,
        use_binary_files => 1,
        script_delay_milliseconds => 50,
        undo_wait_milliseconds => 200,
        undo_repeat_milliseconds => 20,
        undo_many_count => 100,
        undo_includes_view => 0,
        repeated_pasting => 1,
        repaint_same_tile => 0,
        automatic_branching => 0,
        shade_change => 30,
        darken_lighten_change => 30,
        relative_shades => "L,1,T,0,R,2",
        display_palette_index => 0,
        display_color => 0,
        display_key => 0,
        default_scene_file => 'Start',
        default_scene_scale => 1,
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

    DumpFile($CONFIG_FILENAME, $self);

    return;
}

################################################################################

1;
