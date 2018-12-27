#$Id: IsoScene.pm 169 2013-03-02 05:27:05Z ikm $
package IsoScene;

use strict;
use warnings;
#use threads;
#use threads::shared;

use base qw(Class::Accessor::Fast);
use Data::Dumper;
use YAML::XS qw(Dump Load);
use Storable qw(dclone freeze thaw);
use Log::Log4perl qw(get_logger);
use Wx qw(wxTheApp);
use File::Basename;
use IO::Compress::Zip qw(zip $ZipError) ;
use IO::Uncompress::Unzip qw(unzip $UnzipError) ;
use File::Slurp qw(read_file write_file);
use POSIX qw(floor);
use English qw(-no_match_vars);
use Fcntl qw(:seek);

my $log = get_logger;

my $MAX_VERSION = 2;

# OpenGL doesn't like threads; it always crashes at the end of a detached thread.
# So, we create a permanent thread to do the autosaves and only wake it up
# as required, using a semaphore. We're not doing anything related to Wx in the thread,
# otherwise it might not work at all.

use Thread::Semaphore;
my $trigger_autosave_sem = Thread::Semaphore->new(0);
my $save_in_progress_sem = Thread::Semaphore->new(1);
our $save_data : shared;

my $autosave_thread = threads->create(\&autosave_thread);
$autosave_thread->detach;

sub autosave_thread {

    # init log4perl
    $ENV{log_appenders} = "file, screen";
    $ENV{log_level}     = 'INFO';
    $ENV{log_dir}       = '.';
    $ENV{log_file_name} = 'iso_as';

    my $file = 'log4perl.conf';
    Log::Log4perl->init($file);
    my $log = get_logger();

    $log->info("autosave thread logging");

    while (1) {

        # wait for the main thread to raise the semaphore for us
        $trigger_autosave_sem->down;

        # proceed with save
#        print "autosave thread awake ", Dumper($save_data);
        my $data = thaw($save_data);
        $save_data = undef;
#        my $yaml_scene = Dump($data);
#        print "yaml_scene $yaml_scene\n";
#        print "autosave thread awake $save_data->{scene} $save_data->{frame}\n";
#        $save_data->{scene}->save;
        $data->{scene}->save($data->{config});

        print "done autosave\n";
    }
}

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
    version
    sector_size
));

################################################################################
sub new { # {{{1
    my ( $class, $arg ) = @_;

    $log->debug("new IsoScene : " . Dumper($arg));

    my $config = wxTheApp->config;
    if ($arg->{file} && $arg->{file} !~ /\./) {
        $arg->{file} .= $config->use_binary_files
            ? '.isb'
            : $config->use_compressed_files 
                ? '.isz' 
                : '.isc';
    }
    $log->debug("loading $arg->{file}") if $arg->{file};

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
        version => 1,
        sector_size => 5,
    };

    my $scene;
    my $extension;
    if ($arg->{file} && -f $arg->{file}) {

        # we assigned default extensions above, now respect whatever extension we find; you should
        # be able to load either type of file in either mode (compressed or not).

        if ($arg->{file} =~ /\.(\w+)\z/) {
            $extension = $1;
        }
    }

    # if we don't recognise the extension, load the default scene
    if ($extension && $extension =~ /\Ais[bcz]\z/) {

        my $unzipped_yaml;
        if ($extension =~ /\Ais[cz]\z/) {
            if ($arg->{file} eq 'isz') {
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
        }
        else {
            $log->info("load binary file $arg->{file}");
            $scene = load_binary_file($arg->{file}) || $default_scene; 
        }

        # Note that if $scene was loaded from YAML it will already be an IsoScene object,
        # since the type is part of the YAML file. If loaded from binary it will be a plain hash.
        # The upshot is, treat it as a hash, which works in either case.

        # we haven't added new keys to old scenes yet (see below) so we can tell opengl
        # files apart.
        unless ($scene->{opengl}) {
            my $new_grid = {};
            for my $tile (values %{ $scene->{grid} }) {
                my $left = $tile->{left};
                $tile->{left} = - $tile->{right};
                $tile->{right} = - $left;
                $new_grid->{ "$tile->{facing}_$tile->{left}_$tile->{top}_$tile->{right}" } = $tile;
            }
            $scene->{grid} = $new_grid;
        }

        # check scene version and do any conversion work; don't add new keys, that happens below.

        # we should have upgraded this scene to the latest version
        $scene->{version} = $default_scene->{version};

        $log->info("loaded scene with this many tiles: " . scalar keys %{ $scene->{grid} });
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

    # fix files with full paths in filename or renamed pictures
    if ($arg->{file}) {
        $self->filename(File::Basename::basename($arg->{file}, qw(.isb .isc)));
    }

    $self->remember_filename if $self->filename;

    $self->clipboard([]) unless $self->clipboard;

    return $self;
}

################################################################################
# Static function to tell whether an undo/redo action is a branch.
sub action_is_branch { #{{{1
    my ($action) = @_;

    return (ref $action) eq 'HASH' && exists $action->{branches};
}

################################################################################
sub find_sector_key { #{{{1
    my ($self, $tile) = @_;

    # we're accessing the sector size via a hash key so we can use this
    # function while we're still building the scene.
    return floor(($tile->{left} - 1) / $self->{sector_size}) . '_' .
        floor($tile->{top} / $self->{sector_size});
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
sub background_save { #{{{1
    my ($self) = @_;

    my $busy = Wx::BusyCursor->new;

    # freeze the data for the autosave thread; can't shared structured data between threads,
    # plus we want a stable scene when saving
    $log->info("freeze scene");
    my $data = {
        scene => $self,
        config => wxTheApp->config,
    };
    $save_data = freeze($data);
    $log->info("done");

    # allow the autosave thread access to the save_data variable
    $trigger_autosave_sem->up;

    return;
}

################################################################################
sub save { #{{{1
    my ($self, $config) = @_;

    $save_in_progress_sem->down;

    # background saves pass the config in as they can't access the wxTheApp object
    my $bg_save = defined $config;
    $config ||= wxTheApp->config;

    if ($config->{use_binary_files}) {
        $self->save_binary;
    }
    else {
        $self->save_yaml;
    }

    # we don't update the last filename for an autosave
    $self->remember_filename unless $bg_save;

    $save_in_progress_sem->up;

    return;
}

################################################################################
sub save_binary { #{{{1
    my ($self) = @_;

=for comment

File structure

+--------+-------------+-----------------------------------------------------+
| Bytes  | Pack format |                                                     |
+--------+-------------+-----------------------------------------------------+
| 1      | C           | Version number (also present in YAML)               |
+--------+-------------+-----------------------------------------------------+
| 2      | S           | Yaml length = Y                                     |
+--------+-------------+-----------------------------------------------------+
| 4      | L           | Palette count = P                                   |
+--------+-------------+-----------------------------------------------------+
| 4      | L           | Tile list count (not byte length) = T               |
+--------+-------------+-----------------------------------------------------+
| 4      | L           | Grid list count = G                                 |
+--------+-------------+-----------------------------------------------------+
| Y      |             | Yaml text, length from Y                            |
+--------+-------------+-----------------------------------------------------+
| P * 3  |             | Palette bytes, R0G0B0R1G1B1...                      |
+--------+-------------+-----------------------------------------------------+
| T * 11 | CsssL * T   | Tile list; only first G entries are on grid         |
+--------+-------------+-----------------------------------------------------+
| ?      | L*          | Undo tile counts and indexes; count req if > 1.     |
|        |             | The code embedded in the first index tells us if we |
|        |             | have a single index or a count followed by 1+       |
|        |             | subsequent indexes. We might also find a choice     |
|        |             | node, which will have one active item and a number  |
|        |             | of inactive branches, which are encoded in the same |
|        |             | way that the undo stack is since they are logically |
|        |             | the same, ie a list of simple paint/erase nodes or  |
|        |             | further choice nodes.                               |
+--------+-------------+-----------------------------------------------------+
| ?      | L*          | Redo tile nodes                                     |
+--------+-------------+-----------------------------------------------------+
| 4      | L           | Clipboard count = C                                 |
+--------+-------------+-----------------------------------------------------+
| ?      | C * ?       | Clipboard entries; S for name length N, N bytes for |
|        |             | for name, L for tile count T, T tiles.              |
+--------+-------------+-----------------------------------------------------+

=cut

    my $filename = $self->{filename} . '.isb';
    open (my $fh, '>', $filename)
        or $log->logdie("can't write file $filename: $OS_ERROR");

    # set the file version; for now, always write the highest version
    $self->{version} = $MAX_VERSION;

    # set a flag if we're debugging so we can skip the calls otherwise
    my $debugging = $log->debug("detailed debugging check");
    $log->info("save binary file to $filename; detailed debugging is " . ($debugging ? 'on' : 'off'));

    # create a copy of the scene but without the large data chunks, ie grid
    # and the undo/redo stacks. We'll save this at the start of the binary file
    # as a YAML chunk.
    my $extract = {};
    for my $key (keys %{ $self }) {
        next if $key =~ /\A(?:grid|undo_stack|redo_stack|palette|clipboard)\z/;
        $extract->{$key} = $self->{$key};
    }

    my $yaml_chunk = Dump($extract);
    my $yaml_length = length $yaml_chunk;
    if ($debugging) {
        $log->debug("yaml_length $yaml_length");
        $log->debug("extract: " . Dumper($extract));
        $log->debug("yaml: $yaml_chunk");
    }

    # write the version, yaml length and palette length, plus dummy values for the data counts which we
    # don't know yet, then the yaml chunk itself
    my $palette_size = scalar @{ $self->{palette} };
    $log->debug("write header, yaml_length $yaml_length, palette_size $palette_size at " . (tell $fh)) if $debugging;
    print { $fh } pack('CSL4', $self->{version}, $yaml_length, $palette_size, 0, 0, 0), $yaml_chunk;

    # write the palette entries; it's kind of crude to decode the entries again
    # but the scene doesn't have the real values or the brushes, etc, just the string.
    $log->debug("write palette entries at " . (tell $fh)) if $debugging;
    for my $i (0 .. $palette_size - 1) {
        my @rgb = map { hex $_ } $self->{palette}->[$i] =~ /([0-9A-F]{2})/go;
        $log->debug($self->{palette}->[$i] . ", rgb @rgb") if $debugging;
        print { $fh } pack('C3', @rgb);
    }

    my ($tile_count, $grid_count) = (0,0);

    # hash of tile references to position in tile list; we'll use this to encode tile
    # refs in undo/redo lists. We have to use the references not the tile keys because
    # the same key can refer to multiple tiles in the undo list (you can paint, erase
    # and paint the same tile again).
    my %tile_index;

# We aren't using each because that makes the tiles unsortable, and thus makes comparing
# files impossible. Sorting 300000 keys takes 0.5 sec.
#    # use 'each' to avoid building a temporary list
#    while ( my ($key, $tile) = each %{ $self->{grid} }) {

    my @keys = sort keys %{ $self->{grid} };
    $log->debug("write grid tiles at " . (tell $fh)) if $debugging;
    for my $key (@keys) {
        my $tile = $self->{grid}->{$key};

        # $log->debug("adding tile $tile_count $key from grid");
        print { $fh } pack_tile($tile);

        $tile_index{$tile} = $tile_count++;
        $grid_count++;
    }
    $log->debug("done grid tiles");

    # we iterate over the undo & redo lists 2 times; once to find tiles, once
    # to write codes and item details. Annoying but means
    # memory use is not proportional to stack size, ie we aren't using any
    # temporary lists or buffers.

    # scan the specified stack to find tiles not currently on the grid 
    # (paints in undo and erases in redo).

    # write the details of a single normal item (ie not a choice item) from the undo/redo stack, 
    # ie a listref to [ $action, [ $tile1, $tile2, ... ] ] where $action is paint or erase and 
    # $tile1, $tile2 etc are tile references.
    my $write_stack_item;
    $write_stack_item = sub {
        my ($item, $write_tiles) = @_;

        # $write_tiles is true during the first pass when we are simply adding 
        # tile details to the list from the grid so we can reference any tile by index.

        my ($action, $tiles) = @{ $item };

        if ($tiles) {
            # $log->debug("write_stack_item (write_tiles = $write_tiles) with this many tiles : " . ($#{ $tiles } + 1)) if $debugging;
            while ( my ($tile_index, $tile) = each @{ $tiles }) {
                my $file_index = $tile_index{$tile};
                if ($write_tiles) {

                    # write the tile details unless we've seen this tile before
                    unless (defined $file_index) {
                        $log->debug("write details for tile index $tile_count from stack at " . (tell $fh)) if $debugging;
                        print { $fh } pack_tile($tile);
                        $tile_index{$tile} = $tile_count++;
                    }
                }
                else {

                    $log->logdie("don't have file_index for tile during index write") unless defined $file_index;

                    if ($tile_index == 0) {

                        # information for this item; we have to communicate that this is a normal item
                        # (ie not a choice), paint vs erase and the number of tiles. The most common type 
                        # of item will have just one tile, so optimise for this; we always write a single
                        # index, ie a 4 byte integer. After the top bit, we have 2 bits encoding the 4 types;
                        # single paint, multiple paint, single erase, multiple erase) so we have one bit spare. For single tile
                        # items, the 4 byte index + code composite is all that is needed. For multiple items, we follow
                        # the index with a 4 byte count, and then add the remaining indexes. Choices aren't handled here.

                        # for now, encode the 4 bit value with (from most sig down):
                        # 1 = normal (0 signifies a choice node which have different interpretations
                        #             of the next 3 bits)
                        # 0 = erase, 1 = paint
                        # 0 = single, 1 = multiple
                        # 0 unused.
                        my $item_code = 

                            # bit 4 set for all normal items
                            8             

                            # bit 3 set for paint items
                            | ($action eq $IsoFrame::AC_PAINT ? 4 : 0)

                            # bit 2 set for multiple items
                            | ($#{ $tiles } > 0 ? 2 : 0);

                        # This 4 bit value is stamped onto the top 4 bits of the first index.
                        $file_index |= ($item_code << 28);
                    }

                    # write the file index, which now includes the code for the item
                    # if this is the first tile.
                    $log->debug("write stack tile index #$tile_index $file_index at " . (tell $fh)) if $debugging;
                    print { $fh } pack('L', $file_index);

                    # the first tile of a multiple item is followed by the count
                    if ($tile_index == 0 && $#{ $tiles } > 0) {
                        $log->debug("write multiple item count " . ($#{ $tiles } + 1) . " at " . (tell $fh)) if $debugging;
                        print { $fh } pack('L', $#{ $tiles } + 1);
                    }
                }
            }
        }
        else {
            $log->debug("no tiles in stack") if $debugging;
        }
    };

    # recursively process an undo/redo stack; this could be one of the top level stacks,
    # or the action list in an inactive branch. This routine is called in two modes
    # to either write the tile details or the tile indexes; these operations are combined
    # so we don't duplicate the recursion logic.
    my $write_stack_tiles;
    $write_stack_tiles = sub {
        my ($stack, $write_tiles) = @_;

        # write the stack count if we're writing indexes
        unless ($write_tiles) {
            $log->debug("write stack count " . ( $#{ $stack } + 1 ) . " at " . (tell $fh)) if $debugging;
            print { $fh } pack('L', $#{ $stack } + 1);
        }

        while ( my ($stack_index, $item) = each @{ $stack } ) {
            $log->debug("write stack item $stack_index" . Dumper($item)) if $debugging;
            my $item_ref = ref $item;
            if ($item_ref eq 'ARRAY') {

                # regular paint/erase item, with a list of affected tiles, any of which may already have
                # been written.
                $log->debug("write regular item") if $debugging;
                $write_stack_item->($item, $write_tiles);
            }
            elsif ($item_ref eq 'HASH') {
                if (exists $item->{branches}) {

                    # this is a choice node; the branches list contains a mixture of
                    # inactive branch hashes and the active branch list, which is a 
                    # standard undo/redo item.

                    unless ($write_tiles) {

                        # write identifying info for the choice node, ie the 32 bit value with
                        # choice bit (off), branch count and active branch index combined. We're using
                        # 12 bits (max 4095) for each of count and index, which leaves us 7 bits spare.
                        # Since a clear top bit indicates a choice node, we can just leave the top byte
                        # clear, ie ignore it for now.
                        my $branch_count = scalar @{ $item->{branches} };

                        # TODO check this before creating a branch
                        $log->logdie("too many branches") if $branch_count > 4095;
                        my $choice_code = $item->{current_branch} << 12 | $branch_count;
                        $log->debug("write choice_code $choice_code at " . (tell $fh)) if $debugging;
                        print { $fh } pack('L', $choice_code);
                    }

                    for my $branch ( @{ $item->{branches} } ) {

                        if ((ref $branch) eq 'HASH') {

                            # this is an inactive branch; write the timestamp as a 32 bit epoch value
                            # (good til 2038) and invoke this routine on the actions list
                            unless ($write_tiles) {
                                $log->debug("write inactive timestamp at " . (tell $fh)) if $debugging;
                                print { $fh } pack('L', $branch->{last_current_at}->epoch);
                            }
                            $write_stack_tiles->($branch->{actions}, $write_tiles);
                        }
                        else {

                            # this is the active branch, which is a single paint/erase item.
                            $log->debug("write active branch " . (tell $fh)) if $debugging && ! $write_tiles;
                            $write_stack_item->($branch, $write_tiles);
                        }
                    }
                }
                elsif (exists $item->{scale}) {
                    $log->debug("found a view item" . Dumper($item));

                    if ($write_tiles) {
                        $log->debug("skip view item this pass");
                    }
                    else {

                        # view items must encode scale and device origin x&y. Panning doesn't change
                        # scale but zooming does change origin, so we use a common format for both.
                        # Keep the top bit clear to distinguish these from normal items (as for choices)
                        # but then we need to distinguish from choices, which currently have the whole top
                        # byte clear. Scale ranges from 0.25 to 32; we'll * by 4 & subtract 1 to get 0-127,
                        # ie 7 bits.
                        # From the top byte down:
                        # 0 - clear so not a normal item
                        # 1 - set so not a choice item
                        # 2 - set if either origin needs more than 11 bits
                        # 3-9 - scale
                        # 10-20 - 11 bits for x origin; 0 - 2047, so store origin + 1024.
                        # 21-31 - 11 bits for y origin
                        my $scale_code = $item->{scale} * 4 - 1;
                        my $x_code = $item->{x} + 2 ** 10;
                        my $y_code = $item->{y} + 2 ** 10;
                        my $view_code = ($x_code > 0 && $x_code < 2 ** 11 && $y_code > 0 && $y_code < 2 ** 11)
                            ? 2 << 29   # 010; 01 = view item, 0 = view item contains origin
                                | $scale_code << 22
                                | $x_code << 11
                                | $y_code
                            : 3 << 29   # 011; 01 = view item, 1 = origin follows in next word (2x16 bits)
                                | $scale_code << 22;
                        $log->info(sprintf("write view %032b at ", $view_code) . (tell $fh)) if $debugging;
                        print { $fh } pack('L', $view_code);

                        # if either origin is outside the range -1024 - 1023, we have to
                        # write them as 16 bit values, giving us -32767 - 32768.
                        if ($view_code & 0x20000000) {
                            my $origin_code = ($item->{x} + 2 ** 15) << 16 | ($item->{y} + 2 ** 15);
                            $log->info(sprintf("write origin code %032b at ", $origin_code) . (tell $fh)) if $debugging;
                            print { $fh } pack('L', $origin_code);
                        }
                    }

                }
                else {
                    $log->logdie("found a hash in a stack but it's not a choice or a view : " . Dumper($item));
                }
            }
                            
        }
        $log->debug("finished stack") if $debugging;
    };

    # write the tile data for the undo/redo stacks
    $log->debug("write undo stack tiles at " . (tell $fh)) if $debugging;
    $write_stack_tiles->($self->{undo_stack}, 1);
    $log->debug("write redo stack tiles at " . (tell $fh)) if $debugging;
    $write_stack_tiles->($self->{redo_stack}, 1);

    # write the indexes for the undo/redo stacks
    $log->debug("write undo stack indexes at " . (tell $fh)) if $debugging;
    $write_stack_tiles->($self->{undo_stack}, 0);
    $log->debug("write redo stack indexes at " . (tell $fh)) if $debugging;
    $write_stack_tiles->($self->{redo_stack}, 0);

    # write the clipboard tiles; these are never shared with the grid or either
    # stack so we don't need to track the index, so they can appear by themselves.
    $log->debug("write clipboard count at " . (tell $fh)) if $debugging;
    my $clipboard_offset = tell $fh;
    $log->debug(sprintf "clipboard_offset %d %08x", $clipboard_offset, $clipboard_offset) if $debugging;
    print { $fh } pack('L', scalar @{ $self->{clipboard} });
    $log->debug("write clipboard entries at " . (tell $fh)) if $debugging;
    while ( my (undef, $item) = each @{ $self->{clipboard} }) {

        # write length of name and name
        print { $fh } pack('S', length $item->{name} ), $item->{name};

        # write tile count and tiles
        print { $fh } pack('L', scalar @{ $item->{tiles} });
        while ( my (undef, $tile) = each @{ $item->{tiles} } ) {
            print { $fh } pack_tile($tile);
        }
    }

    $log->debug("file complete at " . (tell $fh)) if $debugging;

    # return to the start of the file and write the data counts. Actual sizes are
    # derived from the file version at load.
    seek $fh, 7, SEEK_SET;
    print { $fh } pack('L3', $tile_count, $grid_count, $clipboard_offset);

    close($fh) or $log->logwarn("error closing binary file $filename: $OS_ERROR");
    $log->info("finished writing binary file");

    return;
}

################################################################################
sub save_yaml { #{{{1
    my ($self) = @_;

    my $yaml_scene = Dump($self);

#    if (wxTheApp->config->use_compressed_files) {
#        $log->info("write compressed YAML file");
#        zip \$yaml_scene => $self->filename . '.isz', Name => ($self->filename . '.isc')
#            or $log->logdie("zip failed: $ZipError");
#        $log->debug("saved to " . $self->filename . '.isz');
#        $log->info("done");
#    }
#    else {
        $log->info("write standard YAML file");
        write_file($self->{filename} . '.isc', $yaml_scene);
        $log->debug("saved to " . $self->{filename} . '.isc');
        $log->info("done");
#    }

    return;
}

################################################################################
# Not versioning the pack routine at this stage; we'll always save in the latest
# format.
sub pack_tile { #{{{1 
    my ($tile) = @_;

    my %Shape_code = (
        L => 1,
        T => 2,
        R => 3,
        TR => 4,
        TL => 5,
    );

    # encoding a tile
    # lead byte: shape  : 3 bits (000 = special, 001 = L, 010 = T, 011 = R, 100 = TR, 101 = TL, 110 & 111 are unused)
    #            facing : 1 bit (0 = R, 1 = L)
    #            4 bits unused
    # left, top, right : 2 bytes signed each 
    # brush_index : 4 bytes unsigned
    # => 11 bytes per tile, for Talisman = 350000 * 11 ~= 4mb

    my $compound_byte = $Shape_code{$tile->{shape}} 
        | (($tile->{facing} eq 'R' ? 0 : 1) << 3)
        | (($tile->{selected} ? 1 : 0) << 4);
    my $buffer = pack('CsssL', $compound_byte, $tile->{left}, $tile->{top}, $tile->{right}, $tile->{brush_index});
    # $log->info("pack_tile: bytes " . join(' ', unpack('C*', $buffer)));
    # $log->debug("pack_tile: $tile->{key} " . Dumper($tile));

    return $buffer;
}

################################################################################
sub read_tile { #{{{1 
    my ($fh, $scene) = @_;

    # Version 1
    # encoding a tile
    # lead byte: shape  : 3 bits (000 = special, 001 = L, 010 = T, 011 = R, 100 = TR, 101 = TL, 110 & 111 are unused)
    #            facing : 1 bit (0 = R, 1 = L)
    #            4 bits unused
    # left, top, right : 2 bytes signed each 
    # brush_index : 4 bytes unsigned
    my $tile = {};

    (my $compound_byte, $tile->{left}, $tile->{top}, $tile->{right}, $tile->{brush_index}) 
        = read_and_unpack($fh, 'CsssL', 11) or return;

    my @code_shapes = qw(X L T R TR TL);

    $tile->{shape} = $code_shapes[ $compound_byte & 0x07 ];

    $tile->{facing} = ($compound_byte & 0x08) ? 'L' : 'R';
    $tile->{selected} = ($compound_byte & 0x10) >> 4;

    $tile->{key} = "$tile->{facing}_$tile->{left}_$tile->{top}_$tile->{right}";

    # this is usually a class function but we're only using accessors via their
    # hash keys so we can pass a simple hash and it still works.
    $tile->{sector} = find_sector_key($scene, $tile);

#    $log->info("read_tile: $tile->{key} " . Dumper($tile));
    return $tile;
}

################################################################################
# return a hash with required keys to pass to the IsoScene constructor.
sub load_binary_file { #{{{1
    my ($filename) = @_;

    open (my $fh, '<', $filename)
        or $log->logdie("can't read file $filename: $OS_ERROR");

    my $debugging = $log->debug("load_binary_file; detailed debugging is on");

    # read version byte
    my ($version) = read_and_unpack($fh, 'C', 1) or return;
    if ($version > $MAX_VERSION) {
        $log->warn("unknown version : $version");
        return;
    }

    # read the YAML size and the data counts
    my ($yaml_length, $palette_count, $tile_count, $grid_count) = read_and_unpack($fh, 'SL3', 14);
    $log->debug("yaml_length, palette_count, tile_count, grid_count = $yaml_length, $palette_count, $tile_count, $grid_count") if $debugging;

    # if version >= 2, read the clipboard offset
    my $clipboard_offset = -1;
    if ($version >= 2) {
        ($clipboard_offset) = read_and_unpack($fh, 'L', 4);
    }

    # read the YAML chunk
    my $yaml_chunk;
    if (read($fh, $yaml_chunk, $yaml_length) != $yaml_length) {
        $log->warn("couldn't read $yaml_length bytes for yaml chunk");
        return;
    }

    # create the initialising hash from the yaml chunk; obviously, this is missing the data 
    # attributes eg grid, palette, undo_stack & redo_stack.
    my ($scene) = Load($yaml_chunk);

    $log->debug("yaml: " . Dumper($scene)) if $debugging;

    # read the palette entries
    $log->debug("read palette entries at " . (tell $fh)) if $debugging;
    $scene->{palette} = [];
    for my $i (0 .. $palette_count - 1) {
        my @rgb = read_and_unpack($fh, 'C3', 3);
        push @{ $scene->{palette} }, sprintf('#%02X%02X%02X', @rgb);
    }

    # we can now read the tiles into the grid; these tiles must also be kept in sequence so we
    # can interpret the indexes from the undo/redo stack data.
    $log->debug("read tiles at " . (tell $fh)) if $debugging;
    $scene->{grid} = {};
    my @tiles;
    for my $i (0 .. $tile_count - 1) {
        $tiles[$i] = read_tile($fh, $scene);

        # only add tiles to the grid up to the grid count; the remaining tiles
        # were present in one of the stacks but not the grid
        if ($i < $grid_count) {
            $scene->{grid}->{ $tiles[$i]->{key} } = $tiles[$i];
        }
    }

    # the structure of the undo and redo stacks is next (the tile data has already been
    # read and is referred to in this section by the index into the @tiles array.

    # read the next item and process it if it's a normal item;
    # we need this behaviour isolated so we can also use to read the
    # active branch.
    my $read_non_branch_item = sub {
        my ($fh, $stack) = @_;

        # every item begins with a 32 bit value, which we examine to determine
        # what follows (if anything).
        $log->debug("read item code at " . (tell $fh)) if $debugging;
        my $item_code = read_and_unpack($fh, 'L', 4);
        unless (defined $item_code) {
            $log->warn("couldn't read item code");
            return;
        }

        if ($item_code & 0x80000000) {

            # normal item; extract the top 4 bits to tell us item type
            # and whether this is a single or a multiple tile action
            my $code = $item_code >> 28;

            # create the item; we can tell if it's paint or erase from the code,
            # and we can create the initial list with the first item at the same time.
            # The bottom 28 bits of the code is the index into the tile list.
            my $item = [
                $code & 4 ? $IsoFrame::AC_PAINT : $IsoFrame::AC_ERASE,
                [ $tiles[ $item_code & 0xfFFffFF ] ],
            ];

            # multiple items have a bit set, otherwise we're done
            if ($code & 2) {

                $log->debug("read multiple item count at " . (tell $fh)) if $debugging;
                my $action_count = read_and_unpack($fh, 'L', 4);
                $log->debug("normal multiple item; action_count $action_count") if $debugging;

                # the count includes the first tile (which we've already read), so
                # start from 2
                for my $action_index (2 .. $action_count) {
                    $log->debug("read multiple index $action_index at " . (tell $fh)) if $debugging;
                    my $tile_index = read_and_unpack($fh, 'L', 4);
                    push @{ $item->[1] }, $tiles[ $tile_index ];
                }
            }
            else {
                $log->debug("read single item") if $debugging;
            }

            # add the item to the stack
            push @{ $stack }, $item;
        }
        elsif ($item_code & 0x40000000) {

            # view code; we can always get the scale from the code, we might be able to get the origin too
            # or that might be in a succeeding word.
            my $item = {
                scale => ((($item_code & 0x1FC00000) >> 22) + 1) / 4,
            };
                
            if ($item_code & 0x20000000) {

                # extended view item; read another word
                my $origin_code = read_and_unpack($fh, 'L', 4);
                $item->{x} = (($origin_code & 0xFFFF0000) >> 16) - 2 ** 15;
                $item->{y} = ($origin_code & 0x0000FFFF) - 2 ** 15;
            }
            else {

                # simple view item, get origin from view code
                $item->{x} = (($item_code & 0x003FF800) >> 11) - 2 ** 10;
                $item->{y} = ($item_code & 0x000007FF) - 2 ** 10;
            }

#            $log->info("read view code, item: " . Dumper($item));
            push @{ $stack }, $item;
        }

        return $item_code;
    };

    # this routine reads a single stack, ie a list of normal (paint,erase) and choice items.
    # If it finds a choice item, it will recurse to read each of the inactive branch action lists,
    # since they are similar lists (and hence may recurse again).
    my $read_stack_sub;
    $read_stack_sub = sub {
        my ($stack) = @_;

        $log->debug("read stack count at " . (tell $fh)) if $debugging;
        my $stack_count = read_and_unpack($fh, 'L', 4);
        $log->debug("stack count $stack_count") if $debugging;

        for my $stack_index (0 .. $stack_count - 1) {

            # read the next item and process it if it's a normal item;
            # we need this behaviour isolated so we can also use to read the
            # active branch.
            $log->debug("read stack item $stack_index") if $debugging;
            my $item_code = $read_non_branch_item->($fh, $stack);
            unless (defined $item_code) {
                $log->warn("didn't read lead item code");
                return;
            }

            # filter out normal items (top bit set) and view items (second-top bit set)
            unless ($item_code & 0xC0000000) {

                $log->debug(sprintf("read choice item %032b", $item_code));

                # choice item; the branch information is coded in the bottom 3 bytes of the item code.
                my $branch_count = $item_code & 0xfff;
                my $choice_item = {
                    branches => [],
                    current_branch => ($item_code & 0xfff000) >> 12,
                };

                for my $branch_index (0 .. $branch_count - 1) {
                    
                    # we know which is the current branch (or rather, the next step in the current branch),
                    # which we need to know how to read it.
                    if ($branch_index == $choice_item->{current_branch}) {

                        # this is the next step in the current branch, ie a standard paint/erase item.
                        $read_non_branch_item->($fh, $choice_item->{branches});
                        unless (defined $item_code) {
                            $log->warn("didn't active branch item code");
                            return;
                        }
                    }
                    else {

                        # an inactive branch; read the last-active timestamp and then recurse to read the
                        # stack of items for this branch
                        my $epoch_value = read_and_unpack($fh, 'L', 4);
                        my $branch = {
                            last_current_at => Time::Piece->strptime($epoch_value, '%s'),
                            actions => [],
                        };
                        $log->debug("new branch : " . Dumper($branch)) if $debugging;

                        $read_stack_sub->($branch->{actions});

                        push @{ $choice_item->{branches} }, $branch;
                    }
                }

                # add the choice item to the stack
                push @{ $stack }, $choice_item;
            }
        }
        $log->debug("finished stack") if $debugging;

        return 1;
    };

    $scene->{undo_stack} = [];
    $log->debug("load items to undo_stack at " . (tell $fh)) if $debugging;
    $read_stack_sub->($scene->{undo_stack}) or return;

    $scene->{redo_stack} = [];
    $log->debug("load to redo_stack at " . (tell $fh)) if $debugging;
    $read_stack_sub->($scene->{redo_stack}) or return;

    $scene->{clipboard} = read_clipboard_items($fh, $scene)
        or return;

    return $scene;
}

################################################################################
sub read_and_unpack { #{{{1
    my ($fh, $format, $length) = @_;

    my $buffer;
    if (read($fh, $buffer, $length) != $length) {

        # this might need to go before the read; if we read past eof, it might be too late
        my $pos = tell $fh;

        $log->warn("didn't read $length bytes at $pos");
        return;
    }
#    my @bytes = unpack("C*", $buffer);
#    $log->info("read_and_unpack: format $format, read bytes " .  join(' ', map { sprintf("%02x", $_) } @bytes));
    return unpack($format, $buffer);
}

################################################################################
# This routine is called from two contexts; either to load all the clipboard items
# into a scene during load, or to read the tile details for a single item if
# we're pasting from a different scene; in this case, a name is supplied.
# We're assuming the file is correctly positioned at the clipboard count data.
sub read_clipboard_items { #{{{1
    my ($fh, $scene, $item_name) = @_;

    # read the clipboard count
    my $debugging = $log->debug("read clipboard count at " . (tell $fh));
    my $clipboard_count = read_and_unpack($fh, 'L', 4);
    unless (defined $clipboard_count) {
        $log->warn("failed to read clipboard_count");
        return;
    }

    $log->debug("read $clipboard_count clipboard entries at " . (tell $fh)) if $debugging;

    my $list = [];
    for my $i (0 .. $clipboard_count - 1) {

        my $item = {
            tiles => [],
        };

        # read name length
        my $name_length = read_and_unpack($fh, 'S', 2);
        $item->{name} = read_and_unpack($fh, "A$name_length", $name_length);

        # read tile count and tiles
        my $tile_count = read_and_unpack($fh, 'L', 4);
        for my $j (0 .. $tile_count - 1) {
            my $tile = read_tile($fh, $scene)
                or return;
            push @{ $item->{tiles} }, $tile;
        }

        if ($item_name) {
            if ($item_name eq $item->{name}) {
                return $item;
            }
        }
        else {
            push @{ $list }, $item;
        }
    }

    return $item_name ? undef : $list;
}

################################################################################
# We've chosen a clipboard item from a different scene; we know the scene name
# and the item name (both are encoded in the thumbnail name). We want to open the
# scene file and read the tile list for the specified clipboard item.
# SIDE EFFECT; the palette in the current scene will be expanded as necessary
# to include the colors in the clipboard item.
sub read_clipboard_item { #{{{1
    my ($canvas, $scene_name, $clipboard_item_name) = @_;

    $log->info("scene_name $scene_name, clipboard_item_name $clipboard_item_name");

    my $filename = "$scene_name.isb";

    # Should have been trapped in the paste dialog?
    return undef, "Scene file '$filename' doesn't exist." unless -f $filename;

    open (my $fh, '<', $filename)
        or return undef, "can't read file $filename: $OS_ERROR";

    my $debugging = $log->debug("load_binary_file; detailed debugging is on.");

    # read version byte
    my ($version) = read_and_unpack($fh, 'C', 1) or return;
    return undef, "Couldn't read version from scene file." unless defined $version;
    return undef, "Unknown version '$version'; may not be a scene file." if $version > $MAX_VERSION;
    return undef, "Can't paste from v1 files; resave $scene_name to upgrade it." if $version < 2;

    # read the YAML size and the palette count
    my ($yaml_length, $palette_count) = read_and_unpack($fh, 'SL', 6);
    $log->debug("yaml_length, palette_count = $yaml_length, $palette_count") if $debugging;

    # skip the tile counts
    seek $fh, 8, SEEK_CUR;

    # read the clipboard offset
    my ($clipboard_offset) = read_and_unpack($fh, 'L', 4);
    return undef, "Couldn't read clipboard offset from scene file." unless defined $clipboard_offset;
    $log->debug(sprintf "clipboard_offset %d %08x", $clipboard_offset, $clipboard_offset) if $debugging;

    # skip the YAML content
    seek $fh, $yaml_length, SEEK_CUR;
#    read($fh, my $yaml_chunk, $yaml_length + 8);

    # read the palette entries; we'll need these to convert the brush indexes in the clipboard scene
    # to brush indexes in the current scene
    $log->debug("read palette entries at " . (tell $fh)) if $debugging;
    my @palette = ();
    for my $i (0 .. $palette_count - 1) {
        my @rgb = read_and_unpack($fh, 'C3', 3);
        $palette[$i] = sprintf('#%02X%02X%02X', @rgb);
    }

    # jump to the clipboard
    seek $fh, $clipboard_offset, SEEK_SET;

    # find the item by name in the other scene's clipboard
    my $clipboard_item = read_clipboard_items($fh, $canvas->scene, $clipboard_item_name)
        or return undef, "Couldn't find item '$clipboard_item_name' in ${scene_name}'s clipboard.";

    # convert brush indexes
    my @new_tile_indexes;
    for my $tile ( @{ $clipboard_item->{tiles} } ) {

        # we're caching new indexes so we only check each index once
        unless (defined $new_tile_indexes[ $tile->{brush_index} ]) {

            # find the index for this color string in the current scene (adds automatically if req)
            $new_tile_indexes[ $tile->{brush_index} ] = $canvas->get_palette_index( $palette[ $tile->{brush_index} ] );
        }
        $tile->{brush_index} = $new_tile_indexes[ $tile->{brush_index} ];
    }

    # return list of tiles
    return $clipboard_item->{tiles};
}

################################################################################

1;
