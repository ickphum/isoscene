#!/usr/bin/perl

# Purpose: Determine performance curves for various methods of pushing
#          triangles and quads through the OpenGL pipeline

# Copyright (c) 2004-2006, Geoff Broadwell; this script is released
# as open source and may be distributed and modified under the terms
# of either the Artistic License or the GNU General Public License,
# in the same manner as Perl itself.  These licenses should have been
# distributed to you as part of your Perl distribution, and can be
# read using `perldoc perlartistic` and `perldoc perlgpl` respectively.


use strict;
use warnings;

#use SDL::App;
#use SDL::Constants;
#use SDL::Event;
#use SDL::OpenGL;
use Time::HiRes 'time';

use OpenGL qw/ :all /;
use POGLBench;

our $VERSION = '0.1.24';

our $test = 0;
our $run = 0;
our $done = 0;
our $ready = 0;
our @vbo;

### USER CONFIG

# Primitive sizes (and therefore counts) are integer divisors of
# (A^i * B^j * C^k ...) where good A, B, C, ... are relatively prime;
# this number is used for the draw area height and width and defaults to:
#     2^4 * 3^2 * 5 = 720
# You may also want to get fewer data points across the same range by
# directly using higher powers; for example:
#     16  * 9   * 5 = 720
#
# my @max_powers = (16 => 1, 9 => 1, 5 => 1);
my @max_powers = (2 => 4, 3 => 2, 5 => 1);

# Maximum quads along each axis for known slow versus usually fast tests;
# chosen to be somewhat reasonable for most common settings of @max_powers
# my $max_count_slow = 60;
my $max_count_slow = 154;
my $max_count_fast = 154;

# Font to use to label graphs
#my $font_file = 'round-5x7.txt';
my $font_style = GLUT_BITMAP_HELVETICA_10;


### METHODS TO BENCHMARK

my %va_types = (
    q   => \&make_quads_va,
    t   => \&make_tris_va,
    qs  => \&make_qs_va,
    ts  => \&make_ts_va,
);

my %dl_types = (
    qs  => \&draw_qs,
    ts  => \&draw_ts,
    qsv => \&draw_qs_va,
    tsv => \&draw_ts_va,
);

my @tests = (
    # Nick    Draw Routine     Stats Calc     Type      Graph Color
    [empty => \&draw_empty,    \&stats_empty, 'single', [1 ,  1,  1], 0xFFFF],
    [t     => \&draw_tris,     \&stats_tris,  'slow',   [1 ,  0,  0], 0xAAAA],
    [q     => \&draw_quads,    \&stats_quads, 'slow',   [1 , .5,  0], 0xAAAA],
    [ts    => \&draw_ts,       \&stats_ts,    'slow',   [1 ,  1,  0], 0xAAAA],
    [qs    => \&draw_qs,       \&stats_qs,    'slow',   [0 ,  1,  0], 0xAAAA],
    [tsd   => \&draw_ts_dl,    \&stats_ts,    'fast',   [0 ,  1,  1], 0xAAAA],
    [qsd   => \&draw_qs_dl,    \&stats_qs,    'fast',   [0 ,  0,  1], 0xAAAA],
    [tv    => \&draw_tris_va,  \&stats_tris,  'fast',   [.8,  0,  0], 0xFFFF],
    [qv    => \&draw_quads_va, \&stats_quads, 'fast',   [.8, .4,  0], 0xFFFF],
    [tsv   => \&draw_ts_va,    \&stats_ts,    'fast',   [.8, .8,  0], 0xFFFF],
    [qsv   => \&draw_qs_va,    \&stats_qs,    'fast',   [0 , .8,  0], 0xFFFF],
    [tsvd  => \&draw_ts_va_dl, \&stats_ts,    'fast',   [0 , .8, .8], 0xFFFF],
    [qsvd  => \&draw_qs_va_dl, \&stats_qs,    'fast',   [0 ,  0, .8], 0xFFFF],
);

my $test_types = {
    empty=>'',
    t=>'',
    q=>'',
    ts=>'',
    qs=>'',
    tsd=>'',
    qsd=>'',
    tv=>'t',
    qv=>'q',
    tsv=>'t',
    qsv=>'q',
    tsvd=>'t',
    qsvd=>'q',
};


### MISC GLOBALS

my ($conf, $app, $gl_info);
my ($MIN_FRAMES, $MIN_SECONDS);
my ($w, $h);
my (%dls, %vas);
my (@combos, @slow, @fast);
my ($showing_graph);
my ($empty_time, $empty_frames, @stats, @total, @max);


### CODE

START: main();


sub main
{
    init();

    print "Benchmarks:";

    glutDisplayFunc(\&cbDraw);
    glutIdleFunc(\&cbDraw);
    glutKeyboardFunc(\&cbKeyPressed);

    glutMainLoop();
}


sub init
{
    # Figure out primitive counts for each run of each test type
    my %combos;
    @combos{recurse_combos(@max_powers)} = ();
    @combos = sort {    $a <=> $b             } keys %combos;
    @slow   = grep {    $_ <= $max_count_slow } @combos;
    @fast   = grep {    $_ >  $max_count_slow
                     && $_ <= $max_count_fast } @combos;

    # Choose drawing area size to match counts
    $h = $w = $combos[-1];

    # Do the standard init stuff, including command line processing,
    # window creation, and so on
    my %default_conf = (
			title    => 'Triangle Slammer OpenGL Benchmark',
			geometry => "${w}x$h",
			frames   => 10,
			seconds  => 1,
		       );
    ($conf, $app, $gl_info) = POGLBench::basic_init(\%default_conf);

    # Reduce indirections in inner loops
    ($MIN_FRAMES, $MIN_SECONDS) = ($conf->{frames}, $conf->{seconds});

    # Let user know what's going on
    show_user_message();

    # Change projection to integer-pixel ortho
    glMatrixMode(GL_PROJECTION);
    glOrtho(0, $w, 0, $h, -1, 1);
    glMatrixMode(GL_MODELVIEW);

    # Load font for graph labels
#    $font_style = POGLBench::init_bitmap_font($font_file);
    
    # Make sure GL state is consistent for VA and DL creation
    start_frame();

    # Create vertex arrays and display lists outside timing loop
    init_vertex_arrays();
    init_display_lists();

    # Clean up GL state
    end_frame();
}

sub recurse_combos
{
    my ($base, $max_power, @rest) = @_;

    return (1) unless $base;

    my @combos;
    foreach my $power (0 .. $max_power) {
        my $multiplier = $base ** $power;
        push @combos, $_ * $multiplier foreach recurse_combos(@rest);
    }
    return @combos;
}


sub show_user_message
{
    print <<"EOM";
TRISLAM benchmarks several methods of pushing OpenGL primitives,
testing each method with various primitive counts and sizes.
During the benchmark, the test window will start out black, slowly
brightening to white as testing progresses.  Once benchmarking is
complete, the collected data will be dumped in tabular form.

The configuration for this series of tests will be as follows:

EOM

    POGLBench::show_basic_config($conf, $gl_info, $VERSION);

    print "standard runs:    @slow\n";
    print "extra fast runs:  @fast\n";
    print '-' x 79, "\n";
}


sub init_vertex_arrays
{
    print "Init vertex arrays:";

    @vbo = glGenBuffersARB_p(64);
    die "unable to allocate VBOs\n" if (!scalar(@vbo) || !$vbo[63]);
    my $vbo = 0;

    foreach my $type (sort keys %va_types) {
        print " $type";
        foreach my $count (@slow, @fast) {
            my $data = $va_types{$type}->($count, $w / $count);
#            my $va   = pack 'f*', @$data;
            my $va   = OpenGL::Array->new_list(GL_FLOAT,@$data);

            # Only testing VBOs on larger arrays
            if ($count > 12 && $vbo < 64)
            {
                $va->bind(++$vbo);
                glBufferDataARB_p(GL_ARRAY_BUFFER_ARB, $va, GL_STATIC_DRAW_ARB);
#                glVertexPointer_c(2, GL_FLOAT, 0, 0);
            }

            $vas{"${type}_$count"} = $va;
        }
    }
#    glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);

    print ".\n";
}


sub init_display_lists
{
    print "Init display lists:";

    my $num_lists = (scalar keys %dl_types) * (@slow + @fast);
    my $current   = glGenLists($num_lists);
    my $list;

    foreach my $type (sort keys %dl_types) {
        print " $type";
        foreach my $count (@slow, @fast) {
            $list = $current++;
            $dls{"${type}_$count"} = $list;
            glNewList($list, GL_COMPILE);
            $dl_types{$type}->($count, $w / $count);
            glEndList;
        }
    }

    print ".\n";
}


sub benchmark
{
    if ($test > $#tests)
    {
        print ".\n" if (!$done++);
        return;
    }

    my ($name, $draw, $stats, $class) = @{$tests[$test]};
    my $counts = $class eq 'single' ? [1] :
        $class eq 'slow' ? [@slow] : [@slow, @fast];

    if (!$run)
    {
        print " $name";

        # After printing current test name, busy wait for a second
        # so that the terminal can catch up and not do work while
        # the GL timing is in progress
        my $a = time();
        1 while 1 > time() - $a;
    }

    my $count = $counts->[$run];
    my $size  = $w / $count;

    POGLBench::fade_to_white(($test + ($run / @$counts)) / @tests);

    my $run_done = 0;
    my $frames   = 0;
    my $start    = time();

    while (!$run_done)
    {
        start_frame();
        $draw->($count, $size);
        end_frame();

        $frames++;
        $run_done = 1 if ($MIN_FRAMES <= $frames
             && $MIN_SECONDS <= time - $start);
    }

    glFinish();
    my $end = time();
    my $time = $end - $start;

    push(@stats, [$name, $count, $time, $frames,
        $stats->($count, $size)]);

    if (++$run > $#$counts)
    {
        $test++;
        $run = 0;
    }
}


sub start_frame
{
    glClear(GL_COLOR_BUFFER_BIT |
            GL_DEPTH_BUFFER_BIT);
}


sub draw_empty
{
    my ($count, $size) = @_;
}


sub stats_empty
{
    return (0, 0, 0, 0);
}


sub draw_quads
{
    my ($count, $size) = @_;

    glBegin(GL_QUADS);
    foreach my $y (0 .. $count - 1) {
        foreach my $x ( 0 .. $count - 1) {
            glVertex2f($x * $size        , $y * $size + $size);
            glVertex2f($x * $size        , $y * $size        );
            glVertex2f($x * $size + $size, $y * $size        );
            glVertex2f($x * $size + $size, $y * $size + $size);
        }
    }
    glEnd;
}


sub make_quads_va
{
    my ($count, $size) = @_;
    my @data;

    foreach my $y (0 .. $count - 1) {
        foreach my $x ( 0 .. $count - 1) {
            push @data, $x * $size        , $y * $size + $size;
            push @data, $x * $size        , $y * $size        ;
            push @data, $x * $size + $size, $y * $size        ;
            push @data, $x * $size + $size, $y * $size + $size;
        }
    }

    return \@data;
}


sub draw_quads_va
{
    my ($count, $size) = @_;
    my $va = $vas{"q_$count"};

    glVertexPointer_p(2, $va);

    glEnableClientState(GL_VERTEX_ARRAY);
    glDrawArrays(GL_QUADS, 0, 4 * $count * $count);
    glDisableClientState(GL_VERTEX_ARRAY);
}


sub stats_quads
{
    my ($count, $size) = @_;
    my $length = $size * $count;
    my $area   = $length * $length;
    my $prims  = $count * $count;
    my $tris   = 2 * $prims;
    my $verts  = 4 * $prims;

    return ($area, $prims, $tris, $verts);
}


sub draw_qs
{
    my ($count, $size) = @_;

    foreach my $y (0 .. $count - 1) {
        glBegin(GL_QUAD_STRIP);
        foreach my $x ( 0 .. $count) {
            glVertex2f($x * $size, $y * $size + $size);
            glVertex2f($x * $size, $y * $size        );
        }
        glEnd;
    }
}


sub make_qs_va
{
    my ($count, $size) = @_;
    my @data;

    foreach my $y (0 .. $count - 1) {
        foreach my $x ( 0 .. $count) {
            push @data, $x * $size, $y * $size + $size;
            push @data, $x * $size, $y * $size        ;
        }
    }

    return \@data;
}


sub draw_qs_va
{
    my ($count, $size) = @_;
    my $va  = $vas{"qs_$count"};
    my $row = 2 * ($count + 1);

    glVertexPointer_p(2, $va);

    glEnableClientState(GL_VERTEX_ARRAY);
    foreach my $y (0 .. $count - 1) {
        glDrawArrays(GL_QUAD_STRIP, $y * $row, $row);
    }
    glDisableClientState(GL_VERTEX_ARRAY);
}


sub draw_qs_dl
{
    my ($count, $size) = @_;

    glCallList($dls{"qs_$count"});
}


sub draw_qs_va_dl
{
    my ($count, $size) = @_;
    my $va = $vas{"qs_$count"};

    glVertexPointer_p(2, $va);

    glEnableClientState(GL_VERTEX_ARRAY);
    glCallList($dls{"qsv_$count"});
    glDisableClientState(GL_VERTEX_ARRAY);
}


sub stats_qs
{
    my ($count, $size) = @_;
    my $length = $size * $count;
    my $area   = $length * $length;
    my $prims  = $count;
    my $tris   = 2 *  $count      * $prims;
    my $verts  = 2 * ($count + 1) * $prims;

    return ($area, $prims, $tris, $verts);
}


sub draw_tris
{
    my ($count, $size) = @_;

    glBegin(GL_TRIANGLES);
    foreach my $y (0 .. $count - 1) {
        foreach my $x ( 0 .. $count - 1) {
            glVertex2f($x * $size        , $y * $size + $size);
            glVertex2f($x * $size        , $y * $size        );
            glVertex2f($x * $size + $size, $y * $size + $size);

            glVertex2f($x * $size + $size, $y * $size + $size);
            glVertex2f($x * $size        , $y * $size        );
            glVertex2f($x * $size + $size, $y * $size        );
        }
    }
    glEnd;
}


sub make_tris_va
{
    my ($count, $size) = @_;
    my @data;

    foreach my $y (0 .. $count - 1) {
        foreach my $x ( 0 .. $count - 1) {
            push @data, $x * $size        , $y * $size + $size;
            push @data, $x * $size        , $y * $size        ;
            push @data, $x * $size + $size, $y * $size + $size;

            push @data, $x * $size + $size, $y * $size + $size;
            push @data, $x * $size        , $y * $size        ;
            push @data, $x * $size + $size, $y * $size        ;
        }
    }

    return \@data;
}


sub draw_tris_va
{
    my ($count, $size) = @_;
    my $va = $vas{"t_$count"};

    glVertexPointer_p(2, $va);

    glEnableClientState(GL_VERTEX_ARRAY);
    glDrawArrays(GL_TRIANGLES, 0, 6 * $count * $count);
    glDisableClientState(GL_VERTEX_ARRAY);
}


sub stats_tris
{
    my ($count, $size) = @_;
    my $length = $size * $count;
    my $area   = $length * $length;
    my $prims  = 2 * $count * $count;
    my $tris   =     $prims;
    my $verts  = 3 * $prims;

    return ($area, $prims, $tris, $verts);
}


sub draw_ts
{
    my ($count, $size) = @_;

    foreach my $y (0 .. $count - 1) {
        glBegin(GL_TRIANGLE_STRIP);
        foreach my $x ( 0 .. $count) {
            glVertex2f($x * $size, $y * $size + $size);
            glVertex2f($x * $size, $y * $size        );
        }
        glEnd;
    }
}


sub make_ts_va
{
    my ($count, $size) = @_;
    my @data;

    foreach my $y (0 .. $count - 1) {
        foreach my $x ( 0 .. $count) {
            push @data, $x * $size, $y * $size + $size;
            push @data, $x * $size, $y * $size        ;
        }
    }

    return \@data;
}


sub draw_ts_va
{
    my ($count, $size) = @_;
    my $va  = $vas{"ts_$count"};
    my $row = 2 * ($count + 1);

    glVertexPointer_p(2, $va);

    glEnableClientState(GL_VERTEX_ARRAY);
    foreach my $y (0 .. $count - 1) {
       glDrawArrays(GL_TRIANGLE_STRIP, $y * $row, $row);
    }
    glDisableClientState(GL_VERTEX_ARRAY);
}


sub draw_ts_dl
{
    my ($count, $size) = @_;

    glCallList($dls{"ts_$count"});
}


sub draw_ts_va_dl
{
    my ($count, $size) = @_;
    my $va = $vas{"ts_$count"};

    glVertexPointer_p(2, $va);

    glEnableClientState(GL_VERTEX_ARRAY);
    glCallList($dls{"tsv_$count"});
    glDisableClientState(GL_VERTEX_ARRAY);
}


sub stats_ts
{
    my ($count, $size) = @_;
    my $length = $size * $count;
    my $area   = $length * $length;
    my $prims  = $count;
    my $tris   = 2 *  $count      * $prims;
    my $verts  = 2 * ($count + 1) * $prims;

    return ($area, $prims, $tris, $verts);
}


sub end_frame
{
    glFinish;

#    $sdl_app->sync;
}


# State engine
sub cbDraw
{
    if (!$done)
    {
        benchmark();
    }
    elsif (!$ready)
    {
        cleanup();
    }
}


# Keyboard handler
sub cbKeyPressed
{
  my $key = shift;
  my $c = uc chr $key;
  if ($key == 27 or $c eq 'Q')
  {
    glutDestroyWindow($app);
    exit(0);
  }
  if ($done && $c eq 'R')
  {
    draw_stats();
  }
}


#sub check_events
#{
#    my $e = new SDL::Event;
#
#    $e->pump;
#    while (not $done and $e->poll) {
#        my $type = $e->type;
#        if    ($type eq SDL_QUIT)    {
#            $done = 1;
#        }
#        elsif ($type eq SDL_KEYDOWN) {
#            my $key = SDL::GetKeyName($e->key_sym);
#            $done = 1 if $key eq 'q' or $key eq 'escape';
#        }
#        elsif ($type eq SDL_ACTIVEEVENT) {
#            draw_stats() if $showing_graph and $e->active_gain;
#        }
#    }
#}


sub cleanup
{
    fixup_stats();
    show_stats();
    draw_stats();
#    check_events until $done;

    glDeleteBuffersARB_p(@vbo);
}


sub fixup_stats
{
    my $empty = shift @stats;
    
    $empty_time   = $empty->[2];
    $empty_frames = $empty->[3];
    my $empty_tpf = $empty_time / $empty_frames;

    @total = ('totl,' => avg => (0) x 12);
    @max   = (max   => max => (0) x 12);

    foreach my $stat (@stats) {
        my ($name, $count, $time, $frames, $pixpf, $prmpf, $tpf, $vpf)
            = @$stat;

        # Subtract out empty loop time, and loop if negative result
        # $time -= $empty_tpf * $frames;
        if ($time <= 0) {
            push @$stat, (0) x 5;
            next;
        }

        # Calc "work", the geometric mean of pixels and vertices
        my $workpf = sqrt($pixpf * $vpf);

        # Calc fps
        my $fps = $frames / $time;

        # Calc other perf stats
        my $pixps = $pixpf  * $fps;
        my $prmps = $prmpf  * $fps;
        my $tps   = $tpf    * $fps;
        my $vps   = $vpf    * $fps;
        my $wps   = $workpf * $fps;  
        
        # Add them to stat row
        push @$stat, $fps, $pixps, $prmps, $tps, $vps, $wps;

        # Convert per frame counts to totals
        $stat->[$_] *= $frames foreach 4 .. 7;

        # Update running totals
        $total[$_] += $stat->[$_] foreach 2 .. 7;

        # Update running maximums
        foreach (2 .. 13) {
            $max[$_] = $stat->[$_] if $max[$_] < $stat->[$_];
        }
    }

    # Calc averages for totals line
    $total[$_] = $total[$_ - 5] / $total[2] foreach (8 .. 13);

    $ready++;
}


sub show_stats
{
    my @basic  =  qw( Name Cnt Time );
    my @raw    =  qw( Frms Mpix  Kprim Ktri Kvert );
    my @calc   = @raw;
    my @scale  = (qw( 0    6     3     3    3    )) x 2;
    my @header = (@basic, @raw, @calc);
    @scale     = map {10 ** $_} @scale;
    my $g_form = "%9s%-*s%s\n";
    my $h_form = '%-5s%3s %6s'   . ' %5s' x @raw . '' . ' %5s' x @calc . " VBO\n";
    my $format = '%-5s%3s %6.3f' . ' %5d' x @raw . '' . ' %5d' x @calc . " %s\n";

    printf $g_form, '', 6 * @raw + 8, 'MEASURED', 'PER SECOND';
    printf $h_form, @header;
    printf $format, empty => 1, $empty_time, $empty_frames, (0) x 9, ' ';

    foreach my $stat (@stats, \@total) {
        my @stat = @$stat;

        foreach (0 .. $#scale) {
            $stat[$_ + 3] /= $scale[$_];
        }

        my $name = $stat[0];
        my $count = $stat[1];
        my $va_type = $test_types->{$name} || '';
        
        my $va = $vas{$va_type.'_'.$count};
        $stat[13] = ($va && $va->bound()) ? '*' : ' ';

        printf $format, @stat;
    }
}


sub draw_stats
{
    return if (!$ready);

    # Graph config
    my $x_off     = 10;
    my $y_off     = 10;
    my $tick_size = 3;
    my $val_space = 50;
    my $key_size  = 20;
    my $x_scale   = ($w - 4 * $x_off) / (2 * ($fast[-1] || $slow[-1]));
    my $key_scale = ($h - 4 * $y_off) / (2 * @tests);

    # Get a fresh black frame for graphing
    glClearColor(0, 0, 0, 1);
    start_frame();

    # Use antialiased lines
    glEnable   (GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable   (GL_LINE_SMOOTH);
    glHint     (GL_LINE_SMOOTH_HINT, GL_NICEST);

    # Draw axis ticks
    glColor3f(1, 1, 1);
    glBegin(GL_LINES);
    foreach my $count (0, @slow, @fast) {
        my $x_tick = $count * $x_scale + $x_off;

        glVertex2f($x_tick, $y_off);
        glVertex2f($x_tick, $y_off - $tick_size);
        glVertex2f($x_tick, $y_off + $h / 2);
        glVertex2f($x_tick, $y_off + $h / 2 - $tick_size);
        glVertex2f($x_tick + $w / 2, $y_off + $h / 2);
        glVertex2f($x_tick + $w / 2, $y_off + $h / 2 - $tick_size);
    }
    glEnd;

    my $x_tick = $x_off + 3;
    my $val_max = int(($h / 2 - 2 * $y_off) / $val_space);

    # Work
    foreach my $value (0 .. $val_max) {
        my $y_tick = $value * $val_space + $y_off;

        glBegin(GL_LINES);
        glVertex2f($x_off,              $y_tick);
        glVertex2f($x_off - $tick_size, $y_tick);
        glEnd;
        #draw_string($font_style, $value, $x_tick, $y_tick) if ($value);
    }

    # Pixels
    my $value = 0;
    $val_max = $max[9] / mag_scale($max[9]);
    my $y_scale = ($h - 4 * $y_off) / (2 * $val_max);
    my $val_inc = tick_inc($val_max,5);
    while ($value < $val_max) {
        my $y_tick = ($value * $y_scale)  + $y_off;

        glBegin(GL_LINES);
        glVertex2f($x_off,              $y_tick + $h / 2);
        glVertex2f($x_off - $tick_size, $y_tick + $h / 2);
        glEnd;
        POGLBench::draw_string($font_style, $value, $x_tick, $y_tick + $h / 2) if ($value);
        $value += $val_inc;
    }

    # Vertices
    $value = 0;
    $val_max = $max[12] / mag_scale($max[12]);
    $y_scale = ($h - 4 * $y_off) / (2 * $val_max);
    $val_inc = tick_inc($val_max,5);
    while ($value < $val_max) {
        my $y_tick = ($value * $y_scale)  + $y_off;

        glBegin(GL_LINES);
        glVertex2f($x_off + $w / 2,              $y_tick + $h / 2);
        glVertex2f($x_off + $w / 2 - $tick_size, $y_tick + $h / 2);
        glEnd;
        POGLBench::draw_string($font_style, $value, $x_tick + $w / 2, $y_tick + $h / 2) if ($value);
        $value += $val_inc;
    }

    # Draw axes
    glBegin(GL_LINE_STRIP);
    glVertex2f($x_off,          $h / 2 - $y_off);
    glVertex2f($x_off,          $y_off);
    glVertex2f($w / 2 - $x_off, $y_off);
    glEnd;
    glBegin(GL_LINE_STRIP);
    glVertex2f($x_off,          $h - $y_off);
    glVertex2f($x_off,          $h / 2 + $y_off);
    glVertex2f($w / 2 - $x_off, $h / 2 + $y_off);
    glEnd;
    glBegin(GL_LINE_STRIP);
    glVertex2f($w / 2 + $x_off, $h - $y_off);
    glVertex2f($w / 2 + $x_off, $h / 2 + $y_off);
    glVertex2f($w - $x_off,     $h / 2 + $y_off);
    glEnd;

    # Draw color key
    foreach my $num (0 .. $#tests) {
        my $test = $tests[$num];
        my ($name, $color, $stipple) = @$test[0, -2, -1];

	glEnable(GL_LINE_STIPPLE);
	glLineStipple(3, $stipple);

        glBegin(GL_LINES);
        glColor3f(@$color);
        glVertex2f($x_off + $w / 2,             $y_off + $num * $key_scale);
        glVertex2f($x_off + $w / 2 + $key_size, $y_off + $num * $key_scale);
        glEnd();

	glDisable(GL_LINE_STIPPLE);

        POGLBench::draw_string($font_style, $name, $x_off + $w / 2 + $key_size * 2,  $y_off + $num * $key_scale);
    }

    # Draw performance graph lines

    # Pixels per second
    draw_one_stat($x_off, $y_off + $h / 2, $y_off, $x_scale, 9);
    glColor3f(1, 1, 1);
    POGLBench::draw_string($font_style, mag_char($max[9])." Pixels/Sec", $w / 4, $h - 2 * $y_off);

    # Vertices per second
    draw_one_stat($x_off + $w / 2, $y_off + $h / 2, $y_off, $x_scale, 12);
    glColor3f(1, 1, 1);
    POGLBench::draw_string($font_style, mag_char($max[12])." Vertices/Sec", 3 * $w / 4, $h - 2 * $y_off);

    # "Work" per second, the geometric mean of pixels and vertices
    draw_one_stat($x_off, $y_off, $y_off, $x_scale, 13);
    glColor3f(1, 1, 1);
    POGLBench::draw_string($font_style, "Work/Sec", $w / 4, $h / 2 - 2 * $y_off);

    # Show our graph
    end_frame();
    $showing_graph = 1;
}


sub draw_one_stat
{
    my ($x_loc, $y_loc, $y_off, $x_scale, $num) = @_;

    my $max     = $max[$num];
    my $y_scale = ($h - 4 * $y_off) / (2 * $max);
    my %colors  = map {($_->[0] => $_->[-2])} @tests;
    my %stipple  = map {($_->[0] => $_->[-1])} @tests;
    my $last    = '';

    glEnable(GL_LINE_STIPPLE);
    glBegin(GL_LINE_STRIP);
    foreach my $run (0 .. $#stats) {
        my ($name, $count, $stat) = @{$stats[$run]}[0, 1, $num];

        if ($name ne $last) {
            glEnd;
            glLineStipple(3, $stipple{$name});
            glBegin(GL_LINE_STRIP);
            $last = $name;
        }

        glColor3f(@{$colors{$name}});
        glVertex2f($count * $x_scale + $x_loc, $stat * $y_scale + $y_loc);
    }
    glEnd;
    glDisable(GL_LINE_STIPPLE);
}

sub kilo_mag
{
    my($num) = @_;
    my $mag = int(log($num) / log(10));
    return int($mag / 3);
}

sub mag_char
{
    my($num) = @_;
    return ('','K','M','G','T','P','E','Z','Y')[kilo_mag($num)];
}

sub mag_scale
{
    my($num) = @_;
    return 10 ** (3*kilo_mag($num));
}

sub tick_inc
{
    my($max,$parts) = @_;

    $parts = 5 if (!$parts);
    return $max / $parts if ($max < 1);

    my $mag = int(log($max) / log(10));
    my $scl = 10 ** ($mag - 1);
    my $inc = $max / ($scl * $parts);

    if ($inc > 7.5)
    {
        $inc = 10;
    }
    elsif ($inc > 3.5)
    {
        $inc = 5;
    }
    elsif ($inc > 1.5)
    {
        $inc = 2;
    }
    else
    {
        $inc = 1;
    }

    return $inc * $scl;
}


