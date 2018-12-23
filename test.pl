#!/usr/bin/perl -w -- 

use threads;
use threads::shared;


use Wx qw[:everything];
use strict;
use Log::Log4perl qw(get_logger);
use Data::Dumper qw(Dumper);
use Time::Piece;

my $DONE_EVENT : shared = Wx::NewEventType;

my $log;

#################################################################################
#################################################################################
package TestCanvas; #{{{1

use Wx qw(:everything);
use base qw(Wx::Panel Class::Accessor::Fast);

__PACKAGE__->mk_accessors( qw(
    x
    result
));


sub new { # {{{2
    my( $class, $parent ) = @_;
    my $self = $class->SUPER::new( $parent, -1, [ -1, -1 ], [ -1, -1 ], 0, "test canvas"); 
    $self->x(50);
    $self->result('Never at Never');

    Wx::Event::EVT_PAINT( $self,
        sub {
            my $dc = Wx::PaintDC->new( $self );
            my $x = $self->x;
            $dc->DrawText($self->result, 50, 50);
            $dc->DrawLine($x, 40, $x, 60);
            $self->x($x > 120 ? 50 : $x + 1);

            return;
        }
    );

    my $timer = Wx::Timer->new($self, -1);

    Wx::Event::EVT_TIMER( $self, $timer,
        sub {
            $self->Refresh;
            return;
        }
    );

    $timer->Start(20, wxTIMER_CONTINUOUS);

    return $self;
}

#################################################################################
#################################################################################
package TestFrame; # {{{1

use strict;
use warnings;

use Wx qw[:everything];
use Data::Dumper qw(Dumper);
use base qw(Wx::Frame);
use Convert::Color;
use Convert::Color::RGB8;
use Math::Round;

sub new { # {{{2
    my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;

    $self = $self->SUPER::new( undef , -1, 'Test', [ 1000, 100 ], [ 400, 300 ]);
    $self->{v_sizer} = Wx::BoxSizer->new(wxVERTICAL);
#    $self->{canvas} = TestCanvas->new($self);
#    $self->{v_sizer}->Add($self->{canvas}, 1, wxEXPAND, 1);
    for my $label (qw(darken lighten)) {
        my $button = Wx::Button->new($self, -1, $label);
        $button->SetName($label);

        my $button_handler = {
            darken => \&darken,
            lighten => \&darken,
        };

        if (my $handler = $button_handler->{$label}) {
            Wx::Event::EVT_BUTTON($self, $button, $handler);
        }
        else {
            $log->warn("no handler for $label");
        }

        $self->{v_sizer}->Add($button);

    }

    $self->{label} = Wx::StaticText->new($self, -1, "test label", wxDefaultPosition, wxDefaultSize);
    $self->{v_sizer}->Add($self->{label}, 0, 0, 0);
    $self->{panel} = Wx::Panel->new($self);
    $self->{v_sizer}->Add($self->{panel}, 1, wxEXPAND, 1);
    $self->{panel}->SetBackgroundColour(Wx::Colour->new(172,126,61));

    $self->{undo_stack} = [];
    $self->{redo_stack} = [];
    $self->{actions} = [];
    $self->{action_number} = 1;

    $self->SetSizer($self->{v_sizer});
    $self->SetTitle("Test");

    Wx::Event::EVT_COMMAND($self, -1, $DONE_EVENT, \&done);

    return $self;
}

#################################################################################
sub darken {
    my ($frame, $event) = @_; @_ = ();

    my $inc = $event->GetEventObject->GetName eq 'darken'
        ? -0.05
        : 0.05;

    my $colour = $frame->{panel}->GetBackgroundColour;
    my @rgb = ( $colour->Red, $colour->Green, $colour->Blue );
    my $converter = Convert::Color::RGB8->new( @rgb );
    my $hsv = $converter->convert_to('hsv');
    my ($h, $s, $v) = Math::Round::nearest(0.01, $hsv->hsv);

    $log->info("RGB @rgb, hsv $h $s $v");
    $v += $inc;
    $v = 0 if $v < 0;
    $v = 1 if $v > 1;
    my $c2 = Convert::Color::HSV->new($h, $s, $v);
    my $rgb8 = $c2->convert_to('rgb8');
    $log->info("new rgb " . join(',', $rgb8->rgb8) . ' or ' . $rgb8->hex);
    my $new_color = Wx::Colour->new($rgb8->rgb8);
    $frame->{panel}->SetBackgroundColour(Wx::Colour->new($rgb8->rgb8));

    return;
}

#################################################################################
sub start_read {
    my ($frame, $event) = @_;

    $frame->{canvas}->result('read');
    $frame->{canvas}->Refresh;
    $frame->read_file;

    return;
}

#################################################################################
sub done {
    my ($frame, $event) = @_;

    $log->info("done");
    $frame->{canvas}->result($event->GetData);
    $frame->{canvas}->Refresh;
}

#################################################################################
sub read_file {
    my ($frame) = @_;

    open( my $fh, "<", 'XeviousGL.isc')
        or die "can't read from file";

    my $checksum = 0;
    while (read($fh, my $buffer, 1)) {
        $checksum = $checksum ^ ord($buffer);
    }

    my $result = $checksum . ' ' . (scalar localtime);
    my $threvent = new Wx::PlThreadEvent( -1, $DONE_EVENT, $result );
    Wx::PostEvent($frame, $threvent);

    return;
}

#################################################################################
#################################################################################
package TestApp; # {{{1

use Wx qw[:everything];
use base qw(Wx::App Class::Accessor::Fast);
__PACKAGE__->mk_accessors( qw(frame));

sub new { # {{{2
    my( $class, @args ) = @_;
    my $self = $class->SUPER::new( @args );

    $self->frame( TestFrame->new($self, -1) );
    $self->SetTopWindow($self->frame);
    $self->frame->Show(1);

    return $self;
}

#*******************************************************************************
sub OnInit { # {{{2
    my( $self ) = shift;

    my $rc = $self->SUPER::OnInit();

    return 1;
}

#*******************************************************************************

package main; # {{{1

$ENV{log_appenders} = "file, screen";
$ENV{log_level}     = 'INFO';
$ENV{log_dir}       = ".";
$ENV{log_file_name} = 'test';

Log::Log4perl->init("./log4perl.conf");
$log = get_logger();

unless(caller){

    my $app = TestApp->new();
    $app->MainLoop();
}


################################################################################

__END__

