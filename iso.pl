#!/usr/bin/perl -w -- 
#$Id: iso.pl 151 2012-11-11 10:05:50Z ikm $
# generated by wxGlade 0.6.3 on Mon Feb 23 09:18:07 2009
# To get wxPerl visit http://wxPerl.sourceforge.net/

use strict;
use threads;
use threads::shared;

use Wx qw[:everything];
use Log::Log4perl qw(get_logger);
use Getopt::Long;
use Data::Dumper qw(Dumper);
use FindBin qw($Bin);
use List::Util qw(max min);
# use Math::Trig;
# use Math::Round;
# use Math::VectorReal;
# use Math::MatrixReal;
use English qw(-no_match_vars);

use IsoApp;

# main variables {{{1

my $log;

my %options;

################################################################################

# main functions {{{1

################################################################################
sub assign_event_handler { #{{{2
    my ($control, $event, $handler) = @_;

#    $log->debug("handle $event for " . $self->name);

    my $event_type = "Wx::Event::$event";

    # find out how many args the event type needs to assign the handler
    my $arg_count = length prototype($event_type);

    my @controls = ($control);
    if ($arg_count == 3) {

        # 3 arg events need the parent as the first arg
        unshift @controls, $control->GetParent;
    }
    elsif ($arg_count == 4) {

        # the 4 arg version is used for handlers which affect a range of controls;
        # not modelled yet
        $log->logdie("no 4 arg events yet");
    }
    elsif ($arg_count != 2) {
        $log->logdie("bad event arg $arg_count");
    }

    # assign the handler
    {
        no strict 'refs';
        &{ $event_type }(@controls, $handler);
    }
}

# mainline {{{1

unless(caller){

    # list of options
    my @options = qw(
        man
        usage
        debug
        file=s
        quiet
        geometry=s
        script=s
    );

    GetOptions( \%options, @options ) or pod2usage(2);
    pod2usage(2) if $options{usage};
    pod2usage(1) if $options{help};
    pod2usage( -exitstatus => 0, -verbose => 2 ) if $options{man};

    $Data::Dumper::Sortkeys = 1;

    # put this in %options
    $options{bin_dir} = $Bin;

    $ENV{log_appenders} = $options{quiet} ? 'file' : "file, screen";
    $ENV{log_level}     = $options{debug} ? "DEBUG" : 'INFO';
    $ENV{log_dir}       ||= $options{bin_dir};
    $ENV{log_file_name} ||= 'iso';

    my $file = $options{bin_dir} . '/log4perl.conf';
    Log::Log4perl->init($file);
    $log = get_logger();

    $log->debug("Running $0: " . Dumper(\%options));

    my $app = IsoApp->new(\%options);

    $app->MainLoop();

}

################################################################################

__END__

=head1

TODO

Everything

=cut
