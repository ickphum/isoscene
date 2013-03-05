#$Id: IsoFrame.pm 163 2013-02-21 09:04:34Z ikm $

use strict;
use warnings;

package IsoExportOptions; 

use base qw(Wx::Dialog);
use Wx qw[:everything];
use Data::Dumper;
use Storable qw(dclone);
use YAML::XS qw(DumpFile LoadFile);
use List::Util qw(min max);
use List::MoreUtils qw(firstidx);
use Log::Log4perl qw(get_logger);
use Storable qw(dclone);
use Time::HiRes qw(time);
use File::Copy;
use English qw(-no_match_vars);
use Time::Piece;

use IsoCanvas;

# attributes {{{1

# globals {{{1

my $log = get_logger;

my $Control;

################################################################################
# Constructor. Creates a Wx::Dialog object
sub new { #{{{1
    my ($class, $parent, $pos) = @_;

    $pos    = [ 800, 200 ]       unless defined $pos;

    my $self = bless wxTheApp->xrc->LoadDialog($parent, 'export_options');

    $Control = {};
    for my $child ( $self->GetChildren ) {
        $log->info("child: " . $child->GetName);
        $Control->{ $child->GetName } = $child;
    }

    my $app = wxTheApp;
    if ($app->scene->export_options) {
        $app->load_dialog_settings('export_options', $Control);
    }

    Wx::Event::EVT_BUTTON($self, wxID_OK, \&save_export_options);

    return $self;
}

################################################################################
sub save_export_options { #{{{1
    my ($dialog, $event) = @_;
    $log->info("save_export_options: @_");

    my $app = wxTheApp;

    $app->save_dialog_settings('export_options', $Control);
    $log->info("export_options: " . Dumper($app->scene->export_options));

    $dialog->EndModal(wxID_OK);
    return;
}

################################################################################

1;

# todo...
