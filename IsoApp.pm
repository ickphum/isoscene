#$Id: IsoApp.pm 167 2013-03-01 14:23:56Z ikm $
package IsoApp;

use strict;
use warnings;

use Wx qw[:everything];
use base qw(Wx::App Class::Accessor::Fast);
use Data::Dumper;
use YAML qw(DumpFile LoadFile);
use Storable qw(dclone);
use Log::Log4perl qw(get_logger);
use File::Basename;
use Wx::XRC;

use IsoFrame;
use IsoScene;
use IsoConfig;

my $log = get_logger;

__PACKAGE__->mk_accessors( qw(bitmap scene config autosave_timer frame xrc) );

sub new { # {{{2
    my( $class, $option ) = @_;
    my $self = $class->SUPER::new();

    $self->{xrc} = Wx::XmlResource->new();
    $self->xrc->InitAllHandlers;
    # $self->xrc->Load('paste_selector.xrc');

    my @images = qw(cube menu paint sample import erase undo redo
        select tick_L tick_T tick_R tick_TL tick_TR
        copy cut paste small_paste small_cube);
    for my $image_name (qw(move area_L area_T area_R flood)) {
        push @images, "${image_name}_on", "${image_name}_off";
    }
    for my $action (grep { $_ !~ /$IsoFrame::AC_IMPORT|$IsoFrame::AC_PASTE/ } @IsoFrame::ACTIONS) {
        if ($action =~ /_all\z/) {
            push @images, "action_${action}";
        }
        else {
            for my $side ($IsoFrame::SI_LEFT, $IsoFrame::SI_TOP, $IsoFrame::SI_RIGHT) {
                push @images, "action_${action}_$side";
            }
        }
    }
    my %bitmap;
    for my $image_name (@images) {
        $log->debug("image_name $image_name");
        my $image_file = "images/$image_name.png";
        $log->logconfess("no image $image_name") unless -f $image_file;
        my $image = Wx::Image->new($image_file, wxBITMAP_TYPE_ANY);

        if ($image_name =~ /move|area|flood/) {
            $image->Rescale(40, ($image->GetHeight / $image->GetWidth) * 40, wxIMAGE_QUALITY_HIGH);
        }

        $bitmap{ $image_name } = Wx::Bitmap->new($image);
    }
    $self->bitmap(\%bitmap);

    $self->config( IsoConfig->new() );
    $self->autosave_timer(Wx::Timer->new($self));
    $self->autosave_timer->Start($self->config->autosave_period_seconds * 1000, wxTIMER_CONTINUOUS);
    Wx::Event::EVT_TIMER($self, -1, sub { $self->scene->save; $self->config->save; });

    my $filename = $option->{file} ||
        ((defined wxTheApp->config->previous_scene_file && -f (wxTheApp->config->previous_scene_file . '.isc'))
            ? wxTheApp->config->previous_scene_file
            : undef);

    $self->scene( IsoScene->new({ file => $filename }) );

    $self->frame( IsoFrame->new($self->scene));

    $self->set_frame_title;

    $self->SetTopWindow($self->frame);
    $self->frame->Show(1);

    return $self;
}

################################################################################
# Tricky to find a single point to do this (needed at load, open & save as),
# so just make it easy to call.
sub set_frame_title { #{{{2
    my ($self) = @_;

    my $scene_file = $self->scene->filename;
    $self->frame->SetTitle("$scene_file - IsoScene");

    return;
}

################################################################################
sub set_button_bitmap { #{{{1
    my ($button, $bitmap) = @_;

    $log->logconfess("null button") unless defined $button;
    $log->logconfess("null bitmap") unless defined $bitmap;

    $button->SetBitmap($bitmap);

    return $button;
}

################################################################################

sub OnInit { # {{{2
    my( $self ) = shift;

    Wx::InitAllImageHandlers();

    my $rc = $self->SUPER::OnInit();
    $log->debug("class init $rc");

    return 1;
}

################################################################################

sub OnExit { # {{{2
    my( $self ) = shift;

    $self->scene->save;
    $self->config->save;

    return 1;
}

1;