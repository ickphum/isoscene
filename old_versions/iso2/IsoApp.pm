package IsoApp;

use Wx qw[:everything];
use base qw(Wx::App Class::Accessor::Fast);
use Data::Dumper;
use YAML qw(DumpFile LoadFile);
use Storable qw(dclone);
use Log::Log4perl qw(get_logger);

use IsoFrame;

my $log = get_logger;

__PACKAGE__->mk_accessors( qw(bitmap top_color left_color right_color) );

sub new { # {{{2
    my( $class, @args ) = @_;
    my $self = $class->SUPER::new( @args );

    my @images = qw(cube menu);
    for my $image_name (qw(arrows area_left area_top area_right paintpot)) {
        push @images, "${image_name}_on", "${image_name}_off";
    }
    my %bitmap;
    for my $image_name (@images) {
        $log->info("image_name $image_name");
        $bitmap{ $image_name } = Wx::Bitmap->new(Wx::Image->new("images/$image_name.png", wxBITMAP_TYPE_ANY));
    }
    $self->bitmap(\%bitmap);

    $self->left_color('#234567');
    $self->top_color('#124578');
    $self->left_color('#bc90e3');

    $self->{frame} = IsoFrame->new();

    $self->SetTopWindow($self->{frame});
    $self->{frame}->Show(1);

    return $self;
}

#*******************************************************************************

sub OnInit { # {{{2
    my( $self ) = shift;

    Wx::InitAllImageHandlers();

    my $rc = $self->SUPER::OnInit();
    $log->debug("class init $rc");

    return 1;
}

1;
