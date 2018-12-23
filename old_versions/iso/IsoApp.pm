package IsoApp;

use Wx qw[:everything];
use base qw(Wx::App Class::Accessor::Fast);
use Data::Dumper;
use YAML qw(DumpFile LoadFile);
use Storable qw(dclone);
use Log::Log4perl qw(get_logger);

use IsoFrame;

my $log = get_logger;

__PACKAGE__->mk_accessors( qw(rotate_around_y tick glow_cache things new_thing up
    angle near far named_things current_event selected_things menu_text dirty
    undo_stack popup_thing vector_frame resizing_things initial_eye_pos
    group_reference_vector count_fps moving_eye texture_names shape_names
    walking walk_mode build_grid vertex_sphere ) );

sub new { # {{{2
    my( $class, @args ) = @_;
    my $self = $class->SUPER::new( @args );

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
