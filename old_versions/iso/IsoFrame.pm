#$Id: IsoFrame.pm 118 2010-12-30 13:04:55Z ikm $

use strict;
use warnings;

package IsoFrame; 

use base qw(Wx::Frame Class::Accessor::Fast);
use Wx qw[:everything];
use Data::Dumper;
# use Math::Geometry::Planar;
use Storable qw(dclone);
use YAML::XS qw(DumpFile LoadFile);
use List::Util;
use List::MoreUtils;
use Log::Log4perl qw(get_logger);
use Storable qw(dclone);

use IsoCanvas;
use Iso::Palette;

# globals {{{1

my $log = get_logger;

__PACKAGE__->mk_accessors(qw( canvas palette toolbar scene last_save_name));

our (
    $ID_PROPERTIES,
    $ID_EXPORT,
    $ID_FLOODFILL,
    $ID_SELECTAREA,
    )
    =
    ( 1000 .. 1100 );

################################################################################
# Constructor. Creates a Wx::Frame object, adds a sizer and a status bar and
# sets the window size.
sub new { #{{{1
    my ($class, $parent, $title, $pos, $size) = @_;

    $title  ||= "IsoScene"                 unless defined $title;
    $pos    = [ 200, 200 ]       unless defined $pos;
    $size   = [ 700, 700 ]       unless defined $size;

    my $self = $class->SUPER::new( $parent, -1, $title, $pos, $size);


    $self->{toolbar} = $self->CreateToolBar( wxTB_VERTICAL | wxNO_BORDER | wxTB_FLAT );

#    $self->{toolbar}->AddTool($ID_CLOSE, "", Wx::Bitmap->new("images/close.png", wxBITMAP_TYPE_ANY),
#        wxNullBitmap, wxITEM_NORMAL, "Close", "Close the application");

#    $self->{toolbar}->AddTool(wxID_NEW, "", Wx::Bitmap->new("images/new.png", wxBITMAP_TYPE_ANY),
#        wxNullBitmap, wxITEM_NORMAL, "Start new scene", "Select to begin a new scene");

    $self->{toolbar}->AddTool(wxID_OPEN, "", Wx::Bitmap->new("images/open.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_NORMAL, "Open existing scene", "Select to open an existing scene from file");

    $self->{toolbar}->AddTool(wxID_SAVE, "", Wx::Bitmap->new("images/save.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_NORMAL, "Save scene", "Select to save the current scene to an IsoScene file");

    $self->{toolbar}->AddTool($ID_EXPORT, "", Wx::Bitmap->new("images/export.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_NORMAL, "Export scene", "Select to export the current scene to an image file");

    $self->{toolbar}->AddTool($ID_PROPERTIES, "", Wx::Bitmap->new("images/properties.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_NORMAL, "Change scene properties", "Change the properties of the application or scene");

#    $self->{toolbar}->AddTool(wxID_UNDO, "", Wx::Bitmap->new("images/undo.png", wxBITMAP_TYPE_ANY),
#        wxNullBitmap, wxITEM_NORMAL, "Undo last tile created", "Remove the last tile which was created");

    $self->{toolbar}->AddTool($ID_SELECTAREA, "", Wx::Bitmap->new("images/select.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_CHECK, "Toggle Area Select mode", "Select 2 painted tiles as the opposite corners of an area.");

    $self->{toolbar}->AddTool($ID_FLOODFILL, "", Wx::Bitmap->new("images/floodfill.png", wxBITMAP_TYPE_ANY),
        wxNullBitmap, wxITEM_CHECK, "Toggle Floodfill mode", "Paint all blank tiles around selected tile");

    $self->{toolbar}->Realize;

    $self->palette(Iso::Palette->new($self->{toolbar}, -1));
    $self->{toolbar}->AddControl($self->palette);

    $self->{toolbar}->Realize;

    my $app = wxTheApp;

#    Wx::Event::EVT_TOOL($self, $ID_CLOSE, sub { $app->GetTopWindow->Destroy; });

    # accelerators without menus
    $self->SetAcceleratorTable(
        Wx::AcceleratorTable->new (
            [ wxACCEL_CTRL, 'S', wxID_SAVE ],
            [ wxACCEL_CTRL, 'O', wxID_OPEN ],
            [ wxACCEL_CTRL, 'N', wxID_NEW ],
            [ wxACCEL_CTRL, 'Z', wxID_UNDO ],
#            [ wxACCEL_NORMAL, 'Q', wxID_EXIT ],
#            [ wxACCEL_SHIFT, 'C', wxID_CLEAR ],
        )
    );

    Wx::Event::EVT_MENU( $self, wxID_SAVE, \&save_to_file );
    Wx::Event::EVT_MENU( $self, wxID_OPEN, \&open_from_file );
    Wx::Event::EVT_MENU( $self, wxID_NEW, \&start_new_scene);
    Wx::Event::EVT_MENU( $self, $ID_PROPERTIES, \&display_properties_window);
    Wx::Event::EVT_MENU( $self, $ID_EXPORT, sub { my $self = shift; $self->canvas->export_scene; } );
    Wx::Event::EVT_MENU( $self, $ID_FLOODFILL, sub {
        my ($self, $event) = @_;

        $log->info("floodfill event: checked = " . $event->IsChecked);
        $self->canvas->floodfill($event->IsChecked);
        
        return;
    } );
    Wx::Event::EVT_MENU( $self, $ID_SELECTAREA, sub {
        my ($self, $event) = @_;

        $log->info("selectarea event: checked = " . $event->IsChecked);
        $self->canvas->select_area($event->IsChecked);
        
        return;
    } );
    Wx::Event::EVT_MENU( $self, wxID_UNDO, sub { my $self = shift; $self->canvas->undo_last_create; } );
    Wx::Event::EVT_MENU( $self, wxID_EXIT, sub { $log->debug("quit"); $self->Destroy; } );

    $self->canvas(IsoCanvas->new($self, -1));

    $self->SetSizer(my $sizer = Wx::BoxSizer->new(wxHORIZONTAL));

    $sizer->Add($self->canvas, 1, wxEXPAND, 0 );

    $self->canvas->SetFocus;

    $self->SetBackgroundStyle(wxBG_STYLE_CUSTOM);

    $self->scene({});

    $self->Maximize(1);

    return $self;
}

################################################################################

#*******************************************************************************
sub display_properties_window { #{{{1
    my ($self, $event) = @_;

    my @properties = (
        {
            label => "Background Colour",
            routine => sub { my $self = shift; $self->canvas->change_background },
        },
        {
            label => "Background Line Colour",
            routine => sub { my $self = shift; $self->canvas->change_bg_line_colour },
        },
        {
            label => "Tile Line Colour",
            routine => sub { my $self = shift; $self->canvas->change_tile_line_colour },
        },
    );
    if ((my $property = Wx::GetSingleChoiceIndex( 'Select a Property to change', 'Properties', [ map { $_->{label} } @properties ], $self )) >= 0) {
        $properties[$property]->{routine}->($self);
    }

    return;
}

#*******************************************************************************
sub start_new_scene { #{{{1
    my ($self, $event) = @_;

    $self->scene({});
    $self->canvas->reset_scale;
    $self->last_save_name(0);

    return;
}

#*******************************************************************************
sub display_dialog { #{{{1
    my ($self, %param) = @_;

    my $dialog = Wx::MessageDialog->new($self,
        $param{message} || 'no message specified',
        $param{title} || 'no title specified',
        $param{style} || (wxOK|wxICON_INFORMATION),
    );

    my $rc = $dialog->ShowModal;

    return $rc;
}

#*******************************************************************************
sub save_to_file { #{{{1
    my ($self, $event) = @_;

    # save to previous file saved or loaded w/o prompting. Clearing scene clears
    # save name.
    my $file = $self->last_save_name;
    unless ($file) {
        my $dialog = Wx::FileDialog->new( $self,
            'Save Scene to file',
            '',
            '',
            'IsoScene files (*.isc)|*.isc',
            wxFD_SAVE);

        return 0 if $dialog->ShowModal == wxID_CANCEL;
        return 0 unless $file = ($dialog->GetPaths)[0];
    }

    $file .= ".isc" unless $file =~ /\./;

    my $busy = new Wx::BusyCursor;
    DumpFile($file, 
        {
            scene => $self->scene,
            origin_x => $self->canvas->origin_x,
            origin_y => $self->canvas->origin_y,
            scale => $self->canvas->scale,
            background_rgb => $self->canvas->background_brush->GetColour->GetAsString(wxC2S_HTML_SYNTAX),
            bg_line_rgb => $self->canvas->bg_line_pen->GetColour->GetAsString(wxC2S_HTML_SYNTAX),
            tile_line_rgb => $self->canvas->tile_line_pen->GetColour->GetAsString(wxC2S_HTML_SYNTAX),
            palette => {
                top => $self->palette->top,
                left => $self->palette->left,
                right => $self->palette->right,
                palette => $self->palette->palette,
            },
        });

    $log->info("File $file written ok\n");

    $self->last_save_name($file);

    return;
}

#*******************************************************************************

sub open_from_file { #{{{1
    my ($self, $event, $file) = @_;

    unless ($file) {

        my $dialog = Wx::FileDialog->new( $self,
            'Open Scene from file',
            '',
            '',
            'IsoScene files (*.isc)|*.isc',
            wxFD_OPEN);

        return 0 if $dialog->ShowModal == wxID_CANCEL;
        return 0 unless $file = ($dialog->GetPaths)[0];
    }

    my $busy = new Wx::BusyCursor;
    my $drawing = LoadFile($file);

    $self->scene($drawing->{scene});
    $self->canvas->origin_x($drawing->{origin_x});
    $self->canvas->origin_y($drawing->{origin_y});
    $self->canvas->scale($drawing->{scale});
    $self->canvas->change_background($drawing->{background_rgb});
    $self->canvas->change_bg_line_colour($drawing->{bg_line_rgb});
    $self->canvas->change_tile_line_colour($drawing->{tile_line_rgb});
    $self->palette->top($drawing->{palette}->{top});
    $self->palette->left($drawing->{palette}->{left});
    $self->palette->right($drawing->{palette}->{right});
    $self->palette->palette($drawing->{palette}->{palette});
#    $self->palette->make_brushes;

    $self->canvas->Refresh;
    $self->palette->Refresh;

    $self->last_save_name($file);

    return;
}

#*******************************************************************************
sub set_tool { #{{{1
    my ($self, $tool_id, $enable) = @_;

    $self->{toolbar}->ToggleTool($tool_id, $enable);

    return;
}

1;

# todo...
