#$Id: IsoFrame.pm 163 2013-02-21 09:04:34Z ikm $

use strict;
use warnings;

package IsoPasteSelector; 

use base qw(Wx::Dialog Class::Accessor::Fast);
use Wx qw[:everything];
use Data::Dumper;
use Storable qw(dclone);
use YAML::XS qw(Load);
use List::Util qw(min max);
use List::MoreUtils qw(firstidx);
use Log::Log4perl qw(get_logger);
use Storable qw(dclone);
use Time::HiRes qw(time);
use File::Copy;
use English qw(-no_match_vars);
use Time::Piece;
use IO::Uncompress::Unzip qw(unzip $UnzipError) ;

use IsoCanvas;

# attributes {{{1

__PACKAGE__->mk_accessors( qw( all_thumbs scene_list_ctrl current_thumb));

# globals {{{1

my $log = get_logger;

################################################################################
# Constructor. Creates a Wx::Dialog object, adds a sizer and a status bar and
# sets the window size.
sub new { #{{{1
    my ($class, $parent, $pos) = @_;

    $pos    = [ 800, 200 ]       unless defined $pos;

    my $self = $class->SUPER::new( $parent, -1, "Select Item To Paste", $pos, [ 300, 600 ], wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);

    my $app = wxTheApp;

    $self->SetSizer(my $dialog_sizer = Wx::BoxSizer->new(wxVERTICAL));

    my $top_panel = Wx::Panel->new($self);
    $top_panel->SetSizer(my $top_panel_sizer = Wx::BoxSizer->new(wxHORIZONTAL));
    $dialog_sizer->Add($top_panel, 1, wxEXPAND, 0);

    my $scroll_panel = Wx::ScrolledWindow->new($top_panel, -1, wxDefaultPosition, wxDefaultSize, wxVSCROLL );
    $scroll_panel->SetScrollRate(10, 10);
    $scroll_panel->SetSizer(my $scroll_panel_sizer = Wx::BoxSizer->new(wxVERTICAL));

    # We want to display clipboard items from other scenes; store each scene here as it's loaded.
    my $this_scene_name = $app->scene->filename;
    my %other_scene;

    # We're driving this from the thumbnail images because the whole point is to display
    # the images in a dialog for selection; if there's no image there, the existence of the clipboard
    # item in the scene isn't relevant.
    for my $thumb_file (sort glob "clipboard/*.png") {
        $log->debug("thumb_file $thumb_file");

        if ($thumb_file =~ /clipboard\/((\w+)_\d{8}_\d{6})/) {
            my ($item_name, $scene) = ($1,$2);

            # we only want other scenes, since the current scene's items were displayed
            # in the same popup that led here.
            next if $scene eq $this_scene_name;

            unless ($other_scene{$scene}) {
                if (-f "$scene.isb") {

                    # create the other_scene entry 
                    $other_scene{$scene} = {
                        name => $scene,
                        thumbs => [],
                    };

                }
                else {
                    $log->info("clipboard item for unknown scene $scene in $thumb_file");
                    next;
                }
            }

            push @{ $other_scene{$scene}->{thumbs} }, { file => $thumb_file, name => $item_name };
        }
        else {
            $log->warn("bad clipboard thumb $thumb_file");
        }
    }

    # show thumbnails for all scenes and build a single list holding scene name and clipboard index for all items;
    # all we can get from the selected item is one integer.
    my @all_thumbs = ();
    $self->scene_list_ctrl({});
    for my $other_scene (sort { $a->{name} cmp $b->{name} } grep { scalar @{ $_->{thumbs} } } values %other_scene) {

        my $image_list = Wx::ImageList->new( 64, 64, 1 );
        for my $thumb ( @{ $other_scene->{thumbs} } ) {
            $log->debug("add $thumb->{file}");
            $image_list->Add( Wx::Bitmap->new($thumb->{file}, wxBITMAP_TYPE_ANY));
        }

        my $list = Wx::ListCtrl->new( $scroll_panel, -1, wxDefaultPosition, [ 200, 200], wxLC_ICON );
        $self->scene_list_ctrl->{ $other_scene->{name} } = $list;

        Wx::Event::EVT_LIST_ITEM_SELECTED($self, $list, sub {
            my ($dialog, $event) = @_;
            $log->debug(sprintf "dialog $dialog, selected list pos %d, thumb %s ", $event->GetIndex, $event->GetItem->GetData);

            # clear selections in other lists
            my $thumb_index = $event->GetItem->GetData;
            my $thumb_info = $dialog->all_thumbs->[$thumb_index];
            my ($scene, $index) = @{ $thumb_info };
            $log->debug("selected thumb for $scene->{name}, $index");
            for my $other_scene (grep { $_ ne $scene->{name} } keys %{ $dialog->scene_list_ctrl }) {
                my $list_ctrl = $self->scene_list_ctrl->{$other_scene};
                $log->debug("clear list for $other_scene $list_ctrl");
                for my $row (0 .. $list_ctrl->GetItemCount - 1) {
                    $list_ctrl->SetItemState($row, 0, wxLIST_STATE_SELECTED);
                }
            }

            # remember the most recent selection
            $dialog->current_thumb($thumb_index);
        });

        Wx::Event::EVT_LIST_ITEM_ACTIVATED($self, $list, sub {
            my ($parent, $event) = @_;
            $log->debug(sprintf "dialog $parent, activated list pos %d, thumb %s ", $event->GetIndex, $event->GetItem->GetData);

            # exit the dialog
            $parent->use_clipboard_item;
        });

        $list->AssignImageList($image_list, wxIMAGE_LIST_NORMAL);

        # add all the images to the display
        for my $i (0 .. $image_list->GetImageCount - 1) {
            $list->InsertImageItem($i, $i);
            push @all_thumbs, [ $other_scene, $other_scene->{thumbs}->[$i]->{name} ];
            $list->SetItemData($i, $#all_thumbs);
        }

        $scroll_panel_sizer->Add(Wx::StaticText->new($scroll_panel, -1, $other_scene->{name}), 0, wxEXPAND | wxALL, 3);
        $scroll_panel_sizer->Add($list, 0, wxEXPAND, 0);
    }
    $self->all_thumbs(\@all_thumbs);

    $top_panel_sizer->Add($scroll_panel, 1, wxEXPAND, 0);

    my $std_button_sizer = $self->CreateSeparatedButtonSizer(wxOK | wxCANCEL);
    $dialog_sizer->Add($std_button_sizer, 0, wxEXPAND, 0);

    Wx::Event::EVT_BUTTON($self, wxID_OK, \&use_clipboard_item);

    return $self;
}

################################################################################
sub use_clipboard_item { #{{{1
    my ($dialog, $event) = @_;

    # we've double-clicked or hit return on an item
    $log->info("use_clipboard_item");
    if (defined (my $index = $dialog->current_thumb)) {

        # all we have to do here is put the selected clipboard item on the current
        # scene's clipboard; the success code from the dialog will trigger a paste.
        my $scene = wxTheApp->scene;
        my $canvas = $dialog->GetParent->canvas;
        my $thumb_info = $dialog->all_thumbs->[$index];
        $log->info("index $index, thumb_info " . Dumper($thumb_info));
        my ($other_scene_name, $clipboard_item_name) = ($thumb_info->[0]->{name}, $thumb_info->[1]);

        my ($tiles, $error) = IsoScene::read_clipboard_item($canvas, $other_scene_name, $clipboard_item_name);

        if ($error) {
            $dialog->EndModal(wxID_CANCEL);
            $dialog->GetParent->display_message($error);
            return;
        }

#        # bear in mind that the scene items stored in $dialog->other_scene are
#        # just hashes, not IsoScene objects.
#        my $clipboard_item = $other_scene->{scene}->{clipboard}->[$index];
#        $log->info("clipboard_item $index from $other_scene->{name} : " . Dumper($clipboard_item));

        # assign a new name and copy the thumbnail under that name
        my $new_name = $scene->filename . localtime->strftime("_%Y%m%d_%H%M%S");
        unless (File::Copy::copy("clipboard/$clipboard_item_name.png", "clipboard/$new_name.png")) {
            $log->warn("couldn't copy clipboard/$clipboard_item_name.png to clipboard/$new_name.png : $OS_ERROR");
            return;
        }

        my $clipboard_item = {
            name => $new_name,
            tiles => $tiles,
        };

        # add the item to the clipboard
        unshift @{ $scene->clipboard }, $clipboard_item;

        $dialog->EndModal(wxID_OK);
    }

    return;
}

################################################################################

1;

# todo...
