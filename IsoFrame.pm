#$Id: IsoFrame.pm 169 2013-03-02 05:27:05Z ikm $

use strict;
use warnings;

package IsoFrame; 

use base qw(Wx::Frame Class::Accessor::Fast);
use Wx qw[:everything];
use Data::Dumper;
use Storable qw(dclone);
use YAML::XS qw(DumpFile LoadFile);
use List::Util qw(min max);
use List::MoreUtils qw(firstidx);
use Log::Log4perl qw(get_logger);
use Storable qw(dclone);
use Time::HiRes qw(time);
use Convert::Color;
use File::Copy;
use English qw(-no_match_vars);

use IsoCanvas;
use IsoPasteSelector;
use IsoExportOptions;

# attributes {{{1

__PACKAGE__->mk_accessors( qw(
    tool_panel canvas branch_choice_pnl

    mode action previous_action mode_btn action_btn erase_mode select_mode clipboard_btn misc_btn

    cube_x cube_y cube_width cube_height cube_brush
    _current_side
    
    import_file

    undo_timer undo_or_redo_flag branch_choice_position original_branch

    slow_button_timer slow_button_name
));

# globals {{{1

my $log = get_logger;

our (
    $ID_PROPERTIES,
    $ID_EXPORT,
    $ID_FLOODFILL,
    $ID_SELECTAREA,
    $ID_REDO,
    )
    =
    ( 1000 .. 1100 );

our @SIDES = our (
    $SI_LEFT,
    $SI_TOP,
    $SI_RIGHT,
    ) = qw(L T R);

our %OTHER_SIDES = (
    $SI_LEFT => [ $SI_TOP, $SI_RIGHT ],
    $SI_TOP => [ $SI_LEFT, $SI_RIGHT ],
    $SI_RIGHT => [ $SI_TOP, $SI_LEFT ],
);

our (
    $MO_TILE,
    $MO_AREA,
    $MO_FLOOD,
    $MO_MOVE,
    ) = qw(tile area flood move);

our (
    $AM_CURRENT,
    $AM_OTHERS,
    $AM_ALL,
    ) = qw(current others all);

our @ACTIONS = our (
    $AC_PAINT,
    $AC_SAMPLE,
    $AC_ERASE,
    $AC_ERASE_OTHERS,
    $AC_ERASE_ALL,
    $AC_LIGHTEN,
    $AC_DARKEN,
    $AC_SHADE,
    $AC_IMPORT,
    $AC_PASTE,
    $AC_SELECT,
    $AC_SELECT_OTHERS,
    $AC_SELECT_ALL,
    $AC_CHOOSE_BRANCH,
    ) = qw(paint sample erase_current erase_others erase_all lighten darken shade import paste select_current select_others select_all choose_branch);

################################################################################
# Constructor. Creates a Wx::Frame object, adds a sizer and a status bar and
# sets the window size.
sub new { #{{{1
    my ($class, $scene, $parent, $title, $pos, $size) = @_;

    $title  ||= "IsoScene"                 unless defined $title;
    $pos    = [ 1100, 200 ]       unless defined $pos;
    $size   = [ 700, 700 ]       unless defined $size;

    my $self = $class->SUPER::new( $parent, -1, $title, $pos, $size);

    my $app = wxTheApp;

    # undo timer
    $self->undo_timer(Wx::Timer->new($self));
    Wx::Event::EVT_TIMER($self, $self->undo_timer->GetId, sub {
        $self->canvas->undo_or_redo();

        # the timer is set to a single slow wait period, then switches to fast continuous
        unless ($self->undo_timer->IsRunning) {
            $self->undo_timer->Start(wxTheApp->config->undo_repeat_milliseconds, wxTIMER_CONTINUOUS);
        }
    });

    # slow button timer
    $self->slow_button_timer(Wx::Timer->new($self));
    $self->slow_button_name(0);
    Wx::Event::EVT_TIMER($self, $self->slow_button_timer->GetId, \&slow_button_timer_expired);

    # accelerators without menus
    $self->SetAcceleratorTable(
        Wx::AcceleratorTable->new (
            [ wxACCEL_CTRL, 'S', wxID_SAVE ],
            [ wxACCEL_CTRL, 'O', wxID_OPEN ],
            [ wxACCEL_CTRL, 'Z', wxID_UNDO ],
            [ wxACCEL_CTRL, 'Y', $ID_REDO ],
            [ wxACCEL_CTRL, 'Q', wxID_EXIT ],
#            [ wxACCEL_SHIFT, 'C', wxID_CLEAR ],
        )
    );

    Wx::Event::EVT_MENU( $self, wxID_SAVE, \&save_to_file );
    Wx::Event::EVT_MENU( $self, wxID_OPEN, \&open_from_file );
    Wx::Event::EVT_MENU( $self, wxID_UNDO, sub { my $self = shift; $self->canvas->undo_or_redo(0); } );
    Wx::Event::EVT_MENU( $self, $ID_REDO, sub { my $self = shift; $self->canvas->undo_or_redo(1); } );
    Wx::Event::EVT_MENU( $self, wxID_EXIT, sub { $log->debug("quit"); $self->Destroy; } );

    $self->SetSizer(my $sizer = Wx::BoxSizer->new(wxHORIZONTAL));

    my $bitmap = $app->bitmap;

    $self->tool_panel(my $tool_panel = Wx::Panel->new($self,-1));
    $tool_panel->SetSizer(my $tool_sizer = Wx::BoxSizer->new(wxVERTICAL));

    my $cube_pnl = Wx::Panel->new($tool_panel, -1, wxDefaultPosition, [ $bitmap->{cube}->GetWidth,  $bitmap->{cube}->GetHeight ], 0);
    $tool_sizer->Add($cube_pnl, 0, wxALIGN_CENTRE);
    Wx::Event::EVT_PAINT( $cube_pnl, \&cube_paint_event );
    Wx::Event::EVT_LEFT_DOWN($cube_pnl, sub { $self->slow_button_down($_[1], 'cube'); });
    Wx::Event::EVT_LEFT_UP($cube_pnl, sub { $self->slow_button_up($_[1], 'cube'); });
    $self->cube_width($bitmap->{cube}->GetWidth);
    $self->cube_height($bitmap->{cube}->GetHeight);

    $log->debug("cube built");

    $self->action_btn({});
    my @column_buttons = ();
    for my $action ( $AC_PAINT, $AC_SAMPLE, 'erase', 'select', $AC_LIGHTEN, $AC_DARKEN, $AC_SHADE) {

        my $action_btn = $self->action_btn->{$action} = Wx::BitmapButton->new($tool_panel, -1,
            $action =~ /erase|select/
            ? $bitmap->{"action_${action}_current_R"}
            : $bitmap->{"action_${action}_R"});
        $action_btn->SetToolTip(ucfirst $action);
        if ($action =~ /erase|select/) {
            Wx::Event::EVT_LEFT_DOWN($action_btn, sub { $self->slow_button_down($_[1], $action); });
            Wx::Event::EVT_LEFT_UP($action_btn, sub { $self->slow_button_up($_[1], $action); });
        }
        else {
            Wx::Event::EVT_BUTTON($self, $action_btn, sub { $_[0]->change_action($action); });
        }
        push @column_buttons, $action_btn;
    }

    push @column_buttons, 0, 10, 10;

    $self->mode_btn({});

    my $move_btn = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{move_off});
    $move_btn->SetToolTip("Pan");
    $self->mode_btn->{$MO_MOVE} = $move_btn;
    Wx::Event::EVT_BUTTON($self, $move_btn, sub { $_[0]->change_mode($MO_MOVE); });
    push @column_buttons, $move_btn;

    my $area_btn = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{area_R_off});
    $area_btn->SetToolTip("Use Area");
    $self->mode_btn->{$MO_AREA} = $area_btn;
    Wx::Event::EVT_BUTTON($self, $area_btn, sub { $_[0]->change_mode($MO_AREA); });
    push @column_buttons, $area_btn;

    # clipboard operations
    $self->clipboard_btn({});
    for my $operation (qw(copy cut paste)) {
        my $button = $self->clipboard_btn->{$operation} = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{$operation} );
        $button->SetToolTip( ucfirst $operation );
        if ($operation eq 'paste') {
            Wx::Event::EVT_LEFT_DOWN($button, sub { $self->slow_button_down($_[1], $operation); });
            Wx::Event::EVT_LEFT_UP($button, sub { $self->slow_button_up($_[1], $operation); });
        }
        else {
            Wx::Event::EVT_BUTTON($self, $button, sub { $self->canvas->clipboard_operation($operation); });
        }
        push @column_buttons, $button;
    }

    $self->misc_btn({});

#    my $flood_btn = $self->misc_btn->{flood} = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{flood_off});
#    $self->mode_btn->{$MO_FLOOD} = $flood_btn;
#    Wx::Event::EVT_BUTTON($self, $flood_btn, sub { $_[0]->change_mode($MO_FLOOD); });
#    push @column_buttons, $flood_btn;
#    push @column_buttons, 0;

    my $button = $self->misc_btn->{selection_tools} = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{selection_tools} );
    $button->SetToolTip("Selection Tools");
    Wx::Event::EVT_BUTTON($self, $button, sub { $self->show_button_popup([ qw(select_all select_none select_visible) ], \&do_selection_tool, $button->GetScreenPosition); });
    push @column_buttons, $button;

    my $column_button_szr = Wx::FlexGridSizer->new(0,2,0,0);
    for my $button (@column_buttons) {
        if (ref $button) {
            $button->SetWindowStyleFlag(wxBU_EXACTFIT);
            $column_button_szr->Add($button, 0, wxEXPAND);
        }
        else {

            # spacer height
            $column_button_szr->Add(0, $button);
        }
    }
    $tool_sizer->Add($column_button_szr, 0, wxEXPAND);

    my $undo_redo_button_szr = Wx::BoxSizer->new(wxHORIZONTAL);
    for my $operation (qw(undo undo_redo_menu redo)) {
        my $button = $self->misc_btn->{$operation} = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{$operation}); 
        if ($operation eq 'undo_redo_menu') {
            $button->SetToolTip('Undo/Redo Tools');
            Wx::Event::EVT_BUTTON($self, $button, sub {
                my ($self, $event) = @_;

                $log->info("undo_redo_menu @_");
                my $choices = [ qw(undo_to_branch redo_to_branch new_branch) ];

                # if we're on a branch, display the choose_branch button
                if ((ref $self->canvas->scene->redo_stack->[-1]) eq 'HASH') {
                    unshift @{ $choices }, 'choose_branch';
                }

                $self->show_button_popup($choices, \&do_undo_redo_tool, $button->GetScreenPosition);
                return;
            });
        }
        else {
            Wx::Event::EVT_LEFT_DOWN($button, sub { $self->slow_button_down($_[1], $operation); });
            Wx::Event::EVT_LEFT_UP($button, sub { $self->slow_button_up($_[1], $operation); });
        }
        $button->SetWindowStyleFlag(wxBU_EXACTFIT);
        $undo_redo_button_szr->Add($button, 0, wxEXPAND);
    }

    $tool_sizer->Add($undo_redo_button_szr, 0, wxEXPAND);

    my $menu_btn = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{menu});
    $menu_btn->SetToolTip("Menu");
    $tool_sizer->Add($menu_btn, 0, wxEXPAND);
    Wx::Event::EVT_BUTTON($self, $menu_btn, sub { $_[0]->show_menu; });

    $log->debug("mode buttons");

    $sizer->Add($tool_panel);

    $self->action($AC_PAINT);
    $self->mode($MO_TILE);
    $self->erase_mode($AM_CURRENT);
    $self->select_mode($AM_CURRENT);
    $self->current_side($SI_RIGHT);

    $self->canvas(IsoCanvas->new($self, $scene));
    $log->debug("canvas built ok");

    # fix redo button in case we saved on a branch point
    $self->canvas->set_undo_redo_button_states;

    my $canvas_side_sizer = Wx::BoxSizer->new(wxVERTICAL);

    $canvas_side_sizer->Add($self->canvas, 1, wxEXPAND );

    $self->branch_choice_pnl( $app->xrc->LoadPanel($self, 'choose_branch') );
    $canvas_side_sizer->Add($self->branch_choice_pnl, 0, wxEXPAND );
    $self->branch_choice_pnl->Hide;

    Wx::Event::EVT_CHOICE($self, $self->branch_choice_pnl->FindWindow('branch_chc'), \&change_branch_choice);
    Wx::Event::EVT_BUTTON($self, $self->branch_choice_pnl->FindWindow('ok_btn'), sub { $_[0]->finish_branch_choice(1); } );
    Wx::Event::EVT_BUTTON($self, $self->branch_choice_pnl->FindWindow('cancel_btn'), sub { $_[0]->finish_branch_choice(0); } );

    $sizer->Add($canvas_side_sizer, 1, wxEXPAND);

    $self->canvas->SetFocus;

    $self->SetBackgroundStyle(wxBG_STYLE_CUSTOM);

    $self->cube_brush({
        L => Wx::Brush->new(Wx::Colour->new($scene->left_rgb), wxBRUSHSTYLE_SOLID),
        T => Wx::Brush->new(Wx::Colour->new($scene->top_rgb), wxBRUSHSTYLE_SOLID),
        R => Wx::Brush->new(Wx::Colour->new($scene->right_rgb), wxBRUSHSTYLE_SOLID),
    });

    $self->SetIcon(Wx::Icon->new("images/icon.png", wxBITMAP_TYPE_PNG));

    $log->debug("frame $self");

    # $self->Maximize(1);

    return $self;
}

################################################################################
sub change_action { #{{{1
    my ($self, $action) = @_;

    $log->debug("change_action $action");

    # changing to another action during paste cancels the paste
    if ($self->action eq $AC_PASTE) {
        $self->canvas->paste_list(undef);
        $self->canvas->paint_shape(0);
    }

    # changing to another action during import cancels the import
    elsif ($self->action eq $AC_IMPORT) {
        $self->canvas->cursor_multiplier_x(1);
        $self->canvas->cursor_multiplier_y(1);
    }

    elsif ($action eq $AC_SHADE) {

        # change the cube colors of the unselected sides
        my $side = $self->current_side;
        my $brush = $self->cube_brush->{ $side };
        my $colour = $brush->GetColour;
        my @rgb = ($colour->Red, $colour->Green, $colour->Blue);
        my $change = wxTheApp->config->shade_change;
        my %relative_shade = split ',', wxTheApp->config->relative_shades;
        for my $other_side ( @{ $OTHER_SIDES{ $side } } ) {
            my $side_change = ($relative_shade{$side} - $relative_shade{$other_side}) * $change;
            my @new_rgb = map { List::Util::max(List::Util::min($_ + $side_change, 255), 0) } @rgb;
            $self->cube_brush->{ $other_side }->SetColour(Wx::Colour->new(@new_rgb));
            $self->update_scene_color($other_side);
        }

        # revert to paint
        $action = $AC_PAINT;
    }
    elsif ($action =~ /\A(?:$AC_LIGHTEN|$AC_DARKEN)\z/) {

        # change the cube color of the selected side
        my $side = $self->current_side;
        my $brush = $self->cube_brush->{ $side };
        my $colour = $brush->GetColour;
        my @rgb = ($colour->Red, $colour->Green, $colour->Blue);
        my $change = wxTheApp->config->shade_change;
        $change = - $change if $action eq $AC_DARKEN;
        my @new_rgb = map { List::Util::max(List::Util::min($_ + $change, 255), 0) } @rgb;
        $self->cube_brush->{ $side }->SetColour(Wx::Colour->new(@new_rgb));
        $self->update_scene_color($side);

        # revert to paint
        $action = $AC_PAINT;
    }

    # changing action during move mode cancels the move; we've probably forgotten move is on
    # and are about to use the new action.
    if ($self->mode eq $MO_MOVE) {

        # this looks weird but is due to the mode buttons being toggles;
        # we're already in move mode, so doing this again turns it off.
        $self->change_mode($MO_MOVE);
    }

    $self->action($action);
    $self->canvas->set_cursor;
    $self->Refresh;

    return;
}

################################################################################
sub show_menu { #{{{1
    my ($self) = @_;

    my $app = wxTheApp;

    my ($save, $save_as, $export, $import, $new, $open, $scene_options, $config_options) = ( "Save", "Save As...", qw(Export Import New Open), "Scene Options", "Config Options" );
    my $choices = [ $new, $open, $save_as, $export, $import, $scene_options, $config_options, $save, ];
    if (my $string = Wx::GetSingleChoice( 'File Menu', 'IsoScene', $choices, $self )) {

        if ($string eq $save) {
            $self->save_to_file;
        }
        elsif ($string eq $open) {
            $self->open_from_file;
        }
        elsif ($string eq $new) {
            $app->scene->save;
            $app->scene( IsoScene->new() );
            $self->canvas->scene($app->scene);
            $app->set_frame_title;
            $self->Refresh;
        }
        elsif ($string eq $save_as) {
            my $old_name = $app->scene->filename;

            # clear the name so the file dialog opens for a new name
            $app->scene->filename(0);

            if ($self->save_to_file) {
                $app->set_frame_title;

                # the clipboard thumbnails for this scene now have the wrong name, both
                # internally (in the clipboard list) and on disk. They'll work ok from
                # within this scene (since the names still match, ie the name in the clipboard
                # item points to the correct file on disk) but:
                #   a)  it won't work when displaying the paste selector, since there we work
                #       from thumb name back to to scene name,
                #   b)  it's kind of messy
                for my $item (@{ $app->scene->clipboard }) {
                    my $thumb_file = "clipboard/$item->{name}.png";
                    if (-f $thumb_file ) {

                        if ($item->{name} =~ /(\w+)(_\d{8}_\d{6})/) {
                            my ($scene, $time) = ($1,$2);
                            $item->{name} = $app->scene->filename . $time;
                            File::Copy::move $thumb_file, "clipboard/$item->{name}.png"
                                or $log->warn("failed to move $thumb_file to clipboard/$item->{name}.png : $OS_ERROR");
                        }
                    }
                }

            }
            else {

                # don't let the cancelled save change the name
                $app->scene->filename($old_name);
            }
        }
        elsif ($string eq $export) {
            my $dialog = IsoExportOptions->new($self);
            if ($dialog->ShowModal == wxID_OK) {
                $self->canvas->export_scene;
            }
        }
        elsif ($string eq $import) {
            my $dialog = Wx::FileDialog->new( $self,
                'Open Image File',
                '',
                '',
                'PNG (*.png)|*.png|GIF (*.gif)|*.gif',
                wxFD_OPEN|wxFD_FILE_MUST_EXIST);

            return 0 if $dialog->ShowModal == wxID_CANCEL;
            return 0 unless my $file = ($dialog->GetPaths)[0];
            $self->import_file($file);
            my $bitmap = Wx::Bitmap->new(Wx::Image->new($file, wxBITMAP_TYPE_ANY));

            $self->canvas->cursor_multiplier_x($bitmap->GetWidth);
            $self->canvas->cursor_multiplier_y($bitmap->GetHeight);

            $self->action($AC_IMPORT);
            $self->canvas->set_cursor;
            $self->Refresh;
        }
        elsif ($string eq $scene_options) {
            my $options = [ qw(
                background_rgb
                bg_line_rgb
                tile_line_rgb
            )];
            if (my $option = Wx::GetSingleChoice( 'Scene Options', 'IsoScene', $options, $self )) {
                if (my $value = $self->change_value($option, $app->scene->$option)) {
                    $app->scene->$option($value);
                    $self->canvas->background_brush(Wx::Brush->new(Wx::Colour->new($app->scene->background_rgb), wxBRUSHSTYLE_SOLID)) if $option eq 'background_rgb';
                    $self->canvas->bg_line_pen(Wx::Pen->new(Wx::Colour->new($app->scene->bg_line_rgb), 1, wxPENSTYLE_SOLID)) if $option eq 'bg_line_rgb';
                    $self->canvas->tile_line_pen(Wx::Pen->new(Wx::Colour->new($app->scene->tile_line_rgb), 1, wxPENSTYLE_SOLID)) if $option eq 'tile_line_rgb';
                    $self->canvas->Refresh;
                }
            }
        }
        elsif ($string eq $config_options) {
            my $options = [ qw(
                previous_scene_file 
                autosave_period_seconds 
                undo_wait_milliseconds
                undo_repeat_milliseconds
                shade_change
                default_scene_file
                default_scene_scale
                default_scene_left_rgb
                default_scene_top_rgb
                default_scene_right_rgb
                default_scene_background_rgb
                default_scene_bg_line_rgb
                default_scene_tile_line_rgb
            )];
            if (my $option = Wx::GetSingleChoice( 'Config Options', 'IsoScene', $options, $self )) {
                if (my $value = $self->change_value($option, $app->config->$option)) {
                    $app->config->$option($value);
                }
            }
        }
    }
    return;
}

################################################################################
sub change_value { #{{{1
    my ($self, $label, $value) = @_;
    my $dialog;

    if ($label =~ /_rgb\z/) {
        my $data = Wx::ColourData->new;
        $data->SetChooseFull( 1 );
        $data->SetColour(Wx::Colour->new($value));
        $dialog = Wx::ColourDialog->new( $self, $data );
        if ( $dialog->ShowModal == wxID_CANCEL ) {
            return $value;
        }
        $data = $dialog->GetColourData;
        $value = $data->GetColour->GetAsString(wxC2S_HTML_SYNTAX);
    }
    else {
        $dialog = Wx::TextEntryDialog->new ( $self, "$label", "Enter a value", $value );

        $value = ( $dialog->ShowModal == wxID_OK ) 
            ? $dialog->GetValue
            : undef;
    }

    $dialog->Destroy;

    return $value;
}

################################################################################
sub change_mode { #{{{1
    my ($self, $button_mode) = @_;

    # mode buttons switch on from other modes or switch back to tile from themselves
    my $new_mode = $self->mode eq $button_mode
        ? $MO_TILE
        : $button_mode;

    for my $mode ($self->mode, $new_mode) {

        # nothing to do when switching to TILE
        next if $mode eq $MO_TILE;

        my $image_stub = $mode eq $MO_AREA
            ? "area_" . $self->current_side
            : $mode;
        IsoApp::set_button_bitmap($self->mode_btn->{$mode}, $new_mode eq $mode ? "${image_stub}_on" : "${image_stub}_off");
        $self->mode_btn->{$mode}->Refresh;
    }

    $self->mode($new_mode);

    $self->canvas->set_cursor;

    return;
}

################################################################################
# work out which side has been clicked by taking the gradient from the centre to
# this point.
sub find_cube_side { #{{{1
    my ($self) = @_;

    my $rise = -($self->cube_y - ($self->cube_height / 2));
    my $run = ($self->cube_x - ($self->cube_width / 2));

    # work out the new side based on this click
    my $side;
    if ($run) {
        my $gradient = $rise / $run;

        # removing the duplicated "run > 0 ? right : left" chunk is 
        # programmer efficient, not code efficient.
        $side = $rise < 0
            ? $run > 0
                ? $SI_RIGHT
                : $SI_LEFT
            : abs($gradient) > $self->canvas->control_gradient
                ? $SI_TOP
                : $run > 0
                    ? $SI_RIGHT
                    : $SI_LEFT;
    }
    else {

        # we've clicked exactly above or below the centre, if it's above it's top,
        # if it's below just pick one (ie right in this case).
        $side = $rise > 0
            ? $SI_TOP
            : $SI_RIGHT;
    }

    return $side;
}

################################################################################
sub cube_click { #{{{1
    my ($self) = @_;

    # side according to most recent left click
    my $side = $self->find_cube_side;

    $self->current_side($side);

    $self->Refresh;

    return;
}

################################################################################
sub change_side_colour { #{{{1
    my ($self) = @_;

    # the current colour for the side sets the default colour
    my $side = $self->find_cube_side;
    my $brush = $self->cube_brush->{ $side };

    # we want to change the color for the selected side,
    # and also make it the current painting side
    my $data = Wx::ColourData->new;
    $data->SetChooseFull( 1 );
    $data->SetColour($brush->GetColour);
    my $dialog = Wx::ColourDialog->new( $self, $data );
    if ( $dialog->ShowModal == wxID_CANCEL ) {
        return;
    }
    $data = $dialog->GetColourData;
    my $colour = $data->GetColour;
    $dialog->Destroy;

    $self->current_side($side);
    $self->action($AC_PAINT);
    $self->canvas->set_cursor;

    $brush->SetColour($colour);
    $self->update_scene_color($side);

    $self->Refresh;

    return;
}

################################################################################
sub cube_paint_event { #{{{1
    my ($self) = @_;

    $log->debug("cube_paint_event");

    my $dc = Wx::PaintDC->new( $self );
    my $frame = $self->GetParent->GetParent;

    my ($width, $height) = $dc->GetSizeWH;

    my ($w2,$h2,$h4) = ($width / 2, $height / 2, $height / 4.2);

    # fill cube sides with current colors
    my @left_points = (
        [ 0,0 ],
        [ -$w2 + 3, -$h4 ],
        [ -$w2 + 3, $h4 ],
        [ 0, $h4 * 2 ]
    );
    $dc->SetBrush($frame->cube_brush->{L});
    $dc->DrawPolygon(\@left_points, $w2, $h2);

    my @top_points = (
        [ -$w2 + 3, -$h4 ],
        [ 0, -$h4 * 2 ],
        [ $w2 - 3, -$h4 ],
        [ 0, 0 ]
    );
    $dc->SetBrush($frame->cube_brush->{T});
    $dc->DrawPolygon(\@top_points, $w2, $h2);

    my @right_points = (
        [ 0,0 ],
        [ $w2 - 3, -$h4 ],
        [ $w2 - 3, $h4 ],
        [ 0, $h4 * 2 ]
    );
    $dc->SetBrush($frame->cube_brush->{R});
    $dc->DrawPolygon(\@right_points, $w2, $h2);

    # draw action bitmap(s) as required
    my $action = $frame->action;
    unless ($action =~ /$AC_CHOOSE_BRANCH/) {

        my $app = wxTheApp;
        my $side_bitmap_name = $action =~ /erase/
            ? 'erase'
            : $action =~ /select/
                ? 'select'
                : $action eq $AC_PASTE
                    ? 'small_paste'
                    : $action;
        my $side_bitmap = $app->bitmap->{$side_bitmap_name};

        my %side_offset = (
            $SI_LEFT => [ -40, -3 ],
            $SI_TOP => [ -15, -44 ],
            $SI_RIGHT => [ 10, -3 ],
        );
    #    my @sides = $frame->action =~ /\A(?:paint|erase_current|sample|lighten|darken|shade|import|select_current)\z/
        my @sides = $action =~ /_all|paste/
                ? ($SI_LEFT, $SI_TOP, $SI_RIGHT)
                : $action =~ /_others\z/
                    ? grep { $_ ne $frame->current_side } ($SI_LEFT, $SI_TOP, $SI_RIGHT)
                    : ($frame->current_side);
        for my $side (@sides) {
            my $brush_offset = $side_offset{ $side };
            $dc->DrawBitmap($side_bitmap, $w2 + $brush_offset->[0], $h2 + $brush_offset->[1], 1);
        }
    }

    $log->debug("done cube_paint_event");

    # draw cube frame
    $dc->DrawBitmap(wxTheApp->bitmap->{cube}, 0, 0, 1);

    return;
}

################################################################################
sub current_side { #{{{1
    my ($self, $side) = @_;

    if (defined $side) {
        $self->_current_side($side);

        # switch the image on the area button
        IsoApp::set_button_bitmap($self->mode_btn->{$MO_AREA}, $self->mode eq $MO_AREA ? "area_${side}_on" : "area_${side}_off");
        $self->mode_btn->{$MO_AREA}->Refresh;

        # switch the images on the action buttons
        for my $action ($AC_PAINT, $AC_SAMPLE, $AC_LIGHTEN, $AC_DARKEN, $AC_SHADE) {
            IsoApp::set_button_bitmap($self->action_btn->{$action}, "action_${action}_$side");
            $self->action_btn->{$action}->Refresh;
        }
        my $erase_mode = $self->erase_mode;
        if ($erase_mode ne $AM_ALL) {
            IsoApp::set_button_bitmap($self->action_btn->{erase}, "action_erase_${erase_mode}_$side");
            $self->action_btn->{erase}->Refresh;
        }
        my $select_mode = $self->select_mode;
        if ($select_mode ne $AM_ALL) {
            IsoApp::set_button_bitmap($self->action_btn->{select}, "action_select_${select_mode}_$side");
            $self->action_btn->{select}->Refresh;
        }
    }

    return $self->_current_side;
}

################################################################################
# We have changed a cube brush's colour; update the scene with the colour so it's
# saved with the scene.
sub update_scene_color { #{{{1
    my ($self, $side) = @_;

    my $side_color_property = {
        $SI_LEFT => 'left_rgb',
        $SI_TOP => 'top_rgb',
        $SI_RIGHT => 'right_rgb',
    };

    my $property = $side_color_property->{$side};

    wxTheApp->scene->$property( $self->cube_brush->{$side}->GetColour->GetAsString(wxC2S_HTML_SYNTAX) );

    return;
}

################################################################################
# toggle the appearance of the tool panel
sub toggle_tool_panel { #{{{1
    my ($self) = @_;

    my $hide = $self->tool_panel->IsShown;

    my ($width, $height) = $self->tool_panel->GetSizeWH;
    $log->info("tp width $width");

    # scroll the canvas so the scene remains in the same screen position after 
    # the canvas widens/narrows depending on the state of the tool panel.
    my $origin_x = $self->canvas->scene->origin_x;
    $self->canvas->scene->origin_x($origin_x + ($hide ? 1 : -1) * $width);
    $self->canvas->tile_cache(0);

    $self->tool_panel->Show(! $hide);

    return;
}

################################################################################
sub save_to_file { #{{{1
    my ($self) = @_;

    my $scene = wxTheApp->scene;

    # save to previous file saved or loaded w/o prompting.
    unless ($scene->filename) {
        my $dialog = Wx::FileDialog->new( $self,
            'Save Scene to file',
            '',
            '',
            'IsoScene files (*.isc)|*.isc',
            wxFD_SAVE);

        return 0 if $dialog->ShowModal == wxID_CANCEL;
        return 0 unless my $file = ($dialog->GetPaths)[0];
        $file = File::Basename::basename($file, '.isc');
        $scene->filename($file);
    }

    my $busy = new Wx::BusyCursor;

    $scene->save;

    $log->info("File written ok: " . $scene->filename . "\n");

    return 1;
}

################################################################################
sub open_from_file { #{{{1
    my ($self) = @_;

    my $dialog = Wx::FileDialog->new( $self,
        'Open Scene from file',
        '',
        '',
        'IsoScene files (*.isc)|*.isc',
        wxFD_OPEN);

    return 0 if $dialog->ShowModal == wxID_CANCEL;
    return 0 unless my $file = ($dialog->GetPaths)[0];

    my $busy = new Wx::BusyCursor;

    # save the existing scene
    wxTheApp->scene->save;

    $log->info("load scene");
    wxTheApp->scene( IsoScene->new({ file => $file }) );
    wxTheApp->set_frame_title;
    $log->info("set canvas scene");
    $self->canvas->scene(wxTheApp->scene);

    $self->Refresh;

    return 1;
}

################################################################################
sub start_undo_or_redo { #{{{1
    my ($self, $flag) = @_;
    $log->debug("start_undo_or_redo $flag");

    # start a one-shot timer with the initial period; if this goes off, it will restart
    # on continuous with the quick period.
    $self->undo_timer->Start(wxTheApp->config->undo_wait_milliseconds, wxTIMER_ONE_SHOT);
    $self->undo_or_redo_flag($flag);
    $self->canvas->undo_or_redo();

    return;
}

################################################################################
sub slow_button_down { #{{{1
    my ($self, $event, $operation) = @_;
    $log->debug("slow_button_down: @_");

    $self->slow_button_name($operation);

    if ($operation =~ /undo|redo/) {
        $self->start_undo_or_redo($operation eq 'redo');
    }
    else {

        $self->slow_button_timer->Start(500, wxTIMER_ONE_SHOT);

        # record the button down position on the cube
        if ($operation eq 'cube') {
            my $dc = Wx::ClientDC->new( $event->GetEventObject );
            $self->cube_x($event->GetLogicalPosition($dc)->x);
            $self->cube_y($event->GetLogicalPosition($dc)->y);
        }

    }

    $event->Skip;

    return;
}

################################################################################
sub slow_button_up { #{{{1
    my ($self, $event, $operation) = @_;
    $log->debug("slow_button_up: @_");

    # is the timer still running?
    if (my $name = $self->slow_button_name) {

        $log->debug("slow_button_name $name");

        if ($name =~ /undo|redo/) {
            $self->undo_timer->Stop;
        }
        else {

            # clear the button name so the timer does nothing
            $self->slow_button_name(0);

            my $erase_action = {
                $AM_CURRENT => $AC_ERASE,
                $AM_OTHERS => $AC_ERASE_OTHERS,
                $AM_ALL => $AC_ERASE_ALL,
            };

            my $select_action = {
                $AM_CURRENT => $AC_SELECT,
                $AM_OTHERS => $AC_SELECT_OTHERS,
                $AM_ALL => $AC_SELECT_ALL,
            };

            # what does the button do if it's released before the timer goes off?
            my $fast_button_callback = {
                paste => [ \&IsoCanvas::clipboard_operation, $self->canvas, $operation ],
                erase => [ \&change_action, $self, $erase_action->{ $self->erase_mode } ],
                select => [ \&change_action, $self, $select_action->{ $self->select_mode } ],
                cube => [ \&cube_click, $self, ],
            };

            if (my $callback = $fast_button_callback->{$operation}) {

                my $function = shift @{ $callback };
                $function->(@{ $callback });
            }
            else {
                $log->warn("unknown fast button operation $operation");
            }
        }

    }

    $event->Skip;

    return;
}

################################################################################
sub slow_button_timer_expired { #{{{1
    my ($self, $event) = @_;
    $log->debug("slow_button_timer_expired: @_");

    # is the button still down?
    if (my $name = $self->slow_button_name) {

        # clear the button name so the mouse up event does nothing
        $self->slow_button_name(0);

        $log->debug("do slow button action for $name");

        if ($name eq 'paste') {
            $self->show_paste_list;
        }
        elsif ($name =~ /erase|select/) {
            $self->set_action_mode($name);
        }
        elsif ($name eq 'cube') {
            $self->change_side_colour;
        }
        else {
            $log->info("slow_button_timer_expired for $name, nothing to do");
        }
        
    }

    return;
}

################################################################################
sub show_button_popup { #{{{1
    my ($self, $button_image_names, $callback, $position) = @_;

    my $popup = Wx::PlPopupTransientWindow->new($self);

    $popup->SetSizer(my $sizer = Wx::BoxSizer->new(wxHORIZONTAL));

    my $current_side = $self->current_side;
    my $bitmap = wxTheApp->bitmap;

    # check the size of the first bitmap to find the popup size (assume square images).
    # We add 12 for the borders around the image and the button.
    my $size = $bitmap->{ $button_image_names->[0] }->GetHeight + 12;

    for my $image_name (@{ $button_image_names }) {
        my $button_bitmap = $bitmap->{$image_name};
        my $button = Wx::BitmapButton->new($popup, -1, $button_bitmap);
        $button->SetToolTip( join(' ', map { ucfirst $_ } split(/_/, $image_name)) );
        $button->SetName($image_name);
        $sizer->Add($button);

        Wx::Event::EVT_BUTTON($popup, $button, $callback);
    }

    my $width = $size * scalar @{ $button_image_names };

    $popup->SetSize($width, $size);
    $sizer->Layout;

    # show popup to the side of the invoking button; this looks better (i think) and avoids a bug
    # when a button appears directly under the pointer and is then ignored.
    $popup->Move($position->x + $size, $position->y);
    $popup->Popup;

    return;
}

################################################################################
sub do_undo_redo_tool { #{{{1
    my ($popup, $event) = @_;

    my $frame = $popup->GetParent;

    $popup->Hide;
    $popup->Close;

    my $button_name = $event->GetEventObject->GetName;
    $log->info("do_undo_redo_tool: $button_name");

    if ($button_name eq 'choose_branch') {

        my $branch_chc = $frame->branch_choice_pnl->FindWindow('branch_chc');
        $branch_chc->Clear;

        # grab the top item on the redo stack; it should be a branch node
        my $branch_node = $frame->canvas->scene->redo_stack->[-1];
        $log->logdie("top redo item not a branch node: " . Dumper($branch_node))
            unless (ref $branch_node) eq 'HASH';

        # we have a list of branches in $branch_node->{branches}
        $log->debug("branches " . Dumper($branch_node->{branches}));
        for my $branch ( @{ $branch_node->{branches} } ) {

            # we want to render all tiles in this branch 
            $branch_chc->Append( (ref $branch) eq 'HASH' 
                ? 'Last current: ' . IsoApp::datetime_description($branch->{last_current_at})
                : 'Current branch');
        }

        # remember the original branch in case we cancel, and the current action so we can 
        # restore it after the change.
        $frame->original_branch($branch_node->{current_branch});
        $frame->previous_action($frame->action);

        # the size of the undo stack is our reference point; when we change branches, we must undo to this
        # point before redoing the new branch
        $frame->branch_choice_position( $#{ $frame->canvas->scene->undo_stack } );

        $branch_chc->SetSelection($branch_node->{current_branch});

        $frame->change_action($AC_CHOOSE_BRANCH);
        $frame->change_mode($MO_MOVE) unless $frame->mode eq $MO_MOVE;
        $frame->branch_choice_pnl->Show;
        $frame->toggle_tool_panel;
        $frame->Layout;

        # don't want the scene saving while we're changing branch.
        wxTheApp->autosave_timer->Stop;

    }
    elsif ($button_name eq 'undo_to_branch') {
    }
    elsif ($button_name eq 'redo_to_branch') {
    }
    elsif ($button_name eq 'new_branch') {
    }

    return;
}

################################################################################
sub change_branch_choice { #{{{1
    my ($self, $event) = @_;

    $log->info("change_branch_choice " . $event->GetSelection);
    $self->canvas->change_to_branch($event->GetSelection, 1);

    return;
}

################################################################################
sub finish_branch_choice { #{{{1
    my ($self, $confirm) = @_;

    $log->info("finish_branch_choice");

    # if we're confirming, everything is ok already. Otherwise,
    # change back to original branch but don't redo, which will leave the undo/redo
    # stacks where they were.
    unless ($confirm) {
        $self->canvas->change_to_branch($self->original_branch, 0);
    }

    $self->change_action($self->previous_action);
    $self->branch_choice_pnl->Hide;
    $self->toggle_tool_panel;
    $self->canvas->set_undo_redo_button_states;
    $self->Layout;

    wxTheApp->autosave_timer->Start(wxTheApp->config->autosave_period_seconds * 1000, wxTIMER_CONTINUOUS);

    return;
}

################################################################################
sub do_selection_tool { #{{{1
    my ($popup, $event) = @_;

    my $frame = $popup->GetParent;

    $popup->Hide;
    $popup->Close;

    my $button_name = $event->GetEventObject->GetName;
    $log->debug("do_selection_tool: $button_name");

    if ($button_name eq 'select_none') {
        $frame->canvas->set_selection_for_all(0);
    }
    elsif ($button_name eq 'select_all') {
        $frame->canvas->set_selection_for_all(1);
    }
    elsif ($button_name eq 'select_visible') {
        $frame->canvas->select_visible;
    }

    return;
}

################################################################################
sub set_action_mode { #{{{1
    my ($self, $action) = @_;

    $log->debug("create popup");
    my $popup = Wx::PlPopupTransientWindow->new($self);

    $popup->SetSizer(my $sizer = Wx::BoxSizer->new(wxHORIZONTAL));

    my $current_side = $self->current_side;
    my $bitmap = wxTheApp->bitmap;

    for my $action_mode ($AM_CURRENT, $AM_OTHERS, $AM_ALL) {
        my $action_name = "${action}_${action_mode}";
        my $bitmap_name = "action_$action_name";
        $bitmap_name .= "_$current_side" unless $action_mode eq $AM_ALL;
        $log->debug("action_mode $action_mode, action_name $action_name, bitmap_name $bitmap_name");
        my $button = Wx::BitmapButton->new($popup, -1, $bitmap->{$bitmap_name});
        $sizer->Add($button);

        Wx::Event::EVT_BUTTON($popup, $button, sub { 
            my ($popup, $event) = @_;

            $log->debug("set_action_mode button: @_ $action_mode");

            $popup->Hide;
            $popup->Close;

            if ($action eq 'erase') {
                $self->erase_mode($action_mode);
            }
            elsif ($action eq 'select') {
                $self->select_mode($action_mode);
            }

            IsoApp::set_button_bitmap($self->action_btn->{$action}, $bitmap_name );
            $self->change_action($action_name);
            $self->Refresh;

            return;
        });
    }

    $popup->SetSize(156, 52);
    $sizer->Layout;
    my $button = $self->action_btn->{$action};
    my $position = $button->GetScreenPosition();
    $popup->Move($position->x + 52, $position->y);
    $popup->Popup;

    return;
}

################################################################################
sub show_paste_list { #{{{1
    my ($self) = @_;

        # display the popup window
        $log->debug("create paste list popup");
        my $popup = Wx::PlPopupTransientWindow->new($self);

        $popup->SetSizer(my $sizer = Wx::BoxSizer->new(wxHORIZONTAL));

        # the image list holds all the potential items in the list, it doesn't actually
        # insert any items.
        my $images= Wx::ImageList->new( 64, 64, 1 );

        # initialise with -1 for the paste-selector dialog
        my @clipboard_indexes = (-1);
        $images->Add( Wx::Bitmap->new("images/paste_selector.png", wxBITMAP_TYPE_ANY));

        for my $i (0 .. $#{ $self->canvas->scene->clipboard }) {
            my $item = $self->canvas->scene->clipboard->[$i];
            next unless $item->{name};
            my $thumb_file = "clipboard/$item->{name}.png";
            if (-f $thumb_file) {
                $images->Add( Wx::Bitmap->new($thumb_file, wxBITMAP_TYPE_ANY));
                push @clipboard_indexes, $i;
            }
        }

        my $list = Wx::ListCtrl->new( $popup, -1, wxDefaultPosition, [ $self->GetSize->GetWidth - 10, -1 ],  wxLC_ICON | 0 );
        Wx::Event::EVT_LIST_ITEM_ACTIVATED($popup, $list, sub {
            my ($parent, $event) = @_;
            $log->debug(sprintf "activated list pos %d, clipboard index %d ", $event->GetIndex, $event->GetItem->GetData);

            $popup->Hide;
            $popup->Close;

            if ($event->GetIndex) {
                $self->canvas->clipboard_operation('paste', $event->GetItem->GetData);
            }
            else {
                $log->debug("load paste selector");
                my $dialog = IsoPasteSelector->new($self);
                # my $dialog = wxTheApp->xrc->LoadDialog($self, 'paste_selector');
                $log->debug("show paste selector");
                if ($dialog->ShowModal == wxID_OK) {
                    $log->debug("chose a file");

                    # we've added the clipboard item to the canvas in the dialog,
                    # so all we need do here is trigger a standard paste op.
                    $self->canvas->clipboard_operation('paste');
                }
                $log->debug("shown paste selector");
                $dialog->Destroy;
            }

            $event->Skip;

            $log->debug("done in EVT_LIST_ITEM_ACTIVATED");

            return;
        });

        $list->AssignImageList($images, wxIMAGE_LIST_NORMAL);

        # add all the images to the display
        for my $i (0 .. $images->GetImageCount - 1) {
            $list->InsertImageItem($i, $i);
            $list->SetItemData($i, $clipboard_indexes[$i]);
        }

        $sizer->Add($list, 1, wxEXPAND, 3 );
        $popup->SetSize(min($self->GetSize->GetWidth, $images->GetImageCount * 85),100);

        my $button = $self->clipboard_btn->{paste};
        my $position = $button->GetScreenPosition();
        $popup->Move($position->x + 52, $position->y);

        $popup->Popup;
}

1;

# todo...
