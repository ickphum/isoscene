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
use IsoGlCanvas;
use IsoPasteSelector;
#use IsoExportFrame;

# attributes {{{1

__PACKAGE__->mk_accessors( qw(
    tool_panel canvas branch_choice_pnl message_pnl export_options_pnl

    area_mode pan_mode action previous_action previous_paint_action mode_btn action_btn erase_mode select_mode clipboard_btn misc_btn

    cube_x cube_y cube_width cube_height cube_brush
    _current_side
    
    import_file

    undo_timer undo_or_redo_flag branch_choice_position original_branch

    slow_button_timer slow_button_name

    popup_name
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
    $MO_MOVE,
    ) = qw(tile area move);

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
    $AC_SHADE_CUBE,
    $AC_IMPORT,
    $AC_PASTE,
    $AC_SELECT,
    $AC_SELECT_OTHERS,
    $AC_SELECT_ALL,
    $AC_CHOOSE_BRANCH,
    $AC_VIEW,
    ) = qw(paint sample erase_current erase_others erase_all lighten darken shade shade_cube import paste select_current select_others select_all choose_branch view);

my @MENU_ITEMS = my (
    $MI_NEW,
    $MI_OPEN,
    $MI_SAVE_AS,
    $MI_EXPORT,
    $MI_IMPORT,
    $MI_SCENE_OPTIONS,
    $MI_CONFIG_OPTIONS,
    $MI_SAVE,
    ) = ( "New", "Open", "Save As...", "Export", "Import", "Scene Options", "Config Options", "Save", );

################################################################################
# Constructor. Creates a Wx::Frame object, adds a sizer and a status bar and
# sets the window size.
sub new { #{{{1
    my ($class, $scene, $parent, $title, $pos, $size) = @_;

    $title  ||= "IsoScene"                      unless defined $title;
    $pos    = $scene->position || [ 1300, 300]   unless defined $pos;
    $size   = $scene->size     || [ 700, 700]   unless defined $size;

    my $self = $class->SUPER::new( $parent, -1, $title, $pos, $size);

    my $app = wxTheApp;

    # undo timer
    $self->undo_timer(Wx::Timer->new($self));
    Wx::Event::EVT_TIMER($self, $self->undo_timer->GetId, sub {
        if ($self->canvas->undo_or_redo()) {

            # may be more events to undo/redo; check the timer.
            # the timer is set to a single slow wait period, then switches to fast continuous
            unless ($self->undo_timer->IsRunning) {
                $self->undo_timer->Start(wxTheApp->config->undo_repeat_milliseconds, wxTIMER_CONTINUOUS);
            }
        }
        elsif ($self->undo_timer->IsRunning) {

            # no more events in the relevant queue so stop the timer
            $log->info("finished undo_or_redo");
            $self->undo_timer->Stop;
        }

    });

    # track size and position continually so we don't need to do it on save
    Wx::Event::EVT_SIZE($self, sub { 
        my ($self, $event) = @_;
        
        if (my $scene = $self->canvas->scene) {
            $scene->size([ $self->GetSizeWH ]);
        }

        $event->Skip;
        return;
    });
    Wx::Event::EVT_MOVE($self, sub { 
        my ($self, $event) = @_;

        if (my $scene = $self->canvas->scene) {
            $scene->position([ $event->GetPosition->x, $event->GetPosition->y ]);
        }

        $event->Skip;
        return;
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
    Wx::Event::EVT_MENU( $self, wxID_EXIT, sub { $log->debug("quit"); $self->Close; } );

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
    for my $action ( $AC_PAINT, $AC_SAMPLE, 'erase', 'select', $AC_LIGHTEN, $AC_DARKEN, $AC_SHADE, $AC_SHADE_CUBE) {

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

    $self->mode_btn({});

    my $move_btn = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{move_off});
    $move_btn->SetToolTip("Pan");
    $self->mode_btn->{$MO_MOVE} = $move_btn;
    Wx::Event::EVT_BUTTON($self, $move_btn, sub { $_[0]->change_mode($MO_MOVE); });
    push @column_buttons, $move_btn;

    my $area_btn = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{area_off});
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

    my $button = $self->misc_btn->{selection_tools} = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{selection_tools} );
    $button->SetToolTip("Selection Tools");
    Wx::Event::EVT_BUTTON($self, $button, sub {
        $self->show_button_popup(
            [ qw(select_all select_none select_visible), "select_by_color_" . $self->canvas->select_by_color ],
            \&do_selection_tool, $button->GetScreenPosition);
    });
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
    $tool_sizer->Add($column_button_szr, 1, wxEXPAND);

    my $undo_redo_button_szr = Wx::BoxSizer->new(wxHORIZONTAL);
    for my $operation (qw(undo undo_redo_menu redo)) {
        my $button = $self->misc_btn->{$operation} = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{$operation}); 
        if ($operation eq 'undo_redo_menu') {
            $button->SetToolTip('Undo/Redo Tools');
            Wx::Event::EVT_BUTTON($self, $button, sub {
                my ($self, $event) = @_;

                $log->info("undo_redo_menu @_");
                my $choices = [ qw(undo_to_branch undo_many redo_many redo_to_branch new_branch clear_undo_info) ];

                # if we're on a branch, display the choose_branch button
                if (IsoScene::action_is_branch($self->canvas->scene->redo_stack->[-1])) {
                    push @{ $choices }, 'choose_branch';
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

    my $menu_btn = $self->misc_btn->{menu} = Wx::BitmapButton->new($tool_panel, -1, $bitmap->{menu});
    $menu_btn->SetToolTip("Menu");
    $tool_sizer->Add($menu_btn, 0, wxEXPAND);
    Wx::Event::EVT_BUTTON($self, $menu_btn, sub {
        my ($self) = @_;
        my $items = [ [ 'Recent', reverse @{ $app->config->previous_scene_files }  ], @MENU_ITEMS, ]; 

        # replace 'Scene Options' item with submenu
        if ((my $index = List::MoreUtils::firstidx { $_ eq 'Scene Options' } @{ $items }) >= 0) {
            splice @{ $items }, $index, 1, [ 'Scene Options', qw(background_rgb bg_line_rgb tile_line_rgb tile_border) ];
        }
        else {
            $log->logdie("can't find Scene Options in menu: " . Dumper($items));
        }

        # ditto for Config Options
        if ((my $index = List::MoreUtils::firstidx { $_ eq 'Config Options' } @{ $items }) >= 0) {

            # define the order in which we want to display these options
            my @config_options = qw(
                autosave_on_exit
                autosave_period_seconds 
                autosave_idle_seconds 
                use_binary_files
                use_compressed_files
                undo_wait_milliseconds
                undo_repeat_milliseconds
                undo_many_count
                undo_includes_view
                repeated_pasting
                repaint_same_tile
                automatic_branching
                script_delay_milliseconds
                shade_change
                darken_lighten_change
                display_palette_index
                display_color
                display_key
                default_scene_file
                default_scene_scale
                default_scene_left_rgb
                default_scene_top_rgb
                default_scene_right_rgb
                default_scene_background_rgb
                default_scene_bg_line_rgb
                default_scene_tile_line_rgb
            );

            # append the current value to the boolean options; these will become
            # toggle items.
            for my $option (qw(autosave_on_exit use_binary_files use_compressed_files undo_includes_view repeated_pasting repaint_same_tile automatic_branching display_palette_index display_color display_key)) {
                my $index = List::MoreUtils::firstidx { $_ eq $option } @config_options;
                splice @config_options, $index, 1, $option . "?" . $app->config->$option;
            }

            splice @{ $items }, $index, 1, [ 'Config Options', @config_options ];
        }
        else {
            $log->logdie("can't find Config Options in menu: " . Dumper($items));
        }

        $self->show_text_popup($items, \&do_menu_choice, $self->misc_btn->{menu});
    });

    $log->debug("mode buttons");

    $sizer->Add($tool_panel);

    $self->action($AC_PAINT);
    $self->previous_action($AC_PAINT);
    $self->previous_paint_action($AC_PAINT);
    $self->area_mode(0);
    $self->pan_mode(0);
    $self->erase_mode($AM_CURRENT);
    $self->select_mode($AM_CURRENT);
    $self->current_side($SI_RIGHT);

    my $canvas_side_sizer = Wx::BoxSizer->new(wxVERTICAL);

    $self->canvas(IsoGlCanvas->new($self, $scene));

    # fix redo button in case we saved on a branch point
#    $self->canvas->set_undo_redo_button_states;

    $canvas_side_sizer->Add($self->canvas, 1, wxEXPAND );

    # branch choice panel
    $self->branch_choice_pnl( $app->xrc->LoadPanel($self, 'choose_branch') );
    $canvas_side_sizer->Add($self->branch_choice_pnl, 0, wxEXPAND );
    $self->branch_choice_pnl->Hide;

    Wx::Event::EVT_CHOICE($self, $self->branch_choice_pnl->FindWindow('branch_chc'), \&change_branch_choice);
    Wx::Event::EVT_BUTTON($self, $self->branch_choice_pnl->FindWindow('ok_btn'), sub { $_[0]->finish_branch_choice(1); } );
    Wx::Event::EVT_BUTTON($self, $self->branch_choice_pnl->FindWindow('cancel_choice_btn'), sub { $_[0]->finish_branch_choice(0); } );

    # export options panel
    $self->export_options_pnl( $app->xrc->LoadPanel($self, 'export_options_pnl') );
    $canvas_side_sizer->Add($self->export_options_pnl, 0, wxEXPAND );
    $self->export_options_pnl->Hide;

    Wx::Event::EVT_BUTTON($self, $self->export_options_pnl->FindWindow('export_btn'), \&finish_export_panel);
    Wx::Event::EVT_BUTTON($self, $self->export_options_pnl->FindWindow('cancel_btn'), \&finish_export_panel);
    Wx::Event::EVT_SLIDER($self, $self->export_options_pnl->FindWindow('pixels_per_tile_sld'), sub {
        my ($frame, $event) = @_;
        $log->info("slider @_");

        $event->Skip;
        return;
    });

    # message panel
    $self->message_pnl( $app->xrc->LoadPanel($self, 'message_pane') );
    $canvas_side_sizer->Add($self->message_pnl, 0, wxEXPAND );
    $self->message_pnl->Hide;

    Wx::Event::EVT_BUTTON($self, $self->message_pnl->FindWindow('continue_btn'), 
        sub {
            my ($frame) = @_;
            
            $frame->message_pnl->Hide;
            $frame->Layout;
            my $app = wxTheApp;
            if (my $script_timer = $app->script_timer) {
                $script_timer->Start($app->script_delay, wxTIMER_CONTINUOUS);
            }
        }
    );

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

    Wx::Event::EVT_CLOSE($self, sub {
        my ($self, $event) = @_;

        my $app = wxTheApp;

        $app->autosave_timer->Stop;

        # do the closing save before the windows close
        if ($app->config->autosave_on_exit) {
            my $busy = new Wx::BusyCursor;
            $app->scene->save;
            $app->config->save;
        }

        $event->Skip;
    });

    # $self->Maximize(1);

    return $self;
}

################################################################################
sub change_action { #{{{1
    my ($self, $action) = @_;

    $log->debug("change_action $action");
    $self->previous_action($self->action);
    $self->previous_paint_action($self->action) if $self->action eq $AC_PAINT || $self->action eq $AC_SHADE_CUBE;

    # changing to another action during paste cancels the paste
    if ($self->action eq $AC_PASTE) {
        $self->canvas->paste_list(undef);
        $self->canvas->paint_shape(0);

        # clear last set of floating tiles
        $self->canvas->transient_grid({});
        $self->canvas->rebuild_render_group('transient');
    }

    # changing to another action during import cancels the import
    elsif ($self->action eq $AC_IMPORT) {
        $self->canvas->cursor_multiplier_x(1);
        $self->canvas->cursor_multiplier_y(1);
    }

    elsif ($action eq $AC_SHADE) {

        # change the cube colors of the unselected sides to be shades of the current side
        my @other_shades = $self->find_shades($self->current_side);

        # set the shades as the cube colors for the other sides
        for my $other_side ( @{ $OTHER_SIDES{ $self->current_side } } ) {
            my $new_rgb = shift @other_shades;
            $self->cube_brush->{ $other_side }->SetColour(Wx::Colour->new(@{ $new_rgb }));
            $self->update_scene_color($other_side);
        }

        # revert to paint
        $action = $self->previous_paint_action;
    }
    elsif ($action eq $AC_SHADE_CUBE) {

        # fill in the missing pieces of the cube of which the current tile is part.
    }
    elsif ($action =~ /\A(?:$AC_LIGHTEN|$AC_DARKEN)\z/) {

        # change the cube color of the selected side
        my $side = $self->current_side;
        my $brush = $self->cube_brush->{ $side };
        my $colour = $brush->GetColour;
        my @rgb = ($colour->Red, $colour->Green, $colour->Blue);
        my $change = wxTheApp->config->darken_lighten_change;
        $change = - $change if $action eq $AC_DARKEN;
        my @new_rgb = map { List::Util::max(List::Util::min($_ + $change, 255), 0) } @rgb;
        $self->cube_brush->{ $side }->SetColour(Wx::Colour->new(@new_rgb));
        $self->update_scene_color($side);

        # revert to paint
        $action = $self->previous_paint_action;
    }

    # changing action during move mode cancels the move; we've probably forgotten move is on
    # and are about to use the new action.
    # this looks weird but is due to the mode buttons being toggles;
    # we're already in move mode, so doing this again turns it off.
    $self->change_mode($MO_MOVE) if $self->pan_mode;

    $self->action($action);
    $self->canvas->set_cursor;
    $self->Refresh;

    $log->debug("done change_action $action");

    return;
}

################################################################################
sub do_menu_choice { #{{{1
    my ($self, $event) = @_;

    my $string = $self->popup_name->{ $event->GetId }
        or $log->logdie("no name for popup id " . $event->GetId . ", popup_name = " . Dumper($self->popup_name));

    my $app = wxTheApp;

    if ($string eq $MI_SAVE) {
        $self->save_to_file;
    }
    elsif ($string eq $MI_OPEN) {
        $self->open_from_file;
    }
    elsif ($string eq $MI_NEW) {
        $app->scene->save;
        $app->scene( IsoScene->new() );
        $self->canvas->scene($app->scene);
        $self->canvas->calculate_grid_points;
        $app->set_frame_title;
        $self->Refresh;
    }
    elsif ($string eq $MI_SAVE_AS) {

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
    elsif ($string eq $MI_EXPORT) {

        # display the export panel. Normal drawing operations can continue while this panel is displayed.
        wxTheApp->load_panel_settings($self->export_options_pnl);
        $self->export_options_pnl->Show;
        my @size = $self->export_options_pnl->GetSizeWH;
        $app->scene->origin_y($app->scene->origin_y - $size[1] * $app->scene->scale);
        $self->Layout;
    }
    elsif ($string eq $MI_IMPORT) {
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
    elsif ($string =~ /$MI_SCENE_OPTIONS:(.*)/) {
        my $option = $1;
        if (my $value = $self->change_value($option, $app->scene->$option)) {
            $app->scene->$option($value);

            if ($option eq 'background_rgb') {
                my $color = Wx::Colour->new($app->scene->background_rgb);

                # glClearColor demands floats, whereas glColor (which uses bg_line_color) can accept bytes
                $self->canvas->background_color([ $color->Red / 255, $color->Green / 255, $color->Blue / 255 ] );
            }
            if ($option eq 'bg_line_rgb') {
                my $color = Wx::Colour->new($app->scene->bg_line_rgb);
                $self->canvas->bg_line_color([ $color->Red, $color->Green, $color->Blue ] );
            }
            # $self->canvas->tile_line_pen(Wx::Pen->new(Wx::Colour->new($app->scene->tile_line_rgb), 1, wxPENSTYLE_SOLID)) if $option eq 'tile_line_rgb';
            $self->canvas->Refresh;
        }
    }
    elsif ($string =~ /$MI_CONFIG_OPTIONS:(.*)/) {
        my $option = $1;
        
        # boolean options are toggle items, so selecting one just flips the state
        if ($option =~ /autosave_on_exit|use_binary_files|use_compressed_files|undo_includes_view|repeated_pasting|repaint_same_tile|automatic_branching|display_palette_index|display_color|display_key/) {
            $app->config->$option($app->config->$option ? 0 : 1);
            $log->info("config setting for $option is now " . $app->config->$option);
        }
        elsif (defined(my $value = $self->change_value($option, $app->config->$option))) {
            $app->config->$option($value);
        }
        if ($option =~ /display/) {
            $self->canvas->force_rebuild_everywhere;
        }
        $self->canvas->Refresh;
    }
    elsif ($string =~ /Recent:(.*)/) {
        $self->open_from_file($1);
    }
    else {
        $log->info("unknown menu name: $string");
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

    # This has been changed from the mutex modes tile/pan/area; tile is now assumed 
    # and the others have individual properties, the code knows which takes precedence,
    # eg pan > area.
    # There's probably an easier way to do this but I'm changing as little as possible.
    #
    # The only modes passed in are pan and area, which simply toggle their properties
    # and change the button imagery as before.
    my $flag;
    if ($button_mode eq $MO_AREA) {
        $self->area_mode($flag = ($self->area_mode ? 0 : 1));
    }
    elsif ($button_mode eq $MO_MOVE) {
        $self->pan_mode($flag = ($self->pan_mode ? 0 : 1));
    }

#    my $image_stub = $button_mode eq $MO_AREA
#        ? "area_" . $self->current_side
#        : $button_mode;
#    $log->debug("image_stub $image_stub");
    IsoApp::set_button_bitmap($self->mode_btn->{$button_mode}, $flag ? "${button_mode}_on" : "${button_mode}_off");
    $self->mode_btn->{$button_mode}->Refresh;

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
# change the other sides of the cube to be shades of the specified side. A different
# base colour can be specified (as a brush index), otherwise the current side color will be used.
sub find_shades { #{{{1
    my ($self, $side, $brush_index) = @_;

    my $colour;
    if (defined $brush_index) {
        my @rgb = @{ $self->canvas->palette->[$brush_index] };
        $colour = Wx::Colour->new(sprintf("#%02X%02X%02X", @rgb ));
    }
    else {
        $colour = $self->cube_brush->{ $side }->GetColour;
    }
    my @rgb = ($colour->Red, $colour->Green, $colour->Blue);
    my $change = wxTheApp->config->shade_change;
    my %relative_shade = split ',', wxTheApp->config->relative_shades;
    my @shades = ();
    for my $other_side ( @{ $OTHER_SIDES{ $side } } ) {
        my $side_change = ($relative_shade{$side} - $relative_shade{$other_side}) * $change;
        push @shades, [ map { List::Util::max(List::Util::min($_ + $side_change, 255), 0) } @rgb ];
    }

    return @shades;
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
    $self->action($self->previous_paint_action);
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
#        IsoApp::set_button_bitmap($self->mode_btn->{$MO_AREA}, $self->area_mode ? "area_${side}_on" : "area_${side}_off");
#        $self->mode_btn->{$MO_AREA}->Refresh;

        # switch the images on the action buttons
        for my $action ($AC_PAINT, $AC_SAMPLE, $AC_LIGHTEN, $AC_DARKEN, $AC_SHADE, $AC_SHADE_CUBE) {
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
# display a message, possibly temporarily
sub display_message { #{{{1
    my ($self, $message) = @_;

    my $message_lbl = $self->message_pnl->FindWindow('message_lbl');
    $message_lbl->SetLabel($message);
    $self->message_pnl->Show;
    $self->Layout;

    # stop the script if it's running; hiding the message will restart it
    if (my $timer = wxTheApp->script_timer) {
        $timer->Stop;
    }

    return 1;
}

################################################################################
# toggle the appearance of the tool panel
sub toggle_tool_panel { #{{{1
    my ($self) = @_;

    my $hide = $self->tool_panel->IsShown;

    my ($width, $height) = $self->tool_panel->GetSizeWH;

    # scroll the canvas so the scene remains in the same screen position after 
    # the canvas widens/narrows depending on the state of the tool panel.
    my $origin_x = $self->canvas->scene->origin_x;
    $self->canvas->scene->origin_x($origin_x + ($hide ? 1 : -1) * $width);

    $self->tool_panel->Show(! $hide);
    $self->Layout;

    return;
}

################################################################################
sub save_to_file { #{{{1
    my ($self) = @_;

    my $scene = wxTheApp->scene;
    my $config = wxTheApp->config;

    # save to previous file saved or loaded w/o prompting.
    unless ($scene->filename) {
        my $dialog = Wx::FileDialog->new( $self,
            'Save Scene to file',
            $config->{scene_dir},
            '',
            'IsoScene files (*.isb;*.isz;*.isc)|*.isb;*.isz;*.isc',
            wxFD_SAVE);

        return 0 if $dialog->ShowModal == wxID_CANCEL;
        return 0 unless my $file = ($dialog->GetPaths)[0];
        $file = File::Basename::basename($file, $config->use_binary_files ? '.isb' : '.isz');
        $scene->filename($file);
    }

    $scene->save;

    $log->info("File written ok: " . $scene->filename . "\n");

    return 1;
}

################################################################################
sub open_from_file { #{{{1
    my ($self, $file) = @_;

    unless ($file) {
        my $config = wxTheApp->config;
        my $dialog = Wx::FileDialog->new( $self,
            'Open Scene from file',
            $config->{scene_dir},
            '',
            'IsoScene files (*.isb;*.isz;*.isc)|*.isb;*.isz;*.isc',
            wxFD_OPEN);

        return 0 if $dialog->ShowModal == wxID_CANCEL;
        return 0 unless $file = ($dialog->GetPaths)[0];
    }

    my $busy = new Wx::BusyCursor;

    # save the existing scene
    wxTheApp->scene->save;

    wxTheApp->scene( my $scene = IsoScene->new({ file => $file }) );
    wxTheApp->set_frame_title;
    $self->canvas->scene(wxTheApp->scene);
    $self->Move(@{ $scene->position }) if $scene->position;
    if (my $size = $scene->size) {
        $log->debug("set size to " . Dumper($size));
        $self->SetSize( $size->[0], $size->[1] );
    }

    $self->Refresh;

    return 1;
}

################################################################################
sub finish_export_panel { #{{{1
    my ($self, $event) = @_;

    my $button = $event->GetEventObject;

    my $app = wxTheApp;
    if ($button->GetName eq 'export_btn') {
        $app->save_panel_settings($self->export_options_pnl);
        $self->canvas->export_scene;
    }

    my @size = $self->export_options_pnl->GetSizeWH;
    $self->export_options_pnl->Hide;
    $app->scene->origin_y($app->scene->origin_y + $size[1] * $app->scene->scale);
    $self->Layout;

    return;
}

################################################################################
sub start_undo_or_redo { #{{{1
    my ($self, $flag) = @_;
    $log->debug("start_undo_or_redo $flag");

    # start a one-shot timer with the initial period; if this goes off, it will restart
    # on continuous with the quick period.
    $self->undo_timer->Start(wxTheApp->config->undo_wait_milliseconds, wxTIMER_ONE_SHOT);
    $self->undo_or_redo_flag($flag);

    # do the correct action immediately on mouse down
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
sub show_text_popup { #{{{1
    my ($self, $items, $callback, $button) = @_;

    my $menu = Wx::Menu->new();

    my @ids;

    # link the auto-generated ids to the item name; this hash is built every time
    # we display a popup menu, since only one can be displayed at a time.
    $self->popup_name({});

    my $add_items;
    $add_items = sub {
        my ($parent_menu, $items, $parent_names) = @_;

        for my $item (@{ $items }) {

            my ($menu_item, $item_name);
            if (ref $item) {
                # this is a submenu; do this recursively
                my $sub_menu = Wx::Menu->new();

                # the first item in the list is the submenu label
                $item_name = shift @{ $item };

                $add_items->($sub_menu, $item, [ @{ $parent_names }, $item_name ]);

                # add the submenu to the parent
                $menu_item = $parent_menu->Append(-1, $item_name, $sub_menu);

            }
            else {
                if ($item =~ /(.*)\?([01])/) {
                    $item = $1;
                    my $flag = $2;
                    $menu_item = $parent_menu->AppendCheckItem(-1, $item);
                    $parent_menu->Check($menu_item->GetId, $flag) if $flag;
                }
                else {
                    $menu_item = $parent_menu->Append(-1, $item);
                }
                $item_name = join(':', @{ $parent_names }, $item);
            }

            push @ids, $menu_item->GetId;
            $self->popup_name->{ $menu_item->GetId } = $item_name;
        }
    };

    $add_items->($menu, $items, []);

    Wx::Event::EVT_MENU_RANGE($self, List::Util::min(@ids), List::Util::max(@ids), $callback);

    my @point = $button->GetPositionXY;
    my @size = $button->GetSizeWH;
    $point[0] += $size[0];
    $point[1] += $size[1] - 6;
    $point[1] -= 23 * scalar @{ $items };
    $self->PopupMenu($menu, @point);

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
            unless IsoScene::action_is_branch($branch_node);

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

        # the size of the undo stack is our reference point; when we change branches, we must undo to this
        # point before redoing the new branch
        $frame->branch_choice_position( $#{ $frame->canvas->scene->undo_stack } );

        $branch_chc->SetSelection($branch_node->{current_branch});

        $frame->change_action($AC_CHOOSE_BRANCH);
        $frame->change_mode($MO_MOVE) unless $frame->pan_mode;
        $frame->branch_choice_pnl->Show;
        $frame->toggle_tool_panel;
        $frame->Layout;

        # don't want the scene saving while we're changing branch.
        wxTheApp->autosave_timer->Stop;

    }
    elsif ($button_name =~ /(?:undo|redo)_many/) {

        $frame->canvas->undo_or_redo($button_name eq 'redo_many', 1, 1);
        $frame->canvas->rebuild_and_refresh;
    }
    elsif ($button_name eq 'undo_to_branch') {
        if (@{ $frame->canvas->scene->undo_stack }) {
            while (1) {
                last unless $frame->canvas->undo_or_redo(0,1);
                last if IsoScene::action_is_branch($frame->canvas->scene->redo_stack->[-1]);
            }
            $frame->canvas->rebuild_and_refresh;
        }
        else {
            $frame->display_message("There are no actions to undo.");
        }
    }
    elsif ($button_name eq 'redo_to_branch') {
        if (@{ $frame->canvas->scene->redo_stack }) {
            while (1) {
                last unless $frame->canvas->undo_or_redo(1,1);
                last if IsoScene::action_is_branch($frame->canvas->scene->redo_stack->[-1]);
            }
            $frame->canvas->rebuild_and_refresh;
        }
        else {
            $frame->display_message("There are no actions to redo.");
        }
    }
    elsif ($button_name eq 'new_branch') {
        if (@{ $frame->canvas->scene->redo_stack }) {

            if (IsoScene::action_is_branch($frame->canvas->scene->redo_stack->[-1])) {
                $frame->display_message("There is already a branch at this point.");
            }
            else {

                # turn this normal node into a branch node, so that if we do a new action at this
                # point, the old redo stack will be preserved as a branch.
                my $action = pop @{ $frame->canvas->scene->redo_stack };
                my $branch_node = {
                    current_branch => 0,
                    branches => [ $action ],
                };
                push @{ $frame->canvas->scene->redo_stack }, $branch_node;
                $frame->display_message("Branch created.");
            }
        }
        else {
            $frame->display_message("You must undo some actions before creating a branch.");
        }
    }
    elsif ($button_name eq 'clear_undo_info') {
        $frame->canvas->scene->undo_stack([]);
        $frame->canvas->scene->redo_stack([]);
        $frame->display_message("Undo & Redo information has been cleared.");
        $frame->canvas->set_undo_redo_button_states;
    }


    return;
}

################################################################################
sub change_branch_choice { #{{{1
    my ($self, $event) = @_;

    $log->info("change_branch_choice " . $event->GetSelection);

    # if the undo stack is not at the reference position, undo back to there
    # before we change branch.
    $self->canvas->undo_to_position($self->branch_choice_position);

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

        # if the undo stack is not at the reference position, undo back to there
        # before we change branch.
        $self->canvas->undo_to_position($self->branch_choice_position);

        $self->canvas->change_to_branch($self->original_branch, 0);
    }

    $self->change_action($self->previous_action);
    $self->branch_choice_pnl->Hide;
    $self->toggle_tool_panel;
    $self->canvas->set_undo_redo_button_states;
    $self->Layout;

    wxTheApp->autosave_timer->Start(wxTheApp->config->autosave_period_seconds * 1000, wxTIMER_ONE_SHOT);

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
    elsif ($button_name =~ 'select_by_color_') {
        $frame->canvas->select_by_color($frame->canvas->select_by_color ? 0 : 1);
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

    return;
}

#################################################################################
1;

# todo...
