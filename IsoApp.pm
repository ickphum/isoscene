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
use File::Slurp qw(read_file);
use Wx::XRC;
use Alien::wxWidgets;
use Time::Piece;

use IsoFrame;
use IsoScene;
use IsoConfig;

my $log = get_logger;

__PACKAGE__->mk_accessors( qw(bitmap scene config autosave_timer script_timer script_delay script_lines script_action action_args frame xrc) );

sub new { # {{{1
    my( $class, $option ) = @_;
    my $self = $class->SUPER::new();

    $self->{xrc} = Wx::XmlResource->new();
    $self->xrc->InitAllHandlers;
    $self->xrc->Load('export_options.xrc');
    $self->xrc->Load('export_options_pnl.xrc');
    $self->xrc->Load('choose_branch.xrc');
    $self->xrc->Load('message_pane.xrc');

    my @images = qw(cube menu paint sample import erase shade_cube
        undo undo_many redo redo_many undo_redo_menu branch_redo 
            redo_to_branch undo_to_branch choose_branch new_branch clear_undo_info
        select tick_L tick_T tick_R tick_TL tick_TR
        copy cut paste selection_tools select_all select_none select_visible
        small_paste small_cube);
    for my $image_name (qw(move area_L area_T area_R flood)) {
        push @images, "${image_name}_on", "${image_name}_off";
    }
    for my $action (grep { $_ !~ /$IsoFrame::AC_IMPORT|$IsoFrame::AC_PASTE|$IsoFrame::AC_CHOOSE_BRANCH|$IsoFrame::AC_VIEW/ } @IsoFrame::ACTIONS) {
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

    # try to load the most recent file saved
    my $extension = $self->config->use_binary_files
        ? '.isb'
        : $self->config->use_compressed_files 
            ? '.isz' 
            : '.isc';
    $log->info("extension $extension");
    my $filename = $option->{file} ||
        ( @{ $self->config->previous_scene_files } && -f ($self->config->previous_scene_files->[-1] . $extension)
            ? $self->config->previous_scene_files->[-1]
            : undef);

    $self->scene( IsoScene->new({ file => $filename }) );

    $self->frame( IsoFrame->new($self->scene));

    $self->set_frame_title;

    $self->SetTopWindow($self->frame);
    $self->frame->Show(1);

    $self->autosave_timer(Wx::Timer->new($self));
    Wx::Event::EVT_TIMER($self, $self->autosave_timer, sub {
        my ($parent, $event) = @_;

        # we're either waiting a minute or 2 seconds
        my $interval = $self->autosave_timer->GetInterval;
        if ($interval == $self->config->autosave_period_seconds * 1000) {

            # the long timer has gone off, so we're ready to autosave. 
            # We restart the timer with the short period.
            $log->debug("long timer, wait for idle period");
            $self->autosave_timer->Start($self->config->autosave_idle_seconds * 1000, wxTIMER_ONE_SHOT);
        }
        else {

            # The short timer has gone off, meaning no mouse activity occurred in the 
            # period, we'll save and restart with the long interval again.
            $log->debug("short timer, save and reset to long");

            $self->scene->background_save;
            $self->config->save; 
            $self->autosave_timer->Start($self->config->autosave_period_seconds * 1000, wxTIMER_ONE_SHOT);
        }
    });

    # don't start the autosave timer while we're playing a script. It will start afterward.
    if (my $file = $option->{script}) {

        if (-r $file && -f $file) {
            $self->script_lines([ grep { /^[^#]/ } read_file($file) ]);
#            $self->script_pos(0);
            $self->script_timer(Wx::Timer->new($self));
            Wx::Event::EVT_TIMER($self, $self->script_timer, \&do_script_action);
            $self->frame->toggle_tool_panel;
            $self->script_delay($self->config->script_delay_milliseconds);
            $self->script_timer->Start($self->script_delay, wxTIMER_CONTINUOUS);
            $self->config->autosave_on_exit(0);
            $self->get_next_script_action;
        }
        else {
            $log->debug("Script file '$file' is not a readable plain file");
        }
    }
    else {
        $self->autosave_timer->Start($self->config->autosave_period_seconds * 1000, wxTIMER_ONE_SHOT);
    }

    return $self;
}

################################################################################
sub get_next_script_action { #{{{1
    my ($self) = @_;

    if (my $line = shift @{ $self->script_lines }) {
        if ($line =~ /\A(redo\w*|undo\w*|delay|branch|message|exit)\s?(\S.*)?/) {
            my ($action, $args) = ($1,$2);

            $log->info("new action $action, args " . ($args || ''));
            $self->script_action($action);
            $self->action_args($args);
        }
        else {
            $log->logdie("unknown script line '$line'");
        }
    }
    else {
        $self->stop_script;
    }

    return;
}

################################################################################
sub do_script_action { #{{{1
    my ($self) = @_;

    my $action = $self->script_action;
    my $args = $self->action_args;
    my $rc;

    # assume that the action can be performed at this point, then check for finished afterwards,
    # so we can load the next action once this one is finished.
    if ($action =~ /redo/) {
        $rc = $self->frame->canvas->undo_or_redo(1);
    }
    elsif ($action =~ /undo/) {
        $rc = $self->frame->canvas->undo_or_redo(0);
    }
    elsif ($action eq 'delay') {

        # record new delay so we can restart after a message
        $self->script_delay($args);
        $rc = $self->script_timer->Start($args, wxTIMER_CONTINUOUS);
    }
    elsif ($action eq 'branch') {
        $rc = $self->frame->canvas->change_to_branch($args,0);
    }
    elsif ($action eq 'message') {
        $rc = $self->frame->display_message($args);
    }
    elsif ($action eq 'exit') {
        $self->frame->Destroy;
        $rc = 1;
    }
    else {
        $log->logconfess("bad action $action");
    }

    unless ($rc) {
        $log->warn("failed to do script action $action");
        $self->stop_script;
        return;
    }

    # have we finished now? check now so we can load the next action before the next timed event.
    my $finished;
    if ($action eq 'redo') {
        $args-- if defined $args;
        $finished = scalar @{ $self->scene->redo_stack } == 0 || (defined $args && $args == 0);
    }
    elsif ($action eq 'undo') {
        $args-- if defined $args;
        $finished = scalar @{ $self->scene->undo_stack } == 0 || (defined $args && $args == 0);
    }
    elsif ($action =~ /til_branch/) {

        # we're finished when the top item on the redo stack is a branch; note that we don't want to
        # end (here) when the stack is empty, we want to fail when the next action is attempted.
        if (scalar @{ $self->scene->redo_stack }) {
            $finished = IsoScene::action_is_branch($self->scene->redo_stack->[-1]);
        }
    }
    elsif ($action =~ /delay|branch|message/) {
        $finished = 1;
    }

    # update this in case we changed it
    $self->action_args($args);

    if ($finished) {
        $self->get_next_script_action;
    }

    return;
}

################################################################################
sub stop_script { #{{{1
    my ($self) = @_;

    # script finished, so display controls and start auto-save timer
    $self->script_timer->Stop;
#    $self->frame->toggle_tool_panel;
#    $self->autosave_timer->Start($self->config->autosave_period_seconds * 1000, wxTIMER_CONTINUOUS);

    return;
}

################################################################################
# Tricky to find a single point to do this (needed at load, open & save as),
# so just make it easy to call.
sub set_frame_title { #{{{1
    my ($self, $message) = @_;

    my $scene_file = $self->scene->filename;
    $self->frame->SetTitle("$scene_file - IsoScene" . ($message ? " - $message" : ''));

    return;
}

################################################################################
sub set_button_bitmap { #{{{1
    my ($button, $bitmap_name) = @_;

    $log->logconfess("null button") unless defined $button;
    $log->logconfess("null bitmap_name") unless defined $bitmap_name;
    my $bitmap = wxTheApp->bitmap->{$bitmap_name}
        or $log->logconfess("bitmap $bitmap_name not loaded");

    if (Alien::wxWidgets->version > 2.9) {
        $button->SetBitmap($bitmap);
    }
    else {
        $button->SetBitmapLabel($bitmap);
    }

    return $button;
}

################################################################################
sub save_panel_settings { #{{{1
    my ($self, $panel) = @_;

    # setting group is named for the panel, less "_pnl"
    (my $setting_group = $panel->GetName) =~ s/_pnl//;
    my $scene = $self->scene;
    $scene->$setting_group({});
    my $setting = $scene->$setting_group;

    # recursive sub to find all child controls
    my $get_child_values;
    $get_child_values = sub {
        my ($parent) = @_;

        for my $child ( $parent->GetChildren ) {
            $log->debug("child $child " . $child->GetName . " has children? " . ($child->GetChildren ? 'yes' : 'undef'));
            if ($child->GetChildren) {
                $get_child_values->($child);
            }
            else {
                $setting->{ $child->GetName } = $child->GetValue if $child->GetName =~ /_(?:rbn|chb|txt|sld)\z/;;
            }
        }
    };

    $get_child_values->($panel);

    $log->info("save_panel_settings: $setting_group = " . Dumper($setting));

    return;
}

################################################################################
sub load_panel_settings { #{{{1
    my ($self, $panel) = @_;

    (my $setting_group = $panel->GetName) =~ s/_pnl//;
    my $scene = $self->scene;
    my $setting = $scene->$setting_group;

    for my $control_name (keys %{ $setting } ) {
        next unless $control_name =~ /_(?:rbn|chb|txt|sld)\z/;

        # FindWindow is recursive so we don't need to be
        if (my $control = $panel->FindWindow($control_name)) {
            $control->SetValue($setting->{ $control_name });
        }
    }

    return;
}

################################################################################

sub OnInit { # {{{1
    my( $self ) = shift;

    Wx::InitAllImageHandlers();

    my $rc = $self->SUPER::OnInit();
    $log->debug("class init $rc");

    return 1;
}

################################################################################
# OnExit is called after windows are closed, so don't do anything time-consuming
# as the user might pull the plug, hit ctrl-c, etc, thinking everything is done.
#sub OnExit { # {{{1
#    my( $self ) = shift;
#
#    return 1;
#}

################################################################################
sub datetime_description { #{{{1
    my ($tp) = @_;

    my $now = localtime;
    my $minutes = int(($now - scalar $tp ) / 60);
    my $value;

    if ($minutes < 600) {

        # in the last 10 hours, give a relative time ago ie M minutes ago or H hours, M minutes ago.
        if ($minutes < 60) {
            $value = '';
        }
        else {
            my $hours = int($minutes/60);
            $minutes -= $hours * 60;
            $value = $hours . ' ' . ($hours == 1 ? 'hour' : 'hours') . ', ';
        }
        $value .= $minutes . ' ' . ($minutes == 1 ? 'minute' : 'minutes') . ' ago';
    }
    else {

        # date and time, converting to today, yesterday, day before yesterday, last <weekday> where possible or just date.
        my $elapsed_days = $now->strftime('%j') - $tp->strftime('%j');

        my %day_term = (
            0 => 'today',
            1 => 'yesterday',
            2 => 'day before yesterday',
        );

        my $day = $day_term{$elapsed_days};
        unless ($day) {
            if ($elapsed_days < 7) {

                # use "Last <weekday>" in the last week
                $day = "last " . $tp->strftime("%A");
            }
            else {

                # final catch-all, just Fri Mar 13.
                $day = $tp->strftime("%a %b %d");
            }
        }

        ($value = $tp->strftime("%l:%M %P") . " $day") =~ s/\A\s+//;
    }

    $value =~ s/, 0 minutes//;

    return $value;
}

1;
