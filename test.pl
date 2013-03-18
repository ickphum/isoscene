#!/usr/bin/perl -w -- 

use Wx qw[:everything];
use strict;
use Log::Log4perl qw(get_logger);
use Data::Dumper qw(Dumper);

my $log;

package TestCanvas; #{{{1

use Wx qw(:everything);
use base qw(Wx::Panel);

sub new { # {{{2
    my( $class, $parent ) = @_;
    my $self = $class->SUPER::new( $parent, -1, [ -1, -1 ], [ -1, -1 ], 0, "test canvas"); 

    Wx::Event::EVT_PAINT( $self,
        sub {
            my $dc = Wx::PaintDC->new( $self );
            $dc->DrawText("Test application", 50, 50);

            return;
        }
    );

    return $self;
}

package TestFrame; # {{{1

use Wx qw[:everything];
use Data::Dumper qw(Dumper);
use base qw(Wx::Frame);
use strict;

sub new { # {{{2
    my( $self, $parent, $id, $title, $pos, $size, $style, $name ) = @_;

    $self = $self->SUPER::new( undef , -1, 'Test', [ 2000, 400 ], [ 400, 300 ]);
    $self->{v_sizer} = Wx::BoxSizer->new(wxVERTICAL);
#    $self->{canvas} = TestCanvas->new($self);
#    $self->{v_sizer}->Add($self->{canvas}, 1, wxEXPAND, 1);
    for my $label (qw(action undo redo redo-branch-0 redo-branch-1 redo-branch-2)) {
        my $button = Wx::Button->new($self, -1, $label);

        Wx::Event::EVT_BUTTON($self, $button, sub {

            if ($label eq 'action') {
                my $undo_action = [ $self->{action_number} ];
                if (my $new_branch_node = pop @{ $self->{redo_stack} }) {
                    $log->info("create branch then do new action $self->{action_number}");
                    
                    if ((ref $new_branch_node) ne 'HASH') {

                        # turn this simple item into a branch item pointing to the current redo stack, 
                        # then we can add the new branch.
                        $new_branch_node = {
                            branches => [ $new_branch_node, ],
                            current_branch => 0,
                        };
                    }

                    # data for the current branch is an action; turn this into a list of actions.
                    my $current_branch = $new_branch_node->{current_branch};
                    $new_branch_node->{branches}->[ $current_branch ] = [ @{ $self->{redo_stack} }, $new_branch_node->{branches}->[$current_branch],  ];

                    # add the new branch (containing the new undo action) and make it current
                    push @{ $new_branch_node->{branches} }, $undo_action;
                    $new_branch_node->{current_branch} = $#{ $new_branch_node->{branches} };

                    push @{ $self->{undo_stack} }, $new_branch_node;
                    $self->{redo_stack} = [];
                }
                else {
                    push @{ $self->{undo_stack} }, $undo_action;
                    $log->info("do new action $self->{action_number}");
                }
                push @{ $self->{actions} }, $self->{action_number};
                $self->{action_number}++;
            }
            elsif ($label eq 'undo') {

                if (@{ $self->{undo_stack} }) {
                    my $action = pop @{ $self->{undo_stack} };
                    my $action_number;
                    if ( (ref $action) eq 'HASH' ) {
                        $log->info("undo branch action");
                        $action_number = $action->{branches}->[ $action->{current_branch} ]->[ 0 ];
                    }
                    else {
                        $log->info("undo action $action->[0]");
                        $action_number = $action->[0];
                    }
                    push @{ $self->{redo_stack} }, $action;
                    my $top_action = pop @{ $self->{actions} };
                    if ($top_action != $action_number) {
                        $log->warn("top action $top_action didn't match undo action $action_number");
                    }
                }
                else {
                    $log->warn("can't undo any more");
                }
            }
            elsif ($label =~ /redo/) {

                if (@{ $self->{redo_stack} }) {

                    my $action = pop @{ $self->{redo_stack} };

                    if ($label =~ /redo-branch-(\d+)/) {
                        my $new_branch = $1;
                        if ( (ref $action) eq 'HASH' ) {
                            if ($new_branch <= $#{ $action->{branches} }) {

                                my $current_branch = $action->{current_branch};

                                # turn current redo list (one item in this node plus redo stack) into a list
                                $action->{branches}->[ $current_branch ] = [ @{ $self->{redo_stack} }, $action->{branches}->[$current_branch], ];

                                # set redo stack to the new branch
                                $self->{redo_stack} = $action->{branches}->[ $new_branch ];

                                # pop redo stack into the node's branch list entry for the new branch
                                $action->{branches}->[ $new_branch ] = pop @{ $self->{redo_stack} };

                                # set current branch to new branch
                                $action->{current_branch} = $new_branch;
                            }
                            else {
                                $log->warn("tried to redo branch $new_branch and this branch node doesn't have that many branches");
                            }
                        }
                        else {
                            $log->warn("tried to redo branch $new_branch on a normal node");
                        }
                    }

                    # should be able to do a normal redo now
                    my $action_number;
                    if ( (ref $action) eq 'HASH' ) {

                        # redo the existing current branch for this branch node
                        $log->info("redo branch action");
                        $action_number = $action->{branches}->[ $action->{current_branch} ]->[ 0 ];
                    }
                    else {
                        $log->info("redo action $action->[0]");
                        $action_number = $action->[0];
                    }
                    push @{ $self->{undo_stack} }, $action;
                    push @{ $self->{actions} }, $action_number;
                }
                else {
                    $log->warn("can't redo any more");
                }
            }

            $log->info("undo stack: " . Dumper($self->{undo_stack}));
            for my $node ( @{ $self->{undo_stack} }) {
                $self->display_node($node, 0);
            }

            $log->info("redo stack: " . Dumper($self->{redo_stack}));
            for my $node ( @{ $self->{redo_stack} }) {
                $self->display_node($node, 0);
            }

            my $stack_str .= 'actions ' . join(' ', @{ $self->{actions} }) . "\n";
            $self->{label}->SetLabel($stack_str);

        });

        $self->{v_sizer}->Add($button);
    }

    $self->{label} = Wx::StaticText->new($self, -1, "test label", wxDefaultPosition, wxDefaultSize);
    $self->{v_sizer}->Add($self->{label}, 1, wxEXPAND, 1);

    $self->{undo_stack} = [];
    $self->{redo_stack} = [];
    $self->{actions} = [];
    $self->{action_number} = 1;

    $self->SetSizer($self->{v_sizer});
    $self->SetTitle("Test");

    return $self;
}

#################################################################################
sub display_node { #{{{2
    my ($self, $node, $indent_length) = @_;

    my $indent = '  ' x $indent_length;
    if ((ref $node) eq 'HASH') {
        $log->info("${indent} branch node, current index $node->{current_branch}");
        my $i = 0;
        for my $branch ( @{ $node->{branches} } ) {
            $log->info("$indent branch $i");
            if ($i == $node->{current_branch}) {
                $self->display_node($branch, $indent_length + 1);
            }
            else {
                for my $node ( @{ $branch } ) {
                    $self->display_node($node, $indent_length + 1);
                }
            }
            $i++;
        }
    }
    else {
        $log->info("${indent} normal node, action $node->[0]");
    }

    return;
}

#################################################################################
#
package TestApp; # {{{1

use Wx qw[:everything];
use base qw(Wx::App Class::Accessor::Fast);
__PACKAGE__->mk_accessors( qw(frame));

sub new { # {{{2
    my( $class, @args ) = @_;
    my $self = $class->SUPER::new( @args );

    $self->frame( TestFrame->new($self, -1) );
    $self->SetTopWindow($self->frame);
    $self->frame->Show(1);

    return $self;
}

#*******************************************************************************
sub OnInit { # {{{2
    my( $self ) = shift;

    my $rc = $self->SUPER::OnInit();

    return 1;
}

#*******************************************************************************

package main; # {{{1

$ENV{log_appenders} = "file, screen";
$ENV{log_level}     = 'INFO';
$ENV{log_dir}       = ".";
$ENV{log_file_name} = 'test';

Log::Log4perl->init("./log4perl.conf");
$log = get_logger();

unless(caller){

    my $app = TestApp->new();
    $app->MainLoop();
}

################################################################################

__END__

