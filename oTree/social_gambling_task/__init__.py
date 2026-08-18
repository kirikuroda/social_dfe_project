import json
import time
from otree.api import *
import social_gambling_task.experiment_settings as exp_settings
CONSTANTS = exp_settings.CONSTANTS

doc = "Main task: Social gambling (Kuroda, Ciranka, Wulff, Kurvers)"

class C(BaseConstants):
    NAME_IN_URL = "task"
    MAIN_TEMPLATE = CONSTANTS["MAIN_TEMPLATE"]
    TASK_TEMPLATE = CONSTANTS["TASK_TEMPLATE"]
    NUM_REAL_ROUNDS = CONSTANTS["NUM_REAL_ROUNDS"]
    NUM_ATTENTION_CHECKS = CONSTANTS["NUM_ATTENTION_CHECKS"]
    NUM_ROUNDS = NUM_REAL_ROUNDS + NUM_ATTENTION_CHECKS
    PLAYERS_PER_GROUP = CONSTANTS["PLAYERS_PER_GROUP"]
    SOCIAL_CONDITION = CONSTANTS["SOCIAL_CONDITION"]
    PERSONAL_CONDITION = CONSTANTS["PERSONAL_CONDITION"]
    OPTIONS = CONSTANTS["OPTIONS"]
    MAX_SAMPLE = CONSTANTS["MAX_SAMPLE"]
    POSITION_1 = CONSTANTS["POSITION_1"]
    POSITION_2 = CONSTANTS["POSITION_2"]
    ROUND_ATTENTION_1 = CONSTANTS["ROUND_ATTENTION_1"]
    ROUND_ATTENTION_2 = CONSTANTS["ROUND_ATTENTION_2"]
    ROUND_ATTENTION_3 = CONSTANTS["ROUND_ATTENTION_3"]
    ROUND_ATTENTION_4 = CONSTANTS["ROUND_ATTENTION_4"]
    TIMEOUT_MATCHING = CONSTANTS["TIMEOUT_MATCHING"]
    TIMEOUT_MARGIN = CONSTANTS["TIMEOUT_MARGIN"]
    TIMEOUT_START = CONSTANTS["TIMEOUT_START"]
    IS_REAL_ROUND = True

class Subsession(BaseSubsession):
    # Round type flags
    is_attention_check = models.BooleanField()  # True if this round is an attention check
    n_waiting_players = models.IntegerField(initial=0)  # Number of players waiting for matching

class Group(BaseGroup):
    # Group dynamics
    n_active_players = models.IntegerField(initial=C.PLAYERS_PER_GROUP)  # Number of active players in group
    n_sampled_players = models.LongStringField()  # Number of players who sampled a lottery in group
    n_chosen_players = models.LongStringField()  # Number of players who chose a lottery in group
    
class Player(BasePlayer):
    # Round completion status
    is_round_completed = models.BooleanField(initial=False)
    
    # Round configuration
    option_id = models.IntegerField()  # Index of the lottery option
    position = models.BooleanField()  # True if ab, False if ba
    
    # Lottery sequences (JSON strings)
    seq_a = models.LongStringField()  # Sequence of outcomes for lottery A
    seq_b = models.LongStringField()  # Sequence of outcomes for lottery B
    
    # Raw task data (JSON format)
    taskdata = models.LongStringField()  # Main task data
    taskdata2 = models.StringField(blank=True)  # Honeypot field for bot detection
    
    # Sampling behavior
    sample = models.LongStringField()  # Samples taken during the trial
    rt_sample = models.LongStringField()  # Reaction times for sampling
    feedback_time_sample = models.LongStringField()  # Time spent viewing feedback
    
    # Own choice behavior
    choice_self = models.StringField()  # Final choice ('a', 'b', or 'miss')
    rt_self = models.IntegerField()  # Reaction time for final choice
    frame_self = models.IntegerField()  # Frame number when choice was made
    
    # Other players' behavior (social condition only)
    choice_other = models.LongStringField()  # Other players' choices
    rt_other = models.LongStringField()  # Other players' reaction times
    frame_other = models.LongStringField()  # Other players' frame numbers

def is_attention_check_round(round_number):
    """Check if the given round number is an attention check round."""
    attention_rounds = {
        C.ROUND_ATTENTION_1, 
        C.ROUND_ATTENTION_2, 
        C.ROUND_ATTENTION_3, 
        C.ROUND_ATTENTION_4
    }
    return round_number in attention_rounds

def creating_session(subsession):
    """Initialize session parameters and set attention check flags."""
    condition = subsession.session.config["condition"]
    round_number = subsession.round_number
    
    # Set attention check flag
    subsession.is_attention_check = is_attention_check_round(round_number)
    
    # Set task parameters only for the personal condition on first round
    if round_number == 1 and condition == C.PERSONAL_CONDITION:
        players = subsession.get_players()
        for player in players:
            round_order = exp_settings.shuffle_round_order(C)
            exp_settings.set_player_params(player, C, round_order)

def group_by_arrival_time_method(subsession, waiting_players):
    """Custom matching method that handles timeouts and dropouts."""

    # Matching completed - form a group
    if len(waiting_players) >= C.PLAYERS_PER_GROUP:
        return waiting_players[:C.PLAYERS_PER_GROUP]
    
    # Update the number of waiting players
    subsession.n_waiting_players = len(waiting_players)

    # Check for timeouts
    current_time = time.time()
    for player in waiting_players:
        participant = player.participant
        time_elapsed = current_time - participant.time_last_page
        
        # Dropout due to excessive timeout
        # if time_elapsed > C.TIMEOUT_MATCHING + C.TIMEOUT_MARGIN:
        #     participant.is_dropout_self = True
        #     return [player]
        # Regular timeout (matching timeout)
        if time_elapsed > C.TIMEOUT_MATCHING:
            participant.is_matching_timeout = True
            return [player]

def update_n_active_players(group, n_active_players):
    """Update the number of active players for all remaining rounds."""
    round_number = group.round_number
    groups = group.in_rounds(round_number, C.NUM_ROUNDS)
    for g in groups:
        g.n_active_players = n_active_players

def process_task_data(player, taskdata):
    """Process and store task data from the frontend."""
    # Record the data
    if player.group.n_active_players != taskdata["n_active_players"]:
        update_n_active_players(player.group, taskdata["n_active_players"])

    player.sample = str(taskdata["sample"])
    player.rt_sample = str(taskdata["rt_sample"])
    player.feedback_time_sample = str(taskdata["feedback_time_sample"])
    player.choice_self = taskdata["choice_self"]
    player.rt_self = taskdata["rt_self"]
    player.frame_self = taskdata["frame_self"]
    player.choice_other = str(taskdata["choice_other"])
    player.rt_other = str(taskdata["rt_other"])
    player.frame_other = str(taskdata["frame_other"])

def update_dropout_status(player, taskdata):
    """Update participant dropout status based on task data."""
    participant = player.participant
    participant.is_dropout_self = taskdata["is_dropout_self"]
    participant.is_dropout_other = taskdata["is_dropout_other"]

def calculate_round_payoff(player):
    """Calculate payoff for the current round based on player's choice."""
    choice_self = player.choice_self
    option_id = player.option_id
    option = C.OPTIONS.iloc[option_id]
    
    # Mark round as completed if player made a valid choice
    player.is_round_completed = choice_self != "miss"
    
    if choice_self == "miss":
        player.payoff = 0
    else:
        # Get lottery parameters based on choice
        if choice_self == "a":
            p = option["p_a_high"]
            v_high = option["v_a_high"]
            v_low = option["v_a_low"]
        else:  # choice_self == "b"
            p = option["p_b_high"]
            v_high = option["v_b_high"]
            v_low = option["v_b_low"]
        
        player.payoff = exp_settings.draw_lottery(choice_self, p, v_high, v_low)

############################################
# Pages
############################################

class MatchingWaitPage(WaitPage):
    """Waiting page for matching players in social condition."""
    
    title_text = "Waiting room"
    group_by_arrival_time = True
    template_name = "social_gambling/MatchingWaitPage.html"

    @staticmethod
    def is_displayed(player):
        return (player.session.config["condition"] == C.SOCIAL_CONDITION and
                player.participant.is_dropout_self == False and
                player.round_number == 1)
    
    @staticmethod
    def after_all_players_arrive(group):
        """Set task parameters for all players once matching is complete."""
        if group.session.config["condition"] == C.SOCIAL_CONDITION:
            players = group.get_players()
            round_order = exp_settings.shuffle_round_order(C)
            for player in players:
                # Set task params to group-condition players
                exp_settings.set_player_params(player, C, round_order)
            groups = group.in_rounds(1, C.NUM_ROUNDS)
            for g in groups:
                g.n_sampled_players = json.dumps([0] * (C.MAX_SAMPLE + 1))
                g.n_chosen_players = json.dumps([0] * (C.MAX_SAMPLE + 1))

    @staticmethod
    def js_vars(player):
        return dict(
            time_last_page=player.participant.time_last_page,
            timeout=C.TIMEOUT_MATCHING
        )
    
    @staticmethod
    def vars_for_template(player):
        current_time = time.time()
        time_elapsed = current_time - player.participant.time_last_page
        is_timeout = 1 if time_elapsed > C.TIMEOUT_MATCHING else 0
        return dict(is_timeout=is_timeout)

# Calculate the total payoff and finish the task
class MatchingTimeout(Page):
    """Page shown when matching timeout occurs in social condition."""

    before_next_page = exp_settings.calculate_payoff
    app_after_this_page = exp_settings.go_to_prolific

    @staticmethod
    def is_displayed(player):
        participant = player.participant
        return (player.session.config["condition"] == C.SOCIAL_CONDITION and
                participant.is_matching_timeout == True and
                participant.is_dropout_self == False and
                player.round_number == 1)

# Personal condition
class StartTask1(Page):
    """Task start page for personal condition."""
    @staticmethod
    def is_displayed(player):
        return player.session.config["condition"] == C.PERSONAL_CONDITION and player.round_number == 1

# Social condition
class StartTask2(Page):
    """Task start page for social condition with countdown timer."""

    timeout_seconds = C.TIMEOUT_START
    live_method = exp_settings.check_started
    before_next_page = exp_settings.record_time_last_page

    @staticmethod
    def is_displayed(player):
        participant = player.participant
        return (player.session.config["condition"] == C.SOCIAL_CONDITION and
                participant.is_dropout_self == False and
                participant.is_matching_timeout == False and
                participant.is_page_started == False and
                player.round_number == 1)
    
    @staticmethod
    def vars_for_template(player):
        others = player.get_others_in_group()
        codes = [p.participant.code for p in others]
        return dict(codes=", ".join(codes))

# ITI + trial
class Task(Page):
    """Main task page where participants sample and choose between lotteries."""

    form_model = "player"
    form_fields = ["taskdata", "taskdata2"]

    @staticmethod
    def is_displayed(player):
        participant = player.participant
        if participant.is_page_started:
            participant.is_dropout_self = True
            return False
        return (participant.is_matching_timeout == False and
                participant.is_dropout_self == False and
                participant.is_dropout_other == False)
    
    @staticmethod
    def live_method(player, data):
        """Handle live communication between players in social condition."""
        exp_settings.check_started(player, data)  # Check if the page is started
        # Send responses except "started"
        if "choice" in data and player.session.config["condition"] == C.SOCIAL_CONDITION:
            group = player.group
            # Send the number of sampled players to the frontend
            choice = data["choice"]
            sample_id = data["sample_id"]
            if choice == "sampled":
                n_sampled_players = json.loads(group.n_sampled_players)
                n_sampled_players[sample_id] += 1
                group.n_sampled_players = json.dumps(n_sampled_players)
                response = {
                    "choice": "sampled",
                    "n_sampled_players": json.loads(group.n_sampled_players)[sample_id],
                    "sample_id": sample_id
                }
                return {0: response}
            # Send the choice of other players to the frontend
            elif choice == "a" or choice == "b":
                n_chosen_players = json.loads(group.n_chosen_players)
                for i in range(sample_id, len(n_chosen_players)):
                    n_chosen_players[i] += 1
                group.n_chosen_players = json.dumps(n_chosen_players)

                response = {}
                
                # To self
                response[player.id_in_group] = {
                    "choice": "chosen",
                    "n_chosen_players": n_chosen_players[sample_id],
                    "sample_id": sample_id
                }
                
                # To others
                others = player.get_others_in_group()
                for other in others:
                    response[other.id_in_group] = {
                        "choice": choice, # a or b
                        "n_chosen_players": n_chosen_players[sample_id],
                        "sample_id": sample_id
                    }
                
                return response
        return None
    
    @staticmethod
    def js_vars(player):
        """Send necessary variables to JavaScript for task execution."""
        return dict(
            condition=player.session.config["condition"],
            n_active_players=player.group.n_active_players,
            seq_a=player.seq_a,
            seq_b=player.seq_b,
            position=C.POSITION_1 if player.position else C.POSITION_2,
            round_number=player.round_number,
            max_sample=C.MAX_SAMPLE
        )

    @staticmethod
    def before_next_page(player, timeout_happened):
        taskdata = json.loads(player.taskdata)
        process_task_data(player, taskdata)
        update_dropout_status(player, taskdata)
        exp_settings.record_time_last_page(player, timeout_happened)
        calculate_round_payoff(player)

        # Bot detection
        if player.taskdata2:
            player.participant.is_bot = True

class FinishTask(Page):
    """Task completion page for participants who finished normally."""

    before_next_page = exp_settings.calculate_payoff
    app_after_this_page = exp_settings.go_to_prolific

    @staticmethod
    def is_displayed(player):
        participant = player.participant
        return (participant.is_matching_timeout == False and
                participant.is_dropout_self == False and
                participant.is_dropout_other == False and
                player.round_number == C.NUM_ROUNDS)

class Dropout(Page):
    """Page for participants who dropped out themselves."""

    before_next_page = exp_settings.calculate_payoff
    app_after_this_page = exp_settings.go_to_prolific

    @staticmethod
    def is_displayed(player):
        participant = player.participant
        return participant.is_dropout_self == True and player.round_number == C.NUM_ROUNDS

class DropoutPartner(Page):
    """Page for participants whose partners dropped out."""

    before_next_page = exp_settings.calculate_payoff
    app_after_this_page = exp_settings.go_to_prolific

    @staticmethod
    def is_displayed(player):
        participant = player.participant
        return (participant.is_matching_timeout == False and
                participant.is_dropout_self == False and
                participant.is_dropout_other == True and
                player.round_number == C.NUM_ROUNDS)

class Bot(Page):
    """Page for detected bots."""

    before_next_page = exp_settings.calculate_payoff
    app_after_this_page = exp_settings.go_to_prolific

    @staticmethod
    def is_displayed(player):
        return player.participant.is_bot


############################################
# Data and page_sequence
############################################

def custom_export(players):
    """Export the main task data for analysis."""
    yield [
        "subject_id", "group_id", "player_id", "round_number",
        "option_id", "is_real_round", "is_attention_check", "position",
        "sequence_a", "sequence_b", "samples", "rt_samples", "feedback_time_samples",
        "choice_self", "rt_self", "frame_self",
        "choice_other", "rt_other", "frame_other", "is_round_completed", "payoff_round", "honeypot_text"
    ]
    for p in players:
        subsession = p.subsession
        group = p.group
        participant = p.participant
        yield [
            participant.code, group.id_in_subsession, p.id_in_group, p.round_number,
            p.option_id, C.IS_REAL_ROUND, subsession.is_attention_check, p.position,
            p.seq_a, p.seq_b, p.sample, p.rt_sample, p.feedback_time_sample,
            p.choice_self, p.rt_self, p.frame_self,
            p.choice_other, p.rt_other, p.frame_other, p.is_round_completed, p.payoff, p.taskdata2
        ]

# This is the entire app
page_sequence = [
    MatchingWaitPage,
    MatchingTimeout,
    StartTask1,
    StartTask2,
    Task,
    FinishTask,
    Dropout,
    DropoutPartner,
    Bot
]
