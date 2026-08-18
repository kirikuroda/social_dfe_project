import json
from otree.api import *
import social_gambling_task.experiment_settings as exp_settings
CONSTANTS = exp_settings.CONSTANTS

doc = "Practice"

class C(BaseConstants):
    NAME_IN_URL = "practice"
    MAIN_TEMPLATE = CONSTANTS["MAIN_TEMPLATE"]
    TASK_TEMPLATE = CONSTANTS["TASK_TEMPLATE"]
    NUM_ROUNDS = CONSTANTS["NUM_PRACTICE_ROUNDS"]
    PLAYERS_PER_GROUP = None
    SOCIAL_CONDITION = CONSTANTS["SOCIAL_CONDITION"]
    PERSONAL_CONDITION = CONSTANTS["PERSONAL_CONDITION"]
    PRACTICE_ROUND = CONSTANTS["PRACTICE_ROUND"]
    PRACTICES = CONSTANTS["PRACTICES"]
    MAX_SAMPLE = CONSTANTS["MAX_SAMPLE"]
    POSITION_1 = CONSTANTS["POSITION_1"]
    POSITION_2 = CONSTANTS["POSITION_2"]
    IS_REAL_ROUND = False

class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass

class Player(BasePlayer):
    # Round configuration
    option_id = models.IntegerField()
    position = models.BooleanField()  # True if ab, False if ba
    
    # Lottery sequences
    seq_a = models.LongStringField()  # JSON string of lottery A outcomes
    seq_b = models.LongStringField()  # JSON string of lottery B outcomes
    
    # Raw task data (JSON format)
    taskdata = models.LongStringField()
    
    # Sampling behavior
    sample = models.LongStringField()  # Samples taken during the trial
    rt_sample = models.LongStringField()  # Reaction times for sampling
    feedback_time_sample = models.LongStringField()  # Time spent viewing feedback
    
    # Choice behavior
    choice_self = models.StringField()  # Final choice ('a', 'b', or 'miss')
    rt_self = models.IntegerField()  # Reaction time for final choice
    frame_self = models.IntegerField()  # Frame number when choice was made

def creating_session(subsession):
    """Initialize practice session with randomized round order."""
    if subsession.round_number == 1:
        players = subsession.get_players()
        for player in players:
            # Set task parameters for each player with randomized practice order
            round_order = exp_settings.shuffle_round_order(C)
            exp_settings.set_player_params(player, C, round_order)

def is_first_round(player):
    """Check if this is the first practice round."""
    return player.round_number == 1

############################################
# Pages
############################################

class StartPractice(Page):
    """Introduction page shown only at the beginning of practice rounds."""
    is_displayed = is_first_round

class Practice(Page):
    """Main practice page where participants try out lotteries."""

    form_model = "player"
    form_fields = ["taskdata"]

    @staticmethod
    def js_vars(player):
        """Send necessary variables to JavaScript for practice task."""
        return dict(
            condition=C.PRACTICE_ROUND,
            seq_a=player.seq_a,
            seq_b=player.seq_b,
            position=C.POSITION_1 if player.position else C.POSITION_2,
            round_number=player.round_number,
            max_sample=C.MAX_SAMPLE
        )
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        taskdata = json.loads(player.taskdata)
        player.sample = str(taskdata["sample"])
        player.rt_sample = str(taskdata["rt_sample"])
        player.feedback_time_sample = str(taskdata["feedback_time_sample"])
        player.choice_self = taskdata["choice_self"]
        player.rt_self = taskdata["rt_self"]
        player.frame_self = taskdata["frame_self"]

# Only in the social condition
class BeforeMatching(Page):
    """Information page before matching with other players (social condition only)."""
    timeout_seconds = 120
    @staticmethod
    def is_displayed(player):
        return player.session.config["condition"] == C.SOCIAL_CONDITION and player.round_number == C.NUM_ROUNDS

# Only in the social condition
class SearchingPlayer(Page):
    """Waiting page while searching for other players (social condition only)."""
    before_next_page = exp_settings.record_time_last_page
    
    @staticmethod
    def is_displayed(player):
        return player.session.config["condition"] == C.SOCIAL_CONDITION and player.round_number == C.NUM_ROUNDS

############################################
# Data and page_sequence
############################################

def custom_export(players):
    """Export practice round data for analysis."""
    yield [
        "subject_id", "round_number", "option_id", "is_real_round", "position",
        "sequence_a", "sequence_b", "samples", "rt_samples", "feedback_time_samples",
        "choice_self", "rt_self", "frame_self"
    ]
    for p in players:
        participant = p.participant
        yield [
            participant.code, p.round_number, p.option_id, C.IS_REAL_ROUND, p.position,
            p.seq_a, p.seq_b, p.sample, p.rt_sample, p.feedback_time_sample,
            p.choice_self, p.rt_self, p.frame_self
        ]

# This is the entire app
page_sequence = [
    StartPractice,
    Practice,
    BeforeMatching,
    SearchingPlayer
]
