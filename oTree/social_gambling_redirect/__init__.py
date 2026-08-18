import datetime
import urllib.parse
from otree.api import *
import social_gambling_task.experiment_settings as exp_settings
CONSTANTS = exp_settings.CONSTANTS

doc = "Redirect"

class C(BaseConstants):
    NAME_IN_URL = "redirect"
    NUM_ROUNDS = 1
    PLAYERS_PER_GROUP = None
    PROLIFIC_URL = CONSTANTS["PROLIFIC_URL"]
    COMPLETION_CODE = CONSTANTS["COMPLETION_CODE"]
    DROPOUT_CODE = CONSTANTS["DROPOUT_CODE"]
    QUALTRICS_URL = CONSTANTS["QUALTRICS_URL"]

class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass

class Player(BasePlayer):
    completion_code = models.StringField()
    datetime_finish = models.StringField()

############################################
# Pages
############################################

class Redirect(Page):
    @staticmethod
    def vars_for_template(player):
        player.datetime_finish = str(datetime.datetime.now())
        participant = player.participant
        
        # Determine completion code based on participant status
        if participant.is_bot:
            player.completion_code = C.DROPOUT_CODE
        elif participant.is_quiz_failed:
            player.completion_code = C.DROPOUT_CODE
        elif participant.is_dropout_self:
            player.completion_code = C.DROPOUT_CODE
        elif participant.is_matching_timeout:
            player.completion_code = C.COMPLETION_CODE
        elif participant.is_dropout_other:
            player.completion_code = C.COMPLETION_CODE
        else:
            # Normal completion
            player.completion_code = C.COMPLETION_CODE
        
        # Determine redirect URL
        # Send to Prolific for dropouts and timeouts (with appropriate completion codes)
        if (participant.is_bot or 
            participant.is_quiz_failed or 
            participant.is_dropout_self or 
            participant.is_matching_timeout):
            params = {"cc": player.completion_code}
            redirect_url = C.PROLIFIC_URL + "?" + urllib.parse.urlencode(params)
        else:
            # Send to Qualtrics for successful completion (normal or partner dropout)
            params = {
                "condition": player.session.config["condition"],
                "subject_id": player.participant.code
            }
            redirect_url = C.QUALTRICS_URL + "?" + urllib.parse.urlencode(params)
        
        return dict(redirect_url = redirect_url)

############################################
# Data and page_sequence
############################################

def custom_export(players):
    # For payment purposes
    yield [
        "subject_id", "datetime_finish", "prolific_id", "completion_code",
        "payoff_base", "payoff_task", "payoff_inconvenience", "payoff_bonus"
    ]
    for p in players:
        participant = p.participant
        yield [
            participant.code, p.datetime_finish, participant.label, p.completion_code,
            participant.payoff_base, participant.payoff_task, participant.payoff_inconvenience, participant.payoff
        ]

# This is the entire app
page_sequence = [Redirect]
