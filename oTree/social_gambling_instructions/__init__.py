import time
import datetime
import requests
from otree.api import *
from otree.settings import RECAPTCHA_SECRET_KEY, RECAPTCHA_SITE_KEY
import social_gambling_task.experiment_settings as exp_settings
CONSTANTS = exp_settings.CONSTANTS

doc = "Consent + Instructions"

class C(BaseConstants):
    NAME_IN_URL = "instructions"
    MAIN_TEMPLATE = CONSTANTS["MAIN_TEMPLATE"]
    NUM_ROUNDS = 1
    PLAYERS_PER_GROUP = None
    SOCIAL_CONDITION = CONSTANTS["SOCIAL_CONDITION"]
    PERSONAL_CONDITION = CONSTANTS["PERSONAL_CONDITION"]
    MAX_QUIZ_FAILURE = CONSTANTS["MAX_QUIZ_FAILURE"]

class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass

class Player(BasePlayer):
    # Prolific ID and verification
    welcome_prolific_id = models.StringField(label="")
    welcome_check = models.StringField(blank=True)
    
    # Anti-bot verification fields
    color_confirm = models.StringField(blank=True)
    food_confirm = models.StringField(blank=True)
    place_confirm = models.StringField(blank=True)
    
    # Consent tracking
    consent_participation = models.BooleanField(initial=False)
    consent_data = models.BooleanField(initial=False)
    datetime_start = models.StringField()
    
    # Quiz performance tracking
    count_quiz_failure = models.IntegerField(initial=0)
    
    # Quiz questions (configured based on condition)
    quiz_1 = exp_settings.make_item(
        label="You can try out lotteries by... (Hint: p.9)",
        choices=[
            "pressing the L or R keys.",
            "pressing the space bar or the Enter key.",
            "pressing the left or right arrow keys.",
            "left-clicking or right-clicking.",
        ]
    )
    quiz_2 = exp_settings.make_item(
        label="A lottery... (Hint: p.13)",
        choices=[
            "always shows the same points.",
            "does not necessarily show the same points every time you try it.",
            "never shows the same points.",
            "never affects your bonus because the prize is points, not real money.",
        ]
    )
    quiz_3 = exp_settings.make_item(
        label="Once you have decided which lottery you want to play, you should... (Hint: p.17)",
        choices=[
            "stop pressing the keys and click on the lottery you want to play.",
            "stop pressing the keys and click on the lottery you do NOT want to play.",
            "stop pressing the keys and wait until the next round begins automatically.",
            "keep pressing the keys and trying out the lotteries.",
        ]
    )
    quiz_4 = exp_settings.make_item(
        label="What happens if you do not respond within the maximum time limit of 15 seconds (= until the timer reaches zero)? (Hint: p.21)",
        choices=[
            "The next round will begin automatically.",
            "A message will prompt you to respond again.",
            "You will be considered to have dropped out and will not receive any reward.",
            "You will have to do the task again from the beginning.",
        ]
    )
    quiz_5 = exp_settings.make_item(
        label="You and the other four players will try out... (Hint: pp.24-25)",
        choices=[
            "different lotteries. You need to wait to see the prize until everyone tries or plays a lottery.",
            "the same lotteries. You need to wait to see the prize until everyone tries or plays a lottery.",
            "the same lotteries. You do not need to wait to see the prize until everyone tries or plays a lottery.",
            "the same lotteries to compete against each other.",
        ]
    )

def creating_session(subsession):
    # Initialize participant fields
    players = subsession.get_players()
    for player in players:
        participant = player.participant
        participant.is_bot = False # change to True for testing
        participant.is_quiz_failed = False
        participant.time_last_page = time.time()
        participant.is_dropout_self = False
        participant.is_dropout_other = False
        participant.is_matching_timeout = False
        participant.is_page_started = False
        participant.payoff_base = 0
        participant.payoff_task = 0
        participant.payoff_inconvenience = 0

def recaptcha_valid(response_token):
    try:
        res = requests.post("https://www.google.com/recaptcha/api/siteverify", data = {
            "secret": RECAPTCHA_SECRET_KEY,
            "response": response_token
        })
        result = res.json()
        return result.get("success", False)
    except (requests.RequestException, ValueError, KeyError):
        return False

# Helper functions for quiz validation
def get_quiz_solutions_and_messages(condition):
    """Return quiz solutions and error messages based on condition."""
    solutions = {
        "quiz_2": 2, 
        "quiz_3": 1, 
        "quiz_4": 3
    }
    messages = {
        "quiz_2": "Incorrect: Playing the same lottery again may result in a different prize. So, a lottery will not necessarily show the same points every time you try it. The prizes become your bonus.",
        "quiz_3": "Incorrect: When you have tried out the lotteries enough, stop pressing the keys and click on the lottery you want to play.",
        "quiz_4": "Incorrect: If you do not respond within 15 seconds, you will be considered to have dropped and will not receive any reward.",
    }
    
    if condition == C.SOCIAL_CONDITION:
        solutions["quiz_5"] = 2
        messages["quiz_5"] = "Incorrect: While you are trying out the lotteries, the other players are also trying out the same lotteries at the same pace. You need to wait until everyone tries or plays a lottery. You and the other players are not in competition."
    else:
        solutions["quiz_1"] = 3
        messages["quiz_1"] = "Incorrect: To try out the left (right) lottery, press the left (right) arrow key."
    
    return solutions, messages

def validate_quiz_answers(player, values, solutions, messages):
    """Validate quiz answers and return errors if any."""
    errors = {f: messages[f] if values[f] != solutions[f] else "Correct!" for f in solutions}
    n_correct = sum(1 for f in solutions if errors[f] == "Correct!")
    
    if n_correct < len(errors):
        player.count_quiz_failure += 1
        if player.count_quiz_failure >= C.MAX_QUIZ_FAILURE:
            player.participant.is_quiz_failed = True
        else:
            return errors
    return None

############################################
# Pages
############################################

class ProlificID(Page):

    form_model = "player"
    form_fields = ["welcome_prolific_id", "welcome_check"]
    
    @staticmethod
    def js_vars(player):
        return dict(prolific_id = player.participant.label)

    @staticmethod
    def error_message(player, values):
        if values["welcome_prolific_id"] == '':
            return 'Please answer this question.'
        elif values["welcome_prolific_id"] != player.participant.label:
            return "Please enter the correct Prolific ID."

class Captcha(Page):

    form_model = "player"
    form_fields = ["color_confirm", "food_confirm", "place_confirm"]

    @staticmethod
    def vars_for_template(player):
        return {
            "RECAPTCHA_SITE_KEY": RECAPTCHA_SITE_KEY
        }

    @staticmethod
    def live_method(player, data):
        if recaptcha_valid(data["response_token"]):
            player.participant.is_bot = False
            return {player.id_in_group: "valid"}

    @staticmethod
    def error_message(player, values):
        if player.participant.is_bot:
            return "You did not solve the CAPTCHA."
    
    @staticmethod
    def before_next_page(player, timeout_happened):
        if not player.color_confirm and not player.food_confirm and not player.place_confirm:
            player.participant.is_bot = False
        else:
            player.participant.is_bot = True

class Consent(Page):

    @staticmethod
    def is_displayed(player):
        return player.participant.is_bot == False

    @staticmethod
    def before_next_page(player, timeout_happened):
        player.datetime_start = str(datetime.datetime.now())
        player.consent_participation = True
        player.consent_data = True

class Bot(Page):

    before_next_page = exp_settings.calculate_payoff
    app_after_this_page = exp_settings.go_to_prolific

    @staticmethod
    def is_displayed(player):
        return player.participant.is_bot

class Fullscreen(Page):
    pass

class Instructions(Page):
    
    form_model = "player"
    
    @staticmethod
    def get_form_fields(player):
        if player.session.config["condition"] == C.SOCIAL_CONDITION:
            return ["quiz_2", "quiz_3", "quiz_4", "quiz_5"]
        else:
            return ["quiz_1", "quiz_2", "quiz_3", "quiz_4"]
    
    @staticmethod
    def error_message(player, values):
        solutions, messages = get_quiz_solutions_and_messages(player.session.config["condition"])
        errors = validate_quiz_answers(player, values, solutions, messages)
        return errors
    
    @staticmethod
    def js_vars(player):
        return dict(condition = player.session.config["condition"])

# Failed comprehension checks
class FinishInstructions(Page):

    before_next_page = exp_settings.calculate_payoff
    app_after_this_page = exp_settings.go_to_prolific

    @staticmethod
    def is_displayed(player):
        return player.participant.is_quiz_failed


############################################
# Data and page_sequence
############################################

def custom_export(players):
    # Export consent, quiz, and condition
    yield [
        "subject_id", "welcome_prolific_id", "welcome_check", "condition", "datetime_start", "session_id",
        "is_bot", "color_confirm", "food_confirm", "place_confirm",
        "consent_participation", "consent_data", "count_quiz_failure", "is_quiz_failed",
        "quiz_1", "quiz_2", "quiz_3", "quiz_4", "quiz_5",
        "is_matching_timeout", "is_dropout_self", "is_dropout_other",
    ]
    for p in players:
        session = p.session
        participant = p.participant
        yield [
            participant.code, p.welcome_prolific_id, p.welcome_check, session.config["condition"], p.datetime_start, session.code,
            participant.is_bot, p.color_confirm, p.food_confirm, p.place_confirm,
            p.consent_participation, p.consent_data, p.count_quiz_failure, participant.is_quiz_failed,
            p.quiz_1, p.quiz_2, p.quiz_3, p.quiz_4, p.quiz_5,
            participant.is_matching_timeout, participant.is_dropout_self, participant.is_dropout_other
        ]

# This is the entire app
page_sequence = [
    # ProlificID,
    # Captcha,
    # Consent,
    # Bot,
    # Fullscreen,
    Instructions,
    FinishInstructions
]
