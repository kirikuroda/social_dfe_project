from otree.api import *
import json, random, social_gambling_task.experiment_settings as exp_settings
CONSTANTS = exp_settings.CONSTANTS

doc = "Post-session questionnaire"

class C(BaseConstants):
    NAME_IN_URL = "questionnaire"
    MAIN_TEMPLATE = CONSTANTS["MAIN_TEMPLATE"]
    NUM_ROUNDS = 1
    PLAYERS_PER_GROUP = None
    SOCIAL_CONDITION = CONSTANTS["SOCIAL_CONDITION"]
    PERSONAL_CONDITION = CONSTANTS["PERSONAL_CONDITION"]

class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass

class Player(BasePlayer):
    strategy = models.LongStringField(
        label = "Did you have any strategies for the task? If so, please feel free to write them in the space below.",
        blank = True
    )
    social_info_use = exp_settings.make_item(
        label = "Did you consider Player A's choice when deciding which lottery to play?",
        choices = ["Not at all", "Hardly at all", "Slightly", "Neutral", "Somewhat", "Quite a lot", "Very much"],
        add_number = True
    )
    choose_after_partner = exp_settings.make_item(
        label = "Did you want to wait to see Player A's choice before playing a lottery?",
        choices = ["Not at all", "Hardly at all", "Slightly", "Neutral", "Somewhat", "Quite a lot", "Very much"],
        add_number = True
    )
    delay_choice = exp_settings.make_item(
        label = "Did you intentionally delay your choice to observe Player A's choice?",
        choices = ["Not at all", "Hardly at all", "Slightly", "Neutral", "Somewhat", "Quite a lot", "Very much"],
        add_number = True
    )
    choice_timing = exp_settings.make_item(
        label = "Did you want to answer earlier or later than Player A?",
        choices = ["Much earlier", "Earlier", "Somewhat earlier", "Neutral", "Somewhat later", "Later", "Much later"],
        add_number = True
    )
    suspicion_partner = exp_settings.make_item(
        label = "Did you feel that Player A was a real participant or a bot (= computer program) during the task? (You were actually paired with another participant, but please answer how you felt during the task.)",
        choices = [
            "Definitely a real participant", "Mostly a real participant", "Somewhat a real participant",
            "Unsure", "Somewhat a bot", "Mostly a bot", "Definitely a bot"
        ],
        add_number = True
    )
    age = models.IntegerField(label = "What is your age?", min = 15, max = 99)
    gender = exp_settings.make_item(
        label = "What is your gender?",
        choices = [
            "Man", "Woman", "Non-binary / Third gender", "Prefer not to say", "Prefer to self-describe, below"
        ],
    )
    gender_self_describe = models.StringField(label = "", blank = True)
    item_order = models.StringField()
    comment = models.LongStringField(
        label = "If you have any comments on the study, please write them in the space below.",
        blank = True
    )

def creating_session(subsession):
    # Randomize the item order
    players = subsession.get_players()
    if subsession.session.config["condition"] == C.SOCIAL_CONDITION:
        for player in players:
            item_order = [0,1,2,3]
            player.item_order = str(random.sample(item_order, len(item_order)))
    else:
        for player in players:
            player.item_order = str([])

############################################
# Pages
############################################

class Questionnaire(Page):

    form_model = "player"
    
    @staticmethod
    def get_form_fields(player):
        items = ["social_info_use", "choose_after_partner", "delay_choice", "choice_timing", "suspicion_partner"]
        item_order = json.loads(player.item_order)
        if player.session.config["condition"] == C.SOCIAL_CONDITION:
            form_fields = ["strategy"]
            if player.participant.is_dropout_other:
                form_fields.extend(["age", "gender", "gender_self_describe", "comment"])
            else:
                for i in item_order:
                    form_fields.append(items[i])
                form_fields.extend(["suspicion_partner", "age", "gender", "gender_self_describe", "comment"])
        else:
            form_fields = ["strategy", "age", "gender", "gender_self_describe", "comment"]
        return form_fields

class FinishQuestionnaire(Page):
    pass

############################################
# Data and page_sequence
############################################

def custom_export(players):
    yield [
        "subj_id", "strategy", "social_info_use", "choose_after_partner", "delay_choice",
        "choice_timing", "suspicion_partner", "age", "gender", "gender_self_describe", "item_order", "comment"
    ]
    for p in players:
        participant = p.participant
        yield [
            participant.code, p.strategy, p.social_info_use, p.choose_after_partner, p.delay_choice,
            p.choice_timing, p.suspicion_partner, p.age, p.gender, p.gender_self_describe, p.item_order, p.comment
        ]

# This is the entire app
page_sequence = [
    Questionnaire,
    FinishQuestionnaire
]
