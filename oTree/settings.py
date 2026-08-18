from os import environ

SESSION_CONFIGS = [
    dict(
        name = "individual_lottery",
        display_name = "Individual (sampling paradigm)",
        app_sequence = [
            "social_gambling_instructions",
            "social_gambling_practice",
            "social_gambling_task",
            "social_gambling_redirect",
        ],
        condition = "personal",
        doc = "Control condition: Single individual",
        # use_browser_bots = True
    ),
    dict(
        name = "pair_lottery",
        display_name = "Social (sampling paradigm)",
        app_sequence = [
            "social_gambling_instructions",
            "social_gambling_practice",
            "social_gambling_task",
            "social_gambling_redirect",
        ],
        condition = "social",
        doc = "Social condition: Two participants work on the task simultaneously."
    ),
]

# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = dict(
    num_demo_participants = 10,
    real_world_currency_per_point = 3/100,
    participation_fee = 4.5,
    inconvenience_fee = 1
)

PARTICIPANT_FIELDS = [
    # "finished",
    # "total_payoff",
    # "bonus",
    # "color_pos",
    # "last_page_arrival",
    # "dropout",
    # "other_dropout",
    # "other_timeout",
    "is_bot",
    "is_quiz_failed",
    "time_last_page",
    "is_dropout_self",
    "is_dropout_other",
    "is_matching_timeout",
    "is_page_started",
    "is_round_ready",
    "payoff_base",
    "payoff_task",
    "payoff_inconvenience"
]
SESSION_FIELDS = []

# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = "en"

# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = "GBP"
USE_POINTS = False

ROOMS = []

ADMIN_USERNAME = environ.get('OTREE_ADMIN_USERNAME', '')
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD', '')

DEMO_PAGE_INTRO_HTML = """
The list of KK's experiments
"""


OTREE_AUTH_LEVEL = "STUDY"
SECRET_KEY = environ.get('OTREE_SECRET_KEY', '')
INSTALLED_APPS = ['otree']
OTREE_PRODUCTION = 1

# Functions for multi-language
def import_lexicon(LANGUAGE_CODE, app_name):
    from importlib import import_module
    if LANGUAGE_CODE == 'de':
        m = import_module(app_name + ".lexicon_de")
    else:
        m = import_module(app_name + ".lexicon_en")
    Lexicon = m.Lexicon
    return Lexicon

def set_language(LANGUAGE_CODE):
    # this is the dict you should pass to each page in vars_for_template,
    # enabling you to do if-statements like {{ if de }} Nein {{ else }} No {{ endif }}
    which_language = {'en': False, 'de': False}  # noqa
    which_language[LANGUAGE_CODE[:2]] = True
    return which_language

# settings.py
RECAPTCHA_SITE_KEY = environ.get('RECAPTCHA_SITE_KEY', '')
RECAPTCHA_SECRET_KEY = environ.get('RECAPTCHA_SECRET_KEY', '')