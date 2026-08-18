from os import environ
from otree.api import *
import json, time, pandas as pd, numpy as np
rng = np.random.default_rng()

options = pd.read_csv("_static/social_gambling/csv/options.csv")
practices = pd.read_csv("_static/social_gambling/csv/practices.csv")

def round_df(df):
    df["v_a_high"] = df["v_a_high"].round(1)
    df["v_a_low"] = df["v_a_low"].round(1)
    df["v_b_high"] = df["v_b_high"].round(1)
    df["v_b_low"] = df["v_b_low"].round(1)
    df["p_a_high"] = df["p_a_high"].round(1)
    df["p_a_low"] = df["p_a_low"].round(1)
    df["p_b_high"] = df["p_b_high"].round(1)
    df["p_b_low"] = df["p_b_low"].round(1)
    return df

options = round_df(options)
practices = round_df(practices)

CONSTANTS = {
    "PLAYERS_PER_GROUP": 5,
    "NUM_PRACTICE_ROUNDS": len(practices),
    "MAIN_TEMPLATE": "social_gambling/DefaultPage.html",
    "TASK_TEMPLATE": "social_gambling/TaskPage.html",
    "PRACTICES": practices,
    "OPTIONS": options,
    "NUM_REAL_ROUNDS": len(options) - 4,
    "NUM_ATTENTION_CHECKS": 4,
    "MAX_SAMPLE": 40,
    "TIMEOUT_MATCHING": 300,
    "TIMEOUT_MARGIN": 30,
    "TIMEOUT_START": 15,
    "PRACTICE_ROUND": "practice",
    "SOCIAL_CONDITION": "social",
    "PERSONAL_CONDITION": "personal",
    "POSITION_1": "ab",
    "POSITION_2": "ba",
    "ROUND_ATTENTION_1": 8,
    "ROUND_ATTENTION_2": 16,
    "ROUND_ATTENTION_3": 24,
    "ROUND_ATTENTION_4": 32,
    "PROLIFIC_URL": "https://app.prolific.com/submissions/complete",
    "COMPLETION_CODE": environ.get('PROLIFIC_COMPLETION_CODE', ''),
    "DROPOUT_CODE":    environ.get('PROLIFIC_DROPOUT_CODE', ''),
    "QUALTRICS_URL":   environ.get('QUALTRICS_URL', ''),
    "MAX_QUIZ_FAILURE": 2
}

# Make item
def make_item(label, choices, widget = widgets.RadioSelect, add_number = False):
    for i, choice in enumerate(choices):
        if add_number:
            choices[i] = [i + 1, str(i + 1) + ". " + choices[i]]
        else:
            choices[i] = [i + 1, choices[i]]
    return models.IntegerField(
        label = label,
        choices = choices,
        widget = widget
    )

# Check if the participant already saw the page using live_method
def check_started(player, data):
    if data == "started":
        player.participant.is_page_started = True

# Record the time when the participant finished the page
def record_time_last_page(player, timeout_happened):
    participant = player.participant
    participant.time_last_page = time.time()
    participant.is_page_started = False

# Shuffle practice rounds or real rounds (+ attention checks)
def shuffle_round_order(C):
    if C.IS_REAL_ROUND:
        while True:
            round_order = rng.permutation(np.arange(C.NUM_ROUNDS - C.NUM_ATTENTION_CHECKS)).astype(object)
            valid = True
            count = 1
            # Check whether there is a same position longer than 5
            for i in range(1, len(round_order)):
                if options["type_b"][round_order[i]] == options["type_b"][round_order[i - 1]]:
                    count += 1
                    if count > 5:
                        valid = False
                        break
                else:
                    count = 1
            if valid:
                break
        attention_order = rng.permutation([C.ROUND_ATTENTION_1 - 1, C.ROUND_ATTENTION_2 - 1, C.ROUND_ATTENTION_3 - 1, C.ROUND_ATTENTION_4 - 1])
        round_order = np.insert(round_order, C.ROUND_ATTENTION_1 - 1, -1)
        round_order = np.insert(round_order, C.ROUND_ATTENTION_2 - 1, -1)
        round_order = np.insert(round_order, C.ROUND_ATTENTION_3 - 1, -1)
        round_order = np.insert(round_order, C.ROUND_ATTENTION_4 - 1, -1)
        for counter, attention_trial in enumerate(attention_order):
            round_order[attention_trial] =  C.NUM_ROUNDS - counter - 1
    else:
        round_order = rng.permutation(np.arange(C.NUM_ROUNDS)).astype(object)
    return round_order

def shuffle_position_order(C):
    N = C.NUM_ROUNDS
    if N % 2 != 0:
        raise ValueError("N must be an even number")
    sequence = [C.POSITION_1] * (N // 2) + [C.POSITION_2] * (N // 2)
    # Shuffle until meeting the conditions
    while True:
        sequence = rng.permutation(sequence)
        valid = True
        count = 1
        # Check whether there is a same position longer than 5
        for i in range(1, len(sequence)):
            if sequence[i] == sequence[i - 1]:
                count += 1
                if count > 5:
                    valid = False
                    break
            else:
                count = 1
        if valid:
            break
    return sequence

# Set each round parameter before the task starts
def set_round_params(player, C, option_id):
    if C.IS_REAL_ROUND:
        option = C.OPTIONS.iloc[option_id]
    else:
        option = C.PRACTICES.iloc[option_id]
    seq_a = rng.choice(a = [option["v_a_high"], option["v_a_low"]],
                       size = C.MAX_SAMPLE,
                       p = [option["p_a_high"], option["p_a_low"]])
    seq_b = rng.choice(a = [option["v_b_high"], option["v_b_low"]],
                       size = C.MAX_SAMPLE,
                       p = [option["p_b_high"], option["p_b_low"]])
    seq_a = json.dumps(list(seq_a.astype(np.float64)))
    seq_b = json.dumps(list(seq_b.astype(np.float64)))
    player.option_id = option_id
    player.seq_a = seq_a
    player.seq_b = seq_b

def set_player_params(player, C, round_order):
    position_order = shuffle_position_order(C)
    player_in_all_rounds = player.in_rounds(1, C.NUM_ROUNDS)
    for round_index, player_in_round in enumerate(player_in_all_rounds):
        set_round_params(player_in_round, C, round_order[round_index])
        player_in_round.position = True if position_order[round_index] == C.POSITION_1 else False

def draw_lottery(choice, p, v_high, v_low):
    if choice == "miss":
        return 0
    else:
        if rng.random() < p:
            return v_high
        else:
            return v_low

def calculate_payoff(player, timeout_happened):
    
    session = player.session
    participant = player.participant
    participation_fee = float(session.config["participation_fee"])
    inconvenience_fee = session.config["inconvenience_fee"]
    exchange_rate = session.config["real_world_currency_per_point"]

    min_payoff_task = 0.5
    max_payoff_task = 2.5

    is_bot = participant.is_bot
    is_quiz_failed = participant.is_quiz_failed
    is_dropout_self = participant.is_dropout_self
    is_matching_timeout = participant.is_matching_timeout
    is_dropout_other = participant.is_dropout_other

    if is_bot:
        payoff_base = 0
        payoff_inconvenience = 0
        payoff_task = 0
    elif is_quiz_failed:
        payoff_base = 0
        payoff_inconvenience = 0
        payoff_task = 0
    elif is_dropout_self:
        payoff_base = 0
        payoff_inconvenience = 0
        payoff_task = 0
    elif is_matching_timeout:
        payoff_base = participation_fee
        payoff_inconvenience = inconvenience_fee
        payoff_task = 0
    elif is_dropout_other:
        payoff_base = participation_fee
        payoff_inconvenience = inconvenience_fee
        payoff_task = 0
    else:
        payoff_base = participation_fee
        payoff_inconvenience = 0
        payoff_task = 0
    
    # Calculate task payoff for participants who completed the task normally
    should_calculate_task_payoff = (not is_bot and not is_quiz_failed and 
                                   not is_dropout_self and not is_matching_timeout)
    
    if should_calculate_task_payoff:
        player_in_all_rounds = player.in_all_rounds()
        valid_rounds = [p for p in player_in_all_rounds if p.is_round_completed and p.subsession.is_attention_check == False]
        num_valid_rounds = len(valid_rounds)
        if num_valid_rounds == 0:
            payoff_task = 0
        else:
            if num_valid_rounds < 5:
                chosen_rounds = rng.choice(valid_rounds, num_valid_rounds)
            else:
                chosen_rounds = rng.choice(valid_rounds, 5)
            payoff_task = round(sum(round.payoff for round in chosen_rounds) * exchange_rate, 2)
            if payoff_task < min_payoff_task:
                payoff_task = min_payoff_task
            elif payoff_task > max_payoff_task:
                payoff_task = max_payoff_task

    participant.payoff_base = payoff_base
    participant.payoff_inconvenience = payoff_inconvenience
    participant.payoff_task = payoff_task
    participant.payoff = payoff_inconvenience + payoff_task

def go_to_prolific(player, upcoming_apps):
    return "social_gambling_redirect"
