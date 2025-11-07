% reasoning_engine.pl - Core Ethical Reasoning and Inference
% Contains the logic for evaluating actions according to different ethical frameworks

:- use_module(library(lists)).

% ============================================
% MAIN RECOMMENDATION ENGINE
% ============================================

% Recommend the best action for a dilemma under a specific framework
recommend_action(DilemmaId, Framework, BestAction, BestScore) :-
    dilemma(DilemmaId, _, _, PossibleActions),
    findall(
        score(Action, Score),
        (
            member(Action, PossibleActions),
            evaluate_action(DilemmaId, Framework, Action, Score)
        ),
        ScoredActions
    ),
    ScoredActions \= [],
    find_best_action(ScoredActions, BestAction, BestScore).

% Find the action with the highest score
find_best_action([score(A, S)], A, S).
find_best_action([score(A1, S1), score(A2, S2)|Rest], BestAction, BestScore) :-
    (   S1 >= S2 ->
        find_best_action([score(A1, S1)|Rest], BestAction, BestScore)
    ;   find_best_action([score(A2, S2)|Rest], BestAction, BestScore)
    ).

% ============================================
% ACTION EVALUATION BY FRAMEWORK
% ============================================

% Main evaluation predicate - delegates to framework-specific evaluators
evaluate_action(DilemmaId, Framework, Action, Score) :-
    (   Framework = utilitarianism ->
        evaluate_utilitarian(DilemmaId, Action, Score)
    ;   Framework = deontology ->
        evaluate_deontological(DilemmaId, Action, Score)
    ;   Framework = virtue_ethics ->
        evaluate_virtue_ethics(DilemmaId, Action, Score)
    ;   Framework = care_ethics ->
        evaluate_care_ethics(DilemmaId, Action, Score)
    ;   Score = 0  % Default if framework not recognized
    ).

% ============================================
% UTILITARIAN EVALUATION
% ============================================

% Evaluate action based on consequentialist utility calculation
evaluate_utilitarian(DilemmaId, Action, Score) :-
    % Check for absolute prohibitions first
    (   action_violates_absolute_prohibition(DilemmaId, Action) ->
        Score = -1000
    ;   % Calculate utility based on consequences
        findall(
            Utility,
            (
                action_consequence(DilemmaId, Action, Consequence, Probability),
                consequence_utility(Consequence, BaseUtility),
                Utility is BaseUtility * Probability
            ),
            Utilities
        ),
        (   Utilities = [] ->
            Score = 0
        ;   sum_list(Utilities, TotalUtility),
            length(Utilities, N),
            Score is TotalUtility / N
        )
    ).

% Calculate utility of a consequence
consequence_utility(Consequence, Utility) :-
    (   consequence_impact(Consequence, Impact, Magnitude, Affected) ->
        (   Impact = positive ->
            Utility is Magnitude * Affected
        ;   Impact = negative ->
            Utility is -(Magnitude * Affected)
        )
    ;   Utility = 0
    ).

% ============================================
% DEONTOLOGICAL EVALUATION
% ============================================

% Evaluate action based on duties, rules, and respect for persons
evaluate_deontological(DilemmaId, Action, Score) :-
    % Check for duty violations
    findall(
        Violation,
        (
            duty(Duty, _),
            action_violates_duty(DilemmaId, Action, Duty),
            Violation = -50
        ),
        Violations
    ),
    
    % Check for duty fulfillments
    findall(
        Fulfillment,
        (
            duty(Duty, _),
            action_fulfills_duty(DilemmaId, Action, Duty),
            Fulfillment = 30
        ),
        Fulfillments
    ),
    
    % Check autonomy respect
    (   action_respects_autonomy(DilemmaId, Action) ->
        AutonomyScore = 40
    ;   AutonomyScore = -40
    ),
    
    % Check if action treats people as ends
    (   action_treats_as_ends(DilemmaId, Action) ->
        EndsScore = 40
    ;   EndsScore = -40
    ),
    
    % Calculate total
    sum_list(Violations, ViolationTotal),
    sum_list(Fulfillments, FulfillmentTotal),
    BaseScore is FulfillmentTotal + ViolationTotal + AutonomyScore + EndsScore,
    
    % Apply severe penalty for absolute prohibitions
    (   action_violates_absolute_prohibition(DilemmaId, Action) ->
        Score = -1000
    ;   Score = BaseScore
    ).

% ============================================
% VIRTUE ETHICS EVALUATION
% ============================================

% Evaluate action based on virtues and character
evaluate_virtue_ethics(DilemmaId, Action, Score) :-
    % Find all virtues embodied by the action
    findall(
        VirtueScore,
        (
            action_embodies_virtue_in_context(DilemmaId, Action, Virtue),
            virtue_weight(Virtue, Weight),
            VirtueScore is Weight * 10
        ),
        VirtueScores
    ),
    
    % Find all vices embodied by the action
    findall(
        ViceScore,
        (
            action_embodies_vice_in_context(DilemmaId, Action, Vice),
            vice_weight(Vice, Weight),
            ViceScore is -(Weight * 10)
        ),
        ViceScores
    ),
    
    % Consider practical wisdom (phronesis)
    (   action_shows_practical_wisdom(DilemmaId, Action) ->
        WisdomScore = 30
    ;   WisdomScore = 0
    ),
    
    % Calculate total
    sum_list(VirtueScores, VirtueTotal),
    sum_list(ViceScores, ViceTotal),
    Score is VirtueTotal + ViceTotal + WisdomScore.

% Default virtue weights
virtue_weight(courage, 8).
virtue_weight(compassion, 9).
virtue_weight(honesty, 8).
virtue_weight(justice, 10).
virtue_weight(wisdom, 9).
virtue_weight(temperance, 7).
virtue_weight(integrity, 9).

% Default vice weights
vice_weight(cowardice, 8).
vice_weight(callousness, 9).
vice_weight(dishonesty, 8).
vice_weight(injustice, 10).
vice_weight(recklessness, 7).
vice_weight(selfishness, 8).

% ============================================
% CARE ETHICS EVALUATION
% ============================================

% Evaluate action based on relationships, care, and context
evaluate_care_ethics(DilemmaId, Action, Score) :-
    % Evaluate relationship preservation
    (   action_preserves_relationships(DilemmaId, Action) ->
        RelationshipScore = 40
    ;   action_damages_relationships(DilemmaId, Action) ->
        RelationshipScore = -40
    ;   RelationshipScore = 0
    ),
    
    % Evaluate responsiveness to needs
    findall(
        NeedScore,
        (
            stakeholder_need(DilemmaId, _, Need, Urgency),
            action_addresses_need(DilemmaId, Action, Need),
            NeedScore is Urgency * 10
        ),
        NeedScores
    ),
    
    % Evaluate vulnerability consideration
    (   action_protects_vulnerable(DilemmaId, Action) ->
        VulnerabilityScore = 40
    ;   VulnerabilityScore = 0
    ),
    
    % Evaluate contextual appropriateness
    (   action_contextually_appropriate(DilemmaId, Action) ->
        ContextScore = 30
    ;   ContextScore = 0
    ),
    
    % Calculate total
    sum_list(NeedScores, NeedTotal),
    Score is RelationshipScore + NeedTotal + VulnerabilityScore + ContextScore.

% ============================================
% HELPER PREDICATES FOR EVALUATION
% ============================================

% Check if action violates absolute moral prohibition
action_violates_absolute_prohibition(DilemmaId, Action) :-
    action_property(DilemmaId, Action, violates_prohibition, true).

% Check if action respects autonomy
action_respects_autonomy(DilemmaId, Action) :-
    action_property(DilemmaId, Action, respects_autonomy, true).

% Check if action treats people as ends
action_treats_as_ends(DilemmaId, Action) :-
    \+ action_property(DilemmaId, Action, uses_as_means, true).

% Check if action violates a duty
action_violates_duty(DilemmaId, Action, Duty) :-
    action_property(DilemmaId, Action, violates_duty, Duty).

% Check if action fulfills a duty
action_fulfills_duty(DilemmaId, Action, Duty) :-
    action_property(DilemmaId, Action, fulfills_duty, Duty).

% Check if action embodies virtue in context
action_embodies_virtue_in_context(DilemmaId, Action, Virtue) :-
    action_property(DilemmaId, Action, embodies_virtue, Virtue).

% Check if action embodies vice in context
action_embodies_vice_in_context(DilemmaId, Action, Vice) :-
    action_property(DilemmaId, Action, embodies_vice, Vice).

% Check if action shows practical wisdom
action_shows_practical_wisdom(DilemmaId, Action) :-
    action_property(DilemmaId, Action, shows_wisdom, true).

% Check if action preserves relationships
action_preserves_relationships(DilemmaId, Action) :-
    action_property(DilemmaId, Action, preserves_relationships, true).

% Check if action damages relationships
action_damages_relationships(DilemmaId, Action) :-
    action_property(DilemmaId, Action, damages_relationships, true).

% Check if action addresses a need
action_addresses_need(DilemmaId, Action, Need) :-
    action_property(DilemmaId, Action, addresses_need, Need).

% Check if action protects vulnerable
action_protects_vulnerable(DilemmaId, Action) :-
    action_property(DilemmaId, Action, protects_vulnerable, true).

% Check if action is contextually appropriate
action_contextually_appropriate(DilemmaId, Action) :-
    action_property(DilemmaId, Action, contextually_appropriate, true).

% ============================================
% COMMON DUTIES (for deontological reasoning)
% ============================================

duty(preserve_life, 'Duty to protect human life').
duty(tell_truth, 'Duty to be honest').
duty(keep_promises, 'Duty to honor commitments').
duty(respect_autonomy, 'Duty to respect freedom of choice').
duty(help_others, 'Duty to aid those in need').
duty(avoid_harm, 'Duty not to cause harm').
duty(be_fair, 'Duty to treat people justly').