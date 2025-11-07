% explanation_module.pl - Explanation and Justification Module
% Provides human-readable explanations of ethical reasoning

:- use_module(library(lists)).

% ============================================
% MAIN EXPLANATION GENERATOR
% ============================================

% Generate comprehensive explanation for a decision
explain_decision(DilemmaId, Framework, Action, Explanation) :-
    (   Framework = utilitarianism ->
        explain_utilitarian_decision(DilemmaId, Action, Explanation)
    ;   Framework = deontology ->
        explain_deontological_decision(DilemmaId, Action, Explanation)
    ;   Framework = virtue_ethics ->
        explain_virtue_ethics_decision(DilemmaId, Action, Explanation)
    ;   Framework = care_ethics ->
        explain_care_ethics_decision(DilemmaId, Action, Explanation)
    ;   Explanation = 'Framework not recognized'
    ).

% ============================================
% UTILITARIAN EXPLANATIONS
% ============================================

explain_utilitarian_decision(DilemmaId, Action, Explanation) :-
    % Get consequences
    findall(
        consequence_desc(Cons, Prob, Impact),
        (
            action_consequence(DilemmaId, Action, Cons, Prob),
            consequence_impact(Cons, ImpactType, Magnitude, Affected),
            format(atom(Impact), '~w impact on ~w people (magnitude: ~w)', 
                   [ImpactType, Affected, Magnitude])
        ),
        Consequences
    ),
    
    % Build explanation
    format(atom(Intro), 
           'From a utilitarian perspective, this action maximizes overall welfare. ', []),
    
    (   Consequences = [] ->
        ConseqText = 'The action has no significant consequences listed.'
    ;   format(atom(ConseqText), 
               'Key consequences include: ~w', [Consequences])
    ),
    
    format(atom(Conclusion),
           ' Utilitarianism judges actions by their outcomes, seeking the greatest good for the greatest number.',
           []),
    
    atom_concat(Intro, ConseqText, Temp),
    atom_concat(Temp, Conclusion, Explanation).

% ============================================
% DEONTOLOGICAL EXPLANATIONS
% ============================================

explain_deontological_decision(DilemmaId, Action, Explanation) :-
    % Check duties fulfilled
    findall(
        Duty,
        action_fulfills_duty(DilemmaId, Action, Duty),
        DutiesFulfilled
    ),
    
    % Check duties violated
    findall(
        Duty,
        action_violates_duty(DilemmaId, Action, Duty),
        DutiesViolated
    ),
    
    % Check autonomy
    (   action_respects_autonomy(DilemmaId, Action) ->
        AutonomyText = 'This action respects individual autonomy. '
    ;   AutonomyText = 'This action may compromise autonomy. '
    ),
    
    % Build explanation
    format(atom(Intro),
           'From a deontological perspective, this action aligns with moral duties. ', []),
    
    (   DutiesFulfilled = [] ->
        FulfilledText = ''
    ;   format(atom(FulfilledText),
               'It fulfills duties: ~w. ', [DutiesFulfilled])
    ),
    
    (   DutiesViolated = [] ->
        ViolatedText = ''
    ;   format(atom(ViolatedText),
               'However, it violates: ~w. ', [DutiesViolated])
    ),
    
    Conclusion = 'Deontology emphasizes following moral rules and treating people with respect, regardless of outcomes.',
    
    atom_concat(Intro, FulfilledText, T1),
    atom_concat(T1, ViolatedText, T2),
    atom_concat(T2, AutonomyText, T3),
    atom_concat(T3, Conclusion, Explanation).

% ============================================
% VIRTUE ETHICS EXPLANATIONS
% ============================================

explain_virtue_ethics_decision(DilemmaId, Action, Explanation) :-
    % Find virtues embodied
    findall(
        Virtue,
        action_embodies_virtue_in_context(DilemmaId, Action, Virtue),
        Virtues
    ),
    
    % Find vices embodied
    findall(
        Vice,
        action_embodies_vice_in_context(DilemmaId, Action, Vice),
        Vices
    ),
    
    % Build explanation
    format(atom(Intro),
           'From a virtue ethics perspective, this action reflects good character. ', []),
    
    (   Virtues = [] ->
        VirtueText = 'No specific virtues are prominently displayed. '
    ;   format(atom(VirtueText),
               'It embodies virtues such as: ~w. ', [Virtues])
    ),
    
    (   Vices = [] ->
        ViceText = ''
    ;   format(atom(ViceText),
               'However, it may reflect: ~w. ', [Vices])
    ),
    
    (   action_shows_practical_wisdom(DilemmaId, Action) ->
        WisdomText = 'The action demonstrates practical wisdom (phronesis) in navigating this complex situation. '
    ;   WisdomText = ''
    ),
    
    Conclusion = 'Virtue ethics focuses on what a virtuous person would do, emphasizing character over rules or consequences.',
    
    atom_concat(Intro, VirtueText, T1),
    atom_concat(T1, ViceText, T2),
    atom_concat(T2, WisdomText, T3),
    atom_concat(T3, Conclusion, Explanation).

% ============================================
% CARE ETHICS EXPLANATIONS
% ============================================

explain_care_ethics_decision(DilemmaId, Action, Explanation) :-
    % Check relationship impact
    (   action_preserves_relationships(DilemmaId, Action) ->
        RelText = 'This action helps maintain important relationships. '
    ;   action_damages_relationships(DilemmaId, Action) ->
        RelText = 'This action may strain relationships, but may be necessary. '
    ;   RelText = ''
    ),
    
    % Check needs addressed
    findall(
        Need,
        action_addresses_need(DilemmaId, Action, Need),
        Needs
    ),
    
    % Check vulnerability
    (   action_protects_vulnerable(DilemmaId, Action) ->
        VulnText = 'It shows special care for vulnerable individuals. '
    ;   VulnText = ''
    ),
    
    % Build explanation
    format(atom(Intro),
           'From a care ethics perspective, this action demonstrates attentiveness to relationships and needs. ',
           []),
    
    (   Needs = [] ->
        NeedText = ''
    ;   format(atom(NeedText),
               'It responds to needs: ~w. ', [Needs])
    ),
    
    Conclusion = 'Care ethics emphasizes empathy, relationships, and responding to the particular needs of those in our care.',
    
    atom_concat(Intro, RelText, T1),
    atom_concat(T1, NeedText, T2),
    atom_concat(T2, VulnText, T3),
    atom_concat(T3, Conclusion, Explanation).

% ============================================
% REASONING TRACE GENERATOR
% ============================================

% Generate step-by-step reasoning trace
generate_reasoning_trace(DilemmaId, Framework, Action) :-
    write('Step 1: Identify ethical framework principles'), nl,
    list_framework_principles(Framework),
    nl,
    
    write('Step 2: Analyze action properties'), nl,
    list_action_properties(DilemmaId, Action),
    nl,
    
    write('Step 3: Evaluate consequences and impacts'), nl,
    list_consequences(DilemmaId, Action),
    nl,
    
    write('Step 4: Apply framework-specific reasoning'), nl,
    apply_framework_reasoning(DilemmaId, Framework, Action),
    nl,
    
    write('Step 5: Calculate ethical score'), nl,
    evaluate_action(DilemmaId, Framework, Action, Score),
    format('Final Score: ~w~n', [Score]).

% List principles of framework
list_framework_principles(Framework) :-
    forall(
        principle(Framework, Principle, Description),
        format('  - ~w: ~w~n', [Principle, Description])
    ).

% List action properties
list_action_properties(DilemmaId, Action) :-
    forall(
        action_property(DilemmaId, Action, Property, Value),
        format('  - ~w: ~w~n', [Property, Value])
    ).

% List consequences
list_consequences(DilemmaId, Action) :-
    forall(
        action_consequence(DilemmaId, Action, Consequence, Probability),
        format('  - ~w (probability: ~w)~n', [Consequence, Probability])
    ).

% Apply framework-specific reasoning display
apply_framework_reasoning(DilemmaId, utilitarianism, Action) :-
    write('  Utilitarian analysis:'), nl,
    findall(
        Impact,
        (
            action_consequence(DilemmaId, Action, Cons, _),
            consequence_impact(Cons, ImpactType, Mag, Affected),
            format(atom(Impact), '    ~w affecting ~w people (magnitude ~w)',
                   [ImpactType, Affected, Mag])
        ),
        Impacts
    ),
    (   Impacts = [] ->
        write('    No specific impacts defined'), nl
    ;   forall(member(I, Impacts), (write(I), nl))
    ).

apply_framework_reasoning(DilemmaId, deontology, Action) :-
    write('  Deontological analysis:'), nl,
    (   action_respects_autonomy(DilemmaId, Action) ->
        write('    ✓ Respects autonomy'), nl
    ;   write('    ✗ May violate autonomy'), nl
    ),
    (   action_treats_as_ends(DilemmaId, Action) ->
        write('    ✓ Treats people as ends'), nl
    ;   write('    ✗ May use people as means'), nl
    ),
    forall(
        action_fulfills_duty(DilemmaId, Action, Duty),
        format('    ✓ Fulfills duty: ~w~n', [Duty])
    ),
    forall(
        action_violates_duty(DilemmaId, Action, Duty),
        format('    ✗ Violates duty: ~w~n', [Duty])
    ).

apply_framework_reasoning(DilemmaId, virtue_ethics, Action) :-
    write('  Virtue ethics analysis:'), nl,
    forall(
        action_embodies_virtue_in_context(DilemmaId, Action, Virtue),
        format('    ✓ Embodies virtue: ~w~n', [Virtue])
    ),
    forall(
        action_embodies_vice_in_context(DilemmaId, Action, Vice),
        format('    ✗ Embodies vice: ~w~n', [Vice])
    ),
    (   action_shows_practical_wisdom(DilemmaId, Action) ->
        write('    ✓ Shows practical wisdom'), nl
    ;   write('    ○ Wisdom unclear'), nl
    ).

apply_framework_reasoning(DilemmaId, care_ethics, Action) :-
    write('  Care ethics analysis:'), nl,
    (   action_preserves_relationships(DilemmaId, Action) ->
        write('    ✓ Preserves relationships'), nl
    ;   action_damages_relationships(DilemmaId, Action) ->
        write('    ✗ Damages relationships'), nl
    ;   write('    ○ Relationship impact unclear'), nl
    ),
    forall(
        action_addresses_need(DilemmaId, Action, Need),
        format('    ✓ Addresses need: ~w~n', [Need])
    ),
    (   action_protects_vulnerable(DilemmaId, Action) ->
        write('    ✓ Protects vulnerable'), nl
    ;   true
    ).

% ============================================
% COMPARATIVE ANALYSIS
% ============================================

% Compare how different frameworks evaluate the same action
comparative_analysis(DilemmaId, Action) :-
    write('=== COMPARATIVE FRAMEWORK ANALYSIS ==='), nl, nl,
    
    forall(
        framework(Framework),
        (
            format('~w:~n', [Framework]),
            (   recommend_action(DilemmaId, Framework, Action, Score) ->
                format('  Score: ~w~n', [Score]),
                explain_decision(DilemmaId, Framework, Action, Explanation),
                format('  Reasoning: ~w~n~n', [Explanation])
            ;   write('  Unable to evaluate~n~n')
            )
        )
    ).