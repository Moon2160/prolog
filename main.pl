% main.pl - Ethical AI Advisor Entry Point
% This is the main interface for the Ethical AI Advisor system

:- consult('knowledge_base.pl').
:- consult('reasoning_engine.pl').
:- consult('explanation_module.pl').
:- consult('dilemmas.pl').

% Main entry point
start :-
    write('================================================='), nl,
    write('   ETHICAL AI ADVISOR - Moral Reasoning System   '), nl,
    write('================================================='), nl, nl,
    show_menu.

% Main menu
show_menu :-
    write('Please select an option:'), nl,
    write('1. Analyze a predefined dilemma'), nl,
    write('2. List all ethical frameworks'), nl,
    write('3. Compare frameworks on a dilemma'), nl,
    write('4. View ethical principles'), nl,
    write('5. Exit'), nl,
    write('Enter your choice (1-5): '),
    read(Choice),
    handle_choice(Choice).

% Handle menu choices
handle_choice(1) :- 
    analyze_predefined_dilemma,
    show_menu.

handle_choice(2) :- 
    list_frameworks,
    show_menu.

handle_choice(3) :- 
    compare_frameworks_menu,
    show_menu.

handle_choice(4) :- 
    view_principles,
    show_menu.

handle_choice(5) :- 
    write('Thank you for using Ethical AI Advisor!'), nl,
    halt.

handle_choice(_) :- 
    write('Invalid choice. Please try again.'), nl, nl,
    show_menu.

% Analyze a predefined dilemma
analyze_predefined_dilemma :-
    nl, write('Available Dilemmas:'), nl,
    list_dilemmas,
    write('Enter dilemma ID: '),
    read(DilemmaId),
    write('Select ethical framework:'), nl,
    list_frameworks,
    write('Enter framework name: '),
    read(Framework),
    nl,
    analyze_dilemma(DilemmaId, Framework).

% List all available dilemmas
list_dilemmas :-
    forall(
        dilemma(Id, Name, _, _),
        (write('  '), write(Id), write(': '), write(Name), nl)
    ).

% List all ethical frameworks
list_frameworks :-
    nl, write('Available Ethical Frameworks:'), nl,
    write('  - utilitarianism: Maximize overall happiness/utility'), nl,
    write('  - deontology: Follow moral duties and rules'), nl,
    write('  - virtue_ethics: Act according to virtues'), nl,
    write('  - care_ethics: Prioritize relationships and care'), nl, nl.

% View all ethical principles
view_principles :-
    nl, write('=== ETHICAL PRINCIPLES ==='), nl, nl,
    write('Utilitarian Principles:'), nl,
    forall(
        principle(utilitarianism, P, D),
        (write('  - '), write(P), write(': '), write(D), nl)
    ),
    nl,
    write('Deontological Principles:'), nl,
    forall(
        principle(deontology, P, D),
        (write('  - '), write(P), write(': '), write(D), nl)
    ),
    nl,
    write('Virtue Ethics Principles:'), nl,
    forall(
        principle(virtue_ethics, P, D),
        (write('  - '), write(P), write(': '), write(D), nl)
    ),
    nl,
    write('Care Ethics Principles:'), nl,
    forall(
        principle(care_ethics, P, D),
        (write('  - '), write(P), write(': '), write(D), nl)
    ),
    nl.

% Compare frameworks on a specific dilemma
compare_frameworks_menu :-
    nl, write('Available Dilemmas:'), nl,
    list_dilemmas,
    write('Enter dilemma ID to compare: '),
    read(DilemmaId),
    nl,
    compare_all_frameworks(DilemmaId).

% Compare all frameworks for a given dilemma
compare_all_frameworks(DilemmaId) :-
    dilemma(DilemmaId, Name, Desc, _),
    write('================================================='), nl,
    write('FRAMEWORK COMPARISON FOR: '), write(Name), nl,
    write('================================================='), nl,
    write('Description: '), write(Desc), nl, nl,
    
    compare_framework(DilemmaId, utilitarianism),
    compare_framework(DilemmaId, deontology),
    compare_framework(DilemmaId, virtue_ethics),
    compare_framework(DilemmaId, care_ethics).

% Helper to compare a single framework
compare_framework(DilemmaId, Framework) :-
    write('--- '), write(Framework), write(' ---'), nl,
    (   recommend_action(DilemmaId, Framework, Action, Score) ->
        write('Recommended: '), write(Action), nl,
        write('Score: '), write(Score), nl,
        brief_explanation(DilemmaId, Framework, Action)
    ;   write('No recommendation available'), nl
    ),
    nl.

% Brief explanation helper
brief_explanation(DilemmaId, Framework, Action) :-
    explain_decision(DilemmaId, Framework, Action, Explanation),
    write('Rationale: '), write(Explanation), nl.

% Quick analysis function
analyze_dilemma(DilemmaId, Framework) :-
    write('================================================='), nl,
    write('ETHICAL ANALYSIS'), nl,
    write('================================================='), nl,
    display_dilemma_info(DilemmaId),
    write('Framework: '), write(Framework), nl, nl,
    
    (   recommend_action(DilemmaId, Framework, Action, Score) ->
        write('RECOMMENDED ACTION: '), write(Action), nl,
        write('Ethical Score: '), write(Score), nl, nl,
        write('--- REASONING TRACE ---'), nl,
        generate_reasoning_trace(DilemmaId, Framework, Action),
        nl,
        write('--- DETAILED EXPLANATION ---'), nl,
        explain_decision(DilemmaId, Framework, Action, Explanation),
        write(Explanation), nl, nl
    ;   write('Unable to determine recommendation.'), nl
    ),
    write('================================================='), nl, nl.

% Display dilemma information
display_dilemma_info(DilemmaId) :-
    dilemma(DilemmaId, Name, Description, Actions),
    write('Dilemma: '), write(Name), nl,
    write('Description: '), write(Description), nl,
    write('Possible Actions: '), write(Actions), nl.

% Initialize and start
:- initialization(start).