% dilemmas.pl - Example Moral Dilemmas and Their Properties
% Contains concrete ethical scenarios with their characteristics

% ============================================
% DILEMMA DEFINITIONS
% ============================================

% Format: dilemma(ID, Name, Description, PossibleActions)

dilemma(
    trolley_problem,
    'The Trolley Problem',
    'A runaway trolley is heading toward five people tied on the tracks. You can pull a lever to divert it to another track where one person is tied. What do you do?',
    [pull_lever, do_nothing]
).

dilemma(
    self_driving_car,
    'Autonomous Vehicle Dilemma',
    'A self-driving car\'s brakes fail. It must choose between hitting a group of 5 pedestrians or swerving and hitting a barrier, likely killing its single passenger.',
    [swerve_kill_passenger, continue_kill_pedestrians, attempt_brake]
).

dilemma(
    medical_triage,
    'Emergency Room Triage',
    'An ER doctor has one ventilator left. Patient A is a 30-year-old parent with 70% survival chance. Patient B is a 75-year-old with 40% survival chance. Who receives the ventilator?',
    [give_to_patient_a, give_to_patient_b, flip_coin]
).

dilemma(
    ai_bias,
    'Biased AI Hiring System',
    'Your company\'s AI hiring tool shows better performance but exhibits gender bias, rejecting qualified female candidates at higher rates. Do you deploy it?',
    [deploy_system, fix_bias_first, abandon_ai, deploy_with_disclosure]
).

dilemma(
    whistleblower,
    'Corporate Whistleblower',
    'You discover your company is dumping toxic waste illegally. Reporting it will save the environment but cause job losses including your own. What do you do?',
    [report_to_authorities, report_internally, stay_silent, anonymous_tip]
).

dilemma(
    lying_to_protect,
    'Lying to Protect',
    'A dangerous person asks if you know the location of their intended victim, whom you are hiding. Do you lie to protect them?',
    [lie_to_protect, tell_truth, refuse_to_answer]
).

dilemma(
    resource_allocation,
    'Climate vs Economy',
    'As a policymaker, you must choose between aggressive climate action (economic disruption now) or gradual transition (less disruption, more future harm).',
    [aggressive_action, gradual_transition, balanced_approach]
).

% ============================================
% ACTION PROPERTIES
% ============================================

% Trolley Problem - Pull Lever
action_property(trolley_problem, pull_lever, respects_autonomy, false).
action_property(trolley_problem, pull_lever, uses_as_means, true).
action_property(trolley_problem, pull_lever, embodies_virtue, courage).
action_property(trolley_problem, pull_lever, shows_wisdom, true).
action_property(trolley_problem, pull_lever, contextually_appropriate, true).

% Trolley Problem - Do Nothing
action_property(trolley_problem, do_nothing, respects_autonomy, true).
action_property(trolley_problem, do_nothing, uses_as_means, false).
action_property(trolley_problem, do_nothing, embodies_vice, cowardice).
action_property(trolley_problem, do_nothing, violates_duty, help_others).

% Self-Driving Car - Swerve
action_property(self_driving_car, swerve_kill_passenger, respects_autonomy, false).
action_property(self_driving_car, swerve_kill_passenger, embodies_virtue, courage).
action_property(self_driving_car, swerve_kill_passenger, shows_wisdom, true).
action_property(self_driving_car, swerve_kill_passenger, contextually_appropriate, true).

% Self-Driving Car - Continue
action_property(self_driving_car, continue_kill_pedestrians, respects_autonomy, true).
action_property(self_driving_car, continue_kill_pedestrians, embodies_vice, callousness).

% Self-Driving Car - Attempt Brake
action_property(self_driving_car, attempt_brake, respects_autonomy, true).
action_property(self_driving_car, attempt_brake, embodies_virtue, wisdom).
action_property(self_driving_car, attempt_brake, shows_wisdom, true).

% Medical Triage - Patient A
action_property(medical_triage, give_to_patient_a, embodies_virtue, wisdom).
action_property(medical_triage, give_to_patient_a, shows_wisdom, true).
action_property(medical_triage, give_to_patient_a, contextually_appropriate, true).
action_property(medical_triage, give_to_patient_a, addresses_need, medical_care).

% Medical Triage - Patient B
action_property(medical_triage, give_to_patient_b, embodies_virtue, compassion).
action_property(medical_triage, give_to_patient_b, contextually_appropriate, true).
action_property(medical_triage, give_to_patient_b, addresses_need, medical_care).
action_property(medical_triage, give_to_patient_b, protects_vulnerable, true).

% Medical Triage - Flip Coin
action_property(medical_triage, flip_coin, embodies_virtue, justice).
action_property(medical_triage, flip_coin, respects_autonomy, true).
action_property(medical_triage, flip_coin, fulfills_duty, be_fair).

% AI Bias - Deploy System
action_property(ai_bias, deploy_system, violates_duty, be_fair).
action_property(ai_bias, deploy_system, embodies_vice, injustice).
action_property(ai_bias, deploy_system, damages_relationships, true).

% AI Bias - Fix Bias First
action_property(ai_bias, fix_bias_first, embodies_virtue, justice).
action_property(ai_bias, fix_bias_first, embodies_virtue, integrity).
action_property(ai_bias, fix_bias_first, fulfills_duty, be_fair).
action_property(ai_bias, fix_bias_first, shows_wisdom, true).
action_property(ai_bias, fix_bias_first, contextually_appropriate, true).

% AI Bias - Abandon AI
action_property(ai_bias, abandon_ai, embodies_virtue, integrity).
action_property(ai_bias, abandon_ai, fulfills_duty, be_fair).

% AI Bias - Deploy with Disclosure
action_property(ai_bias, deploy_with_disclosure, embodies_virtue, honesty).
action_property(ai_bias, deploy_with_disclosure, fulfills_duty, tell_truth).
action_property(ai_bias, deploy_with_disclosure, violates_duty, be_fair).

% Whistleblower - Report to Authorities
action_property(whistleblower, report_to_authorities, embodies_virtue, courage).
action_property(whistleblower, report_to_authorities, embodies_virtue, integrity).
action_property(whistleblower, report_to_authorities, fulfills_duty, tell_truth).
action_property(whistleblower, report_to_authorities, shows_wisdom, true).
action_property(whistleblower, report_to_authorities, damages_relationships, true).

% Whistleblower - Report Internally
action_property(whistleblower, report_internally, embodies_virtue, courage).
action_property(whistleblower, report_internally, preserves_relationships, true).
action_property(whistleblower, report_internally, contextually_appropriate, true).

% Whistleblower - Stay Silent
action_property(whistleblower, stay_silent, embodies_vice, cowardice).
action_property(whistleblower, stay_silent, violates_duty, tell_truth).
action_property(whistleblower, stay_silent, violates_duty, help_others).
action_property(whistleblower, stay_silent, preserves_relationships, true).

% Whistleblower - Anonymous Tip
action_property(whistleblower, anonymous_tip, embodies_virtue, wisdom).
action_property(whistleblower, anonymous_tip, shows_wisdom, true).
action_property(whistleblower, anonymous_tip, contextually_appropriate, true).

% Lying to Protect - Lie
action_property(lying_to_protect, lie_to_protect, embodies_virtue, compassion).
action_property(lying_to_protect, lie_to_protect, violates_duty, tell_truth).
action_property(lying_to_protect, lie_to_protect, fulfills_duty, help_others).
action_property(lying_to_protect, lie_to_protect, protects_vulnerable, true).
action_property(lying_to_protect, lie_to_protect, shows_wisdom, true).

% Lying to Protect - Tell Truth
action_property(lying_to_protect, tell_truth, embodies_virtue, honesty).
action_property(lying_to_protect, tell_truth, fulfills_duty, tell_truth).
action_property(lying_to_protect, tell_truth, violates_duty, help_others).
action_property(lying_to_protect, tell_truth, embodies_vice, callousness).

% Lying to Protect - Refuse
action_property(lying_to_protect, refuse_to_answer, embodies_virtue, wisdom).
action_property(lying_to_protect, refuse_to_answer, shows_wisdom, true).
action_property(lying_to_protect, refuse_to_answer, contextually_appropriate, true).

% Climate Policy - Aggressive Action
action_property(resource_allocation, aggressive_action, embodies_virtue, courage).
action_property(resource_allocation, aggressive_action, shows_wisdom, true).
action_property(resource_allocation, aggressive_action, protects_vulnerable, true).

% Climate Policy - Gradual Transition
action_property(resource_allocation, gradual_transition, embodies_virtue, temperance).
action_property(resource_allocation, gradual_transition, preserves_relationships, true).

% Climate Policy - Balanced Approach
action_property(resource_allocation, balanced_approach, embodies_virtue, wisdom).
action_property(resource_allocation, balanced_approach, shows_wisdom, true).
action_property(resource_allocation, balanced_approach, contextually_appropriate, true).

% ============================================
% ACTION CONSEQUENCES
% ============================================

% Format: action_consequence(DilemmaID, Action, Consequence, Probability)

% Trolley Problem
action_consequence(trolley_problem, pull_lever, save_five_lives, 1.0).
action_consequence(trolley_problem, pull_lever, kill_one_person, 1.0).
action_consequence(trolley_problem, do_nothing, kill_five_people, 1.0).

% Self-Driving Car
action_consequence(self_driving_car, swerve_kill_passenger, save_five_pedestrians, 0.95).
action_consequence(self_driving_car, swerve_kill_passenger, kill_passenger, 0.8).
action_consequence(self_driving_car, continue_kill_pedestrians, kill_five_pedestrians, 0.9).
action_consequence(self_driving_car, continue_kill_pedestrians, save_passenger, 0.95).
action_consequence(self_driving_car, attempt_brake, reduce_casualties, 0.6).
action_consequence(self_driving_car, attempt_brake, some_deaths, 0.4).

% Medical Triage
action_consequence(medical_triage, give_to_patient_a, patient_a_survives, 0.7).
action_consequence(medical_triage, give_to_patient_a, patient_b_dies, 1.0).
action_consequence(medical_triage, give_to_patient_b, patient_b_survives, 0.4).
action_consequence(medical_triage, give_to_patient_b, patient_a_dies, 1.0).
action_consequence(medical_triage, flip_coin, fair_process, 1.0).

% AI Bias
action_consequence(ai_bias, deploy_system, discrimination_against_women, 1.0).
action_consequence(ai_bias, deploy_system, improved_efficiency, 0.8).
action_consequence(ai_bias, fix_bias_first, fair_hiring, 0.9).
action_consequence(ai_bias, fix_bias_first, delayed_deployment, 1.0).
action_consequence(ai_bias, abandon_ai, no_discrimination, 1.0).
action_consequence(ai_bias, abandon_ai, reduced_efficiency, 0.7).

% Whistleblower
action_consequence(whistleblower, report_to_authorities, environmental_protection, 0.9).
action_consequence(whistleblower, report_to_authorities, job_losses, 0.8).
action_consequence(whistleblower, report_internally, potential_fix, 0.5).
action_consequence(whistleblower, report_internally, job_security, 0.7).
action_consequence(whistleblower, stay_silent, continued_pollution, 1.0).
action_consequence(whistleblower, stay_silent, job_security, 0.95).

% Lying to Protect
action_consequence(lying_to_protect, lie_to_protect, victim_protected, 0.9).
action_consequence(lying_to_protect, lie_to_protect, erosion_of_trust, 0.3).
action_consequence(lying_to_protect, tell_truth, victim_harmed, 0.8).
action_consequence(lying_to_protect, tell_truth, maintain_honesty, 1.0).
action_consequence(lying_to_protect, refuse_to_answer, victim_protected, 0.7).
action_consequence(lying_to_protect, refuse_to_answer, partial_honesty, 1.0).

% Climate Policy
action_consequence(resource_allocation, aggressive_action, climate_protection, 0.8).
action_consequence(resource_allocation, aggressive_action, economic_disruption, 0.7).
action_consequence(resource_allocation, gradual_transition, moderate_climate_action, 0.6).
action_consequence(resource_allocation, gradual_transition, economic_stability, 0.8).
action_consequence(resource_allocation, balanced_approach, reasonable_climate_action, 0.7).
action_consequence(resource_allocation, balanced_approach, manageable_disruption, 0.7).

% ============================================
% CONSEQUENCE IMPACTS
% ============================================

% Format: consequence_impact(Consequence, ImpactType, Magnitude, NumberAffected)

consequence_impact(save_five_lives, positive, 10, 5).
consequence_impact(kill_one_person, negative, 10, 1).
consequence_impact(kill_five_people, negative, 10, 5).
consequence_impact(save_five_pedestrians, positive, 10, 5).
consequence_impact(kill_passenger, negative, 10, 1).
consequence_impact(kill_five_pedestrians, negative, 10, 5).
consequence_impact(save_passenger, positive, 10, 1).
consequence_impact(reduce_casualties, positive, 7, 4).
consequence_impact(some_deaths, negative, 10, 2).

consequence_impact(patient_a_survives, positive, 10, 1).
consequence_impact(patient_b_dies, negative, 10, 1).
consequence_impact(patient_b_survives, positive, 10, 1).
consequence_impact(patient_a_dies, negative, 10, 1).
consequence_impact(fair_process, positive, 7, 2).

consequence_impact(discrimination_against_women, negative, 8, 1000).
consequence_impact(improved_efficiency, positive, 5, 500).
consequence_impact(fair_hiring, positive, 9, 1000).
consequence_impact(delayed_deployment, negative, 3, 500).
consequence_impact(no_discrimination, positive, 9, 1000).
consequence_impact(reduced_efficiency, negative, 4, 500).

consequence_impact(environmental_protection, positive, 9, 100000).
consequence_impact(job_losses, negative, 7, 500).
consequence_impact(potential_fix, positive, 6, 100000).
consequence_impact(job_security, positive, 5, 500).
consequence_impact(continued_pollution, negative, 9, 100000).

consequence_impact(victim_protected, positive, 10, 1).
consequence_impact(erosion_of_trust, negative, 5, 2).
consequence_impact(victim_harmed, negative, 10, 1).
consequence_impact(maintain_honesty, positive, 6, 2).
consequence_impact(partial_honesty, positive, 4, 2).

consequence_impact(climate_protection, positive, 10, 8000000000).
consequence_impact(economic_disruption, negative, 6, 500000000).
consequence_impact(moderate_climate_action, positive, 6, 8000000000).
consequence_impact(economic_stability, positive, 7, 500000000).
consequence_impact(reasonable_climate_action, positive, 8, 8000000000).
consequence_impact(manageable_disruption, negative, 4, 500000000).

% ============================================
% STAKEHOLDER NEEDS
% ============================================

% Format: stakeholder_need(DilemmaID, Stakeholder, Need, Urgency)

stakeholder_need(medical_triage, patient_a, medical_care, 10).
stakeholder_need(medical_triage, patient_b, medical_care, 10).
stakeholder_need(ai_bias, female_candidates, fair_treatment, 9).
stakeholder_need(whistleblower, community, clean_environment, 10).
stakeholder_need(whistleblower, employees, job_security, 7).
stakeholder_need(lying_to_protect, victim, safety, 10).
stakeholder_need(resource_allocation, future_generations, livable_planet, 10).
stakeholder_need(resource_allocation, current_workers, economic_stability, 8).