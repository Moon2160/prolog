% knowledge_base.pl - Ethical Principles and Foundational Facts
% Contains ethical frameworks, principles, and moral values

% ============================================
% ETHICAL FRAMEWORKS
% ============================================

% Define available ethical frameworks
framework(utilitarianism).
framework(deontology).
framework(virtue_ethics).
framework(care_ethics).

% ============================================
% ETHICAL PRINCIPLES BY FRAMEWORK
% ============================================

% Utilitarian Principles - Maximize overall well-being
principle(utilitarianism, maximize_happiness, 'Actions are right if they maximize overall happiness').
principle(utilitarianism, minimize_suffering, 'Reduce suffering for the greatest number').
principle(utilitarianism, impartial_consideration, 'Everyone\'s happiness counts equally').
principle(utilitarianism, consequentialism, 'Outcomes determine morality, not intentions').

% Deontological Principles - Duty-based ethics
principle(deontology, categorical_imperative, 'Act only on maxims that can be universal laws').
principle(deontology, respect_autonomy, 'Treat people as ends, never merely as means').
principle(deontology, duty_to_truth, 'Always tell the truth').
principle(deontology, protect_innocent, 'Never harm innocent persons').
principle(deontology, keep_promises, 'Honoring commitments is a moral duty').

% Virtue Ethics Principles - Character-based ethics
principle(virtue_ethics, courage, 'Act bravely in the face of difficulty').
principle(virtue_ethics, compassion, 'Show empathy and kindness to others').
principle(virtue_ethics, justice, 'Treat people fairly and equitably').
principle(virtue_ethics, wisdom, 'Apply practical reasoning to ethical situations').
principle(virtue_ethics, temperance, 'Exercise self-control and moderation').
principle(virtue_ethics, integrity, 'Maintain moral consistency and honesty').

% Care Ethics Principles - Relationship-based ethics
principle(care_ethics, maintain_relationships, 'Preserve and nurture meaningful connections').
principle(care_ethics, respond_to_needs, 'Be attentive to others\' needs and vulnerabilities').
principle(care_ethics, contextual_judgment, 'Consider specific contexts and relationships').
principle(care_ethics, empathetic_understanding, 'Understand situations from others\' perspectives').

% ============================================
% MORAL VALUES AND THEIR WEIGHTS
% ============================================

% Format: value(ValueName, Description, UtilWeight, DeonWeight, VirtueWeight, CareWeight)
% Weights range from 0-10 indicating importance in each framework

value(preserve_life, 'Protecting human life', 10, 10, 9, 10).
value(prevent_harm, 'Avoiding causing harm to others', 9, 10, 8, 9).
value(autonomy, 'Respecting individual freedom and choice', 7, 10, 7, 6).
value(fairness, 'Treating people equally and justly', 8, 9, 9, 7).
value(honesty, 'Being truthful and transparent', 6, 10, 9, 7).
value(wellbeing, 'Promoting happiness and flourishing', 10, 6, 8, 8).
value(minimize_suffering, 'Reducing pain and distress', 10, 8, 7, 9).
value(trust, 'Maintaining trustworthy relationships', 6, 8, 8, 10).
value(compassion, 'Showing empathy and care', 7, 6, 10, 10).
value(justice, 'Ensuring fair outcomes and processes', 8, 9, 10, 7).

% ============================================
% MORAL CONSTRAINTS (Hard Rules)
% ============================================

% Format: constraint(FrameworkList, ConstraintName, Description)
% These are absolute prohibitions or requirements

constraint([deontology], no_killing_innocent, 'Never intentionally kill innocent people').
constraint([deontology], no_lying, 'Never lie, even for good outcomes').
constraint([deontology], no_using_people, 'Never use people merely as means to an end').
constraint([all], prohibit_torture, 'Torture is always wrong').
constraint([virtue_ethics, care_ethics], maintain_integrity, 'Act in accordance with virtuous character').

% ============================================
% STAKEHOLDER TYPES
% ============================================

% Different types of stakeholders who may be affected by decisions
stakeholder_type(individual, 'A single person directly affected').
stakeholder_type(group, 'A collection of people with shared interests').
stakeholder_type(vulnerable, 'People in positions of weakness or dependency').
stakeholder_type(society, 'The broader community or public').
stakeholder_type(future_generation, 'People who will be affected in the future').

% ============================================
% CONSEQUENCE TYPES
% ============================================

% Types of consequences that actions can produce
consequence_type(physical_harm, negative, 'Bodily injury or death').
consequence_type(psychological_harm, negative, 'Mental distress or trauma').
consequence_type(economic_harm, negative, 'Financial loss or deprivation').
consequence_type(autonomy_violation, negative, 'Restriction of freedom or choice').
consequence_type(wellbeing_increase, positive, 'Enhanced happiness or flourishing').
consequence_type(harm_prevention, positive, 'Avoiding negative outcomes').
consequence_type(relationship_damage, negative, 'Harm to trust or connections').
consequence_type(social_benefit, positive, 'Improved community welfare').

% ============================================
% VIRTUES AND VICES
% ============================================

% Virtues (positive character traits)
virtue(courage, 'Facing challenges with bravery').
virtue(compassion, 'Caring deeply for others\' welfare').
virtue(honesty, 'Commitment to truth').
virtue(justice, 'Fairness in treatment of others').
virtue(wisdom, 'Good judgment in complex situations').
virtue(temperance, 'Self-control and moderation').
virtue(integrity, 'Moral consistency and wholeness').
virtue(generosity, 'Willingness to give and help').

% Vices (negative character traits)
vice(cowardice, 'Avoiding necessary action from fear').
vice(callousness, 'Indifference to others\' suffering').
vice(dishonesty, 'Deception and lying').
vice(injustice, 'Unfair treatment of others').
vice(recklessness, 'Poor judgment and impulsivity').
vice(selfishness, 'Excessive self-interest').

% ============================================
% HELPER PREDICATES
% ============================================

% Check if a value is important for a given framework
value_important_for(Value, Framework, Weight) :-
    value(Value, _, U, D, V, C),
    (   Framework = utilitarianism -> Weight = U
    ;   Framework = deontology -> Weight = D
    ;   Framework = virtue_ethics -> Weight = V
    ;   Framework = care_ethics -> Weight = C
    ).

% Check if an action violates a constraint
violates_constraint(Action, Framework, Constraint) :-
    constraint(Frameworks, Constraint, _),
    (member(all, Frameworks) ; member(Framework, Frameworks)),
    action_property(Action, violates, Constraint).

% Get all principles for a framework
framework_principles(Framework, Principles) :-
    findall(P, principle(Framework, P, _), Principles).

% Check if action embodies a virtue
action_embodies_virtue(Action, Virtue) :-
    virtue(Virtue, _),
    action_property(Action, embodies, Virtue).

% Check if action embodies a vice
action_embodies_vice(Action, Vice) :-
    vice(Vice, _),
    action_property(Action, embodies, Vice).