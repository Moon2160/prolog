 Ethical AI Advisor - A Prolog-Based Moral Reasoning System
   - An intelligent system that analyzes moral dilemmas using formalized ethical theories including Utilitarianism, Deontology, Virtue Ethics, and Care Ethics.

🎯 Overview

The Ethical AI Advisor is an expert system built in Prolog that provides justified ethical decisions for complex moral dilemmas. It implements multiple ethical frameworks and offers transparent reasoning traces, making it ideal for:

- 🎓 Educational purposes:- Teaching ethics and moral philosophy
- 🔬 Research: - Studying automated ethical reasoning
- 💼 Decision support:- Exploring ethical implications of choices
- 🤝 AI Ethics: - Understanding how machines can reason about morality

Key Capabilities

✅ Analyze moral dilemmas from 4 different ethical perspectives  
✅ Provide scored recommendations with detailed justifications  
✅ Compare how different frameworks approach the same problem  
✅ Generate step-by-step reasoning traces  
✅ Interactive GUI chatbot interface  

✨ Features

   Core Features

  - Multi-Framework Analysis: Evaluate dilemmas using:
  - 🎯 Utilitarianism - Maximize overall welfare
  - ⚖️ Deontology - Follow moral duties and rules
  - 💎 Virtue Ethics - Act according to virtuous character
  - ❤️ Care Ethics - Prioritize relationships and care

  - Rich Knowledge Base:
  - 20+ ethical principles
  - 10 moral values with framework-specific weights
  - 7 real-world dilemmas with comprehensive properties
  - Virtues, vices, and moral constraints

  - Transparent Reasoning:
  - Step-by-step logical traces
  - Framework-specific evaluation algorithms
  - Human-readable explanations
  - Comparative analysis across frameworks

  - Multiple Interfaces:
  - Command-line Prolog interface
  - Python GUI chatbot
  - Terminal-style interactive GUI

 Unique Advantages

🔍 Explainable AI - Every decision includes full reasoning trace  
📊 Quantitative Scoring - Numerical evaluation for comparison  
🎓 Educational - Perfect for learning about ethical theories  
🔧 Extensible - Easy to add new dilemmas and frameworks  


🏗️ System Architecture


┌─────────────────────────────────────────────────────────┐
│                   User Interfaces                       │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │   CLI Menu   │  │ Python GUI   │  │ Terminal GUI │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
└──────────────────────────┬──────────────────────────────┘
                           │
┌──────────────────────────▼──────────────────────────────┐
│              Prolog Reasoning Engine                    │
│  ┌────────────────────────────────────────────────────┐ │
│  │  main.pl - Entry point & interface logic          │ │
│  └────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────┐ │
│  │  reasoning_engine.pl - Core evaluation algorithms │ │
│  │  • evaluate_utilitarian()                          │ │
│  │  • evaluate_deontological()                        │ │
│  │  • evaluate_virtue_ethics()                        │ │
│  │  • evaluate_care_ethics()                          │ │
│  └────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────┐ │
│  │  explanation_module.pl - Justification generator  │ │
│  └────────────────────────────────────────────────────┘ │
└──────────────────────────┬──────────────────────────────┘
                           │
┌──────────────────────────▼──────────────────────────────┐
│                  Knowledge Base                         │
│  ┌────────────────────────────────────────────────────┐ │
│  │  knowledge_base.pl - Ethical principles & values  │ │
│  └────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────┐ │
│  │  dilemmas.pl - Moral scenarios & properties       │ │
│  └────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘

🚀 Installation

 Prerequisites

- SWI-Prolog (version 8.0 or higher)
  - Download: [https://www.swi-prolog.org/download/stable](https://www.swi-prolog.org/download/stable)
- Python 3.7+ (for GUI interfaces)
  - Download: [https://www.python.org/downloads/](https://www.python.org/downloads/)

prolog
Please select an option:
1. Analyze a predefined dilemma
2. List all ethical frameworks
3. Compare frameworks on a dilemma
4. View ethical principles
5. Exit
Enter your choice (1-5): 1.

Available Dilemmas:
  trolley_problem: The Trolley Problem
  ai_bias: Biased AI Hiring System
  ...
Enter dilemma ID: trolley_problem.
Option 2: Python GUI Chatbot

bash
python chatbot.py

Features:
- Terminal-style interface
- Direct Prolog integration
- Auto-completion of periods
- Color-coded output
- Real-time responses

Option 3: Standalone Python Chatbot (No Prolog Required)

For quick demos without Prolog:

bash
python chatbot.py

📚 Ethical Frameworks

 1. 🎯 Utilitarianism

Core Principle: Actions are right if they maximize overall happiness and minimize suffering.

Key Features:
- Consequence-based evaluation
- Calculates utility: `(magnitude × probability × people_affected)`
- Impartial consideration of all stakeholders

Example: In the trolley problem, pull the lever to save 5 lives at the cost of 1 (net: +4 lives).

2. ⚖️ Deontology

Core Principle: Follow moral duties and rules, regardless of consequences.

Key Features:
- Duty-based evaluation
- Categorical imperative: "Treat people as ends, never merely as means"
- Respects individual autonomy and rights

Example: Don't pull the lever because actively killing is always wrong, even to save more lives.

3. 💎 Virtue Ethics
Core Principle: Act according to virtuous character traits.

Key Features:
- Character-based evaluation
- Cultivates virtues: courage, compassion, honesty, justice, wisdom
- Applies practical wisdom (phronesis)
*Example: A courageous and compassionate person would pull the lever, taking responsibility for a difficult decision.

4. ❤️ Care Ethics

Core Principle: Maintain relationships and respond to needs.

Key Features:
- Relationship-based evaluation
- Contextual decision-making
- Prioritizes care for vulnerable individuals

Example: Consider specific relationships and responsibilities before deciding.

🎭 Example Dilemmas

1. The Trolley Problem

> A runaway trolley is heading toward five people. You can pull a lever to divert it to another track where one person is tied. What do you do?

Possible Actions: `pull_lever`, `do_nothing`

Framework Recommendations:
- Utilitarianism: Pull lever (Score: 40)
- Deontology: Do nothing (Score: 30)
- Virtue Ethics: Pull lever with courage (Score: 50)
- Care Ethics: Context-dependent (Score: 35)

2. Biased AI Hiring System

> Your company's AI tool is efficient but discriminates against female candidates. Do you deploy it?

Possible Actions: `deploy_system`, `fix_bias_first`, `abandon_ai`, `deploy_with_disclosure`

Framework Recommendations:
- Utilitarianism: Fix bias first (Score: 45)
- Deontology: Fix bias first (Score: 90)
- Virtue Ethics: Fix bias first (Score: 90)
- Care Ethics: Fix bias first (Score: 80)

3. Corporate Whistleblower

> You discover illegal toxic waste dumping. Reporting will save the environment but cause job losses. What do you do?

**Possible Actions**: `report_to_authorities`, `report_internally`, `stay_silent`, `anonymous_tip`

Framework Recommendations:
- Utilitarianism: Report to authorities (Score: 82)
- Deontology: Report to authorities (Score: 70)
- Virtue Ethics: Report with courage (Score: 85)
- Care Ethics: Report internally first (Score: 65)


Other Dilemmas

4. Autonomous Vehicle Dilemma- Who should self-driving cars save?
5. Medical Triage - Who receives the last ventilator?
6. Lying to Protect - Is it ethical to lie to save someone?
7. Climate vs Economy - Aggressive climate action vs gradual transition?

📁 Project Structure

ethical-ai-advisor/
│
├── prolog/
│   ├── main.pl                    # Main entry point and interface
│   ├── knowledge_base.pl          # Ethical principles and frameworks
│   ├── reasoning_engine.pl        # Core evaluation algorithms
│   ├── explanation_module.pl      # Decision justification generator
│   └── dilemmas.pl                # Moral scenarios and properties
│
├── python/
│   ├── prolog_gui_chatbot.py      # Terminal-style GUI (Prolog-connected)
│   ├── chatbot_simple.py          # Standalone chatbot (no Prolog)
│   └── copilot.py                 # Simple launcher script
│
├── docs/
│   ├── ARCHITECTURE.md            # System architecture details
│   ├── FRAMEWORKS.md              # Detailed framework explanations
│   └── API.md                     # Prolog predicates documentation
│
├── examples/
│   ├── example_queries.pl         # Sample Prolog queries
│   └── example_outputs.txt        # Expected outputs
│
├── tests/
│   ├── test_reasoning.pl          # Unit tests for reasoning engine
│   └── test_dilemmas.pl           # Tests for dilemma properties
│
├── README.md                      # This file
├── LICENSE                        # MIT License
└── .gitignore                     # Git ignore file

📸 Screenshots
 Command-Line Interface

=================================================
   ETHICAL AI ADVISOR - Moral Reasoning System
=================================================

Please select an option:
1. Analyze a predefined dilemma
2. List all ethical frameworks
3. Compare frameworks on a dilemma
4. View ethical principles
5. Exit


Python GUI Chatbot
<img width="1920" height="1080" alt="image" src="https://github.com/user-attachments/assets/0ec173cd-7297-4bdf-b74b-6aafe4fdaf36" />

 Analysis Output

=================================================
ETHICAL ANALYSIS
=================================================
Dilemma: The Trolley Problem
Framework: utilitarianism

RECOMMENDED ACTION: pull_lever
Ethical Score: 40.0

--- REASONING TRACE ---
Step 1: Identify ethical framework principles
  - maximize_happiness: Actions are right if they maximize overall happiness
  - minimize_suffering: Reduce suffering for the greatest number

Step 2: Analyze action properties
  - respects_autonomy: false
  - uses_as_means: true

Step 3: Evaluate consequences and impacts
  - save_five_lives (probability: 1.0)
  - kill_one_person (probability: 1.0)

--- DETAILED EXPLANATION ---
From a utilitarian perspective, this action maximizes overall welfare...


🤝 Contributing

We welcome contributions! Here's how you can help:

### Ways to Contribute

1. **Add New Dilemmas**
   - Create realistic ethical scenarios
   - Define action properties and consequences
   - Document your reasoning

2. **Extend Frameworks**
   - Implement additional ethical theories
   - Improve existing evaluation algorithms
   - Add new principles

3. **Improve Documentation**
   - Write tutorials
   - Add more examples
   - Translate to other languages

4. **Bug Fixes & Enhancements**
   - Report issues
   - Submit pull requests
   - Suggest features


🎓 Educational Use

This system is designed for educational purposes and research. It can be used for:

- **University Courses**: Ethics, Philosophy, AI courses
- **Workshops**: Teaching ethical reasoning
- **Research**: Studying automated moral reasoning
- **Self-Learning**: Exploring ethical frameworks

Suggested Activities

1. **Compare Frameworks**: Analyze the same dilemma with all frameworks
2. **Create Dilemmas**: Add your own ethical scenarios
3. **Debate**: Discuss whether the system's reasoning is sound
4. **Extend**: Implement new ethical theories (e.g., Confucian ethics)

⚠️ Limitations & Disclaimers

   Important Notes

- 📌 **Not a Substitute for Human Judgment**: This is an educational tool, not a decision-making authority
- 📌 **Simplified Models**: Real ethical reasoning is more nuanced
- 📌 **Western Philosophy Focus**: Current frameworks reflect Western ethical traditions
- 📌 **Incomplete Information**: Real dilemmas often lack clear probabilities
- 📌 **No Meta-Ethics**: The system doesn't evaluate frameworks themselves

Ethical Considerations

This system demonstrates how machines can reason about ethics, but:

- Moral decisions require empathy, wisdom, and context
- Different cultures may have different ethical frameworks
- Quantifying ethics has inherent limitations
- Always consult ethics experts for real-world decisions


## 🙏 Acknowledgments

 Inspirations

- **Philosophical Works**:
  - Jeremy Bentham & John Stuart Mill - *Utilitarianism*
  - Immanuel Kant - *Groundwork of the Metaphysics of Morals*
  - Aristotle - *Nicomachean Ethics*
  - Carol Gilligan - *In a Different Voice*

- **Research Papers**:
  - "Machine Ethics" by Michael Anderson & Susan Leigh Anderson
  - "Moral Machines" by Wendell Wallach & Colin Allen

### Tools & Technologies

- [SWI-Prolog](https://www.swi-prolog.org/) - The Prolog development environment
- [Python](https://www.python.org/) - For GUI interfaces
- [Tkinter](https://docs.python.org/3/library/tkinter.html) - GUI framework




</div>
