# UPP-DAT-pMNL-Teaching-Example
This repository contains a teaching example based on the Understanding Persistent Pain Decision Aid Tool (UPP DAT). The code demonstrates how individual patient preferences can be estimated from a small set of discrete choice experiment (DCE) tasks using a penalised multinomial logit model.

Purpose

The goal is to show how responses to DCE choice tasks can be transformed into a patient-friendly preference report that could support shared decision-making.

The workflow is:

Choice Tasks
    ↓
Patient Responses
    ↓
pMNL Model
    ↓
Preference Coefficients
    ↓
Relative Importance Scores
    ↓
Patient-Friendly Report

Choice tasks:

The UPP choice task participant handout contains the 12 choice tasks. 

The R script:

Defines eight persistent pain management attributes.
Sets up 12 paired choice tasks.
Records participant choices between Plan A and Plan B.
Converts the choices into a model-ready format.
Estimates individual-level preference coefficients using a penalised multinomial logit model.
Converts model coefficients into relative importance scores.
Produces two patient-facing plots:
Actions you prefer
Outcomes that matter most to you
Exports:
UPP_individual_preference_report.csv
UPP_actions_plot.png
UPP_outcomes_plot.png
Required R Packages
install.packages(c(
  "dplyr",
  "tibble",
  "tidyr",
  "ggplot2",
  "readr"
))
How to Use

Open the R script and edit the participant_choices vector:

participant_choices <- c(
  "A", "A", "A", "A", "A", "A",
  "A", "A", "A", "A", "A", "A"
)

Replace each value with the participant’s selected option for each of the 12 choice tasks.

Choices must be entered as either:

"A"

or

"B"

Then run the script.

Outputs
Preference Coefficients

The model first estimates coefficients for each attribute.

A positive coefficient means the participant preferred the 1-coded level of that attribute.

A negative coefficient means the participant preferred the 0-coded level.

Larger absolute values indicate stronger preferences.

Relative Importance Scores

The coefficients are rescaled into relative importance scores so they are easier to interpret.

Within each group, the most influential attribute is scaled to 100, and all other attributes are expressed relative to that attribute.

Teaching Use

This script is intended for teaching and demonstration purposes.

It can be used to help students understand:

How DCE choice data are structured
Why individual-level estimation is challenging with small datasets
How penalisation helps stabilise estimates
How model coefficients can be translated into patient-facing outputs
How preference elicitation can support shared decision-making
Important Note

This is a simplified teaching implementation. It is not the original UPP DAT production algorithm and should not be used for clinical decision-making.

Suggested Citation

Ryan M, Loría-Rebolledo L, Adam R, Bond C, Murchie P. Developing a person-centred discrete choice experiment to promote shared decision making in the patient-pharmacist interaction: Final Report. Pharmacy Research UK; 2023.

Loría-Rebolledo LE, Ryan M, Bond C, Porteous T, Murchie P, Adam R. Using a discrete choice experiment to develop a decision aid tool to inform the management of persistent pain in pharmacy: a protocol for a randomised feasibility study. BMJ Open. 2022;12. doi:10.1136/bmjopen-2022-066379.
