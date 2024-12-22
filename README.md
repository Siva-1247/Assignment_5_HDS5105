# Assignment_5_HDS5105

Assignment 5 - Statistical Computing for Biomedical Data

*Group - Sivagami Nedumaran and Brendan Woods*

#**Project Summary:DIG Trial Analysis Dashboard**

The DIG Trial Analysis Dashboard is an interactive web application developed using R and the Shiny framework. Its purpose is to provide a comprehensive interface for exploring, analyzing, and visualizing data from the DIG trial, a clinical study investigating patient outcomes and related metrics. The dashboard offers dynamic visualizations, statistical summaries, and data filtering options tailored for clinical researchers, healthcare professionals, and other stakeholders.

Core Features:

Overview Tab:

Displays key summary statistics such as total patients, mortality rate, hospitalization rate, and median follow-up time in value boxes.
Includes a detailed textual overview of the study, highlighting its key findings.
Demographics Tab:

Visualizes patient demographic data, including gender, race, and age distributions.
Provides breakdowns of age by treatment group for deeper analysis.

Clinical Data Tab:

Enables exploration of clinical measures through interactive plots and tables.
Features filtering tools to stratify clinical data by variables such as age range and other user-defined parameters.
Outcomes Tab:

Visualizes patient outcome trends and provides summary statistics for selected metrics.
Allows users to customize analyses by selecting specific metrics and date ranges.

Mortality Tab:

Focuses on mortality analysis with tools for stratifying and filtering data by age, treatment, gender, and diabetes status.
Includes a dynamic table summarizing mortality-related findings.

Adverse Events Tab:

Displays information on adverse events, including hospitalization numbers and causes of death.
Provides visual insights into the frequency and distribution of these events.

Risk Analysis Tab:

Highlights key risk factors and their hazard ratios through forest plots.
Includes a correlation heatmap to visualize relationships between clinical variables.
Features a summary statistics table for risk-related metrics.

Technical Highlights:
Dynamic Interactivity: Dropdown menus, sliders, and date range inputs allow users to filter and customize analyses.

Visualizations: Integrated use of plotOutput for graphical representation of trends and data distributions.

Data Tables: Dynamic tables powered by DT for easy exploration and sorting of trial data.

Custom Styling: Consistent pastel color schemes and FontAwesome icons enhance usability and aesthetics.
