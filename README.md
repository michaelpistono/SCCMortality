# Unhoused Mortality Data Processing & Analysis

**Author:** Michael Pistono  
**Date:** February 1, 2025

---

## Overview

This repository contains two R scripts to process and analyze mortality records for people experiencing homelessness in Santa Clara County (SCC):

1. **`sort_unhoused_deaths.R`**  
   - Classifies each death into one of eight mutually exclusive cause‐of‐death categories.  
   - Produces a master dataframe (`master`) for downstream analyses.

2. **`analyze_unhoused_deaths.R`**  
   - Takes each cause‐specific subset (accidents, chronic disease, infectious, overdose) and further breaks them down by type (e.g. pedestrian vs. collision, cancer vs. cardiac, bacterial vs. viral pathogens, substance classes).  
   - Aggregates annual counts and produces ggplot2 charts.

---

## Repository Structure

. ├── README.md
├── homeless_deaths.csv Raw input data ├── sort_unhoused_deaths.R Scripts to classify and merge causes └── analyze_unhoused_deaths.R Scripts to detail and plot sub‐categories


# Unhoused Mortality Data Sorting Algorithm

## Overview

This R script processes raw mortality records for people experiencing homelessness in Santa Clara County (SCC) and classifies each death into mutually exclusive cause‐of‐death categories. The output is a “master” dataframe that can be used for downstream analyses, visualization, or export.

### Key Features

- Imports SCC unhoused mortality data from a CSV file.
- Converts and filters date fields to select the study period: 2019–2023.
- Assigns each decedent to:
  - **COVID**
  - **Undetermined**
  - **Homicide**
  - **Suicide**
  - **Infection** (other infectious diseases)
  - **Chronic** (non‐infectious/natural)
  - **Accident** (unintentional injuries)
  - **Substance** (overdose)
- Creates demographic groupings:
  - Age groups (0–17, 18–24, …, 65+)
  - Race/ethnicity categories consistent with ACS definitions.
- Validates and tabulates case counts by category, year, age group, gender, and race/ethnicity.

---

- **homeless_deaths.csv**: Raw mortality data for unhoused individuals in SCC.
- **sort_unhoused_deaths.R**: Main script implementing the classification algorithm.

---

## Dependencies

- R ≥ 4.0  
- The following R packages:
  - **tidyverse** (for data manipulation)
  - **lubridate** (for date parsing and extraction)

You can install missing packages with:

```r
install.packages(c("tidyverse", "lubridate"))


Usage
Clone the repo

bash
Copy
Edit
git clone https://github.com/<your-username>/unhoused-mortality-scc.git
cd unhoused-mortality-scc
Prepare your data
Place homeless_deaths.csv in the project root, or adjust the read_csv() path in each script.

Run the sorter

r
Copy
Edit
source("sort_unhoused_deaths.R")
# result: `master` dataframe in your R environment
write_csv(master, "unhoused_deaths_classified.csv")
Run the analyzer

r
Copy
Edit
source("analyze_unhoused_deaths.R")
# produces ggplot figures in your R plotting pane
Script Details
1. sort_unhoused_deaths.R
Purpose: Read the raw CSV, standardize column names, parse and filter dates (2019–2023), assign demographic bins (age, race/ethnicity), then sequentially extract decedents by:

COVID-19

Undetermined

Homicide

Suicide

Infection

Chronic

Accident

Substance

Output: master dataframe with columns:

sql
Copy
Edit
case_number, Age, Age_Group, Gender, Race_Ethnicity, year,
Cause, cause_of_death, other_significant_condition
2. analyze_unhoused_deaths.R
Purpose: Take each subset created by the sorter (accident_related_cases, chronic_related_cases, infection_related_cases, overdose_related_cases) and:

Define keyword vectors for sub‐types (e.g. “pedestrian”, “cancer”, “Influenza”, “fentanyl”).

Loop to add T/F flags for each keyword, convert to 0/1 counts.

Collapse by year into wide tables, then melt to long format.

Generate line or bar charts showing annual trends by sub‐category.

Sections:

Accidental Injuries
– Pedestrian, Collision, Drowning, Hypothermia, Carbon Monoxide

Chronic Disease
– Cancer, Cardiovascular, Renal, Diabetes, Hepatic, Respiratory, Neurological

Infectious Diseases
– Bacterial vs. Viral pathogens (e.g. E. coli, Pneumococcal, Influenza, RSV)
– Syndromes (Sepsis, Pneumonia, Endocarditis, etc.)

Substance Overdose
– Alcohol, Methamphetamine, Fentanyl, Polysubstance, Stimulants, Opioids

Next Steps
Export plots to files (e.g. ggsave()).

Integrate workflow in an R Markdown or drake/targets pipeline.

Deploy summary figures in a Shiny app or Power BI dashboard.


