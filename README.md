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

