################################################################################

#Author: Michael Pistono
#Date: 1 February 2025
#Purpose: Sorting algorithm for Unhoused Mortality data in Santa Clara County

# Load libraries
library(tidyverse)
library(lubridate)

################### Define cause-related keywords ##############################
################################################################################

# Define keywords for COVID-19
covid_terms = c(
  "covid", "covid-19", "sars-cov-2")

# Define keywords for infectious disease-related deaths
infection_terms <- c(
  "acinetobacter", "adenov", "aspergil", "aureus", "bacter", "bacterial", 
  "bacteremia", "beta-lactamase", "bordatella", "campy", "campylobacter", 
  "candida", "cellulitis", "cholera", "clostrid", "clostridium", "coccidi", 
  "coccidio", "cocci", "coccus", "cytomegalo", "dengue", "e. coli", "enterobacter",
  "escherichia", "fungal", "gram", "group a", "hanta", "haemophilus", "hepatitic",
  "hepatitis", "herpes", "HIV", "HSV", "infection", "infective", "influenza", 
  "kleb", "listeria", "meningitis", "metapneumo", "methicillin", "microb", "MRSA",
  "mycoplas", "neisseria", "norovirus", "omonas", "organism", "pneumococcal", 
  "pneumococcus", "pneumonia", "pneumoniae", "proteus", "protozo", "psuedomonas",
  "pseudomonas", "resist", "resist", "rotavirus", "salmonella", "septic", "sepsis",
  "serratia", "shiga", "shigella", "staph", "stenotrophomonas", "strep", 
  "syncytial", "tubercu", "typhoid", "viremia", "viral", "virus", "west nile", 
  "zoster")

# Define accident and injury-related terms
accident_terms <- c(
  "aspiration", "asphyxia", "blunt", "choking", "collision", "drowning",
  "exposure", "force", "gunshot", "hypothermia", "inhalation", "injur",
  "mersion", "monoxide", "multiple", "pedestrian", "thermal", "water")

################### Import ME-C data for unhoused population ###################
################################################################################

#import raw mortality data for the unhoused population of SCC
setwd("~/Desktop/SCC Mortality Study")
unhoused = read_csv("homeless_deaths.csv")

#rename columns
unhoused = unhoused%>%
  rename(case_number = "Case_Number",
         case_status = "Case_Status",
         manner_of_death = "Manner_of_Death",
         death_date = "Death_Date",
         death_city = "Death_City",
         death_zip = "Death_Zip",
         resident_city = "Resident_City",
         resident_zip = "Resident_Zip",
         incident_location = "Incident_Location",
         incident_city = "Incident_City",
         incident_zip = "Incident_Zip",
         cause_of_death = "Cause_of_Death",
         other_significant_condition = "Other_Significant_Condition")

#format date/time group so that deaths can be aggregated by year
if ("death_date" %in% colnames(unhoused)) {
  unhoused$death_date <- as.POSIXct(unhoused$death_date,
                                    format = "%Y-%m-%d-%H-%M-%S",
                                    tz ="UTC")
  unhoused$year = year(unhoused$death_date)
} else {
  stop("Date column missing")
}

#add columns for age group and race/ethnicity
#remove deaths for non-live births
#add age groupings consistent with ACS 10-year age groups
#filter for study period
unhoused = unhoused%>%
  filter(year < 2024 & year > 2018,
         Age != 0)%>%
  mutate(Race_Ethnicity = case_when(
    str_detect(Race, "(?i)american indian|alaskan native") ~ "American Indian/Alaska Native",
    str_detect(Race, "(?i)black|africanamerican") ~ "Black/African American",
    str_detect(Race, "(?i)asian") ~ "Asian",
    str_detect(Race, "(?i)pacific islander|islander") ~ "Native Hawaiian/Pacific Islander",
    str_detect(Race, "(?i)white") ~ "White",
    str_detect(Race, "(?i)hispanic|latino") ~ "Hispanic/Latino",
    TRUE ~ "Other/Multiracial"  # Catches categories like "Unknown" and "Other"
  ),
  Age_Group = cut(Age, 
                  breaks = c(0, 17, 24, 34, 44, 54, 64, Inf), 
                  labels = c("0-17", "18-24", "25-34", "35-44", "45-54",
                             "55-64", "65+"), 
                  right = TRUE))


######################### Sort deaths by cause #################################
################################################################################


#################################################################
########################### COVID-19 ############################
#################################################################

# Create a pattern by joining terms with '|'
pattern <- paste(covid_terms, collapse = "|")

# Filter rows based on "Cause_of_Death" to be consistent with CDC WONDER
# underlying cause of death (UCD) ICD-10 coding for general population

COVIDonly <- unhoused[
  grepl(pattern, unhoused$cause_of_death, ignore.case = TRUE), 
]

#clean up columns for master dataframe and remove cases where ultimate cause of
#death could not be determined by ME-C

COVIDonly= COVIDonly%>%
  filter(manner_of_death != "Undetermined")%>%
  mutate(Cause = "COVID")%>%
  dplyr::select(case_number, Age, Age_Group, Gender, Race_Ethnicity, year, Cause,
                cause_of_death, other_significant_condition)

#create master dataframe
master = COVIDonly


############################################################
####################### UNDETERMINED #######################
############################################################

#sort deaths where cause was not determined by the coroner
undetermined_cases = unhoused%>%
  filter(manner_of_death == "Undetermined")

#clean up columns 
undetermined_cases = undetermined_cases%>%
  mutate(Cause = "Undetermined")%>%
  dplyr::select(case_number, Age, Age_Group, Gender, Race_Ethnicity, year, Cause,
                cause_of_death, other_significant_condition)

#bind new rows to master dataframe
master = rbind(undetermined_cases, master)


###########################################################
######################## HOMICIDES ########################
###########################################################

#sort deaths that were ruled homicides by the coroner
homicide_related_cases = unhoused%>%
  filter(manner_of_death == "Homicide")

#clean up columns 
homicide_related_cases = homicide_related_cases%>%
  mutate(Cause = "Homicide")%>%
  dplyr::select(case_number, Age, Age_Group, Gender, Race_Ethnicity, year, Cause,
                cause_of_death, other_significant_condition)

#join to master dataframe
master = rbind(homicide_related_cases, master)


########################################################
####################### SUICIDES #######################
########################################################

#sort deaths that were ruled suicides by the coroner
suicide_related_cases = unhoused%>%
  filter(manner_of_death == "Suicide")

#clean up columns and ensure none are double-counted in the master dataframe
suicide_related_cases = suicide_related_cases%>%
  mutate(Cause = "Suicide")%>%
  dplyr::select(case_number, Age, Age_Group, Gender, Race_Ethnicity, year, Cause,
                cause_of_death, other_significant_condition)

#join to master dataframe
master = rbind(suicide_related_cases, master)


################################################################################
###################### ACCIDENTAL or NATURAL CAUSES ############################
################################################################################

#divide remaining deaths into accidental and natural causes groups

#use anti_join() to ensure nothing is double-counted

accidental_cases = unhoused%>%
  filter(manner_of_death == "Accident")%>%
  anti_join(master, by = "case_number")

# after reviewing "cause of death" and "other significant conditions" columns,
# if manner_of_death is "N/A," include in natural causes classification
# since all "N/A" rows are either infectious or chronic disease deaths

natural_cases = unhoused%>%
  filter(manner_of_death == "Natural" | manner_of_death == "N/A")%>%
  anti_join(master, by = "case_number")


################################################################################
############################## NATURAL CAUSES ##################################
################################################################################


####################################
## 1. INFECTIOUS DISEASE DEATHS ####
####################################

# Create a pattern by joining terms with '|'
pattern <- paste(infection_terms, collapse = "|")

# analyze rows based on "Cause_of_Death" to be consistent with UCD-ICD-10 coding
#in CDC WONDER database for general population
infection_related_cases <- natural_cases[
  grepl(pattern, natural_cases$cause_of_death, ignore.case = TRUE) , ]

#clean up columns and make sure that cases aren't double-counted based on
#case numbers of previous decedents
infection_related_cases = infection_related_cases%>%
  mutate(Cause = "Infection")%>%
  anti_join(master, by = "case_number")%>%
  dplyr::select(case_number, Age, Age_Group, Gender, Race_Ethnicity, year, Cause,
                cause_of_death, other_significant_condition)

#join infectious disease deaths to master dataframe
master = rbind(infection_related_cases, master)


########################################################
### 2. NON-INFECTIOUS ILLNESS/CHRONIC DISEASE DEATHS ###
########################################################

#filter remaining natural causes deaths and make sure they don't overlap with
#infectious disease cases
chronic_related_cases = natural_cases%>%
  anti_join(infection_related_cases, by = "case_number")

#clean up columns and ensure nothing in the master dataframe is double-counted
chronic_related_cases = chronic_related_cases%>%
  mutate(Cause = "Chronic")%>%
  anti_join(master, by = "case_number")%>%
  dplyr::select(case_number, Age, Age_Group, Gender, Race_Ethnicity, year, Cause,
                cause_of_death, other_significant_condition)

#bind new rows to master dataframe
master = rbind(chronic_related_cases, master)


################################################################################
########################## ACCIDENTAL DEATHS ###################################
################################################################################


###############################################
#### 1. DEATHS FROM ACCIDENTS & INJURIES ######
###############################################

# Create a pattern by joining terms with '|'
pattern <- paste(accident_terms, collapse = "|")

# analyze rows based on "Cause_of_Death" to be consistent with UCD-ICD-10 coding
#in CDC WONDER database for general population
accident_related_cases <- accidental_cases[
  grepl(pattern, accidental_cases$cause_of_death, ignore.case = TRUE) ,]

#clean up columns and ensure none are double-counted in the master dataframe
accident_related_cases = accident_related_cases%>%
  anti_join(master, by = "case_number")%>%
  mutate(Cause = "Accident")%>%
  dplyr::select(case_number, Age, Age_Group, Gender, Race_Ethnicity, year, Cause,
                cause_of_death, other_significant_condition)

#join to master dataframe
master = rbind(accident_related_cases, master)


###############################################
#### 2. OVERDOSE/SUBSTANCE-RELATED DEATHS #####
###############################################

#remaining accidental cases are overdoses/substance-related deaths by
#process of elimination
overdose_related_cases = accidental_cases%>%
  anti_join(accident_related_cases, by = "case_number")

#clean up columns and ensure none are double-counted in the master dataframe
overdose_related_cases = overdose_related_cases%>%
  anti_join(master, by = "case_number")%>%
  mutate(Cause = "Substance")%>%
  dplyr::select(case_number, Age, Age_Group, Gender, Race_Ethnicity, year, Cause,
                cause_of_death, other_significant_condition)

#join to master dataframe
master = rbind(overdose_related_cases, master)


################################################################################
########################### DATA VALIDATION & TOTALS ###########################
################################################################################

length(unique(master$case_number))

table(master$Age_Group)

table(master$Gender)

table(master$Race_Ethnicity)

table(master$year)

table(master$Cause, master$year)

table(master$Cause)

################################################################################
