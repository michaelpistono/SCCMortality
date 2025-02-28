## Analysis algorithm ###

library(reshape2)
library(tidyverse)

########################## Accidental Injury #################################
##############################################################################


#define keywords to search for in cause of death column
accident_keywords = c(
  "pedestrian", "collision", "drowning", "hypothermia", "gunshot", "monoxide", 
  "exposure", "multiple", "injur", "thermal", "inhalation")

#create T/F column by searching cause of death column for above keywords
for (keyword in accident_keywords){
  accident_related_cases[[keyword]] <- grepl(keyword, 
                                             accident_related_cases$cause_of_death, 
                                             ignore.case = TRUE)   
}

#sort accident types for plotting by using the newly created T/F columns
accident_related_cases <- accident_related_cases %>%
  filter(year < 2024 & Age > 0) %>%
  mutate(Pedestrian = (pedestrian == TRUE),
         Collision = !Pedestrian & (collision == TRUE),
         Drowning = (drowning == TRUE),
         Hypothermia = (hypothermia ==TRUE | exposure == TRUE),
         Unspecified =
           !Pedestrian & !Collision & !Drowning &((multiple == TRUE & injur == TRUE) | thermal == TRUE),
         Unhoused = 1
  )

#ensure counts are integers instead of T/F
accident_related_cases$Pedestrian = 
  as.integer(accident_related_cases$Pedestrian)
accident_related_cases$Collision = 
  as.integer(accident_related_cases$Collision)
accident_related_cases$Drowning = 
  as.integer(accident_related_cases$Drowning)
accident_related_cases$Unspecified = 
  as.integer(accident_related_cases$Unspecified)
accident_related_cases$Hypothermia = 
  as.integer(accident_related_cases$Hypothermia)
accident_related_cases$monoxide = 
  as.integer(accident_related_cases$monoxide)

#rename variable
accident_related_cases <- accident_related_cases %>%
  rename("Carbon Monoxide" = "monoxide")

#create dataframe from annual counts for plot
annual_accident = data.frame(table(accident_related_cases$year))

#choose set of variables for plotting
accident_variables <- c("Pedestrian", 
                        "Drowning", 
                        "Collision", 
                        "Hypothermia",
                        "Carbon Monoxide")

#aggregate count by type
accident_counts_by_year <- accident_related_cases %>%
  group_by(year) %>%
  summarise(across(all_of(accident_variables), sum, na.rm = TRUE))%>%
  mutate(Total = annual_accident$Freq)

#rename for plot
accident_counts_by_year = accident_counts_by_year%>%
  rename("Motor Vehicle Accident" = "Collision",
         "Pedestrian Accident" = "Pedestrian")

#create transposed table for plotting
counts_long_accident <- melt(accident_counts_by_year, id.vars = "year", 
                             variable.name = "Injury", value.name = "Count")%>%
  filter(Injury != "Total")



#plot annual counts of accidents by type identified
ggplot(counts_long_accident, aes(x = as.numeric(year), y = Count, color = Injury, group = Injury)) +
  geom_line(size = 0.75) +
  geom_point(size = 2) +
  labs(
    title = "Mortality from Accident/Injury in the Unhoused Population",
    x = "Year",
    y = "Count",
    color = "Injury"
  ) +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


########################## Non-infectious Disease #############################
##############################################################################


#identify keywords to filter
chronic_keywords = c(
  "neoplasm", "malignan", "metastat", "cancer", "renal", "obstructive", "cardiac", "athlero",
  "rosis", "coronary", "rheumat", "kidney", "COPD", "congestive", "cirrhosis", "liver",
  "steato", "hypertens", "congestive", "intoxication", "covid", "ethanol", "alcohol",
  "diabet", "obesity", "mixed", "combined", "drug", "toxicity", "amphetamine", "fentanyl",
  "toxic", "cocaine", "heroin", "substance", "sarcoidosis", "dementia", "seizure", "pulmo",
  "alzhe", "blunt", "force", "sepsis", "bacteremia", "hypothermia", "septic", "asthma", "arrest",
  "exposure", "thermal", "prematurity", "epiploic")

#create new T/F columns by looking for keywords in the cause of death narrative
for (keyword in chronic_keywords){
  chronic_related_cases[[keyword]] <- grepl(keyword, chronic_related_cases$cause_of_death, 
                                            ignore.case = TRUE)   
}

#sort the newly created T/F columns 
chronic_related_cases <- chronic_related_cases %>%
  filter(year < 2024 & Age > 0) %>%
  mutate(Covid = (covid == TRUE), 
         Substance = (amphetamine == TRUE | ethanol == TRUE | alcohol == TRUE |
                        (combined == TRUE & toxic == TRUE) |
                        (mixed == TRUE & toxic == TRUE) |
                        (amphetamine == TRUE & (fentanyl == TRUE)) |
                        (heroin == TRUE) |
                        (cocaine == TRUE)),
         Other = (blunt == TRUE | force == TRUE | hypothermia == TRUE | sepsis == TRUE | bacteremia == TRUE | septic == TRUE | exposure == TRUE | thermal == TRUE),
         Cancer = (!Substance & !Covid & !Other & (cancer == TRUE | neoplasm == TRUE | malignan == TRUE | metastat == TRUE)),
         Cardiac = (!Substance & !Covid & !Other & (cardiac == TRUE | coronary == TRUE | congestive == TRUE | athlero == TRUE | hypertens ==TRUE | arrest == TRUE)),
         Renal = (!Substance & !Covid & !Other & (renal == TRUE | kidney == TRUE)),
         Diabetes = (!Substance & !Covid & !Other & (diabet == TRUE)),
         Hepatic = (!Substance & !Covid & !Other & (cirrhosis == TRUE | liver ==TRUE | steato == TRUE)),
         Respiratory =  (!Substance & !Covid & !Other & (obstructive == TRUE | sarcoidosis == TRUE | pulmo == TRUE | asthma == TRUE)),
         Neurological = (!Substance & !Covid & !Other & (dementia ==TRUE | seizure == TRUE | alzhe == TRUE)),
         Unhoused = 1
  )

#ensure no COVID-19, overdose cases or accidents are included
chronic_related_cases = chronic_related_cases%>%
  filter(!(Covid | Substance | Other))


#ensure T/F variables are integers
chronic_related_cases$Cancer = 
  as.integer(chronic_related_cases$Cancer)
chronic_related_cases$Cardiac = 
  as.integer(chronic_related_cases$Cardiac)
chronic_related_cases$Renal = 
  as.integer(chronic_related_cases$Renal)
chronic_related_cases$Diabetes = 
  as.integer(chronic_related_cases$Diabetes)
chronic_related_cases$Hepatic = 
  as.integer(chronic_related_cases$Hepatic)
chronic_related_cases$Respiratory = 
  as.integer(chronic_related_cases$Respiratory)
chronic_related_cases$Neurological = 
  as.integer(chronic_related_cases$Neurological)

#rename variable
chronic_related_cases = chronic_related_cases%>%
  rename(Cardiovascular = "Cardiac")


#create dataframe from annual counts
annual_chronic = data.frame(table(chronic_related_cases$year))


#sort keywords further
chronic_variables <- c("Cancer", "Cardiovascular", "Renal", "Diabetes", "Hepatic", "Respiratory", "Neurological")

#aggregate causes annually for plot
chronic_counts_by_year <- chronic_related_cases %>%
  group_by(year) %>%
  summarise(across(all_of(chronic_variables), sum, na.rm = TRUE))%>%
  mutate(Total = annual_chronic$Freq)

#transpose for plot
counts_long_chronic <- melt(chronic_counts_by_year, id.vars = "year", 
                            variable.name = "Illness", value.name = "Count")%>%
  filter(Illness != "Total")


#create plot
ggplot(counts_long_chronic, aes(x = as.numeric(year), y = Count, color = Illness, group = Illness)) +
  geom_line(size = 0.75) +
  geom_point(size = 2) +
  labs(
    title = "Annual Chronic Disease Mortality in the Unhoused Population",
    x = "Year",
    y = "Count",
    color = "Illness"
  ) +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


########################## Infectious Diseases ###############################
##############################################################################


#define keywords to search for in cause of death column
species_keywords = c("COVID", "Hepatitis", "Influenza", "Syncytial", "HIV", "coli", 
                     "pneumoniae", "strep", "staph", "resist", "haemo", 
                     "kleb", "omonas", "clostrid")

#search for keywords in cause of death column and create T/F column for each
for (keyword in species_keywords){
  infection_related_cases[[keyword]] <- grepl(keyword, infection_related_cases$cause_of_death, 
                                              ignore.case = TRUE) 
}

#aggregate found keywords into disease types by T/F
infection_related_cases = infection_related_cases%>%
  rename(RSV = "Syncytial",
         E_Coli = "coli",
         Klebsiella = "kleb",
         Staphylococcus = "staph",
         Haemophilus = "haemo",
         Pseudomonas = "omonas",
         Clostridium = "clostrid",
         Anti_Microbial_Resistant = "resist")%>%
  mutate(Pneumococcal = !Klebsiella & (pneumoniae == TRUE),
         Streptococcus = !Pneumococcal & (strep == TRUE))

#ensure T/F variables are integers
infection_related_cases$Hepatitis = 
  as.integer(infection_related_cases$Hepatitis)
infection_related_cases$Influenza = 
  as.integer(infection_related_cases$Influenza)
infection_related_cases$RSV = 
  as.integer(infection_related_cases$RSV)
infection_related_cases$HIV = 
  as.integer(infection_related_cases$HIV)

infection_related_cases$E_Coli = 
  as.integer(infection_related_cases$E_Coli)
infection_related_cases$Pneumococcal = 
  as.integer(infection_related_cases$Pneumococcal)
infection_related_cases$Streptococcus = 
  as.integer(infection_related_cases$Streptococcus)
infection_related_cases$Staphylococcus = 
  as.integer(infection_related_cases$Staphylococcus)
infection_related_cases$Haemophilus = 
  as.integer(infection_related_cases$Haemophilus)
infection_related_cases$Anti_Microbial_Resistant = 
  as.integer(infection_related_cases$Anti_Microbial_Resistant)
infection_related_cases$Klebsiella = 
  as.integer(infection_related_cases$Klebsiella)
infection_related_cases$Pseudomonas = 
  as.integer(infection_related_cases$Pseudomonas)
infection_related_cases$Clostridium= 
  as.integer(infection_related_cases$Clostridium)

#rename variables
infection_related_cases = infection_related_cases%>%
  rename("Group A/B/C Strep" = "Streptococcus",
         "Staph/MRSA" = "Staphylococcus",
         "E. coli" = "E_Coli")

#ensure only non-covid cases are counted
infection_related_cases = infection_related_cases%>%
  filter(COVID == FALSE)


# Define further variables of interest
variables <- c("E. coli", "Pneumococcal", "Group A/B/C Strep", "Staph/MRSA")
virus_variables <- c("Influenza", "RSV", "Hepatitis")

# Initialize an empty data frame to store counts
counts_by_year <- data.frame(Year = unique(infection_related_cases$year))


# Summarize the counts of each variable by year
counts_by_year <- infection_related_cases %>%
  group_by(year) %>%
  summarise(across(all_of(variables), sum, na.rm = TRUE))

virus_counts_by_year <- infection_related_cases %>%
  group_by(year) %>%
  summarise(across(all_of(virus_variables), sum, na.rm = TRUE))

# Melt the data into long format for plotting, seperate viral from bacterial infections
counts_long.b <- melt(counts_by_year, id.vars = "year",
                      variable.name = "Pathogen", value.name = "Count")
counts_long.v <- melt(virus_counts_by_year, id.vars = "year", 
                      variable.name = "Pathogen", value.name = "Count")

counts_long.b = counts_long.b%>%
  mutate(Status = "Bacterial")

counts_long.v = counts_long.v%>%
  mutate(Status = "Viral")

counts_long = rbind(counts_long.b, counts_long.v)


# Create the line plot using ggplot2
ggplot(counts_long, aes(x = as.numeric(year), y = Count, fill = Pathogen, 
                        group = Pathogen)) +
  geom_bar(stat = "identity", aes(color = Pathogen)) +
  labs(
    title = "Select Pathogens Identified by ME-C in the Unhoused Population",
    x = "Year",
    y = "Count",
    color = "Pathogen"
  ) +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )+
  facet_grid(~Status)



#create a line plot
ggplot(counts_long, aes(x = as.numeric(year), y = Count, color = Pathogen, group = Pathogen)) +
  geom_line(size = 0.75) +
  geom_point(size = 2) +
  labs(
    title = "Annual Non-COVID Infectious Disease Mortality in the Unhoused Population",
    x = "Year",
    y = "Count",
    color = "Pathogen"
  ) +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  facet_grid(Status ~ .)


#create plot by syndrome
#create columns corresponding to keywords in the cause of death and significant
#conditions columns

#use same logic as before to filter by syndrome instead of pathogen
syndrome_keywords = c("pneumonia", "sepsis", "septic", "endocarditis", "mening",
                      "fascii", "bacteremia")

for (keyword in syndrome_keywords){
  infection_related_cases[[keyword]] <- grepl(keyword, infection_related_cases$cause_of_death, 
                                              ignore.case = TRUE) 
}

#turn the true/false into binary 0/1 and rename columns
infection_related_cases = infection_related_cases%>%
  mutate(Sepsis = (septic == TRUE | sepsis == TRUE),
         Pneumonia = (pneumonia == TRUE),
         Bacteremia = (bacteremia == TRUE)
  )%>%
  rename(
    Endocarditis = "endocarditis",
    Meningitis = "mening",
    Fasciitis = "fascii")

infection_related_cases$Pneumonia = 
  as.integer(infection_related_cases$Pneumonia)
infection_related_cases$Sepsis = 
  as.integer(infection_related_cases$Sepsis)
infection_related_cases$Endocarditis = 
  as.integer(infection_related_cases$Endocarditis)
infection_related_cases$Meningitis = 
  as.integer(infection_related_cases$Meningitis)
infection_related_cases$Fasciitis = 
  as.integer(infection_related_cases$Fasciitis)
infection_related_cases$Bacteremia = 
  as.integer(infection_related_cases$Bacteremia)

infection_related_cases = infection_related_cases%>%
  rename("Necrotizing Fasciitis" = "Fasciitis",
         "Sepsis/Septic Shock" = "Sepsis",
         "Infective Endocarditis" = "Endocarditis")

# Define variables of interest
syndrome_variables <- c("Sepsis/Septic Shock", "Pneumonia", "Bacteremia", 
                        "Infective Endocarditis", "Meningitis", "Necrotizing Fasciitis" )


# Initialize an empty data frame to store counts
syndrome_counts_by_year <- data.frame(Year = unique(infection_related_cases$year))


# Summarize the counts of each variable by year

syndrome_counts_by_year <- infection_related_cases %>%
  group_by(year) %>%
  summarise(across(all_of(syndrome_variables), sum, na.rm = TRUE))



# Melt the data into long format for plotting
counts_long.syndrome <- melt(syndrome_counts_by_year, id.vars = "year", variable.name = "Syndrome", value.name = "Count")


counts_long.syndrome = counts_long.syndrome%>%
  mutate(Status = "Syndrome")



ggplot(counts_long.syndrome, aes(x = as.numeric(year), y = Count, group = Syndrome)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Infectious Disease Mortality by Syndrome in the Unhoused",
    x = "Year",
    y = "Count",
    color = "Syndrome"
  ) +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    name = "Deaths",  # Primary y-axis
    breaks = seq(0, max(counts_long.syndrome$Count, na.rm = TRUE), by = 2),
    
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )+
  facet_wrap(~Syndrome, ncol = 3)



########################## Substance Use/Overdose #############################
##############################################################################



#define keywords to search for in cause of death column
overdose_terms = c("amphetamine", "fentanyl", "anpp", "heroin", "morphine",
                   "methadone", "codeine", "codone", "alcohol", "ethanol", "cocaine",
                   "pcp", "phencyclidine", "tramadol", "benzodiazepine", "zolam",
                   "zepam", "mixed", "combined", "toxic", "hypothermia", "monoxide",
                   "exposure", "drowning", "septic", "ETOH", "alcoholism")


for (keyword in overdose_terms){
  overdose_related_cases[[keyword]] <- grepl(keyword, overdose_related_cases$cause_of_death, 
                                             ignore.case = TRUE)                              
}

#add columns denoting deaths where attributed to a single or combined substance
#and ensuring that substances are not double-counted among the homeless

overdose_related_cases <- overdose_related_cases %>%
  mutate(
    Other = (monoxide == TRUE | hypothermia == TRUE | exposure == TRUE | drowning == TRUE ),
    Polysubstance = !Other&(
      (combined == TRUE & toxic == TRUE) |
        (mixed == TRUE & toxic == TRUE) |
        (amphetamine == TRUE & (fentanyl == TRUE | morphine == TRUE | methadone == TRUE | heroin == TRUE | codeine == TRUE | alcohol == TRUE | ethanol == TRUE | phencyclidine == TRUE | cocaine == TRUE | zolam == TRUE | zepam == TRUE)) |
        (heroin == TRUE & (methadone == TRUE | cocaine == TRUE | phencyclidine == TRUE)) |
        (morphine == TRUE & (methadone == TRUE | alcohol == TRUE | codeine == TRUE | cocaine == TRUE)) |
        (tramadol == TRUE & zepam == TRUE) |
        (codone == TRUE & (cocaine == TRUE | zolam == TRUE)) |
        (fentanyl == TRUE & (cocaine == TRUE | ethanol == TRUE)) |
        (ethanol == TRUE & zepam == TRUE) |
        (phencyclidine == TRUE & cocaine == TRUE)
    ),
    
    Alcohol = !Other&((alcohol == TRUE | ethanol == TRUE | alcoholism == TRUE | ETOH == TRUE)),
    Fentanyl = !Other&(fentanyl == TRUE | anpp == TRUE),
    Opioid = !Other&((heroin == TRUE | morphine == TRUE | methadone == TRUE | tramadol == TRUE | codone == TRUE | codeine == TRUE)),
    Benzodiazepine = !Other&((zolam == TRUE | zepam == TRUE)),
    Methamphetamine = !Other&(amphetamine == TRUE),
    Stimulants = !Other&((cocaine == TRUE | phencyclidine == TRUE)),
    Unhoused = 1
  )

# Function to create the "All" column based on TRUE/FALSE values in specified columns
add_all_column <- function(data) {
  data %>%
    mutate(All = ifelse(Methamphetamine | Fentanyl | Polysubstance | Stimulants | Alcohol | Opioid, TRUE, FALSE))
}

overdose_related_cases <- add_all_column(overdose_related_cases)

#remove accidental deaths
overdose_related_cases = overdose_related_cases%>%filter(!(Other))


#rename variable
overdose_related_cases = overdose_related_cases%>%
  rename("Multiple" = "Polysubstance")

#turn the true/false into binary 0/1 and rename columns
overdose_related_cases$Multiple = 
  as.integer(overdose_related_cases$Multiple)
overdose_related_cases$Alcohol = 
  as.integer(overdose_related_cases$Alcohol)
overdose_related_cases$Fentanyl = 
  as.integer(overdose_related_cases$Fentanyl)
overdose_related_cases$Opioid = 
  as.integer(overdose_related_cases$Opioid)
overdose_related_cases$Benzodiazepine = 
  as.integer(overdose_related_cases$Benzodiazepine)
overdose_related_cases$Methamphetamine = 
  as.integer(overdose_related_cases$Methamphetamine)
overdose_related_cases$Stimulants = 
  as.integer(overdose_related_cases$Stimulants)

#rename variables
overdose_related_cases = overdose_related_cases%>%
  rename("Other Opioid" = "Opioid",
         "Multiple/Polysubstance" = "Multiple")

# Define further variables of interest
variables <- c("Alcohol", "Methamphetamine", "Fentanyl", "Multiple/Polysubstance", "Stimulants",
               "Other Opioid")


# Initialize an empty data frame to store counts
counts_by_year <- data.frame(Year = unique(overdose_related_cases$year))


# Summarize the counts of each variable by year
counts_by_year <- overdose_related_cases %>%
  group_by(year) %>%
  summarise(across(all_of(variables), sum, na.rm = TRUE))


# Melt the data into long format for plotting
counts_long.od <- melt(counts_by_year, id.vars = "year", variable.name = "Substance", value.name = "Count")
counts_long.od = counts_long.od%>%
  mutate(Status = "Substance Use")


#create line plot
ggplot(counts_long.od, aes(x = as.numeric(year), y = Count, color = Substance, group = Substance)) +
  geom_line(size = 0.75) +
  geom_point(size = 2) +
  labs(
    title = "Annual Substance-related Mortality in the Unhoused Population",
    x = "Year",
    y = "Count",
    color = "Substance"
  ) +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))