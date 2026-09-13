# AMR R Analysis
# 03 - AMR CSV Analysis
# Author: Ogbole Michael Ogbole

# ----------------------------
# 1. Load packages
# ----------------------------

library(dplyr)

# ----------------------------
# 2. Import data
# ----------------------------

amr_data <- read.csv("data/amr_example.csv")

# ----------------------------
# 3. Inspect data
# ----------------------------

amr_data
str(amr_data)
summary(amr_data)

# ----------------------------
# 4. Data quality checks
# ----------------------------

table(amr_data$resistance)
table(amr_data$antibiotic)
table(amr_data$species)

# Check missing values
colSums(is.na(amr_data))

# Dataset dimensions
dim(amr_data)

# ----------------------------
# 5. Overall AMR prevalence
# ----------------------------

mean(amr_data$resistance) * 100

# ----------------------------
# 6. AMR by antibiotic
# ----------------------------

antibiotic_summary <- amr_data %>%
  group_by(antibiotic) %>%
  summarise(
    total = n(),
    resistant = sum(resistance),
    resistance_rate = mean(resistance) * 100
  )

antibiotic_summary

# ----------------------------
# 7. AMR by bacterial species
# ----------------------------

species_summary <- amr_data %>%
  group_by(species) %>%
  summarise(
    total = n(),
    resistant = sum(resistance),
    resistance_rate = mean(resistance) * 100
  )

species_summary