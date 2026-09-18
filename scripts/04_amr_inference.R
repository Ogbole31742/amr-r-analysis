# AMR R Analysis
# 04 - AMR Statistical Inference
# Author: Ogbole Michael Ogbole

# ----------------------------
# 1. Load packages
# ----------------------------

library(dplyr)

# ----------------------------
# 2. Import data
# ----------------------------

amr_100 <- read.csv("data/amr_100_isolates.csv")

# Convert categorical variables to factors

amr_100$species <- factor(amr_100$species)
amr_100$antibiotic <- factor(amr_100$antibiotic)
amr_100$sample_type <- factor(amr_100$sample_type)

# ----------------------------
# 3. Overall AMR prevalence
# ----------------------------

overall_amr <- binom.test(
  sum(amr_100$resistance),
  length(amr_100$resistance)
)

print(overall_amr)

# ----------------------------
# 4. Species vs resistance
# ----------------------------

species_table <- table(
  amr_100$species,
  amr_100$resistance
)

print(species_table)

# Chi-square test

chisq_species <- chisq.test(species_table)

print(chisq_species)

# Fisher's exact test

fisher_species <- fisher.test(species_table)

print(fisher_species)

# ----------------------------
# 5. Antibiotic vs resistance
# ----------------------------

antibiotic_table <- table(
  amr_100$antibiotic,
  amr_100$resistance
)

print(antibiotic_table)

chisq_antibiotic <- chisq.test(antibiotic_table)

print(chisq_antibiotic)

# ----------------------------
# 6. Logistic regression
# ----------------------------

model_full <- glm(
  resistance ~ species + antibiotic + year + sample_type,
  data = amr_100,
  family = binomial
)

print(summary(model_full))

# ----------------------------
# 7. Odds ratios
# ----------------------------

odds_ratios <- exp(coef(model_full))

print(odds_ratios)

# ----------------------------
# 8. 95% confidence intervals
# ----------------------------

confidence_intervals <- exp(
  confint(model_full)
)

print(confidence_intervals)

# ----------------------------
# 9. Logistic regression results table
# ----------------------------

model_results <- data.frame(
  Predictor = names(coef(model_full)),
  Odds_Ratio = exp(coef(model_full)),
  CI_Lower = exp(confint(model_full))[, 1],
  CI_Upper = exp(confint(model_full))[, 2],
  P_value = summary(model_full)$coefficients[, 4]
)

model_results$Odds_Ratio <- round(
  model_results$Odds_Ratio, 2
)

model_results$CI_Lower <- round(
  model_results$CI_Lower, 2
)

model_results$CI_Upper <- round(
  model_results$CI_Upper, 2
)

model_results$P_value <- round(
  model_results$P_value, 3
)

print(model_results)

# ----------------------------
# 10. Model comparison
# ----------------------------

anova_result <- anova(
  model_full,
  test = "Chisq"
)

print(anova_result)

# ----------------------------------------
# Temporal logistic regression by antibiotic
# ----------------------------------------

amp_model <- glm(
  resistance ~ year,
  data = amr_100[amr_100$antibiotic == "AMP", ],
  family = binomial
)

cip_model <- glm(
  resistance ~ year,
  data = amr_100[amr_100$antibiotic == "CIP", ],
  family = binomial
)

ctx_model <- glm(
  resistance ~ year,
  data = amr_100[amr_100$antibiotic == "CTX", ],
  family = binomial
)

gen_model <- glm(
  resistance ~ year,
  data = amr_100[amr_100$antibiotic == "GEN", ],
  family = binomial
)

# Extract odds ratios, confidence intervals and p-values

temporal_results <- data.frame(
  antibiotic = c("AMP", "CIP", "CTX", "GEN"),
  OR = c(
    exp(coef(amp_model)["year"]),
    exp(coef(cip_model)["year"]),
    exp(coef(ctx_model)["year"]),
    exp(coef(gen_model)["year"])
  ),
  CI_lower = c(
    exp(confint(amp_model)["year", 1]),
    exp(confint(cip_model)["year", 1]),
    exp(confint(ctx_model)["year", 1]),
    exp(confint(gen_model)["year", 1])
  ),
  CI_upper = c(
    exp(confint(amp_model)["year", 2]),
    exp(confint(cip_model)["year", 2]),
    exp(confint(ctx_model)["year", 2]),
    exp(confint(gen_model)["year", 2])
  ),
  p_value = c(
    summary(amp_model)$coefficients["year", "Pr(>|z|)"],
    summary(cip_model)$coefficients["year", "Pr(>|z|)"],
    summary(ctx_model)$coefficients["year", "Pr(>|z|)"],
    summary(gen_model)$coefficients["year", "Pr(>|z|)"]
  )
)

print(temporal_results)

# Save results

write.csv(
  temporal_results,
  "results/temporal_logistic_regression_results.csv",
  row.names = FALSE
)