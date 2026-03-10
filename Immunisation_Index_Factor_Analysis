############################################################
# Childhood Immunisation Index using Factor Analysis
# Author: Kaustav Das
# Description:
# This script constructs a composite childhood immunisation
# performance index using factor analysis based on
# birth-dose vaccination indicators.
############################################################


############################################################
# 1. Load Required Libraries
############################################################

library(psych)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(writexl)
library(openxlsx)


############################################################
# 2. Load Dataset
############################################################

df <- read_excel("Child_Immunisation_FINAL.xlsx") %>%
  select(
    Month,
    District,
    Values,
    `Percentage of BCG`,
    `Percentage of OPV 0 (Birth Dose)`,
    `Percentage of Hepatitis-B0 (Birth Dose)`
  )


############################################################
# 3. Prepare Data for Factor Analysis
############################################################

X <- df %>%
  select(
    `Percentage of BCG`,
    `Percentage of OPV 0 (Birth Dose)`,
    `Percentage of Hepatitis-B0 (Birth Dose)`
  )

X_scaled <- scale(X)


############################################################
# 4. Factor Suitability Diagnostics
############################################################

# Kaiser-Meyer-Olkin Test
print(KMO(X_scaled))

# Bartlett's Test of Sphericity
print(cortest.bartlett(cor(X_scaled), n = nrow(X_scaled)))

# Correlation matrix
print(cor(X_scaled))


############################################################
# 5. Factor Analysis Model
############################################################

fa_model <- fa(
  X_scaled,
  nfactors = 1,
  fm = "minres",
  rotate = "none",
  scores = "regression"
)

print(fa_model$loadings)
print(fa_model$Vaccounted)

df$Immunisation_Index <- as.numeric(fa_model$scores)


############################################################
# 6. District Level Immunisation Index
############################################################

district_index <- df %>%
  group_by(District) %>%
  summarise(
    Index = mean(Immunisation_Index, na.rm = TRUE),
    .groups = "drop"
  )


############################################################
# 7. Month-wise Immunisation Index
############################################################

month_index <- df %>%
  group_by(Month) %>%
  summarise(
    Index = mean(Immunisation_Index, na.rm = TRUE),
    .groups = "drop"
  )


############################################################
# 8. District Risk Classification
############################################################

district_index <- district_index %>%
  mutate(
    Risk = cut(
      Index,
      breaks = quantile(Index, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
      labels = c("High Risk", "Moderate Risk", "Low Risk"),
      include.lowest = TRUE
    )
  )


############################################################
# 9. Descriptive Statistics by Risk Group
############################################################

risk_summary <- district_index %>%
  group_by(Risk) %>%
  summarise(
    Mean = mean(Index),
    SD = sd(Index),
    N = n(),
    CI_lower = Mean - qt(0.975, N - 1) * SD / sqrt(N),
    CI_upper = Mean + qt(0.975, N - 1) * SD / sqrt(N),
    .groups = "drop"
  )

print(risk_summary)


############################################################
# 10. District Performance Visualization
############################################################

district_index$Risk <- factor(
  district_index$Risk,
  levels = c("High Risk", "Moderate Risk", "Low Risk")
)

ggplot(
  district_index,
  aes(x = reorder(District, Index), y = Index, fill = Risk)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "High Risk" = "red",
      "Moderate Risk" = "orange",
      "Low Risk" = "green"
    )
  ) +
  labs(
    title = "District-wise Immunisation Index",
    subtitle = "Quantile-based Risk Classification",
    x = "District",
    y = "Immunisation Index"
  ) +
  theme_minimal(base_size = 14)


############################################################
# 11. Export Outputs
############################################################

write_xlsx(district_index, "District_Immunisation_Index.xlsx")
write_xlsx(month_index, "Monthwise_Immunisation_Index.xlsx")
write_xlsx(risk_summary, "Risk_Group_Summary.xlsx")


############################################################
# End of Script
############################################################
