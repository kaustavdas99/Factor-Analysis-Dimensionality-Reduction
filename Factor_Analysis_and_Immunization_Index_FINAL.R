############################################################
# Libraries
############################################################
library(psych)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(writexl)

############################################################
# Load & prepare data
############################################################
df <- read_excel("Child_Immunisation_FINAL.xlsx") %>%
  select(
    Month, District, Values,
    `Percentage of BCG`,
    `Percentage of OPV 0 (Birth Dose)`,
    `Percentage of Hepatitis-B0 (Birth Dose)`
  )

X <- df %>%
  select(`Percentage of BCG`,
         `Percentage of OPV 0 (Birth Dose)`,
         `Percentage of Hepatitis-B0 (Birth Dose)`)

############################################################
# VALIDATION: Factor suitability
############################################################
print(KMO(X)$MSA)
X <- scale(X)
KMO(X)
print(cortest.bartlett(cor(X), n = nrow(X)))
print(cor(X))

############################################################
# FACTOR ANALYSIS (GLOBAL MODEL)
############################################################
fa_res <- fa(
  X,
  nfactors = 1,
  fm = "minres",
  rotate = "none",
  scores = "regression"
)

print(fa_res$loadings)
print(fa_res$Vaccounted)

df$Immunisation_Index <- as.numeric(fa_res$scores)

############################################################
# DISTRICT & MONTH AGGREGATION
############################################################
district_index <- df %>%
  group_by(District) %>%
  summarise(Index = mean(Immunisation_Index, na.rm = TRUE),
            .groups = "drop")

month_index <- df %>%
  group_by(Month) %>%
  summarise(Index = mean(Immunisation_Index, na.rm = TRUE),
            .groups = "drop")

############################################################
# RISK CLASSIFICATION (ONE LOGIC ONLY – QUANTILES)
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
# DESCRIPTIVE DIAGNOSTICS (NOT INFERENTIAL)
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
# DISTRICT BAR PLOT
############################################################
district_index$Risk <- factor(
  district_index$Risk,
  levels = c("High Risk", "Moderate Risk", "Low Risk")
)

ggplot(district_index,
       aes(x = reorder(District, Index),
           y = Index,
           fill = Risk)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("High Risk" = "red",
                               "Moderate Risk" = "orange",
                               "Low Risk" = "green")) +
  labs(
    title = "District-wise Immunisation Index",
    subtitle = "Quantile-based Risk Classification",
    x = "District",
    y = "Immunisation Index"
  ) +
  theme_minimal(base_size = 14)

###################################
# CUMMULATIVE INDEX VALUES AFTER FA
###################################

# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(psych)

# Read dataset
df <- read_excel("Child_Immunisation_FINAL.xlsx") %>%
  select(
    Month, District, Values,
    `Percentage of BCG`,
    `Percentage of OPV 0 (Birth Dose)`,
    `Percentage of Hepatitis-B0 (Birth Dose)`
  )

# Fill missing district-month-Values combinations with 0
df_filled <- df %>%
  tidyr::complete(Month, District, Values, fill = list(
    `Percentage of BCG` = 0,
    `Percentage of OPV 0 (Birth Dose)` = 0,
    `Percentage of Hepatitis-B0 (Birth Dose)` = 0
  ))

# Aggregate percentages across Values to get district-month level
df_agg <- df_filled %>%
  group_by(Month, District) %>%
  summarise(across(starts_with("Percentage"), mean, na.rm = TRUE)) %>%
  ungroup()

# Prepare matrix for factor analysis
X <- df_agg %>% select(starts_with("Percentage"))

# Run factor analysis (1 factor)
fa_res <- fa(X, nfactors = 1, rotate = "none", fm = "ml")

# Extract factor scores as total immunisation index
df_agg$Immunisation_Index <- fa_res$scores

# Final output: district × month × total index
final_index <- df_agg %>%
  select(Month, District, Immunisation_Index)

# Result
print(final_index)

library(openxlsx)
write.xlsx(final_index, "Child_Immunisation_Total_Index_2.xlsx")


#####################################################
# RURAL URBAN PUBLIC PRIVATE VALUES OF INDEX AFTER FA 
#####################################################

library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(purrr)  

# Read dataset
df <- read_excel("Child_Immunisation_FINAL.xlsx") %>%
  select(
    Month, District, Values,
    `Percentage of BCG`,
    `Percentage of OPV 0 (Birth Dose)`,
    `Percentage of Hepatitis-B0 (Birth Dose)`
  )

# Fill missing district-month-Values combinations with 0
df_filled <- df %>%
  tidyr::complete(Month, District, Values, fill = list(
    `Percentage of BCG` = 0,
    `Percentage of OPV 0 (Birth Dose)` = 0,
    `Percentage of Hepatitis-B0 (Birth Dose)` = 0
  ))

# Split by Values and compute separate factor scores
categories <- unique(df_filled$Values)

index_list <- lapply(categories, function(cat){
  df_cat <- df_filled %>% filter(Values == cat)
  
  # Prepare matrix for factor analysis
  X <- df_cat %>% select(starts_with("Percentage"))
  
  # Run factor analysis (1 factor)
  fa_res <- fa(X, nfactors = 1, rotate = "none", fm = "ml")
  
  # Extract factor scores
  df_cat$Index <- fa_res$scores
  
  # Keep Month, District, Index, rename Index to category name
  df_cat %>% select(Month, District, Index) %>%
    rename(!!cat := Index)
})

# Combine all category indices into a single wide table
final_index_wide <- index_list %>%
  reduce(full_join, by = c("Month", "District"))

# Result
print(final_index_wide)
 write.csv(final_index_wide, "Final_Index.csv")

#############################################################

library(dplyr)
library(ggplot2)
library(tidyr)
 
 # Compute cumulative (average) index per district
 district_risk <- final_index_wide %>%
   group_by(District) %>%
   summarise(Average_Index = mean(c_across(where(is.numeric)), na.rm = TRUE),
             .groups = "drop")
 
 # Assign risk based on quantiles (lower index = higher risk)
 district_risk <- district_risk %>%
   mutate(Risk = cut(Average_Index,
                     breaks = quantile(Average_Index, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                     labels = c("High Risk", "Moderate Risk", "Low Risk"),
                     include.lowest = TRUE))
 
 # Make Risk a factor with proper order
 district_risk$Risk <- factor(district_risk$Risk,
                              levels = c("High Risk", "Moderate Risk", "Low Risk")) 
 

ggplot(district_risk, aes(x = reorder(District, Average_Index), 
                          y = Average_Index, fill = Risk)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Average_Index, 2),
                hjust = ifelse(Average_Index >= 0, -0.1, 1.1)), 
            size = 4, color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("High Risk" = "red",
                               "Moderate Risk" = "orange",
                               "Low Risk" = "green")) +
  labs(title = "District-wise Cumulative Immunisation Risk",
       subtitle = "Lower immunisation index → Higher risk",
       x = "District",
       y = "Cumulative Immunisation Index - (Birth Dose)",
       fill = "Risk Level") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",           
    legend.justification = "center",   
    legend.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5),      
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  expand_limits(y = c(min(district_risk$Average_Index) * 1.1, 
                      max(district_risk$Average_Index) * 1.1))


# Data
print(district_risk)
library(knitr)
kable(district_risk)

library(writexl)
write_xlsx(district_risk, "District_Cumulative_Immunisation_Risk.xlsx")


###############################################################
#Rural Urban Public Private Together
###############################################################
library(ggplot2)
library(dplyr)
library(tidyr)

# Dataset
df <- read.csv("Child_Immunization_Index_FA_Category.csv")  

df_long <- df %>%
  pivot_longer(cols = c("Private_Facility", "Public_Facility", "Rural", "Urban"),
               names_to = "Category",
               values_to = "Index")

# District-wise average index across all months
df_plot <- df_long %>%
  group_by(District, Category) %>%
  summarise(
    Index = mean(Index, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(df_plot, aes(x = reorder(District, Index), y = Index, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Index, 2)),
            position = position_dodge(width = 0.8),
            hjust = ifelse(df_plot$Index >= 0, -0.1, 1.1), 
            size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Private_Facility" = "#e41a1c",
                               "Public_Facility" = "#377eb8",
                               "Rural" = "#4daf4a",
                               "Urban" = "#984ea3")) +
  labs(title = "District-wise Immunisation Index by Category",
       subtitle = "Private/Public Facilities and Rural/Urban Factor Scores",
       x = "District",
       y = "Immunisation Index - (Birth Dose)",
       fill = "Category") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  expand_limits(y = c(min(df_plot$Index) * 1.1, max(df_plot$Index) * 1.1))


#Save Values to Excel

library(dplyr)
library(tidyr)
library(writexl)

df <- read.csv("Child_Immunization_Index_FA_Category.csv")

df_long <- df %>%
  pivot_longer(
    cols = c("Private_Facility", "Public_Facility", "Rural", "Urban"),
    names_to = "Category",
    values_to = "Index"
  )

###############################################################
#Separte Plots ######
###############################################################

# ============================================================
# District-wise Average Immunisation Index by Category
# (Rural / Urban / Public / Private)
# ============================================================

# -----------------------------
# Load required libraries
# -----------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# -----------------------------
# Read the dataset
# -----------------------------

df <- read.csv("Child_Immunization_Index_FA_Category.csv")

df_long <- df %>%
  pivot_longer(
    cols = c(Private_Facility, Public_Facility, Rural, Urban),
    names_to = "Category",
    values_to = "Index"
  )

# -----------------------------
# District-wise average across ALL months
# -----------------------------
df_plot <- df_long %>%
  group_by(District, Category) %>%
  summarise(
    Index = mean(Index, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# Rural vs Urban (Average Index)
# ============================================================

df_rural_urban <- df_plot %>%
  filter(Category %in% c("Rural", "Urban"))

ggplot(df_rural_urban,
       aes(x = reorder(District, Index),
           y = Index,
           fill = Category)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = round(Index, 2)),
    position = position_dodge(width = 0.8),
    hjust = ifelse(df_rural_urban$Index >= 0, -0.1, 1.1),
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Rural" = "#4daf4a",
    "Urban" = "#984ea3"
  )) +
  labs(
    title = "District-wise Average Rural vs Urban Immunisation Index",
    subtitle = "Average across all months (Birth Dose Index)",
    x = "District",
    y = "Immunisation Index",
    fill = "Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# ============================================================
# Public vs Private Facility (Average Index)
# ============================================================

df_pub_priv <- df_plot %>%
  filter(Category %in% c("Public_Facility", "Private_Facility"))

ggplot(df_pub_priv,
       aes(x = reorder(District, Index),
           y = Index,
           fill = Category)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = round(Index, 2)),
    position = position_dodge(width = 0.8),
    hjust = ifelse(df_pub_priv$Index >= 0, -0.1, 1.1),
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Public_Facility" = "#377eb8",
    "Private_Facility" = "#e41a1c"
  )) +
  labs(
    title = "District-wise Average Public vs Private Facility Immunisation Index",
    subtitle = "Average across all months (Birth Dose Index)",
    x = "District",
    y = "Immunisation Index",
    fill = "Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


library(writexl)
write_xlsx(df_plot,
           "District_Average_Immunisation_Index_By_Category.xlsx")


###########################################################

#TESTS

############################################################
# STATISTICAL RESULTS – FACTOR MODEL VALIDATION
############################################################

cat("\n===== FACTOR ADEQUACY TESTS =====\n")

# KMO
kmo_res <- KMO(X)
cat("KMO (Overall):", round(kmo_res$MSA, 3), "\n")

# Bartlett’s test
bart_res <- cortest.bartlett(cor(X), n = nrow(X))
cat("Bartlett Chi-square:", round(bart_res$chisq, 2), "\n")
cat("df:", bart_res$df, "\n")
cat("p-value:", format.pval(bart_res$p.value), "\n")

cat("\n===== FACTOR MODEL RESULTS =====\n")
print(fa_res$loadings)
cat("Variance explained by factor:",
    round(fa_res$Vaccounted["Proportion Var", 1] * 100, 2), "%\n")

############################################################
# STATISTICAL RESULTS – INDEX DISTRIBUTION
############################################################

cat("\n===== SHAPIRO-WILK TEST (District Index) =====\n")

shapiro_res <- shapiro.test(district_index$Index)
cat("W statistic:", round(shapiro_res$statistic, 3), "\n")
cat("p-value:", format.pval(shapiro_res$p.value), "\n")


############################################################
# STATISTICAL RESULTS – RURAL vs URBAN
############################################################

cat("\n===== WILCOXON TEST: RURAL vs URBAN =====\n")

rural_urban_test <- wilcox.test(
  final_index_wide$Rural,
  final_index_wide$Urban,
  paired = TRUE,
  exact = FALSE
)

print(rural_urban_test)


############################################################
# STATISTICAL RESULTS – PUBLIC vs PRIVATE
############################################################

cat("\n===== WILCOXON TEST: PUBLIC vs PRIVATE =====\n")

public_private_test <- wilcox.test(
  final_index_wide$Public_Facility,
  final_index_wide$Private_Facility,
  paired = TRUE,
  exact = FALSE
)

print(public_private_test)


############################################################
# STATISTICAL RESULTS – RISK GROUP DIFFERENCES
############################################################

cat("\n===== KRUSKAL-WALLIS TEST (Risk Groups) =====\n")

risk_test <- kruskal.test(Index ~ Risk, data = district_index)
print(risk_test)

############################################################


