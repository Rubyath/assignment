rm(list=ls())

library(dplyr)
library(haven)
anc_data <- read_dta("BDNR81FL.dta")
anc_data <- as.data.frame(anc_data)
View(anc_data)

anc_final <- as.data.frame(anc_data %>% filter(v238 != 0) %>% filter (m14 <= 20) %>% filter (!is.na(m14)))
nrow(anc_final)
View(anc_final)

anc_final$anc_4plus <- ifelse(anc_final$m14 >= 4, "4plus", "Incmplt")
anc_final$anc_4plus <- as.factor(anc_final$anc_4plus)
anc_final <- anc_final %>% mutate(anc_provider = ifelse(m2a == 1 | m2b == 1 | m2c == 1 | m2d == 1 | m2e == 1, "skilled_prov", "non_skilled"))
anc_final$anc_provider <- as.factor(anc_final$anc_provider)
anc_final <- anc_final %>% mutate(test_all = ifelse(m42a == 1 & m42c == 1 & m42d == 1 & m42e == 1 & m42m == 1, "tested", "not_tested" ))
anc_final$test_all <- as.factor(anc_final$test_all)
anc_final <- anc_final %>% mutate(quality_anc = ifelse(anc_4plus == "4plus" & anc_provider == "skilled_prov" & test_all == "tested", "Yes", "No"))
anc_final$quality_anc <- as.factor(anc_final$quality_anc)

library(labelled)
attr(anc_final$s115_1, "labels")
anc_final$s115_1 <- factor(anc_final$s115_1, levels = c(0, 1, 2, 3, 4), labels = c("no", "primary_incomplete", "prim_comp", "inc_secondary", "secondary_higher"))

attr(anc_final$v013, "labels")
anc_final$v013 <- factor(anc_final$v013, levels = c(1:7), labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))
anc_final$v013 <- case_when(anc_final$v013 %in% c("15-19") ~ "<20",
  anc_final$v013 %in% c("20-24", "25-29", "30-34") ~ "20-34",
  anc_final$v013 %in% c("35-39", "40-44", "45-49") ~ "35-49")

attr(anc_final$v102, "labels")
anc_final$v102 <- factor(anc_final$v102, levels = c(1, 2), labels = c("urban", "rural"))

attr(anc_final$v169a, "labels")
anc_final$v169a <- factor(anc_final$v169a, levels = c(0, 1), labels = c("phone_n", "phone_y"))

attr(anc_final$v190, "labels")
anc_final$v190 <- factor(anc_final$v190, levels = c(1:5), labels = c("poorest", "poorer", "middle", "richer", "richest"))

attr(anc_final$v024, "labels")
anc_final$v024 <- factor(anc_final$v024, levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("barishal", "chattogram", "dhaka", "khulna", "mymensingh", "rajshahi", "rangpur", "sylhet"))

# creating survey weights
anc_final <- anc_final %>% mutate(wt = v005 / 1000000)
library(survey)
# creating survey design object with weights
survey_design <- svydesign(id = ~v001, strata = ~v023, weights = anc_final$wt, data = anc_final)

#descriptive analysis
edu_chr <- svytable(~s115_1, design = survey_design)
edu_chr
age_chr <- svytable(~v013, design = survey_design)
age_chr
res_chr <- svytable(~v102, design = survey_design)
res_chr
phone_chr <- svytable(~v169a, design = survey_design)
phone_chr
wealth_chr <- svytable(~v190, design = survey_design)
wealth_chr
div_chr <- svytable(~v024, design = survey_design)
div_chr

#Cross tabulation for proportion of quality ANC per category
#s115_1 here, analysis for education
edu_table <- svytable(~ s115_1 + anc_final$quality_anc, design = survey_design)
percent_edu <- prop.table(edu_table, margin = 1) * 100
round_edu <- round(percent_edu, 2)

#v013 here, analysis for age
age_table <- svytable(~ v013 + anc_final$quality_anc, design = survey_design)
percent_mage <- prop.table(age_table, margin = 1) * 100
round_mage <- round(percent_mage, 2)

#v102 here, analysis for residence
res_table <- svytable(~ v102 + anc_final$quality_anc, design = survey_design)
percent_res <- prop.table(res_table, margin = 1) * 100
round_res <- round(percent_res, 2)
round_res

#v169a here, analysis for phone
phn_table <- svytable(~ v169a + anc_final$quality_anc, design = survey_design)
percent_phn <- prop.table(phn_table, margin = 1) * 100
round_phn <- round(percent_phn, 2)

#v190 here, analysis for wealth
wealth_table <- svytable(~ v190 + anc_final$quality_anc, design = survey_design)
percent_wealth <- prop.table(wealth_table, margin = 1) * 100
round_wealth <- round(percent_wealth, 2)
round_wealth

#v024 here, analysis for division
div_table <- svytable(~ v024 + anc_final$quality_anc, design = survey_design)
percent_div <- prop.table(div_table, margin = 1) * 100
round_div <- round(percent_div, 2)
round_div

#results of distribution
edu_table
round_edu
age_table
round_mage
res_table
round_res
phn_table
round_phn
wealth_table
round_wealth
div_table
round_div

#Confidence interval per category
edu_ci <- svyby(~quality_anc, ~s115_1, survey_design, svymean, vartype = c("se", "ci"))
print(edu_ci)
age_ci <- svyby(~quality_anc, ~v013, survey_design, svymean, vartype = c("se", "ci"))
print(age_ci)
res_ci <- svyby(~quality_anc, ~v102, survey_design, svymean, vartype = c("se", "ci"))
print(res_ci)
wealtin_ci <- svyby(~quality_anc, ~v190, survey_design, svymean, vartype = c("se", "ci"))
print(wealth_ci)
phn_ci <- svyby(~quality_anc, ~v169a, survey_design, svymean, vartype = c("se", "ci"))
print(phn_ci)
div_ci <- svyby(~quality_anc, ~v169a, survey_design, svymean, vartype = c("se", "ci"))
print(div_ci)

#bivariaate_analysis, chi test
svychisq(~ s115_1 + quality_anc, design = survey_design)
svychisq(~ v013 + quality_anc, design = survey_design)
svychisq(~ v102 + quality_anc, design = survey_design)
svychisq(~ v169a + quality_anc, design = survey_design)
svychisq(~ v190 + quality_anc, design = survey_design)
svychisq(~ v024 + quality_anc, design = survey_design)

--------------------------------------------------------------------------------
##Additional code
# Variables to loop over for % by provider categories
prov_vars <- paste0("m2", letters[1:14])  # Generates "m2a", "m2b", ..., "m2g"
# Create an empty list to store results
results_list <- list()
# Loop over each provider variable
for (var in prov_vars) {
  prov_table <- svytable(as.formula(paste("~ v013 +", var)), design = survey_design)
  percent_prov <- prop.table(prov_table, margin = 1) * 100
  round_prov <- round(percent_prov, 2)
  results_list[[var]] <- round_prov}

print(results_list)

#for renaming of the variable
names(anc_final)[names(anc_final) == "v013"] <- "Mother's age group"
names(anc_final)[names(anc_final) == "s115_1"] <- "Education"
names(anc_final)[names(anc_final) == "v102"] <- "Residence"
names(anc_final)[names(anc_final) == "v169a"] <- "Mobile phone ownership"
names(anc_final)[names(anc_final) == "v190"] <- "Wealth quantile"
names(anc_final)[names(anc_final) == "v024"] <- "Division"

# Association analysis with Logistic regression
edu_model <- svyglm(quality_anc ~ s115_1, design = survey_design, family = binomial())
summary(edu_model)
age_model <- svyglm(quality_anc ~ v013, design = survey_design, family = binomial())
summary(age_model)
res_model <- svyglm(quality_anc ~ v102, design = survey_design, family = binomial())
summary(res_model)
phn_model <- svyglm(quality_anc ~ v169a, design = survey_design, family = binomial())
summary(phn_model)
wealth_model <- svyglm(quality_anc ~ v190, design = survey_design, family = binomial())
summary(wealth_model)
div_model <- svyglm(quality_anc ~ v024, design = survey_design, family = binomial())
summary(div_model)

# Multivariate logistic regression model
multivar_model <- svyglm(quality_anc ~ s115_1 + v013 + v102 + v169a + v190 + v024, 
                         design = survey_design, family = binomial())
summary(multivar_model)

# Interaction term between education and wealth
interaction_model <- svyglm(quality_anc ~ s115_1 * v190, design = survey_design, family = binomial())
summary(interaction_model)

# Loop for all interactions
variables <- c("v024", "s115_1", "v190", "v169a", "v102", "v013")
# Loop through all possible pairs and fit interaction models
for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) 
    {interaction_formula <- as.formula(
      paste("quality_anc ~", variables[i], "*", variables[j]))
    
interaction_model <- svyglm(interaction_formula, design = survey_design, family = binomial())
cat("\nInteraction between:", variables[i], "and", variables[j], "\n")
print(summary(interaction_model))}}
