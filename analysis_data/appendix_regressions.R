# ---------------------------------------------------------------------------- #
# Elections, Electoral Loss Probabilities, and Government Fiscal Policies
# Online Appendix Material
# Version 2
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(lubridate)
library(DataCombine)
library(ggplot2)

# Set working directory. Change as needed.
setwd('/git_repositories/financial_crisis_fiscal_policy/')

comb <- import('analysis_data/covariate_data/epfms_covariates.csv')

# Convert key varaiables to factors
comb$iso2c <- comb$iso2c %>% as.factor
comb$election_year <- comb$election_year %>% as.factor
comb$election_year_1 <- comb$election_year_1 %>% as.factor

##### Create residuals #####
# Create output gap change and government liabilities change variables

# Output Gap Residuals
m_r1_cent <- lm(cent_debt_gdp2005 ~ cent_debt_gdp2005_1 + output_gap + 
               iso2c, data = comb)

residuals_stress_debt <- comb %>% DropNA(c('cent_debt_gdp2005', 'cent_debt_gdp2005_1', 
                                  'output_gap'))
residuals_stress_debt$residuals_output_debt <- residuals(m_r1_cent)
residuals_stress_debt <- slide(residuals_stress_debt, Var = 'residuals_output_debt', 
                      NewVar = 'residuals_output_liab_1',
                      GroupVar = 'country', TimeVar = 'year')

# Financial Stress Residuals
m_r2_cent <- lm(residuals_output_debt ~ residuals_output_liab_1 + mean_stress + 
               iso2c, data = residuals_stress_debt)
residuals_stress_debt <- residuals_stress_debt %>% DropNA(c('residuals_output_liab_1', 
                                          'mean_stress'))
residuals_stress_debt$residuals_stress_debt <- residuals(m_r2_cent)

residuals_stress_debt <- slide(residuals_stress_debt, Var = 'residuals_stress_debt', 
                      NewVar = 'residuals_stress_debt_1',
                      GroupVar = 'country', TimeVar = 'year')

residuals_stress_debt$rs_change_debt <- residuals_stress_debt$residuals_stress_debt -
    residuals_stress_debt$residuals_stress_debt_1 

residuals_stress_debt <- slide(residuals_stress_debt, Var = 'rs_change_debt', 
                      NewVar = 'rs_change_debt_1',
                      GroupVar = 'country', TimeVar = 'year')


# ------------------------------- Regressions -------------------------------- #
# Central government debt

# Election year
m5_t0_cent_debt <- lm(rs_change_debt ~ rs_change_debt_1 + election_year*lpr_1 + 
                          execrlc + 
                          polconiii + fixed_exchange + iso2c, 
                data = residuals_stress_debt)

# Post-election year
m5_t1_cent_debt <- lm(rs_change_debt ~ rs_change_debt_1 + election_year_1*lpr + execrlc + 
                          polconiii + fixed_exchange, 
                      data = residuals_stress_debt)
