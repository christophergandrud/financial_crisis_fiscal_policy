# ---------------------------------------------------------------------------- #
# Elections, Electoral Loss Probabilities, and Government Fiscal Policies
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
m_r1 <- lm(gov_liabilities_gdp2005 ~ gov_liabilities_gdp2005_1 + output_gap + 
               iso2c, data = comb)

sub_gov_liab <- comb %>% DropNA(c('gov_liabilities_gdp2005_1', 'output_gap'))
sub_gov_liab$residuals_output_liab <- residuals(m_r1)
sub_gov_liab <- slide(sub_gov_liab, Var = 'residuals_output_liab', 
                            NewVar = 'residuals_output_liab_1',
                            GroupVar = 'country', TimeVar = 'year')

# Financial Stress Residuals
m_r2 <- lm(residuals_output_liab ~ residuals_output_liab_1 + mean_stress + 
               iso2c, data = sub_gov_liab)
sub_gov_liab <- sub_gov_liab %>% DropNA(c('mean_stress'))
sub_gov_liab$residuals_stress_liab <- residuals(m_r2)

sub_gov_liab <- slide(sub_gov_liab, Var = 'residuals_stress_liab', 
                      NewVar = 'residuals_stress_liab_1',
                      GroupVar = 'country', TimeVar = 'year')

sub_gov_liab$rs_change_liab <- sub_gov_liab$residuals_stress_liab -
                            sub_gov_liab$residuals_stress_liab_1 

# ------------------------- Econ Spending Residuals -------------- #
#### Create Total Spending Residuals ####
m_r1_econ <- lm(gov_econ_spend_gdp2005 ~ gov_econ_spend_gdp2005_1 + output_gap + iso2c,
                data = comb)

sub_gov_spend <- comb %>% DropNA(c('gov_econ_spend_gdp2005', 
                                        'gov_econ_spend_gdp2005_1',
                                        'output_gap'))
sub_gov_spend$residuals_output_spend <- residuals(m_r1_econ)
sub_gov_spend <- slide(sub_gov_spend, Var = 'residuals_output_spend', 
                      NewVar = 'residuals_output_spend_1',
                      GroupVar = 'country', TimeVar = 'year')

m_r2_econ <- lm(residuals_output_spend ~ residuals_output_spend_1 + 
                    mean_stress + iso2c, 
                data = sub_gov_spend)
sub_gov_spend <- sub_gov_spend %>% 
                        DropNA(c('residuals_output_spend_1', 'mean_stress'))
sub_gov_spend$residuals_stress_spend <- residuals(m_r2_econ)

sub_gov_spend <- slide(sub_gov_spend, Var = 'residuals_stress_spend', 
                      NewVar = 'residuals_stress_spend_1',
                      GroupVar = 'country', TimeVar = 'year')

sub_gov_spend$rs_change_spend <- sub_gov_spend$residuals_stress_spend -
                                    sub_gov_spend$residuals_stress_spend_1 

sub_gov_spend <- slide(sub_gov_spend, Var = 'rs_change_spend', 
                      NewVar = 'rs_change_spend_1',
                      GroupVar = 'country', TimeVar = 'year')

# Merge change in off-trend spending to off-trend liabilities
to_merge <- sub_gov_spend %>% select(iso2c, year, rs_change_spend, 
                                     rs_change_spend_1)

sub_gov_liab <- merge(sub_gov_liab, to_merge, by = c('iso2c', 'year'), 
                      all.x = T)

# ------------------------------- Regressions -------------------------------- #
#### Election Year #### 
# Spending
m1_t0 <- lm(rs_change_spend ~ election_year + iso2c, data = sub_gov_spend)

m2_t0 <- lm(rs_change_spend ~ election_year + lpr_1 + iso2c, data = sub_gov_spend)

m3_t0 <- lm(rs_change_spend ~ election_year*lpr_1 + iso2c, 
                  data = sub_gov_spend)

m4_t0 <- lm(rs_change_spend ~ election_year*lpr_1 + execrlc + polconiii + 
                fixed_exchange + iso2c, data = sub_gov_spend)

# Liabilities
m5_t0 <- lm(rs_change_liab ~ election_year + lpr_1 + execrlc + polconiii + 
                fixed_exchange + iso2c, data = sub_gov_liab)

#### Post-Election Year ####
# Liabilities
m1_t1 <- lm(rs_change_liab ~ election_year_1 + iso2c, data = sub_gov_liab)

m2_t1 <- lm(rs_change_liab ~ election_year_1 + lpr + iso2c, data = sub_gov_liab)

m3_t1 <- lm(rs_change_liab ~ election_year_1*lpr + iso2c, data = sub_gov_liab)

m4_t1 <- lm(rs_change_liab ~ election_year_1*lpr + execrlc + polconiii +
                fixed_exchange + iso2c, 
            data = sub_gov_liab)

m5_t1 <- lm(rs_change_liab ~ election_year_1*lpr + execrlc + polconiii +
                fixed_exchange + rs_change_spend + iso2c, 
            data = sub_gov_liab)

m6_t1 <- lm(rs_change_liab ~ election_year_1*lpr + execrlc + polconiii +
                fixed_exchange + rs_change_spend_1 + iso2c, 
            data = sub_gov_liab)

# Spending
m7_t1 <- lm(rs_change_spend ~ election_year_1 + lpr + execrlc + polconiii + 
                fixed_exchange + iso2c, data = sub_gov_spend)
