#___________________________----
# SET UP ----

# An analysis of the relationship between forewing size and mean annual June temperature in Hesperia Comma.

#__________________________----

# 📜 SOURCE PREVIOUS SCRIPT ----

source("scripts/04042023_data_import.R") # import tidied butterfly data and insights

#__________________________----

# 📦 PACKAGES ----

library(emmeans) # produce mean value estimates of categorical variables
library(performance) # produce visual checks of model assumptions

#__________________________----

# 🗿 MODEL DATA ----

butterfly_ls1 <- lm(forewing_length ~ jun_mean + 
                      year +
                      sex,
                    data = butterfly) # make a two-way anova of forewing_length as a response of year, jun_mean, sex

butterfly_ls1 %>% 
  broom::tidy() # summarise linear model

#__________________________----

# 🔍 MODEL CHECKING ----

check_model(butterfly_ls1, check = "linearity") # check linearity: reference line curved (poor fit)
check_model(butterfly_ls1, check = "homogeneity") # check homogeneity: reference line poor at fitting high end of data
check_model(butterfly_ls1, check = "outliers") # check outliers: no influential outliers
check_model(butterfly_ls1, check = "vif") # check collinearity: no collinearity
check_model(butterfly_ls1, check = "qq") # check residual normality: non-normal residuals at high end of values

MASS::boxcox(butterfly_ls1) # check if a transformation would improve model fit

# boxcox output between 0 and 0.5 recommends log transformation of the data

#__________________________----

# 🕵 MODEL REFINING ----

butterfly_ls1_log <- lm(log(forewing_length) ~ jun_mean + 
                           year +
                           sex,
                         data = butterfly) # make a new linear model with the log transformation on forewing_length applied

check_model(butterfly_ls1_log, check = "linearity") # check linearity: not improved since log transformation
check_model(butterfly_ls1_log, check = "homogeneity") # check homogeneity: not improved since log transformation
check_model(butterfly_ls1_log, check = "outliers") # check outliers: no change
check_model(butterfly_ls1_log, check = "vif") # check collinearity: no change
check_model(butterfly_ls1_log, check = "qq") # check residual normality: no change

drop1(butterfly_ls1, test = "F") # drop terms to improve model fit

# year is not explaining much variance so can be removed to simplify the model

butterfly_ls2 <- lm(forewing_length ~ jun_mean + sex,
                    data = butterfly) # create new linear model without term 'year'

butterfly_ls2 %>% 
  broom::tidy() # summarise linear model

check_model(butterfly_ls2, check = "linearity") # check linearity: not improved since removal of 'year'
check_model(butterfly_ls2, check = "homogeneity") # check homogeneity: not improved since removal of 'year'
check_model(butterfly_ls2, check = "outliers") # check outliers: no change
check_model(butterfly_ls2, check = "vif") # check collinearity: no change
check_model(butterfly_ls2, check = "qq") # check residual normality: no change

# butterfly_ls2 preferred.

broom::tidy(butterfly_ls2, conf.int=T, conf.level=0.95) # add confidence intervals to butterfly_ls2 model output

#__________________________----

# 📬 POSTHOC ANALYSES ----

emmeans::emmeans(butterfly_ls2, specs = pairwise ~ sex + jun_mean)


