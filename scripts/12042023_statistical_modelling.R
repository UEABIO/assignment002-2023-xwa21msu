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
library(broom)

#__________________________----

# 🕵 HYPOTHESES ONE ----
# increases in jun_mean will cause an increase in forewing_length exacerbated by females due to sexual dimorphism.


# 🗿 MODEL DATA ----

butterfly_ls1 <- lm(forewing_length ~ jun_mean + 
                      year +
                      sex,
                    data = butterfly) # make an ANOVA of forewing_length as a response of year, jun_mean, sex

butterfly_ls1 %>% 
  broom::tidy() # summarise linear model


# 🔍 MODEL CHECKING ----

check_model(butterfly_ls1, check = "linearity") # check linearity: reference line curved (poor fit)
check_model(butterfly_ls1, check = "homogeneity") # check homogeneity: reference line poor at fitting high end of data
check_model(butterfly_ls1, check = "outliers") # check outliers: no influential outliers
check_model(butterfly_ls1, check = "vif") # check collinearity: no collinearity
check_model(butterfly_ls1, check = "qq") # check residual normality: non-normal residuals at high end of values

MASS::boxcox(butterfly_ls1) # check if a transformation would improve model fit

# boxcox output between 0 and 0.5 recommends log transformation of the data


# 💎 MODEL REFINING ----

butterfly_ls1_log <- lm(log(forewing_length) ~ jun_mean + 
                           year +
                           sex,
                         data = butterfly) # make a new linear model with the log transformation on forewing_length applied

check_model(butterfly_ls1_log, check = "linearity") # check linearity: not improved since log transformation
check_model(butterfly_ls1_log, check = "homogeneity") # check homogeneity: not improved since log transformation
check_model(butterfly_ls1_log, check = "outliers") # check outliers: no change
check_model(butterfly_ls1_log, check = "vif") # check collinearity: no change
check_model(butterfly_ls1_log, check = "qq") # check residual normality: no change

# butterfly_ls1 preferred.

drop1(butterfly_ls1, test = "F") # drop terms to improve model fit

# year is not explaining much variance so can be removed to simplify the model

butterfly_ls2 <- lm(forewing_length ~ jun_mean + sex,
                    data = butterfly) # create new linear model without term 'year'

butterfly_ls2 %>% 
  broom::tidy() # summarise linear model

drop1(butterfly_ls2, test = "F") # check model fit again

summary(butterfly_ls2) # summarise linear model to identify residual degrees of freedom

check_model(butterfly_ls2, check = "linearity") # check linearity: not improved since removal of 'year'
check_model(butterfly_ls2, check = "homogeneity") # check homogeneity: not improved since removal of 'year'
check_model(butterfly_ls2, check = "outliers") # check outliers: no change
check_model(butterfly_ls2, check = "vif") # check collinearity: no change
check_model(butterfly_ls2, check = "qq") # check residual normality: no change

# butterfly_ls2 preferred.

broom::tidy(butterfly_ls2, conf.int=T, conf.level=0.95) # add confidence intervals to butterfly_ls2 model output

#__________________________

# calculating the change in forewing size due to June temperature for each sex individually

#__________________________

# 🗿 MODEL DATA

butterfly_male <- filter(.data = butterfly, sex == "Male") # filter butterfly dataset by males only

butterfly_ls1_male <- lm(forewing_length ~ jun_mean,
                    data = butterfly_male) # create linear model of forewing_length and jun_mean

summary(butterfly_ls1_male) # summarise linear model

# 🔍 MODEL CHECKING

check_model(butterfly_ls1_male, check = "linearity") # check linearity: very poor fitting on high values, poor fitting on low values 
check_model(butterfly_ls1_male, check = "homogeneity") # check homogeneity: curved to the right
check_model(butterfly_ls1_male, check = "outliers") # check outliers: one influential outlier (26) 
check_model(butterfly_ls1_male, check = "qq") # check residual normality: one residual that is non-normal

# 💎 MODEL REFINING

MASS::boxcox(butterfly_ls1) # no transformations suggested to improve model fit

# filter out the influential outlier to improve model fit

butterfly_male <- filter(.data = butterfly, sex == "Male", jun_mean != 16.4) # filter out row 26 causing influential outlier

butterfly_ls2_male <- lm(forewing_length ~ jun_mean,
                         data = butterfly_male) # remake model without influential outlier

check_model(butterfly_ls2_male, check = "linearity") # check linearity: improved since outlier removal
check_model(butterfly_ls2_male, check = "homogeneity") # check homogeneity: curved still
check_model(butterfly_ls2_male, check = "outliers") # check outliers: no outliers 
check_model(butterfly_ls2_male, check = "qq") # check residual normality: all normal residuals

# butterfly_ls2_male preferred.

summary(butterfly_ls2_male) # summarise model

broom::tidy(butterfly_ls2_male, conf.int=T, conf.level=0.95) # add confidence intervals

drop1(butterfly_ls2_male, test = "F") # find overall effect of jun_mean on male forewing length


# 🗿 MODEL DATA

butterfly_female <- filter(.data = butterfly, sex == "Female") # filter butterfly dataset by females only

butterfly_ls1_female <- lm(forewing_length ~ jun_mean,
                         data = butterfly_female) # model forewing_length as a response of jun_mean for female butterflies

# 🔍 MODEL CHECKING

check_model(butterfly_ls1_female, check = "linearity") # check linearity: relatively flat
check_model(butterfly_ls1_female, check = "homogeneity") # check homogeneity: curves down at high values
check_model(butterfly_ls1_female, check = "outliers") # check outliers: no outliers
check_model(butterfly_ls1_female, check = "qq") # check residual normality: slightly poor fitting at low values

# butterfly_ls1_female preferred

summary(butterfly_ls1_female) # summarise linear model

broom::tidy(butterfly_ls1_female, conf.int=T, conf.level=0.95) # add confidence intervals

drop1(butterfly_ls1_female, test = "F") # find overall effect of jun_mean on female forewing length


# 📬 POSTHOC ANALYSES ----

emmeans::emmeans(butterfly_ls2, specs = pairwise ~ sex + jun_mean) # obtain mean values of male and female butterfleis when jun_mean is fixed.


#__________________________----

# 🕵 HYPOTHESIS TWO ----
# jun_mean temperatures will increase between 1880 and 1973.

# 🗿 MODEL DATA ----

cor.test(butterfly$year, butterfly$jun_mean, method = "pearson")

# year and jun_mean both normally distributed (see script: 040423_data_import)
# year and jun_mean both interval variables
# therefore assumptions are met for Pearsons R

