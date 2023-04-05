#___________________________----
# SET UP ----

# An analysis of the relationship between forewing size and mean annual June temperature in Hesperia Comma.

#__________________________----

# 🌱 CLEAR ENVIRONMENT ----

rm(list = ls())

#__________________________----

# 📦 PACKAGES ----
library(tidyverse) # tidy data structures
library(janitor) # clean column and variable names
library(here) # to make filepaths

#__________________________----

# 📂 IMPORT DATA ----

butterfly <- read_csv(here("data", "univoltine_butterfly.csv"))

#__________________________----

# 🔍 CHECK DATA----

head(butterfly) # check data loaded correctly

colnames(butterfly) # see column names: not in snake case

butterfly <- janitor::clean_names(butterfly) # change column names to lower snake case

colnames(butterfly) # check column names: all in snake case

str(butterfly) # check structure of data
# year, forewing_length, jun_mean, rain_jun all numerical variables
# sex is a character variable

#__________________________----

# 🧹 TIDY ----

butterfly %>% 
  duplicated() %>%
  sum() # check for duplicated rows of data: output = 0

butterfly %>% 
  is.na() %>% 
  sum() # check for missing data: output = 0

butterfly %>% 
  distinct(sex) # check for typos

# some values coded as 'Maes', 'females', 'males'
# mutate value names to 'Male' and 'Female' only

butterfly <- butterfly %>%
  mutate(sex = recode(sex, Maes = 'Male', Males = 'Male', Females = 'Female')) # change value names to correct typos

butterfly %>% 
  distinct(sex) # check the typos have been corrected

butterfly %>% 
  distinct(year) # check for typos: no typos

butterfly %>% 
  summarise(min=min(forewing_length, na.rm=TRUE), 
            max=max(forewing_length, na.rm=TRUE)) # check for typos by identifying impossible values

# no impossible values found.

butterfly %>% 
  summarise(min=min(jun_mean, na.rm=TRUE), 
            max=max(jun_mean, na.rm=TRUE)) # check for typos by identifying impossible values

# no impossible values found.

butterfly %>% 
  summarise(min=min(rain_jun, na.rm=TRUE), 
            max=max(rain_jun, na.rm=TRUE)) # check for typos by identifying impossible values

# max value is 577mm of rain and seems impossible
# filter to exclude this value

butterfly <- filter(.data = butterfly, rain_jun != 577.0) # remove row including impossible value

butterfly %>% 
  is.na() %>% 
  sum() # check how many missing values there are: sum = 0.

summary(butterfly) # quick summary of dataset

#__________________________----

# 💡 DATA INSIGHTS ----

glimpse(butterfly) # look at the type of variables

# year = integer
# jun_mean, rain_jun and forewing_length = continuous
# sex = categorical

butterfly %>% 
  group_by(sex) %>% 
  summarise(n = n()) # check frequency of males to females

rel_freq_sex <- butterfly %>% 
  group_by(sex) %>% 
  summarise(n = n()) %>% 
  mutate(prob_obs = n/sum(n)) # calculate relative frequency of males to females

rel_freq_sex # call dataframe

# roughly 50% males to females

butterfly %>% 
  ggplot()+
  geom_histogram(aes(x=jun_mean),
                 bins=10) # check variation with frequency distribution

butterfly %>% 
  ggplot()+
  geom_histogram(aes(x=rain_jun),
                 bins=10) # check variation with frequency distribution
