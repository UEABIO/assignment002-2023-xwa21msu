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
library(patchwork) # to combine plots
library(ggridges) # add extra geoms to ggplot

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
# create new dataset that filters to exclude this value

butterfly_filtered <- filter(.data = butterfly, rain_jun != 577.0) # remove row including impossible value

butterfly_filtered %>% 
  summarise(min=min(rain_jun, na.rm=TRUE), 
            max=max(rain_jun, na.rm=TRUE)) # check again for typos by identifying impossible values

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
  geom_histogram(aes(x=year),
                 bins=10) # check variation with frequency distribution

butterfly_filtered %>% # using filtered dataset to exclude impossible values
  ggplot()+
  geom_histogram(aes(x=rain_jun),
                 bins=10) # check variation with frequency distribution

butterfly %>% 
  ggplot()+
  geom_histogram(aes(x=forewing_length),
                 bins=10) # check variation with frequency distribution

colour_fill <- "darkorange" # code fill colours
colour_line <- "steelblue" # code line colours
ggplot(butterfly, aes(x = forewing_length, y = sex)) + 
  ggridges::geom_density_ridges(fill = colour_fill,
                                colour = colour_line,
                                alpha = 0.8) # check variation with sex as a covariate

# male and female butterflies normally distributed

# rain_jun, mean_jun and forewing_length symmetrically distributed
# check for normal distribution in jun_mean, rain_jun and forewing_length with QQ plot

butterfly %>% 
  pull(jun_mean) %>% 
  car::qqPlot() # create QQ plot for jun_mean

butterfly_filtered %>%
  pull(rain_jun) %>%
  car::qqPlot() # create QQ plot for rain_jun

butterfly_filtered %>%
  pull(forewing_length) %>%
  car::qqPlot() # create QQ plot for forewing_length

# jun_mean, rain_jun and forewing_length are normally distributed
# use mean and medians to measure central tendency

jun_mean_summary <- butterfly %>% 
  summarise(mean_jun_temp=mean(jun_mean, na.rm=T), 
            sd = sd(jun_mean, na.rm = T),
            median_jun_temp=median(jun_mean, na.rm=T)) # calculate mean and median temperature for june 

jun_mean_summary # call dataframe

rain_mean_summary <- butterfly_filtered %>%
  summarise(mean_rain_jun=mean(rain_jun, na.rm=T),
            sd = sd(rain_jun, na.rm = T),
            median_rain_jun=median(rain_jun, na.rm=T)) # calculate mean and median rainfall for june between 1880 and 1973

rain_mean_summary # call dataframe

forewing_length_mean_summary <- butterfly %>%
  summarise(mean_forewing_length=mean(forewing_length, na.rm=T),
            sd = sd(rain_jun, na.rm = T),
            median_forewing_length=median(forewing_length, na.rm=T)) # calculate mean and median forewing length

forewing_length_mean_summary # call dataframe

# produce plots to visualise dispersion of jun_mean

lims1 <- c(10, 17)
jun_mean_plot <- function(){
  butterfly %>% 
    ggplot(aes(x="",
               y= jun_mean))+
    labs(x= " ",
         y = "Mean June Temperatures (degree celcius)")+
    scale_y_continuous(limits = lims1)+
    theme_minimal()
}

plot_1 <- jun_mean_plot()+
  geom_jitter(fill = colour_fill,
              colour = colour_line,
              width = 0.2,
              shape = 21)

plot_2 <- jun_mean_plot()+
  geom_boxplot(fill = colour_fill,
               colour = colour_line,
               width = 0.4)

plot_1 + plot_2

# evenly dispersed datapoints, no extreme outliers

# produce plots to visualise dispersion of rain_jun

lims2 <- c(0, 100)

rain_mean_plot <- function(){
  butterfly %>% 
    ggplot(aes(x="",
               y= rain_jun))+
    labs(x= " ",
         y = "Mean June Rainfall (mm)")+
    scale_y_continuous(limits = lims2)+
    theme_minimal()
}

plot_3 <- rain_mean_plot()+
  geom_jitter(fill = colour_fill,
              colour = colour_line,
              width = 0.2,
              shape = 21)

plot_4 <- rain_mean_plot()+
  geom_boxplot(fill = colour_fill,
               colour = colour_line,
               width = 0.4)

plot_3 + plot_4

# evenly dispersed datapoints, no extreme outliers

# produce plots to visualise dispersion of forewing_length

lims3 <- c(10, 16)

forewing_mean_plot <- function(){
  butterfly %>% 
    ggplot(aes(x="",
               y= forewing_length))+
    labs(x= " ",
         y = "Mean Forewing Length (cm)")+
    scale_y_continuous(limits = lims3)+
    theme_minimal()
}

plot_5 <- forewing_mean_plot()+
  geom_jitter(fill = colour_fill,
              colour = colour_line,
              width = 0.2,
              shape = 21)

plot_6 <- forewing_mean_plot()+
  geom_boxplot(fill = colour_fill,
               colour = colour_line,
               width = 0.4)

plot_5 + plot_6

# evenly dispersed datapoints, no extreme outliers

