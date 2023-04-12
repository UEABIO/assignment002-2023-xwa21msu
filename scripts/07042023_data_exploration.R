#___________________________----
# SET UP ----

# An analysis of the relationship between forewing size and mean annual June temperature in Hesperia Comma.

#__________________________----

# 📜 SOURCE PREVIOUS SCRIPT ----

source("scripts/04042023_data_import.R") # import tidied butterfly data and insights

#__________________________----

# 📦 PACKAGES ----

library(rstatix)

#__________________________----

# 🎒 EXPLORATORY ANALYSIS ----

# is there an association between forewing size and temperature?

butterfly %>% 
  ggplot(aes(x=jun_mean, y=forewing_length))+
  geom_point() # create a scatter plot of forewing_length and jun_mean

# could be a positive relationship
# test with Pearson's R

butterfly %>% 
  cor_test(forewing_length, jun_mean)

# positive but weak relationship: cor = 0.29
# add sex as a covariate

butterfly %>% 
  ggplot(aes(x=jun_mean, y=forewing_length))+
  geom_point(aes(colour = sex)) + # scatter plot of forewing_length and jun_mean with points coloured by sex
  geom_smooth(method="lm",
              se=FALSE,
              aes(colour=sex)) # add a linear model line per sex

# evident that female butterflies have bigger forewing length than males
# evidence for interaction of sex on forewing_length
# may be a positive relationship between forewing_length and jun_mean for males and females separately
# check this with Pearson's R

butterfly_males <- filter(.data = butterfly, sex == "Male") # create new dataset containing only male butterflies

butterfly_males %>% 
  cor_test(forewing_length, jun_mean) # generate Pearson's R test for male butterflies

# relationship is quite strongly positive: cor = 0.62

butterfly_females <- filter(.data = butterfly, sex == "Female") # create new dataset containing only female butterflies

butterfly_females %>% 
  cor_test(forewing_length, jun_mean) # generate Pearson's R test for female butterflies

# relationship is positive but weak: cor = 0.27

# does temperature increase with years passed?

butterfly %>% 
  ggplot(aes(x=year, y=jun_mean))+
  geom_point()

# doesn't appear that there is a relationship
# test this with Pearson's R

butterfly %>% 
  cor_test(year, jun_mean) 

# positive but very weak relationship: cor = 0.19

# does rainfall decrease average june temperatures?

butterfly_filtered %>% 
  ggplot(aes(x=rain_jun, y=jun_mean))+
  geom_point()

# no obvious relationship
# test this with Pearson's R

butterfly %>% 
  cor_test(rain_jun, jun_mean) 

# very weak negative relationship between rain_jun and jun_mean: cor = 0.014

# does rainfall increase with years passed?

butterfly_filtered %>% 
  ggplot(aes(x=year, y=rain_jun))+
  geom_point()

# no obvious relationship, could be negative
# test this with Pearson's R

butterfly %>% 
  cor_test(rain_jun, year) 

# very weak negative relationship between rain_jun and year.

#__________________________----

# 📝 SUMMARY ----

# sex interacts with forewing_length and jun_mean
# males appear to have a strong correlation between forewing_length and jun_mean
# females appear to have a weaker correlation between forewing_length and jun_mean
# females forewing_length is larger than males
# a model must be created to test this observation.

# temperature and year has a weak positive correlation
# rainfall and year has a weak negative correlation
# rainfall doesn't have a relationship with temperature


