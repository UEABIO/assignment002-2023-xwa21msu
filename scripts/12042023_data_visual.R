#___________________________----
# SET UP ----

# An analysis of the relationship between forewing size and mean annual June temperature in Hesperia Comma.

#__________________________----

# 📜 SOURCE PREVIOUS SCRIPT ----

source("scripts/04042023_data_import.R") # import tidied butterfly data and insights

#__________________________----

# 📊 DATA VISUALISATION ----

butterfly %>% 
  ggplot(aes(x=jun_mean, y=forewing_length))+
  geom_point(aes(colour = sex)) +
  geom_smooth(method="lm",
              se=FALSE,
              aes(colour=sex))