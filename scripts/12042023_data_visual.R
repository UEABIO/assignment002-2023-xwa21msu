#___________________________----
# SET UP ----

# An analysis of the relationship between forewing size and mean annual June temperature in Hesperia Comma.

#__________________________----

# 📜 SOURCE PREVIOUS SCRIPT ----

source("scripts/04042023_data_import.R") # import tidied butterfly data and insights

#__________________________----

# 📊 DATA VISUALISATION ----

plot_7 <- butterfly %>% 
  ggplot(aes(x=jun_mean, y=forewing_length))+
  geom_point(aes(colour = sex)) +
  geom_smooth(method="lm",
              se=TRUE,
              alpha = 0.15,
              aes(colour=sex, fill = sex)) +
  labs(x = "Mean June Temperature (°C)",
       y = "Forewing Length (mm)") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "grey")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) # produce regression plot of forewing length and jun_mean for each sex

plot_7

colorBlindness::cvdPlot()

plot_8 <- butterfly %>%
  ggplot(aes(x = sex, y = forewing_length)) +
  geom_boxplot(aes(fill = sex), alpha = 0.1, width = 0.5) +
  geom_point(aes(colour = sex, fill = sex), alpha = 0.5, position = position_jitter(width = 0.2)) +
  labs(x = "",
       y = "Forewing Length (mm)") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "grey"), legend.position = "none") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) # create boxplot of male and female size difference

plot_8

colorBlindness::cvdPlot()
