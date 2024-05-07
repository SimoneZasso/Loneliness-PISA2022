# Store only countries with RMSEA_D < 0.08 --------------------------------

# This object refers to countires supporting full mediation by deleting the follwoing:
# c(807, 76, 170, 208, 32, 504, 860, 222, 608, 214, 591) # These are indeed the countreis for partial mediation

sem_analysis_country_fullMed <- change_name_country(sem_analysis_country) %>% 
  filter(Country != "North Macedonia",
         Country != "Brazil",
         Country != "Colombia",
         Country != "Denmark",
         Country != "Argentina",
         Country != "Morocco",
         Country != "Uzbekistan",
         Country != "El Salvador",
         Country != "Philippines",
         Country != "Dominican Republic",
         Country != "Panama"
  )


# Plot for Full Mediation models ------------------------------------------

data_plot            <- sem_analysis_country_fullMed %>% filter(., Model == 2)
data_plot$ind_effect <- as.numeric(data_plot$ind_effect)
data_plot$LL4        <- as.numeric(data_plot$LL4)
data_plot$UL4        <- as.numeric(data_plot$UL4)

forest_plot_FullMed <- ggplot(data_plot, aes(x = ind_effect, y = reorder(Country, ind_effect))) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = LL4, xmax = UL4), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Mediation Effect",
       y = "Country",
       title = "Forest Plot",
       subtitle = "Mediation estimates with 95% confidence intervals (Full Mediation models)") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray", size = 0.5))
ggsave("./figs/forest_plot_FullMed.png", bg = "white")



# Plot for Partial Mediation models ------------------------------------------

data_plot2            <- sem_analysis_country_PartMed2 %>% filter(., Model == 1)
data_plot2$ind_effect <- as.numeric(data_plot2$ind_effect)
data_plot2$LL4        <- as.numeric(data_plot2$LL4)
data_plot2$UL4        <- as.numeric(data_plot2$UL4)

forest_plot_FullMed <- ggplot(data_plot2, aes(x = ind_effect, y = reorder(Country, ind_effect))) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = LL4, xmax = UL4), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Mediation Effect",
       y = "Country",
       title = "Forest Plot",
       subtitle = "Mediation estimates with 95% confidence intervals (Partial Mediation models)") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray", size = 0.5))
  ggsave("./figs/forest_plot_PartMed.png", bg = "white")
