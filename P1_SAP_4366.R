# Libraries 
library(readxl)
library(tidyverse)

# Data
softball = read_xlsx('/Users/djara/Desktop/Cal Poly/STAT 4366/Project 1/Softball.xlsx')

softball_new <- softball |> 
  rename(Confidence = `Scale: Confidence & Self-Perception`, 
         Perceived_Opportunity = `Scale: Perceived Opportunity & Inclusion`, 
         Motivation = `Scale: Motivation & Enjoyment/Perceived Benefit`, 
         External_Social = `Scale: External Social Environment`, 
         Gait_Speed = `Gait Speed (m/s)`, 
         Num_Sports = `Number of Sports Played`, 
         Throwing_Acc = `Throwing Accuracy Score`, 
         Double_Support = `Double Support (% gait cycle)`, 
         Baserunning = `Base Running Time (s)`, 
         Sports_Hrs = `Total Lifetime Organized Sport Hours`,
         Norm_Stride = `NORMALIZED Stride Length (m)`,
         SL_3D = `SL 3D RMS (mm/s^2)`)|> 
  filter(!is.na(Confidence)) |> 
  select(!c(`Stride Length (m)`, `Step Width (cm)`, `Cadence (steps/min)`, 
            `Step Length Symmetry (%, R/L)`, `Balance Footwear`))

# Pivoted Dataset for 
 softball_plot <- softball_new |>
  pivot_longer(cols = c(
      Confidence,
      Perceived_Opportunity,
      Motivation,
      External_Social),
    names_to = "Psychosocial",
    values_to = "Score") 
 
 # Plots
 # Baserunning - Performance 
 softball_plot |> 
  ggplot(aes(x = Score, y = Baserunning)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ Psychosocial, ncol = 2) +
  labs(
    x = "Psychosocial scale score (1-5)",
    y = "Base-running time (seconds; lower = faster)"
  ) +
  theme_bw()

# Single Leg Balance - Balance
 softball_plot |> 
   ggplot(aes(x = Score, y = SL_3D)) +
   geom_point() +
   geom_smooth(method = "lm", se = FALSE, color = "blue") +
   facet_wrap(~ Psychosocial, ncol = 2) +
   labs(
     x = "Psychosocial scale score (1-5)",
     y = "Single-Leg RMS (mm/s^2)"
   ) +
   theme_bw()
 
 # Double Support % - Gait 
 softball_plot |> 
   ggplot(aes(x = Score, y = Double_Support)) +
   geom_point() +
   geom_smooth(method = "lm", se = FALSE, color = "coral3") +
   facet_wrap(~ Psychosocial, ncol = 2) +
   labs(
     x = "Psychosocial scale score (1-5)",
     y = "Double Support %") +
   theme_bw()
 
 # Throwing Accuracy - Performance 
 softball_plot |> 
   ggplot(aes(x = Score, y = Throwing_Acc)) +
   geom_point() +
   geom_jitter()+
   geom_smooth(method = "lm", se = FALSE, color = "coral3") +
   facet_wrap(~ Psychosocial, ncol = 2) +
   labs(
     x = "Psychosocial scale score (1-5)",
     y = "Num Sports") +
   theme_bw()
