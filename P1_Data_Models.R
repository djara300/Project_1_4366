# Libraries 
library(readxl)
library(tidyverse)

# Data
softball = read_xlsx('/Users/djara/Desktop/Cal Poly/STAT 4366/Project 1/Softball.xlsx')

# Cleaning Data 
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
         SL_3D = `SL 3D RMS (mm/s^2)`) |> #Renanme multiple variables for easier reproducability
  filter(!is.na(Confidence)) #Removes participants who didn't fill out survey
  
# Baserunning Data + Model + Regressional Plot
softball_baserunning <- softball_new |> #Creates dataset where all participants who didn't do baserunning are removed
  filter(!is.na(Baserunning))

model_br = lm(Baserunning ~ External_Social, data = softball_baserunning)
summary(model_br) # Significant **

softball_baserunning |> 
  ggplot(aes(x = External_Social, y = Baserunning)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red1") +
  labs(
    title = "External Social Environment v. Baserunning Time",
    subtitle = "Every point represents one survey respondent; n = 17",
    x = "External Social factor score (1-5)",
    y = "Baserunning Time (sec)"
  ) +
  theme_bw()

# Douple Support Data + Model + Regression Plot
softball_double_sup <- softball_new |> 
  filter(!is.na(Double_Support))

model_ds = lm(Double_Support ~ Motivation, data = softball_double_sup)
summary(model_ds) # Not significant (lowest p-value model)

softball_double_sup |> 
  ggplot(aes(x = Motivation, y = Double_Support)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red1") +
  labs(
    title = "Motivation v. Double Support %",
    subtitle = "Every point represents one survey respondent; n = 17",
    x = "Motivation factor score (1-5)",
    y = "Double Support %"
  ) +
  theme_bw()

# Single Leg Balance Data + Model + Regression Plot
softball_SL3D <- softball_new |> 
  filter(!is.na(SL_3D))

model_sl3d = lm(SL_3D ~ Motivation, data = softball_SL3D)
summary(model_sl3d) # Not significant (lowest p-value model)

softball_SL3D |> 
  ggplot(aes(x = Motivation, y = SL_3D)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red1") +
  labs(
    title = "Motivation v. Single Leg Balance (3D RMS)",
    subtitle = "Every point represents one survey respondent; n = 18",
    x = "Motivation factor score (1-5)",
    y = "Single-Leg RMS (mm/s^2)"
  ) +
  theme_bw()

# Throwing Accuracy Data + Model + Regression Plot
softball_throw <- softball_new |> 
  filter(!is.na(Throwing_Acc))

model_throw = lm(Throwing_Acc ~ Confidence, data = softball_throw)
summary(model_throw) # Not significant (lowest p-value model)

softball_throw |> 
  ggplot(aes(x = Confidence, y = Throwing_Acc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red1") +
  labs(
    title = "Confidence v. Throwing Accuracy",
    subtitle = "Every point represents one survey respondent; n = 18",
    x = "Confidence factor score (1-5)",
    y = "Throwing Accuracy (# of targets hit out of 10)"
  ) +
  theme_bw()

# Sample Size Plots 
softball_plot = tibble(
  Dataset = c("Overall Softball Dataset", 
              "Psychosocial Survey Respondents", 
              "Base-running Analysis", 
              "Double Support % Analysis", 
              "Single Leg Balance Analysis", 
              "Throwing Accuracy Analysis"), 
  Sample_Size  = c(nrow(softball), 
                   nrow(softball_new), 
                   nrow(softball_baserunning), 
                   nrow(softball_double_sup), 
                   nrow(softball_SL3D), 
                   nrow(softball_throw))) |> 
  mutate(Dataset = fct_reorder(Dataset, Sample_Size))

softball_plot |>
  ggplot(aes(x = Dataset, y = Sample_Size, fill = Dataset)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = Sample_Size),
    hjust = -0.25,
    size = 5) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Overall Softball Dataset" = "#1F4E79",
    "Psychosocial Survey Respondents" = "#5B9BD5",
    "Base-running Analysis" = "#A5C4E8",
    "Double Support % Analysis" = "#A5C4E8",
    "Single Leg Balance Analysis" = "#5B9BD5",
    "Throwing Accuracy Analysis" = "#5B9BD5")) +
  labs(
    title = "Sample Sizes for Psychosocial Analyses",
    subtitle = "Only Participants who filled out the survey are including in the regression models",
    x = "Dataset or analysis sample",
    y = "Number of players") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))) +
  theme_bw()
