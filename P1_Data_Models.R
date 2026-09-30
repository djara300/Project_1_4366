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
         SL_3D = `SL 3D RMS (mm/s^2)`)
  
# Baserunning Data + Model 
softball_baserunning <- softball_new |> 
  filter(!is.na(Confidence), 
         !is.na(Baserunning))

model_br = lm(Baserunning ~ External_Social, data = softball_baserunning)
summary(model_br) # Significant **

# Douple Support Data + Model 
softball_double_sup <- softball_new |> 
  filter(!is.na(Confidence), 
         !is.na(Double_Support))

model_ds = lm(Double_Support ~ Motivation, data = softball_double_sup)
summary(model_ds) # Not significant (lowest p-value model)

# Single Leg Balance Data + Model 
softball_SL3D <- softball_new |> 
  filter(!is.na(Confidence), 
         !is.na(SL_3D))

model_sl3d = lm(SL_3D ~ Motivation, data = softball_SL3D)
summary(model_sl3d) # Not significant (lowest p-value model)

# Throwing Accuracy Data + Model 
softball_throw <- softball_new |> 
  filter(!is.na(Confidence), 
         !is.na(Throwing_Acc))

model_throw = lm(Throwing_Acc ~ Confidence, data = softball_throw)
summary(model_throw) # Not significant (lowest p-value model)
