install.packages("tidyverse")
library(tidyverse)


mecha_mpg <- read.csv(file = 'C:/Users/seang/OneDrive/Documents/R_Analysis/01_Demo/MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# Linear regression on each variable
lm(mpg ~ vehicle_length, mecha_mpg) 
lm(mpg ~ vehicle_weight, mecha_mpg) 
lm(mpg ~ spoiler_angle, mecha_mpg) 
lm(mpg ~ ground_clearance, mecha_mpg) 
lm(mpg ~ AWD, mecha_mpg) 

# Summary stats for each variable
summary(lm(mpg ~ vehicle_length, mecha_mpg))
summary(lm(mpg ~ vehicle_weight, mecha_mpg)) 
summary(lm(mpg ~ spoiler_angle, mecha_mpg))
summary(lm(mpg ~ ground_clearance, mecha_mpg))
summary(lm(mpg ~ AWD, mecha_mpg))

# Perform multiple linear regression on mpg for mecha_mpg dataframe
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mecha_mpg) #create linear model


# Summary statistics
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mecha_mpg)) #summarize linear model

####################### End Linear Regression ##########################

# Read the Suspension_Coil.csv
psi_analysis <- read.csv(file = 'C:/Users/seang/OneDrive/Documents/R_Analysis/01_Demo/Suspension_Coil.csv',check.names=F,stringsAsFactors = F)


total_summary <- psi_analysis %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))  #create summary table

lot_summary <- psi_analysis %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))  #create summary table



