library(dplyr)

mpg_table <- read.csv('../MechaCar_Statistical_Analysis/MechaCar_mpg.csv',check.names = F,stringsAsFactors =  F)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = mpg_table)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = mpg_table))

suspension_table <- read.csv('../MechaCar_Statistical_Analysis/Suspension_Coil.csv',check.names = F,stringsAsFactors =  F)

total_summary <- suspension_table %>% summarize(Mean=mean(PSI) , Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI) , Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

t.test(suspension_table$PSI,mu=mean(suspension_table$PSI))

lot1 <- suspension_table %>% filter(Manufacturing_Lot=='Lot1')  
lot2 <- suspension_table %>% filter(Manufacturing_Lot=='Lot2') 
lot3 <- suspension_table %>% filter(Manufacturing_Lot=='Lot3') 

t.test(lot1$PSI,mu=mean(suspension_table$PSI))
t.test(lot2$PSI,mu=mean(suspension_table$PSI))
t.test(lot3$PSI,mu=mean(suspension_table$PSI))

