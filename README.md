# MechaCar Statistical Analysis

## Overview/Purpose
Analyzing a "MechaCar" prototype dataset by:
* Using linear regression
* Getting summary statistics of Suspension Coils
* Performing t-tests on the suspension coil data to check if any of the 3 manufacturing lots have a statistically significant difference from the mean population
* Designing a study to compare "MechaCar" vehicles and vehicles made by other manufacturers 

## Results

### Linear Regression to Predict MPG

![](resources/q1.png)

Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?
* The variables that had the most impact would be ground clearance and vehicle length.

Is the slope of the linear model considered to be zero? Why or why not?
* No, the slope of the linear model is not zero. The reason this distinction can be made is because of the extremely small p-value(5.35e-ll.)

Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?
* Yes, the model is able to effectively predict the mpg of MechaCar prototypes. 

### Summary Statistics on Suspension Coils

Total Summary

![](resources/totalsummary.png)

Lot Summary

![](resources/lotsummary.png)

The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Why or why not?
* If the aggregate variation value was used, the variation would be within the limits of this metric. When looking at the variation by lots, lot 1 and 2 fit the metric, but lot 3 fails to meet this metric since it has too high of a variance(170.2861224.)

### T-Tests on Suspension Coils

All lots t-test

![](resources/suspensionttest.png)

lot 1 t-test

![](resources/lot1ttest.png)

lot 2 t-test

![](resources/lot2ttest.png)

lot 3 t-test

![](resources/lot3ttest.png)

Similarly to the summary statistics shown above, there is something happening in the third lot. The first and second t-test show that they do not have a statistically significant p-value, while lot 3 seems to have a statistically significant p-value of 0.1589. 

### Study Design: MechaCar vs Competition

Write a short description of a statistical study that can quantify how the MechaCar performs against the competition. In your study design, think critically about what metrics would be of interest to a consumer: for a few examples, cost, city or highway fuel efficiency, horse power, maintenance cost, or safety rating.


What metric or metrics are you going to test?
* The metrics I would use would be horse power, feul efficiency, and cost. 

What is the null hypothesis or alternative hypothesis?
* The null hypothesis would be that there is not a statistically significant difference in the mean horse power and feul efficiency of MechaCar and their competitor.
* The alternative hypothesis would be that there is a statistically significant difference in the mean horse power and feul efficiency of MechaCar and their competitor.

What statistical test would you use to test the hypothesis? And why?
* To test the hypothesis, the best choice would be to use a two sample t-test as this would allow for the comparison of the feul efficiency and horse power means between MechaCar and their competitor.

What data is needed to run the statistical test?
* The data would have to be very similar in terms of scale and units used. To form a strong claim we would need at least 100 samples. If more samples are provided, the claim can be made stronger.
