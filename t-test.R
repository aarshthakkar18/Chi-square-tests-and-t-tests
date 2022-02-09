library(rstatix)
library(readxl)

# Problem 
# In the “Horseshoe Crabs” dataset, a marine biologist was concerned with the populations
# of horseshoe crabs at 25 different beaches along the New England coastline. In 2011 and
# again in 2012, he and his team painstakingly counted the populations of horseshoe crabs
# at each location and recorded them in the database. According to this research, is Maine
# witnessing a decline in the population of horseshoe crabs?


#solution

# Paired Samples t-test
#   
# Null hypothesis: There is no decline in the population of horseshoe crabs
# 
# Alternative hypothesis: There is a decline in the population of horseshoe crabs

# As we're comparing means here in a before after kinda situation, we'll be using the paired samples t-test
ques_1 <- read_excel("pst.xlsx")
colnames(ques_1) <- c("Beach","pop_in_2011","pop_in_2012")

#we can’t do much with “before” - 2011 and “after” - 2012 values in separate columns
#let’s gather them into the SAME column in a new table
ques_1.long <- gather(ques_1, key = "year", value = "population", 'pop_in_2011', 'pop_in_2012')

# now let's get some summary statistics
ques_1.grouping <- group_by(ques_1.long,year)
get_summary_stats(ques_1.grouping, population, type = "mean_sd")

#it’s not the scores that matter, but the differences
#so we need a new column that shows the differences
#the mutate function lets you transform data and add columns
ques_1 <- mutate(ques_1, differences = pop_in_2011 - pop_in_2012)
head(ques_1)

#we also need to identify any outliers in the new differences column
identify_outliers(ques_1, differences)
# there are a few outliers but not extreme so we'll ignore

# Shapiro-wilk test to check the normality of our data
# we test for normality within the differences distribution
#if p is > .05, the distribution is normal
shapiro_test(ques_1, differences)

#yay! Differences are normally distributed
#we DON’T need to test homogeneity of variances, since we’re
#only evaluating ONE field – the differences column!

#yay! now we can run our t-test!
#we will set paired = TRUE
#note that we’re running the test on ques_1.long – the table
#we reorganized at the start of the analysis
t_test(ques_1.long, population ~ year, paired = TRUE)

#that p value is small – the test shows a significant difference```

# you can use the add_significance function to be lazy and know the significance of the t-test
t.result <- t_test(ques_1.long, population ~ year, paired = TRUE)
add_significance(t.result)

#we need to perform Cohen’s d to measure effect size
#we will set paired = TRUE
#don’t forget we’re using the reorganized table
cohens_d(ques_1.long, population ~ year, paired = TRUE)
#that’s a small effect size!


#post-test writeup
# The current research sought to determine if there is a decline in the population of horseshoe 
# crabs at 25 different beaches around the New England coastline. The population was painstakingly
# counted by the marine biologist and his team for before(2011) and after(2012). The differences 
# in the population between 2011 and 2012 showed no extreme outliers. A Shapiro-Wilk test on the 
# distribution of population differences demonstrated normality. The mean population of the 
# horseshoe crabs in 2011 was 71888.12(SD = 54814.77) and, the mean population of the horseshoe 
# crabs in 2012 was 56862.72(SD = 46370.05). As the p-value is less than 0.05 we can conclude 
# that there was a significant small decrease in the population of horseshoe crabs from 2011 to 
# 2012, t(24) = 2.15, p = 0.04, d = 0.43.



#Problem
# Investigating environmental causes of disease, a scientist collected data on the annual
# mortality rate (deaths per 100,00) of males in 61 large towns in the North and South
# of the United Kingdom. In addition, the researcher recorded the water hardness as the
# calcium concentration (parts per million) in the drinking water. Is there a difference in
# male mortality between the North and South?

#solution

# Independent sample t-test
#   
# Null hypothesis: There is no difference in the male mortality rate between the North and South region
# 
# Alternative hypothesis: There is a difference in the male mortality rate between the North and South region

# As we're comparing means for different sample groups we'll be using Independent sample t-test
# Some summary statistics of our data
ques_3 <- read_excel("ist.xlsx")
ques_3.grouping <- group_by(ques_3, Region)
get_summary_stats(ques_3.grouping, Mortality, type = "mean_sd")

#we need to test some assumptions about our data first!
#first, does the sample contain any extreme outliers?
identify_outliers(ques_3.grouping, Mortality)
#yay! there are no extreme outliers in the sample

# Shapiro-wilk test to check the normality of our data
#if p is > .05, the answer is yes
shapiro_test(ques_3.grouping, Mortality)
#yay! Shapiro says our sample is normally distributed!

#we’re not done testing yet!
#finally, we will be performing levene's test to test for homogeneity of variance
#the ~ operator is a model separator:
#what’s on the right of the ~ influences what’s on the left
#if p is > .05, variances are homogenous
levene_test(ques_3, Mortality ~ Region)
#yayyyyy the variance between groups isn't different!

#yay! now we can run our t-test!
#we will set var.equal to TRUE because of the Levene results
#this is called a “Student’s” t-test
t_test(ques_3, Mortality ~ Region, var.equal=TRUE)
#p-value is very small which suggests there is a significant difference

#if you want to be lazy you can run this function for Knowing the significance of our t-test
t.result <- t_test(ques_3, Mortality ~ Region, var.equal = TRUE)
add_significance(t.result)

#you can’t use p values to describe effect size
#instead, you would use Cohen’s d
#once again, we set var.equal to FALSE because of the Levene result
cohens_d(ques_3, Mortality ~ Region, var.equal = TRUE)
#that’s a nice big effect size!


# The current study sought to determine whether or not region affected mortality rate due to 
# environmental problems. Data of annual mortality rate for males was collected from 61  large 
# towns for the sample (34 North, 27 South). The sample contained no extreme outliers. 
# A Shapiro-Wilk test demonstrated normality by region. Levene's test result demonstrated 
# homogeneity of variance. The mean mortality rate for men in the North region was 1631.58 
# (SD = 138.47) whereas the mean mortality rate for men in the South region was 1388.85 
# (SD = 151.11). A Student's independent t-test showed that the mean difference of mortality 
# rate between the north and south regions in the sample was statistically significant, 
# t(59) = 6.53, p<0.0001, d = 1.68, with the north region having more mortality than the south.