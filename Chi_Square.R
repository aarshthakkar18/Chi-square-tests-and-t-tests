# Importing the library
library(rstatix)
library(readxl)
library(chisq.posthoc.test)

#Problem
#For the past five years, the distribution of Japanese car sales in Ontario has held constant:
#18% of sales have gone to Nissan, 10% to Mazda, 35% to Toyota, and 37% to Honda.
#This year, an interested researcher randomly sampled records of 1000 new car purcahses
#from the Ministry of Transportation. She observed the frequencies shown in the table
#below. Is the distribution of the records the researcher pulled the same as the distribution
#observed for the past five years?

#solution:

# Chi-square goodness of fit test
#   
# Null hypothesis: Car sales distribution is same over the period
# 
# Alternative hypothesis: There is a difference in the car sales distribution over the period.

# As we're trying to fit in the observed frequency distribution to the expected frequency distribution, we'll be using the chi-square goodness of fit test
sales_obs <- c(150, 65, 385, 400)
sales_exp <- c(0.18, 0.10, 0.35, 0.37)
chisq.test(x = sales_obs, p = sales_exp)

# Post-test Writeup
# The current study seeks to determine how closely the distribution of Japanese car sales
# in Ontario for a randomly recorded sample of 1000 new car purchases fits the car sales of the 
# previous five years. According to the sample, Nissan sold 150, Mazda sold 65, Toyota sold 385, 
# and Honda sold 400 cars. A Chi-square Goodness of Fit test revealed that the observed car sales 
# are not similar to the expected model, X2(3, N=1000) = 23.182, p < 0.05. The sample distribution 
# of the current sample of car sales is in contrast with other studies that have used the same 
# data before.



#Problem:
# In the ???Popular Kids??? dataset, 478 students in grades 4 ??? 6 were asked whether good
# grades, athletic ability, or popularity (Goals) was most important to them. Were boys
# and girls likely to have similar goals? If not, how did boys??? and girls??? goal preference
# differ?
  
# Chi-square test of independence
#   
# Null hypothesis: There is no difference in the goal preference of boys and girls
# 
# Alternative hypothesis: There is a significant difference in the goal preference of boys and girls

# we'll be using chi-square test of independence
ques_4 <- read_excel("A_2_4_data.xlsx")
ques_4_table <- table(ques_4$Gender, ques_4$Goals)

#run the chi-square test letting R simulate proportions
#since you???re looking for independence and not goodness-of-fit
chisq.test(x = ques_4_table, simulate.p.value = TRUE)

#determine your alpha level ??? in this case .05
#run the post hoc test
chisq.posthoc.test(ques_4_table)

#post-test writeup
# The present research seeks to determine whether the goals of a student depend on their gender. 
# 478 students (227 boys and 251 girls) were asked to indicate if their goal was Grades(247), 
# Popularity(141), and Sports(90). A Chi-Square test for Independence revealed that gender and 
# goals were significantly related, X2(2, N = 478) = 21.455, p = 0.0004. A post hoc test for 
# proportions revealed that boys were significantly more determined towards sports. 
# Whereas, girls had their goals for becoming popular. No proportion differences were 
# found between boys and girls for the goal Grades.