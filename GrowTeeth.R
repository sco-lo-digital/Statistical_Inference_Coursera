#Scott Jacobs
#Statistical Inference
#Course Project
#Part 2

#Question 1. Response follows;

#Load the data ToothGrowth and provide a basic summary
data(ToothGrowth)

#Question 2. Response follows;

#Print out a summary of the information
summary(ToothGrowth)
#Give a sense for what the data set looks like
head(ToothGrowth)

#The Effect of Vitamin C on Tooth Growth in Guinea Pigs
#Description
#The response is the length of odontoblasts (teeth) in 
#each of 10 guinea pigs at each of three dose levels of 
#Vitamin C (0.5, 1, and 2 mg) with each of two delivery 
#methods (orange juice or ascorbic acid).

#Let's get an understanding of how the two supplements compared
library(ggplot2)
qplot(supp, len, data=ToothGrowth, geom ="boxplot", color = supp) + scale_fill_brewer(name="supp", palette = "Set3")

ggplot(ToothGrowth, aes(x=factor(supp),y=len,fill=factor(supp))) +
  geom_boxplot(notch=F) + 
  scale_x_discrete("Supplement") + 
  scale_y_continuous("Length of Growth") + 
  scale_fill_brewer(name="Supplement", palette = "Set3") +
  ggtitle("Analysis of Tooth Growth")

#Based on the plot, we see that the ascorbic acid produced a wider range of lengths,
#but the median length was higher for those that received OJ as the supplement.

#To take it a step further, let's investigate each Supplement by does and 
#observe the range and median of those
ggplot(ToothGrowth, aes(x=factor(dose),y=len,fill=factor(dose))) +
  geom_boxplot(notch=F) + facet_grid(.~supp) + 
    scale_x_discrete("Dose") + 
      scale_y_continuous("Length of Growth") + 
        scale_fill_brewer(name="Dose", palette = "Set3") +
          ggtitle("Analysis of Tooth Growth - broken out by Dose")

#Question 3. Response as follows;

#In order to better understand which one was truly more effective, let's use a t test.
#Using a 95% confidence interval let's compare the supplements

#Start by subsetting the data so we can gather intervals 
dose2<-subset(ToothGrowth, dose==2)
dose1<-subset(ToothGrowth, dose==1)
dose.5<-subset(ToothGrowth, dose==.5)


#Let's exampine the entire data set
t.test(len ~supp, paired=F, var.equal=F, data=ToothGrowth)
#The results her inform us that 

#Let's test the half dose first
t.test(len ~supp, paired=F, var.equal=F, data=dose.5)
#The results here inform us that the the interval is between 1.719057 and 8.780943
#Since 0 is not included in the interval, we conclude that the greater mean length
#of teeth with supplement OJ is statistically significant

#Now let's test the 1 dose
t.test(len ~supp, paired=F, var.equal=F, data=dose1)
#The results in this case inform us that the interval is between 2.802148 and 9.057852
#Since 0 is not included in this interval, we conclude tha the greater mean length
#of teeth with supplement OJ is statistically significant

#Now let's test the 2 dose
t.test(len ~supp, paired=F, var.equal=F, data=dose2)
#The results in this case inform us that the interval is between -3.79807 and 3.63807
#Since 0 is included in this interval, we can not reject the null hypothesis
#Therefore, we remain uncertain as to whether OJ or VC contributes to greater tooth length

#Since we just explored the difference between the two supplements,
#let's examine the contibution of the variable 'dose' to the length of teeth.

#Subset the OJ supplements by dose
oj2<-subset(ToothGrowth, supp=="OJ" & dose==2)
oj1<-subset(ToothGrowth, supp=="OJ" & dose==1)
oj.5<-subset(ToothGrowth, supp=="OJ" & dose==.5)

#Compare the oj1 to the oj.5 to see if the change in dose is attributable to greater length
t.test(oj.5$len, oj1$len, paired=F, var.equal=F)
#We observe an interval of -13.42 and -5.52, which does not include 0
#We can then conclude that the greater mean length associate with
#the higher dose is statistically significant.

#Compare the oj1 to the oj2 to see if the change in dose is attributable to greater length
t.test(oj1$len, oj2$len, paired=F, var.equal=F)
#We observe an interval of -6.53 and -.19, which does not include 0
#We can then conclude that the greater mean length associate with
#the higher dose is statistically significant.

#Subset the VC supplements by dose
vc2<-subset(ToothGrowth, supp=="VC" & dose==2)
vc1<-subset(ToothGrowth, supp=="VC" & dose==1)
vc.5<-subset(ToothGrowth, supp=="VC" & dose==.5)

#Compare the vc1 to the vc.5 to see if the change in dose is attributable to greater length
t.test(vc.5$len, vc1$len, paired=F, var.equal=F)
#We observe an interval of -11.27 and -6.31, which does not include 0
#We can then conclude that the greater mean length associate with
#the higher dose is statistically significant.

#Compare the vc1 to the vc.5 to see if the change in dose is attributable to greater length
t.test(vc1$len, vc2$len, paired=F, var.equal=F)
#We observe an interval of -13.05 and -5.69, which does not include 0
#We can then conclude that the greater mean length associate with
#the higher dose is statistically significant. 

#Conclusions and Assumptions

#Conclusions
#1. The pigs given doses of .5 or 1 experience greater growth with supplement "OJ"
#2. Increseases in dossages lead to greater tooth length.
#3. All pigs that received a dose of 2, regardless of supplement, registered simlar relative growth

#Assumptions
#1. Variances are not equal, hence var.equal=F
#2. The data is not paired
