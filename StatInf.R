#Scott Jacobs
#Statistical Inference 
#statinference-014
#Course Project

#Question 1-Show the sample mean and compare it to the theoretical mean 
#of the distribution.

#Create a distribution of 1000 exponentials with lambda of .2
expdist<-rexp(1000, 0.2)
#Display results in a histogram, Fig. 1
hist(expdist, main="Fig. 1 - 1000 exponentials with lambda of .2")

#Generate distribution of 1000 averages of 40 exponentials
mns=NULL
for (i in 1:1000) mns= c(mns, mean(rexp(40, 0.2)))
#Display results in a histogram, Fig. 2
hist(mns, main="Fig. 2 - 1000 averages of 40 exponentials")

#Findings: The second distribution, the distribution of averages, has a more 
#Gaussian distribution than the first histogram of 1000 exponentials.



#Calculate the theoretical mean
tm<-1/.20
#Calculate the sample mean
sm<-mean(mns)


#Plot a histogram of the averages
hist(mns, main="Fig. 3 - Distribution of Sample Means of Exponentials")
#Add a red line that shows the theoretical mean, tm
abline(v=tm, col="red")
#Add a blue line that shows the sample mean, sm
abline(v=sm, col="blue")

#Question 2 . Show how variable the sample is (via variance) 
#and compare it to the theoretical variance of the distribution.

#Calculate Theoretical Variance
tv<-((1/.20)^2)/40
#Calculate Sample Variance
sv<-var(mns)
#Print them both so you can see they are almost the same
tv
sv

#Question 3 - Show that the distribution is approximately normal.

ssd<-sd(mns)
tsd<-tm

#Create a curve with the sample data and a curve with the theoretical data
#Overlay those two over the density histogram of the distribution
#This shows how similar they are via the symmetry about the mean with a bell shaped curve, see Fig. 4
hist(mns, main="Fig. 4 Dist of Sample Mean vs. Theoretical",prob=T)
curve(dnorm(x, mean=mean(mns), sd=sd(mns)), add=TRUE, col="darkblue", lwd=.5) 
curve(dnorm(x, mean=tm, sd=sqrt(tv)), add=TRUE, col="red", lwd=.5) 