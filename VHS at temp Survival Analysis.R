require(readr)

# Import and look at your data, you will need to specify the code to where you have stored these data
#or you can use the 'import dataset' button in the top right panel

VHS_Temp <- read_csv("C:/Users/mayag/Dropbox/VHS/VHS modeling/VHS data 10-8/VHS 10-8 for R.csv")
summary(VHS_Temp)

# Look at the data. Do all of the columns make sense? 
# Each column refers to an individual fish
# Status refers to whether a fish is alive (0) or dead (1)
# Day is the day the fish died, or else the last day it was observed (25, because that is the last day of the experiment)
# Specify that our experimental treatments are factors

VHS_Temp$Temp<-as.factor(VHS_Temp$Temp)
VHS_Temp$Exposure<-as.factor(VHS_Temp$Exposure)

# Summarize data. 'ddply' is a really useful function from the 'plyr' package. If you haven't used this before, check it out by typing '?ddply'. 
# There is a course on this package (plyr) in swirl!

install.packages("plyr")
require (plyr)
summary<-ddply(VHS_Temp, .(Temp, Exposure, Tank),summarize,n=length(Tank),mean=mean(Status) )

# Look at the column in 'summary' called 'mean'. What is this telling us?


#Now we will run a survival analysis

#First install the appropriate packages
install.packages("survival")
install.packages("survminer")
install.packages("coxme")

require(survival) #for basic survival analysis
require(survminer) # helpful for graphing survival curves
require(coxme) #mixed effects cox PH model

VHS_Temp$Temp <- relevel(VHS_Temp$Temp, ref = "14C") # this specifies that our reference is 14 C


#Plot data- does it seem reasonable? 
surv_object<-Surv(time=VHS_Temp$Day, event=VHS_Temp$Status) #Specify event, and time)
fit<-survfit(surv_object~Tank, data=VHS_Temp) #Let's just look at each tank first
ggsurvplot(fit, data=VHS_Temp)

#There are two outlier tanks: A7 and H6

surv_object<-Surv(time=VHS_Temp$Day, event=VHS_Temp$Status) #Specify event, and time)
fit<-survfit(surv_object~Exposure+Temp, data=VHS_Temp) #Let's just look at each tank first
ggsurvplot(fit, data=VHS_Temp)

#Remove two tanks that were VHS exposed and had no mortality- likely due to ineffective exposure

VHS_limited<-VHS_Temp[VHS_Temp$Tank!='A7', ]
VHS_limited<-VHS_limited[VHS_limited$Tank!='H6', ]
VHS_limited<-as.data.frame(VHS_limited)

#Cox Proportional Hazards Model to look at treatment effects, including the nesting of fish within tanks
fit1 <- coxme(Surv(Day,Status) ~ Temp*Exposure +(1|Tank), data = VHS_limited)
summary(fit1)

#What can you say from these results? 
#The exp(coef) gives an odd ratio (or hazard ratio) relative to the reference condition (in this case 14C Control)
#A higher positive number means the hazard of mortality is greater by a factor of that value

#This is great for analyzing differences between data, but not terribly useful for parameterizing a model
#Let's fit the data with an exponential function using the 'survreg function' (fits a regression to a survival 

?survreg

ExpFit<-survreg(Surv(Day, Status)~Temp*Exposure, data=VHS_limited, dist="exponential")

WeibFit<-survreg(Surv(Day, Status)~Temp*Exposure, data=VHS_limited, dist="weibull")

summary(ExpFit)
summary(WeibFit)

#Let's compare our two models to see which is better. The model with the lower 'AIC' value is the best fit. 
#This is called model selection

AIC(ExpFit, WeibFit)

#For plotting the package 'flexsurv' is really useful!

require(flexsurv)
?flexsurvreg

fitE <- flexsurvreg(formula = Surv(Day, Status) ~ Temp*Exposure, data = VHS_limited, dist="exponential")

fitW <- flexsurvreg(formula = Surv(Day, Status) ~ Temp*Exposure, data = VHS_limited, dist="weibull")

plot(fitW)
lines(fitE, col='blue')


##Neither of these curves is a great fit to the data. Can you try some other distributions? Check the help page for some other options. 
