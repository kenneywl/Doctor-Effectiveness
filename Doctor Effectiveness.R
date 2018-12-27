
#Objective: To maximize net profit by considering doctor effectiveness individually
#           and in pairs. 

#Methods:1.Clean the data and put it in usable form.
#        2.Do power analysis to determine what we should expect.
#        3A.Determine the effectiveness of each doctor individually. (block by month and day)
#        3B.Determine if certain doctor pairs work better or worse together.
#        3C. Model adequcy checking.
#        4.Interpretation and discussion.
#        5. Final words.
#################################################################################
#################################################################################
#1) Clean the data.

#First we clean the data and put in all in a dataframe so that it is usable.
#This first part is parsing the data to get in a usable form.
#It is not important to follow this part.
sw <- read.csv("SW2018.csv",header=T)
swp <- sw[11:153,c(8,9,13,6,7,10,20,23:26)]
swp <- swp[swp[,"X..OF.DOCTORS"]!=0,]
#there is only one day with one doctor,
#this is not included in the analysis.
swp <- swp[-135,]

swp[,"Pediatric.Dentist.Day."] <- factor(swp[,"Pediatric.Dentist.Day."])
swp[,"TOTAL.PRODUCTION"] <- as.numeric(substring(gsub(",","",gsub(" ","",
                                                 as.character(swp[,"TOTAL.PRODUCTION"]))),2))
swp[,"X..OF.DOCTORS"] <- as.integer(as.character(swp[,"X..OF.DOCTORS"]))

#Now we have some missing values for total patients, we delete them
swp <- swp[-(136:139),]

#This will make the indicator matrix of each doctor:
library(ade4)
docsn <- swp[,8:11]
docsn[docsn==""] <- NA
docsn <- data.frame(sapply(docsn,function(x){as.factor(x)}))
docs <- acm.disjonctif(docsn)
#we can just add repeat docs together to consolidate
names(docs) <- sapply(names(docs),function(x){substring(x,13)})

for(i in 1:22){
  for(j in (i+1):23){
    if(names(docs)[i] == names(docs)[j]){
      docs[i] <- docs[i]+docs[j]
    }
  }
}
#I nulled out the duplicates by hand to avoid a complicated for loop.
docs[,c(8,16,9,17,10,11,18,12,15,19,22,20,21,23)] <- NULL

#I changed a single day with 2 temps to 1 temp to smooth out the analysis
#it makes understanding the data easier and will have little
#effect on the final analysis.

docs[c(32,118),9] <- 1
docs <- data.frame(sapply(docs,function(x){y <- as.factor(x);y}))

#and combind docs with swp:

swp <- cbind(swp[,1:7],docs)
#I remove the factor pediatric here because the effect is included
#when we test for "pino", who is the only pediatric doctor.

names(swp) <- c("Total_Operative","Total_Hygiene","Total_Production",
                "Number_of_Doctors","Pediatric","Total_Patients",
                "Appt_Scheduled", names(swp)[8:16])

sx <- sw[11:153,c(3,4,6)]
sx <- sx[sx[,"X..OF.DOCTORS"]!=0,]
#there is only one day with one doctor,
#this is not included in the analysis.
sx <- sx[-135,]
sx <- sx[-(136:139),]

swp <- cbind(swp,sx[,c(1,2)])

#there is an extranous level in DAY

swp$DAY <- factor(swp$DAY)
swp$MONTH <- factor(swp$MONTH)

#Our data is now usable. The data frame is swp.
#The first few rows are:
head(swp,4)
##########################################################
##########################################################
#2) Do power analysis. What should we expect with
#   different numbers of predictors?

#I included a power analysis because the number of predictors
#is high compared to the number of data points (p=46, n=135)

#We want to use each doctor and also each doctor pair as a predictor.
#and block by 8 months.
#Thats 9 doctors + (9 choose 2) pairs + 8 months + a constant

9+choose(9,2)+8+1

#54 predictors total. We use the library pwr.
#with an effect ratio of .15 and alpha level .05

library(pwr)
pwr.f2.test(u=54,v=135-54,f2=.15,sig.level=.05)

#our power is 36%, that means we have much worse than
#equal chance of finding significance. The standard
#is to want a power of 80%

#At best, assuming the month factor is insignificant we have
#9 + 9 choose 2 + 1 predictors

pwr.f2.test(u=46,v=135-46,f2=.15,sig.level=.05)

#A power of 42% that is too low. 
#Effectivly we don't have enough data.

#If we just had the doctors and the 8 months

pwr.f2.test(u=18,v=135-18,f2=.15,sig.level=.05)

#That's not terrible. 74% is just about good enough.
#Unfortunately, the answer they want is whether
#doctor pairs make any difference. 

#The answer is "Not enough data"

#It is determined that we do not have enough data to
#reasonably expect to get significance from each doctor pair.
#I'll run the models, but I don't expect to get anything.

########################################################################
#############################################################################
#3a) Detrmine the effectiveness of individual doctors.

#Lets build just the blocked factors.

contrasts(swp$DAY) <- "contr.sum" #Force factor to sum to zero.
contrasts(swp$MONTH) <- "contr.sum"

swlm <- lm(Total_Production ~ DAY + MONTH,data=swp)
summary(swlm)

#adj R is .39
#Lets build the main effects.

docsnam <- paste0(c("(",paste0(names(docs),collapse="+"),")"),collapse="")

form <- formula(paste0(c("Total_Production ~  MONTH + DAY +",docsnam), collapse=""))
swlm1 <- lm(form,data=swp)

#Lets look at the model:
anova(swlm1)
summary(swlm1)

#Adjusted R^2 is low: .45


###########################################################################
###########################################################################
#Lets run the doctor pairs.

#3b) Determine effectiveness of doctors indivdually and in pairs.
#We build a linear model. 
#Net_Production ~ DAY + Month + (each doct)+(each doc pair interaction)

#These are the pairs for interaction term.
interact <- NULL
for(i in 1:8){
  for(j in (i+1):9){
    intname <- paste0(c(names(docs)[i],names(docs)[j]),collapse = "*")
    interact <- c(interact,intname)
  }
}

#There should be 9 Choose 2 of them:

choose(9,2);length(interact)

#Good. Now lets put it all together.
docsnam_all <- paste0(c("(",paste0(c(names(docs),interact),collapse="+"),")"),collapse="")

form <- formula(paste0(c("Total_Production ~ MONTH + DAY+",docsnam_all), collapse=""))
swlm2 <- lm(form,data=swp)
summary(swlm2)
anova(swlm2)

#Adj R squared is .53, not terrible, but not good.

#I'll fit one more model with just the significant terms of the above model.
docsnam_int_sig <- paste0(c("(",paste0(c(names(docs),interact[c(21,9,8,4,2)]),
                                       collapse="+"),")"),collapse="")

form <- formula(paste0(c("Total_Production ~ MONTH + DAY +",docsnam_int_sig), collapse=""))
swlm3 <- lm(form,data=swp)
summary(swlm3)

#Adj R is .55

#######################################################################################
#######################################################################################
#3c) Model Adequecy


#We need to first look at correlation of the factors.

docs_n <- sapply(docs,FUN=as.numeric)
cor(docs_n)

#We have two stronger than .5, but not by much. .59 and .54

res <- resid(swlm)
res1 <- resid(swlm1)
res2 <- resid(swlm2)
res3 <- resid(swlm3)

hist(res)
qqnorm(res);qqline(res)

hist(res1)
qqnorm(res1);qqline(res1)

hist(res2)
qqnorm(res2);qqline(res2)

hist(res3)
qqnorm(res3);qqline(res3)

#Residuals are normal across the board.

plot(res~fitted(swlm));abline(h=0)
plot(res1~fitted(swlm1));abline(h=0)
plot(res2~fitted(swlm2));abline(h=0)
plot(res3~fitted(swlm3));abline(h=0)

#There is heteroscedasticity. 
#There is definatly a trend.
#For some the middle the variance increases.
#it starts low gets high and then gets low.
#For some there is an increase in variance
#with an increase in level.

#This does not effect estimates
#but it does effect estimated variances (and hence p-values.)
#Our p-values may be lower than they should be.

plot(fitted(swlm)~swp$Total_Production);abline(c(0,1))
plot(fitted(swlm1)~swp$Total_Production);abline(c(0,1))
plot(fitted(swlm2)~swp$Total_Production);abline(c(0,1))
plot(fitted(swlm3)~swp$Total_Production);abline(c(0,1))

#Actual vs fitted looks good, on the whole.
#there is some slight non-linearity.
#but it is small.

plot(res);abline(h=0)
plot(res1);abline(h=0)
plot(res2);abline(h=0)
plot(res3);abline(h=0)

#Not serially correlated.

#Lets check residuals vs each factor.

plot(res~swp$MONTH)
plot(res~swp$DAY)

#Yep, heteroscedasticity.

for(i in attr(swlm1$terms,"term.labels")[1:11]){
  plot(res1~swp[,i],main=paste0("Residual vs ",i))
  readline("Press enter to continue.")
}

#Yep, heteroscedasticity.

for(i in attr(swlm2$terms,"term.labels")[1:11]){
  plot(res2~swp[,i],main=paste0("Residual vs ",i))
  readline("Press enter to continue.")
}

#Yep, heteroscedasticity.

for(i in attr(swlm3$terms,"term.labels")[1:11]){
  plot(res3~swp[,i],main=paste0("Residual vs ",i))
  readline("Press enter to continue.")
}

#Yep, heteroscedasticity.

#So there may be significance when there isn't any.
#We press forward with this in mind.
##############################################################
##############################################################
#4) Interpretation and Discussion.

#Lets look at all the models together.

anova(swlm,swlm1,swlm2,swlm3)

#The absolutly most significant model compared to our
#blocking factors (Month and Day) is the model with
#the main effects of the doctors alone (with blocked factors.)

#The reduced F-Test shows that the interaction model
#is somewhat significant at .06 compared to blocked factors.
#All the interaction terms together are significant

anova(swlm2)

#The last model is just the significant interaction terms
#and the main effects (and blocked factors)

#Lets look at the significant doctor pairs.
#Kevin and Temp 
#Brittany and Temp
#Heather and Samara
#Brittany and Kevin

#This model look more closely at those

summary(swlm3)

#Heather and Kevin appears to be a good team.
#But Brittany and kevin aren't too good.

#I believe that these interaction terms are not
#important enough to consider when scheduling doctors.
#Not when considering the main effects.

#Lets look at the individual doctor main effects.

summary(swlm1)

#If we just look at significace:
#Pino, Pouyan are significant. But we know that Pino
#Is signifinant because he is a pediatric doctor and
#charges more. So that doesn't help.
#The others are then "average" and should be considered equal.

#The estimated impact on prodution of Pino and Pouyan is:
#Pouyan: +$1267
#Pino: +$1490

#But this doesn't help very much.

#Unfortinuatly there isn't much to help for scheduling.
#It turns out that there isn't much to get from the data.
#Except for Pouyan, which it is known that he produces more.

#Lets look at the base, blocked model.

summary(swlm)

#It is likely that is best to just schedule based on the day.
#Lets make a quick model with just the day.

levels(swp$DAY)

#And it appears that Friday is terrible and monday is great for
#for production, but this makes lots of sense.

levels(swp$MONTH)

#August is good and January isn't.
##########################################################################
##########################################################################
#5) Final words.

#We determined that the data doesn't really have much to help.
#The most signifince came from known points.

#Pouyan makes a lot of money, (he is a pediatric doctor.)
#Friday is terrible, monday is better. So the advice is to try to 
#schedule more on friday to make more even, or schdule more
#doctors on monday. 

#The doctor pairs don't have much significance. Heather and Kevin seem to be
#a good pair and Brittany and Kevin aren't too good.

#August makes a lot of money and January doesn't make a lot. This might
#help in determing the best days to take a vacation.

#There is some information that can be sussed out fromt this data set
#but not much of it is too helpful. The initial question was to determine
#doctor effectivness in order to better schedule.

#The final answer is that the data doesn't show much
#more than they already know.



