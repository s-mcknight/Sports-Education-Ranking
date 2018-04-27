#read in data
setwd("~/Classes/Education-Sports-University-Classification/data")
colleges <- read.csv("Colleges.csv", header=TRUE)[,-1]
rownames(colleges) <- colleges$SchoolName

#smaller version of dataset with only important variables
colleges_small <- colleges[, c(2, 4:6, 11, 13, 15, 17, 21:22, 24:34)]

#manipulating variables
colleges_small$Div <- as.factor(colleges_small$Div)
colleges_small$Accredited <- as.factor(colleges_small$Accredited)

#removing colleges that aren't accredited (1 college)
colleges_small <- colleges_small[-c(which(colleges_small$Accredited == 0)),]

#removing accredited variable
colleges_small <- colleges_small[,-12]

#missingness without sports
require(Amelia)
missmap(colleges_small[,-c(5:8)]) #PT Ret and ACT/SAT have most missing

M <- cor(colleges_small[,-1], use="pairwise.complete.obs")
corrplot::corrplot.mixed(M) 

par(mfrow=c(2,2), xpd=FALSE)
library(beanplot)
plot(colleges_small$AvgSAT~colleges_small$MedACT, xlab="Median ACT", ylab="Average SAT", main="a) SAT vs. ACT")
abline(lm(colleges_small$AvgSAT~colleges_small$MedACT),col="red")
text(x=15, y=1400, "r=0.99", col="red")
plot(colleges_small$Comp150~colleges_small$FTRet, xlab="Full-Time Retention Rate", ylab="6-year Completion Rate", main="b) Completion Rate vs. Retention Rate")
abline(lm(colleges_small$Comp150~colleges_small$FTRet), col="red")
text(x=0.4, y=0.9, "r=0.88", col="red")
beanplot(colleges_small$PctPT ~ is.na(colleges_small$PTRet), main="c) Percent Part-Time by Missingness of Part-Time Retention", xlab="Part-Time Retention Missing?", ylab="Percent Part-Time", what=c(1,1,1,0))
boxplot(colleges_small$MedEarn10Yr, main="d) Median Earnings After 10 Years", horizontal=TRUE, xlab="Earnings ($)")


cor(colleges_small$AvgSAT,colleges_small$PctAdmit, use="pairwise.complete.obs")


par(mfrow=c(1,2))
plot(colleges_small$Comp150~colleges_small$PctPT, main="a) Completion Rate vs. Percent Part-Time", xlab="Percent Part-Time", ylab="6-year Completion Rate")
abline(lm(colleges_small$Comp150~colleges_small$PctPT), col="red")
text(x=0.6, y=0.9, "r=-0.52", col="red")
plot(colleges_small$AvgSAT~colleges_small$PctAdmit, xlab="Percent Admitted", ylab="Average SAT", main="b) SAT vs. Admission")
abline(lm(colleges_small$AvgSAT~colleges_small$PctAdmit), col="red")
text(x=0.9, y=1400, "r=-0.45", col="red")

boxplot(colleges_small$PctPT ~ is.na(colleges_small$PTRet), main="Percent Part-Time by Missingness of Part-Time Retention", xlab="Part-Time Retention Missing?", ylab="Percent Part-Time")
#association between Percent part-time and missingness of part-time retention(delete part-time retention)

#deleting variables above
colleges_small <- colleges_small[,-c(9,13,14)]

#variable manipulation
colleges_small$Div <- ifelse(colleges_small$Div==1, 1, 0)
colleges_small <- plyr::rename(colleges_small, c(PCTFLOAN = "PctFLoan"))
colleges_small$PctFLoan <- as.numeric(colleges_small$PctFLoan)

pairs(colleges_small[,c(2:8, 14)]) #sports and completion rate
pairs(colleges_small[,9:17]) #student/school qualities
#median earnings looks skewed

#univariate plots of median earnings
boxplot(colleges_small$MedEarn10Yr, main="Median Earnings After 10 Years", horizontal=TRUE)
boxplot(log(colleges_small$MedEarn10Yr), main="Log Median Earnings After 10 Years", horizontal=TRUE)

#take log of median earnings
colleges_small$logMedEarn10Yr <- log(colleges_small$MedEarn10Yr)
pairs(colleges_small[,c(9:12, 14:18)]) #plot above using log median earnings

#football win percent has about 50% missingness; this is a variable for the existence of a football team
colleges_small$Football <- ifelse(is.na(colleges_small$FBWinPct), 0, 1)

library(GGally)
library(ggplot2)
ggpairs(colleges_small[,c(1,3,5,6,8,19,9,10,18,14,2,17)], columnLabels = c("Division", "# Sports", "MBB Win %", "WBB Win %", "WVB Win %", "Football", "Avg. SAT", "% Part-Time", "log(Earnings)", "6-Yr Completion", "GSR", "% Admit"))
ggpairs(colleges_small[,c(9,18,14,2,3)], columnLabels=c("Avg. Sat", "log(Earnings)", "6-Yr Completion", "GSR", "# Sports")) 

model4 <- '
  #Measurement model
Sports=~Div+NoSports+MBBWinPct+WBBWinPct+WVBWinPct+Football
Student=~AvgSAT+PctPT+logMedEarn10Yr+Comp150+GSR+PctAdmit

#Make a latent trait from two latent traits
overall=~Sports+Student
'
lavaan_sem2 <- lavaan::sem(model4, data=colleges_small, std.lv=TRUE) 
lavaan::summary(lavaan_sem2, fit.measures=TRUE) #fit is  
semPlot::semPaths(lavaan_sem2, "par", mar=c(10,10,10,10), fade=F, layout="tree", nCharNodes=8, label.cex=1.5)
#Robust SEs:
lavaan_sem_r2 <- lavaan::sem(model4, data=colleges_small, std.lv=TRUE,se="robust.huber.white")
lavaan::summary(lavaan_sem_r2, fit.measures=TRUE, standardized=TRUE)
lavaan::parameterEstimates(lavaan_sem_r2)

plot(colleges_small$PctPT, colleges_small$Comp150)
plot(colleges_small$PctAdmit, colleges_small$AvgSAT)

model5 <- 'School=~AvgSAT+logMedEarn10Yr+Comp150+GSR+NoSports'
lavaan_sem3 <- lavaan::sem(model5, data=colleges_small, std.lv=TRUE, se="robust.huber.white") 
lavaan::summary(lavaan_sem3, fit.measures=TRUE, standardized=TRUE) #fit is  
semPlot::semPaths(lavaan_sem3, "par", mar=c(10,10,10,10), fade=F, layout="tree", nCharNodes=8, label.cex=1.5)

require(mice)
set.seed(062117)
# generate 5 multiple complete datasets 
out <- mice(colleges_small, m=5) 
D1 <- complete(out, 1) 
D2 <- complete(out, 2) 
D3 <- complete(out, 3) 
D4 <- complete(out, 4) 
D5 <- complete(out, 5) 

# fit model for each complete dataset 
require(lavaan)
fit1 <- sem(model5, data=D1, std.lv=TRUE, se="robust.huber.white") 
fit2 <- sem(model5, data=D2, std.lv=TRUE, se="robust.huber.white") 
fit3 <- sem(model5, data=D3, std.lv=TRUE, se="robust.huber.white") 
fit4 <- sem(model5, data=D4, std.lv=TRUE, se="robust.huber.white") 
fit5 <- sem(model5, data=D5, std.lv=TRUE, se="robust.huber.white") 

# predict scores for all models 
p1 <- predict(fit1) 
p2 <- predict(fit2) 
p3 <- predict(fit3) 
p4 <- predict(fit4) 
p5 <- predict(fit5) 

# compute average across 5 sets of scores: 
scores_mice <- (p1 + p2 + p3 + p4 + p5)/5 

par(mfrow=c(1,2))
qqnorm(scores_mice, main="Q-Q Plot of Scores")
plot(density(scores_mice), xlab="Score", ylab="Frequency", main="Density of Scores")

require(beanplot)
beanplot(scores_micedf, what=c(0,1,0,0), col="white", main="Distribution of Scores", ylab="Highest Scores      Lowest Scores")
beanplot(scores_micedf[-321], col=c(0,8,8,8), what=c(0,0,0,1), method="stack", add=TRUE)
beanplot(scores_micedf[321], col=c(0,4,4,4), what=c(1,0,1,1), method="stack", add=TRUE)
legend("bottomright", col=4, legend="Montana State", lty=1, box.lty=0)

scores_micedf <- scores_mice[order(scores_mice)]
scores_micedf <- as.data.frame(scores_micedf)
rownames(scores_micedf) <- rownames(colleges_small[order(scores_mice),])
scores_micedf$rank <- 1:646
write.csv(scores_micedf, "rankings.csv")
