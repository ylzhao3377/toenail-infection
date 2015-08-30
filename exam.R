require(faraway)
require(corrplot)
require(lme4)
require(car)
require(MASS)
require(MESS)
require(gee)
require(ggplot2)
require(xlsx)
require(MuMIn)
setwd('~/Documents/stat426/exam4')
View(toenail)
str(toenail)
## Check Correlation
corr <- cor(data.matrix(toenail))
corrplot.mixed(corr)
ggplot(toenail,aes(factor(treatment), y = month, fill=factor(treatment))) +
  geom_boxplot()+ggtitle("Boxplot for treatment and month")+labs(x='treatment')
## Transform some variables into factor
toenail[,c(2,3,5)] <- lapply(toenail[,c(2,3,5)], as.factor)
str(toenail)
## GLMM model
toenail <- data.frame(toenail)
attach(toenail)
mod.1 <- glmer(outcome ~ treatment*month + (1|ID),family = binomial)
summary(mod.1)
mod.1.temp <- glmer(outcome ~ treatment+month + (1|ID),family = binomial)
summary(mod.1.temp)
anova(mod.1,mod.1.temp)
## outliers and influential points
inf <- influence(mod.1, group='ID')
coods.d <- cooks.distance(inf)
cooks.d <- data.frame(coods.d)
dimnames(cooks.d)
par(mar=c(5,3,3,3))
ggplot(aes(x=dimnames(cooks.d)[[1]],y=coods.d))+geom_point()
plot(x=row.names(cooks.d),cooks.d[,1],pch=20,main='Cooks Distance for each ID group',xlab='ID',ylab='Cooks.distance')
identify(x=row.names(cooks.d),cooks.d[,1],row.names(coods.d),tolerance=0.5)
plot(mod.1,id=0.05,idLables=~.ID,pch=3,col='blue',main='Residuals VS fitted values')
remove <- toenail[which(abs(residuals(object = mod.1, type='pearson'))>2),]$ID
toenail.fix <- toenail[which(!toenail$ID %in% remove),]
mod.1.fix <- glmer(outcome ~ treatment*month + (1|ID),data=toenail.fix,family = binomial)
summary(mod.1.fix)

## confidence interval
se <- summary(mod.1.fix)$coefficients[,2]
CI.1 <- cbind(Estimate = summary(mod.1.fix)$coefficients[,1],
                LL = summary(mod.1.fix)$coefficients[,1]-1.96*se,
                UL = summary(mod.1.fix)$coefficients[,1]+1.96*se,
              P.value = summary(mod.1.fix)$coefficients[,4])
write.xlsx(CI.1,file='CI_GLMM.xls')
## fitted.values
attach(toenail.fix)
fitted.values <- ifelse(predict(mod.1.fix,type='response') > 0.5, 1, 0)
Table.1 <- matrix(0,2,2,dimnames = list(c('mod_outcome=1',"mod_outcome=0"),c('data_oucome=1','data_outcome=0')))
Table.1[1,1] <- sum((fitted.values==1)*(outcome==1))
Table.1[1,2] <- sum((fitted.values==0)*(outcome==1))
Table.1[2,1] <- sum((fitted.values==1)*(outcome==0))
Table.1[2,2] <- sum((fitted.values==0)*(outcome==0))
sensitivity=Table.1[1,1]/(Table.1[1,1]+Table.1[2,1])
specificity=Table.1[2,2]/(Table.1[2,2]+Table.1[1,2])
write.xlsx(Table.1,file='Table_GLMM.xls')

## GEE model
mod.2 <- gee(outcome ~ treatment*month,id = ID, 
             corstr="unstructured",data=toenail,scale.fix=TRUE,family = binomial)
summary(mod.2)
mod.2.1 <- update(mod.2, corstr='exchangeable')
mod.2.2 <- update(mod.2, corstr='independence')
model.sel(mod.2,mod.2.1,mod.2.2, rank = QIC)
QIC(mod.2)
## Confidence interval for GEE
se <- summary(mod.2)$coefficients[,4]
CI.2 <- cbind(Estimate = summary(mod.2)$coefficients[,1],
              LL = summary(mod.2)$coefficients[,1]-1.96*se,
              UL = summary(mod.2)$coefficients[,1]+1.96*se,
              P.value = round(2*pnorm(summary(mod.2)$coefficients[,5],0,1),6))
write.xlsx(CI.2,file='CI_GEE.xls')


attach(toenail)
fitted.values <- ifelse(exp(predict(mod.2)) > 0.5, 1, 0)
Table.2 <- matrix(0,2,2,dimnames = list(c('mod_outcome=1',"mod_outcome=0"),c('data_outcome=1','data_outcome=0')))
Table.2[1,1] <- sum((fitted.values==1)*(outcome==1))
Table.2[1,2] <- sum((fitted.values==0)*(outcome==1))
Table.2[2,1] <- sum((fitted.values==1)*(outcome==0))
Table.2[2,2] <- sum((fitted.values==0)*(outcome==0))
sensitivity=Table.2[1,1]/(Table.2[1,1]+Table.2[2,1])
specificity=Table.2[2,2]/(Table.2[2,2]+Table.2[1,2])
write.xlsx(Table.2,file='Table_GEE.xls')

## Transition Model
IDfreq<-table(toenail[,1])
IDlength<-length(IDfreq)
newtoenail<-NULL
for(i in 1:IDlength)
{
  if(IDfreq[i]>1)
  {
    current_upper<-sum(IDfreq[1:i])
    if(i==1)
    {
      current_lower<-1
    }else {
      current_lower<-sum(IDfreq[1:(i-1)])+1
    }
    temp1<-toenail[(current_lower+1):current_upper,]
    temp2<-toenail[current_lower:(current_upper-1),2]
    temp<-cbind(temp1,temp2)
    newtoenail<-rbind(newtoenail,temp)
  }
}
colnames(newtoenail)<-c(colnames(toenail),'preoutcome')

## build transition Model
mod.3 <- glm(outcome ~ treatment*month + preoutcome,family = binomial,data=newtoenail)
summary(mod.3)
mod.3.fix <- stepAIC(mod.3)
summary(mod.3.fix)
## outliers and influential points
par(mar=c(5,3,3,3))
plot(mod.3.fix)
Presiduals = residuals(object = mod.3.fix, type='pearson')
h <- lm.influence(model=mod.3.fix)$h
Sresiduals <- Presiduals/sqrt(1-h)
plot(1:1614,pch=20,Sresiduals, xlab="Observation",col='blue', ylab="Standardized residuals",main='Plot for Outliers')
identify(row.names(newtoenail),Sresiduals,newtoenail$ID,tolerance=0.5)
remove <- which(abs(Sresiduals)>2)
outlierTest(mod.3.fix)
plot(cooks.distance(mod.3.fix),pch=20,col='blue',ylab='Cooks.Distance',main='Plot for Influential points')
identify(row.names(newtoenail),cooks.distance(mod.3),newtoenail$ID,tolerance=0.5)
newtoenail.fix <- newtoenail[-remove,]

## rebuild the model
mod.3.fix <- glm(outcome ~ treatment + month + preoutcome,family = binomial,data=newtoenail.fix)
summary(mod.3.fix)

## confidence interval
se <- summary(mod.3.fix)$coefficients[,2]
CI.3 <- cbind(Estimate = summary(mod.3.fix)$coefficients[,1],
              LL = summary(mod.3.fix)$coefficients[,1]-1.96*se,
              UL = summary(mod.3.fix)$coefficients[,1]+1.96*se,
              P.value = summary(mod.3.fix)$coefficients[,4])
write.xlsx(CI.3,file='CI_TRN.xls')

## Fitted Values
fitted.values <- ifelse(predict(mod.3.fix,type='response') > 0.5, 1, 0)
dim(newtoenail.fix)
length(fitted.values)
attach(newtoenail.fix)
Table.3 <- matrix(0,2,2,dimnames = list(c('mod_outcome=1',"mod_outcome=0"),c('data_outcome=1','data_outcome=0')))
Table.3[1,1] <- sum((fitted.values==1)*(outcome==1))
Table.3[1,2] <- sum((fitted.values==0)*(outcome==1))
Table.3[2,1] <- sum((fitted.values==1)*(outcome==0))
Table.3[2,2] <- sum((fitted.values==0)*(outcome==0))
sensitivity=Table.3[1,1]/(Table.3[1,1]+Table.3[2,1])
specificity=Table.3[2,2]/(Table.3[2,2]+Table.3[1,2])
write.xlsx(Table.3,file='Table_TRN.xls')

## naive model
mod.4 <- glm(outcome ~ treatment*month,data=toenail,family = binomial)
summary(mod.4)
mod.4 <- stepAIC(mod.4)
plot(mod.4)
Presiduals = residuals(object = mod.4, type='pearson')
h <- lm.influence(model=mod.4)$h
Sresiduals <- Presiduals/sqrt(1-h)
plot(1:1908,pch=20,Sresiduals, xlab="Observation",col='blue', ylab="Standardized residuals",main='Plot for Outliers')
identify(row.names(toenail),Sresiduals,toenail$ID,tolerance=0.5)
outlierTest(mod.4)
remove <- which(abs(Sresiduals)>2)
toenail.fix <- toenail[-remove,]


mod.4.fix <- glm(outcome ~ treatment*month,family = binomial,data=toenail.fix)
summary(mod.4.fix)
fitted.values <- ifelse(mod.4.fix$fitted.values > 0.5, 1, 0)
dim(toenail.fix)
length(fitted.values)
attach(toenail.fix)
Table.3 <- matrix(0,2,2,dimnames = list(c('mod_outcome=1',"mod_outcome=0"),c('data_outcome=1','data_outcome=0')))
Table.3[1,1] <- sum((fitted.values==1)*(outcome==1))
Table.3[1,2] <- sum((fitted.values==0)*(outcome==1))
Table.3[2,1] <- sum((fitted.values==1)*(outcome==0))
Table.3[2,2] <- sum((fitted.values==0)*(outcome==0))
sensitivity=Table.3[1,1]/(Table.3[1,1]+Table.3[2,1])
specificity=Table.3[2,2]/(Table.3[2,2]+Table.3[1,2])
write.xlsx(Table.3,file='Table_Na.xls')
ggplot(toenail,aes(visit,fill=outcome))+geom_bar()+facet_wrap(~treatment)+scale_fill_brewer()+
  ggtitle('Outcome at different Visit')

