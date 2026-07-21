rm(list=ls())
library(readxl)
heart_failure_data <- read_excel("C:/Users/Hp/Desktop/heart failure data.xlsx")
data=heart_failure_data
#EDA
head(data)
class(data)
structure(data)
summary(data)
colSums(is.na(data))
sum(duplicated(data))
library(psych)
describe(data)
#a)distribution
library(ggplot2)
num_var=c("age","creatinine_phosphokinase","ejection_fraction","platelets","serum_creatinine","serum_sodium")
for(var in num_var){
  print(ggplot(data,aes(x=.data[[var]]))+geom_histogram(fill="plum",color="black",bins=30)+
          labs(title=paste("Histogram of",var),x=var,y="Frequency"))
}
for(var in num_var){
  print(ggplot(data,aes(x=.data[[var]]))+geom_boxplot(fill="lightblue",color="black")+
          labs(title=paste("Boxplot of",var),x=var,y="Frequency"))
}
#b)correlation matrix
library(corrplot)
cor_matrix=cor(data[,sapply(data,is.numeric)]);cor_matrix
par(mfrow=c(1,1))
corrplot(cor_matrix,method="color",type="full",addCoef.col="black",number.cex=0.7,tl.col="black",tl.srt=45,tl.cex=0.6)
library(ggcorrplot)
ggcorrplot(cor_matrix,type="full",lab=TRUE,lab_size=3,colors=c("purple","white","blue"))
library(ggcorrheatmap)
ggcorrhm(cor_matrix,show_names=TRUE,show_values=TRUE)
#pairplot
library(GGally)
ggpairs(data)
#outcome distribution
ggplot(data,aes(as.factor(DEATH_EVENT)))+geom_bar(fill='purple')+xlab("Death event")
#barcharts for categorical varibales
fac_var=c("anaemia","diabetes","high_blood_pressure","sex","smoking")
for(var in fac_var){
  print(ggplot(data,aes(as.factor(.data[[var]])))+geom_bar(fill="blue")+labs(title=paste("Barplot of",var),x="var",y="Frequency"))
}
  
shapiro.test(data$age)
shapiro.test(data$creatinine_phosphokinase)
shapiro.test(data$ejection_fraction)
shapiro.test(data$platelets)
shapiro.test(data$serum_creatinine)
shapiro.test(data$serum_sodium)
shapiro.test(data$time)

wilcox.test(age~DEATH_EVENT,data=data)
wilcox.test(creatinine_phosphokinase~DEATH_EVENT,data=data)
wilcox.test(ejection_fraction~DEATH_EVENT,data=data)
wilcox.test(platelets~DEATH_EVENT,data=data)
wilcox.test(serum_creatinine~DEATH_EVENT,data=data)
wilcox.test(serum_sodium~DEATH_EVENT,data=data)
wilcox.test(time~DEATH_EVENT,data=data)

tab=table(data$anaemia,data$DEATH_EVENT);tab
chisq.test(tab)
tab=table(data$diabetes,data$DEATH_EVENT)
chisq.test(tab)
tab=table(data$high_blood_pressure,data$DEATH_EVENT)
chisq.test(tab)
tab=table(data$sex,data$DEATH_EVENT)
chisq.test(tab)
tab=table(data$smoking,data$DEATH_EVENT)
chisq.test(tab)

#Logistic regression
model=glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+platelets+serum_creatinine+serum_sodium+time,data=data,family="binomial");model
data$pred_prob=predict(model,type="response")
data$pred_prob
ggplot(data,aes(x=age,y=pred_prob))+geom_point(alpha=0.6)+geom_smooth(method="loess", se=TRUE,color="blue")+
  labs(title="Predicted Probability of Death vs Age",x="Age",y="Predicted Probability")+theme_minimal()
#roc
library(pROC)
roc=roc(data$DEATH_EVENT,fitted(model),xlim=c(0:1));roc
ggroc(roc,colour="blue",linewidth=1.2)+labs(title="ROC curve for logistic regression")+theme_minimal()
or=exp(coef(model))
or
auc=auc(roc);auc
#odds ratio plot
library(broom)
coef_df=tidy(model,exponentiate=TRUE,conf.int=TRUE)
ggplot(coef_df[-1,],aes(x=reorder(term,estimate),y=estimate))+geom_point(size=3)+geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0.2)+
  geom_hline(yintercept=1,linetype="dashed",colour="red")+coord_flip()+
  labs(title="Odds ratio with 95% confidence intervals",
       x="Variables",
       y="Odds ratio")+theme_minimal()
coef_table=cbind(Estimate=coef(model),
                 "Odds ratio"=or,
                 "Lower 95% CI"=exp(confint(model))[,1],
                 "Upper 95% CI"=exp(confint(model))[,2],
                 "p-value"=summary(model)$coefficients[,4])
round(coef_table,4)
summary(model)

#model validation
library(caret)
set.seed(123)
trainIndex=createDataPartition(data$DEATH_EVENT,p=0.8,list=FALSE)
train=data[trainIndex,]
test=data[-trainIndex,]
newmodel=glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+platelets+serum_creatinine+serum_sodium+time,data=train,family="binomial");newmodel
prob=predict(newmodel,newdata=test,type="response");prob
pred=ifelse(prob>0.5,1,0);pred
confusionMatrix(as.factor(pred),as.factor(test$DEATH_EVENT))

#barplots
ggplot(data,aes(as.factor(diabetes),fill=as.factor(diabetes)))+geom_bar(col="black")+ggtitle("Barplot of diabetes")+scale_fill_manual(values=c("0"="skyblue","1"="tomato"),labels=c("No","Yes"),name="Diabetes")+labs(x="Diabetes",y="Count")+theme_minimal()
ggplot(data,aes(as.factor(anaemia),fill=as.factor(anaemia)))+geom_bar(col="black")+ggtitle("Barplot of anaemia")+scale_fill_manual(values=c("0"="skyblue","1"="tomato"),labels=c("Anaemic","Not anaemic"),name="Anaemia")+labs(x="Anaemia",y="Count")+theme_minimal()
ggplot(data,aes(as.factor(high_blood_pressure),fill=as.factor(high_blood_pressure)))+geom_bar(col="black")+ggtitle("Barplot of high blood pressure")+scale_fill_manual(values=c("0"="skyblue","1"="tomato"),labels=c("No","Yes"),name="high blood pressure")+labs(x="high blood pressure",y="Count")+theme_minimal()
ggplot(data,aes(as.factor(sex),fill=as.factor(sex)))+geom_bar(col="black")+ggtitle("Barplot of sex")+scale_fill_manual(values=c("0"="skyblue","1"="tomato"),labels=c("Woman","Man"),name="Sex")+labs(x="Sex",y="Count")+theme_minimal()
ggplot(data,aes(as.factor(smoking),fill=as.factor(smoking)))+geom_bar(col="black")+ggtitle("Barplot of smoking")+scale_fill_manual(values=c("0"="skyblue","1"="tomato"),labels=c("No","Yes"),name="Smoking")+labs(x="Smoking",y="Count")+theme_minimal()

