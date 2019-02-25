
  ##1: Load
  
setwd("C://Users/reem/Dropbox/WGU/C772")
rawdata  <- read.csv("wa.csv", header = TRUE)
names(rawdata)
summary(rawdata)
#str(rawdata)
rawdata$EmployeeCount <-NULL
rawdata$EmployeeNumber <-NULL
rawdata$StandardHours <-NULL
rawdata$Over18 <-NULL
raw<-rawdata


##2: Univariate & Bivariate Exploration

library(RColorBrewer)
library(ggplot2)
library(gridExtra)
cc<- scale_fill_brewer(palette="Dark2")
ggplot(raw,aes(Attrition,fill=Attrition))+geom_bar()+cc+ggtitle("Target Variable")
ages<- ggplot(raw,aes(Age,fill=Attrition))+geom_bar()+guides(fill=FALSE)+cc
commute <- ggplot(raw,aes(DistanceFromHome,fill=Attrition))+geom_bar()+guides(fill=FALSE)+cc
dept <- ggplot(raw,aes(Department,fill = Attrition))+geom_bar()+cc
Edu <- ggplot(raw,aes(Education,fill=Attrition))+geom_bar()+guides(fill=FALSE)+cc#+scale_x_discrete(limit = c("1", "2", "3","4","5"), labels = c("HS","College","Bachelors","Masters","Doctor"),palette="Dark2")
EduF <- ggplot(raw,aes(EducationField,fill=Attrition))+geom_bar()+guides(fill=FALSE)+cc
Env <- ggplot(raw,aes(raw$EnvironmentSatisfaction,fill=Attrition))+geom_bar()+guides(fill=FALSE)+cc#+scale_x_continuous(limit = c("1", "2", "3","4"), labels = c("Low","Med","High","V.High"))
HRate <- ggplot(raw,aes(HourlyRate,fill=Attrition))+geom_bar()+cc+guides(fill=FALSE)
Income <- ggplot(raw,aes(MonthlyIncome,fill=Attrition))+geom_histogram()+cc+guides(fill=FALSE)
JInv <- ggplot(raw,aes(JobInvolvement,fill=Attrition))+geom_bar()+guides(fill=FALSE)+cc#+scale_x_discrete(limit = c("1", "2", "3","4"), labels = c("Low","Med","High","V.High"),palette="Dark2")
JLev <- ggplot(raw,aes(JobLevel,fill=Attrition))+geom_bar()+cc+guides(fill=FALSE)
JSat <- ggplot(raw,aes(JobSatisfaction,fill=Attrition))+geom_bar()+guides(fill=FALSE)+cc#+scale_x_discrete(limit = c("1", "2", "3","4"), labels = c("Low","Med","High","V.High"),palette="Dark2")
MF <- ggplot(raw,aes(Gender,fill=Attrition))+geom_bar()+guides(fill=FALSE)+cc
MRate <- ggplot(raw,aes(MonthlyRate,fill=Attrition))+geom_histogram()+cc+guides(fill=FALSE)
NumC <- ggplot(raw,aes(NumCompaniesWorked,fill=Attrition))+geom_histogram()+cc
OT <- ggplot(raw,aes(OverTime,fill=Attrition))+geom_bar()+cc+guides(fill=FALSE)
Option <- ggplot(raw,aes(StockOptionLevel,fill = Attrition))+geom_bar()+cc+guides(fill=FALSE)
RSat <- ggplot(raw,aes(RelationshipSatisfaction,fill = Attrition))+geom_bar()+guides(fill=FALSE)+cc#+scale_x_discrete(limit = c("1", "2", "3","4"), labels = c("Low","Med","High","V.High"),palette="Dark2")
Raise <- ggplot(raw,aes(PercentSalaryHike,Attrition))+geom_point(size=4,alpha = 0.01)+cc
Score <- ggplot(raw,aes(PerformanceRating,fill = Attrition))+geom_bar()+guides(fill=FALSE)+cc#+scale_x_discrete(limit = c("1", "2", "3","4"), labels = c("Low","Good","Excellent","Outstanding"),palette="Dark2")
Status <- ggplot(raw,aes(MaritalStatus,fill=Attrition))+geom_bar()+cc+guides(fill=FALSE)
trav <- ggplot(raw,aes(BusinessTravel,fill=Attrition))+geom_bar()+guides(fill=FALSE)+cc
Trning <- ggplot(raw,aes(TrainingTimesLastYear,fill = Attrition))+geom_bar()+cc+guides(fill=FALSE)
Wlb <- ggplot(raw,aes(WorkLifeBalance,fill = Attrition))+geom_bar()+cc+guides(fill=FALSE)#+scale_x_discrete(limit = c("1", "2", "3","4"), labels = c("Bad","Good","Better","Best"),palette="Dark2")
YrAtCom <- ggplot(raw,aes(YearsAtCompany,fill = Attrition))+geom_bar()+cc
YrInCurr <- ggplot(raw,aes(YearsInCurrentRole,fill = Attrition))+geom_bar()+guides(fill=FALSE)+cc
YrsSinceProm <- ggplot(raw,aes(YearsSinceLastPromotion,fill = Attrition))+geom_bar()+guides(fill=FALSE)+cc
YrsCurrMan <- ggplot(raw,aes(YearsWithCurrManager,fill = Attrition))+geom_bar()+guides(fill=FALSE)+cc
grid.arrange(Status,ages,Edu,EduF,MF, NumC,ncol=3,top = "Demographics")
grid.arrange(Option,Trning,Wlb,trav,JLev, commute,OT,dept,ncol=4,top = "Job Features")
grid.arrange(Score,JInv,JSat,RSat,Income,MRate,HRate,Env,YrInCurr,YrsSinceProm,YrsCurrMan,YrAtCom,ncol=3,top = "Engagement")


#Outliers

out <- boxplot.stats(raw$TotalWorkingYears)$out  # outlier values.
boxplot(raw$TotalWorkingYears, main="TotalWorkingYrs", boxwex=0.1)
mtext(paste("Outliers: ", paste(out, collapse=", ")), cex=0.6)
colnames(raw.numeric)
zz <- lm(Age ~ ., data=raw.numeric)
cooksd <- cooks.distance(zz)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 5*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>5*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


#Association Tests w/Target

##4: Association tests w/Target
library(lsr)
att<-raw$Attrition
l<-table(raw$BusinessTravel,att)
chisq.test(l) #X-squared = 24.182, df = 2, p-value = 5.609e-06 
cramersV(l)

m<-table(raw$Department,att)
n<-table(raw$JobRole,att)
o<-table(raw$OverTime,att)
p<-table(raw$Gender,att)
q<-table(raw$MaritalStatus,att)
r<-table(raw$EducationField,att)
s<-table(raw$Education,att)
t<-table(raw$JobInvolvement,att)
u<-table(raw$JobLevel,att)
v<-table(raw$JobRole,att)
w<-table(raw$JobSatisfaction,att)
x<-table(raw$EnvironmentSatisfaction,att)
y<-table(raw$RelationshipSatisfaction,att)
z<-table(raw$PerformanceRating,att)
aa<-table(raw$StockOptionLevel,att)
bb<-table(raw$WorkLifeBalance,att)


chisq.test(m) #X-squared = 10.796, df = 2, p-value = 0.004526 Reject
chisq.test(n) #X-squared = 86.19, df = 8, p-value = 2.802e-15 Reject
chisq.test(o) #X-squared = 87.564, df = 1, p-value < 2.2e-16 Reject
chisq.test(p) #X-squared = 1.117, df = 1, p-value = 0.2906
chisq.test(q) #X-squared = 46.164, df = 2, p-value = 9.456e-11 
chisq.test(r) #X-squared = 16.025, df = 5, p-value = 0.006774 
chisq.test(s)#X-squared = 3.074, df = 4, p-value = 0.5455
chisq.test(t)#X-squared = 28.492, df = 3, p-value = 2.863e-06 
chisq.test(u)#X-squared = 72.529, df = 4, p-value = 6.635e-15 Reject
chisq.test(v)#X-squared = 86.19, df = 8, p-value = 2.802e-15 Reject
chisq.test(w) #X-squared = 17.505, df = 3, p-value = 0.0005563 
chisq.test(x)#X-squared = 22.504, df = 3, p-value = 5.123e-05
chisq.test(y)#X-squared = 5.2411, df = 3, p-value = 0.155
chisq.test(z)#X-squared = 0.00015475, df = 1, p-value = 0.9901
chisq.test(aa) #X-squared = 60.598, df = 3, p-value = 4.379e-13 Reject
chisq.test(bb)#X-squared = 16.325, df = 3, p-value = 0.0009726 Reject


cramersV(m)
cramersV(n)
cramersV(o)
cramersV(p)
cramersV(q)
cramersV(r)
cramersV(s)
cramersV(t)
cramersV(u)
cramersV(v)
cramersV(w)
cramersV(x)
cramersV(y)
cramersV(z)
cramersV(aa)
cramersV(bb)


#Association with e/o




#Cramers V

library(lsr)

a<-table(raw$JobRole,raw$JobLevel) 
b<-table(raw$JobRole,raw$Education)
c<-table(raw$JobRole,raw$Department)
d<-table(raw$JobRole,raw$EducationField)
e<-table(raw$JobRole,raw$MaritalStatus)
f<-table(raw$JobRole,raw$Gender)
g<-table(raw$JobLevel,raw$EducationField)
h<-table(raw$JobLevel,raw$Department)
i<-table(raw$JobLevel,raw$StockOptionLevel)
j<-table(raw$MaritalStatus,raw$StockOptionLevel) 
k<-table(raw$EducationField,raw$Department)
chisq.test(a) 
chisq.test(b)
chisq.test(c)
chisq.test(d)
chisq.test(e)
chisq.test(f)
chisq.test(g)
chisq.test(h)
chisq.test(i)
chisq.test(j)
chisq.test(k)

cramersV(a)
cramersV(b)
cramersV(c)
cramersV(d)
cramersV(e)
cramersV(f)
cramersV(g)
cramersV(h)
cramersV(i)
cramersV(j)
cramersV(k)



#Remove irrelevant categorical vars

raw$Gender<-NULL
raw$PerformanceRating<-NULL
raw$Education<-NULL
raw$RelationshipSatisfaction<-NULL
str(raw)




#visualization 

cc<- scale_fill_brewer(palette="Reds")
#JE<- ggplot(raw,aes(Education,fill=JobRole))+geom_bar()+cc
JEF<- ggplot(raw,aes(EducationField,fill=JobRole))+geom_bar()+cc
#JG<- ggplot(raw,aes(JobRole,fill=Gender))+geom_bar()+cc
JJ<- ggplot(raw,aes(JobLevel,fill=JobRole))+geom_bar()+cc
JD<- ggplot(raw,aes(JobRole,fill=Department))+geom_bar()+cc
DEF<- ggplot(raw,aes(EducationField,fill=Department))+geom_bar()+cc
DJ<- ggplot(raw,aes(JobLevel,fill=Department))+geom_bar()+cc
#EFJ<- ggplot(raw,aes(JobLevel,fill=EducationField))+geom_bar()+cc
#JM<- ggplot(raw,aes(JobRole,fill=MaritalStatus))+geom_bar()+cc
MS<- ggplot(raw,aes(StockOptionLevel,fill=MaritalStatus))+geom_bar()+cc
#SJ<- ggplot(raw,aes(StockOptionLevel,fill=JobLevel))+geom_bar()+cc

grid.arrange(JEF,DEF,ncol=2,top = "Education Field Associations")
grid.arrange(JD,MS,ncol=2,top = "Other Associations")
grid.arrange(DJ,JJ,ncol=2,top = "Job Level Associations")



#Correlations

##3: Create an initial correlation plot to explore relationships 
library(dplyr)
library(corpcor)
library(corrplot)
attach(raw)
shapiro.test(YearsInCurrentRole)
shapiro.test(YearsSinceLastPromotion)
shapiro.test(YearsWithCurrManager)
shapiro.test(YearsAtCompany)
shapiro.test(TotalWorkingYears)
shapiro.test(Age)
shapiro.test(DailyRate)
shapiro.test(MonthlyRate)
shapiro.test(MonthlyIncome)
shapiro.test(NumCompaniesWorked)
shapiro.test(DistanceFromHome)
raw.numeric <- select_if(raw, is.numeric)
#raw.numeric<-subset(raw.numeric, select=-c(4,5,7,8,9,14:16,19))
colnames(raw.numeric)
correlations <-cor(raw.numeric)
corrplot(correlations, tl.srt=25,type="lower", method="number",
         tl.col="black", sig.level=0.05, insig="blank",tl.cex=.8, 
         main="Numeric & Ordinal Correlations",mar=c(0,0,2,0))
#as.data.frame(correlations)

#Continuous Vars

##5: Explore relationships between highly correlated variables
library(caret)
set.seed(11)
Flagged <- findCorrelation(correlations,0.7, verbose=FALSE, names=TRUE,exact=TRUE) #flag only correlations above the cutoff
print(Flagged) #Flagged Columns:"TotalWorkingYears","JobLevel","YearsAtCompany","YearsInCurrentRole","YearsWithCurrManager","MaritalStatus","PercentSalaryHike"  




#Association between continuous vars and target

att<-raw$Attrition
dd<-table(raw$Age,att)
nn<-table(raw$DailyRate,att)
oo<-table(raw$DistanceFromHome,att)
pp<-table(raw$HourlyRate,att)
qq<-table(raw$MonthlyRate,att)
rr<-table(raw$MonthlyIncome,att)
ss<-table(raw$NumCompaniesWorked,att)
tt<-table(raw$PercentSalaryHike,att)
uu<-table(raw$TotalWorkingYears,att)
vv<-table(raw$TrainingTimesLastYear,att)
ww<-table(raw$YearsInCurrentRole,att)
xx<-table(raw$YearsSinceLastPromotion,att)
yy<-table(raw$YearsWithCurrManager,att)
zz<-table(raw$YearsAtCompany,att)

chisq.test(dd)$p.value
chisq.test(nn)$p.value 
chisq.test(oo)$p.value
chisq.test(pp)$p.value
chisq.test(qq)$p.value
chisq.test(rr)$p.value
chisq.test(ss)$p.value
chisq.test(tt)$p.value
chisq.test(uu)$p.value
chisq.test(vv)$p.value
chisq.test(ww)$p.value
chisq.test(xx)$p.value
chisq.test(yy)$p.value
chisq.test(zz)$p.value

cramersV(dd)
cramersV(nn)
cramersV(oo)
cramersV(pp)
cramersV(qq)
cramersV(rr)
cramersV(ss)
cramersV(tt)
cramersV(uu)
cramersV(vv)
cramersV(ww)
cramersV(xx)
cramersV(yy)
cramersV(zz)
colnames(raw.numeric)
ggplot(raw,aes(MonthlyIncome,fill=Attrition))+geom_histogram(position="dodge")+ggtitle("Attrition by Income Level")
ggplot(raw,aes(JobLevel,fill=Attrition))+geom_histogram(position="dodge")+ggtitle("Attrition by Income Level")



#Remove redundant continuous vars

raw$DailyRate<-NULL
raw$MonthlyRate<-NULL
raw$YearsInCurrentRole<-NULL
raw$YearsWithCurrManager<-NULL
raw$MonthlyIncome<-NULL


raw.numeric2 <- select_if(raw, is.numeric)
cor2<-cor(raw.numeric2)
Flagged <- findCorrelation(cor2,0.7, verbose=FALSE, names=TRUE,exact=TRUE) #flag only correlations above the cutoff
print(Flagged)

str(raw)




#Feature Selection

library(randomForest)
library(caret)
library(nnet)

rawX<-raw[complete.cases(raw),]
p<-rawX 
str(raw)
r.control <- trainControl(method="repeatedcv", number=5, repeats=3) #Variable Importance via Classification
set.seed(11)
#importance.model.rf <- train(Attrition~., data=p, method="rf", trControl=raw.ctrl)
#library(party)
#rf2<-cforest(Attrition ~ . , data=p, control=cforest_unbiased(mtry=2,ntree=50))
#rf3<-varimpAUC(rf2)

#set.seed(11)
#importance.model.nn <- train(Attrition~., data=p, method="mlp", trControl=raw.ctrl)
set.seed(11)
importance.model.glm <- train(Attrition~., data=p, method="glm", trControl=raw.ctrl)
set.seed(11)
importance.model.svm <- train(Attrition~., data=p, method="svmRadial", trControl=raw.ctrl)

#Feature selection part 2

# var importance
redundant.glm <- varImp(importance.model.glm, scale=TRUE)
#redundant.svm <- varImp(importance.model.svm, scale=TRUE)
redundant.rf <- varImp(importance.model.rf, scale=TRUE)
#redundant.nn <- varImp(importance.model.nn, scale=TRUE)
#redundant.pca <-varImp(importance.model.pca, scale=TRUE)
c<-ggplot(redundant.glm,mapping=aes(redundant.glm))+ggtitle("GLM Feature Importance")
#d<-ggplot(redundant.svm,mapping=aes(redundant.svm))+ggtitle("SVM Feature Importance")
e<-ggplot(redundant.rf,mapping=aes(redundant.rf))+ggtitle("RF Feature Importance")#+geom_bar()#+scale_color_brewer(palette = "Dark2")
#e<-ggplot(rf3,aes(rf3))+ggtitle("RF Feature Importance")+geom_bar()+scale_color_brewer(palette = "Dark2")
#f<-ggplot(redundant.nn,mapping=aes(redundant.nn))+ggtitle("NN Feature Importance")
#grid.arrange(c,d,ncol=2,top="Most important Features: GLM & SVM")
#grid.arrange(e,c,ncol=2,top="Most important Features: RF & GLM")
c
e



##6: Feature Selection



#raw.4<-subset(raw,select=c("Department","Attrition", "EnvironmentSatisfaction", "JobInvolvement","JobLevel", "JobSatisfaction", "MaritalStatus", "OverTime","StockOptionLevel","WorkLifeBalance",   "NumCompaniesWorked", "TrainingTimesLastYear", "YearsSinceLastPromotion")) #rf features
#RF FEATURES
raw.5<-subset(raw,select=c(Age,Attrition,TotalWorkingYears,HourlyRate,DistanceFromHome,OverTime,YearsAtCompany,PercentSalaryHike,NumCompaniesWorked,EnvironmentSatisfaction,JobLevel,StockOptionLevel,JobInvolvement,TrainingTimesLastYear,WorkLifeBalance,JobSatisfaction))
#svm features

#raw.6<-subset(raw,select=c("Age","Attrition","PercentSalaryHike","YearsSinceLastPromotion","DistanceFromHome","JobRole","JobLevel","EnvironmentSatisfaction", "JobSatisfaction","WorkLifeBalance","NumCompaniesWorked","YearsAtCompany",  "OverTime","MaritalStatus","TotalWorkingYears"))
colnames(raw)#dataset1
colnames(raw.5)#RF feature dataset


library(e1071)
library(DMwR)
library(rpart)
library(ROSE)
#Oversampling Trials
#SPLIT
split0<-createDataPartition(raw$Attrition,p = .75, list = FALSE) #raw.4
traw <- raw[split0, ] #training
sraw  <- raw[-split0, ] #test
#split4 <- createDataPartition(raw.4$Attrition,p = .75, list = FALSE) #raw.4
#traw4 <- raw.4[split4, ] #training
#sraw4  <- raw.4[-split4, ] #test
set.seed(11)
split5 <- createDataPartition(raw.5$Attrition,p = .75, list = FALSE) #raw.5
traw5 <- raw.5[split5, ] #training
sraw5  <- raw.5[-split5, ] #test
set.seed(11)
#split6 <- createDataPartition(raw.6$Attrition ,p = .75, list = FALSE) #raw.6
#traw6 <- raw.6[split6, ] #training
#sraw6  <- raw.6[-split6, ] #test
table(raw.5$Attrition)
table(traw5$Attrition)

#SMOTE
library(DMwR)
os0 <-SMOTE(Attrition ~ ., traw, perc.over = 100, perc.under=200)
#os4 <- SMOTE(Attrition ~ ., traw4, perc.over = 100, perc.under=200)
os5 <- SMOTE(Attrition ~ ., traw5, perc.over = 100, perc.under=200)
#os6 <- SMOTE(Attrition ~ ., traw6, perc.over = 100, perc.under=200)

prop.table(table(os0$Attrition))
prop.table(table(os5$Attrition))
#prop.table(table(os6$Attrition))

#MODELS

#TRAIN
set.seed(11)
Train<- os0
Test<- sraw
set.seed(11)
fit.rf <- train(Attrition ~.,Train,
                method = 'rf', ntree = 2000, 
                tuneGrid = data.frame(mtry = 6)) 
set.seed(11)
fit.glm <- train(Attrition ~.,Train,
                 method = 'glm') 
set.seed(11)
fit.svm <- train(Attrition~.,Train,
                 method = 'svmRadial', 
                 cost=1,gamma=0.001)
set.seed(11)
fit.nn <- train(Attrition ~.,Train,
                method = 'pcaNNet')
set.seed(11)
fit.dt <- train(Attrition ~.,Train,
                method = 'rpart') #Decision Tree
print(fit.rf)

#PREDICTIONS

#PREDICT
set.seed(11)
pred.rf <- predict(fit.rf, Test[,-2])
set.seed(11)
pred.nn <- predict(fit.nn, Test[,-2])
set.seed(11)
pred.glm <- predict(fit.glm, Test[,-2])
set.seed(11)
pred.svm <- predict(fit.svm,Test[,-2])
set.seed(11)
pred.dt <- predict(fit.dt,Test[,-2])
#gamma.range<-10^(-3:3)
#cost.range<-10^(-2:2)
#tuning<-tune.svm(Attrition~.,type="C-classification",
# gamma=gamma.range,cost=cost.range, data=os6)
#tuning$best.parameters
#tuning
summary(pred.rf)


#VALIDATE
b<-Test$Attrition
roc.curve(b,pred.glm,main="ROC curve: RF")
roc.curve(b,pred.svm,add.roc=TRUE, col=2)
roc.curve(b,pred.nn,add.roc=TRUE, col=3)
roc.curve(b,pred.rf,add.roc=TRUE, col=4)
roc.curve(b,pred.dt,add.roc=TRUE, col=5)
legend("bottomright", c("Logistic Regression", "Support Vector Machines", "Neural Network", "Random Forest","Decision Tree"), 
       col=1:6, lty=1:6, lwd=2)


confusionMatrix(b,pred.glm)
confusionMatrix(b,pred.nn)
confusionMatrix(b,pred.rf)
confusionMatrix(b,pred.svm)
confusionMatrix(b,pred.dt)
