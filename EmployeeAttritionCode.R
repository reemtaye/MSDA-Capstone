#Project c772 Employee Attrition
install.packages(c("factoextra","arules","graphics","MASS","ggpubr","gmodels","ca","lsr","data.table","gtools","ggplot2","plotly","caTools","caret","purrr","tidyr","GGally","corpcor","corrplot","mctest","tidyverse"))
library(corpcor)
setwd("C://Users/reem/Dropbox/WGU/C772")
rawdata <- read.csv("rawdata.csv", header = TRUE)

#Remove Factors with only 1 level and ID Columns
str(rawdata)
rawd<-subset(rawdata,select=-c(Over18,EmployeeCount,StandardHours,EmployeeNumber))
str(rawd)
rawd<-rawdata
#dummy coding factors as integers

rawd$Attrition <- ifelse(rawd$Attrition=="Yes", 1, 0)
rawd$Gender <- ifelse(rawd$Gender=="Male",0,1)
rawd$OT <- ifelse(rawd$OT=="Yes",1,0)
rawd$BusinessTravel<-as.integer(rawd$BusinessTravel)
rawd$Attrition<-as.integer(rawd$Attrition)
rawd$Education<-as.integer(rawd$Education)
rawd$EnvironmentSatisfaction<-as.integer(rawd$EnvironmentSatisfaction)
rawd$Gender<-as.integer(rawd$Gender)
rawd$JobInvolvement<-as.integer(rawd$JobInvolvement)
rawd$JobLevel<-as.integer(rawd$JobLevel)
rawd$JobSatisfaction<-as.integer(rawd$JobSatisfaction)
rawd$MaritalStatus<-as.integer(rawd$MaritalStatus)
rawd$Department <-as.integer(rawd$Department )
rawd$EducationField <-as.integer(rawd$EducationField )
rawd$JobRole <-as.integer(rawd$JobRole )
rawd$OT <-as.integer(rawd$OT )
#validate data types are now all int
str(rawd)


#checking for missing values
rawd[!complete.cases(rawd),]

#Quick Univariate Frequencies
rawd %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(binwidth = 1)


#Run Factor Analysis on all features to find correlations

library(FactoMineR)
dfm<-as.data.frame(rawd)
FAMD(dfm,graph=TRUE)
#corrplot to show associations needing investigation


N <- cor(read.csv("emat.csv", header = TRUE))
corrplot(N, type = "upper",order = "hclust" ,method="circle", tl.col="black",tl.srt=75, 
         sig.level = 0.05, insig = "blank", tl.cex=.7 )

CO<-subset(N,select=-c(CommuteDistance,HrlyRate,Raise,
                          Performance,Gender,DailyRate,Travel))
str(CO)
Z<- cor(CO)
corrplot(Z, type = "upper",order = "hclust" ,method="number", tl.col="black",tl.srt=75,sig.level = 0.05, insig = "blank", tl.cex=.7 )

co1<-subset(N,select=c(YrsSinceLastPromotion,YrsInCurrentRole,YrsWithCurrManager, YrsAtCompany,Age,TotalWorkingYears,MonthlyIncome,MaritalStatus,StockOptionLevel,Attrition ))
corrplot(cor(co1), number.cex=.7,type = "upper",order = "hclust" ,method="number", tl.col="black",tl.srt=75,sig.level = 0.05, insig = "blank", tl.cex=.7 )

col2<-subset(N,select=c(YrsSinceLastPromotion,YrsInCurrentRole,TotalWorkingYears,MaritalStatus,Attrition ))
corrplot(cor(col2), number.cex=.7,type = "upper",order = "hclust" ,method="number", tl.col="black",tl.srt=75,sig.level = 0.05, insig = "blank", tl.cex=.7 )



#Correlation Plots
f<-data.frame(N)
ggplot(f,aes(MaritalStatus,StockOptionLevel)) +
  geom_point() +
  theme_minimal()

ggplot(f, aes(YrsSinceLastPromotion,YrsInCurrentRole)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +theme_light()

ggplot(f, aes(YrsSinceLastPromotion,YrsWithCurrManager)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +theme_light()

ggplot(f, aes(YrsSinceLastPromotion,YrsAtCompany)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +theme_light()

ggplot(f, aes(YrsWithCurrManager,YrsAtCompany)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +theme_light()

ggplot(f, aes(YrsWithCurrManager,YrsInCurrentRole)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +theme_light()

ggplot(f, aes(YrsAtCompany,YrsInCurrentRole)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +theme_light()

ggplot(f, aes(Department,JobRole))

cor.test(f)

#Correlation values 
round(N,4)
round(Z,3)
cor.test(f$YrsSinceLastPromotion,rawd$YrsInCurrentRole, method="pearson")
cor.test(f$YrsSinceLastPromotion,rawd$YrsInCurrentRole, method="spearm", exact=FALSE)
cor.test(rawd$JobLevel,rawd$YrsAtCompany, method="pearson", exact=FALSE)
cor.test(rawd$Department,rawd$JobRole, method="pearson")

#Scatter
ggscatter(f, x = "YrsSinceLastPromotion", y = "YrsInCurrentRole", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "YrsSinceLastPromotion", ylab = "YrsAtCompany")
ggscatter(f, x = "YrsSinceLastPromotion", y = "YrsAtCompany", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "YrsAtCompany", ylab = "YrsInCurrentRole")
ggscatter(f, x = "YrsAtCompany", y = "YrsInCurrentRole", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "YrsSinceLastPromotion", ylab = "YrsInCurrentRole")


cor.test(f$YrsSinceLastPromotion, f$YrsInCurrentRole, 
                    method = "pearson")
cor.test(f$YrsSinceLastPromotion, f$YrsAtCompany, 
                    method = "pearson")
cor.test(f$YrsAtCompany, f$YrsInCurrentRole, 
                    method = "pearson")
cor.test(f$MonthlyIncome, f$YrsInCurrentRole, 
                    method = "pearson")
cor.test(f$YrsSinceLastPromotion, f$MonthlyIncome, 
                    method = "pearson")
cor.test(f$MonthlyIncome, f$TotalWorkingYears, 
                    method = "pearson")
cor.test(f$MonthlyIncome, f$YrsAtCompany, 
                    method = "pearson")
cor.test(f$YrsAtCompany, f$YrsInCurrentRole, 
                    method = "pearson")
cor.test(f$YrsSinceLastPromotion, f$YrsInCurrentRole, 
                    method = "pearson")
cor.test(f$YrsSinceLastPromotion, f$YrsInCurrentRole, 
                    method = "pearson")






#Contingency Tables
colnames(rawd)

CrossTable(rawd$Department,rawd$JobRole)
CrossTable(rawd$MaritalStatus,rawd$Attrition)
CrossTable(rawd$JobSatisfaction,rawd$Attrition)
CrossTable(rawd$WorkLifeBalance,rawd$Attrition)


#CramersV test for independent variables vs. target variable
cramersV(table(rawd$Attrition, rawd$EnvironmentSatisfaction))
cramersV(table(rawd$Attrition, rawd$JobRole))
cramersV(table(rawd$Attrition, rawd$WorkLifeBalance))
cramersV(table(rawd$Attrition, rawd$BusinessTravel))
cramersV(table(rawd$Attrition, rawd$Department))
cramersV(table(rawd$Attrition, rawd$MaritalStatus))
cramersV(table(rawd$Attrition, rawd$Education))
cramersV(table(rawd$Attrition, rawd$Gender))
cramersV(table(rawd$Attrition, rawd$RelationshipSatisfaction))
cramersV(table(rawd$Attrition, rawd$PerformanceRating))
cramersV(table(rawd$Attrition, rawd$JobLevel ))
cramersV(table(rawd$Attrition, rawd$JobInvolvement ))
cramersV(table(rawd$Attrition, rawd$JobSatisfaction ))
cramersV(table(rawd$Attrition, rawd$StockOptionLevel ))
cramersV(table(rawd$MaritalStatus, rawd$StockOptionLevel ))


#PCA
rawp<-rawd[,c(1,4,6,11,17:19,21,25:26,28:31)]
raw.pca <-prcomp(rawp)
summary(raw.pca)
library(FactoMineR)
estim_ncp(rawp, ncp.min=0, ncp.max=NULL, scale=TRUE, method="GCV")
PCA(rawp, ind.sup =14 )
class(rawd)
HCPC(rawp, nb.clust=0, consol=TRUE)
plot(raw.pca, axes=c(1,2), choice="3D.map", rect=TRUE,
     draw.tree=TRUE, ind.names=TRUE, t.level="all","Hierarchical Classification on Principle Components", centers.plot=FALSE)
str(raw.pca)

rawd2<- cbind(rawd,raw.pca$x[,1:3])


biplot(raw.pca)
dfm<-as.data.frame(rawd)
FAMD(dfm,graph=TRUE)

#svm
library(e1071)
frm = data.frame(rawd, y = as.factor(Attrition))
svmfit = svm(Attrition ~ ., data = rawd, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
(svmfit)

# Classification by SVM
# Creat data for training and test
set.seed(2017)
tr.number<-sample(nrow(rawd),nrow(rawd)/2) 
d.train<-rawd[tr.number,]
d.test<-rawd[-tr.number,]

# base model
svm.model.1<-svm(d.train$Attrition~.,type="C-classification",data=d.train)

# Prediction table for training data
t=table(d.train$Attrition, predict(svm.model.1,d.train[,-2]))/nrow(d.train)
sum(diag(t)) # Correct rate 
# Prediction for test data
t=table(d.test$Attrition,predict(svm.model.1,d.test[,-2]))/nrow(d.test)
sum(diag(t))
d.train[,-2]

# 3-fold cross validation 
svm.model<-svm(Attrition~.,type="C-classification",cost=4,data=rawd,cross=4)
summary(svm.model)

# tuning parameters
# First search 
gamma.range<-10^(-3:3)
cost.range<-10^(-2:2)
tuning<-tune.svm(
  Attrition~.,
  type="C-classification",
  gamma=gamma.range,
  cost=cost.range,
  data=rawd,
  tunecontrol=tune.control(sampling="cross",cross=4)
)
tuning$best.parameters # best parameters at this point 
# gamma=0.001=10^(-3) and cost=100=10^2

set.seed(11)
data_split1 <- createDataPartition(raw.1$Attrition,p = .80, list = FALSE) #raw.1
tr.raw1 <- raw.1[data_split1, ] #training
tst.raw1  <- raw.1[-data_split1, ] #test
set.seed(11)
data_split2 <- createDataPartition(raw.2$Attrition,p = .80, list = FALSE) #raw.2
tr.raw2 <- raw.2[data_split2, ] #raw.
tst.raw2  <- raw.2[-data_split2, ] #test
set.seed(11)
data_split4 <- createDataPartition(raw.4$Attrition,p = .80, list = FALSE) #raw.4
tr.raw4 <- raw.4[data_split4, ] #training
tst.raw4  <- raw.4[-data_split4, ] #test
set.seed(11)
data_split5 <- createDataPartition(raw.5$Attrition,p = .80, list = FALSE) #raw.5
tr.raw5 <- raw.5[data_split5, ] #training
tst.raw5  <- raw.5[-data_split5, ] #test
set.seed(11)
data_split6 <- createDataPartition(raw.6$Attrition ,p = .80, list = FALSE) #raw.6
tr.raw6 <- raw.6[data_split6, ] #training
tst.raw6  <- raw.6[-data_split6, ] #test
table(raw.6$Attrition)
table(tr.raw6$Attrition)
set.seed(11)
Usplit <- createDataPartition(raw$Attrition, p = 0.80, list = FALSE)
train_data <- raw[Usplit, ]
test_data  <- raw[-Usplit, ]
##8: Train Models
# Folds are created on the basis of target variable
set.seed(11)
Train<- tr.raw6
Test<- tst.raw6
tr1<- trainControl(method = 'cv',number = 5)
set.seed(11)
fit.rf <- train(Attrition ~.,Train,method = 'rf', trControl = tr1, ntree=2000) 
set.seed(11)
fit.glm <- train(Attrition ~.,Train,method = 'glm', trControl = trainControl(method = 'cv',number = 3)) 
set.seed(11)
fit.svm <- train(Attrition~.,Train,method = 'svmRadial',trControl = tr1)
set.seed(11)
fit.nn <- train(Attrition ~.,Train,method = 'pcaNNet',trControl = tr1)
set.seed(11)
fit.dt <- train(Attrition ~.,Train,method = 'rpart', trControl = tr1) #Decision Tree
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

```
##9: Vaidate
```{r}

b<-Test$Attrition
roc.curve(b,pred.glm,main="ROC curve \n (RF Feature Selection)")
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
```

s.2=tune.s(svm,X1 ~ ., data=rawd,kernel="radial",
        ranges=list(cost=c(0.001,0.01),gamma=c(0.5,0.1)))
svf = s.2$best.model
summary(svf)
pred=predict(svf,test)

1-tuning$best.performance # accuracy 

# plot of performance grid
plot(tuning, transform.x=log10, transform.y=log10)
# the darker the colour is, there is a higher probability of 
# existance of optimum parameters
# We will explore around a diamond shape place in the plot 
# gamma around 10^(-2) and cost about 10^(1)

# we do not search the left-upper couner because
# as cost parameter hikes, it results in a better 
# one-shot prediction but usually this is over-fitting


# Second search
gamma.range<-10^seq(-3,-1,length=10) # -2 +-1
cost.range<-10^seq(0,2,length=10) # 1 +-1
tuning<-tune.svm(
  Attrition~.,
  type="C-classification",
  gamma=gamma.range,
  cost=cost.range,
  data=d,
  tunecontrol=tune.control(sampling="cross",cross=3)
)
tuning$best.parameters 
plot(tuning, transform.x=log10, transform.y=log10)
# now we get best parameters 
# gamma=0.001668101 and cost=35.93814
# using this result 
tuned.model<-svm(
  Attrition~.,
  type="C-classification",
  gamma=0.001668101,
  cost=35.93814,
  data=d,
  cross=3
)
summary(tuned.model)
# accuracy improved
