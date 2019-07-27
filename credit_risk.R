data <- read.csv('LoansTrainingSetV2.csv')
summary(data)
str(data)
names(data)
data_1 <- data[3:19]
names(data_1)

summary(data_1)
str(data_1)

data_1$Tax.Liens = ifelse(is.na(data_1$Tax.Liens),
                     ave(data_1$Tax.Liens, FUN = function(x) mean(x, na.rm = TRUE)),
                     data_1$Tax.Liens)
which(is.na(data_1$Tax.Liens))

data_1$Bankruptcies = ifelse(is.na(data_1$Bankruptcies),
                          ave(data_1$Bankruptcies, FUN = function(x) mean(x, na.rm = TRUE)),
                          data_1$Bankruptcies)

which(is.na(data_1$Bankruptcies))

str(data_1$Maximum.Open.Credit)
which(is.na(data_1$Maximum.Open.Credit))

data_1$Maximum.Open.Credit[data_1$Maximum.Open.Credit == '#VALUE!'] <- NA
data_1$Maximum.Open.Credit <- as.numeric(data_1$Maximum.Open.Credit)

data_1$Maximum.Open.Credit = ifelse(is.na(data_1$Maximum.Open.Credit),
                                    ave(data_1$Maximum.Open.Credit, FUN = function(x) mean(x, na.rm = TRUE)),
                                    data_1$Maximum.Open.Credit)

str(data_1$Current.Credit.Balance)
which(is.na(data_1$Current.Credit.Balance))

str(data_1$Number.of.Credit.Problems)
which(is.na(data_1$Number.of.Credit.Problems))

str(data_1$Number.of.Open.Accounts)
which(is.na(data_1$Number.of.Open.Accounts))

#####################################################################################
str(data_1$Months.since.last.delinquent)
sum(is.na(data_1$Months.since.last.delinquent))
sum(is.na(data_1$Months.since.last.delinquent))/dim(data_1)[1]*100
#####################################################################################

str(data_1$Years.of.Credit.History)
sum(is.na(data_1$Years.of.Credit.History))


str(data_1$Monthly.Debt)
data_1$Monthly.Debt <- as.numeric(lapply(data_1$Monthly.Debt, function(x) as.numeric(gsub("[,$]", "", x))))
str(data_1$Monthly.Debt)
sum(is.na(data_1$Monthly.Debt))

str(data_1$Purpose)
sort(table(data_1$Purpose))

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
data_1$Purpose <- trim(data_1$Purpose)

data_1$Purpose <- gsub('\\s+', '_', data_1$Purpose)

data_1$Purpose <- gsub('other', 'Other', data_1$Purpose)
sum(is.na(data_1$Purpose))

str(data_1$Annual.Income)
sum(is.na(data_1$Annual.Income))

str(data_1$Home.Ownership)
table(data_1$Home.Ownership)
sum(is.na(data_1$Home.Ownership))

str(data_1$Years.in.current.job)
sum(is.na(data_1$Years.in.current.job))
sort(table(data_1$Years.in.current.job))

str(data_1$Credit.Score)
sum(is.na(data_1$Credit.Score))

str(data_1$Term)
table(data_1$Term)
sum(is.na(data_1$Term))

str(data_1$Current.Loan.Amount)
sum(is.na(data_1$Current.Loan.Amount))

str(data_1$Loan.Status)
table(data_1$Loan.Status)
sum(is.na(data_1$Loan.Status))

names(data_1)

write.csv(data_1,"loan_default.csv")

library(sqldf)
simple<-sqldf("select `Current.Loan.Amount`,`Term`,`Credit.Score`,`Years.in.current.job`,
              `Home.Ownership`,`Annual.Income`,`Purpose`,`Monthly.Debt`,`Years.of.Credit.History`,`Months.since.last.delinquent`,
              `Number.of.Open.Accounts`,`Number.of.Credit.Problems`,
              `Current.Credit.Balance`,`Maximum.Open.Credit`,`Bankruptcies`,`Tax.Liens` from data_1")
summary(simple)
# 
# 
library(mice)
md.pattern(simple)
imp_mice<-mice(simple,seed=123,m = 1, maxit = 1)
# 
# imp_mice$
summary(imp_mice)
# 
# 
# imputed<-complete(imp_mice,1)
# summary(imputed)

# polling$Rasmussen<-imputed$Rasmussen
# polling$SurveyUSA<-imputed$SurveyUSA
# summary(polling)

table(data_2$Loan.Status)/nrow(data_2)
#table(train_data$Loan.Status)/nrow(train_data)

library(caTools)
split <- sample.split(data_2$Loan.Status,SplitRatio = 0.7)
test_data <- subset(data_2, split == T)
train_data <- subset(data_2, split ==F)

model_lr <- glm(as.factor(Loan.Status)~.-Tax.Liens-Current.Credit.Balance-Number.of.Open.Accounts
                -Years.of.Credit.History-Maximum.Open.Credit,
                data = train_data, family = 'binomial')

summary(model_lr)

predTrain <- predict(model_lr,type = "response")
hist(predTrain)

conf_matrix <- table(train_data$Loan.Status,predTrain>=0.8)
table(train_data$Loan.Status)

predTest <- predict(model_lr,newdata = test_data, type = "response")
conf_test <- table(test_data$Loan.Status, predTest>=0.8)
table(test_data$Loan.Status)

# ROC in R 
library(ROCR)
ROCRpred<-prediction(predTrain,train_data$Loan.Status)
# Using the performance function 
ROCRperf<-performance(ROCRpred,"tpr","fpr")
performance(ROCRpred,"auc")
plot(ROCRperf)
# Add colors
plot(ROCRperf,colorize=TRUE)

# Add labels 
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

################################################################
### DEcision Tree##############
library(caret)
library(rpart.plot)

tc <- trainControl(method = 'cv', number = 10)
cp <- expand.grid(cp = seq(0.25,0.3,0.0001))
set.seed(1234)
dt <- train(as.factor(Loan.Status)~., data = train_data, method = 'rpart', trControl = tc, 
            tuneGrid = cp)
prp(dt$finalModel)
plot(varImp(dt))

p1 <- predict(dt)
table(train_data$Loan.Status,p1)

p2 <- predict(dt, newdata = test_data)
table(test_data$Loan.Status,p2)
################################################################
### randomforest

tc <- trainControl(method = 'cv', number = 10)
mtry <- expand.grid(mtry = seq(1,5,1))
set.seed(1234)
rf <- train(as.factor(Loan.Status)~., data = train_data, method = 'rf', trControl = tc, 
            tuneGrid = mtry)
prp(rf$finalModel)
plot(varImp(rf))

p1 <- predict(rf)
table(train_data$Loan.Status,p1)

############## ROSE ##############
library(ROSE)
table(train_data$Loan.Status)
over <- ovun.sample(as.factor(Loan.Status)~., data = train_data, method = "under",N = 11478)$data
table(over$Loan.Status)

both <- ovun.sample(as.factor(Loan.Status)~., data = train_data, method = "both",N = 25000)$data
table(both$Loan.Status)


tc <- trainControl(method = 'cv', number = 10)
cp <- expand.grid(cp = seq(0.25,0.3,0.00001))
set.seed(1234)
dt <- train(as.factor(Loan.Status)~., data = both, method = 'rpart', trControl = tc, 
            tuneGrid = cp)
p1 <- predict(dt,newdata = train_data)
table(train_data$Loan.Status,p1)

p2 <- predict(dt,newdata = test_data)
table(test_data$Loan.Status,p2)

#################### SVM 
library(e1071)
library(caret)
tc <- trainControl(method = "cv", number = 10)
c <- expand.grid(C = seq(1,10,length = 10))
set.seed(123)
svm <- train(as.factor(Loan.Status)~., data = both, method = "svmLinear", trControl = tc,
             tuneGrid = c)

#############################
library(neuralnet)
