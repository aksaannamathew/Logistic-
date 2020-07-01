install.packages("readr")
library(readr)
install.packages("ROCR")
library(ROCR)
install.packages("ROSE")
library(ROSE)
install.packages("moments")
library(moments)

bank <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\8 - Logistic\\bank-full.csv")
attach(bank)
View(bank)
str(bank)
summary(bank)

sum(is.na(bank$y)) #no na values

#Creating Dummy Variables
table(bank$job)
bank["Job"] <- as.integer(as.character(factor(bank$job, levels = c("admin", "blue-collar", "entrepreneur", "housemaid", "management",
                                                       "retired", "self-employed", "services", "student", "technician",
                                                       "unemployed", "unknown"), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))))

table(bank$marital)
bank["Marital"] <- as.integer(as.character(factor(bank$marital, levels = c("divorced", "married", "single"), labels = c(0, 1, 2))))

table(bank$education)
bank["Education"] <- as.integer(as.character(factor(bank$education, levels = c("primary", "secondary", "tertiary", "unknown"), labels = c(1, 2, 3, 4))))

table(bank$default)
bank["Default"] <- as.integer(as.character(factor(bank$default, levels = c("no", "yes"), labels = c(0, 1))))

table(bank$housing)
bank["Housing"] <- as.integer(as.character(factor(bank$housing, levels = c("no", "yes"), labels = c(0, 1))))

table(bank$loan)
bank["Loan"] <- as.integer(as.character(factor(bank$loan, levels = c("no", "yes"), labels = c(0, 1))))

table(bank$contact)
bank["Contact"] <- as.integer(as.character(factor(bank$contact, levels = c("cellular", "telephone", "unknown"), labels = c(1, 2, 3))))

table(bank$month)
bank["Month"] <- as.integer(as.character(factor(bank$month, levels = c("apr", "aug", "dec", "feb", "jan", "jul", 
                                                           "jun", "mar", "may", "nov", "oct", "sep"),
                            labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))))

table(bank$poutcome)
bank["Poutcome"] <- as.integer(as.character(factor(bank$poutcome, levels = c("failure", "other", "success", "unknown"), labels= c(1, 2, 3, 4))))

table(bank$y)
bank["Y"] <- as.integer(as.character(factor(bank$y, levels = c("yes", "no"), labels = c(0, 1))))

bank <- subset(bank, select = -c(job,marital,education,default,housing,loan,contact,month,poutcome,y))
attach(bank)
View(bank)
summary(bank)
sapply(bank, skewness)
sapply(bank, kurtosis)

table(bank$Y)
prop.table(table(bank$Y))

#Imbalanced data
bank_balanced_over <- ovun.sample(Y~., data = bank, method = "over", N=79844)$data
table(bank_balanced_over$Y)

bank_balanced_under <- ovun.sample(Y~., data = bank, method = "under", N=10578)$data
table(bank_balanced_under$Y)

bank_balanced_both <- ovun.sample(Y~., data = bank, method = "both", seed = 222, p=0.5, N=45211)$data
table(bank_balanced_both$Y)
attach(bank_balanced_both)

summary(bank_balanced_both)

corr<-cor(bank_balanced_both)
corr
colnames(bank)

#Model Building
model_bank1 <- glm(Y~., data =bank, family = "binomial")
summary(model_bank1)

model_bank2 <- glm(Y~age+balance+duration+campaign+pdays+previous+Job+Marital+Education+Housing+Loan+Contact+Month+Poutcome,
                   data = bank, family = "binomial")
summary(model_bank2)

model_bank3 <- glm(Y~., data = bank_balanced_both, family = "binomial")
summary(model_bank3)

#Prediction
pred_model_1 <- predict(model_bank1, type = c("response"), bank)
confusion_model1 <- table(pred_model_1>0.5, bank$Y)
confusion_model1
TRP1 <- 34682/(700+34682)
TRP1                      #0.9802159
FPR1 <- 3652/(1006+3652)
FPR1                      #0.7840275

pred_model_2 <- predict(model_bank2, type = c("response"), bank)
confusion_model2 <- table(pred_model_2>0.5, bank$Y)
confusion_model2
TPR2 <- 34682/(34682+700) 
TPR2                      #0.9802159
FPR2 <- 3646/(3646+1012)
FPR2                      #0.7827394

pred_model_3 <- predict(model_bank3, type = c("response"), bank_balanced_both)
confusion_model3 <- table(pred_model_3>0.5, bank_balanced_both$Y)
confusion_model3
TPR3 <- 18447/(18447+4190)
TPR3                     #0.8149048            
FPR3 <- 4419/(4419+18155)
FPR3                     #0.1957562

#Accuracy
acc_model1 <-sum(diag(confusion_model1))/sum(confusion_model1)
acc_model1                         #0.8913087

acc_model2 <-sum(diag(confusion_model2))/sum(confusion_model2)
acc_model2                         #0.8914585

acc_model3 <-sum(diag(confusion_model3))/sum(confusion_model3)
acc_model3                         #0.8095817

#ROC curve
roc_curve <- prediction(pred_model_3, bank_balanced_both$Y)
roc_perf <- performance(roc_curve, 'tpr', 'fpr')
plot(roc_perf, colorize=T)
abline(a=0, b=1)


## identify the best value 
rocper<-performance(roc_curve,'acc')
max<-which.max(slot(roc_perf,"y.values")[[1]])
acc<-slot(roc_perf,"y.values")[[1]][max]
cut<-slot(roc_perf,"x.values")[[1]][max]

