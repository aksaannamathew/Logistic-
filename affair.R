install.packages("readr")
library(readr)
install.packages("ROSE")
library(ROSE)
install.packages("ROCR")
library(ROCR)

aff <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\8 - Logistic\\Logistic regression\\affairs.csv")
attach(aff)
View(aff)

sum(is.na(aff$affairs)) #no missing values
str(aff)
summary(aff)

#Creating Dummy variable
aff['Gend'] <- as.integer(factor(aff$gender, levels = c("male", "female"), labels = c(1, 2)))
aff['Child'] <- as.integer(factor(aff$children, levels = c("yes", "no"), labels = c(1, 2)))
View(aff)
aff["aff.air"] <- ifelse(aff$affairs>0, '1', '0')
aff <- subset(aff, select = -c(affairs, gender, children))
attach(aff)
View(aff)

table(aff$aff.air)
prop.table(table(aff$aff.air))

#Imbalanced data
aff_balanced_over <- ovun.sample(aff.air~., data = aff, method = "over", N=902)$data
table(aff_balanced_over$aff.air)

aff_balanced_under <- ovun.sample(aff.air~., data = aff, method = "under", N=300, seed = 1)$data
table(aff_balanced_under$aff.air)

data_balanced_both <- ovun.sample(aff.air~., data = aff, method = "both", p=0.5, N=601, seed = 222)$data
table(data_balanced_both$aff.air)

summary(data_balanced_both)
attach(data_balanced_both)

plot(data_balanced_both) #Graplical Representation

#Model Building
model <- glm(factor(factor(aff.air))~., data = aff, family = "binomial")
summary(model)

model_aff1 <- glm(factor(aff.air)~yearsmarried+religiousness+rating, family = "binomial")
summary(model_aff1)

model_aff2 <- glm(factor(aff.air)~., data = data_balanced_both, family = "binomial")
summary(model_aff2)

model_aff3 <- glm(factor(aff.air)~religiousness+rating+Child, data = data_balanced_both, family = "binomial")
summary(model_aff3)

exp(coef(model_aff3))
table(data_balanced_both$aff.air)

#Confusion Matrix
prob <- predict(model_aff3,type=c("response"), data = data_balanced_both)
confusion<-table(prob>0.5, data_balanced_both$aff.air)
confusion
TPR = 208/(98+208)
TPR              #0.6797386
FPR = 116/(116+179)
FPR              #0.3932203


#Model Accuracy
Accuracy <- sum(diag(confusion))/sum(confusion)
Accuracy         #0.6439268
Error <- 1-Accuracy
Error            #0.3560732

#ROC Curve
rocrpred <- prediction(prob, data_balanced_both$aff.air)
rocrperf <- performance(rocrpred, 'tpr', 'fpr')
plot(rocrperf, colorize = T)

#Area under the curve
auc <- performance(rocrpred, 'acc')
auc
