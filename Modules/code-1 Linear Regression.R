library(caTools)
library(varhandle)

##### TCS #####
TCS = read.csv('TCS.NS.csv')
TCS = unfactor(TCS)
TCS[TCS == "null"] = NA
TCS = na.omit(TCS)

TCS$Open = as.numeric(TCS$Open)
TCS$Close = as.numeric(TCS$Close)
TCS$High = as.numeric(TCS$High)
TCS$Low = as.numeric(TCS$Low)
TCS$Adj.Close = as.numeric(TCS$Adj.Close)

training_set = TCS[, c("Open", "Close", "High", "Low", "Adj.Close")]
training_set = training_set[, c(2, 1, 3, 4, 5)]
New_open = c()
for(i in 2:length(training_set[, 2])){
  New_open[i] = training_set[i, 2]
}
New_open = na.omit(New_open)
prediction = training_set[length(training_set[,1]), -2]

training_set = training_set[-nrow(training_set),]
training_set$Open = New_open
training_set[,] = scale(training_set[,])

regressor = lm(formula = Open ~ Close + High + Low + Adj.Close,
               data = training_set)
summary(regressor)

regressor = lm(formula = Open ~ Close + Low + Adj.Close,
               data = training_set)
summary(regressor)

#regressor = lm(formula = Open ~ Close + Adj.Close,
#               data = training_set)
#summary(regressor)

y_pred_tcs = predict(regressor, newdata = prediction)


##### WIPRO #####
WIPRO = read.csv('WIPRO.NS.csv')
WIPRO = unfactor(WIPRO)
WIPRO[WIPRO == "null"] = NA
WIPRO = na.omit(WIPRO)

WIPRO$Open = as.numeric(WIPRO$Open)
WIPRO$High = as.numeric(WIPRO$High)
WIPRO$Low = as.numeric(WIPRO$Low)
WIPRO$Close = as.numeric(WIPRO$Close)
WIPRO$Adj.Close = as.numeric(WIPRO$Adj.Close)

training_set = WIPRO[, c("Close", "Open", "High", "Low", "Adj.Close")]
New_open = c()
for(i in 2:length(training_set[, 2])){
  New_open[i] = training_set[i, 2]
}
New_open = na.omit(New_open)
prediction = training_set[length(training_set[,1]), -2]

training_set = training_set[-nrow(training_set),]
training_set$Open = New_open
training_set[,] = scale(training_set[,])

regressor = lm(formula = Open ~ High + Low + Close + Adj.Close,
               data = training_set)
summary(regressor)

regressor = lm(formula = Open ~ High + Low + Close,
               data = training_set)
summary(regressor)

y_pred_wipro = predict(regressor, newdata = prediction)


##### TECHM #####
TECHM = read.csv('TECHM.NS.csv')
TECHM = unfactor(TECHM)
TECHM[TECHM == "null"] = NA
TECHM = na.omit(TECHM)

TECHM$Open = as.numeric(TECHM$Open)
TECHM$High = as.numeric(TECHM$High)
TECHM$Low = as.numeric(TECHM$Low)
TECHM$Close = as.numeric(TECHM$Close)
TECHM$Adj.Close = as.numeric(TECHM$Adj.Close)

training_set = TECHM[, c("Close", "Open", "High", "Low", "Adj.Close")]
New_open = c()
for(i in 2:length(training_set[, 2])){
  New_open[i] = training_set[i, 2]
}
New_open = na.omit(New_open)
prediction = training_set[length(training_set[,1]), -2]

training_set = training_set[-nrow(training_set),]
training_set$Open = New_open
training_set[,] = scale(training_set[,])

regressor = lm(formula = Open ~ High + Low + Close + Adj.Close,
               data = training_set)
summary(regressor)

regressor = lm(formula = Open ~ High + Close + Adj.Close,
               data = training_set)
summary(regressor)

regressor = lm(formula = Open ~ High + Close,
               data = training_set)
summary(regressor)

y_pred_techm = predict(regressor, newdata = prediction)


##### INFOSYS #####
INFY = read.csv('INFY.NS.csv')
INFY = unfactor(INFY)
INFY[INFY == "null"] = NA
INFY = na.omit(INFY)

INFY$Open = as.numeric(INFY$Open)
INFY$High = as.numeric(INFY$High)
INFY$Low = as.numeric(INFY$Low)
INFY$Close = as.numeric(INFY$Close)
INFY$Adj.Close = as.numeric(INFY$Adj.Close)

training_set = INFY[, c("Close", "Open", "High", "Low", "Adj.Close")]
New_open = c()
for(i in 2:length(training_set[, 2])){
  New_open[i] = training_set[i, 2]
}
New_open = na.omit(New_open)
prediction = training_set[length(training_set[,1]), -2]

training_set = training_set[-nrow(training_set),]
training_set$Open = New_open
training_set[,] = scale(training_set[,])

regressor = lm(formula = Open ~ High + Low + Close + Adj.Close,
               data = training_set)
summary(regressor)

regressor = lm(formula = Open ~ High + Close + Adj.Close,
               data = training_set)
summary(regressor)

y_pred_infy = predict(regressor, newdata = prediction)
