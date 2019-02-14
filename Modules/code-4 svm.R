library(caTools)
library(e1071)
library(varhandle)

##### TCS #####
TCS = read.csv('TCS.NS.csv')

TCS[TCS == "null"] = NA
TCS = na.omit(TCS)
TCS = unfactor(TCS)
training_set = TCS[, c("Open", "Close")]
training_set = training_set[, c(2, 1)]
New_open = c()
for(i in 2:length(training_set[, 2])){
  New_open[i] = training_set[i, 2]
}
New_open = na.omit(New_open)
prediction = training_set[length(training_set[,1]), 1]
training_set = training_set[-nrow(training_set),]
training_set$Open = New_open

regressor = svm(formula = Open ~ .,
                data = training_set,
                type = 'eps-regression',
                kernel = 'radial', gamma = 0.59)

y_pred_tcs = predict(regressor, data.frame(Close = prediction))


##### WIPRO #####
WIPRO = read.csv('WIPRO.NS.csv')

WIPRO[WIPRO == "null"] = NA
WIPRO = na.omit(WIPRO)
WIPRO = unfactor(WIPRO)
training_set = WIPRO[, c("Open", "Close")]
training_set = training_set[, c(2, 1)]
New_open = c()
for(i in 2:length(training_set[, 2])){
  New_open[i] = training_set[i, 2]
}
New_open = na.omit(New_open)
prediction = training_set[length(training_set[,1]), 1]
training_set = training_set[-nrow(training_set),]
training_set$Open = New_open

regressor = svm(formula = Open ~ .,
                data = training_set,
                type = 'eps-regression',
                kernel = 'radial', gamma = 1)

y_pred_wipro = predict(regressor, data.frame(Close = prediction))


##### TECHM #####
TECHM = read.csv('TECHM.NS.csv')

TECHM[TECHM == "null"] = NA
TECHM = na.omit(TECHM)
TECHM = unfactor(TECHM)
training_set = TECHM[, c("Open", "Close")]
training_set = training_set[, c(2, 1)]
New_open = c()
for(i in 2:length(training_set[, 2])){
  New_open[i] = training_set[i, 2]
}
New_open = na.omit(New_open)
prediction = training_set[length(training_set[,1]), 1]
training_set = training_set[-nrow(training_set),]
training_set$Open = New_open

regressor = svm(formula = Open ~ .,
                data = training_set,
                type = 'nu-regression',
                kernel = 'linear')

y_pred_techm = predict(regressor, data.frame(Close = prediction))


##### INFOSYS #####
INFY = read.csv('INFY.NS.csv')

INFY[INFY == "null"] = NA
INFY = na.omit(INFY)
INFY = unfactor(INFY)
training_set = INFY[, c("Open", "Close")]
training_set = training_set[, c(2, 1)]
New_open = c()
for(i in 2:length(training_set[, 2])){
  New_open[i] = training_set[i, 2]
}
New_open = na.omit(New_open)
prediction = training_set[length(training_set[,1]), 1]
training_set = training_set[-nrow(training_set),]
training_set$Open = New_open

regressor = svm(formula = Open ~ .,
                data = training_set,
                type = 'eps-regression',
                kernel = 'linear')

y_pred_infy = predict(regressor, data.frame(Close = prediction))
