
#=============================================================================================
# Model performance due to increase in sample size ,model complexity....
#effct on test error and RMSE
#=============================================================================================


#Data :Cars93
library(MASS)
df<-Cars93
str(df)

nrow(df)

set.seed(1)
df <- df[1:93,]

train_df <- sample(1:nrow(df),20)
df_test <- df[-train_df,]
test_df <- sample(1:nrow(df_test),)


train = df[train_df,]
test = df[test_df, ]

train_rss <- c()
test_rss <- c()
View(Cars93)
#=============================================================================================
#=============================================================================================
#I  - Fitting model with order as 7 , having different sample sizes
#=============================================================================================


m1 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)

m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss <- c(train_rss,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m1)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 30
#=============================================================================================

rand = sample(1:nrow(df),30)

train = df[rand,]

m2 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')


#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss <- c(train_rss,tr_m2)


#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 50
#=============================================================================================

rand = sample(1:nrow(df),50)

train = df[rand,]

m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m3

plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')


#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss <- c(train_rss,tr_m3)


#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 70
#=============================================================================================

rand = sample(1:nrow(df),70)

train = df[rand,]

m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='red',type='l')


#Train Accuracy :
tr_m4 = sum(m3$residuals^2)
train_rss <- c(train_rss,tr_m4)


#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m4)



#Dataset obsservation is small 
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 100
#=============================================================================================

rand = sample(1:nrow(df),100)

train = df[rand,]

m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='red',type='l')


#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss <- c(train_rss,tr_m5)


#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m5)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 200
#=============================================================================================

rand = sample(1:nrow(df),200)

train = df[rand,]

m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='red',type='l')


#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss <- c(train_rss,tr_m6)


#Test Accuracy :
pred = predict(m6, newdata=test)
ts_m6 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m6)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 350
#=============================================================================================


rand = sample(1:nrow(df),350)

train = df[rand,]

m7 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m7

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m7)[order(train$Weight)],col='red',type='l')


#Train Accuracy :
tr_m7 = sum(m7$residuals^2)
train_rss <- c(train_rss,tr_m7)


#Test Accuracy :
pred = predict(m7, newdata=test)
ts_m7 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m7)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 400
#=============================================================================================

rand = sample(1:nrow(df),400)

train = df[rand,]

m8 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m8

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m8)[order(train$Weight)],col='red',type='l')


#Train Accuracy :
tr_m8 = sum(m8$residuals^2)
train_rss <- c(train_rss,tr_m8)


#Test Accuracy :
pred = predict(m, newdata=test)
ts_m8 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m8)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 450
#=============================================================================================

rand = sample(1:nrow(df),450)

train = df[rand,]

m9 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m9

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')


#Train Accuracy :
tr_m9 = sum(m9$residuals^2)
train_rss <- c(train_rss,tr_m9)


#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m9 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m9)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 500
#=============================================================================================

rand = sample(1:nrow(df),500)

train = df[rand,]

m10 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
          + I(Weight^6) + I(Weight^7),train)
m10

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Train$Weight',ylab='Train$MPG.city',main = 'MPG.city(miles per gallon) vs Weight of a car')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')


#Train Accuracy :
tr_m10 = sum(m10$residuals^2)
train_rss <- c(train_rss,tr_m10)


#Test Accuracy :
pred = predict(m10, newdata=test)
ts_m10 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m10)

print(train_rss)
print(test_rss)

# ------cant run------
#=============================================================================================
#Plotting Test Error vs sample size
x <-  c(20,30,50,70)
plot(x = x,y = test_rss,type='l',col='dark green',main = 'Test RSS of different sample size',ylab = 'Test RSS ',xlab = 'Sample size n')
text(x, test_rss, round(test_rss, 2), cex=0.6,col='black')

#=============================================================================================


#=============================================================================================
#=============================================================================================
#II -  model with same sample size and different orders of complexity
#=============================================================================================
#=============================================================================================


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size = 20
#=============================================================================================

train_df <- sample(1:nrow(df),20)
set.seed(1)
test_df <- sample(1:nrow(df_test),)


train = df[train_df,]
test = df[test_df, ]

train_rss <- c()
test_rss <- c()


m1 <- lm(MPG.city ~ Weight,train)
m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,type='n',pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg(miler per galon) inside city',main = 'Regression line fitted Over different Order Polynomial')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='brown',type='l')


#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss <- c(train_rss,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$Weight)^2)

test_rss <- c(test_rss,ts_m1)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size = 20
#=============================================================================================

m2 <- lm(MPG.city ~ Weight + I(Weight^2),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,type='n',pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg(miler per galon) inside city',main = 'Regression line fitted Over different Order Polynomial')
lines(sort(train$Weight),fitted(m2)[order(train$Weight)],col='brown',type='l')


#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss <- c(train_rss,tr_m2)

#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$Weight)^2)
test_rss <- c(test_rss,ts_m2)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3  --- With Training sample size = 20
#=============================================================================================


m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3),train)
m3

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,type='n',pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg(miler per galon) inside city',main = 'Regression line fitted Over different Order Polynomial')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='brown',type='l')


#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss <- c(train_rss,tr_m3)

#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$Weight)^2)
test_rss <- c(test_rss,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4  --- With Training sample size = 20
#=============================================================================================


m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4),train)
m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,type='n',pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg(miler per galon) inside city',main = 'Regression line fitted Over different Order Polynomial')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='brown',type='l')


#Train Accuracy :
tr_m4 = sum(m4$residuals^2)
train_rss <- c(train_rss,tr_m4)

#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$Weight)^2)
test_rss <- c(test_rss,ts_m4)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5  --- With Training sample size = 20
#=============================================================================================


m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,type='n',pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg(miler per galon) inside city',main = 'Regression line fitted Over different Order Polynomial')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='brown',type='l')


#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss <- c(train_rss,tr_m5)

#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$Weight)^2)
test_rss <- c(test_rss,ts_m5)




#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6  --- With Training sample size = 20
#=============================================================================================


m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,type='n',pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg(miler per galon) inside city',main = 'Regression line fitted Over different Order Polynomial')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='brown',type='l')


#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss <- c(train_rss,tr_m5)

#Test Accuracy :
pred = predict(m6, newdata=test)
ts_m6 = sum((pred-test$Weight)^2)
test_rss <- c(test_rss,ts_m6)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size = 20
#=============================================================================================


m7 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6) + I(Weight^7),train)
m7

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,type='n',pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg(miler per galon) inside city',main = 'Regression line fitted Over different Order Polynomial')
lines(sort(train$Weight),fitted(m7)[order(train$Weight)],col='brown',type='l')


#Train Accuracy :
tr_m7 = sum(m7$residuals^2)
train_rss <- c(train_rss,tr_m7)

#Test Accuracy :
pred = predict(m7, newdata=test)
ts_m7 = sum((pred-test$Weight)^2)
test_rss <- c(test_rss,ts_m7)
#adding legend to graph

legend(x=-1.013172,y = 0.03949515, legend=c("Order 1","Order 2","Order 3","Order 4","Order 5","Order 6","Order 7"),
       col=c("blue", "pink","magenta","orange","black","purple","green"), 
       lty=1,cex=0.5,box.col = 'Blue')

print(train_rss)
print(test_rss)

#=============================================================================================
#=============================================================================================
#III-  train a model with 4 different sample size of 20 with different order of complexity
#=============================================================================================
#=============================================================================================

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(s1) = 20 
#=============================================================================================


train_df <- sample(1:nrow(df),20)
set.seed(1)
test_df <- sample(1:nrow(df_test),50)


train = df[train_df,]
test = df[test_df, ]

train_rss <- c()
test_rss <- c()


m1 <- lm(MPG.city ~ Weight ,train)
m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss <- c(train_rss,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m1)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S1) = 20
#=============================================================================================


m2 <- lm(MPG.city ~ Weight + I(Weight^2),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m2)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss <- c(train_rss,tr_m2)

#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S1) = 20
#=============================================================================================


m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m3

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss <- c(train_rss,tr_m3)

#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S1) = 20
#=============================================================================================


m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8),train)
m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m4 = sum(m4$residuals^2)
train_rss <- c(train_rss,tr_m4)

#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m4)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S1) = 20
#=============================================================================================


m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss <- c(train_rss,tr_m5)

#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m5)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S1) = 20
#=============================================================================================


m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9)
         + I(Weight^10),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='MPG at City',main = 'Weight vs Mpg.city')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss <- c(train_rss,tr_m6)



#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m6 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m6)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S2) = 20 
#==========================================================



train_df <- sample(1:nrow(df),20)
set.seed(2)
test_df <- sample(1:nrow(df_test),50)


train = df[train_df,]
test = df[test_df, ]

train_rss_1 <- c()
test_rss_1 <- c()


m1 <- lm(MPG.city ~ Weight ,train)
m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m1)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S2) = 20
#=============================================================================================


m2 <- lm(MPG.city ~ Weight + I(Weight^2),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m2)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m2)

#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S2) = 20
#=============================================================================================


m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m3

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m3)

#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S2) = 20
#=============================================================================================


m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8),train)
m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m4 = sum(m4$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m4)

#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m4)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S2) = 20
#=============================================================================================


m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m5)

#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m5)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S2) = 20
#=============================================================================================


m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9)
         + I(Weight^10),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m6)

#Test Accuracy :
pred = predict(m6, newdata=test)
ts_m6 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m6)

print(train_rss_1)
print(test_rss_1)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S3) = 20 
#=============================================================================================


train_df <- sample(1:nrow(df),20)
set.seed(2)
test_df <- sample(1:nrow(df_test),50)


train = df[train_df,]
test = df[test_df, ]

train_rss_2 <- c()
test_rss_2 <- c()


m1 <- lm(MPG.city ~ Weight ,train)
m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m1)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S3) = 20
#=============================================================================================


m2 <- lm(MPG.city ~ Weight + I(Weight^2),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m2)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m2)

#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S3) = 20
#=============================================================================================


m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m3

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m3)

#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S3) = 20
#=============================================================================================


m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8),train)
m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m4 = sum(m4$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m4)

#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m4)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S3) = 20
#=============================================================================================


m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m5)

#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m5)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S3) = 20
#=============================================================================================


m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9)
         + I(Weight^10),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m6)

#Test Accuracy :
pred = predict(m6, newdata=test)
ts_m6 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m6)

print(train_rss_2)
print(test_rss_2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S4) = 20 
#=============================================================================================


train_df <- sample(1:nrow(df),20)
set.seed(3)
test_df <- sample(1:nrow(df_test),50)


train = df[train_df,]
test = df[test_df, ]

train_rss_3 <- c()
test_rss_3 <- c()


m1 <- lm(MPG.city ~ Weight ,train)
m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m1)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S4) = 20
#=============================================================================================


m2 <- lm(MPG.city ~ Weight + I(Weight^2),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m2)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m2)

#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S4) = 20
#=============================================================================================


m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m3

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m3)

#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S4) = 20
#=============================================================================================


m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8),train)
m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m4 = sum(m4$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m4)

#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m4)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S4) = 20
#=============================================================================================


m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m5)

#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m5)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S4) = 20
#=============================================================================================


m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9)
         + I(Weight^10),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m6)

#Test Accuracy :
pred = predict(m6, newdata=test)
ts_m6 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m6)


print(train_rss_3)
print(test_rss_3)



x <-  c(1,2,7,8,9,10)
plot(x = x,y = test_rss_1,type='l',col='red',main = 'Test Error Vs Complexity over different sample size (n =20)',ylab = 'Test Error Observed(e)',xlab = 'Complexity(Degree of Polynomial)')
lines(x = x,y= test_rss_1 , type='l',col = 'blue')
lines(x = x,y= test_rss_2 , type='l',col = 'green')
lines(x = x,y= test_rss_3 , type='l',col = 'pink')
legend(x=0.8299141,y = 9303091, legend=c("Sample 1","Sample 2","Sample 3","Sample 4"),
       col=c("red","black","green","pink"), 
       lty=1,cex=0.5,box.col = 'Blue')


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S1) = 100
#=============================================================================================


train_df <- sample(1:nrow(df),100)
set.seed(1)
test_df <- sample(1:nrow(df_test),150)


train = df[train_df,]
test = df[test_df, ]

train_rss <- c()
test_rss <- c()


m1 <- lm(MPG.city ~ Weight ,train)
m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss <- c(train_rss,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m1)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S1) = 100
#=============================================================================================


m2 <- lm(MPG.city ~ Weight + I(Weight^2),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m2)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss <- c(train_rss,tr_m2)

#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S1) = 100
#=============================================================================================


m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m3

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss <- c(train_rss,tr_m3)

#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S1) = 100
#=============================================================================================


m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8),train)
m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m4 = sum(m4$residuals^2)
train_rss <- c(train_rss,tr_m4)

#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m4)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S1) = 100
#=============================================================================================


m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss <- c(train_rss,tr_m5)

#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m5)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S1) = 100
#=============================================================================================


m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9)
         + I(Weight^10),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss <- c(train_rss,tr_m6)

#Test Accuracy :
pred = predict(m6, newdata=test)
ts_m6 = sum((pred-test$MPG.city)^2)
test_rss <- c(test_rss,ts_m6)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S2) = 100 
#=============================================================================================


train_df <- sample(1:nrow(df),100)
set.seed(2)
test_df <- sample(1:nrow(df_test),150)


train = df[train_df,]
test = df[test_df, ]

train_rss_1 <- c()
test_rss_1 <- c()


m1 <- lm(MPG.city ~ Weight ,train)
m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m1)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S2) = 100
#=============================================================================================


m2 <- lm(MPG.city ~ Weight + I(Weight^2),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m2)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m2)

#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S2) = 100
#=============================================================================================


m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m3

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m3)

#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S2) = 100
#=============================================================================================


m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8),train)
m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m4 = sum(m4$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m4)

#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m4)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S2) = 100
#=============================================================================================


m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m5)

#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m5)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S2) = 100
#=============================================================================================


m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9)
         + I(Weight^10),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss_1 <- c(train_rss_1,tr_m6)

#Test Accuracy :
pred = predict(m6, newdata=test)
ts_m6 = sum((pred-test$MPG.city)^2)
test_rss_1 <- c(test_rss_1,ts_m6)

print(train_rss_1)
print(test_rss_1)



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S3) = 100 
#=============================================================================================


train_df <- sample(1:nrow(df),100)
set.seed(2)
test_df <- sample(1:nrow(df_test),150)


train = df[train_df,]
test = df[test_df, ]

train_rss_2 <- c()
test_rss_2 <- c()


m1 <- lm(MPG.city ~ Weight ,train)
m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m1)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S3) = 100
#=============================================================================================


m2 <- lm(MPG.city ~ Weight + I(Weight^2),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m2)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m2)

#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S3) = 100
#=============================================================================================


m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m3

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m3)

#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S3) = 100
#=============================================================================================


m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8),train)

m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m4 = sum(m4$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m4)

#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m4)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S3) = 100
#=============================================================================================


m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m5)

#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m5)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S3) = 100
#=============================================================================================


m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9)
         + I(Weight^10),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss_2 <- c(train_rss_2,tr_m6)

#Test Accuracy :
pred = predict(m6, newdata=test)
ts_m6 = sum((pred-test$MPG.city)^2)
test_rss_2 <- c(test_rss_2,ts_m6)

print(train_rss_2)
print(test_rss_2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1  --- With Training sample size(S4) = 100 
#=============================================================================================


train_df <- sample(1:nrow(df),100)
set.seed(3)
test_df <- sample(1:nrow(df_test),150)


train = df[train_df,]
test = df[test_df, ]

train_rss_3 <- c()
test_rss_3 <- c()


m1 <- lm(MPG.city ~ Weight ,train)
m1

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m1)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m1 = sum(m1$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m1)

#Test Accuracy :
pred = predict(m1, newdata=test)
ts_m1 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m1)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2  --- With Training sample size(S4) = 100
#=============================================================================================


m2 <- lm(MPG.city ~ Weight + I(Weight^2),train)
m2

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m2)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m2 = sum(m2$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m2)

#Test Accuracy :
pred = predict(m2, newdata=test)
ts_m2 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m2)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7  --- With Training sample size(S4) = 100
#=============================================================================================


m3 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7),train)
m3

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m3)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m3 = sum(m3$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m3)

#Test Accuracy :
pred = predict(m3, newdata=test)
ts_m3 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m3)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8  --- With Training sample size(S4) = 100
#=============================================================================================


m4 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8),train)
m4

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m4)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m4 = sum(m4$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m4)

#Test Accuracy :
pred = predict(m4, newdata=test)
ts_m4 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m4)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9  --- With Training sample size(S4) = 100
#=============================================================================================


m5 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9),train)
m5

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m5)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m5 = sum(m5$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m5)

#Test Accuracy :
pred = predict(m5, newdata=test)
ts_m5 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m5)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10  --- With Training sample size(S4) = 100
#=============================================================================================


m6 <- lm(MPG.city ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)
         + I(Weight^6) + I(Weight^7) + I(Weight^8) + I(Weight^9)
         + I(Weight^10),train)
m6

#Plotting model over train Data :
plot(train$Weight,train$MPG.city,pch=19,cex=0.5,xlab = 'Weight',ylab='Mpg.city',main = 'Weight vs Mpgcity')
lines(sort(train$Weight),fitted(m6)[order(train$Weight)],col='red',type='l')

#Train Accuracy :
tr_m6 = sum(m6$residuals^2)
train_rss_3 <- c(train_rss_3,tr_m6)

#Test Accuracy :
pred = predict(m6, newdata=test)
ts_m6 = sum((pred-test$MPG.city)^2)
test_rss_3 <- c(test_rss_3,ts_m6)


print(train_rss_3)
print(test_rss_3)



x <-  c(1,2,7,8,9,10)
plot(x = x,y = test_rss,type='l',col='red',main = 'Test Error  Vs Complexity over different sample size (n=100)',ylab = 'Test Error',xlab = 'Complexity')
lines(x = x,y= test_rss_1 , type='l',col = 'black')
lines(x = x,y= test_rss_2 , type='l',col = 'green')
lines(x = x,y= test_rss_3 , type='l',col = 'pink')
legend(x=0.941718,y = 164865.4, legend=c("Sample 1","Sample 2","Sample 3","Sample 4"),
       col=c("red","black","green","pink"), 
       lty=1,cex=0.5,box.col = 'Blue')

#locator(1)

#=============================================================================================
#Plot for RMSE(Root Mean Square Error) VS Complexity.
#=============================================================================================


rmse_train <- sqrt((train_rss)/100)
rmse_train


rmse_test <- sqrt((test_rss)/150)
rmse_test

x <-  c(1,2,7,8,9,10)
plot(x = x,y = rmse_train,type='l',col='red',main = 'RMSE Vs Complexity ',ylab = 'RMSE',xlab = 'Complexity')
lines(x = x,y= rmse_test , type='l',col = 'green')
legend(x=7.722765,y = 0.08767066, legend=c("Train RMSE","Test RMSE"),
       col=c("red","green"), 
       lty=1,cex=0.5,box.col = 'Blue')


