install.packages('varhandle')

library(tidyverse)
library(glmnet)
library(gpairs)
library(corrplot)
library(ggplot2)
library(coefplot)
library(mice)
library(readxl)
library(recipes)
library(varhandle)

Cnb_Rate_Summary<-read.csv(file.choose(), header=TRUE, sep=",")

cnb_rate_final<-Cnb_Rate_Summary%>%
  mutate(SALES=as.double(Tot_SALES))%>%
  mutate(SALES=(SALES-mean(SALES))/sqrt(var(SALES)))

y_train<-log(cnb_rate_final$Cannibalization.Rate)


x_train<-model.matrix(UPC_all~CATEGORY+PRODUCT+DESCRIPTION+SEASON+SWEETNER+SALES+TYPE+FLAVOUR+ONTARIO_HOLIDAY+CONSUMPTION_OCCASION,cnb_rate_final)

#ridge
ridge.fit<-glmnet(x = x_train, y = y_train, alpha = 0)
plot(ridge.fit, xvar = "lambda")
crossval <-  cv.glmnet(x = x_train, y = y_train, alpha = 0) 
plot(crossval)
penalty.ridge <- crossval$lambda.min
ridge.opt.fit <-glmnet(x = x_train, y = y_train, alpha = 0, lambda = penalty.ridge) 
coef(ridge.opt.fit)
summary(ridge.opt.fit)

Ridge_Coef<-coef(ridge.opt.fit)
a<-Ridge_Coef@Dimnames[[1]][2:88]
b<-Ridge_Coef@x
Ridge_Coef<-cbind(a,b)

write.csv(Ridge_Coef,'Ridge_Coef_3.csv')

