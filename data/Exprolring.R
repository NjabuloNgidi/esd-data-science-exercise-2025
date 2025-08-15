data <- data
?data
dim(data)
names(data)
rownames(data)
summary  (data)
data_1 <- na.omit(data)
# all the outlines are also the max of each observations we got these by 
boxplot.stats(data_1$GDP)$out
which.max(data_1$GDP)
data_2 <- data_1[-c(47,65,101,81,112,85),]
data_3 <- data_1[-c(which.max(data_1$GDP),which.max(data_1$GFCF),which.max(data_1$UNEM),
                    which.max(data_1$ConsumerPrices),which.max(data_1$GovExp),
                    which.max(data_1$HouseExp)),]
#similarly this is done for each variable this is a long way around however it works 
boxplot(data_3, plot = TRUE, main = "All box plots")
hist(data_3$GDP, plot = TRUE, main = "Histogram of GDP")

boxplot(data_3$GFCF, plot = TRUE , main = "Boxplot of GFCF")
hist(data_3$GFCF, plot = TRUE, main = "Histogram of GFCF")

boxplot(data_3$UNEM, plot = TRUE, main = "Boxplot of Unemployment")
hist(data_3$UNEM, plot = TRUE, main = "Histogram of Unemployment")

boxplot(data_3$ConsumerPrices, plot = TRUE, main = "Boxplot of Consumer Prices")
hist(data_3$ConsumerPrices, plot = TRUE , main = "Histogram of Consumer Prices")

boxplot(data_3$GovExp , plot = TRUE , main = "Box Plot of Govement Expenses")
hist(data_3$GovExp, plot = TRUE , main = "Histogram of Govement Expenses")

boxplot(data_3$HouseExp, plot = TRUE, main = "Boxplot of Household Expenses")
hist(data_3$HouseExp, plot = TRUE, main =  "Histogram of Houshold Expenses")

# Ploting line graphs 
plot (data_2$Date,data_2$GDP,type = "l", main = "Line graph of GDP and time")
plot (data_2$Date,data_2$GFCF,type = "l", main = "Line graph of GFCF and time")
plot (data_2$Date,data_2$UNEM,type = "l", main = "Line graph of UNEM and time")
plot (data_2$Date,data_2$ConsumerPrices,type = "l", main = "Line graph of Consumer Prices and time")
plot (data_2$Date,data_2$GovExp,type = "l", main = "Line graph of GovExp and time")
plot (data_2$Date,data_2$HouseExp,type = "l", main = "Line graph of HouseExp and time")
# all the graphs seem to have an upward trend and seem to be correlated 
library(ggcorrheatmap)
data_no_date <- subset(data_3, select = -Date )
ggcorrhm(data_no_date)
data_no_date <- subset(data_3, select = -Date )
ggcorrhm(data_no_date)
 gghm(data_no_date)
ggcorrhm(data_no_date)
plot(data_3$GDP, data_3$GFCF)
plot(data_3$GDP, data_3$UNEM)
plot(data_3$GDP, data_3$ConsumerPrices)
plot(data_3$GDP, data_3$GovExp)
plot(data_3$GDP, data_3$HouseExp)
plot(data_3$GDP, data_3$HouseExp, main = "Scatter plot of GDP VS houeseExp")
plot(data_3$GDP, data_3$ConsumerPrices, main = "Scatter plot of GDP VS Consumer Prices")
 plot(data_3$GDP, data_3$ConsumerPrices, main = "Scatter plot of GDP VS Consumer Prices")
 plot(data_3$GDP, data_3$GovExp, main = "Scatter plot of GDP VS Govoment Expense")
 plot (data_2$Date,data_2$GDP,type = "l")
plot (data_2$Date,data_2$GDP,type = "l", main = "Line graph of GDP and time")
plot (data_2$Date,data_2$GFCF,type = "l", main = "Line graph of GFCF and time")
plot (data_2$Date,data_2$UNEM,type = "l", main = "Line graph of UNEM and time")
plot (data_2$Date,data_2$ConsumerPrices,type = "l", main = "Line graph of Consumer Prices and time")
plot (data_2$Date,data_2$GovExp,type = "l", main = "Line graph of GovExp and time")
 plot (data_2$Date,data_2$HouseExp,type = "l", main = "Line graph of HouseExp and time")
cor(data_3) model <- lm(data_3$GDP ~ data_3$GFCF +  data_3$UNEM + data_3$ConsumerPrices +  data_3$GovExp + data_3$HouseExp, data = data_3)
summary(model)

Call:
  lm(formula = data_3$GDP ~ data_3$GFCF + data_3$UNEM + data_3$ConsumerPrices + 
       data_3$GovExp + data_3$HouseExp, data = data_3)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-76627 -21057  -1781  21398  60778 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            4.642e+05  4.120e+04  11.266  < 2e-16 ***
#  data_3$GFCF            7.948e-01  7.265e-02  10.940  < 2e-16 ***
#  data_3$UNEM            7.813e+03  1.616e+03   4.835 4.67e-06 ***
#  data_3$ConsumerPrices -4.653e+02  6.761e+02  -0.688 0.492865    
#data_3$GovExp          4.846e-01  1.319e-01   3.673 0.000382 ***
#  data_3$HouseExp        9.702e-01  4.731e-02  20.506  < 2e-16 ***
 # ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 32580 on 103 degrees of freedom
#Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9983 
#F-statistic: 1.25e+04 on 5 and 103 DF,  p-value: < 2.2e-16

model_2 <- lm(data_3$GDP ~ data_3$GFCF +  data_3$UNEM  +  data_3$GovExp + data_3$HouseExp, data = data_3)
model_2 <- lm(data_3$GDP ~ data_3$GFCF +  data_3$UNEM  +  data_3$GovExp + data_3$HouseExp, data = data_3)
summary(model_2)