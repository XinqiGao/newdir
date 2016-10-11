library(ISLR)
library(Hmisc)
library(scatterplot3d)
library(ggplot2)
# library(car)
## Set working directory
#setwd("C:/Users/Aakriti/Desktop/House Prices")
#getwd()

Housing <- read.csv("~/Documents/newdir/R file.csv")
## Read data
#Housing=read.csv(file=file.choose())
data<-Housing[order(Housing$SalePrice),]
# cleaning the data
#changing name of Lot Area column
colnames(data)[colnames(data)=="Lot.Area"]<-"Lot_Area"
# removing the  null values from the data
apply(data,2,max,na.rm=TRUE)
attach(data)
head(data)
View(data)
summary(data)
dim(data)


#to determine the outliers in the the dependent variable house_prices
boxplot(SalePrice,main="Boxplot of House Prices to detect outliers")
#We observe few outliers in the data
#Since there are very few outliers as compared to the number of observations we did not eliminate
#them to maintain the sancitity of the data.

#We break down the above Boxplot of SalePrice by Lot_Area
boxplot(SalePrice~Lot_Area,main="Boxplot of Sale Price vs the Lot area of the house",xlab="Area",ylab="House Sale Price")
#There are no specific outliers and it shows the general trend that the house price is increasing with increase in Square foot area

#We now verify the house sale price with the year the house was built in
boxplot(SalePrice~Year.Built,main="Price vs Year Built",xlab="Year_built",ylab="Price")
# We observe that the Price of the house decreases with increase in the age of the house.

#We now verify the price of the house with respect to quality
boxplot(SalePrice~Overall.Qual,main="Price vs Quality",xlab="Quality",ylabel="Price")
#This graph is a clear indication taht with increase in Quality- Overall material and finish of the house. the hosue price increases

#Null Hypothesis

#Step-wise model
#ccs <-as.matrix(data)
#rcorr(ccs,type="pearson")
model_lot_area <-lm(SalePrice~Lot_Area)
summary(model_lot_area)
#R squared value is 5.9%
model_Overall_Qual <-lm(SalePrice~Overall.Qual)
summary(model_Overall_Qual)
#R squared is 64.73%

model_year_built <-lm(SalePrice~Year.Built)
summary(model_year_built)
#R squared value is 32.69%

model_year_remodelled <-lm(SalePrice~Year.Remod.Add)
summary(model_year_remodelled)
# R squared value is 28.62%

model_lot_area1 <-lm(SalePrice~ Gr.Liv.Area + Garage.Area + Total.Bsmt.SF + Mas.Vnr.Area)
summary(model_lot_area1)
#This increases R squared to 70.43%
#Lot_area is not to be included because it it not evry prominent

model_street <-lm(SalePrice ~ Street)
summary(model_street)
#R square really low

model_rooms <-lm( SalePrice ~ TotRms.AbvGrd+Bedroom.AbvGr)
summary(model_rooms)
#Individually has 25.43% R squared but together 32.24%


model_air_condit <-lm(SalePrice ~ Central.Air)
summary(model_air_condit)
# Not a prominent Factor

#Nominal to be converted to Continuous
data$Bldg.Type.f <- factor(data$Bldg.Type)
is.factor(data$Bldg.Type.f)
data$Bldg.Type.f[1:4]

model_bldg_type <- lm(SalePrice ~ data$Bldg.Type.f)
summary(model_bldg_type)

#Converting Model Shape to continuous
data$Lot.Shape.f <- factor(data$Lot.Shape)
is.factor(data$Lot.Shape.f)
data$Lot.Shape.f[1:2]

model_lot_shape <- lm(SalePrice ~ data$Lot.Shape.f)
summary(model_lot_shape)

#Descriptive Statistics
qplot(data$SalePrice)
qplot(SalePrice, data=data, geom="density", fill=Bldg.Type, alpha=I(.5), 
      main="Distribution of SalePrice by Bldg.Type", xlab="SalePrice", 
      ylab="Density")

# qplot(SalePrice, data=data, geom="density", fill=Overall.Qual, alpha=I(.5), 
#       main="Distribution of SalePrice by Overall.Qual", xlab="SalePrice", 
#       ylab="Density")

qplot(Bldg.Type, SalePrice, data=data, geom=c("boxplot"), 
      fill=Bldg.Type, main="SalePrice by Bldg.Type",
      xlab="", ylab="SalePrice")
ggplot(data, aes(x = Overall.Qual, y = SalePrice, fill = Overall.Qual)) + geom_boxplot() +
  facet_wrap(~ Overall.Qual, ncol = 10)


#3d scatterplot
best <-scatterplot3d(TotRms.AbvGrd+Bedroom.AbvGr,Overall.Qual,SalePrice,pch = 18, main="test",xlab="rooms",ylab="area",zlab = "price")
best$plane3d(model_rooms,col="blueviolet")

#Regression Model
final_model <- lm(SalePrice ~ Year.Built + Overall.Qual + Bldg.Type + Lot.Shape+ Mas.Vnr.Area+Total.Bsmt.SF+Gr.Liv.Area+Bedroom.AbvGr+TotRms.AbvGrd+Garage.Area)
summary(final_model)
# R squared value is 82.4%

#We now check the confidence intervals
#model_conf <- predict(model_Overall_Qual, interval = "confidence")
#plot(Overall.Qual,SalePrice,main="Test interval",xlab="Quality",ylab="Price",col="pink",pch=18)
#lines(Overall.Qual,model_conf[,2],lty=2,lwd=2,col="blueviolet")
#lines(Overall.Qual,model_conf[,3],lty=2,lwd=2,col="blueviolet")

#Verifying Model Assumptions
#1. We verify whether the data meets the normality assumption
qqnorm(residuals(final_model))
qqline(residuals(final_model))
#The residuals appear to be normally distributed . majority fit the line well. The tail being
# off could be because of few outliers

#2 Checking model fit
model.resid <-final_model$residuals
plot(model.resid,main="Residuals",pch=20)
abline(0,0,lwd=2,col="blueviolet")
# Residual is teh difference between the actual and fitted values . Most of the values here are 
#distributed closely around 0 indicating the model is a good fit

#Regression Over

