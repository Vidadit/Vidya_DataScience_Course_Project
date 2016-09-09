##R code for the Capstone project :By Vidya R 
##A Capstone project: ##Pricing pattern determination of Organic versus Conventional  Produce(Fruits and vegetables)
##  Vidya Ramasamy
##Under the mentorship: 
##Dr. Marko Mitic (Data Scientist at Telenor, Belgrade, Serbia)
##For the course: 
##Foundations of Data-science(Springboard)
##Data Wrangling:
setwd("C:/Users/vidya/Desktop/datasets")
veg <- read.csv("tmktprice.csv", stringsAsFactors = FALSE)
str(veg)
View(veg)
## Adding Libraries:
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(pastecs)
#6#DESCRIPTIVE ANALYSIS
veg_num <- veg[,c(6:29)]
sapply(veg_num, mean, na.rm = TRUE)
summary(veg_num)
#install.packages("pastecs")
#library(pastecs)
stat.desc(veg_num) 
##===========================================================================================
##SIMPLE BOX PLOT :
##This simple Box plot reflect the average price pattern; 
##The average median price is higher at Atlanta than San Francisco.
##The scattered points depict the prices of various commodities (vegetables and fruits)
p <- ggplot(veg, aes(factor(Terminal_.Market), Mean.price2012))
p + geom_boxplot()
p + geom_boxplot() + geom_jitter()
##===========================================================================================
#6#DESCRIPTIVE ANALYSIS
lapply(veg_num, mean, na.rm = TRUE)
boxplot(veg$December_2013)
##===========================================================================================
#7# EXPLORATORY ANALYSIS
dim(veg)
nrow(veg)
ncol(veg)
head(veg)
tail(veg)
names(veg)
str(veg)
levels(veg)
## colorful scatterplot:Grouping Variables: median prices of 2013 and 2012 categorised by Commodity:
ggplot(veg, aes( x = Medianprice_2012, y = Medianprice_2013, col= factor(Commodity))) + geom_point() + stat_smooth(method = "lm", se = FALSE)
##=============================================================================================
#Graphical representation of packaging of produce:
icx <- agrep(pattern = "cartons", x= veg$Package,ignore.case = FALSE, value = FALSE, max.distance = 3)
veg$Package[icx] <- "cartons"
ifx <- agrep(pattern = "flats", x= veg$Package,ignore.case = FALSE, value = FALSE, max.distance = 3)
veg$Package[ifx] <- "flats" 
veg$Package
barplot(table(veg$Package), col= c("blue","green"), main = "Packaging types for  wholesale Produce")
##barplot(table(veg$Commodity,veg$Package), col= c("blue","green"), main = "Packaged format for Produce")
##==============================================================================================
##PREDICTIVE ANALYSIS 
##linear Regression model 
Jan_month_lm <- lm(January_2012 ~ January_2013, data = veg)
summary(Jan_month_lm)
#model plot
par(mar = c(4,4,2,2), mfrow = c(1,2))
plot(Jan_month_lm) 
## linear regression Price differentiation
##Atlanta:
Org_ATL <- filter(veg, Terminal_.Market== "Atlanta" & Category== "Org") 
View(Org_ATL)
##LM 
ggplot(Org_ATL,aes(Mean.price2012,Mean.price2013)) + geom_smooth(method='lm')
#SAN FRANCISCO
Org_SFO <- filter(veg, Terminal_.Market== "San Francisco" & Category== "Org")
ggplot(Org_SFO,aes(Mean.price2012,Mean.price2013)) + geom_smooth(method='lm')
#####linear regression model for median price range :graphical representation:
ggplot(veg, aes( x = Medianprice_2012 , y = Medianprice_2013)) + geom_point() + stat_smooth(method = "lm", se = FALSE) 
####======================================================================
##Anova 
Jan_lm <- lm(formula = January_2012 ~ January_2013, data = veg)
summary(Jan_lm)
#model plot
par(mar = c(4,4,2,2), mfrow = c(1,2))
plot(Jan_lm)
###----------------------------------------------------------
#JAN- FEB- MARCH 2013 :Annova test 
Q1_analysis_2013 <- veg[,c(4,7,9,11)]
fit_jf_2013 <- aov(febuary_2013 ~ January_2013, data= Q1_analysis_2013)
fit_fm_2013 <- aov( febuary_2013 ~ March_2013, data= Q1_analysis_2013)
anova(fit_jf_2013, fit_fm_2013)
hist(residuals(fit_jf_2013))
hist(residuals(fit_fm_2013))
ggplot(Q1_analysis_2013, aes( x = febuary_2013 , y = March_2013, col= Category)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
##--------------------------------------------
#APRIL- MAY- JUNE 2013 :Annova test
Q2_analysis_2013 <- veg[,c(4,13,15,17)]
fit_am_2013 <- aov(May_2013 ~ April_2013, data= Q2_analysis_2013)
fit_mj_2013 <- aov( May_2013 ~ June_2013, data= Q2_analysis_2013)
anova(fit_am_2013, fit_mj_2013)
hist(residuals(fit_am_2013))
hist(residuals(fit_mj_2013))
ggplot(Q2_analysis_2013, aes( x = May_2013 , y = June_2013, col= Category)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
##-----------------------------------------------
## JULY- AUG- SEPT 2013 :Annova test
Q3_analysis_2013 <- veg[,c(4,19,21,23)]
fit_ja_2013 <- aov(August_2013 ~ July_2013, data= Q3_analysis_2013)
fit_as_2013 <- aov(August_2013 ~ September_2013, data= Q3_analysis_2013)
anova(fit_ja_2013, fit_as_2013)
hist(residuals(fit_ja_2013))
hist(residuals(fit_as_2013))
ggplot(Q3_analysis_2013, aes( x = August_2013, y = September_2013, col= Category)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
##----------------------------------------------------
# OCT- NOV- DEC 2013 :Annova test
Q4_analysis_2013 <- veg[,c(4,25,27,29)]
fit_on_2013 <- aov(November_2013 ~ October_2013, data= Q4_analysis_2013)
fit_nd_2013 <- aov(November_2013 ~ December_2013, data= Q4_analysis_2013)
anova(fit_on_2013, fit_nd_2013)
hist(residuals(fit_on_2013))
hist(residuals(fit_nd_2013))
ggplot(Q4_analysis_2013, aes( x = November_2013, y = October_2013, col= Category)) + geom_point() + stat_smooth(method = "lm", se = FALSE) 
###==========================================================================
##K MEAN CLUSTERING:
dat = veg[c(30,31)]
boxplot(dat, main = "% of favourable pattern in the 2yr Median prices", pch =20, cex =2)
set.seed(33)
km1 = kmeans(dat, 3, nstart=100)
plot(dat, col =(km1$cluster +1) , main="K-Means result with 3 clusters", pch=20, cex=2)
##============================================================================
##LOCATION ANALYSIS
##2) Predictive analysis  of mean prices in Locations:
##plot for meanconventional prices
ggplot(con_mean_df, aes( x = Medianprice_2012, y = Medianprice_2013, col= Terminal_.Market),  colors(distinct = FALSE) ) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(con_mean_df, aes( x = Mean.price2012, y = Mean.price2013, col= Category),  colors(distinct = FALSE) ) + geom_point() + stat_smooth(method = "lm", se = FALSE)
location_analysis <-  veg[,c(5,32,33)]
filter(location_analysis, Terminal_.Market = "San Francisco")
filter(location_analysis, Terminal_.Market == "San Francisco")
location_sfo <- filter(location_analysis, Terminal_.Market == "San Francisco")
location_atl <- filter(location_analysis, Terminal_.Market == "Atlanta")
ggplot(location_sfo, aes( x = Mean.price2012, y = Mean.price2013, col= Terminal_.Market),  colors(distinct = TRUE) ) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(location_sfo, aes( x = Mean.price2012, y = Mean.price2013, col= Terminal_.Market),  colors(distinct = TRUE) ) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(location_atl, aes( x = Mean.price2012, y = Mean.price2013, col= Terminal_.Market),  colors(distinct = FALSE) ) + geom_point() + stat_smooth(method = "lm", se = TRUE)
ggplot(location_sfo, aes( x = Mean.price2012, y = Mean.price2013, col= Terminal_.Market),  colors(distinct = FALSE) ) + geom_point() + stat_smooth(method = "lm", se = TRUE)
##======================================================================================================
##BOX PLOTS:
location_plot1 <- ggplot(location_sfo, aes(x= Mean.price2012 , Mean.price2013))
location_plot1 + geom_boxplot(aes(fill = factor(Terminal_.Market)))
location_plot2 <- ggplot(location_atl, aes(x= Mean.price2012 , Mean.price2013))
location_plot2 + geom_boxplot(aes(fill = factor(Terminal_.Market)))
##========================================================================================
#3)consumers can be selective on the seasonal availability/Month-wise pricing)
##  example: what the restaurant will decide on the type of produce to buy according to the price pattern (month-wise)
commodity_wise <- veg[,c(1,7,9,11,13,15,17,19,21,23,25,27,29)]
ggplot(commodity_wise, aes(x = January_2013, y = febuary_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE) 
ggplot(commodity_wise, aes(x = febuary_2013, y = March_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE) 
ggplot(commodity_wise, aes(x = March_2013, y = April_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(commodity_wise, aes(x = April_2013, y = May_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(commodity_wise, aes(x = May_2013, y = June_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(commodity_wise, aes(x = June_2013, y = July_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(commodity_wise, aes(x = July_2013, y = August_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(commodity_wise, aes(x = August_2013, y = September_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(commodity_wise, aes(x = September_2013, y = October_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(commodity_wise, aes(x = October_2013, y = November_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
ggplot(commodity_wise, aes(x = November_2013, y = December_2013, col= Commodity)) + geom_point() + stat_smooth(method = "lm", se = FALSE)
##=========================================================================================
        
        