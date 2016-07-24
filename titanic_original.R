setwd("C:/Users/vidya/Desktop")
read.csv("titanic_original.csv")
titanic_original <- read.csv("C:/Users/vidya/Desktop/titanic_original.csv")
View(titanic_original)
library(dplyr)
library(tidyr)
titanic_original$embarked[embarked == ""] = "S"
titanic_original$age[is.na(titanic_original$age)] = mean(titanic_original$age,na.rm= TRUE)
titanic_original$boat[titanic_original$boat == ""] <- NA
titanic_original$cabin_number_na <- as.numeric(titanic_original$cabin != "")
View(titanic_original)

