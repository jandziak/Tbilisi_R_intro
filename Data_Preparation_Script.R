install.packages(c("caret","ggplot2","dplyr","shiny",
                   "leaflet","plotly","networkD3",
                   "knitr"))

library(caret)
library(dplyr)
library(ggplot2)
library(plotly)
data("GermanCredit")

str(GermanCredit)


names(GermanCredit)[1:20]


## Data Preparation cd...

GermanCredit$Sex <- 
  GermanCredit$Personal.Male.Divorced.Seperated +
  GermanCredit$Personal.Male.Single + 
  GermanCredit$Personal.Male.Married.Widowed

GermanCredit$Sex <- ifelse(GermanCredit$Sex, "Male", "Female")

## Data Preparation cd...

GermanCredit$Property <- 
  sapply(1:dim(GermanCredit)[1], function(i)
    if(GermanCredit$Property.RealEstate[i]){
      "RealEstate"
    } else if(GermanCredit$Property.Insurance[i]) {
      "Insurance"
    } else if(GermanCredit$Property.CarOther[i]) {
      "CarOther"
    } else {
      "Unknown"
    })

## Data Preparation cd...

GermanCredit$Purpose <- 
  names(GermanCredit[20:30])[max.col(GermanCredit[20:30])]

## Data Preparation cd... 

GermanCredit <- GermanCredit[, c(1:7, 10, 63:65)]
str(GermanCredit, width = 80, strict.width = "wrap")