## loading data

house_data <- read.csv("train.csv")

## Getting glimpse of data
head(house_data)
str(house_data)


## understanding data
## changing MSSubclass to factor

house_data$MSSubClass <- as.factor(house_data$MSSubClass)


#  replace NA in Alley column with no alleys
sum(is.na(house_data$Alley))
table(house_data$Alley)

house_data$Alley <- ifelse(is.na(house_data$Alley), "No Alleys", house_data$Alley)
str(house_data$Alley)

house_data$Alley <- as.factor(house_data$Alley)

levels(house_data$Alley) <- c("Grvl", "Pave", "No Alleys" )



## Replacing Na in BsmtQual with "No basmt"


house_data$BsmtQual <- as.character(house_data$BsmtQual)

house_data$BsmtQual <- ifelse(is.na(house_data$BsmtQual), "No basmt", house_data$BsmtQual)
str(house_data$BsmtQual)

house_data$BsmtQual <- as.factor(house_data$BsmtQual)

## Replacing Na in BsmtCond with "No basmt"

house_data$BsmtCond <- as.character(house_data$BsmtCond)

house_data$BsmtCond <- ifelse(is.na(house_data$BsmtCond), "No basmt", house_data$BsmtCond)
str(house_data$BsmtCond)

house_data$BsmtCond <- as.factor(house_data$BsmtCond)


## Replacing Na in BsmtExposure with "No basmt"

house_data$BsmtExposure <- as.character(house_data$BsmtExposure)

house_data$BsmtExposure <- ifelse(is.na(house_data$BsmtExposure), "No basmt", house_data$BsmtExposure)
str(house_data$BsmtExposure)

house_data$BsmtExposure <- as.factor(house_data$BsmtExposure)




## Replacing Na in BsmtFinType1 with "No basmt"

house_data$BsmtFinType1 <- as.character(house_data$BsmtFinType1)

house_data$BsmtFinType1 <- ifelse(is.na(house_data$BsmtFinType1), "No basmt", house_data$BsmtFinType1)
str(house_data$BsmtFinType1)

house_data$BsmtFinType1 <- as.factor(house_data$BsmtFinType1)



## Replacing Na in BsmtFinType2 with "No basmt"

house_data$BsmtFinType2 <- as.character(house_data$BsmtFinType2)

house_data$BsmtFinType2 <- ifelse(is.na(house_data$BsmtFinType2), "No basmt", house_data$BsmtFinType2)
str(house_data$BsmtFinType2)

house_data$BsmtFinType2 <- as.factor(house_data$BsmtFinType2)

## Replacing Na in FireplaceQu with "No Fireplace"

house_data$FireplaceQu <- as.character(house_data$FireplaceQu)

house_data$FireplaceQu <- ifelse(is.na(house_data$FireplaceQu), "No Fireplace", house_data$FireplaceQu)
str(house_data$FireplaceQu)

house_data$FireplaceQu <- as.factor(house_data$FireplaceQu)


## Replacing Na in GarageType, GarageQual,GarageYrBlt, GarageCond and GarageFinish with "No Garage"

house_data$GarageType <- as.character(house_data$GarageType)

house_data$GarageType <- ifelse(is.na(house_data$GarageType), "No Garage", house_data$GarageType)
str(house_data$GarageType)

house_data$GarageType <- as.factor(house_data$GarageType)



house_data$GarageFinish <- as.character(house_data$GarageFinish)

house_data$GarageFinish <- ifelse(is.na(house_data$GarageFinish), "No Garage", house_data$GarageFinish)
str(house_data$GarageFinish)

house_data$GarageFinish <- as.factor(house_data$GarageFinish)




house_data$GarageYrBlt <- as.character(house_data$GarageYrBlt)

house_data$GarageYrBlt <- ifelse(is.na(house_data$GarageYrBlt), "No Garage", house_data$GarageYrBlt)
str(house_data$GarageYrBlt)

house_data$GarageYrBlt <- as.factor(house_data$GarageYrBlt)



house_data$GarageQual <- as.character(house_data$GarageQual)

house_data$GarageQual <- ifelse(is.na(house_data$GarageQual), "No Garage", house_data$GarageQual)
str(house_data$GarageQual)

house_data$GarageQual <- as.factor(house_data$GarageQual)



house_data$GarageCond <- as.character(house_data$GarageCond)

house_data$GarageCond <- ifelse(is.na(house_data$GarageCond), "No Garage", house_data$GarageCond)
str(house_data$GarageCond)

house_data$GarageCond <- as.factor(house_data$GarageCond)





## Replacing Na in PoolQC with "No Pool"




house_data$PoolQC <- as.character(house_data$PoolQC)

house_data$PoolQC <- ifelse(is.na(house_data$PoolQC), "No Pool", house_data$PoolQC)
str(house_data$PoolQC)

house_data$PoolQC <- as.factor(house_data$PoolQC)


## Replacing Na in Fence with "No Fence"




house_data$Fence <- as.character(house_data$Fence)

house_data$Fence <- ifelse(is.na(house_data$Fence), "No Fence", house_data$Fence)
str(house_data$Fence)

house_data$Fence <- as.factor(house_data$Fence)



## Replacing Na in MiscFeature with "None"




house_data$MiscFeature <- as.character(house_data$MiscFeature)

house_data$MiscFeature <- ifelse(is.na(house_data$MiscFeature), "None", house_data$MiscFeature)
str(house_data$MiscFeature)

house_data$MiscFeature <- as.factor(house_data$MiscFeature)





## checking Nas in data set now

library(e1071)
sapply(house_data, function(x) {sum(is.na(x))})


## checking column LotFrontage as it has 257 NAs

##skewness 2.15 so replacinf NAs with median

house_data$LotFrontage <-ifelse(is.na(house_data$LotFrontage), median(house_data$LotFrontage, na.rm = T), house_data$LotFrontage)

## 8 rows have Nas for MasVnrType so removing them

library(dplyr)

house_data<-house_data %>% filter(!is.na(MasVnrType))

## removing one observation related  Electrical with NA value
house_data<-house_data %>% filter(!is.na(Electrical))


## checking data set
str(house_data)


## Exploring categorical features



## changing level names for better visualization and exploration

levels(house_data$MSZoning)<- c("Commercial", "FloatingVill", "Res. high density", 
                                  "Res. low density", "Res. med density")



levels(house_data$LotShape)<- c("Irregular(slight)", "Irregular(Moder)","Irregular","Regular")
levels(house_data$LandContour)<- c("Banked", "HillSide", "Low", "Level")
levels(house_data$Utilities)<- c("All", "No sewarage")
levels(house_data$LotConfig)[3:4]<- c("Frontage(2 sides)","Frontage(3 sides)" )

levels(house_data$LandSlope)<- c("Gentle", "Moderate","Severe")

levels(house_data$ExterQual )<- c("Excellent", "Fair","Good","Typical")

levels(house_data$ExterCond)<- c("Excellent","Fair","Good","Poor","Typical")

levels(house_data$BsmtQual )<- c("Excellent","Fair","Good","No basmt","Typical")

levels(house_data$BsmtCond )<- c("Fair","Good","No bsmt","Poor","Typical")


levels(house_data$BsmtExposure)<- c("Average","Good","Minimum","No","No basmt")

levels(house_data$BsmtFinType1)<- c("Avg. LQ","Below avg. LQ", "Good LQ","Low Quality","No basmnt","Avg. RecRoom","Unfinished" )

levels(house_data$BsmtFinType2)<-c("Avg. LQ","Below avg. LQ", "Good LQ","Low Quality","No basmnt","Avg. RecRoom","Unfinished" )


levels(house_data$HeatingQC)<- c("Excellent","Fair","Good","Poor","Typical")


levels(house_data$KitchenQual)<-  c("Excellent","Fair","Good","Typical")


levels(house_data$Functional)<- c("MajorDeductn1","MajorDeductn2","MinorDeduc1","MinorDeduc","Moder.Deduction","SeverelyDamaged","Typical")


levels(house_data$FireplaceQu)<- c("Excellent","Fair","Good","No Fireplace","Poor","Typical")


levels(house_data$GarageFinish)<- c("Finished","No Garage","RoughFinished","Unfinished")

levels(house_data$GarageQual)<- c("Excellent","Fair","Good","No Garage","Poor","Typical")


levels(house_data$GarageCond)<- c("Excellent","Fair","Good","No Garage","Poor","Typical")


levels(house_data$PavedDrive) <- c("No","Partial","Yes")


levels(house_data$PoolQC)<- c("Excellent","Fair","Good","No Pool")


levels(house_data$Fence) <- c("GoodPrivacy","GoodWood","MinimumPrivacy","MinimumWood","NoFence")






## converting columns to factors

house_data$YrSold <- as.factor(house_data$YrSold)

house_data$MoSold <- as.factor(house_data$MoSold)

##house_data$GarageCars <- as.factor(house_data$GarageCars)

##house_data$GarageCars <- as.character(house_data$GarageCars)


## combine categories for 3 or more garagecars

for ( i in 1:length(house_data$GarageCars)){

  if((house_data$GarageCars[i]) >=3)
  {
    
    
    house_data$GarageCars[i] = "3 or more"
    
  }
}


## converting to factor

house_data$GarageCars <- as.factor(house_data$GarageCars)





##house_data$Fireplaces <- as.factor( house_data$Fireplaces)

##house_data$Fireplaces <- as.character(house_data$Fireplaces)


for ( i in 1:length(house_data$Fireplaces)){
  
  if((house_data$Fireplaces[i]) >=2)
  {
    
    
    house_data$Fireplaces[i] = "2 or more"
    
  }
}


house_data$Fireplaces <- as.factor( house_data$Fireplaces)

## combine categories for TotRmsAbvGrd 

for ( i in 1:length(house_data$TotRmsAbvGrd)){
  
  

  
  if((house_data$TotRmsAbvGrd[i]) == 10|(house_data$TotRmsAbvGrd[i]) == 11|(house_data$TotRmsAbvGrd[i]) == 12|
     (house_data$TotRmsAbvGrd[i]) == 14)
  {
    
    
    house_data$TotRmsAbvGrd[i] = "10 or more"
    
  }
  
  if((house_data$TotRmsAbvGrd[i]) == 2|(house_data$TotRmsAbvGrd[i]) == 3| (house_data$TotRmsAbvGrd[i]) == 4)
  {
    
    
    house_data$TotRmsAbvGrd[i] = "4 or less"
    
  }
}

house_data$TotRmsAbvGrd <- as.factor(house_data$TotRmsAbvGrd)



##kitchenabvgr combinging categores
table(house_data$KitchenAbvGr)
for ( i in 1:length(house_data$KitchenAbvGr)){
  
  if((house_data$KitchenAbvGr[i]) == 0|(house_data$KitchenAbvGr[i]) == 1)
  {
    
    
    house_data$KitchenAbvGr[i] = "1 or less"
    
  }
  
  if((house_data$KitchenAbvGr[i]) == 2|(house_data$KitchenAbvGr[i]) == 3)  {
    
    
    house_data$KitchenAbvGr[i] = "2 or more"
    
  }
}


house_data$KitchenAbvGr <- as.factor(house_data$KitchenAbvGr)


## combining categories for BedroomAbvGr

table(house_data$BedroomAbvGr)


for ( i in 1:length(house_data$BedroomAbvGr)){
  
  if((house_data$BedroomAbvGr[i]) <=2)
  {
    
    
    house_data$BedroomAbvGr[i] = "2 or less"
    
  }
  
  if((house_data$BedroomAbvGr[i]) >= 4)  {
    
    
    house_data$BedroomAbvGr[i] = "4 or more"
    
  }
}


house_data$BedroomAbvGr <- as.factor(house_data$BedroomAbvGr)



##combining HalfBath categories and converting to factor

table(house_data$HalfBath)

for ( i in 1:length(house_data$HalfBath)){
  
  if((house_data$HalfBath[i]) >=1)
  {
    
    house_data$HalfBath[i] = "1 or more"
    
  }
}

house_data$HalfBath <- as.factor(house_data$HalfBath)

## combining categoreis of fullbath and converting to factor

table(house_data$FullBath)

for ( i in 1:length(house_data$FullBath)){
  
  if((house_data$FullBath[i]) <=1)
  {
    
    house_data$FullBath[i] = "1 or less"
    
  }
  if((house_data$FullBath[i]) >= 2)
  {
    
    house_data$FullBath[i] = "2 or more"
    
  }
}

house_data$FullBath <- as.factor(house_data$FullBath)

## combining categories and converting to factor for bsmthalfbath



table(house_data$BsmtHalfBath)

for ( i in 1:length(house_data$BsmtHalfBath)){
  
  if((house_data$BsmtHalfBath[i]) >=1)
  {
    
    house_data$BsmtHalfBath[i] = "1 or more"
    
  }
}
house_data$BsmtHalfBath <- as.factor(house_data$BsmtHalfBath)

## combining categories and converting to factor for bsmtfullbah



table(house_data$BsmtFullBath)

for ( i in 1:length(house_data$BsmtFullBath)){
  
  if((house_data$BsmtFullBath[i]) >=1)
  {
    
    house_data$BsmtFullBath[i] = "1 or more"
    
  }
}

house_data$BsmtFullBath <- as.factor(house_data$BsmtFullBath)


##combining categories and converting to factor for yearremodadd



table(house_data$YearRemodAdd)

for ( i in 1:length(house_data$YearRemodAdd)){
  
  if(((house_data$YearRemodAdd[i]) >=1950 && (house_data$YearRemodAdd[i]) <= 1955)  )
  {
    
    house_data$YearRemodAdd[i] = "1950-1955"
    
  }
  
  if(((house_data$YearRemodAdd[i]) >=1956 && (house_data$YearRemodAdd[i]) <= 1960) )
  {
    
    house_data$YearRemodAdd[i] = "1956-1960"
    
  }
  
  if(((house_data$YearRemodAdd[i]) >=1961 && (house_data$YearRemodAdd[i]) <= 1965)  )
  {
    
    house_data$YearRemodAdd[i] = "1961-1965"
    
  }
  
  
  if(((house_data$YearRemodAdd[i]) >=1966 && (house_data$YearRemodAdd[i]) <= 1970 ))
  {
    
    house_data$YearRemodAdd[i] = "1966-1970"
    
  }
  
  
  if(((house_data$YearRemodAdd[i]) >=1971 && (house_data$YearRemodAdd[i]) <= 1975)  )
  {
    
    house_data$YearRemodAdd[i] = "1971-1975"
    
  }
  
  if(((house_data$YearRemodAdd[i]) >=1976 && (house_data$YearRemodAdd[i]) <= 1980) )
  {
    
    house_data$YearRemodAdd[i] = "1976-1980"
    
  }
  
  
  if(((house_data$YearRemodAdd[i]) >=1981 && (house_data$YearRemodAdd[i]) <= 1985)  )
  {
    
    house_data$YearRemodAdd[i] = "1981-1985"
    
  }
  
  if(((house_data$YearRemodAdd[i]) >=1986 && (house_data$YearRemodAdd[i]) <= 1990)  )
  {
    
    house_data$YearRemodAdd[i] = "1986-1990"
    
  }
  
  if(((house_data$YearRemodAdd[i]) >=1991 && (house_data$YearRemodAdd[i]) <= 1995)  )
  {
    
    house_data$YearRemodAdd[i] = "1991-1995"
    
  }
  
  if(((house_data$YearRemodAdd[i]) >=1996 && (house_data$YearRemodAdd[i]) <= 2000)  )
  {
    
    house_data$YearRemodAdd[i] = "1996-2000"
    
  }
  
  if(((house_data$YearRemodAdd[i]) >=2001 &&  (house_data$YearRemodAdd[i]) <= 2005)  )
  {
    
    house_data$YearRemodAdd[i] = "2001-2005"
    
  }
  
  if(((house_data$YearRemodAdd[i]) >=2006 &&  (house_data$YearRemodAdd[i]) <= 2010 ) )
  {
    
    house_data$YearRemodAdd[i] = "2006-2010"
    
  }
  

}



house_data$YearRemodAdd <- as.factor(house_data$YearRemodAdd)

##combining categories and converting to factor for YearBuilt



table(house_data$YearBuilt)

for ( i in 1:length(house_data$YearBuilt)){
  
  if(((house_data$YearBuilt[i]) >=1871 && (house_data$YearBuilt[i]) <= 1900)  )
  {
    
    house_data$YearBuilt[i] = "Before 1900"
    
  }
  
  if(((house_data$YearBuilt[i]) >=1901 && (house_data$YearBuilt[i]) <= 1925) )
  {
    
    house_data$YearBuilt[i] = "1901-1925"
    
  }
  
  if(((house_data$YearBuilt[i]) >=1926 && (house_data$YearBuilt[i]) <= 1950)  )
  {
    
    house_data$YearBuilt[i] = "1926-1950"
    
  }
  
  
  if(((house_data$YearBuilt[i]) >=1951 && (house_data$YearBuilt[i]) <= 1975 ))
  {
    
    house_data$YearBuilt[i] = "1951-1975"
    
  }
  
  
  if(((house_data$YearBuilt[i]) >=1976 && (house_data$YearBuilt[i]) <= 2000)  )
  {
    
    house_data$YearBuilt[i] = "1976-2000"
    
  }
  
  if(((house_data$YearBuilt[i]) >=2001 && (house_data$YearBuilt[i]) <= 2005) )
  {
    
    house_data$YearBuilt[i] = "2001-2005"
    
  }
  

  
  if(((house_data$YearBuilt[i]) >=2006 &&  (house_data$YearBuilt[i]) <= 2010 ) )
  {
    
    house_data$YearBuilt[i] = "2006-2010"
    
  }
  
  
}


house_data$YearBuilt <- as.factor( house_data$YearBuilt)








##combining categories and converting to factor for OverallCond

table(house_data$OverallCond)


for ( i in 1:length(house_data$OverallCond)){
  
  if((house_data$OverallCond[i]) <= 4)
  {
    
    house_data$OverallCond[i] = "4 or less"
    
  }
  
  
  if((house_data$OverallCond[i]) >= 8)
  {
    
    house_data$OverallCond[i] = "8 or more"
    
  }
}

house_data$OverallCond <- as.factor(house_data$OverallCond)



##combining categories and converting to factor for OverallQual

table(house_data$OverallQual)


for ( i in 1:length(house_data$OverallQual)){
  
  if((house_data$OverallQual[i]) <= 4)
  {
    
    house_data$OverallQual[i] = "4 or less"
    
  }
  
  
  if((house_data$OverallQual[i]) >= 8)
  {
    
    house_data$OverallQual[i] = "8 or more"
    
  }
}

house_data$OverallQual <- as.factor(house_data$OverallQual)


scatterplots(house_data, "GarageCars", "SalePrice")



##house_data$MiscFeature major values are none does not seem to be imp for prediction based on plot


## combining categories of poolQC

table(house_data$PoolQC)

house_data$PoolQC <- as.character(house_data$PoolQC)


for ( i in 1:length(house_data$PoolQC)){
  
  if((house_data$PoolQC[i] == "Fair") || (house_data$PoolQC[i] == "Good"))
  {
    
    house_data$PoolQC[i] = "Fair/Good"
    
  }
  
}


house_data$PoolQC <- as.factor(house_data$PoolQC)






## combining categories of GarageCond

table(house_data$GarageCond)

house_data$GarageCond <- as.character(house_data$GarageCond)


for ( i in 1:length(house_data$GarageCond)){
  
  if((house_data$GarageCond[i] == "Fair") || (house_data$GarageCond[i] == "Poor"))
  {
    
    house_data$GarageCond[i] = "Fair/Poor"
    
  }
  
  if((house_data$GarageCond[i] == "Good") || (house_data$GarageCond[i] == "Typical"))
  {
    
    house_data$GarageCond[i] = "Good/Typical"
    
  }
  
}


house_data$GarageCond <- as.factor(house_data$GarageCond)




## combining categories of GarageQual

table(house_data$GarageQual)

house_data$GarageQual <- as.character(house_data$GarageQual)


for ( i in 1:length(house_data$GarageQual)){
  
  if((house_data$GarageQual[i] == "Fair") || (house_data$GarageQual[i] == "Poor"))
  {
    
    house_data$GarageQual[i] = "Fair/Poor"
    
  }
  
  if((house_data$GarageQual[i] == "Good") || (house_data$GarageQual[i] == "Typical"))
  {
    
    house_data$GarageQual[i] = "Good/Typical"
    
  }
  
}


house_data$GarageQual <- as.factor(house_data$GarageQual)


## combining categories of Fence

table(house_data$Fence)

house_data$Fence <- as.character(house_data$Fence)


for ( i in 1:length(house_data$Fence)){
  
  if((house_data$Fence[i] == "GoodWood") || (house_data$Fence[i] == "MinimumWood"))
  {
    
    house_data$Fence[i] = "Good/MinimumWood"
    
  }
  
 
}


house_data$Fence <- as.factor(house_data$Fence)


## combining categories of Functional column

table(house_data$Functional)

house_data$Functional <- as.character(house_data$Functional)


for ( i in 1:length(house_data$Functional)){
  
  if((house_data$Functional[i] == "MajorDeductn1") || (house_data$Functional[i] == "MajorDeductn2"))
  {
    
    house_data$Functional[i] = "MajorDeductn"
    
  }
  
  
  
  if((house_data$Functional[i] == "MinorDeduc1") || (house_data$Functional[i] == "MinorDeduc"))
  {
    
    house_data$Functional[i] = "MinorDeductn"
    
  }
  
  
}


house_data$Functional <- as.factor(house_data$Functional)
## combine categories of Electrical


table(house_data$Electrical)



house_data$Electrical <- as.character(house_data$Electrical)



for ( i in 1:length(house_data$Electrical)){
  
  if((house_data$Electrical[i] == "FuseP") || (house_data$Electrical[i] == "Mix")||
     (house_data$Electrical[i] == "FuseF"))
  {
    
    house_data$Electrical[i] = "FuseP/F/Mix"
    
  }
  

}


house_data$Electrical <- factor(house_data$Electrical)


## combine categories of HeatingQC


table(house_data$HeatingQC)



house_data$HeatingQC <- as.character(house_data$HeatingQC)



for ( i in 1:length(house_data$HeatingQC)){
  
  if((house_data$HeatingQC[i] == "Fair") || (house_data$HeatingQC[i] == "Poor")||
     (house_data$HeatingQC[i] == "Typical"))
  {
    
    house_data$HeatingQC[i] = "Fair/Poor/Typical"
    
  }
  
  
}


house_data$HeatingQC <- factor(house_data$HeatingQC)





## combine categories of Heating


table(house_data$Heating)



house_data$Heating <- as.character(house_data$Heating)



for ( i in 1:length(house_data$Heating)){
  
  if((house_data$Heating[i] == "Floor") || (house_data$Heating[i] == "Grav")||
     (house_data$Heating[i] == "OthW")|| (house_data$Heating[i] == "Wall"))
  {
    
    house_data$Heating[i] = "Others"
    
  }
  
  
}


house_data$Heating <- factor(house_data$Heating)






## combine categories of BsmtFinType1 and BsmtFinType2


table(house_data$BsmtFinType1)



house_data$BsmtFinType1 <- as.character(house_data$BsmtFinType1)



for ( i in 1:length(house_data$BsmtFinType1)){
  
  if((house_data$BsmtFinType1[i] == "Unfinished") || (house_data$BsmtFinType1[i] == "Avg. RecRoom")||
     (house_data$BsmtFinType1[i] == "Low Quality"))
  {
    
    house_data$BsmtFinType1[i] = "LowQuality/Unfinished"
    
  }
  
  
}


house_data$BsmtFinType1 <- factor(house_data$BsmtFinType1)






table(house_data$BsmtFinType2)



house_data$BsmtFinType2 <- as.character(house_data$BsmtFinType2)



for ( i in 1:length(house_data$BsmtFinType2)){
  
  if((house_data$BsmtFinType2[i] == "Unfinished") || (house_data$BsmtFinType2[i] == "Avg. RecRoom")||
     (house_data$BsmtFinType2[i] == "Low Quality"))
  {
    
    house_data$BsmtFinType2[i] = "LowQuality/Unfinished"
    
  }
  
  if((house_data$BsmtFinType2[i] == "Avg. LQ") || (house_data$BsmtFinType2[i] == "Below avg. LQ")||
     (house_data$BsmtFinType2[i] == "Good LQ"))
  {
    
    house_data$BsmtFinType2[i] = "Good/Avg/BAvg LQ"
    
  }
  
  
}


house_data$BsmtFinType2 <- factor(house_data$BsmtFinType2)


## combining categories for house_data$BsmtQual

table(house_data$BsmtQual)

house_data$BsmtQual <- as.character(house_data$BsmtQual)



for ( i in 1:length(house_data$BsmtQual)){
  
  if((house_data$BsmtQual[i] == "Fair") || (house_data$BsmtQual[i] == "Typical"))
  {
    
    house_data$BsmtQual[i] = "Fair/Typical"
    
  }
}
house_data$BsmtQual <- as.factor(house_data$BsmtQual)





## combining categories for house_data$BsmtCond

table(house_data$BsmtCond)

house_data$BsmtCond <- as.character(house_data$BsmtCond)



for ( i in 1:length(house_data$BsmtCond)){
  
  if((house_data$BsmtCond[i] == "Poor") || (house_data$BsmtCond[i] == "Fair"))
  {
    
    house_data$BsmtCond[i] = "Fair/Poor"
    
  }
}
house_data$BsmtCond <- as.factor(house_data$BsmtCond)




# combining categories for house_data$Foundation

table(house_data$Foundation)

house_data$Foundation <- as.character(house_data$Foundation)



for ( i in 1:length(house_data$Foundation)){
  
  if((house_data$Foundation[i] == "Slab") || (house_data$Foundation[i] == "Stone")||
    ( house_data$Foundation[i]=="Wood"))
  {
    
    house_data$Foundation[i] = "Other"
    
  }
}
house_data$Foundation <- as.factor(house_data$Foundation)






# combining categories for house_data$ExterQual

table(house_data$ExterQual)

house_data$ExterQual <- as.character(house_data$ExterQual)



for ( i in 1:length(house_data$ExterQual)){
  
  if((house_data$ExterQual[i] == "Fair") || (house_data$ExterQual[i] == "Typical"))
  {
    
    house_data$ExterQual[i] = "Fair/Typical"
    
  }
}
house_data$ExterQual <- as.factor(house_data$ExterQual)





# combining categories for house_data$ExterCond

table(house_data$ExterCond)

house_data$ExterCond <- as.character(house_data$ExterCond)



for ( i in 1:length(house_data$ExterCond)){
  
  if((house_data$ExterCond[i] == "Fair") || (house_data$ExterCond[i] == "Typical")
     ||(house_data$ExterCond[i] == "Poor"))
  {
    
    house_data$ExterCond[i] = "Fair/Typical/Poor"
    
    
    
  }
  
  
  
}
house_data$ExterCond <- as.factor(house_data$ExterCond)



### combining categories for house_data$RoofMatl

table(house_data$RoofMatl)

house_data$RoofMatl <- as.character(house_data$RoofMatl)



for ( i in 1:length(house_data$RoofMatl)){
  
  if((house_data$RoofMatl[i] == "ClyTile") || (house_data$RoofMatl[i] == "Membran")
     ||(house_data$RoofMatl[i] == "Metal")||(house_data$RoofMatl[i] == "Roll")
     ||(house_data$RoofMatl[i] == "Tar&Grv")||(house_data$RoofMatl[i] == "WdShake")
     ||(house_data$RoofMatl[i] == "WdShngl"))
  {
    
    house_data$RoofMatl[i] = "Other"
    
    
    
  }
  
  
  
}
house_data$RoofMatl <- as.factor(house_data$RoofMatl)



###### combining categories for house_data$RoofStyle

table(house_data$RoofStyle)

house_data$RoofStyle <- as.character(house_data$RoofStyle)



for ( i in 1:length(house_data$RoofStyle)){
  
  if((house_data$RoofStyle[i] == "Flat") || (house_data$RoofStyle[i] == "Gambrel")
     ||(house_data$RoofStyle[i] == "Mansard")||(house_data$RoofStyle[i] == "Shed"))
  {
    
    house_data$RoofStyle[i] = "Other"
    
    
    
  }
  
  
  
}
house_data$RoofStyle <- as.factor(house_data$RoofStyle)
 
## changing house style as per test data labels

house_data$HouseStyle <- as.character(house_data$HouseStyle)



for ( i in 1:length(house_data$HouseStyle)){
  
  if(house_data$HouseStyle[i] == "2.5Fin")
  {
    
    house_data$HouseStyle[i] = "2Story"
    
    
    
  }
  
  
  
}
house_data$HouseStyle <- as.factor(house_data$HouseStyle)


##
## changing Exterior1st as per test data labels

house_data$Exterior1st <- as.character(house_data$Exterior1st)



for ( i in 1:length(house_data$Exterior1st)){
  
  if(house_data$Exterior1st[i] == "ImStucc"|| house_data$Exterior1st[i] == "Stone")
  {
    
    house_data$Exterior1st[i] = "CemntBd"
    
    
    
  }
  
  if(house_data$Exterior1st[i] == "AsphShn"|| house_data$Exterior1st[i] == "BrkComm"
     ||house_data$Exterior1st[i] == "CBlock" )
  {
    
    house_data$Exterior1st[i] = "CBlock"
    
    
    
  }
  
  
}
house_data$Exterior1st <- as.factor(house_data$Exterior1st)






house_data$Exterior2nd <- as.character(house_data$Exterior2nd)



for ( i in 1:length(house_data$Exterior2nd)){
  
  if(house_data$Exterior2nd[i] == "ImStucc"||house_data$Exterior2nd[i] == "Other")
  {

    house_data$Exterior2nd[i] = "CmentBd"



  }

  if(house_data$Exterior2nd[i] == "AsphShn"|| house_data$Exterior2nd[i] == "Brk Cmn"
     ||house_data$Exterior2nd[i] == "CBlock" )
  {

    house_data$Exterior2nd[i] = "AsbShng"



  }

  if(house_data$Exterior2nd[i] == "Stone")
  {

    house_data$Exterior2nd[i] = "Stucco"


  
  
  } 
  
}
house_data$Exterior2nd <- as.factor(house_data$Exterior2nd)




##
## changing MiscFeature as per test data labels

house_data$MiscFeature <- as.character(house_data$MiscFeature)



for ( i in 1:length(house_data$MiscFeature)){
  
  if(house_data$MiscFeature[i] == "Gar2"|| house_data$MiscFeature[i] == "TenC")
  {
    
    house_data$MiscFeature[i] = "None"
    
    
    
  }
  
  if(house_data$MiscFeature[i] == "Othr" )
  {
    
    house_data$MiscFeature[i] = "Shed"
    
    
    
  }
  
  
}
house_data$MiscFeature <- as.factor(house_data$MiscFeature)





##
## changing GarageQual as per test data labels

house_data$GarageQual <- as.character(house_data$GarageQual)



for ( i in 1:length(house_data$GarageQual)){
  
  
  
  if(house_data$GarageQual[i] == "Excellent" )
  {
    
    house_data$GarageQual[i] = "Good/Typical"
    
    
    
  }
  
  
}
house_data$GarageQual <- as.factor(house_data$GarageQual)



##Recombining categories for better prediction

##for MSSubclass 30 and 45 and 180
## 40 and 50
## 190 and 90
##75 and 80 and 85


house_data$MSSubClass <- as.character(house_data$MSSubClass)
for ( i in 1:length(house_data$MSSubClass)){
  
  if(house_data$MSSubClass[i] == "30"||house_data$MSSubClass[i] == "45"
     ||house_data$MSSubClass[i] == "180")
  {
    
    house_data$MSSubClass[i] = "30/45/180"
    
    
    
  }
  
  if(house_data$MSSubClass[i] == "75"|| house_data$MSSubClass[i] == "80"
     ||house_data$MSSubClass[i] == "85" )
  {
    
    house_data$MSSubClass[i] = "75/80/85"
    
    
    
  }
  
  if(house_data$MSSubClass[i] == "40"||house_data$MSSubClass[i]== "50")
  {
    
    house_data$MSSubClass[i] = "40/50"
    
    
    
    
  } 
  
  if(house_data$MSSubClass[i] == "190"||house_data$MSSubClass[i]== "90")
  {
    
    house_data$MSSubClass[i] = "90/190"
    
    
    
    
  } 
  
}
house_data$MSSubClass <- as.factor(house_data$MSSubClass)


##
## changing MSZoning as per test data labels

house_data$MSZoning <- as.character(house_data$MSZoning)



for ( i in 1:length(house_data$MSZoning)){
  
  
  
  if(house_data$MSZoning[i] == "Res. high density"||house_data$MSZoning[i] == "Res. med density" )
  {
    
    house_data$MSZoning[i] = "Res. high/med"
    
    
    
  }
  
  
}
house_data$MSZoning <- as.factor(house_data$MSZoning)




##
## changing LotShape as per test data labels

house_data$LotShape <- as.character(house_data$LotShape)



for ( i in 1:length(house_data$LotShape)){
  
  
  
  if(house_data$LotShape[i] == "Irregular(slight)"||house_data$LotShape[i] == "Irregular(Moder)"
     || house_data$LotShape[i] =="Irregular")
  {
    
    house_data$LotShape[i] = "Irregular"
    
    
    
  }
  
  
}
house_data$LotShape <- as.factor(house_data$LotShape)


##
## changing LandContour as per test data labels

house_data$LandContour <- as.character(house_data$LandContour)



for ( i in 1:length(house_data$LandContour)){
  
  
  
  if(house_data$LandContour[i] == "HillSide"||house_data$LandContour[i] == "Low")
  {
    
    house_data$LandContour[i] = "HillSide/Low"
    
    
    
  }
  
  
}
house_data$LandContour <- as.factor(house_data$LandContour)


##
## changing LotConfig as per test data labels

house_data$LotConfig <- as.character(house_data$LotConfig)



for ( i in 1:length(house_data$LotConfig)){
  
  
  
  if(house_data$LotConfig[i] == "CulDSac"||house_data$LotConfig[i] == "Frontage(3 sides)")
  {
    
    house_data$LotConfig[i] = "CulDSac/Frontage3Sides"
    
    
    
  }
  
  
}
house_data$LotConfig <- as.factor(house_data$LotConfig)





##
## changing LandSlope as per test data labels

house_data$LandSlope <- as.character(house_data$LandSlope)



for ( i in 1:length(house_data$LandSlope)){
  
  
  
  if(house_data$LandSlope[i] == "Moderate"||house_data$LandSlope[i] == "Severe")
  {
    
    house_data$LandSlope[i] = "Moderate/Severe"
    
    
    
  }
  
  
}
house_data$LandSlope <- as.factor(house_data$LandSlope)





## changing Condition1 as per test data labels


house_data$Condition1 <- as.character(house_data$Condition1)



for ( i in 1:length(house_data$Condition1)){
  
  if(house_data$Condition1[i] == "RRAe"||house_data$Condition1[i] == "RRAn"
     ||house_data$Condition1[i] == "RRNe"||house_data$Condition1[i] == "RRNn")
  {
    
    house_data$Condition1[i] = "RRA/RRN"
    
    
    
  }
  
  if(house_data$Condition1[i] == "PosA"|| house_data$Condition1[i] == "PosN")
  {
    
    house_data$Condition1[i] = "PosA/PosN"
    
    
    
  }
  
  
  
}
house_data$Condition1 <- as.factor(house_data$Condition1)



##
## changing BldgType as per test data labels

house_data$BldgType <- as.character(house_data$BldgType)



for ( i in 1:length(house_data$BldgType)){
  
  
  
  if(house_data$BldgType[i] == "2fmCon"||house_data$BldgType[i] == "Duplex")
  {
    
    house_data$BldgType[i] = "2fmCon/Duplex"
    
    
    
  }
  
  
}
house_data$BldgType <- as.factor(house_data$BldgType)


## changing BldgType as per test data labels

house_data$BldgType <- as.character(house_data$BldgType)



for ( i in 1:length(house_data$BldgType)){
  
  
  
  if(house_data$BldgType[i] == "2fmCon"||house_data$BldgType[i] == "Duplex")
  {
    
    house_data$BldgType[i] = "2fmCon/Duplex"
    
    
    
  }
  
  
}
house_data$BldgType <- as.factor(house_data$BldgType)

## changing HouseStyle as per test data labels

house_data$HouseStyle <- as.character(house_data$HouseStyle)



for ( i in 1:length(house_data$HouseStyle)){
  
  
  
  if(house_data$HouseStyle[i] == "1.5Unf"||house_data$HouseStyle[i] == "2.5Unf"
     ||house_data$HouseStyle[i] == "SFoyer"||house_data$HouseStyle[i] == "SLvl")
  {
    
    house_data$HouseStyle[i] = "Others"
    
    
    
  }
  
  
}
house_data$HouseStyle <- as.factor(house_data$HouseStyle)

##WdShing and wd Sdng
##AsbShng and CBlock



## changing Exterior1st as per test data labels

house_data$Exterior1st <- as.character(house_data$Exterior1st)



for ( i in 1:length(house_data$Exterior1st)){
  
  
  
  if(house_data$Exterior1st[i] == "AsbShng"||house_data$Exterior1st[i] == "CBlock"
     )
  {
    
    house_data$Exterior1st[i] = "AsbShng/CBlock"
    
    
    
  }
  
  if(house_data$Exterior1st[i] == "WdShing"||house_data$Exterior1st[i] == "Wd Sdng"
  )
  {
    
    house_data$Exterior1st[i] = "WdShing/Wd Sdng"
    
    
    
  }
  
  
}
house_data$Exterior1st <- as.factor(house_data$Exterior1st)


## changing Exterior2nd as per test data labels

house_data$Exterior2nd <- as.character(house_data$Exterior2nd)



for ( i in 1:length(house_data$Exterior2nd)){
  
  
  
  if(house_data$Exterior2nd[i] == "Stucco"||house_data$Exterior2nd[i] == "Plywood"
  )
  {
    
    house_data$Exterior2nd[i] = "Stucco/Plywood"
    
    
    
  }
  
  if(house_data$Exterior2nd[i] == "BrkFace"||house_data$Exterior2nd[i] == "CmentBd"
  )
  {
    
    house_data$Exterior2nd[i] = "BrkFace/CmentBd"
    
    
    
  }
  
  if(house_data$Exterior2nd[i] == "WdShing"||house_data$Exterior2nd[i] == "Wd Shng"
     ||house_data$Exterior2nd[i] == "AsbShng")
  {
    
    house_data$Exterior2nd[i] = "WdShing/Wd Sdng/AsbShng"
    
    
    
  }
  
  
}
house_data$Exterior2nd <- as.factor(house_data$Exterior2nd)






##
## changing MasVnrType as per test data labels

house_data$MasVnrType <- as.character(house_data$MasVnrType)



for ( i in 1:length(house_data$MasVnrType)){
  
  
  
  if(house_data$MasVnrType[i] == "None"||house_data$MasVnrType[i] == "BrkCmn")
  {
    
    house_data$MasVnrType[i] = "None/BrkCmn"
    
    
    
  }
  
  
}
house_data$MasVnrType <- as.factor(house_data$MasVnrType)




## changing ExterCond as per test data labels

house_data$ExterCond <- as.character(house_data$ExterCond)



for ( i in 1:length(house_data$ExterCond)){
  
  
  
  if(house_data$ExterCond[i] == "Excellent"||house_data$ExterCond[i] == "Fair/Typical/Poor")
  {
    
    house_data$ExterCond[i] = "Others"
    
    
    
  }
  
  
}
house_data$ExterCond <- as.factor(house_data$ExterCond)
##function to create a bar plot
library(ggplot2)

factor_cols <- names(house_data)[sapply(house_data, is.factor)]






boxplots  <-function(df,factors,coly)
  
{
  
  for (col in factors )
  {
    
    p <- ggplot(df) + 
      geom_boxplot(aes_string(col, coly), fill ="royalblue3")+
      labs(y = coly, x=col, title =paste(coly,"vs",col))+
      theme( panel.background = element_rect(fill =   "lavender"))
    
    print(p)
    
    
  }
}

##boxplots(house_data,factor_cols, "SalePrice")


# Graphical analysis of individual variable
# 
# SaleCondition - Partial has most price and there are categorical differences for each category
# 
# 
# SAletype <- there are differences
# 
# MiscFeature<- just little bit differences(categories can be clubbed)
# FenceM- no such differnce within fence types(cat can be clubbed)-
#   PoolQC <- execlent pool quality has a major difference in price
# 
# PavedDrve <- smalllittle differences
# garageCond <- small little diff(
#   garage qal <- there are differences
#   garagefinish <- there are differences
#   
#   
#   
#   Check and combine garage year built???
#     
#     
#     garage type there are differences
#   
#   FireplaceQu there are differences
#   
#   Functional is pretty close with small lil differences
#   
#   kitchen qual has major differences
#   
#   Apart from one category electrical has no major differences categories may be combined
#   
#   heating can be combined
#   
#   basmittype2 no major differences
#   basmmit type 1 little differnces
#   
#   basmt exposure smailldiffernces




## exloring non factor columns and its relationship with the label


non_factors <- names(house_data)[!sapply(house_data, is.factor)]

## removing id and SalePrice
non_factors <- non_factors[-1]
non_factors<- non_factors[-20]

scatterplots  <-function(df,non_factor_cols,coly)
  
{
  
  for (col in non_factor_cols )
  {
    
    p <- ggplot(df) + 
      geom_point(aes_string(col, coly), fill ="royalblue3")+
      labs(y = coly, x=col, title =paste(coly,"vs",col))+
      theme( panel.background = element_rect(fill =   "lavender"))
    
    print(p)
    
    
  }
}

##scatterplots(house_data,non_factors, "SalePrice")



## columns which need to be converted to lables
## YrSold, MoSold, GarageCars, fireplaces, tormsabvgrd, kitchenabvgr, 
##bedroom abvGr, halfbath,
## fullbath, bsmthalfbath, bsmtfullbath,yearremodadd, yearbuilt, 
##overallcond, overall qaul,  

## Miscval does not seem to matter most of row values are 0s same for pool area and same is for X3ssnporch
## lowQaulFinSF




### trying to check if relationship with log of price is better as compared to just the price

house_data <- house_data %>% mutate(SalePrice_log = log(SalePrice))

##scatterplots(house_data,non_factors, "SalePrice_log")

##boxplots(house_data, factor_cols, "SalePrice_log")


##Does not seem to matter
## year sold, Mosold, basmthalfbath,basmtfullbath, miscvalue, poolarea,porches both, low 
##lowqualFinSF, bsmtfnSf2,bsmtfinsf1


##splitting training set into 3 parts training, validation set , and test set
library(caret)

set.seed(100)
partition <- createDataPartition(house_data[,"BldgType"],times =1, p=0.80, list = FALSE )
train1 <- house_data[partition,]
test_set <- house_data[-partition,]


set.seed(123)
partition <- createDataPartition(train1[,"BldgType"],times =1, p=0.75, list = FALSE )
training_set <- train1[partition,]
validation_set <- train1[-partition,]






## preprocess data


## numerical features

set.seed(123)
preProcValues <- preProcess(training_set[,non_factors], method = c("center", "scale"))
training_set[, non_factors] <- predict(preProcValues, training_set[, non_factors])
test_set[, non_factors] <- predict(preProcValues, test_set[, non_factors])
validation_set[, non_factors] <- predict(preProcValues, validation_set[, non_factors])


## actual test_set
house_test[, non_factors] <- predict(preProcValues, house_test[, non_factors])


## categorical features


set.seed(12)

dummies <- dummyVars( ~ ., data = training_set)
 training_set <- as.data.frame(predict(dummies,training_set))
 test_set <- as.data.frame(predict(dummies,test_set))
 validation_set <- as.data.frame(predict(dummies, validation_set))
 
 
 ## for actual test data
 dummies_test <- dummyVars( ~ ., data = house_test)
 house_test_dummies <- as.data.frame(predict(dummies_test,house_test))


 
 
 
 ## gotta check this out
##house_test <- as.data.frame(predict(dummies,house_test))
 
 
 
 ## setting up linear regression model 
 
 
 lin_mod1 <- lm(SalePrice_log ~ ., data = select(training_set, -c("SalePrice", "Id") ))
 
 score1 <- predict(lin_mod1, select(test_set, -c("SalePrice", "Id") ))
 
 
 ## create function print metrics to determine model's performance. 
 
 print_metrics = function(lin_mod, df, score, label){
   resids = df[,label] - score
   resids2 = resids**2
   N = length(score)
   r2 = as.character(round(summary(lin_mod)$r.squared, 4))
   adj_r2 = as.character(round(summary(lin_mod)$adj.r.squared, 4))
   cat(paste('Mean Square Error      = ', as.character(round(sum(resids2)/N, 4)), '\n'))
   cat(paste('Root Mean Square Error = ', as.character(round(sqrt(sum(resids2)/N), 4)), '\n'))
   cat(paste('Mean Absolute Error    = ', as.character(round(sum(abs(resids))/N, 4)), '\n'))
   cat(paste('Median Absolute Error  = ', as.character(round(median(abs(resids)), 4)), '\n'))
   cat(paste('R^2                    = ', r2, '\n'))
   cat(paste('Adjusted R^2           = ', adj_r2, '\n'))
 }
 
 
 print_metrics(lin_mod1, test_set, score1, "SalePrice_log")
 

 
 
 
 
 ## checking important predictors
 
 library(gsub)
 varImps1<- varImp(lin_mod1)
 varImps1[,2] <- row.names(varImps1)
 varImps1 <- varImps1[,c(2,1)]
 row.names(varImps1) <- NULL
 names(varImps1)[1] <- "feature"
 
varImps1$feature <- gsub("`", "", as.character(varImps1$feature))

varImps1<- varImps1[order(-varImps1$Overall),]

## keeping oonly features which have importances greater than 2
features_updated1 <- NULL
 for(i in 1:length(varImps1$Overall)){
  if(varImps1$Overall[i] > 2)
    features_updated1 <- c( features_updated1, varImps1$feature[i])
  
 }


## testing model with this

features_updated1 <- c(features_updated1, "SalePrice_log")
lin_mod2 <- lm(SalePrice_log ~ ., data = select(training_set,features_updated1 ))

score_test2 <- predict(lin_mod2, select(test_set, features_updated1 ))


print_metrics(lin_mod2, test_set, score_test2, "SalePrice_log")
print_metrics(lin_mod1, test_set, score1, "SalePrice_log")




## model performance improved, lets further remove features
features_updated2 <- NULL
for(i in 1:length(varImps1$Overall)){
  if(varImps1$Overall[i] > 3.5)
    features_updated2 <- c( features_updated2, varImps1$feature[i])
  
}



features_updated2 <- c(features_updated2, "SalePrice_log")
lin_mod3 <- lm(SalePrice_log ~ ., data = select(training_set,features_updated2 ))

score_test3 <- predict(lin_mod3, select(test_set, features_updated2 ))

print_metrics(lin_mod3, test_set, score_test3, "SalePrice_log")
print_metrics(lin_mod2, test_set, score_test2, "SalePrice_log")
print_metrics(lin_mod1, test_set, score1, "SalePrice_log")


## validation set
score_v2 <- predict(lin_mod2, select(validation_set, features_updated1))
score_v3 <-  predict(lin_mod3, select(validation_set, features_updated2))

print_metrics(lin_mod3, validation_set, score_v2, "SalePrice_log")
print_metrics(lin_mod1, validation_set, score_v3, "SalePrice_log")


## checking actual results. 


predict_basic <- predict(lin_mod2, select(house_test_dummies, features_updated1))



trainControl
train()

