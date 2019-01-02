
## the script is for data preparation of actual test data and it involves performing same operations on 
## test data as performed on train data


## loading data

house_test <- read.csv("test.csv")

## Getting glimpse of data
head(house_test)
str(house_test)


## understanding data
## changing MSSubclass to factor

for(  i in 1:length(house_test$MSSubClass)){
  if( house_test$MSSubClass[i] == 150)
    house_test$MSSubClass[i] =160
}

house_test$MSSubClass <- as.factor(house_test$MSSubClass)


#  replace NA in Alley column with no alleys
sum(is.na(house_test$Alley))
table(house_test$Alley)

house_test$Alley <- ifelse(is.na(house_test$Alley), "No Alleys", house_test$Alley)
str(house_test$Alley)

house_test$Alley <- as.factor(house_test$Alley)

levels(house_test$Alley) <- c("Grvl", "Pave", "No Alleys" )



## Replacing Na in BsmtQual with "No basmt"


house_test$BsmtQual <- as.character(house_test$BsmtQual)

house_test$BsmtQual <- ifelse(is.na(house_test$BsmtQual), "No basmt", house_test$BsmtQual)
str(house_test$BsmtQual)

house_test$BsmtQual <- as.factor(house_test$BsmtQual)

## Replacing Na in BsmtCond with "No basmt"

house_test$BsmtCond <- as.character(house_test$BsmtCond)

house_test$BsmtCond <- ifelse(is.na(house_test$BsmtCond), "No basmt", house_test$BsmtCond)
str(house_test$BsmtCond)

house_test$BsmtCond <- as.factor(house_test$BsmtCond)


## Replacing Na in BsmtExposure with "No basmt"

house_test$BsmtExposure <- as.character(house_test$BsmtExposure)

house_test$BsmtExposure <- ifelse(is.na(house_test$BsmtExposure), "No basmt", house_test$BsmtExposure)
str(house_test$BsmtExposure)

house_test$BsmtExposure <- as.factor(house_test$BsmtExposure)




## Replacing Na in BsmtFinType1 with "No basmt"

house_test$BsmtFinType1 <- as.character(house_test$BsmtFinType1)

house_test$BsmtFinType1 <- ifelse(is.na(house_test$BsmtFinType1), "No basmt", house_test$BsmtFinType1)
str(house_test$BsmtFinType1)

house_test$BsmtFinType1 <- as.factor(house_test$BsmtFinType1)



## Replacing Na in BsmtFinType2 with "No basmt"

house_test$BsmtFinType2 <- as.character(house_test$BsmtFinType2)

house_test$BsmtFinType2 <- ifelse(is.na(house_test$BsmtFinType2), "No basmt", house_test$BsmtFinType2)
str(house_test$BsmtFinType2)

house_test$BsmtFinType2 <- as.factor(house_test$BsmtFinType2)

## Replacing Na in FireplaceQu with "No Fireplace"

house_test$FireplaceQu <- as.character(house_test$FireplaceQu)

house_test$FireplaceQu <- ifelse(is.na(house_test$FireplaceQu), "No Fireplace", house_test$FireplaceQu)
str(house_test$FireplaceQu)

house_test$FireplaceQu <- as.factor(house_test$FireplaceQu)


## Replacing Na in GarageType, GarageQual,GarageYrBlt, GarageCond and GarageFinish with "No Garage"

house_test$GarageType <- as.character(house_test$GarageType)

house_test$GarageType <- ifelse(is.na(house_test$GarageType), "No Garage", house_test$GarageType)
str(house_test$GarageType)

house_test$GarageType <- as.factor(house_test$GarageType)



house_test$GarageFinish <- as.character(house_test$GarageFinish)

house_test$GarageFinish <- ifelse(is.na(house_test$GarageFinish), "No Garage", house_test$GarageFinish)
str(house_test$GarageFinish)

house_test$GarageFinish <- as.factor(house_test$GarageFinish)




house_test$GarageYrBlt <- as.character(house_test$GarageYrBlt)

house_test$GarageYrBlt <- ifelse(is.na(house_test$GarageYrBlt), "No Garage", house_test$GarageYrBlt)
str(house_test$GarageYrBlt)

house_test$GarageYrBlt <- as.factor(house_test$GarageYrBlt)



house_test$GarageQual <- as.character(house_test$GarageQual)

house_test$GarageQual <- ifelse(is.na(house_test$GarageQual), "No Garage", house_test$GarageQual)
str(house_test$GarageQual)

house_test$GarageQual <- as.factor(house_test$GarageQual)



house_test$GarageCond <- as.character(house_test$GarageCond)

house_test$GarageCond <- ifelse(is.na(house_test$GarageCond), "No Garage", house_test$GarageCond)
str(house_test$GarageCond)

house_test$GarageCond <- as.factor(house_test$GarageCond)





## Replacing Na in PoolQC with "No Pool"




house_test$PoolQC <- as.character(house_test$PoolQC)

house_test$PoolQC <- ifelse(is.na(house_test$PoolQC), "No Pool", house_test$PoolQC)
str(house_test$PoolQC)

house_test$PoolQC <- as.factor(house_test$PoolQC)


## Replacing Na in Fence with "No Fence"




house_test$Fence <- as.character(house_test$Fence)

house_test$Fence <- ifelse(is.na(house_test$Fence), "No Fence", house_test$Fence)
str(house_test$Fence)

house_test$Fence <- as.factor(house_test$Fence)



## Replacing Na in MiscFeature with "None"




house_test$MiscFeature <- as.character(house_test$MiscFeature)

house_test$MiscFeature <- ifelse(is.na(house_test$MiscFeature), "None", house_test$MiscFeature)
str(house_test$MiscFeature)

house_test$MiscFeature <- as.factor(house_test$MiscFeature)





## checking Nas in data set now

library(e1071)
sapply(house_test, function(x) {sum(is.na(x))})




##skewness0.6603101 so replacinf NAs with mean

house_test$LotFrontage <-ifelse(is.na(house_test$LotFrontage), mean(house_test$LotFrontage, na.rm = T), house_test$LotFrontage)


sapply(house_test, function(x) {sum(is.na(x))})


## repacing MSZoning NA values with majority values
house_test$MSZoning <- as.character(house_test$MSZoning)

house_test$MSZoning <-ifelse(is.na(house_test$MSZoning), 'RL', house_test$MSZoning)
house_test$MSZoning <- as.factor(house_test$MSZoning)



##
## repacing Utilities NA values with majority values
house_test$Utilities <- as.character(house_test$Utilities)

house_test$Utilities <-ifelse(is.na(house_test$Utilities), 'AllPub', house_test$Utilities)
house_test$Utilities <- as.factor(house_test$Utilities)




##
## repacing Exterior1st NA values with majority values
house_test$Exterior1st <- as.character(house_test$Exterior1st)

house_test$Exterior1st <-ifelse(is.na(house_test$Exterior1st), 'VinylSd', house_test$Exterior1st)
house_test$Exterior1st <- as.factor(house_test$Exterior1st)


##
## repacing Exterior2nd NA values with majority values
house_test$Exterior2nd <- as.character(house_test$Exterior2nd)

house_test$Exterior2nd <-ifelse(is.na(house_test$Exterior2nd), 'VinylSd', house_test$Exterior2nd)
house_test$Exterior2nd <- as.factor(house_test$Exterior2nd)



##
## repacing MasVnrType NA values with majority values
house_test$MasVnrType <- as.character(house_test$MasVnrType)

house_test$MasVnrType <-ifelse(is.na(house_test$MasVnrType), 'None', house_test$MasVnrType)
house_test$MasVnrType <- as.factor(house_test$MasVnrType)



##replacing Nas with mean for MasVnrArea

house_test$MasVnrArea <-ifelse(is.na(house_test$MasVnrArea), mean(house_test$MasVnrArea, na.rm = T), house_test$MasVnrArea)



##replacing Nas with 0 for BsmtFinSF1

house_test$BsmtFinSF1 <-ifelse(is.na(house_test$BsmtFinSF1), 0, house_test$BsmtFinSF1)

house_test$BsmtFinSF2 <-ifelse(is.na(house_test$BsmtFinSF2), 0, house_test$BsmtFinSF2)

house_test$BsmtUnfSF <-ifelse(is.na(house_test$BsmtUnfSF), 0, house_test$BsmtUnfSF)
house_test$TotalBsmtSF <-ifelse(is.na(house_test$TotalBsmtSF), 0, house_test$TotalBsmtSF)


house_test$BsmtFullBath <-ifelse(is.na(house_test$BsmtFullBath), 0, house_test$BsmtFullBath)


house_test$BsmtHalfBath <-ifelse(is.na(house_test$BsmtHalfBath), 0, house_test$BsmtHalfBath)



##replacing Nas with majority values for KitchenQual
house_test$KitchenQual <- as.character(house_test$KitchenQual)

house_test$KitchenQual <-ifelse(is.na(house_test$KitchenQual), 'Gd', house_test$KitchenQual)
house_test$KitchenQual <- as.factor(house_test$KitchenQual)



##replacing Nas with majority values for Functional
house_test$Functional <- as.character(house_test$Functional)

house_test$Functional <-ifelse(is.na(house_test$Functional), 'Typ', house_test$Functional)
house_test$Functional <- as.factor(house_test$Functional)


## replacing Nas with 0 for GarageCars,garageArea



house_test$GarageArea <-ifelse(is.na(house_test$GarageArea), 0, house_test$GarageArea)


house_test$GarageCars <-ifelse(is.na(house_test$GarageCars), 0, house_test$GarageCars)


##replacing Nas with majority values for SaleType
house_test$SaleType <- as.character(house_test$SaleType)

house_test$SaleType <-ifelse(is.na(house_test$SaleType), 'WD', house_test$SaleType)
house_test$SaleType <- as.factor(house_test$SaleType)


## Checking if we still have any NAs on test data
any(is.na(house_test))




## Exploring categorical features



## changing level names for better visualization and exploration

levels(house_test$MSZoning)<- c("Commercial", "FloatingVill", "Res. high density", 
                                "Res. low density", "Res. med density")



levels(house_test$LotShape)<- c("Irregular(slight)", "Irregular(Moder)","Irregular","Regular")
levels(house_test$LandContour)<- c("Banked", "HillSide", "Low", "Level")
levels(house_test$Utilities)<- c("All", "No sewarage")
levels(house_test$LotConfig)[3:4]<- c("Frontage(2 sides)","Frontage(3 sides)" )

levels(house_test$LandSlope)<- c("Gentle", "Moderate","Severe")

levels(house_test$ExterQual )<- c("Excellent", "Fair","Good","Typical")

levels(house_test$ExterCond)<- c("Excellent","Fair","Good","Poor","Typical")

levels(house_test$BsmtQual )<- c("Excellent","Fair","Good","No basmt","Typical")

levels(house_test$BsmtCond )<- c("Fair","Good","No bsmt","Poor","Typical")


levels(house_test$BsmtExposure)<- c("Average","Good","Minimum","No","No basmt")

levels(house_test$BsmtFinType1)<- c("Avg. LQ","Below avg. LQ", "Good LQ","Low Quality","No basmnt","Avg. RecRoom","Unfinished" )



levels(house_test$BsmtFinType2)<-c("Avg. LQ","Below avg. LQ", "Good LQ","Low Quality","No basmnt","Avg. RecRoom","Unfinished" )


levels(house_test$HeatingQC)<- c("Excellent","Fair","Good","Poor","Typical")


levels(house_test$KitchenQual)<-  c("Excellent","Fair","Good","Typical")


levels(house_test$Functional)<- c("MajorDeductn1","MajorDeductn2","MinorDeduc1","MinorDeduc","Moder.Deduction","SeverelyDamaged","Typical")


levels(house_test$FireplaceQu)<- c("Excellent","Fair","Good","No Fireplace","Poor","Typical")


levels(house_test$GarageFinish)<- c("Finished","No Garage","RoughFinished","Unfinished")
## different
levels(house_test$GarageQual)<- c("Fair","Good","No Garage","Poor","Typical")


levels(house_test$GarageCond)<- c("Excellent","Fair","Good","No Garage","Poor","Typical")


levels(house_test$PavedDrive) <- c("No","Partial","Yes")

##diff
levels(house_test$PoolQC)<- c("Excellent","Good","No Pool")


levels(house_test$Fence) <- c("GoodPrivacy","GoodWood","MinimumPrivacy","MinimumWood","NoFence")









## converting columns to factors

house_test$YrSold <- as.factor(house_test$YrSold)

house_test$MoSold <- as.factor(house_test$MoSold)

##house_test$GarageCars <- as.factor(house_test$GarageCars)

##house_test$GarageCars <- as.character(house_test$GarageCars)


## combine categories for 3 or more garagecars

for ( i in 1:length(house_test$GarageCars)){
  
  if((house_test$GarageCars[i]) >=3)
  {
    
    
    house_test$GarageCars[i] = "3 or more"
    
  }
}


## converting to factor

house_test$GarageCars <- as.factor(house_test$GarageCars)


## fireplaces
for ( i in 1:length(house_test$Fireplaces)){
  
  if((house_test$Fireplaces[i]) >=2)
  {
    
    
    house_test$Fireplaces[i] = "2 or more"
    
  }
}


house_test$Fireplaces <- as.factor( house_test$Fireplaces)

## combine categories for TotRmsAbvGrd 

for ( i in 1:length(house_test$TotRmsAbvGrd)){
  
  
  
  
  if((house_test$TotRmsAbvGrd[i]) == 10|(house_test$TotRmsAbvGrd[i]) == 11|(house_test$TotRmsAbvGrd[i]) == 12|
     (house_test$TotRmsAbvGrd[i]) == 14 | (house_test$TotRmsAbvGrd[i]) == 13|
     (house_test$TotRmsAbvGrd[i]) == 15)
  {
    
    
    house_test$TotRmsAbvGrd[i] = "10 or more"
    
  }
  
  if((house_test$TotRmsAbvGrd[i]) == 2|(house_test$TotRmsAbvGrd[i]) == 3| (house_test$TotRmsAbvGrd[i]) == 4)
  {
    
    
    house_test$TotRmsAbvGrd[i] = "4 or less"
    
  }
}

house_test$TotRmsAbvGrd <- as.factor(house_test$TotRmsAbvGrd)



##kitchenabvgr combinging categores
table(house_test$KitchenAbvGr)
for ( i in 1:length(house_test$KitchenAbvGr)){
  
  if((house_test$KitchenAbvGr[i]) == 0|(house_test$KitchenAbvGr[i]) == 1)
  {
    
    
    house_test$KitchenAbvGr[i] = "1 or less"
    
  }
  
  if((house_test$KitchenAbvGr[i]) == 2|(house_test$KitchenAbvGr[i]) == 3)  {
    
    
    house_test$KitchenAbvGr[i] = "2 or more"
    
  }
}


house_test$KitchenAbvGr <- as.factor(house_test$KitchenAbvGr)


## combining categories for BedroomAbvGr

table(house_test$BedroomAbvGr)


for ( i in 1:length(house_test$BedroomAbvGr)){
  
  if((house_test$BedroomAbvGr[i]) <=2)
  {
    
    
    house_test$BedroomAbvGr[i] = "2 or less"
    
  }
  
  if((house_test$BedroomAbvGr[i]) >= 4)  {
    
    
    house_test$BedroomAbvGr[i] = "4 or more"
    
  }
}


house_test$BedroomAbvGr <- as.factor(house_test$BedroomAbvGr)



##combining HalfBath categories and converting to factor

table(house_test$HalfBath)

for ( i in 1:length(house_test$HalfBath)){
  
  if((house_test$HalfBath[i]) >=1)
  {
    
    house_test$HalfBath[i] = "1 or more"
    
  }
}

house_test$HalfBath <- as.factor(house_test$HalfBath)

## combining categoreis of fullbath and converting to factor

table(house_test$FullBath)

for ( i in 1:length(house_test$FullBath)){
  
  if((house_test$FullBath[i]) <=1)
  {
    
    house_test$FullBath[i] = "1 or less"
    
  }
  if((house_test$FullBath[i]) >= 2)
  {
    
    house_test$FullBath[i] = "2 or more"
    
  }
}

house_test$FullBath <- as.factor(house_test$FullBath)

## combining categories and converting to factor for bsmthalfbath



table(house_test$BsmtHalfBath)

for ( i in 1:length(house_test$BsmtHalfBath)){
  
  if((house_test$BsmtHalfBath[i]) >=1)
  {
    
    house_test$BsmtHalfBath[i] = "1 or more"
    
  }
}
house_test$BsmtHalfBath <- as.factor(house_test$BsmtHalfBath)

## combining categories and converting to factor for bsmtfullbah



table(house_test$BsmtFullBath)

for ( i in 1:length(house_test$BsmtFullBath)){
  
  if((house_test$BsmtFullBath[i]) >=1)
  {
    
    house_test$BsmtFullBath[i] = "1 or more"
    
  }
}

house_test$BsmtFullBath <- as.factor(house_test$BsmtFullBath)


##combining categories and converting to factor for yearremodadd



table(house_test$YearRemodAdd)

for ( i in 1:length(house_test$YearRemodAdd)){
  
  if(((house_test$YearRemodAdd[i]) >=1950 && (house_test$YearRemodAdd[i]) <= 1955)  )
  {
    
    house_test$YearRemodAdd[i] = "1950-1955"
    
  }
  
  if(((house_test$YearRemodAdd[i]) >=1956 && (house_test$YearRemodAdd[i]) <= 1960) )
  {
    
    house_test$YearRemodAdd[i] = "1956-1960"
    
  }
  
  if(((house_test$YearRemodAdd[i]) >=1961 && (house_test$YearRemodAdd[i]) <= 1965)  )
  {
    
    house_test$YearRemodAdd[i] = "1961-1965"
    
  }
  
  
  if(((house_test$YearRemodAdd[i]) >=1966 && (house_test$YearRemodAdd[i]) <= 1970 ))
  {
    
    house_test$YearRemodAdd[i] = "1966-1970"
    
  }
  
  
  if(((house_test$YearRemodAdd[i]) >=1971 && (house_test$YearRemodAdd[i]) <= 1975)  )
  {
    
    house_test$YearRemodAdd[i] = "1971-1975"
    
  }
  
  if(((house_test$YearRemodAdd[i]) >=1976 && (house_test$YearRemodAdd[i]) <= 1980) )
  {
    
    house_test$YearRemodAdd[i] = "1976-1980"
    
  }
  
  
  if(((house_test$YearRemodAdd[i]) >=1981 && (house_test$YearRemodAdd[i]) <= 1985)  )
  {
    
    house_test$YearRemodAdd[i] = "1981-1985"
    
  }
  
  if(((house_test$YearRemodAdd[i]) >=1986 && (house_test$YearRemodAdd[i]) <= 1990)  )
  {
    
    house_test$YearRemodAdd[i] = "1986-1990"
    
  }
  
  if(((house_test$YearRemodAdd[i]) >=1991 && (house_test$YearRemodAdd[i]) <= 1995)  )
  {
    
    house_test$YearRemodAdd[i] = "1991-1995"
    
  }
  
  if(((house_test$YearRemodAdd[i]) >=1996 && (house_test$YearRemodAdd[i]) <= 2000)  )
  {
    
    house_test$YearRemodAdd[i] = "1996-2000"
    
  }
  
  if(((house_test$YearRemodAdd[i]) >=2001 &&  (house_test$YearRemodAdd[i]) <= 2005)  )
  {
    
    house_test$YearRemodAdd[i] = "2001-2005"
    
  }
  
  if(((house_test$YearRemodAdd[i]) >=2006 &&  (house_test$YearRemodAdd[i]) <= 2010 ) )
  {
    
    house_test$YearRemodAdd[i] = "2006-2010"
    
  }
  
  
}



house_test$YearRemodAdd <- as.factor(house_test$YearRemodAdd)

##combining categories and converting to factor for YearBuilt



table(house_test$YearBuilt)

for ( i in 1:length(house_test$YearBuilt)){
  
  if(((house_test$YearBuilt[i]) >=1871 && (house_test$YearBuilt[i]) <= 1900)  )
  {
    
    house_test$YearBuilt[i] = "Before 1900"
    
  }
  
  if(((house_test$YearBuilt[i]) >=1901 && (house_test$YearBuilt[i]) <= 1925) )
  {
    
    house_test$YearBuilt[i] = "1901-1925"
    
  }
  
  if(((house_test$YearBuilt[i]) >=1926 && (house_test$YearBuilt[i]) <= 1950)  )
  {
    
    house_test$YearBuilt[i] = "1926-1950"
    
  }
  
  
  if(((house_test$YearBuilt[i]) >=1951 && (house_test$YearBuilt[i]) <= 1975 ))
  {
    
    house_test$YearBuilt[i] = "1951-1975"
    
  }
  
  
  if(((house_test$YearBuilt[i]) >=1976 && (house_test$YearBuilt[i]) <= 2000)  )
  {
    
    house_test$YearBuilt[i] = "1976-2000"
    
  }
  
  if(((house_test$YearBuilt[i]) >=2001 && (house_test$YearBuilt[i]) <= 2005) )
  {
    
    house_test$YearBuilt[i] = "2001-2005"
    
  }
  
  
  
  if(((house_test$YearBuilt[i]) >=2006 &&  (house_test$YearBuilt[i]) <= 2010 ) )
  {
    
    house_test$YearBuilt[i] = "2006-2010"
    
  }
  
  
}


house_test$YearBuilt <- as.factor( house_test$YearBuilt)








##combining categories and converting to factor for OverallCond

table(house_test$OverallCond)


for ( i in 1:length(house_test$OverallCond)){
  
  if((house_test$OverallCond[i]) <= 4)
  {
    
    house_test$OverallCond[i] = "4 or less"
    
  }
  
  
  if((house_test$OverallCond[i]) >= 8)
  {
    
    house_test$OverallCond[i] = "8 or more"
    
  }
}

house_test$OverallCond <- as.factor(house_test$OverallCond)



##combining categories and converting to factor for OverallQual

table(house_test$OverallQual)


for ( i in 1:length(house_test$OverallQual)){
  
  if((house_test$OverallQual[i]) <= 4)
  {
    
    house_test$OverallQual[i] = "4 or less"
    
  }
  
  
  if((house_test$OverallQual[i]) >= 8)
  {
    
    house_test$OverallQual[i] = "8 or more"
    
  }
}

house_test$OverallQual <- as.factor(house_test$OverallQual)







## combining categories of poolQC

table(house_test$PoolQC)

levels(house_test$PoolQC)[2] <- "Fair/Good"



## combining categories of GarageCond

table(house_test$GarageCond)

house_test$GarageCond <- as.character(house_test$GarageCond)


for ( i in 1:length(house_test$GarageCond)){
  
  if((house_test$GarageCond[i] == "Fair") || (house_test$GarageCond[i] == "Poor"))
  {
    
    house_test$GarageCond[i] = "Fair/Poor"
    
  }
  
  if((house_test$GarageCond[i] == "Good") || (house_test$GarageCond[i] == "Typical"))
  {
    
    house_test$GarageCond[i] = "Good/Typical"
    
  }
  
}


house_test$GarageCond <- as.factor(house_test$GarageCond)




## combining categories of GarageQual

table(house_test$GarageQual)

house_test$GarageQual <- as.character(house_test$GarageQual)


for ( i in 1:length(house_test$GarageQual)){
  
  if((house_test$GarageQual[i] == "Fair") || (house_test$GarageQual[i] == "Poor"))
  {
    
    house_test$GarageQual[i] = "Fair/Poor"
    
  }
  
  if((house_test$GarageQual[i] == "Good") || (house_test$GarageQual[i] == "Typical")||
     (house_test$GarageQual[i] == "Excellent"))
  {
    
    house_test$GarageQual[i] = "Good/Typical"
    
  }
  
}


house_test$GarageQual <- as.factor(house_test$GarageQual)


## combining categories of Fence

table(house_test$Fence)

house_test$Fence <- as.character(house_test$Fence)


for ( i in 1:length(house_test$Fence)){
  
  if((house_test$Fence[i] == "GoodWood") || (house_test$Fence[i] == "MinimumWood"))
  {
    
    house_test$Fence[i] = "Good/MinimumWood"
    
  }
  
  
}


house_test$Fence <- as.factor(house_test$Fence)


## combining categories of Functional column

table(house_test$Functional)

house_test$Functional <- as.character(house_test$Functional)


for ( i in 1:length(house_test$Functional)){
  
  if((house_test$Functional[i] == "MajorDeductn1") || (house_test$Functional[i] == "MajorDeductn2"))
  {
    
    house_test$Functional[i] = "MajorDeductn"
    
  }
  
  
  
  if((house_test$Functional[i] == "MinorDeduc1") || (house_test$Functional[i] == "MinorDeduc"))
  {
    
    house_test$Functional[i] = "MinorDeductn"
    
  }
  
  
}



house_test$Functional <- as.factor(house_test$Functional)



## combine categories of Electrical


table(house_test$Electrical)



house_test$Electrical <- as.character(house_test$Electrical)



for ( i in 1:length(house_test$Electrical)){
  
  if((house_test$Electrical[i] == "FuseP") || (house_test$Electrical[i] == "Mix")||
     (house_test$Electrical[i] == "FuseF"))
  {
    
    house_test$Electrical[i] = "FuseP/F/Mix"
    
  }
  
  
}


house_test$Electrical <- factor(house_test$Electrical)


## combine categories of HeatingQC


table(house_test$HeatingQC)



house_test$HeatingQC <- as.character(house_test$HeatingQC)



for ( i in 1:length(house_test$HeatingQC)){
  
  if((house_test$HeatingQC[i] == "Fair") || (house_test$HeatingQC[i] == "Poor")||
     (house_test$HeatingQC[i] == "Typical"))
  {
    
    house_test$HeatingQC[i] = "Fair/Poor/Typical"
    
  }
  
  
}


house_test$HeatingQC <- as.factor(house_test$HeatingQC)





## combine categories of Heating


table(house_test$Heating)



house_test$Heating <- as.character(house_test$Heating)



for ( i in 1:length(house_test$Heating)){
  
  if((house_test$Heating[i] == "Floor") || (house_test$Heating[i] == "Grav")||
     (house_test$Heating[i] == "OthW")|| (house_test$Heating[i] == "Wall"))
  {
    
    house_test$Heating[i] = "Others"
    
  }
  
  
}


house_test$Heating <- factor(house_test$Heating)






## combine categories of BsmtFinType1 and BsmtFinType2


table(house_test$BsmtFinType1)



house_test$BsmtFinType1 <- as.character(house_test$BsmtFinType1)



for ( i in 1:length(house_test$BsmtFinType1)){
  
  if((house_test$BsmtFinType1[i] == "Unfinished") || (house_test$BsmtFinType1[i] == "Avg. RecRoom")||
     (house_test$BsmtFinType1[i] == "Low Quality"))
  {
    
    house_test$BsmtFinType1[i] = "LowQuality/Unfinished"
    
  }
  
  
}


house_test$BsmtFinType1 <- factor(house_test$BsmtFinType1)






table(house_test$BsmtFinType2)



house_test$BsmtFinType2 <- as.character(house_test$BsmtFinType2)



for ( i in 1:length(house_test$BsmtFinType2)){
  
  if((house_test$BsmtFinType2[i] == "Unfinished") || (house_test$BsmtFinType2[i] == "Avg. RecRoom")||
     (house_test$BsmtFinType2[i] == "Low Quality"))
  {
    
    house_test$BsmtFinType2[i] = "LowQuality/Unfinished"
    
  }
  
  if((house_test$BsmtFinType2[i] == "Avg. LQ") || (house_test$BsmtFinType2[i] == "Below avg. LQ")||
     (house_test$BsmtFinType2[i] == "Good LQ"))
  {
    
    house_test$BsmtFinType2[i] = "Good/Avg/BAvg LQ"
    
  }
  
  
}


house_test$BsmtFinType2 <- factor(house_test$BsmtFinType2)


## combining categories for house_test$BsmtQual

table(house_test$BsmtQual)

house_test$BsmtQual <- as.character(house_test$BsmtQual)



for ( i in 1:length(house_test$BsmtQual)){
  
  if((house_test$BsmtQual[i] == "Fair") || (house_test$BsmtQual[i] == "Typical"))
  {
    
    house_test$BsmtQual[i] = "Fair/Typical"
    
  }
}
house_test$BsmtQual <- as.factor(house_test$BsmtQual)





## combining categories for house_test$BsmtCond

table(house_test$BsmtCond)

house_test$BsmtCond <- as.character(house_test$BsmtCond)



for ( i in 1:length(house_test$BsmtCond)){
  
  if((house_test$BsmtCond[i] == "Poor") || (house_test$BsmtCond[i] == "Fair"))
  {
    
    house_test$BsmtCond[i] = "Fair/Poor"
    
  }
}
house_test$BsmtCond <- as.factor(house_test$BsmtCond)




# combining categories for house_test$Foundation

table(house_test$Foundation)

house_test$Foundation <- as.character(house_test$Foundation)



for ( i in 1:length(house_test$Foundation)){
  
  if((house_test$Foundation[i] == "Slab") || (house_test$Foundation[i] == "Stone")||
     ( house_test$Foundation[i]=="Wood"))
  {
    
    house_test$Foundation[i] = "Other"
    
  }
}
house_test$Foundation <- as.factor(house_test$Foundation)






# combining categories for house_test$ExterQual

table(house_test$ExterQual)

house_test$ExterQual <- as.character(house_test$ExterQual)



for ( i in 1:length(house_test$ExterQual)){
  
  if((house_test$ExterQual[i] == "Fair") || (house_test$ExterQual[i] == "Typical"))
  {
    
    house_test$ExterQual[i] = "Fair/Typical"
    
  }
}
house_test$ExterQual <- as.factor(house_test$ExterQual)





# combining categories for house_test$ExterCond

table(house_test$ExterCond)

house_test$ExterCond <- as.character(house_test$ExterCond)



for ( i in 1:length(house_test$ExterCond)){
  
  if((house_test$ExterCond[i] == "Fair") || (house_test$ExterCond[i] == "Typical")
     ||(house_test$ExterCond[i] == "Poor"))
  {
    
    house_test$ExterCond[i] = "Fair/Typical/Poor"
    
    
    
  }
  
  
  
}
house_test$ExterCond <- as.factor(house_test$ExterCond)



### combining categories for house_test$RoofMatl

table(house_test$RoofMatl)

house_test$RoofMatl <- as.character(house_test$RoofMatl)



for ( i in 1:length(house_test$RoofMatl)){
  
  if((house_test$RoofMatl[i] == "ClyTile") || (house_test$RoofMatl[i] == "Membran")
     ||(house_test$RoofMatl[i] == "Metal")||(house_test$RoofMatl[i] == "Roll")
     ||(house_test$RoofMatl[i] == "Tar&Grv")||(house_test$RoofMatl[i] == "WdShake")
     ||(house_test$RoofMatl[i] == "WdShngl"))
  {
    
    house_test$RoofMatl[i] = "Other"
    
    
    
  }
  
  
  
}
house_test$RoofMatl <- as.factor(house_test$RoofMatl)



###### combining categories for house_test$RoofStyle

table(house_test$RoofStyle)

house_test$RoofStyle <- as.character(house_test$RoofStyle)



for ( i in 1:length(house_test$RoofStyle)){
  
  if((house_test$RoofStyle[i] == "Flat") || (house_test$RoofStyle[i] == "Gambrel")
     ||(house_test$RoofStyle[i] == "Mansard")||(house_test$RoofStyle[i] == "Shed"))
  {
    
    house_test$RoofStyle[i] = "Other"
    
    
    
  }
  
  
  
}
house_test$RoofStyle <- as.factor(house_test$RoofStyle)


house_test$Exterior1st <- as.character(house_test$Exterior1st)



for ( i in 1:length(house_test$Exterior1st)){
  
  if(house_test$Exterior1st[i] == "ImStucc"|| house_test$Exterior1st[i] == "Stone")
  {
    
    house_test$Exterior1st[i] = "CemntBd"
    
    
    
  }
  
  if(house_test$Exterior1st[i] == "AsphShn"|| house_test$Exterior1st[i] == "BrkComm"
     ||house_test$Exterior1st[i] == "CBlock" )
  {
    
    house_test$Exterior1st[i] = "CBlock"
    
    
    
  }
  
  
}
house_test$Exterior1st <- as.factor(house_test$Exterior1st)





house_test$Exterior2nd <- as.character(house_test$Exterior2nd)



for ( i in 1:length(house_test$Exterior2nd)){
  
  if(house_test$Exterior2nd[i] == "ImStucc"||house_test$Exterior2nd[i] == "Other")
  {
    
    house_test$Exterior2nd[i] = "CmentBd"
    
    
    
  }
  
  if(house_test$Exterior2nd[i] == "AsphShn"|| house_test$Exterior2nd[i] == "Brk Cmn"
     ||house_test$Exterior2nd[i] == "CBlock" )
  {
    
    house_test$Exterior2nd[i] = "AsbShng"
    
    
    
  }
  
  if(house_test$Exterior2nd[i] == "Stone")
  {
    
    house_test$Exterior2nd[i] = "Stucco"
    
    
    
    
  } 
  
}
house_test$Exterior2nd <- as.factor(house_test$Exterior2nd)




##
## changing MiscFeature as per test data labels

house_test$MiscFeature <- as.character(house_test$MiscFeature)



for ( i in 1:length(house_test$MiscFeature)){
  
  if(house_test$MiscFeature[i] == "Gar2"|| house_test$MiscFeature[i] == "TenC")
  {
    
    house_test$MiscFeature[i] = "None"
    
    
    
  }
  
  if(house_test$MiscFeature[i] == "Othr" )
  {
    
    house_test$MiscFeature[i] = "Shed"
    
    
    
  }
  
  
}
house_test$MiscFeature <- as.factor(house_test$MiscFeature)




##Recombining categories for better prediction

##for MSSubclass 30 and 45 and 180
## 40 and 50
## 190 and 90
##75 and 80 and 85


house_test$MSSubClass <- as.character(house_test$MSSubClass)
for ( i in 1:length(house_test$MSSubClass)){
  
  if(house_test$MSSubClass[i] == "30"||house_test$MSSubClass[i] == "45"
     ||house_test$MSSubClass[i] == "180")
  {
    
    house_test$MSSubClass[i] = "30/45/180"
    
    
    
  }
  
  if(house_test$MSSubClass[i] == "75"|| house_test$MSSubClass[i] == "80"
     ||house_test$MSSubClass[i] == "85" )
  {
    
    house_test$MSSubClass[i] = "75/80/85"
    
    
    
  }
  
  if(house_test$MSSubClass[i] == "40"||house_test$MSSubClass[i]== "50")
  {
    
    house_test$MSSubClass[i] = "40/50"
    
    
    
    
  } 
  
  if(house_test$MSSubClass[i] == "190"||house_test$MSSubClass[i]== "90")
  {
    
    house_test$MSSubClass[i] = "90/190"
    
    
    
    
  } 
  
}
house_test$MSSubClass <- as.factor(house_test$MSSubClass)


##
## changing MSZoning as per test data labels

house_test$MSZoning <- as.character(house_test$MSZoning)



for ( i in 1:length(house_test$MSZoning)){
  
  
  
  if(house_test$MSZoning[i] == "Res. high density"||house_test$MSZoning[i] == "Res. med density" )
  {
    
    house_test$MSZoning[i] = "Res. high/med"
    
    
    
  }
  
  
}
house_test$MSZoning <- as.factor(house_test$MSZoning)



##
## changing LotShape as per test data labels

house_test$LotShape <- as.character(house_test$LotShape)



for ( i in 1:length(house_test$LotShape)){
  
  
  
  if(house_test$LotShape[i] == "Irregular(slight)"||house_test$LotShape[i] == "Irregular(Moder)"
     || house_test$LotShape[i] =="Irregular")
  {
    
    house_test$LotShape[i] = "Irregular"
    
    
    
  }
  
  
}
house_test$LotShape <- as.factor(house_test$LotShape)



##
## changing LandContour as per test data labels

house_test$LandContour <- as.character(house_test$LandContour)



for ( i in 1:length(house_test$LandContour)){
  
  
  
  if(house_test$LandContour[i] == "HillSide"||house_test$LandContour[i] == "Low")
  {
    
    house_test$LandContour[i] = "HillSide/Low"
    
    
    
  }
  
  
}
house_test$LandContour <- as.factor(house_test$LandContour)





##
## changing LotConfig as per test data labels

house_test$LotConfig <- as.character(house_test$LotConfig)



for ( i in 1:length(house_test$LotConfig)){
  
  
  
  if(house_test$LotConfig[i] == "CulDSac"||house_test$LotConfig[i] == "Frontage(3 sides)")
  {
    
    house_test$LotConfig[i] = "CulDSac/Frontage3Sides"
    
    
    
  }
  
  
}
house_test$LotConfig <- as.factor(house_test$LotConfig)





##
## changing LandSlope as per test data labels

house_test$LandSlope <- as.character(house_test$LandSlope)



for ( i in 1:length(house_test$LandSlope)){
  
  
  
  if(house_test$LandSlope[i] == "Moderate"||house_test$LandSlope[i] == "Severe")
  {
    
    house_test$LandSlope[i] = "Moderate/Severe"
    
    
    
  }
  
  
}
house_test$LandSlope <- as.factor(house_test$LandSlope)



## changing Condition1 as per test data labels


house_test$Condition1 <- as.character(house_test$Condition1)



for ( i in 1:length(house_test$Condition1)){
  
  if(house_test$Condition1[i] == "RRAe"||house_test$Condition1[i] == "RRAn"
     ||house_test$Condition1[i] == "RRNe"||house_test$Condition1[i] == "RRNn")
  {
    
    house_test$Condition1[i] = "RRA/RRN"
    
    
    
  }
  
  if(house_test$Condition1[i] == "PosA"|| house_test$Condition1[i] == "PosN")
  {
    
    house_test$Condition1[i] = "PosA/PosN"
    
    
    
  }
  
  
  
}
house_test$Condition1 <- as.factor(house_test$Condition1)




## changing BldgType as per test data labels

house_test$BldgType <- as.character(house_test$BldgType)



for ( i in 1:length(house_test$BldgType)){
  
  
  
  if(house_test$BldgType[i] == "2fmCon"||house_test$BldgType[i] == "Duplex")
  {
    
    house_test$BldgType[i] = "2fmCon/Duplex"
    
    
    
  }
  
  
}
house_test$BldgType <- as.factor(house_test$BldgType)


## changing HouseStyle as per test data labels

house_test$HouseStyle <- as.character(house_test$HouseStyle)



for ( i in 1:length(house_test$HouseStyle)){
  
  
  
  if(house_test$HouseStyle[i] == "1.5Unf"||house_test$HouseStyle[i] == "2.5Unf"
     ||house_test$HouseStyle[i] == "SFoyer"||house_test$HouseStyle[i] == "SLvl")
  {
    
    house_test$HouseStyle[i] = "Others"
    
    
    
  }
  
  
}
house_test$HouseStyle <- as.factor(house_test$HouseStyle)




## changing Exterior1st as per test data labels

house_test$Exterior1st <- as.character(house_test$Exterior1st)



for ( i in 1:length(house_test$Exterior1st)){
  
  
  
  if(house_test$Exterior1st[i] == "AsbShng"||house_test$Exterior1st[i] == "CBlock"
  )
  {
    
    house_test$Exterior1st[i] = "AsbShng/CBlock"
    
    
    
  }
  
  if(house_test$Exterior1st[i] == "WdShing"||house_test$Exterior1st[i] == "Wd Sdng"
  )
  {
    
    house_test$Exterior1st[i] = "WdShing/Wd Sdng"
    
    
    
  }
  
  
}
house_test$Exterior1st <- as.factor(house_test$Exterior1st)


## changing Exterior2nd as per test data labels

house_test$Exterior2nd <- as.character(house_test$Exterior2nd)



for ( i in 1:length(house_test$Exterior2nd)){
  
  
  
  if(house_test$Exterior2nd[i] == "Stucco"||house_test$Exterior2nd[i] == "Plywood"
  )
  {
    
    house_test$Exterior2nd[i] = "Stucco/Plywood"
    
    
    
  }
  
  if(house_test$Exterior2nd[i] == "BrkFace"||house_test$Exterior2nd[i] == "CmentBd"
  )
  {
    
    house_test$Exterior2nd[i] = "BrkFace/CmentBd"
    
    
    
  }
  
  if(house_test$Exterior2nd[i] == "WdShing"||house_test$Exterior2nd[i] == "Wd Shng"
     ||house_test$Exterior2nd[i] == "AsbShng")
  {
    
    house_test$Exterior2nd[i] = "WdShing/Wd Sdng/AsbShng"
    
    
    
  }
  
  
}
house_test$Exterior2nd <- as.factor(house_test$Exterior2nd)






## changing MasVnrType as per test data labels

house_test$MasVnrType <- as.character(house_test$MasVnrType)



for ( i in 1:length(house_test$MasVnrType)){
  
  
  
  if(house_test$MasVnrType[i] == "None"||house_test$MasVnrType[i] == "BrkCmn")
  {
    
    house_test$MasVnrType[i] = "None/BrkCmn"
    
    
    
  }
  
  
}
house_test$MasVnrType <- as.factor(house_test$MasVnrType)




## changing ExterCond as per test data labels

house_test$ExterCond <- as.character(house_test$ExterCond)



for ( i in 1:length(house_test$ExterCond)){
  
  
  
  if(house_test$ExterCond[i] == "Excellent"||house_test$ExterCond[i] == "Fair/Typical/Poor")
  {
    
    house_test$ExterCond[i] = "Others"
    
    
    
  }
  
  
}
house_test$ExterCond <- as.factor(house_test$ExterCond)
