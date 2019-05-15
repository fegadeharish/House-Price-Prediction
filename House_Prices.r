setwd("D:/R Examples/Hackathon")

a <- read.csv("train.csv")
View(a)
b <- read.csv("test.csv")
View(b)

c <- rbind(a[,-81],b)
View(c)

summary(c)
str(c)

colnames(c)[colSums(is.na(c))>0]

c$MSZoning[is.na(c$MSZoning)] <- "RL"
hist(c$LotFrontage)
c$LotFrontage[is.na(c$LotFrontage)] <- 68
c$Exterior1st[is.na(c$Exterior1st)] <- "VinylSd"
c$Exterior2nd[is.na(c$Exterior2nd)] <- "VinylSd"
c$MasVnrType[is.na(c$MasVnrType)] <- "None"
c$MasVnrArea[is.na(c$MasVnrArea)] <- 0
levels(c$BsmtQual) <- c("Ex", "Fa", "Gd", "TA", "No Basement")
c$BsmtQual[is.na(c$BsmtQual)] <- "No Basement"
levels(c$BsmtExposure) <- c("Av", "Gd", "Mn", "No", "No Basement")
c$BsmtExposure[is.na(c$BsmtExposure)] <- "No Basement"
levels(c$BsmtFinType1) <- c("ALQ", "BLQ", "GLQ", "LwQ", "Rec", "Unf", "No Basement")
c$BsmtFinType1[is.na(c$BsmtFinType1)] <- "No Basement"
hist(c$BsmtFinSF1)
c$BsmtFinSF1[is.na(c$BsmtFinSF1)] <- 368
hist(c$BsmtUnfSF)
c$BsmtUnfSF[is.na(c$BsmtUnfSF)] <- 467
c$TotalBsmtSF[is.na(c$TotalBsmtSF)] <- 990
hist(c$TotalBsmtSF)
c$BsmtFullBath <- as.factor(c$BsmtFullBath)
c$BsmtFullBath[is.na(c$BsmtFullBath)] <- 0
c$KitchenQual[is.na(c$KitchenQual)] <- "TA"
levels(c$FireplaceQu) <- c("Ex", "Fa", "Gd", "Po", "TA", "No Fireplace")
c$FireplaceQu[is.na(c$FireplaceQu)] <- "No Fireplace"
levels(c$GarageType) <- c("2Types", "Attchd",  "Basment", "BuiltIn", "CarPort", "Detchd", "No Garage")
c$GarageType[is.na(c$GarageType)] <- "No Garage"
levels(c$GarageYrBlt)
c$GarageYrBlt <- as.factor(c$GarageYrBlt)
c$GarageYrBlt[is.na(c$GarageYrBlt)] <- "2005"
levels(c$GarageFinish) <- c("Fin", "RFn", "Unf", "No Garage")
c$GarageFinish[is.na(c$GarageFinish)] <- "No Garage"
c$GarageCars <- as.factor(c$GarageCars)
c$GarageCars[is.na(c$GarageCars)] <- 2
hist(c$GarageArea)
c$GarageArea[is.na(c$GarageArea)] <- 473
levels(c$Fence) <- c("GdPrv", "GdWo",  "MnPrv", "MnWw", "No Fence")
c$Fence[is.na(c$Fence)] <- "No Fence"

c$GarageYrBlt <- as.numeric(c$GarageYrBlt)


summary(c)

c$GarageCars <- as.numeric(c$GarageCars)


aa <- subset(c, c$Id<1461)
aa <- aa[,c(-6,-7,-73,-72,-71,-70,-42,-40,-43,-56,-64,-65,-66,-75,-10,-13,-14,-32,-23,-9,-12,-29,-36,-37,-79,-49,-49)]
aa <- cbind(aa,a$SalePrice)
View(aa)


bb <- subset(c, c$Id>1460)
bb <- bb[,c(-6,-7,-73,-72,-71,-70,-42,-40,-43,-56,-64,-65,-66,-75,-10,-13,-14,-32,-23,-9,-12,-29,-36,-37,-79,-49,-49)]
View(bb)

colnames(aa)[55] <- "SalePrice"

model1 <- lm(SalePrice~.-Exterior2nd-BsmtFinType1-GarageFinish-GrLivArea, data = aa)
summary(model1)

px <- predict(model1, bb)

View(px)

write.csv(px, file = "sample prediction.csv")
?write.csv
