##################################################
## MAT 272 FINAL PROJECT
## Section 2 - Graphical Display and Summary Statistics
##################################################

## Load packages
library(mosaic)
library(lessR)

##################################################
## Import the ORIGINAL dataset
##################################################

D <- read.csv("EPA_Gasoline_FWD_RWD_Sample500.csv")

## Check the dataset
head(D)
dim(D)
names(D)

##################################################
## Create categorical variables for the project
## Original CSV is NOT changed
##################################################

## Transmission: Manual = 0, Automatic = 1
D$TransmissionType <- ifelse(grepl("^Manual", D$Transmission), "Manual",
                             ifelse(grepl("^Automatic", D$Transmission), "Automatic", NA))

D$TransmissionCode <- ifelse(D$TransmissionType == "Manual", 0,
                             ifelse(D$TransmissionType == "Automatic", 1, NA))

## Fuel Type: Regular = 0, Premium = 1
## Midgrade is kept in the full dataset but coded NA for this comparison
D$FuelCode <- ifelse(D$FuelGrade == "Regular", 0,
                     ifelse(D$FuelGrade == "Premium", 1, NA))

## Separate dataset only for Regular vs Premium analysis
D_fuel <- D[D$FuelGrade %in% c("Regular", "Premium"), ]

##################################################
## Verify the data
##################################################

dim(D)

sum(duplicated(D$VehicleID))

colSums(is.na(D[c("CombinedMPG",
                  "Displacement_L",
                  "DriveType",
                  "TransmissionType",
                  "FuelGrade")]))

table(D$DriveType)

table(D$TransmissionType)

table(D$TransmissionCode)

table(D$FuelGrade)

table(D$FuelCode, useNA = "ifany")

nrow(D_fuel)

##################################################
## QUANTITATIVE VARIABLE 1
## Combined MPG
##################################################

## Summary statistics
mean(D$CombinedMPG)

sd(D$CombinedMPG)

summary(D$CombinedMPG)

##################################################
## Dot Plot - Combined MPG
##################################################

dev.new(width = 7, height = 5)

dotPlot(
  D$CombinedMPG,
  n = 20,
  xlab = "Combined MPG",
  ylab = "Frequency",
  main = "Distribution of Combined MPG"
)

##################################################
## Histogram - Combined MPG
##################################################

dev.new(width = 7, height = 5)

hist(
  D$CombinedMPG,
  breaks = 10,
  main = "Histogram of Combined MPG",
  xlab = "Combined MPG",
  ylab = "Frequency",
  col = "lightblue"
)

##################################################
## Boxplot - Combined MPG
##################################################

dev.new(width = 7, height = 5)

boxplot(
  D$CombinedMPG,
  horizontal = TRUE,
  main = "Boxplot of Combined MPG",
  xlab = "Combined MPG",
  col = "lightblue"
)

## Check boxplot outliers
boxplot.stats(D$CombinedMPG)$out

##################################################
## QUANTITATIVE VARIABLE 2
## Engine Displacement
##################################################

## Summary statistics
mean(D$Displacement_L)

sd(D$Displacement_L)

summary(D$Displacement_L)

##################################################
## Dot Plot - Engine Displacement
##################################################

dev.new(width = 7, height = 5)

dotPlot(
  D$Displacement_L,
  n = 20,
  xlab = "Engine Displacement (Liters)",
  ylab = "Frequency",
  main = "Distribution of Engine Displacement"
)

##################################################
## Histogram - Engine Displacement
##################################################

dev.new(width = 7, height = 5)

hist(
  D$Displacement_L,
  breaks = 12,
  main = "Histogram of Engine Displacement",
  xlab = "Engine Displacement (Liters)",
  ylab = "Frequency",
  col = "lightblue"
)

##################################################
## Boxplot - Engine Displacement
##################################################

dev.new(width = 7, height = 5)

boxplot(
  D$Displacement_L,
  horizontal = TRUE,
  main = "Boxplot of Engine Displacement",
  xlab = "Engine Displacement (Liters)",
  col = "lightblue"
)

## Check boxplot outliers
boxplot.stats(D$Displacement_L)$out


##################################################
## CATEGORICAL VARIABLE 1
## Drive Type
##################################################

## Frequency table
drive_count <- table(D$DriveType)
drive_count

## Percentage table
drive_percent <- prop.table(drive_count) * 100
drive_percent <- round(drive_percent, 2)
drive_percent

## Combined table
drive_table <- cbind(
  Frequency = drive_count,
  Percentage = drive_percent
)
drive_table

##################################################
## Bar Chart - Drive Type
##################################################

dev.new(width = 7, height = 5)

barplot(
  drive_percent,
  main = "Drive Type of Vehicles",
  xlab = "Drive Type",
  ylab = "Percentage",
  ylim = c(0, 60),
  col = "lightblue"
)

##################################################
## Pie Chart - Drive Type
##################################################

drive_labels <- paste(
  names(drive_percent),
  drive_percent,
  "%",
  sep = " "
)

dev.new(width = 7, height = 5)

pie(
  drive_count,
  labels = drive_labels,
  main = "Drive Type of Vehicles"
)


##################################################
## CATEGORICAL VARIABLE 2
## Transmission Type
##################################################

## Frequency table
trans_count <- table(D$TransmissionType)
trans_count

## Percentage table
trans_percent <- prop.table(trans_count) * 100
trans_percent <- round(trans_percent, 2)
trans_percent

## Combined table
trans_table <- cbind(
  Frequency = trans_count,
  Percentage = trans_percent
)
trans_table

##################################################
## Bar Chart - Transmission Type
##################################################

dev.new(width = 7, height = 5)

barplot(
  trans_percent,
  main = "Transmission Type of Vehicles",
  xlab = "Transmission Type",
  ylab = "Percentage",
  ylim = c(0, 100),
  col = "lightblue"
)

##################################################
## Pie Chart - Transmission Type
##################################################

trans_labels <- paste(
  names(trans_percent),
  trans_percent,
  "%",
  sep = " "
)

dev.new(width = 7, height = 5)

pie(
  trans_count,
  labels = trans_labels,
  main = "Transmission Type of Vehicles"
)

##################################################
## CATEGORICAL VARIABLE 3
## Fuel Type
##################################################

## Frequency table
fuel_count <- table(D_fuel$FuelGrade)
fuel_count

## Percentage table
fuel_percent <- prop.table(fuel_count) * 100
fuel_percent <- round(fuel_percent, 2)
fuel_percent

## Combined table
fuel_table <- cbind(
  Frequency = fuel_count,
  Percentage = fuel_percent
)
fuel_table

##################################################
## Bar Chart - Fuel Type
##################################################

dev.new(width = 7, height = 5)

barplot(
  fuel_percent,
  main = "Fuel Type of Vehicles",
  xlab = "Fuel Type",
  ylab = "Percentage",
  ylim = c(0, 60),
  col = "lightblue"
)

##################################################
## Pie Chart - Fuel Type
##################################################

fuel_labels <- paste(
  names(fuel_percent),
  fuel_percent,
  "%",
  sep = " "
)

dev.new(width = 7, height = 5)

pie(
  fuel_count,
  labels = fuel_labels,
  main = "Fuel Type of Vehicles"
)


##################################################
## RELATIONSHIP BETWEEN CATEGORICAL AND QUANTITATIVE
## Combined MPG by Drive Type
##################################################

## Sample size by Drive Type
table(D$DriveType)

## Mean Combined MPG by Drive Type
tapply(D$CombinedMPG, D$DriveType, mean)

## Standard deviation by Drive Type
tapply(D$CombinedMPG, D$DriveType, sd)

## Five-number summary by Drive Type
tapply(D$CombinedMPG, D$DriveType, summary)

##################################################
## Side-by-Side Boxplot
## Combined MPG by Drive Type
##################################################

dev.new(width = 7, height = 5)

boxplot(
  CombinedMPG ~ DriveType,
  data = D,
  main = "Combined MPG by Drive Type",
  xlab = "Drive Type",
  ylab = "Combined MPG",
  col = "lightblue"
)

##################################################
## Dot Plot
## Combined MPG by Drive Type
##################################################

dev.new(width = 7, height = 5)

stripchart(
  CombinedMPG ~ DriveType,
  data = D,
  method = "jitter",
  vertical = TRUE,
  pch = 19,
  cex = 0.6,
  main = "Combined MPG by Drive Type",
  xlab = "Drive Type",
  ylab = "Combined MPG"
)

##################################################
## Histograms
## Combined MPG by Drive Type
##################################################

dev.new(width = 9, height = 5)

par(mfrow = c(1, 2))

hist(
  D$CombinedMPG[D$DriveType == "FWD"],
  breaks = 10,
  main = "Combined MPG - FWD",
  xlab = "Combined MPG",
  ylab = "Frequency",
  col = "lightblue",
  xlim = c(10, 40)
)

hist(
  D$CombinedMPG[D$DriveType == "RWD"],
  breaks = 10,
  main = "Combined MPG - RWD",
  xlab = "Combined MPG",
  ylab = "Frequency",
  col = "lightblue",
  xlim = c(10, 40)
)

## Reset graph layout
par(mfrow = c(1, 1))



##################################################
## RELATIONSHIP BETWEEN CATEGORICAL AND QUANTITATIVE
## Combined MPG by Transmission Type
##################################################

## Sample size by Transmission Type
table(D$TransmissionType)

## Mean Combined MPG by Transmission Type
tapply(D$CombinedMPG, D$TransmissionType, mean)

## Standard deviation by Transmission Type
tapply(D$CombinedMPG, D$TransmissionType, sd)

## Five-number summary by Transmission Type
tapply(D$CombinedMPG, D$TransmissionType, summary)

##################################################
## Side-by-Side Boxplot
## Combined MPG by Transmission Type
##################################################

dev.new(width = 7, height = 5)

boxplot(
  CombinedMPG ~ TransmissionType,
  data = D,
  main = "Combined MPG by Transmission Type",
  xlab = "Transmission Type",
  ylab = "Combined MPG",
  col = "lightblue"
)

##################################################
## Dot Plot
## Combined MPG by Transmission Type
##################################################

dev.new(width = 7, height = 5)

stripchart(
  CombinedMPG ~ TransmissionType,
  data = D,
  method = "jitter",
  vertical = TRUE,
  pch = 19,
  cex = 0.6,
  main = "Combined MPG by Transmission Type",
  xlab = "Transmission Type",
  ylab = "Combined MPG"
)

##################################################
## Histograms
## Combined MPG by Transmission Type
##################################################

dev.new(width = 9, height = 5)

par(mfrow = c(1, 2))

hist(
  D$CombinedMPG[D$TransmissionType == "Automatic"],
  breaks = 10,
  main = "Combined MPG - Automatic",
  xlab = "Combined MPG",
  ylab = "Frequency",
  col = "lightblue",
  xlim = c(10, 40)
)

hist(
  D$CombinedMPG[D$TransmissionType == "Manual"],
  breaks = 10,
  main = "Combined MPG - Manual",
  xlab = "Combined MPG",
  ylab = "Frequency",
  col = "lightblue",
  xlim = c(10, 40)
)

## Reset graph layout
par(mfrow = c(1, 1))



##################################################
## RELATIONSHIP BETWEEN CATEGORICAL AND QUANTITATIVE
## Combined MPG by Fuel Type
## Regular vs Premium only
##################################################

## Sample size by Fuel Type
table(D_fuel$FuelGrade)

## Mean Combined MPG by Fuel Type
tapply(D_fuel$CombinedMPG, D_fuel$FuelGrade, mean)

## Standard deviation by Fuel Type
tapply(D_fuel$CombinedMPG, D_fuel$FuelGrade, sd)

## Five-number summary by Fuel Type
tapply(D_fuel$CombinedMPG, D_fuel$FuelGrade, summary)

##################################################
## Side-by-Side Boxplot
## Combined MPG by Fuel Type
##################################################

dev.new(width = 7, height = 5)

boxplot(
  CombinedMPG ~ FuelGrade,
  data = D_fuel,
  main = "Combined MPG by Fuel Type",
  xlab = "Fuel Type",
  ylab = "Combined MPG",
  col = "lightblue"
)

##################################################
## Dot Plot
## Combined MPG by Fuel Type
##################################################

dev.new(width = 7, height = 5)

stripchart(
  CombinedMPG ~ FuelGrade,
  data = D_fuel,
  method = "jitter",
  vertical = TRUE,
  pch = 19,
  cex = 0.6,
  main = "Combined MPG by Fuel Type",
  xlab = "Fuel Type",
  ylab = "Combined MPG"
)

##################################################
## Histograms
## Combined MPG by Fuel Type
##################################################

dev.new(width = 9, height = 5)

par(mfrow = c(1, 2))

hist(
  D_fuel$CombinedMPG[D_fuel$FuelGrade == "Regular"],
  breaks = 10,
  main = "Combined MPG - Regular",
  xlab = "Combined MPG",
  ylab = "Frequency",
  col = "lightblue",
  xlim = c(10, 40)
)

hist(
  D_fuel$CombinedMPG[D_fuel$FuelGrade == "Premium"],
  breaks = 10,
  main = "Combined MPG - Premium",
  xlab = "Combined MPG",
  ylab = "Frequency",
  col = "lightblue",
  xlim = c(10, 40)
)

## Reset graph layout
par(mfrow = c(1, 1))




##################################################
## SECTION 3 - STATISTICAL ANALYSIS AND INFERENCE
##################################################


##################################################
## 3.1 Classical 95% CI for Mean Combined MPG
##################################################

stats::t.test(D$CombinedMPG, conf.level = .95)


##################################################
## 3.2 FWD vs RWD Mean Combined MPG
##################################################

FWD <- D$CombinedMPG[D$DriveType == "FWD"]
RWD <- D$CombinedMPG[D$DriveType == "RWD"]

mean(FWD)
mean(RWD)

stats::t.test(FWD, RWD, alternative = "two.sided", conf.level = .95)


##################################################
## 3.3 Bootstrap 95% CI for Mean Combined MPG
##################################################

data <- D$CombinedMPG
B <- 10000
BootMean <- numeric(B)

for(i in 1:B){
  bootsample <- sample(data, length(data), replace = TRUE)
  BootMean[i] <- mean(bootsample)
}

quantile(BootMean, c(.025, .975))

windows(width = 10, height = 7)

hist(BootMean,
     main = "Bootstrap Distribution of Mean Combined MPG",
     xlab = "Mean Combined MPG",
     ylab = "Frequency",
     breaks = 20,
     cex.main = 1.4,
     cex.lab = 1.3,
     cex.axis = 1.2)


##################################################
## 3.4 Bootstrap 95% CI for Difference in Means
## FWD - RWD
##################################################

sample1 <- RWD
sample2 <- FWD

B <- 10000
BootMeanDiff <- numeric(B)

for(i in 1:B){
  bootsample1 <- sample(sample1, length(sample1), replace = TRUE)
  bootsample2 <- sample(sample2, length(sample2), replace = TRUE)
  meanS1 <- mean(bootsample1)
  meanS2 <- mean(bootsample2)
  BootMeanDiff[i] <- meanS2 - meanS1
}

quantile(BootMeanDiff, c(.025, .975))

windows(width = 10, height = 7)

hist(BootMeanDiff,
     main = "Bootstrap Distribution of FWD - RWD Mean MPG",
     xlab = "Difference in Mean Combined MPG",
     ylab = "Frequency",
     breaks = 20,
     cex.main = 1.4,
     cex.lab = 1.3,
     cex.axis = 1.2)
