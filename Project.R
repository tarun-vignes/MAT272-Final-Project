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
