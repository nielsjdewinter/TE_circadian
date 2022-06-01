# Script to plot the results of variance decomposition (Figure 6)

require(tidyverse)
require(RColorBrewer)
require(gridExtra)
require(signal)

# Load and trim data (following previously established cutoffs)
load("<path>/Shell2_1_dated_aligned_cross.rda") # Load Shell2_1 data
dat2_1 <- dat_trim # Rename to avoid conflict
dat2_1_trim <- dat2_1[which(dat2_1$Depth > 7000), ]

load("<path>/Shell2_2_dated_cross.rda") # Load Shell2_2 data
dat2_2 <- dat_trim # Rename to avoid conflict
dat2_2_trim <- dat2_2[which(dat2_2$Depth > 3500), ]

load("<path>/Shell3_1_dated_aligned_cross.rda") # Load Shell3_1 data
dat3_1 <- dat_trim # Rename to avoid conflict
dat3_1_trim <- dat3_1[which(dat3_1$Depth > 1000), ]

load("<path>/Shell3_2_dated_cross.rda") # Load Shell3_2 data
dat3_2 <- dat_trim # Rename to avoid conflict
dat3_2_trim <- dat3_2[which(dat3_2$Depth > 4500), ]

load("<path>/Shell4_dated_cross.rda") # Load Shell4 data
dat4 <- dat_trim # Rename to avoid conflict
dat4_trim <- dat4[which(dat4$Depth > 2000), ]

maxima29 <- read.csv("<path>/Line_Tridacnid_NdW_29_recalc.csv")
maxima29_trim <- maxima29[which((maxima29$depth < 52) & (maxima29$depth > 0.04)), ]
maxima29_trim <- maxima29_trim[-which((maxima29_trim$depth < 33) & (maxima29_trim$depth > 31.5)), ]

maxima84 <- read.csv("<path>/Line_Tridacnid_NdW_84_recalc.csv")
maxima84_trim <- maxima84[which((maxima84$depth < 37) & (maxima84$depth > 0.01)), ]
maxima84_trim <- maxima84_trim[-which((maxima84_trim$depth < 4.05) & (maxima84_trim$depth > 3.8)), ]
maxima84_trim <- maxima84_trim[-which((maxima84_trim$depth < 4.7) & (maxima84_trim$depth > 4.5)), ]
maxima84_trim <- maxima84_trim[-which((maxima84_trim$depth < 6.3) & (maxima84_trim$depth > 6.05)), ]
maxima84_trim <- maxima84_trim[-which((maxima84_trim$depth < 17.3) & (maxima84_trim$depth > 17)), ]
maxima84_trim <- maxima84_trim[-which((maxima84_trim$depth < 29.5) & (maxima84_trim$depth > 29)), ]

squamosa85 <- read.csv("<path>/Line_Tridacnid_NdW_85_recalc.csv")
squamosa85_trim <- squamosa85[which((squamosa85$depth < 39.7) & (squamosa85$depth > 0.6)), ]

squamosaFRS1 <- read.csv("<path>/Line_Tridacnid_NdW_FRS1_recalc.csv")
squamosaFRS1_trim <- squamosaFRS1[which((squamosaFRS1$depth < 32.8) & (squamosaFRS1$depth > 0.04)), ]

squamosaM1 <- read.csv("<path>/Line_Tridacnid_NdW_M1_recalc.csv")
squamosaM1_trim <- squamosaM1[which((squamosaM1$depth < 42.35) & (squamosaM1$depth > 0.015) & (squamosaM1$X25Mg.43Ca < 25) & (squamosaM1$X137Ba.43Ca < 0.01)), ]
squamosaM1_trim <- squamosaM1_trim[-which((squamosaM1_trim$depth < 3.15) & (squamosaM1_trim$depth > 2.8)), ]
squamosaM1_trim <- squamosaM1_trim[-which((squamosaM1_trim$depth < 4) & (squamosaM1_trim$depth > 3.65)), ]

squamosinaSQSA1 <- read.csv("<path>/Line_Tridacnid_NdW_SQSA1_recalc.csv")
squamosinaSQSA1_trim <- squamosinaSQSA1[which((squamosinaSQSA1$depth < 38.75) & (squamosinaSQSA1$depth > 0.015) & (squamosinaSQSA1$X25Mg.43Ca < 10) & (squamosinaSQSA1$X137Ba.43Ca < 0.01)), ]
squamosinaSQSA1_trim <- squamosinaSQSA1_trim[-which((squamosinaSQSA1_trim$depth < 4.05) & (squamosinaSQSA1_trim$depth > 3.8)), ]
squamosinaSQSA1_trim <- squamosinaSQSA1_trim[-which((squamosinaSQSA1_trim$depth < 26) & (squamosinaSQSA1_trim$depth > 25.7)), ]
squamosinaSQSA1_trim <- squamosinaSQSA1_trim[-which((squamosinaSQSA1_trim$depth < 25.2) & (squamosinaSQSA1_trim$depth > 24.5)), ]

# Rename columns for easier reference
colnames(maxima29_trim)[c(3:8, 10)] <- colnames(maxima84_trim)[c(3:8, 10)] <- colnames(squamosa85_trim)[c(3:8, 10)] <- colnames(squamosaFRS1_trim)[c(3:8, 10)] <- colnames(squamosaM1_trim)[c(3:8, 10)] <- colnames(squamosinaSQSA1_trim)[c(3:8, 10)] <- c("MgCa", "CaCa", "MnCa", "Sr87Ca", "SrCa", "BaCa", "Day")
colnames(dat2_1_trim)[2:6] <- colnames(dat2_2_trim)[2:6] <- colnames(dat3_1_trim)[2:6] <- colnames(dat3_2_trim)[2:6] <- colnames(dat4_trim)[2:6] <- c("MgCa", "MnCa", "Sr87Ca", "SrCa", "BaCa")
colnames(maxima29)[c(3:8, 10)] <- colnames(maxima84)[c(3:8, 10)] <- colnames(squamosa85)[c(3:8, 10)] <- colnames(squamosaFRS1)[c(3:8, 10)] <- colnames(squamosaM1)[c(3:8, 10)] <- colnames(squamosinaSQSA1)[c(3:8, 10)] <- c("MgCa", "CaCa", "MnCa", "Sr87Ca", "SrCa", "BaCa", "Day")
colnames(dat2_1)[2:6] <- colnames(dat2_2)[2:6] <- colnames(dat3_1)[2:6] <- colnames(dat3_2)[2:6] <- colnames(dat4)[2:6] <- c("MgCa", "MnCa", "Sr87Ca", "SrCa", "BaCa")

# Add specimen columns
dat2_1$Specimen <- "P. maximus 2_1"
dat2_2$Specimen <- "P. maximus 2_2"
dat3_1$Specimen <- "P. maximus 3_1"
dat3_2$Specimen <- "P. maximus 3_2"
dat4$Specimen <- "P. maximus 4"
maxima29$Specimen <- "T. maxima 29"
maxima84$Specimen <- "T. maxima 84"
squamosa85$Specimen <- "T. squamosa 85"
squamosaFRS1$Specimen <- "T. squamosa FRS1"
squamosaM1$Specimen <- "T. squamosa M1"
squamosinaSQSA1$Specimen <- "T. squamosina SQSA1"

dat2_1_trim$Specimen <- "P. maximus 2_1 trim"
dat2_2_trim$Specimen <- "P. maximus 2_2 trim"
dat3_1_trim$Specimen <- "P. maximus 3_1 trim"
dat3_2_trim$Specimen <- "P. maximus 3_2 trim"
dat4_trim$Specimen <- "P. maximus 4 trim"
maxima29_trim$Specimen <- "T. maxima 29 trim"
maxima84_trim$Specimen <- "T. maxima 84 trim"
squamosa85_trim$Specimen <- "T. squamosa 85 trim"
squamosaFRS1_trim$Specimen <- "T. squamosa FRS1 trim"
squamosaM1_trim$Specimen <- "T. squamosa M1 trim"
squamosinaSQSA1_trim$Specimen <- "T. squamosina SQSA1 trim"

# Filtering

# Apply Savitzky-Golay filters
dat2_1_trim2 <- dat2_1_trim
dat2_1_trim2$BaCa[dat2_1_trim2$BaCa == 0] <- mean(dat2_1_trim2$BaCa[dat2_1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

dat2_1_filtered <- as.data.frame(cbind(dat2_1_trim2$Depth,
    dat2_1_trim2$Day,
    apply(dat2_1_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(dat2_1_filtered)[c(1, 2)] <- c("Depth", "Day")
dat2_1_filtered$datetime <- dat2_1_trim2$datetime

dat2_2_trim2 <- dat2_2_trim
dat2_2_trim2$BaCa[dat2_2_trim2$BaCa == 0] <- mean(dat2_2_trim2$BaCa[dat2_2_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

dat2_2_filtered <- as.data.frame(cbind(dat2_2_trim2$Depth,
    dat2_2_trim2$Day,
    apply(dat2_2_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(dat2_2_filtered)[c(1, 2)] <- c("Depth", "Day")
dat2_2_filtered$datetime <- dat2_2_trim2$datetime

dat3_1_trim2 <- dat3_1_trim
dat3_1_trim2$BaCa[dat3_1_trim2$BaCa == 0] <- mean(dat3_1_trim2$BaCa[dat3_1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

dat3_1_filtered <- as.data.frame(cbind(dat3_1_trim2$Depth,
    dat3_1_trim2$Day,
    apply(dat3_1_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(dat3_1_filtered)[c(1, 2)] <- c("Depth", "Day")
dat3_1_filtered$datetime <- dat3_1_trim2$datetime

dat3_2_trim2 <- dat3_2_trim
dat3_2_trim2$BaCa[dat3_2_trim2$BaCa == 0] <- mean(dat3_2_trim2$BaCa[dat3_2_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

dat3_2_filtered <- as.data.frame(cbind(dat3_2_trim2$Depth,
    dat3_2_trim2$Day,
    apply(dat3_2_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(dat3_2_filtered)[c(1, 2)] <- c("Depth", "Day")
dat3_2_filtered$datetime <- dat3_2_trim2$datetime

dat4_trim2 <- dat4_trim
dat4_trim2$BaCa[dat4_trim2$BaCa == 0] <- mean(dat4_trim2$BaCa[dat4_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

dat4_filtered <- as.data.frame(cbind(dat4_trim2$Depth,
    dat4_trim2$Day,
    apply(dat4_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(dat4_filtered)[c(1, 2)] <- c("Depth", "Day")
dat4_filtered$datetime <- dat4_trim2$datetime

maxima29_trim2 <- maxima29_trim
maxima29_trim2$BaCa[maxima29_trim2$BaCa == 0] <- mean(maxima29_trim2$BaCa[maxima29_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
maxima29_trim2$SrCa[is.na(maxima29_trim2$SrCa)] <- mean(maxima29_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
maxima29_trim2$MnCa[is.na(maxima29_trim2$MnCa)] <- mean(maxima29_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
maxima29_trim2$MgCa[is.na(maxima29_trim2$MgCa)] <- mean(maxima29_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
maxima29_trim2$BaCa[is.na(maxima29_trim2$BaCa)] <- mean(maxima29_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

maxima29_filtered <- as.data.frame(cbind(maxima29_trim2$depth,
    maxima29_trim2$time,
    apply(maxima29_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(maxima29_filtered)[c(1, 2)] <- c("depth", "Day")

maxima84_trim2 <- maxima84_trim
maxima84_trim2$BaCa[maxima84_trim2$BaCa == 0] <- mean(maxima84_trim2$BaCa[maxima84_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
maxima84_trim2$SrCa[is.na(maxima84_trim2$SrCa)] <- mean(maxima84_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
maxima84_trim2$MnCa[is.na(maxima84_trim2$MnCa)] <- mean(maxima84_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
maxima84_trim2$MgCa[is.na(maxima84_trim2$MgCa)] <- mean(maxima84_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
maxima84_trim2$BaCa[is.na(maxima84_trim2$BaCa)] <- mean(maxima84_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

maxima84_filtered <- as.data.frame(cbind(maxima84_trim2$depth,
    maxima84_trim2$time,
    apply(maxima84_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(maxima84_filtered)[c(1, 2)] <- c("depth", "Day")

squamosa85_trim2 <- squamosa85_trim
squamosa85_trim2$BaCa[squamosa85_trim2$BaCa == 0] <- mean(squamosa85_trim2$BaCa[squamosa85_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
squamosa85_trim2$SrCa[is.na(squamosa85_trim2$SrCa)] <- mean(squamosa85_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
squamosa85_trim2$MnCa[is.na(squamosa85_trim2$MnCa)] <- mean(squamosa85_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
squamosa85_trim2$MgCa[is.na(squamosa85_trim2$MgCa)] <- mean(squamosa85_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
squamosa85_trim2$BaCa[is.na(squamosa85_trim2$BaCa)] <- mean(squamosa85_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

squamosa85_filtered <- as.data.frame(cbind(squamosa85_trim2$depth,
    squamosa85_trim2$time,
    apply(squamosa85_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(squamosa85_filtered)[c(1, 2)] <- c("depth", "Day")

squamosaFRS1_trim2 <- squamosaFRS1_trim
squamosaFRS1_trim2$BaCa[squamosaFRS1_trim2$BaCa == 0] <- mean(squamosaFRS1_trim2$BaCa[squamosaFRS1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
squamosaFRS1_trim2$SrCa[is.na(squamosaFRS1_trim2$SrCa)] <- mean(squamosaFRS1_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
squamosaFRS1_trim2$MnCa[is.na(squamosaFRS1_trim2$MnCa)] <- mean(squamosaFRS1_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
squamosaFRS1_trim2$MgCa[is.na(squamosaFRS1_trim2$MgCa)] <- mean(squamosaFRS1_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
squamosaFRS1_trim2$BaCa[is.na(squamosaFRS1_trim2$BaCa)] <- mean(squamosaFRS1_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

squamosaFRS1_filtered <- as.data.frame(cbind(squamosaFRS1_trim2$depth,
    squamosaFRS1_trim2$time,
    apply(squamosaFRS1_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(squamosaFRS1_filtered)[c(1, 2)] <- c("depth", "Day")

squamosaM1_trim2 <- squamosaM1_trim
squamosaM1_trim2$BaCa[squamosaM1_trim2$BaCa == 0] <- mean(squamosaM1_trim2$BaCa[squamosaM1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
squamosaM1_trim2$SrCa[is.na(squamosaM1_trim2$SrCa)] <- mean(squamosaM1_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
squamosaM1_trim2$MnCa[is.na(squamosaM1_trim2$MnCa)] <- mean(squamosaM1_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
squamosaM1_trim2$MgCa[is.na(squamosaM1_trim2$MgCa)] <- mean(squamosaM1_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
squamosaM1_trim2$BaCa[is.na(squamosaM1_trim2$BaCa)] <- mean(squamosaM1_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

squamosaM1_filtered <- as.data.frame(cbind(squamosaM1_trim2$depth,
    squamosaM1_trim2$time,
    apply(squamosaM1_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(squamosaM1_filtered)[c(1, 2)] <- c("depth", "Day")

squamosinaSQSA1_trim2 <- squamosinaSQSA1_trim
squamosinaSQSA1_trim2$BaCa[squamosinaSQSA1_trim2$BaCa == 0] <- mean(squamosinaSQSA1_trim2$BaCa[squamosinaSQSA1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
squamosinaSQSA1_trim2$SrCa[is.na(squamosinaSQSA1_trim2$SrCa)] <- mean(squamosinaSQSA1_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
squamosinaSQSA1_trim2$MnCa[is.na(squamosinaSQSA1_trim2$MnCa)] <- mean(squamosinaSQSA1_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
squamosinaSQSA1_trim2$MgCa[is.na(squamosinaSQSA1_trim2$MgCa)] <- mean(squamosinaSQSA1_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
squamosinaSQSA1_trim2$BaCa[is.na(squamosinaSQSA1_trim2$BaCa)] <- mean(squamosinaSQSA1_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

squamosinaSQSA1_filtered <- as.data.frame(cbind(squamosinaSQSA1_trim2$depth,
    squamosinaSQSA1_trim2$time,
    apply(squamosinaSQSA1_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(squamosinaSQSA1_filtered)[c(1, 2)] <- c("depth", "Day")

# Add specimen columns
dat2_1_filtered$Specimen <- "P. maximus 2_1 filtered"
dat2_2_filtered$Specimen <- "P. maximus 2_2 filtered"
dat3_1_filtered$Specimen <- "P. maximus 3_1 filtered"
dat3_2_filtered$Specimen <- "P. maximus 3_2 filtered"
dat4_filtered$Specimen <- "P. maximus 4 filtered"
maxima29_filtered$Specimen <- "T. maxima 29 filtered"
maxima84_filtered$Specimen <- "T. maxima 84 filtered"
squamosa85_filtered$Specimen <- "T. squamosa 85 filtered"
squamosaFRS1_filtered$Specimen <- "T. squamosa FRS1 filtered"
squamosaM1_filtered$Specimen <- "T. squamosa M1 filtered"
squamosinaSQSA1_filtered$Specimen <- "T. squamosina SQSA1 filtered"

# Smoothing

# Apply LOESS smoothing
# Loess span of 0.2 is equivalent to 50 days
dat2_1_loess02 <- data.frame(Depth = dat2_1_trim$Depth,
    Day = dat2_1_trim$Day,
    MgCa = predict(loess(dat2_1_trim$MgCa ~ dat2_1_trim$Day, span = 0.2)),
    MnCa = predict(loess(dat2_1_trim$MnCa ~ dat2_1_trim$Day, span = 0.2)),
    SrCa = predict(loess(dat2_1_trim$SrCa ~ dat2_1_trim$Day, span = 0.2)),
    BaCa = predict(loess(dat2_1_trim2$BaCa ~ dat2_1_trim2$Day, span = 0.2))
)
dat2_1_loess02$datetime <- dat2_1_trim$datetime

dat2_2_loess02 <- data.frame(Depth = dat2_2_trim$Depth,
    Day = dat2_2_trim$Day,
    MgCa = predict(loess(dat2_2_trim$MgCa ~ dat2_2_trim$Day, span = 0.2)),
    MnCa = predict(loess(dat2_2_trim$MnCa ~ dat2_2_trim$Day, span = 0.2)),
    SrCa = predict(loess(dat2_2_trim$SrCa ~ dat2_2_trim$Day, span = 0.2)),
    BaCa = predict(loess(dat2_2_trim2$BaCa ~ dat2_2_trim2$Day, span = 0.2))
)
dat2_2_loess02$datetime <- dat2_2_trim$datetime

dat3_1_loess02 <- data.frame(Depth = dat3_1_trim$Depth,
    Day = dat3_1_trim$Day,
    MgCa = predict(loess(dat3_1_trim$MgCa ~ dat3_1_trim$Day, span = 0.2)),
    MnCa = predict(loess(dat3_1_trim$MnCa ~ dat3_1_trim$Day, span = 0.2)),
    SrCa = predict(loess(dat3_1_trim$SrCa ~ dat3_1_trim$Day, span = 0.2)),
    BaCa = predict(loess(dat3_1_trim2$BaCa ~ dat3_1_trim2$Day, span = 0.2))
)
dat3_1_loess02$datetime <- dat3_1_trim$datetime

dat3_2_loess02 <- data.frame(Depth = dat3_2_trim$Depth,
    Day = dat3_2_trim$Day,
    MgCa = predict(loess(dat3_2_trim$MgCa ~ dat3_2_trim$Day, span = 0.2)),
    MnCa = predict(loess(dat3_2_trim$MnCa ~ dat3_2_trim$Day, span = 0.2)),
    SrCa = predict(loess(dat3_2_trim$SrCa ~ dat3_2_trim$Day, span = 0.2)),
    BaCa = predict(loess(dat3_2_trim2$BaCa ~ dat3_2_trim2$Day, span = 0.2))
)
dat3_2_loess02$datetime <- dat3_2_trim$datetime

dat4_loess02 <- data.frame(Depth = dat4_trim$Depth,
    Day = dat4_trim$Day,
    MgCa = predict(loess(dat4_trim$MgCa ~ dat4_trim$Day, span = 0.2)),
    MnCa = predict(loess(dat4_trim$MnCa ~ dat4_trim$Day, span = 0.2)),
    SrCa = predict(loess(dat4_trim$SrCa ~ dat4_trim$Day, span = 0.2)),
    BaCa = predict(loess(dat4_trim2$BaCa ~ dat4_trim2$Day, span = 0.2))
)
dat4_loess02$datetime <- dat4_trim$datetime

maxima29_loess02 <- data.frame(depth = maxima29_filtered$depth,
    Day = maxima29_filtered$Day,
    MgCa = predict(loess(maxima29_filtered$MgCa ~ maxima29_filtered$Day, span = 0.2)),
    MnCa = predict(loess(maxima29_filtered$MnCa ~ maxima29_filtered$Day, span = 0.2)),
    SrCa = predict(loess(maxima29_filtered$SrCa ~ maxima29_filtered$Day, span = 0.2)),
    BaCa = predict(loess(maxima29_filtered$BaCa ~ maxima29_filtered$Day, span = 0.2))
)

maxima84_loess02 <- data.frame(depth = maxima84_filtered$depth,
    Day = maxima84_filtered$Day,
    MgCa = predict(loess(maxima84_filtered$MgCa ~ maxima84_filtered$Day, span = 0.2)),
    MnCa = predict(loess(maxima84_filtered$MnCa ~ maxima84_filtered$Day, span = 0.2)),
    SrCa = predict(loess(maxima84_filtered$SrCa ~ maxima84_filtered$Day, span = 0.2)),
    BaCa = predict(loess(maxima84_filtered$BaCa ~ maxima84_filtered$Day, span = 0.2))
)

squamosa85_loess02 <- data.frame(depth = squamosa85_filtered$depth,
    Day = squamosa85_filtered$Day,
    MgCa = predict(loess(squamosa85_filtered$MgCa ~ squamosa85_filtered$Day, span = 0.2)),
    MnCa = predict(loess(squamosa85_filtered$MnCa ~ squamosa85_filtered$Day, span = 0.2)),
    SrCa = predict(loess(squamosa85_filtered$SrCa ~ squamosa85_filtered$Day, span = 0.2)),
    BaCa = predict(loess(squamosa85_filtered$BaCa ~ squamosa85_filtered$Day, span = 0.2))
)

squamosaFRS1_loess02 <- data.frame(depth = squamosaFRS1_filtered$depth,
    Day = squamosaFRS1_filtered$Day,
    MgCa = predict(loess(squamosaFRS1_filtered$MgCa ~ squamosaFRS1_filtered$Day, span = 0.2)),
    MnCa = predict(loess(squamosaFRS1_filtered$MnCa ~ squamosaFRS1_filtered$Day, span = 0.2)),
    SrCa = predict(loess(squamosaFRS1_filtered$SrCa ~ squamosaFRS1_filtered$Day, span = 0.2)),
    BaCa = predict(loess(squamosaFRS1_filtered$BaCa ~ squamosaFRS1_filtered$Day, span = 0.2))
)

squamosaM1_loess02 <- data.frame(depth = squamosaM1_filtered$depth,
    Day = squamosaM1_filtered$Day,
    MgCa = predict(loess(squamosaM1_filtered$MgCa ~ squamosaM1_filtered$Day, span = 0.2)),
    MnCa = predict(loess(squamosaM1_filtered$MnCa ~ squamosaM1_filtered$Day, span = 0.2)),
    SrCa = predict(loess(squamosaM1_filtered$SrCa ~ squamosaM1_filtered$Day, span = 0.2)),
    BaCa = predict(loess(squamosaM1_filtered$BaCa ~ squamosaM1_filtered$Day, span = 0.2))
)

squamosinaSQSA1_loess02 <- data.frame(depth = squamosinaSQSA1_filtered$depth,
    Day = squamosinaSQSA1_filtered$Day,
    MgCa = predict(loess(squamosinaSQSA1_filtered$MgCa ~ squamosinaSQSA1_filtered$Day, span = 0.2)),
    MnCa = predict(loess(squamosinaSQSA1_filtered$MnCa ~ squamosinaSQSA1_filtered$Day, span = 0.2)),
    SrCa = predict(loess(squamosinaSQSA1_filtered$SrCa ~ squamosinaSQSA1_filtered$Day, span = 0.2)),
    BaCa = predict(loess(squamosinaSQSA1_filtered$BaCa ~ squamosinaSQSA1_filtered$Day, span = 0.2))
)

dat2_1_loess02$Specimen <- "P. maximus 2_1 loess02"
dat2_2_loess02$Specimen <- "P. maximus 2_2 loess02"
dat3_1_loess02$Specimen <- "P. maximus 3_1 loess02"
dat3_2_loess02$Specimen <- "P. maximus 3_2 loess02"
dat4_loess02$Specimen <- "P. maximus 4 loess02"
maxima29_loess02$Specimen <- "T. maxima 29 loess02"
maxima84_loess02$Specimen <- "T. maxima 84 loess02"
squamosa85_loess02$Specimen <- "T. squamosa 85 loess02"
squamosaFRS1_loess02$Specimen <- "T. squamosa FRS1 loess02"
squamosaM1_loess02$Specimen <- "T. squamosa M1 loess02"
squamosinaSQSA1_loess02$Specimen <- "T. squamosina SQSA1 loess02"

# Apply filtering and smoothing
dat2_1b <- data.frame(
    Depth = dat2_1_filtered$Depth,
    Day = dat2_1_filtered$Day,
    MgCa = dat2_1_filtered$MgCa - dat2_1_loess02$MgCa,
    SrCa = dat2_1_filtered$SrCa - dat2_1_loess02$SrCa,
    MnCa = dat2_1_filtered$MnCa - dat2_1_loess02$MnCa,
    BaCa = dat2_1_filtered$BaCa - dat2_1_loess02$BaCa,
    datetime = dat2_1_filtered$datetime
)

dat2_2b <- data.frame(
    Depth = dat2_2_filtered$Depth,
    Day = dat2_2_filtered$Day,
    MgCa = dat2_2_filtered$MgCa - dat2_2_loess02$MgCa,
    SrCa = dat2_2_filtered$SrCa - dat2_2_loess02$SrCa,
    MnCa = dat2_2_filtered$MnCa - dat2_2_loess02$MnCa,
    BaCa = dat2_2_filtered$BaCa - dat2_2_loess02$BaCa,
    datetime = dat2_2_filtered$datetime
)

dat3_1b <- data.frame(
    Depth = dat3_1_filtered$Depth,
    Day = dat3_1_filtered$Day,
    MgCa = dat3_1_filtered$MgCa - dat3_1_loess02$MgCa,
    SrCa = dat3_1_filtered$SrCa - dat3_1_loess02$SrCa,
    MnCa = dat3_1_filtered$MnCa - dat3_1_loess02$MnCa,
    BaCa = dat3_1_filtered$BaCa - dat3_1_loess02$BaCa,
    datetime = dat3_1_filtered$datetime
)

dat3_2b <- data.frame(
    Depth = dat3_2_filtered$Depth,
    Day = dat3_2_filtered$Day,
    MgCa = dat3_2_filtered$MgCa - dat3_2_loess02$MgCa,
    SrCa = dat3_2_filtered$SrCa - dat3_2_loess02$SrCa,
    MnCa = dat3_2_filtered$MnCa - dat3_2_loess02$MnCa,
    BaCa = dat3_2_filtered$BaCa - dat3_2_loess02$BaCa,
    datetime = dat3_2_filtered$datetime
)

dat4b <- data.frame(
    Depth = dat4_filtered$Depth,
    Day = dat4_filtered$Day,
    MgCa = dat4_filtered$MgCa - dat4_loess02$MgCa,
    SrCa = dat4_filtered$SrCa - dat4_loess02$SrCa,
    MnCa = dat4_filtered$MnCa - dat4_loess02$MnCa,
    BaCa = dat4_filtered$BaCa - dat4_loess02$BaCa,
    datetime = dat4_filtered$datetime
)

maxima29b <- data.frame(
    depth = maxima29_filtered$depth,
    Day = maxima29_filtered$Day,
    MgCa = maxima29_filtered$MgCa - maxima29_loess02$MgCa,
    SrCa = maxima29_filtered$SrCa - maxima29_loess02$SrCa,
    MnCa = maxima29_filtered$MnCa - maxima29_loess02$MnCa,
    BaCa = maxima29_filtered$BaCa - maxima29_loess02$BaCa
)

maxima84b <- data.frame(
    depth = maxima84_filtered$depth,
    Day = maxima84_filtered$Day,
    MgCa = maxima84_filtered$MgCa - maxima84_loess02$MgCa,
    SrCa = maxima84_filtered$SrCa - maxima84_loess02$SrCa,
    MnCa = maxima84_filtered$MnCa - maxima84_loess02$MnCa,
    BaCa = maxima84_filtered$BaCa - maxima84_loess02$BaCa
)

squamosa85b <- data.frame(
    depth = squamosa85_filtered$depth,
    Day = squamosa85_filtered$Day,
    MgCa = squamosa85_filtered$MgCa - squamosa85_loess02$MgCa,
    SrCa = squamosa85_filtered$SrCa - squamosa85_loess02$SrCa,
    MnCa = squamosa85_filtered$MnCa - squamosa85_loess02$MnCa,
    BaCa = squamosa85_filtered$BaCa - squamosa85_loess02$BaCa
)

squamosaFRS1b <- data.frame(
    depth = squamosaFRS1_filtered$depth,
    Day = squamosaFRS1_filtered$Day,
    MgCa = squamosaFRS1_filtered$MgCa - squamosaFRS1_loess02$MgCa,
    SrCa = squamosaFRS1_filtered$SrCa - squamosaFRS1_loess02$SrCa,
    MnCa = squamosaFRS1_filtered$MnCa - squamosaFRS1_loess02$MnCa,
    BaCa = squamosaFRS1_filtered$BaCa - squamosaFRS1_loess02$BaCa
)

squamosaM1b <- data.frame(
    depth = squamosaM1_filtered$depth,
    Day = squamosaM1_filtered$Day,
    MgCa = squamosaM1_filtered$MgCa - squamosaM1_loess02$MgCa,
    SrCa = squamosaM1_filtered$SrCa - squamosaM1_loess02$SrCa,
    MnCa = squamosaM1_filtered$MnCa - squamosaM1_loess02$MnCa,
    BaCa = squamosaM1_filtered$BaCa - squamosaM1_loess02$BaCa
)

squamosinaSQSA1b <- data.frame(
    depth = squamosinaSQSA1_filtered$depth,
    Day = squamosinaSQSA1_filtered$Day,
    MgCa = squamosinaSQSA1_filtered$MgCa - squamosinaSQSA1_loess02$MgCa,
    SrCa = squamosinaSQSA1_filtered$SrCa - squamosinaSQSA1_loess02$SrCa,
    MnCa = squamosinaSQSA1_filtered$MnCa - squamosinaSQSA1_loess02$MnCa,
    BaCa = squamosinaSQSA1_filtered$BaCa - squamosinaSQSA1_loess02$BaCa
)

dat2_1b$Specimen <- "P. maximus 2_1 smoothed"
dat2_2b$Specimen <- "P. maximus 2_2 smoothed"
dat3_1b$Specimen <- "P. maximus 3_1 smoothed"
dat3_2b$Specimen <- "P. maximus 3_2 smoothed"
dat4b$Specimen <- "P. maximus 4 smoothed"
maxima29b$Specimen <- "T. maxima 29 smoothed"
maxima84b$Specimen <- "T. maxima 84 smoothed"
squamosa85b$Specimen <- "T. squamosa 85 smoothed"
squamosaFRS1b$Specimen <- "T. squamosa FRS1 smoothed"
squamosaM1b$Specimen <- "T. squamosa M1 smoothed"
squamosinaSQSA1b$Specimen <- "T. squamosina SQSA1 smoothed"

# Bandpass

# Load bandpass filter data
dat2_1_bp <- read.csv("<path>/dat2_1_bp.csv", header = TRUE) # Load Shell2_1 bp data
dat2_2_bp <- read.csv("<path>/dat2_2_bp.csv", header = TRUE) # Load Shell2_2 bp data
dat3_1_bp <- read.csv("<path>/dat3_1_bp.csv", header = TRUE) # Load Shell3_1 bp data
dat3_2_bp <- read.csv("<path>/dat3_2_bp.csv", header = TRUE) # Load Shell3_2 bp data
dat4_bp <- read.csv("<path>/dat4_bp.csv", header = TRUE) # Load Shell4 bp data
maxima29_bp <- read.csv("<path>/maxima29_bp.csv", header = TRUE) # Load maxima 29 bp data
maxima84_bp <- read.csv("<path>/maxima84_bp.csv", header = TRUE) # Load maxima 84 bp data
squamosa85_bp <- read.csv("<path>/squamosa85_bp.csv", header = TRUE) # Load squamosa 85 bp data
squamosaFRS1_bp <- read.csv("<path>/squamosaFRS1_bp.csv", header = TRUE) # Load squamosa FRS1 bp data
squamosaM1_bp <- read.csv("<path>/squamosaM1_bp.csv", header = TRUE) # Load squamosa M1 bp data
squamosinaSQSA1_bp <- read.csv("<path>/squamosinaSQSA1_bp.csv", header = TRUE) # Load squamosina SQSA1 bp data

colnames(dat2_2_bp)[4:7] <- c("SrCa_tidal", "MgCa_daily", "MnCa_daily", "BaCa_tidal")

# Add specimen columns
dat2_1_bp$Specimen <- "P. maximus 2_1 bp"
dat2_2_bp$Specimen <- "P. maximus 2_2 bp"
dat3_1_bp$Specimen <- "P. maximus 3_1 bp"
dat3_2_bp$Specimen <- "P. maximus 3_2 bp"
dat4_bp$Specimen <- "P. maximus 4 bp"
maxima29_bp$Specimen <- "T. maxima 29 bp"
maxima84_bp$Specimen <- "T. maxima 84 bp"
squamosa85_bp$Specimen <- "T. squamosa 85 bp"
squamosaFRS1_bp$Specimen <- "T. squamosa FRS1 bp"
squamosaM1_bp$Specimen <- "T. squamosa M1 bp"
squamosinaSQSA1_bp$Specimen <- "T. squamosina SQSA1 bp"

# Calculate variances
dat2_1_var <- data.frame(
    Specimen = rep("P. maximus 2_1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(dat2_1[, which(colnames(dat2_1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat2_1[, which(colnames(dat2_1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(dat2_1_trim[, which(colnames(dat2_1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat2_1_trim[, which(colnames(dat2_1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(dat2_1_filtered[, which(colnames(dat2_1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat2_1_filtered[, which(colnames(dat2_1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(dat2_1b[, which(colnames(dat2_1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat2_1b[, which(colnames(dat2_1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
dat2_1_var$bp_daily <- NA
dat2_1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(dat2_1_var$record[i], "_daily", sep = "") %in% colnames(dat2_1_bp)){
        dat2_1_var$bp_daily[i] <- var(dat2_1_bp[, which(colnames(dat2_1_bp) == paste(dat2_1_var$record[i], "_daily", sep = ""))])
    }else{
        dat2_1_var$bp_daily[i] <- NA
    }
    if(paste(dat2_1_var$record[i], "_tidal", sep = "") %in% colnames(dat2_1_bp)){
        dat2_1_var$bp_tidal[i] <- var(dat2_1_bp[, which(colnames(dat2_1_bp) == paste(dat2_1_var$record[i], "_tidal", sep = ""))])
    }else{
        dat2_1_var$bp_tidal[i] <- NA
    }
}

dat2_2_var <- data.frame(
    Specimen = rep("P. maximus 2_2", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(dat2_2[, which(colnames(dat2_2) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat2_2[, which(colnames(dat2_2) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(dat2_2_trim[, which(colnames(dat2_2_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat2_2_trim[, which(colnames(dat2_2_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(dat2_2_filtered[, which(colnames(dat2_2_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat2_2_filtered[, which(colnames(dat2_2_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(dat2_2b[, which(colnames(dat2_2b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat2_2b[, which(colnames(dat2_2b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
dat2_2_var$bp_daily <- NA
dat2_2_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(dat2_2_var$record[i], "_daily", sep = "") %in% colnames(dat2_2_bp)){
        dat2_2_var$bp_daily[i] <- var(dat2_2_bp[, which(colnames(dat2_2_bp) == paste(dat2_2_var$record[i], "_daily", sep = ""))])
    }else{
        dat2_2_var$bp_daily[i] <- NA
    }
    if(paste(dat2_2_var$record[i], "_tidal", sep = "") %in% colnames(dat2_2_bp)){
        dat2_2_var$bp_tidal[i] <- var(dat2_2_bp[, which(colnames(dat2_2_bp) == paste(dat2_2_var$record[i], "_tidal", sep = ""))])
    }else{
        dat2_2_var$bp_tidal[i] <- NA
    }
}

dat3_1_var <- data.frame(
    Specimen = rep("P. maximus 3_1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(dat3_1[, which(colnames(dat3_1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat3_1[, which(colnames(dat3_1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(dat3_1_trim[, which(colnames(dat3_1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat3_1_trim[, which(colnames(dat3_1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(dat3_1_filtered[, which(colnames(dat3_1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat3_1_filtered[, which(colnames(dat3_1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(dat3_1b[, which(colnames(dat3_1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat3_1b[, which(colnames(dat3_1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
dat3_1_var$bp_daily <- NA
dat3_1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(dat3_1_var$record[i], "_daily", sep = "") %in% colnames(dat3_1_bp)){
        dat3_1_var$bp_daily[i] <- var(dat3_1_bp[, which(colnames(dat3_1_bp) == paste(dat3_1_var$record[i], "_daily", sep = ""))])
    }else{
        dat3_1_var$bp_daily[i] <- NA
    }
    if(paste(dat3_1_var$record[i], "_tidal", sep = "") %in% colnames(dat3_1_bp)){
        dat3_1_var$bp_tidal[i] <- var(dat3_1_bp[, which(colnames(dat3_1_bp) == paste(dat3_1_var$record[i], "_tidal", sep = ""))])
    }else{
        dat3_1_var$bp_tidal[i] <- NA
    }
}

dat3_2_var <- data.frame(
    Specimen = rep("P. maximus 3_2", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(dat3_2[, which(colnames(dat3_2) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat3_2[, which(colnames(dat3_2) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(dat3_2_trim[, which(colnames(dat3_2_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat3_2_trim[, which(colnames(dat3_2_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(dat3_2_filtered[, which(colnames(dat3_2_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat3_2_filtered[, which(colnames(dat3_2_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(dat3_2b[, which(colnames(dat3_2b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat3_2b[, which(colnames(dat3_2b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
dat3_2_var$bp_daily <- NA
dat3_2_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(dat3_2_var$record[i], "_daily", sep = "") %in% colnames(dat3_2_bp)){
        dat3_2_var$bp_daily[i] <- var(dat3_2_bp[, which(colnames(dat3_2_bp) == paste(dat3_2_var$record[i], "_daily", sep = ""))])
    }else{
        dat3_2_var$bp_daily[i] <- NA
    }
    if(paste(dat3_2_var$record[i], "_tidal", sep = "") %in% colnames(dat3_2_bp)){
        dat3_2_var$bp_tidal[i] <- var(dat3_2_bp[, which(colnames(dat3_2_bp) == paste(dat3_2_var$record[i], "_tidal", sep = ""))])
    }else{
        dat3_2_var$bp_tidal[i] <- NA
    }
}

dat4_var <- data.frame(
    Specimen = rep("P. maximus 4", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(dat4[, which(colnames(dat4) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat4[, which(colnames(dat4) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(dat4_trim[, which(colnames(dat4_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat4_trim[, which(colnames(dat4_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(dat4_filtered[, which(colnames(dat4_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat4_filtered[, which(colnames(dat4_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(dat4b[, which(colnames(dat4b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(dat4b[, which(colnames(dat4b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
dat4_var$bp_daily <- NA
dat4_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(dat4_var$record[i], "_daily", sep = "") %in% colnames(dat4_bp)){
        dat4_var$bp_daily[i] <- var(dat4_bp[, which(colnames(dat4_bp) == paste(dat4_var$record[i], "_daily", sep = ""))])
    }else{
        dat4_var$bp_daily[i] <- NA
    }
    if(paste(dat4_var$record[i], "_tidal", sep = "") %in% colnames(dat4_bp)){
        dat4_var$bp_tidal[i] <- var(dat4_bp[, which(colnames(dat4_bp) == paste(dat4_var$record[i], "_tidal", sep = ""))])
    }else{
        dat4_var$bp_tidal[i] <- NA
    }
}

maxima29_var <- data.frame(
    Specimen = rep("T. maxima 29", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(maxima29[, which(colnames(maxima29) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(maxima29[, which(colnames(maxima29) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(maxima29_trim[, which(colnames(maxima29_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(maxima29_trim[, which(colnames(maxima29_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(maxima29_filtered[, which(colnames(maxima29_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(maxima29_filtered[, which(colnames(maxima29_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(maxima29b[, which(colnames(maxima29b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(maxima29b[, which(colnames(maxima29b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
maxima29_var$bp_daily <- NA
maxima29_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(maxima29_var$record[i], "_daily", sep = "") %in% colnames(maxima29_bp)){
        maxima29_var$bp_daily[i] <- var(maxima29_bp[, which(colnames(maxima29_bp) == paste(maxima29_var$record[i], "_daily", sep = ""))])
    }else{
        maxima29_var$bp_daily[i] <- NA
    }
    if(paste(maxima29_var$record[i], "_tidal", sep = "") %in% colnames(maxima29_bp)){
        maxima29_var$bp_tidal[i] <- var(maxima29_bp[, which(colnames(maxima29_bp) == paste(maxima29_var$record[i], "_tidal", sep = ""))])
    }else{
        maxima29_var$bp_tidal[i] <- NA
    }
}

maxima84_var <- data.frame(
    Specimen = rep("T. maxima 84", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(maxima84[, which(colnames(maxima84) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(maxima84[, which(colnames(maxima84) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(maxima84_trim[, which(colnames(maxima84_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(maxima84_trim[, which(colnames(maxima84_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(maxima84_filtered[, which(colnames(maxima84_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(maxima84_filtered[, which(colnames(maxima84_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(maxima84b[, which(colnames(maxima84b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(maxima84b[, which(colnames(maxima84b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
maxima84_var$bp_daily <- NA
maxima84_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(maxima84_var$record[i], "_daily", sep = "") %in% colnames(maxima84_bp)){
        maxima84_var$bp_daily[i] <- var(maxima84_bp[, which(colnames(maxima84_bp) == paste(maxima84_var$record[i], "_daily", sep = ""))])
    }else{
        maxima84_var$bp_daily[i] <- NA
    }
    if(paste(maxima84_var$record[i], "_tidal", sep = "") %in% colnames(maxima84_bp)){
        maxima84_var$bp_tidal[i] <- var(maxima84_bp[, which(colnames(maxima84_bp) == paste(maxima84_var$record[i], "_tidal", sep = ""))])
    }else{
        maxima84_var$bp_tidal[i] <- NA
    }
}

squamosa85_var <- data.frame(
    Specimen = rep("T. squamosa 85", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(squamosa85[, which(colnames(squamosa85) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosa85[, which(colnames(squamosa85) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(squamosa85_trim[, which(colnames(squamosa85_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosa85_trim[, which(colnames(squamosa85_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(squamosa85_filtered[, which(colnames(squamosa85_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosa85_filtered[, which(colnames(squamosa85_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(squamosa85b[, which(colnames(squamosa85b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosa85b[, which(colnames(squamosa85b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
squamosa85_var$bp_daily <- NA
squamosa85_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(squamosa85_var$record[i], "_daily", sep = "") %in% colnames(squamosa85_bp)){
        squamosa85_var$bp_daily[i] <- var(squamosa85_bp[, which(colnames(squamosa85_bp) == paste(squamosa85_var$record[i], "_daily", sep = ""))])
    }else{
        squamosa85_var$bp_daily[i] <- NA
    }
    if(paste(squamosa85_var$record[i], "_tidal", sep = "") %in% colnames(squamosa85_bp)){
        squamosa85_var$bp_tidal[i] <- var(squamosa85_bp[, which(colnames(squamosa85_bp) == paste(squamosa85_var$record[i], "_tidal", sep = ""))])
    }else{
        squamosa85_var$bp_tidal[i] <- NA
    }
}

squamosaFRS1_var <- data.frame(
    Specimen = rep("T. squamosa FRS1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(squamosaFRS1[, which(colnames(squamosaFRS1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosaFRS1[, which(colnames(squamosaFRS1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(squamosaFRS1_trim[, which(colnames(squamosaFRS1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosaFRS1_trim[, which(colnames(squamosaFRS1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(squamosaFRS1_filtered[, which(colnames(squamosaFRS1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosaFRS1_filtered[, which(colnames(squamosaFRS1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(squamosaFRS1b[, which(colnames(squamosaFRS1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosaFRS1b[, which(colnames(squamosaFRS1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
squamosaFRS1_var$bp_daily <- NA
squamosaFRS1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(squamosaFRS1_var$record[i], "_daily", sep = "") %in% colnames(squamosaFRS1_bp)){
        squamosaFRS1_var$bp_daily[i] <- var(squamosaFRS1_bp[, which(colnames(squamosaFRS1_bp) == paste(squamosaFRS1_var$record[i], "_daily", sep = ""))])
    }else{
        squamosaFRS1_var$bp_daily[i] <- NA
    }
    if(paste(squamosaFRS1_var$record[i], "_tidal", sep = "") %in% colnames(squamosaFRS1_bp)){
        squamosaFRS1_var$bp_tidal[i] <- var(squamosaFRS1_bp[, which(colnames(squamosaFRS1_bp) == paste(squamosaFRS1_var$record[i], "_tidal", sep = ""))])
    }else{
        squamosaFRS1_var$bp_tidal[i] <- NA
    }
}

squamosaM1_var <- data.frame(
    Specimen = rep("T. squamosa M1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(squamosaM1[, which(colnames(squamosaM1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosaM1[, which(colnames(squamosaM1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(squamosaM1_trim[, which(colnames(squamosaM1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosaM1_trim[, which(colnames(squamosaM1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(squamosaM1_filtered[, which(colnames(squamosaM1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosaM1_filtered[, which(colnames(squamosaM1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(squamosaM1b[, which(colnames(squamosaM1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosaM1b[, which(colnames(squamosaM1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
squamosaM1_var$bp_daily <- NA
squamosaM1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(squamosaM1_var$record[i], "_daily", sep = "") %in% colnames(squamosaM1_bp)){
        squamosaM1_var$bp_daily[i] <- var(squamosaM1_bp[, which(colnames(squamosaM1_bp) == paste(squamosaM1_var$record[i], "_daily", sep = ""))])
    }else{
        squamosaM1_var$bp_daily[i] <- NA
    }
    if(paste(squamosaM1_var$record[i], "_tidal", sep = "") %in% colnames(squamosaM1_bp)){
        squamosaM1_var$bp_tidal[i] <- var(squamosaM1_bp[, which(colnames(squamosaM1_bp) == paste(squamosaM1_var$record[i], "_tidal", sep = ""))])
    }else{
        squamosaM1_var$bp_tidal[i] <- NA
    }
}

squamosinaSQSA1_var <- data.frame(
    Specimen = rep("T. squamosina SQSA1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(squamosinaSQSA1[, which(colnames(squamosinaSQSA1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosinaSQSA1[, which(colnames(squamosinaSQSA1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(squamosinaSQSA1_trim[, which(colnames(squamosinaSQSA1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosinaSQSA1_trim[, which(colnames(squamosinaSQSA1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(squamosinaSQSA1_filtered[, which(colnames(squamosinaSQSA1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosinaSQSA1_filtered[, which(colnames(squamosinaSQSA1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(squamosinaSQSA1b[, which(colnames(squamosinaSQSA1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(squamosinaSQSA1b[, which(colnames(squamosinaSQSA1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
squamosinaSQSA1_var$bp_daily <- NA
squamosinaSQSA1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(squamosinaSQSA1_var$record[i], "_daily", sep = "") %in% colnames(squamosinaSQSA1_bp)){
        squamosinaSQSA1_var$bp_daily[i] <- var(squamosinaSQSA1_bp[, which(colnames(squamosinaSQSA1_bp) == paste(squamosinaSQSA1_var$record[i], "_daily", sep = ""))])
    }else{
        squamosinaSQSA1_var$bp_daily[i] <- NA
    }
    if(paste(squamosinaSQSA1_var$record[i], "_tidal", sep = "") %in% colnames(squamosinaSQSA1_bp)){
        squamosinaSQSA1_var$bp_tidal[i] <- var(squamosinaSQSA1_bp[, which(colnames(squamosinaSQSA1_bp) == paste(squamosinaSQSA1_var$record[i], "_tidal", sep = ""))])
    }else{
        squamosinaSQSA1_var$bp_tidal[i] <- NA
    }
}

# Combine variance data for export
combined_variance <- rbind(dat2_1_var,
    dat2_2_var,
    dat3_1_var,
    dat3_2_var,
    dat4_var,
    maxima29_var,
    maxima84_var,
    squamosa85_var,
    squamosaFRS1_var,
    squamosaM1_var,
    squamosinaSQSA1_var)

write.csv(combined_variance, "<path>/Variance_decomposition.csv")

# ------------------------------------------------------------------------------
# Example plot for specimen 2_1

# Combine and demean data for comparison
dat2_1_SrCa_all <- data.frame(
    SrCa = c(dat2_1[, 5] - median(dat2_1[, 5]),
        dat2_1_trim[, 5] - median(dat2_1_trim[, 5]),
        dat2_1_filtered[, 5] - median(dat2_1_filtered[, 5]),
        dat2_1b[, 4],
        dat2_1_bp[, 5],
        dat2_1_bp[, 4]),
    Dataset = c(rep("Full dataset", length(dat2_1[, 5])),
        rep("Trimmed dataset", length(dat2_1_trim[, 5])),
        rep("Filtered dataset", length(dat2_1_filtered[, 5])),
        rep("Smoothed dataset", length(dat2_1b[, 4])),
        rep("Daily variability", length(dat2_1_bp[, 5])),
        rep("Tidal (12h) variability", length(dat2_1_bp[, 4])))
)

# Order factor of categories
dat2_1_SrCa_all$Dataset <- factor(dat2_1_SrCa_all$Dataset, levels = unique(dat2_1_SrCa_all$Dataset))

# Create colorscale
Specimencolors <- c(brewer.pal(11, "RdBu")[11:7], "#000000", brewer.pal(11, "RdBu")[5:1])
names(Specimencolors) <- c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1", "PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")

# Datasetcolors <- rep(Specimencolors[7], 6)
Datasetcolors <- brewer.pal(6, "Greens")
names(Datasetcolors) <- unique(dat2_1_SrCa_all$Dataset)

Varlabels <- data.frame(Dataset = as.character(unique(dat2_1_SrCa_all$Dataset)),
    SrCa = rep(-1, 6),
    var = as.numeric(dat2_1_var[4, -(1:2)]))

SrCa_2_1_varplot <- ggplot(data = dat2_1_SrCa_all) +
    geom_violin(aes(x = Dataset,
            y = SrCa,
            fill = Dataset),
        kernel = "rectangular",
        scale = "width",
        position = "identity",
        width = .5,
        cex = 0,
        alpha = .3,
        color = "black",
        trim = TRUE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Dataset,
            y = SrCa,
            fill = Dataset),
        width = .2,
        outlier.shape = NA,
        alpha = .5,
        na.rm = TRUE) +
    geom_text(data = Varlabels,
        aes(x = Dataset,
            y = SrCa,
            label = signif(var, 3))) +
    scale_y_continuous("Sr/Ca [mmol/mol]",
        breaks = seq(-1, 1, 0.2),
        limits = c(-1, 1)) +
    scale_fill_manual(values = Datasetcolors) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

# ------------------------------------------------------------------------------
# Plot summary of variance decomposition

# Load summary data
var_decomp_summary <- read.csv("<path>/Variance_rel_smooth_summary.csv", header = TRUE)

Varcomp_summary_plot <- ggplot(subset(var_decomp_summary, stat != "mean"),
        aes(x = interaction(stat, cycle),
            fraction,
            fill = species
        )
    ) +
    geom_bar(stat = "identity",
        position = position_dodge()
    ) +
    scale_y_continuous("Percentage of variance (%)",
        breaks = seq(0, 0.1, 0.01),
        labels = seq(0, 10, 1)) +
    scale_x_discrete("") +
    scale_fill_brewer("Dark2") +
    theme_bw()

# Load full summary
var_decomp_summary2 <- read.csv("<path>/Variance_decomposition.csv", header = TRUE)
var_decomp_summary2$Specimen_name <- var_decomp_summary2$Specimen # Store full name

# Add columns of daily and tidal variance relative to smoothed record
var_decomp_summary2$daily_rel <- var_decomp_summary2$bp_daily / var_decomp_summary2$smooth
var_decomp_summary2$tidal_rel <- var_decomp_summary2$bp_tidal / var_decomp_summary2$smooth
var_decomp_summary2$daily_rel[which(is.na(var_decomp_summary2$daily_rel))] <- 0
var_decomp_summary2$tidal_rel[which(is.na(var_decomp_summary2$tidal_rel))] <- 0
var_decomp_summary2$record <- ordered(var_decomp_summary2$record, levels = unique(var_decomp_summary2$record))

# Variance composition summary plot with breakdown per specimen

# Create colorscale
Specimencolors <- c(brewer.pal(11, "RdBu")[11:7], "#000000", brewer.pal(11, "RdBu")[5:1])
names(Specimencolors) <- c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1", "PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")

Varcomp_summary_plot_daily <- ggplot(data = var_decomp_summary2,
        aes(x = record,
            y = daily_rel,
            color = Specimen)) +
    geom_point(cex = 3,
        pch = 19) +
    geom_point(data = var_decomp_summary2[which(var_decomp_summary2$Specimen == "TSM1"),],
        aes(x = record,
            y = daily_rel,
            color = Specimen),
        color = "black",
        size = 5,
        shape = 21,
        alpha = 0.5) +
    scale_color_manual(values = Specimencolors) +
    scale_y_continuous("Percentage of variance (%)",
        breaks = seq(0, 0.2, 0.05),
        labels = seq(0, 20, 5),
        minor_breaks = seq(0, 0.2, 0.01),
        limits = c(0, 0.21)) +
    scale_x_discrete("") +
    ggtitle("Daily variability") +
    theme_bw()

Varcomp_summary_plot_tidal <- ggplot(data = var_decomp_summary2,
        aes(x = record,
            y = tidal_rel,
            color = Specimen)) +
    geom_point(cex = 3,
        pch = 19) +
    geom_point(data = var_decomp_summary2[which(var_decomp_summary2$Specimen == "TSM1"),],
        aes(x = record,
            y = tidal_rel,
            group = Specimen,
            color = Specimen),
        color = "black",
        size = 5,
        shape = 21,
        alpha = 0.5) +
    scale_color_manual(values = Specimencolors) +
    scale_y_continuous("Percentage of variance (%)",
        breaks = seq(0, 0.2, 0.05),
        labels = seq(0, 20, 5),
        minor_breaks = seq(0, 0.2, 0.01),
        limits = c(0, 0.21)) +
    scale_x_discrete("") +
    ggtitle("Tidal variability") +
    theme_bw()

combined_varcomp_summary <- grid.arrange(Varcomp_summary_plot_daily + theme(legend.position = "none"),
    Varcomp_summary_plot_tidal + theme(legend.position = "none"),
    ncol = 2)