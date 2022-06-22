# Script to plot the results of variance decomposition (Figure 6)

require(tidyverse)
require(RColorBrewer)
require(gridExtra)
require(signal)

# Load and trim data (following previously established cutoffs)
load("<path>/Shell2_1_dated_aligned_cross.rda") # Load Shell2_1 data
PM2_1 <- dat_trim # Rename to avoid conflict
PM2_1_trim <- PM2_1[which(PM2_1$Depth > 7000), ]

load("<path>/Shell2_2_dated_cross.rda") # Load Shell2_2 data
PM2_2 <- dat_trim # Rename to avoid conflict
PM2_2_trim <- PM2_2[which(PM2_2$Depth > 3500), ]

load("<path>/Shell3_1_dated_aligned_cross.rda") # Load Shell3_1 data
PM3_1 <- dat_trim # Rename to avoid conflict
PM3_1_trim <- PM3_1[which(PM3_1$Depth > 1000), ]

load("<path>/Shell3_2_dated_cross.rda") # Load Shell3_2 data
PM3_2 <- dat_trim # Rename to avoid conflict
PM3_2_trim <- PM3_2[which(PM3_2$Depth > 4500), ]

load("<path>/Shell4_dated_cross.rda") # Load Shell4 data
PM4 <- dat_trim # Rename to avoid conflict
PM4_trim <- PM4[which(PM4$Depth > 2000), ]

TM29 <- read.csv("<path>/Line_Tridacnid_NdW_29_recalc.csv")
TM29_trim <- TM29[which((TM29$depth < 52) & (TM29$depth > 0.04)), ]
TM29_trim <- TM29_trim[-which((TM29_trim$depth < 33) & (TM29_trim$depth > 31.5)), ]

TM84 <- read.csv("<path>/Line_Tridacnid_NdW_84_recalc.csv")
TM84_trim <- TM84[which((TM84$depth < 37) & (TM84$depth > 0.01)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 4.05) & (TM84_trim$depth > 3.8)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 4.7) & (TM84_trim$depth > 4.5)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 6.3) & (TM84_trim$depth > 6.05)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 17.3) & (TM84_trim$depth > 17)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 29.5) & (TM84_trim$depth > 29)), ]

TS85 <- read.csv("<path>/Line_Tridacnid_NdW_85_recalc.csv")
TS85_trim <- TS85[which((TS85$depth < 39.7) & (TS85$depth > 0.6)), ]

TSFRS1 <- read.csv("<path>/Line_Tridacnid_NdW_FRS1_recalc.csv")
TSFRS1_trim <- TSFRS1[which((TSFRS1$depth < 32.8) & (TSFRS1$depth > 0.04)), ]

TSM1 <- read.csv("<path>/Line_Tridacnid_NdW_M1_recalc.csv")
TSM1_trim <- TSM1[which((TSM1$depth < 42.35) & (TSM1$depth > 0.015) & (TSM1$X25Mg.43Ca < 25) & (TSM1$X137Ba.43Ca < 0.01)), ]
TSM1_trim <- TSM1_trim[-which((TSM1_trim$depth < 3.15) & (TSM1_trim$depth > 2.8)), ]
TSM1_trim <- TSM1_trim[-which((TSM1_trim$depth < 4) & (TSM1_trim$depth > 3.65)), ]

SQSA1 <- read.csv("<path>/Line_Tridacnid_NdW_SQSA1_recalc.csv")
SQSA1_trim <- SQSA1[which((SQSA1$depth < 38.75) & (SQSA1$depth > 0.015) & (SQSA1$X25Mg.43Ca < 10) & (SQSA1$X137Ba.43Ca < 0.01)), ]
SQSA1_trim <- SQSA1_trim[-which((SQSA1_trim$depth < 4.05) & (SQSA1_trim$depth > 3.8)), ]
SQSA1_trim <- SQSA1_trim[-which((SQSA1_trim$depth < 26) & (SQSA1_trim$depth > 25.7)), ]
SQSA1_trim <- SQSA1_trim[-which((SQSA1_trim$depth < 25.2) & (SQSA1_trim$depth > 24.5)), ]

# Rename columns for easier reference
colnames(TM29_trim)[c(3:8, 10)] <- colnames(TM84_trim)[c(3:8, 10)] <- colnames(TS85_trim)[c(3:8, 10)] <- colnames(TSFRS1_trim)[c(3:8, 10)] <- colnames(TSM1_trim)[c(3:8, 10)] <- colnames(SQSA1_trim)[c(3:8, 10)] <- c("MgCa", "CaCa", "MnCa", "Sr87Ca", "SrCa", "BaCa", "Day")
colnames(PM2_1_trim)[2:6] <- colnames(PM2_2_trim)[2:6] <- colnames(PM3_1_trim)[2:6] <- colnames(PM3_2_trim)[2:6] <- colnames(PM4_trim)[2:6] <- c("MgCa", "MnCa", "Sr87Ca", "SrCa", "BaCa")
colnames(TM29)[c(3:8, 10)] <- colnames(TM84)[c(3:8, 10)] <- colnames(TS85)[c(3:8, 10)] <- colnames(TSFRS1)[c(3:8, 10)] <- colnames(TSM1)[c(3:8, 10)] <- colnames(SQSA1)[c(3:8, 10)] <- c("MgCa", "CaCa", "MnCa", "Sr87Ca", "SrCa", "BaCa", "Day")
colnames(PM2_1)[2:6] <- colnames(PM2_2)[2:6] <- colnames(PM3_1)[2:6] <- colnames(PM3_2)[2:6] <- colnames(PM4)[2:6] <- c("MgCa", "MnCa", "Sr87Ca", "SrCa", "BaCa")

# Add specimen columns
PM2_1$Specimen <- "P. maximus 2_1"
PM2_2$Specimen <- "P. maximus 2_2"
PM3_1$Specimen <- "P. maximus 3_1"
PM3_2$Specimen <- "P. maximus 3_2"
PM4$Specimen <- "P. maximus 4"
TM29$Specimen <- "T. maxima 29"
TM84$Specimen <- "T. maxima 84"
TS85$Specimen <- "T. squamosa 85"
TSFRS1$Specimen <- "T. squamosa FRS1"
TSM1$Specimen <- "T. squamosa M1"
SQSA1$Specimen <- "T. squamosina SQSA1"

PM2_1_trim$Specimen <- "P. maximus 2_1 trim"
PM2_2_trim$Specimen <- "P. maximus 2_2 trim"
PM3_1_trim$Specimen <- "P. maximus 3_1 trim"
PM3_2_trim$Specimen <- "P. maximus 3_2 trim"
PM4_trim$Specimen <- "P. maximus 4 trim"
TM29_trim$Specimen <- "T. maxima 29 trim"
TM84_trim$Specimen <- "T. maxima 84 trim"
TS85_trim$Specimen <- "T. squamosa 85 trim"
TSFRS1_trim$Specimen <- "T. squamosa FRS1 trim"
TSM1_trim$Specimen <- "T. squamosa M1 trim"
SQSA1_trim$Specimen <- "T. squamosina SQSA1 trim"

# Filtering

# Apply Savitzky-Golay filters
PM2_1_trim2 <- PM2_1_trim
PM2_1_trim2$BaCa[PM2_1_trim2$BaCa == 0] <- mean(PM2_1_trim2$BaCa[PM2_1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM2_1_filtered <- as.data.frame(cbind(PM2_1_trim2$Depth,
    PM2_1_trim2$Day,
    apply(PM2_1_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM2_1_filtered)[c(1, 2)] <- c("Depth", "Day")
PM2_1_filtered$datetime <- PM2_1_trim2$datetime

PM2_2_trim2 <- PM2_2_trim
PM2_2_trim2$BaCa[PM2_2_trim2$BaCa == 0] <- mean(PM2_2_trim2$BaCa[PM2_2_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM2_2_filtered <- as.data.frame(cbind(PM2_2_trim2$Depth,
    PM2_2_trim2$Day,
    apply(PM2_2_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM2_2_filtered)[c(1, 2)] <- c("Depth", "Day")
PM2_2_filtered$datetime <- PM2_2_trim2$datetime

PM3_1_trim2 <- PM3_1_trim
PM3_1_trim2$BaCa[PM3_1_trim2$BaCa == 0] <- mean(PM3_1_trim2$BaCa[PM3_1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM3_1_filtered <- as.data.frame(cbind(PM3_1_trim2$Depth,
    PM3_1_trim2$Day,
    apply(PM3_1_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM3_1_filtered)[c(1, 2)] <- c("Depth", "Day")
PM3_1_filtered$datetime <- PM3_1_trim2$datetime

PM3_2_trim2 <- PM3_2_trim
PM3_2_trim2$BaCa[PM3_2_trim2$BaCa == 0] <- mean(PM3_2_trim2$BaCa[PM3_2_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM3_2_filtered <- as.data.frame(cbind(PM3_2_trim2$Depth,
    PM3_2_trim2$Day,
    apply(PM3_2_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM3_2_filtered)[c(1, 2)] <- c("Depth", "Day")
PM3_2_filtered$datetime <- PM3_2_trim2$datetime

PM4_trim2 <- PM4_trim
PM4_trim2$BaCa[PM4_trim2$BaCa == 0] <- mean(PM4_trim2$BaCa[PM4_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM4_filtered <- as.data.frame(cbind(PM4_trim2$Depth,
    PM4_trim2$Day,
    apply(PM4_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM4_filtered)[c(1, 2)] <- c("Depth", "Day")
PM4_filtered$datetime <- PM4_trim2$datetime

TM29_trim2 <- TM29_trim
TM29_trim2$BaCa[TM29_trim2$BaCa == 0] <- mean(TM29_trim2$BaCa[TM29_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
TM29_trim2$SrCa[is.na(TM29_trim2$SrCa)] <- mean(TM29_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
TM29_trim2$MnCa[is.na(TM29_trim2$MnCa)] <- mean(TM29_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
TM29_trim2$MgCa[is.na(TM29_trim2$MgCa)] <- mean(TM29_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
TM29_trim2$BaCa[is.na(TM29_trim2$BaCa)] <- mean(TM29_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

TM29_filtered <- as.data.frame(cbind(TM29_trim2$depth,
    TM29_trim2$time,
    apply(TM29_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TM29_filtered)[c(1, 2)] <- c("depth", "Day")

TM84_trim2 <- TM84_trim
TM84_trim2$BaCa[TM84_trim2$BaCa == 0] <- mean(TM84_trim2$BaCa[TM84_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
TM84_trim2$SrCa[is.na(TM84_trim2$SrCa)] <- mean(TM84_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
TM84_trim2$MnCa[is.na(TM84_trim2$MnCa)] <- mean(TM84_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
TM84_trim2$MgCa[is.na(TM84_trim2$MgCa)] <- mean(TM84_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
TM84_trim2$BaCa[is.na(TM84_trim2$BaCa)] <- mean(TM84_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

TM84_filtered <- as.data.frame(cbind(TM84_trim2$depth,
    TM84_trim2$time,
    apply(TM84_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TM84_filtered)[c(1, 2)] <- c("depth", "Day")

TS85_trim2 <- TS85_trim
TS85_trim2$BaCa[TS85_trim2$BaCa == 0] <- mean(TS85_trim2$BaCa[TS85_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
TS85_trim2$SrCa[is.na(TS85_trim2$SrCa)] <- mean(TS85_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
TS85_trim2$MnCa[is.na(TS85_trim2$MnCa)] <- mean(TS85_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
TS85_trim2$MgCa[is.na(TS85_trim2$MgCa)] <- mean(TS85_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
TS85_trim2$BaCa[is.na(TS85_trim2$BaCa)] <- mean(TS85_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

TS85_filtered <- as.data.frame(cbind(TS85_trim2$depth,
    TS85_trim2$time,
    apply(TS85_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TS85_filtered)[c(1, 2)] <- c("depth", "Day")

TSFRS1_trim2 <- TSFRS1_trim
TSFRS1_trim2$BaCa[TSFRS1_trim2$BaCa == 0] <- mean(TSFRS1_trim2$BaCa[TSFRS1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
TSFRS1_trim2$SrCa[is.na(TSFRS1_trim2$SrCa)] <- mean(TSFRS1_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
TSFRS1_trim2$MnCa[is.na(TSFRS1_trim2$MnCa)] <- mean(TSFRS1_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
TSFRS1_trim2$MgCa[is.na(TSFRS1_trim2$MgCa)] <- mean(TSFRS1_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
TSFRS1_trim2$BaCa[is.na(TSFRS1_trim2$BaCa)] <- mean(TSFRS1_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

TSFRS1_filtered <- as.data.frame(cbind(TSFRS1_trim2$depth,
    TSFRS1_trim2$time,
    apply(TSFRS1_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TSFRS1_filtered)[c(1, 2)] <- c("depth", "Day")

TSM1_trim2 <- TSM1_trim
TSM1_trim2$BaCa[TSM1_trim2$BaCa == 0] <- mean(TSM1_trim2$BaCa[TSM1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
TSM1_trim2$SrCa[is.na(TSM1_trim2$SrCa)] <- mean(TSM1_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
TSM1_trim2$MnCa[is.na(TSM1_trim2$MnCa)] <- mean(TSM1_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
TSM1_trim2$MgCa[is.na(TSM1_trim2$MgCa)] <- mean(TSM1_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
TSM1_trim2$BaCa[is.na(TSM1_trim2$BaCa)] <- mean(TSM1_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

TSM1_filtered <- as.data.frame(cbind(TSM1_trim2$depth,
    TSM1_trim2$time,
    apply(TSM1_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TSM1_filtered)[c(1, 2)] <- c("depth", "Day")

SQSA1_trim2 <- SQSA1_trim
SQSA1_trim2$BaCa[SQSA1_trim2$BaCa == 0] <- mean(SQSA1_trim2$BaCa[SQSA1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
SQSA1_trim2$SrCa[is.na(SQSA1_trim2$SrCa)] <- mean(SQSA1_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
SQSA1_trim2$MnCa[is.na(SQSA1_trim2$MnCa)] <- mean(SQSA1_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
SQSA1_trim2$MgCa[is.na(SQSA1_trim2$MgCa)] <- mean(SQSA1_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
SQSA1_trim2$BaCa[is.na(SQSA1_trim2$BaCa)] <- mean(SQSA1_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

SQSA1_filtered <- as.data.frame(cbind(SQSA1_trim2$depth,
    SQSA1_trim2$time,
    apply(SQSA1_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3,
        n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(SQSA1_filtered)[c(1, 2)] <- c("depth", "Day")

# Add specimen columns
PM2_1_filtered$Specimen <- "P. maximus 2_1 filtered"
PM2_2_filtered$Specimen <- "P. maximus 2_2 filtered"
PM3_1_filtered$Specimen <- "P. maximus 3_1 filtered"
PM3_2_filtered$Specimen <- "P. maximus 3_2 filtered"
PM4_filtered$Specimen <- "P. maximus 4 filtered"
TM29_filtered$Specimen <- "T. maxima 29 filtered"
TM84_filtered$Specimen <- "T. maxima 84 filtered"
TS85_filtered$Specimen <- "T. squamosa 85 filtered"
TSFRS1_filtered$Specimen <- "T. squamosa FRS1 filtered"
TSM1_filtered$Specimen <- "T. squamosa M1 filtered"
SQSA1_filtered$Specimen <- "T. squamosina SQSA1 filtered"

# Smoothing

# Apply LOESS smoothing
# Loess span of 0.2 is equivalent to 50 days
PM2_1_loess02 <- data.frame(Depth = PM2_1_trim$Depth,
    Day = PM2_1_trim$Day,
    MgCa = predict(loess(PM2_1_trim$MgCa ~ PM2_1_trim$Day, span = 0.2)),
    MnCa = predict(loess(PM2_1_trim$MnCa ~ PM2_1_trim$Day, span = 0.2)),
    SrCa = predict(loess(PM2_1_trim$SrCa ~ PM2_1_trim$Day, span = 0.2)),
    BaCa = predict(loess(PM2_1_trim2$BaCa ~ PM2_1_trim2$Day, span = 0.2))
)
PM2_1_loess02$datetime <- PM2_1_trim$datetime

PM2_2_loess02 <- data.frame(Depth = PM2_2_trim$Depth,
    Day = PM2_2_trim$Day,
    MgCa = predict(loess(PM2_2_trim$MgCa ~ PM2_2_trim$Day, span = 0.2)),
    MnCa = predict(loess(PM2_2_trim$MnCa ~ PM2_2_trim$Day, span = 0.2)),
    SrCa = predict(loess(PM2_2_trim$SrCa ~ PM2_2_trim$Day, span = 0.2)),
    BaCa = predict(loess(PM2_2_trim2$BaCa ~ PM2_2_trim2$Day, span = 0.2))
)
PM2_2_loess02$datetime <- PM2_2_trim$datetime

PM3_1_loess02 <- data.frame(Depth = PM3_1_trim$Depth,
    Day = PM3_1_trim$Day,
    MgCa = predict(loess(PM3_1_trim$MgCa ~ PM3_1_trim$Day, span = 0.2)),
    MnCa = predict(loess(PM3_1_trim$MnCa ~ PM3_1_trim$Day, span = 0.2)),
    SrCa = predict(loess(PM3_1_trim$SrCa ~ PM3_1_trim$Day, span = 0.2)),
    BaCa = predict(loess(PM3_1_trim2$BaCa ~ PM3_1_trim2$Day, span = 0.2))
)
PM3_1_loess02$datetime <- PM3_1_trim$datetime

PM3_2_loess02 <- data.frame(Depth = PM3_2_trim$Depth,
    Day = PM3_2_trim$Day,
    MgCa = predict(loess(PM3_2_trim$MgCa ~ PM3_2_trim$Day, span = 0.2)),
    MnCa = predict(loess(PM3_2_trim$MnCa ~ PM3_2_trim$Day, span = 0.2)),
    SrCa = predict(loess(PM3_2_trim$SrCa ~ PM3_2_trim$Day, span = 0.2)),
    BaCa = predict(loess(PM3_2_trim2$BaCa ~ PM3_2_trim2$Day, span = 0.2))
)
PM3_2_loess02$datetime <- PM3_2_trim$datetime

PM4_loess02 <- data.frame(Depth = PM4_trim$Depth,
    Day = PM4_trim$Day,
    MgCa = predict(loess(PM4_trim$MgCa ~ PM4_trim$Day, span = 0.2)),
    MnCa = predict(loess(PM4_trim$MnCa ~ PM4_trim$Day, span = 0.2)),
    SrCa = predict(loess(PM4_trim$SrCa ~ PM4_trim$Day, span = 0.2)),
    BaCa = predict(loess(PM4_trim2$BaCa ~ PM4_trim2$Day, span = 0.2))
)
PM4_loess02$datetime <- PM4_trim$datetime

TM29_loess02 <- data.frame(depth = TM29_filtered$depth,
    Day = TM29_filtered$Day,
    MgCa = predict(loess(TM29_filtered$MgCa ~ TM29_filtered$Day, span = 0.2)),
    MnCa = predict(loess(TM29_filtered$MnCa ~ TM29_filtered$Day, span = 0.2)),
    SrCa = predict(loess(TM29_filtered$SrCa ~ TM29_filtered$Day, span = 0.2)),
    BaCa = predict(loess(TM29_filtered$BaCa ~ TM29_filtered$Day, span = 0.2))
)

TM84_loess02 <- data.frame(depth = TM84_filtered$depth,
    Day = TM84_filtered$Day,
    MgCa = predict(loess(TM84_filtered$MgCa ~ TM84_filtered$Day, span = 0.2)),
    MnCa = predict(loess(TM84_filtered$MnCa ~ TM84_filtered$Day, span = 0.2)),
    SrCa = predict(loess(TM84_filtered$SrCa ~ TM84_filtered$Day, span = 0.2)),
    BaCa = predict(loess(TM84_filtered$BaCa ~ TM84_filtered$Day, span = 0.2))
)

TS85_loess02 <- data.frame(depth = TS85_filtered$depth,
    Day = TS85_filtered$Day,
    MgCa = predict(loess(TS85_filtered$MgCa ~ TS85_filtered$Day, span = 0.2)),
    MnCa = predict(loess(TS85_filtered$MnCa ~ TS85_filtered$Day, span = 0.2)),
    SrCa = predict(loess(TS85_filtered$SrCa ~ TS85_filtered$Day, span = 0.2)),
    BaCa = predict(loess(TS85_filtered$BaCa ~ TS85_filtered$Day, span = 0.2))
)

TSFRS1_loess02 <- data.frame(depth = TSFRS1_filtered$depth,
    Day = TSFRS1_filtered$Day,
    MgCa = predict(loess(TSFRS1_filtered$MgCa ~ TSFRS1_filtered$Day, span = 0.2)),
    MnCa = predict(loess(TSFRS1_filtered$MnCa ~ TSFRS1_filtered$Day, span = 0.2)),
    SrCa = predict(loess(TSFRS1_filtered$SrCa ~ TSFRS1_filtered$Day, span = 0.2)),
    BaCa = predict(loess(TSFRS1_filtered$BaCa ~ TSFRS1_filtered$Day, span = 0.2))
)

TSM1_loess02 <- data.frame(depth = TSM1_filtered$depth,
    Day = TSM1_filtered$Day,
    MgCa = predict(loess(TSM1_filtered$MgCa ~ TSM1_filtered$Day, span = 0.2)),
    MnCa = predict(loess(TSM1_filtered$MnCa ~ TSM1_filtered$Day, span = 0.2)),
    SrCa = predict(loess(TSM1_filtered$SrCa ~ TSM1_filtered$Day, span = 0.2)),
    BaCa = predict(loess(TSM1_filtered$BaCa ~ TSM1_filtered$Day, span = 0.2))
)

SQSA1_loess02 <- data.frame(depth = SQSA1_filtered$depth,
    Day = SQSA1_filtered$Day,
    MgCa = predict(loess(SQSA1_filtered$MgCa ~ SQSA1_filtered$Day, span = 0.2)),
    MnCa = predict(loess(SQSA1_filtered$MnCa ~ SQSA1_filtered$Day, span = 0.2)),
    SrCa = predict(loess(SQSA1_filtered$SrCa ~ SQSA1_filtered$Day, span = 0.2)),
    BaCa = predict(loess(SQSA1_filtered$BaCa ~ SQSA1_filtered$Day, span = 0.2))
)

PM2_1_loess02$Specimen <- "P. maximus 2_1 loess02"
PM2_2_loess02$Specimen <- "P. maximus 2_2 loess02"
PM3_1_loess02$Specimen <- "P. maximus 3_1 loess02"
PM3_2_loess02$Specimen <- "P. maximus 3_2 loess02"
PM4_loess02$Specimen <- "P. maximus 4 loess02"
TM29_loess02$Specimen <- "T. maxima 29 loess02"
TM84_loess02$Specimen <- "T. maxima 84 loess02"
TS85_loess02$Specimen <- "T. squamosa 85 loess02"
TSFRS1_loess02$Specimen <- "T. squamosa FRS1 loess02"
TSM1_loess02$Specimen <- "T. squamosa M1 loess02"
SQSA1_loess02$Specimen <- "T. squamosina SQSA1 loess02"

# Apply filtering and smoothing
PM2_1b <- data.frame(
    Depth = PM2_1_filtered$Depth,
    Day = PM2_1_filtered$Day,
    MgCa = PM2_1_filtered$MgCa - PM2_1_loess02$MgCa,
    SrCa = PM2_1_filtered$SrCa - PM2_1_loess02$SrCa,
    MnCa = PM2_1_filtered$MnCa - PM2_1_loess02$MnCa,
    BaCa = PM2_1_filtered$BaCa - PM2_1_loess02$BaCa,
    datetime = PM2_1_filtered$datetime
)

PM2_2b <- data.frame(
    Depth = PM2_2_filtered$Depth,
    Day = PM2_2_filtered$Day,
    MgCa = PM2_2_filtered$MgCa - PM2_2_loess02$MgCa,
    SrCa = PM2_2_filtered$SrCa - PM2_2_loess02$SrCa,
    MnCa = PM2_2_filtered$MnCa - PM2_2_loess02$MnCa,
    BaCa = PM2_2_filtered$BaCa - PM2_2_loess02$BaCa,
    datetime = PM2_2_filtered$datetime
)

PM3_1b <- data.frame(
    Depth = PM3_1_filtered$Depth,
    Day = PM3_1_filtered$Day,
    MgCa = PM3_1_filtered$MgCa - PM3_1_loess02$MgCa,
    SrCa = PM3_1_filtered$SrCa - PM3_1_loess02$SrCa,
    MnCa = PM3_1_filtered$MnCa - PM3_1_loess02$MnCa,
    BaCa = PM3_1_filtered$BaCa - PM3_1_loess02$BaCa,
    datetime = PM3_1_filtered$datetime
)

PM3_2b <- data.frame(
    Depth = PM3_2_filtered$Depth,
    Day = PM3_2_filtered$Day,
    MgCa = PM3_2_filtered$MgCa - PM3_2_loess02$MgCa,
    SrCa = PM3_2_filtered$SrCa - PM3_2_loess02$SrCa,
    MnCa = PM3_2_filtered$MnCa - PM3_2_loess02$MnCa,
    BaCa = PM3_2_filtered$BaCa - PM3_2_loess02$BaCa,
    datetime = PM3_2_filtered$datetime
)

PM4b <- data.frame(
    Depth = PM4_filtered$Depth,
    Day = PM4_filtered$Day,
    MgCa = PM4_filtered$MgCa - PM4_loess02$MgCa,
    SrCa = PM4_filtered$SrCa - PM4_loess02$SrCa,
    MnCa = PM4_filtered$MnCa - PM4_loess02$MnCa,
    BaCa = PM4_filtered$BaCa - PM4_loess02$BaCa,
    datetime = PM4_filtered$datetime
)

TM29b <- data.frame(
    depth = TM29_filtered$depth,
    Day = TM29_filtered$Day,
    MgCa = TM29_filtered$MgCa - TM29_loess02$MgCa,
    SrCa = TM29_filtered$SrCa - TM29_loess02$SrCa,
    MnCa = TM29_filtered$MnCa - TM29_loess02$MnCa,
    BaCa = TM29_filtered$BaCa - TM29_loess02$BaCa
)

TM84b <- data.frame(
    depth = TM84_filtered$depth,
    Day = TM84_filtered$Day,
    MgCa = TM84_filtered$MgCa - TM84_loess02$MgCa,
    SrCa = TM84_filtered$SrCa - TM84_loess02$SrCa,
    MnCa = TM84_filtered$MnCa - TM84_loess02$MnCa,
    BaCa = TM84_filtered$BaCa - TM84_loess02$BaCa
)

TS85b <- data.frame(
    depth = TS85_filtered$depth,
    Day = TS85_filtered$Day,
    MgCa = TS85_filtered$MgCa - TS85_loess02$MgCa,
    SrCa = TS85_filtered$SrCa - TS85_loess02$SrCa,
    MnCa = TS85_filtered$MnCa - TS85_loess02$MnCa,
    BaCa = TS85_filtered$BaCa - TS85_loess02$BaCa
)

TSFRS1b <- data.frame(
    depth = TSFRS1_filtered$depth,
    Day = TSFRS1_filtered$Day,
    MgCa = TSFRS1_filtered$MgCa - TSFRS1_loess02$MgCa,
    SrCa = TSFRS1_filtered$SrCa - TSFRS1_loess02$SrCa,
    MnCa = TSFRS1_filtered$MnCa - TSFRS1_loess02$MnCa,
    BaCa = TSFRS1_filtered$BaCa - TSFRS1_loess02$BaCa
)

TSM1b <- data.frame(
    depth = TSM1_filtered$depth,
    Day = TSM1_filtered$Day,
    MgCa = TSM1_filtered$MgCa - TSM1_loess02$MgCa,
    SrCa = TSM1_filtered$SrCa - TSM1_loess02$SrCa,
    MnCa = TSM1_filtered$MnCa - TSM1_loess02$MnCa,
    BaCa = TSM1_filtered$BaCa - TSM1_loess02$BaCa
)

SQSA1b <- data.frame(
    depth = SQSA1_filtered$depth,
    Day = SQSA1_filtered$Day,
    MgCa = SQSA1_filtered$MgCa - SQSA1_loess02$MgCa,
    SrCa = SQSA1_filtered$SrCa - SQSA1_loess02$SrCa,
    MnCa = SQSA1_filtered$MnCa - SQSA1_loess02$MnCa,
    BaCa = SQSA1_filtered$BaCa - SQSA1_loess02$BaCa
)

PM2_1b$Specimen <- "P. maximus 2_1 smoothed"
PM2_2b$Specimen <- "P. maximus 2_2 smoothed"
PM3_1b$Specimen <- "P. maximus 3_1 smoothed"
PM3_2b$Specimen <- "P. maximus 3_2 smoothed"
PM4b$Specimen <- "P. maximus 4 smoothed"
TM29b$Specimen <- "T. maxima 29 smoothed"
TM84b$Specimen <- "T. maxima 84 smoothed"
TS85b$Specimen <- "T. squamosa 85 smoothed"
TSFRS1b$Specimen <- "T. squamosa FRS1 smoothed"
TSM1b$Specimen <- "T. squamosa M1 smoothed"
SQSA1b$Specimen <- "T. squamosina SQSA1 smoothed"

# Bandpass

# Load bandpass filter data
PM2_1_bp <- read.csv("<path>/PM2_1_bp.csv", header = TRUE) # Load Shell2_1 bp data
PM2_2_bp <- read.csv("<path>/PM2_2_bp.csv", header = TRUE) # Load Shell2_2 bp data
PM3_1_bp <- read.csv("<path>/PM3_1_bp.csv", header = TRUE) # Load Shell3_1 bp data
PM3_2_bp <- read.csv("<path>/PM3_2_bp.csv", header = TRUE) # Load Shell3_2 bp data
PM4_bp <- read.csv("<path>/PM4_bp.csv", header = TRUE) # Load Shell4 bp data
TM29_bp <- read.csv("<path>/TM29_bp.csv", header = TRUE) # Load maxima 29 bp data
TM84_bp <- read.csv("<path>/TM84_bp.csv", header = TRUE) # Load maxima 84 bp data
TS85_bp <- read.csv("<path>/TS85_bp.csv", header = TRUE) # Load squamosa 85 bp data
TSFRS1_bp <- read.csv("<path>/TSFRS1_bp.csv", header = TRUE) # Load squamosa FRS1 bp data
TSM1_bp <- read.csv("<path>/TSM1_bp.csv", header = TRUE) # Load squamosa M1 bp data
SQSA1_bp <- read.csv("<path>/SQSA1_bp.csv", header = TRUE) # Load squamosina SQSA1 bp data

colnames(PM2_2_bp)[4:7] <- c("SrCa_tidal", "MgCa_daily", "MnCa_daily", "BaCa_tidal")

# Add specimen columns
PM2_1_bp$Specimen <- "P. maximus 2_1 bp"
PM2_2_bp$Specimen <- "P. maximus 2_2 bp"
PM3_1_bp$Specimen <- "P. maximus 3_1 bp"
PM3_2_bp$Specimen <- "P. maximus 3_2 bp"
PM4_bp$Specimen <- "P. maximus 4 bp"
TM29_bp$Specimen <- "T. maxima 29 bp"
TM84_bp$Specimen <- "T. maxima 84 bp"
TS85_bp$Specimen <- "T. squamosa 85 bp"
TSFRS1_bp$Specimen <- "T. squamosa FRS1 bp"
TSM1_bp$Specimen <- "T. squamosa M1 bp"
SQSA1_bp$Specimen <- "T. squamosina SQSA1 bp"

# Calculate variances
PM2_1_var <- data.frame(
    Specimen = rep("P. maximus 2_1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(PM2_1[, which(colnames(PM2_1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM2_1[, which(colnames(PM2_1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(PM2_1_trim[, which(colnames(PM2_1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM2_1_trim[, which(colnames(PM2_1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(PM2_1_filtered[, which(colnames(PM2_1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM2_1_filtered[, which(colnames(PM2_1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(PM2_1b[, which(colnames(PM2_1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM2_1b[, which(colnames(PM2_1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
PM2_1_var$bp_daily <- NA
PM2_1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(PM2_1_var$record[i], "_daily", sep = "") %in% colnames(PM2_1_bp)){
        PM2_1_var$bp_daily[i] <- var(PM2_1_bp[, which(colnames(PM2_1_bp) == paste(PM2_1_var$record[i], "_daily", sep = ""))])
    }else{
        PM2_1_var$bp_daily[i] <- NA
    }
    if(paste(PM2_1_var$record[i], "_tidal", sep = "") %in% colnames(PM2_1_bp)){
        PM2_1_var$bp_tidal[i] <- var(PM2_1_bp[, which(colnames(PM2_1_bp) == paste(PM2_1_var$record[i], "_tidal", sep = ""))])
    }else{
        PM2_1_var$bp_tidal[i] <- NA
    }
}

PM2_2_var <- data.frame(
    Specimen = rep("P. maximus 2_2", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(PM2_2[, which(colnames(PM2_2) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM2_2[, which(colnames(PM2_2) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(PM2_2_trim[, which(colnames(PM2_2_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM2_2_trim[, which(colnames(PM2_2_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(PM2_2_filtered[, which(colnames(PM2_2_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM2_2_filtered[, which(colnames(PM2_2_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(PM2_2b[, which(colnames(PM2_2b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM2_2b[, which(colnames(PM2_2b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
PM2_2_var$bp_daily <- NA
PM2_2_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(PM2_2_var$record[i], "_daily", sep = "") %in% colnames(PM2_2_bp)){
        PM2_2_var$bp_daily[i] <- var(PM2_2_bp[, which(colnames(PM2_2_bp) == paste(PM2_2_var$record[i], "_daily", sep = ""))])
    }else{
        PM2_2_var$bp_daily[i] <- NA
    }
    if(paste(PM2_2_var$record[i], "_tidal", sep = "") %in% colnames(PM2_2_bp)){
        PM2_2_var$bp_tidal[i] <- var(PM2_2_bp[, which(colnames(PM2_2_bp) == paste(PM2_2_var$record[i], "_tidal", sep = ""))])
    }else{
        PM2_2_var$bp_tidal[i] <- NA
    }
}

PM3_1_var <- data.frame(
    Specimen = rep("P. maximus 3_1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(PM3_1[, which(colnames(PM3_1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM3_1[, which(colnames(PM3_1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(PM3_1_trim[, which(colnames(PM3_1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM3_1_trim[, which(colnames(PM3_1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(PM3_1_filtered[, which(colnames(PM3_1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM3_1_filtered[, which(colnames(PM3_1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(PM3_1b[, which(colnames(PM3_1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM3_1b[, which(colnames(PM3_1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
PM3_1_var$bp_daily <- NA
PM3_1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(PM3_1_var$record[i], "_daily", sep = "") %in% colnames(PM3_1_bp)){
        PM3_1_var$bp_daily[i] <- var(PM3_1_bp[, which(colnames(PM3_1_bp) == paste(PM3_1_var$record[i], "_daily", sep = ""))])
    }else{
        PM3_1_var$bp_daily[i] <- NA
    }
    if(paste(PM3_1_var$record[i], "_tidal", sep = "") %in% colnames(PM3_1_bp)){
        PM3_1_var$bp_tidal[i] <- var(PM3_1_bp[, which(colnames(PM3_1_bp) == paste(PM3_1_var$record[i], "_tidal", sep = ""))])
    }else{
        PM3_1_var$bp_tidal[i] <- NA
    }
}

PM3_2_var <- data.frame(
    Specimen = rep("P. maximus 3_2", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(PM3_2[, which(colnames(PM3_2) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM3_2[, which(colnames(PM3_2) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(PM3_2_trim[, which(colnames(PM3_2_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM3_2_trim[, which(colnames(PM3_2_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(PM3_2_filtered[, which(colnames(PM3_2_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM3_2_filtered[, which(colnames(PM3_2_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(PM3_2b[, which(colnames(PM3_2b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM3_2b[, which(colnames(PM3_2b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
PM3_2_var$bp_daily <- NA
PM3_2_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(PM3_2_var$record[i], "_daily", sep = "") %in% colnames(PM3_2_bp)){
        PM3_2_var$bp_daily[i] <- var(PM3_2_bp[, which(colnames(PM3_2_bp) == paste(PM3_2_var$record[i], "_daily", sep = ""))])
    }else{
        PM3_2_var$bp_daily[i] <- NA
    }
    if(paste(PM3_2_var$record[i], "_tidal", sep = "") %in% colnames(PM3_2_bp)){
        PM3_2_var$bp_tidal[i] <- var(PM3_2_bp[, which(colnames(PM3_2_bp) == paste(PM3_2_var$record[i], "_tidal", sep = ""))])
    }else{
        PM3_2_var$bp_tidal[i] <- NA
    }
}

PM4_var <- data.frame(
    Specimen = rep("P. maximus 4", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(PM4[, which(colnames(PM4) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM4[, which(colnames(PM4) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    trim = apply(PM4_trim[, which(colnames(PM4_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM4_trim[, which(colnames(PM4_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    filt = apply(PM4_filtered[, which(colnames(PM4_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM4_filtered[, which(colnames(PM4_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))],
    smooth = apply(PM4b[, which(colnames(PM4b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)[order(names(apply(PM4b[, which(colnames(PM4b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var)))]
)
PM4_var$bp_daily <- NA
PM4_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(PM4_var$record[i], "_daily", sep = "") %in% colnames(PM4_bp)){
        PM4_var$bp_daily[i] <- var(PM4_bp[, which(colnames(PM4_bp) == paste(PM4_var$record[i], "_daily", sep = ""))])
    }else{
        PM4_var$bp_daily[i] <- NA
    }
    if(paste(PM4_var$record[i], "_tidal", sep = "") %in% colnames(PM4_bp)){
        PM4_var$bp_tidal[i] <- var(PM4_bp[, which(colnames(PM4_bp) == paste(PM4_var$record[i], "_tidal", sep = ""))])
    }else{
        PM4_var$bp_tidal[i] <- NA
    }
}

TM29_var <- data.frame(
    Specimen = rep("T. maxima 29", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(TM29[, which(colnames(TM29) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TM29[, which(colnames(TM29) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(TM29_trim[, which(colnames(TM29_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TM29_trim[, which(colnames(TM29_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(TM29_filtered[, which(colnames(TM29_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TM29_filtered[, which(colnames(TM29_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(TM29b[, which(colnames(TM29b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TM29b[, which(colnames(TM29b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
TM29_var$bp_daily <- NA
TM29_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(TM29_var$record[i], "_daily", sep = "") %in% colnames(TM29_bp)){
        TM29_var$bp_daily[i] <- var(TM29_bp[, which(colnames(TM29_bp) == paste(TM29_var$record[i], "_daily", sep = ""))])
    }else{
        TM29_var$bp_daily[i] <- NA
    }
    if(paste(TM29_var$record[i], "_tidal", sep = "") %in% colnames(TM29_bp)){
        TM29_var$bp_tidal[i] <- var(TM29_bp[, which(colnames(TM29_bp) == paste(TM29_var$record[i], "_tidal", sep = ""))])
    }else{
        TM29_var$bp_tidal[i] <- NA
    }
}

TM84_var <- data.frame(
    Specimen = rep("T. maxima 84", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(TM84[, which(colnames(TM84) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TM84[, which(colnames(TM84) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(TM84_trim[, which(colnames(TM84_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TM84_trim[, which(colnames(TM84_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(TM84_filtered[, which(colnames(TM84_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TM84_filtered[, which(colnames(TM84_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(TM84b[, which(colnames(TM84b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TM84b[, which(colnames(TM84b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
TM84_var$bp_daily <- NA
TM84_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(TM84_var$record[i], "_daily", sep = "") %in% colnames(TM84_bp)){
        TM84_var$bp_daily[i] <- var(TM84_bp[, which(colnames(TM84_bp) == paste(TM84_var$record[i], "_daily", sep = ""))])
    }else{
        TM84_var$bp_daily[i] <- NA
    }
    if(paste(TM84_var$record[i], "_tidal", sep = "") %in% colnames(TM84_bp)){
        TM84_var$bp_tidal[i] <- var(TM84_bp[, which(colnames(TM84_bp) == paste(TM84_var$record[i], "_tidal", sep = ""))])
    }else{
        TM84_var$bp_tidal[i] <- NA
    }
}

TS85_var <- data.frame(
    Specimen = rep("T. squamosa 85", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(TS85[, which(colnames(TS85) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TS85[, which(colnames(TS85) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(TS85_trim[, which(colnames(TS85_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TS85_trim[, which(colnames(TS85_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(TS85_filtered[, which(colnames(TS85_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TS85_filtered[, which(colnames(TS85_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(TS85b[, which(colnames(TS85b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TS85b[, which(colnames(TS85b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
TS85_var$bp_daily <- NA
TS85_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(TS85_var$record[i], "_daily", sep = "") %in% colnames(TS85_bp)){
        TS85_var$bp_daily[i] <- var(TS85_bp[, which(colnames(TS85_bp) == paste(TS85_var$record[i], "_daily", sep = ""))])
    }else{
        TS85_var$bp_daily[i] <- NA
    }
    if(paste(TS85_var$record[i], "_tidal", sep = "") %in% colnames(TS85_bp)){
        TS85_var$bp_tidal[i] <- var(TS85_bp[, which(colnames(TS85_bp) == paste(TS85_var$record[i], "_tidal", sep = ""))])
    }else{
        TS85_var$bp_tidal[i] <- NA
    }
}

TSFRS1_var <- data.frame(
    Specimen = rep("T. squamosa FRS1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(TSFRS1[, which(colnames(TSFRS1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TSFRS1[, which(colnames(TSFRS1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(TSFRS1_trim[, which(colnames(TSFRS1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TSFRS1_trim[, which(colnames(TSFRS1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(TSFRS1_filtered[, which(colnames(TSFRS1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TSFRS1_filtered[, which(colnames(TSFRS1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(TSFRS1b[, which(colnames(TSFRS1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TSFRS1b[, which(colnames(TSFRS1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
TSFRS1_var$bp_daily <- NA
TSFRS1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(TSFRS1_var$record[i], "_daily", sep = "") %in% colnames(TSFRS1_bp)){
        TSFRS1_var$bp_daily[i] <- var(TSFRS1_bp[, which(colnames(TSFRS1_bp) == paste(TSFRS1_var$record[i], "_daily", sep = ""))])
    }else{
        TSFRS1_var$bp_daily[i] <- NA
    }
    if(paste(TSFRS1_var$record[i], "_tidal", sep = "") %in% colnames(TSFRS1_bp)){
        TSFRS1_var$bp_tidal[i] <- var(TSFRS1_bp[, which(colnames(TSFRS1_bp) == paste(TSFRS1_var$record[i], "_tidal", sep = ""))])
    }else{
        TSFRS1_var$bp_tidal[i] <- NA
    }
}

TSM1_var <- data.frame(
    Specimen = rep("T. squamosa M1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(TSM1[, which(colnames(TSM1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TSM1[, which(colnames(TSM1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(TSM1_trim[, which(colnames(TSM1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TSM1_trim[, which(colnames(TSM1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(TSM1_filtered[, which(colnames(TSM1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TSM1_filtered[, which(colnames(TSM1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(TSM1b[, which(colnames(TSM1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(TSM1b[, which(colnames(TSM1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
TSM1_var$bp_daily <- NA
TSM1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(TSM1_var$record[i], "_daily", sep = "") %in% colnames(TSM1_bp)){
        TSM1_var$bp_daily[i] <- var(TSM1_bp[, which(colnames(TSM1_bp) == paste(TSM1_var$record[i], "_daily", sep = ""))])
    }else{
        TSM1_var$bp_daily[i] <- NA
    }
    if(paste(TSM1_var$record[i], "_tidal", sep = "") %in% colnames(TSM1_bp)){
        TSM1_var$bp_tidal[i] <- var(TSM1_bp[, which(colnames(TSM1_bp) == paste(TSM1_var$record[i], "_tidal", sep = ""))])
    }else{
        TSM1_var$bp_tidal[i] <- NA
    }
}

SQSA1_var <- data.frame(
    Specimen = rep("T. squamosina SQSA1", 4),
    record = sort(c("MgCa", "SrCa", "MnCa", "BaCa")),
    raw = apply(SQSA1[, which(colnames(SQSA1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(SQSA1[, which(colnames(SQSA1) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    trim = apply(SQSA1_trim[, which(colnames(SQSA1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(SQSA1_trim[, which(colnames(SQSA1_trim) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    filt = apply(SQSA1_filtered[, which(colnames(SQSA1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(SQSA1_filtered[, which(colnames(SQSA1_filtered) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))],
    smooth = apply(SQSA1b[, which(colnames(SQSA1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)[order(names(apply(SQSA1b[, which(colnames(SQSA1b) %in% c("MgCa", "SrCa", "MnCa", "BaCa"))], 2, var, na.rm = TRUE)))]
)
SQSA1_var$bp_daily <- NA
SQSA1_var$bp_tidal <- NA
for(i in 1:4){
    if(paste(SQSA1_var$record[i], "_daily", sep = "") %in% colnames(SQSA1_bp)){
        SQSA1_var$bp_daily[i] <- var(SQSA1_bp[, which(colnames(SQSA1_bp) == paste(SQSA1_var$record[i], "_daily", sep = ""))])
    }else{
        SQSA1_var$bp_daily[i] <- NA
    }
    if(paste(SQSA1_var$record[i], "_tidal", sep = "") %in% colnames(SQSA1_bp)){
        SQSA1_var$bp_tidal[i] <- var(SQSA1_bp[, which(colnames(SQSA1_bp) == paste(SQSA1_var$record[i], "_tidal", sep = ""))])
    }else{
        SQSA1_var$bp_tidal[i] <- NA
    }
}

# Combine variance data for export
combined_variance <- rbind(PM2_1_var,
    PM2_2_var,
    PM3_1_var,
    PM3_2_var,
    PM4_var,
    TM29_var,
    TM84_var,
    TS85_var,
    TSFRS1_var,
    TSM1_var,
    SQSA1_var)

write.csv(combined_variance, "<path>/Variance_decomposition.csv")

# ------------------------------------------------------------------------------
# Example plot for specimen 2_1

# Combine and demean data for comparison
PM2_1_SrCa_all <- data.frame(
    SrCa = c(PM2_1[, 5] - median(PM2_1[, 5]),
        PM2_1_trim[, 5] - median(PM2_1_trim[, 5]),
        PM2_1_filtered[, 5] - median(PM2_1_filtered[, 5]),
        PM2_1b[, 4],
        PM2_1_bp[, 5],
        PM2_1_bp[, 4]),
    Dataset = c(rep("Full dataset", length(PM2_1[, 5])),
        rep("Trimmed dataset", length(PM2_1_trim[, 5])),
        rep("Filtered dataset", length(PM2_1_filtered[, 5])),
        rep("Smoothed dataset", length(PM2_1b[, 4])),
        rep("Daily variability", length(PM2_1_bp[, 5])),
        rep("Tidal (12h) variability", length(PM2_1_bp[, 4])))
)

# Order factor of categories
PM2_1_SrCa_all$Dataset <- factor(PM2_1_SrCa_all$Dataset, levels = unique(PM2_1_SrCa_all$Dataset))

# Create colorscale
Specimencolors <- c(brewer.pal(11, "RdBu")[11:7], "#000000", brewer.pal(11, "RdBu")[5:1])
names(Specimencolors) <- c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1", "PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")

# Datasetcolors <- rep(Specimencolors[7], 6)
Datasetcolors <- brewer.pal(6, "Greens")
names(Datasetcolors) <- unique(PM2_1_SrCa_all$Dataset)

Varlabels <- data.frame(Dataset = as.character(unique(PM2_1_SrCa_all$Dataset)),
    SrCa = rep(-1, 6),
    var = as.numeric(PM2_1_var[4, -(1:2)]))

SrCa_2_1_varplot <- ggplot(data = PM2_1_SrCa_all) +
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

Varcomp_daily_bar <- ggplot(data = var_decomp_summary2[which(var_decomp_summary2$daily_rel > 0), ],
        aes(x = record,
            y = daily_rel,
            fill = Specimen),
        color = NA) +
    geom_bar(stat="identity",
        width = 1,
        position = position_dodge2(width = 0.9, preserve = "single")) +
    scale_fill_manual(values = Specimencolors) +
    scale_y_continuous("Percentage of variance (%)",
        breaks = seq(0, 0.2, 0.05),
        labels = seq(0, 20, 5),
        minor_breaks = seq(0, 0.2, 0.01),
        limits = c(0, 0.21)) +
    scale_x_discrete("") +
    ggtitle("Daily variability") +
    theme_classic()

Varcomp_tidal_bar <- ggplot(data = var_decomp_summary2[which(var_decomp_summary2$tidal_rel > 0), ],
        aes(x = record,
            y = tidal_rel,
            fill = Specimen),
        color = NA) +
    geom_bar(stat="identity",
        width = 1,
        position = position_dodge2(width = 0.9, preserve = "single")) +
    scale_fill_manual(values = Specimencolors) +
    scale_y_continuous("Percentage of variance (%)",
        breaks = seq(0, 0.2, 0.05),
        labels = seq(0, 20, 5),
        minor_breaks = seq(0, 0.2, 0.01),
        limits = c(0, 0.21)) +
    scale_x_discrete("") +
    ggtitle("Tidal variability") +
    theme_classic()

combined_varcomp_bar_summary <- ggarrange(Varcomp_daily_bar,
    Varcomp_tidal_bar,
    ncol = 2,
    legend = "right",
    common.legend = TRUE)

# ------------------------------------------------------------------------------
# As scatterplot

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
