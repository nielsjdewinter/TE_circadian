# Script used to generate overview plots of TE records (Figure 2)

# ------------------------------------------------------------------------------
# PREPARE DATA

require(tidyverse)
require(RColorBrewer)
require(gridExtra)
require(signal)
require(ggpubr)

devtools::install_github("zeehio/facetscales") # Install facetscales package
library(facetscales)

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
PM2_1$Specimen <- "PM2_1"
PM2_2$Specimen <- "PM2_2"
PM3_1$Specimen <- "PM3_1"
PM3_2$Specimen <- "PM3_2"
PM4$Specimen <- "PM4"
TM29$Specimen <- "TM29"
TM84$Specimen <- "TM84"
TS85$Specimen <- "TS85"
TSFRS1$Specimen <- "TSFRS1"
TSM1$Specimen <- "TSM1"
SQSA1$Specimen <- "SQSA1"

PM2_1_trim$Specimen <- "PM2_1 trim"
PM2_2_trim$Specimen <- "PM2_2 trim"
PM3_1_trim$Specimen <- "PM3_1 trim"
PM3_2_trim$Specimen <- "PM3_2 trim"
PM4_trim$Specimen <- "PM4 trim"
TM29_trim$Specimen <- "TM29 trim"
TM84_trim$Specimen <- "TM84 trim"
TS85_trim$Specimen <- "TS85 trim"
TSFRS1_trim$Specimen <- "TSFRS1 trim"
TSM1_trim$Specimen <- "TSM1 trim"
SQSA1_trim$Specimen <- "SQSA1 trim"

# Filtering

# Apply Savitzky-Golay filters
PM2_1_trim2 <- PM2_1_trim
PM2_1_trim2$BaCa[PM2_1_trim2$BaCa == 0] <- mean(PM2_1_trim2$BaCa[PM2_1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM2_1_filtered <- as.data.frame(cbind(PM2_1_trim2$Depth,
    PM2_1_trim2$Day,
    apply(PM2_1_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM2_1_filtered)[c(1, 2)] <- c("Depth", "Day")
PM2_1_filtered$datetime <- PM2_1_trim2$datetime

PM2_2_trim2 <- PM2_2_trim
PM2_2_trim2$BaCa[PM2_2_trim2$BaCa == 0] <- mean(PM2_2_trim2$BaCa[PM2_2_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM2_2_filtered <- as.data.frame(cbind(PM2_2_trim2$Depth,
    PM2_2_trim2$Day,
    apply(PM2_2_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM2_2_filtered)[c(1, 2)] <- c("Depth", "Day")
PM2_2_filtered$datetime <- PM2_2_trim2$datetime

PM3_1_trim2 <- PM3_1_trim
PM3_1_trim2$BaCa[PM3_1_trim2$BaCa == 0] <- mean(PM3_1_trim2$BaCa[PM3_1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM3_1_filtered <- as.data.frame(cbind(PM3_1_trim2$Depth,
    PM3_1_trim2$Day,
    apply(PM3_1_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM3_1_filtered)[c(1, 2)] <- c("Depth", "Day")
PM3_1_filtered$datetime <- PM3_1_trim2$datetime

PM3_2_trim2 <- PM3_2_trim
PM3_2_trim2$BaCa[PM3_2_trim2$BaCa == 0] <- mean(PM3_2_trim2$BaCa[PM3_2_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM3_2_filtered <- as.data.frame(cbind(PM3_2_trim2$Depth,
    PM3_2_trim2$Day,
    apply(PM3_2_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM3_2_filtered)[c(1, 2)] <- c("Depth", "Day")
PM3_2_filtered$datetime <- PM3_2_trim2$datetime

PM4_trim2 <- PM4_trim
PM4_trim2$BaCa[PM4_trim2$BaCa == 0] <- mean(PM4_trim2$BaCa[PM4_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

PM4_filtered <- as.data.frame(cbind(PM4_trim2$Depth,
    PM4_trim2$Day,
    apply(PM4_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
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
    TM29_trim2$Day,
    apply(TM29_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TM29_filtered)[c(1, 2)] <- c("depth", "Day")

TM84_trim2 <- TM84_trim
TM84_trim2$BaCa[TM84_trim2$BaCa == 0] <- mean(TM84_trim2$BaCa[TM84_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
TM84_trim2$SrCa[is.na(TM84_trim2$SrCa)] <- mean(TM84_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
TM84_trim2$MnCa[is.na(TM84_trim2$MnCa)] <- mean(TM84_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
TM84_trim2$MgCa[is.na(TM84_trim2$MgCa)] <- mean(TM84_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
TM84_trim2$BaCa[is.na(TM84_trim2$BaCa)] <- mean(TM84_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

TM84_filtered <- as.data.frame(cbind(TM84_trim2$depth,
    TM84_trim2$Day,
    apply(TM84_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TM84_filtered)[c(1, 2)] <- c("depth", "Day")

TS85_trim2 <- TS85_trim
TS85_trim2$BaCa[TS85_trim2$BaCa == 0] <- mean(TS85_trim2$BaCa[TS85_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
TS85_trim2$SrCa[is.na(TS85_trim2$SrCa)] <- mean(TS85_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
TS85_trim2$MnCa[is.na(TS85_trim2$MnCa)] <- mean(TS85_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
TS85_trim2$MgCa[is.na(TS85_trim2$MgCa)] <- mean(TS85_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
TS85_trim2$BaCa[is.na(TS85_trim2$BaCa)] <- mean(TS85_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

TS85_filtered <- as.data.frame(cbind(TS85_trim2$depth,
    TS85_trim2$Day,
    apply(TS85_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TS85_filtered)[c(1, 2)] <- c("depth", "Day")

TSFRS1_trim2 <- TSFRS1_trim
TSFRS1_trim2$BaCa[TSFRS1_trim2$BaCa == 0] <- mean(TSFRS1_trim2$BaCa[TSFRS1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
TSFRS1_trim2$SrCa[is.na(TSFRS1_trim2$SrCa)] <- mean(TSFRS1_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
TSFRS1_trim2$MnCa[is.na(TSFRS1_trim2$MnCa)] <- mean(TSFRS1_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
TSFRS1_trim2$MgCa[is.na(TSFRS1_trim2$MgCa)] <- mean(TSFRS1_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
TSFRS1_trim2$BaCa[is.na(TSFRS1_trim2$BaCa)] <- mean(TSFRS1_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

TSFRS1_filtered <- as.data.frame(cbind(TSFRS1_trim2$depth,
    TSFRS1_trim2$Day,
    apply(TSFRS1_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TSFRS1_filtered)[c(1, 2)] <- c("depth", "Day")

TSM1_trim2 <- TSM1_trim
TSM1_trim2$BaCa[TSM1_trim2$BaCa == 0] <- mean(TSM1_trim2$BaCa[TSM1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
TSM1_trim2$SrCa[is.na(TSM1_trim2$SrCa)] <- mean(TSM1_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
TSM1_trim2$MnCa[is.na(TSM1_trim2$MnCa)] <- mean(TSM1_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
TSM1_trim2$MgCa[is.na(TSM1_trim2$MgCa)] <- mean(TSM1_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
TSM1_trim2$BaCa[is.na(TSM1_trim2$BaCa)] <- mean(TSM1_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

TSM1_filtered <- as.data.frame(cbind(TSM1_trim2$depth,
    TSM1_trim2$Day,
    apply(TSM1_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TSM1_filtered)[c(1, 2)] <- c("depth", "Day")

SQSA1_trim2 <- SQSA1_trim
SQSA1_trim2$BaCa[SQSA1_trim2$BaCa == 0] <- mean(SQSA1_trim2$BaCa[SQSA1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
SQSA1_trim2$SrCa[is.na(SQSA1_trim2$SrCa)] <- mean(SQSA1_trim2$SrCa, na.rm = TRUE) # Remove NAs from SrCa data for filtering and smoothing
SQSA1_trim2$MnCa[is.na(SQSA1_trim2$MnCa)] <- mean(SQSA1_trim2$MnCa, na.rm = TRUE) # Remove NAs from MnCa data for filtering and smoothing
SQSA1_trim2$MgCa[is.na(SQSA1_trim2$MgCa)] <- mean(SQSA1_trim2$MgCa, na.rm = TRUE) # Remove NAs from MgCa data for filtering and smoothing
SQSA1_trim2$BaCa[is.na(SQSA1_trim2$BaCa)] <- mean(SQSA1_trim2$BaCa, na.rm = TRUE) # Remove NAs from BaCa data for filtering and smoothing

SQSA1_filtered <- as.data.frame(cbind(SQSA1_trim2$depth,
    SQSA1_trim2$Day,
    apply(SQSA1_trim2[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(SQSA1_filtered)[c(1, 2)] <- c("depth", "Day")

# Add specimen columns
PM2_1_filtered$Specimen <- "PM2_1 filtered"
PM2_2_filtered$Specimen <- "PM2_2 filtered"
PM3_1_filtered$Specimen <- "PM3_1 filtered"
PM3_2_filtered$Specimen <- "PM3_2 filtered"
PM4_filtered$Specimen <- "PM4 filtered"
TM29_filtered$Specimen <- "TM29 filtered"
TM84_filtered$Specimen <- "TM84 filtered"
TS85_filtered$Specimen <- "TS85 filtered"
TSFRS1_filtered$Specimen <- "TSFRS1 filtered"
TSM1_filtered$Specimen <- "TSM1 filtered"
SQSA1_filtered$Specimen <- "SQSA1 filtered"

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

PM2_1_loess02$Specimen <- "PM2_1 loess02"
PM2_2_loess02$Specimen <- "PM2_2 loess02"
PM3_1_loess02$Specimen <- "PM3_1 loess02"
PM3_2_loess02$Specimen <- "PM3_2 loess02"
PM4_loess02$Specimen <- "PM4 loess02"
TM29_loess02$Specimen <- "TM29 loess02"
TM84_loess02$Specimen <- "TM84 loess02"
TS85_loess02$Specimen <- "TS85 loess02"
TSFRS1_loess02$Specimen <- "TSFRS1 loess02"
TSM1_loess02$Specimen <- "TSM1 loess02"
SQSA1_loess02$Specimen <- "SQSA1 loess02"

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

PM2_1b$Specimen <- "PM2_1 smoothed"
PM2_2b$Specimen <- "PM2_2 smoothed"
PM3_1b$Specimen <- "PM3_1 smoothed"
PM3_2b$Specimen <- "PM3_2 smoothed"
PM4b$Specimen <- "PM4 smoothed"
TM29b$Specimen <- "TM29 smoothed"
TM84b$Specimen <- "TM84 smoothed"
TS85b$Specimen <- "TS85 smoothed"
TSFRS1b$Specimen <- "TSFRS1 smoothed"
TSM1b$Specimen <- "TSM1 smoothed"
SQSA1b$Specimen <- "SQSA1 smoothed"


# ------------------------------------------------------------------------------
# Combined records
combined_raw <- data.frame(
    Depth = c(PM2_1_trim$Depth,
        PM2_2_trim$Depth,
        PM3_1_trim$Depth,
        PM3_2_trim$Depth,
        PM4_trim$Depth,
        TM29_trim$depth * 1000,
        TM84_trim$depth * 1000,
        TS85_trim$depth * 1000,
        TSFRS1_trim$depth * 1000,
        TSM1_trim$depth * 1000,
        SQSA1_trim$depth * 1000),
    Day = c(PM2_1_trim$Day,
        PM2_2_trim$Day,
        PM3_1_trim$Day,
        PM3_2_trim$Day,
        PM4_trim$Day,
        TM29_trim$Day,
        TM84_trim$Day,
        TS85_trim$Day,
        TSFRS1_trim$Day,
        TSM1_trim$Day,
        SQSA1_trim$Day),
    MgCa = c(PM2_1_trim$MgCa,
        PM2_2_trim$MgCa,
        PM3_1_trim$MgCa,
        PM3_2_trim$MgCa,
        PM4_trim$MgCa,
        TM29_trim$MgCa,
        TM84_trim$MgCa,
        TS85_trim$MgCa,
        TSFRS1_trim$MgCa,
        TSM1_trim$MgCa,
        SQSA1_trim$MgCa),
    SrCa = c(PM2_1_trim$SrCa,
        PM2_2_trim$SrCa,
        PM3_1_trim$SrCa,
        PM3_2_trim$SrCa,
        PM4_trim$SrCa,
        TM29_trim$SrCa,
        TM84_trim$SrCa,
        TS85_trim$SrCa,
        TSFRS1_trim$SrCa,
        TSM1_trim$SrCa,
        SQSA1_trim$SrCa),
    MnCa = c(PM2_1_trim$MnCa,
        PM2_2_trim$MnCa,
        PM3_1_trim$MnCa,
        PM3_2_trim$MnCa,
        PM4_trim$MnCa,
        TM29_trim$MnCa,
        TM84_trim$MnCa,
        TS85_trim$MnCa,
        TSFRS1_trim$MnCa,
        TSM1_trim$MnCa,
        SQSA1_trim$MnCa),
    BaCa = c(PM2_1_trim$BaCa,
        PM2_2_trim$BaCa,
        PM3_1_trim$BaCa,
        PM3_2_trim$BaCa,
        PM4_trim$BaCa,
        TM29_trim$BaCa,
        TM84_trim$BaCa,
        TS85_trim$BaCa,
        TSFRS1_trim$BaCa,
        TSM1_trim$BaCa,
        SQSA1_trim$BaCa),
    Specimen = c(PM2_1_trim$Specimen,
        PM2_2_trim$Specimen,
        PM3_1_trim$Specimen,
        PM3_2_trim$Specimen,
        PM4_trim$Specimen,
        TM29_trim$Specimen,
        TM84_trim$Specimen,
        TS85_trim$Specimen,
        TSFRS1_trim$Specimen,
        TSM1_trim$Specimen,
        SQSA1_trim$Specimen)
)

combined_smoothed <- data.frame(
    Depth = c(PM2_1b$Depth,
        PM2_2b$Depth,
        PM3_1b$Depth,
        PM3_2b$Depth,
        PM4b$Depth,
        TM29b$depth * 1000,
        TM84b$depth * 1000,
        TS85b$depth * 1000,
        TSFRS1b$depth * 1000,
        TSM1b$depth * 1000,
        SQSA1b$depth * 1000),
    Day = c(PM2_1b$Day,
        PM2_2b$Day,
        PM3_1b$Day,
        PM3_2b$Day,
        PM4b$Day,
        TM29b$Day,
        TM84b$Day,
        TS85b$Day,
        TSFRS1b$Day,
        TSM1b$Day,
        SQSA1b$Day),
    MgCa = c(PM2_1b$MgCa,
        PM2_2b$MgCa,
        PM3_1b$MgCa,
        PM3_2b$MgCa,
        PM4b$MgCa,
        TM29b$MgCa,
        TM84b$MgCa,
        TS85b$MgCa,
        TSFRS1b$MgCa,
        TSM1b$MgCa,
        SQSA1b$MgCa),
    SrCa = c(PM2_1b$SrCa,
        PM2_2b$SrCa,
        PM3_1b$SrCa,
        PM3_2b$SrCa,
        PM4b$SrCa,
        TM29b$SrCa,
        TM84b$SrCa,
        TS85b$SrCa,
        TSFRS1b$SrCa,
        TSM1b$SrCa,
        SQSA1b$SrCa),
    MnCa = c(PM2_1b$MnCa,
        PM2_2b$MnCa,
        PM3_1b$MnCa,
        PM3_2b$MnCa,
        PM4b$MnCa,
        TM29b$MnCa,
        TM84b$MnCa,
        TS85b$MnCa,
        TSFRS1b$MnCa,
        TSM1b$MnCa,
        SQSA1b$MnCa),
    BaCa = c(PM2_1b$BaCa,
        PM2_2b$BaCa,
        PM3_1b$BaCa,
        PM3_2b$BaCa,
        PM4b$BaCa,
        TM29b$BaCa,
        TM84b$BaCa,
        TS85b$BaCa,
        TSFRS1b$BaCa,
        TSM1b$BaCa,
        SQSA1b$BaCa),
    Specimen = c(PM2_1b$Specimen,
        PM2_2b$Specimen,
        PM3_1b$Specimen,
        PM3_2b$Specimen,
        PM4b$Specimen,
        TM29b$Specimen,
        TM84b$Specimen,
        TS85b$Specimen,
        TSFRS1b$Specimen,
        TSM1b$Specimen,
        SQSA1b$Specimen)
)


combined_filtered <- data.frame(
    Depth = c(PM2_1_filtered$Depth,
        PM2_2_filtered$Depth,
        PM3_1_filtered$Depth,
        PM3_2_filtered$Depth,
        PM4_filtered$Depth,
        TM29_filtered$depth * 1000,
        TM84_filtered$depth * 1000,
        TS85_filtered$depth * 1000,
        TSFRS1_filtered$depth * 1000,
        TSM1_filtered$depth * 1000,
        SQSA1_filtered$depth * 1000),
    Day = c(PM2_1_filtered$Day,
        PM2_2_filtered$Day,
        PM3_1_filtered$Day,
        PM3_2_filtered$Day,
        PM4_filtered$Day,
        TM29_filtered$Day,
        TM84_filtered$Day,
        TS85_filtered$Day,
        TSFRS1_filtered$Day,
        TSM1_filtered$Day,
        SQSA1_filtered$Day),
    MgCa = c(PM2_1_filtered$MgCa,
        PM2_2_filtered$MgCa,
        PM3_1_filtered$MgCa,
        PM3_2_filtered$MgCa,
        PM4_filtered$MgCa,
        TM29_filtered$MgCa,
        TM84_filtered$MgCa,
        TS85_filtered$MgCa,
        TSFRS1_filtered$MgCa,
        TSM1_filtered$MgCa,
        SQSA1_filtered$MgCa),
    SrCa = c(PM2_1_filtered$SrCa,
        PM2_2_filtered$SrCa,
        PM3_1_filtered$SrCa,
        PM3_2_filtered$SrCa,
        PM4_filtered$SrCa,
        TM29_filtered$SrCa,
        TM84_filtered$SrCa,
        TS85_filtered$SrCa,
        TSFRS1_filtered$SrCa,
        TSM1_filtered$SrCa,
        SQSA1_filtered$SrCa),
    MnCa = c(PM2_1_filtered$MnCa,
        PM2_2_filtered$MnCa,
        PM3_1_filtered$MnCa,
        PM3_2_filtered$MnCa,
        PM4_filtered$MnCa,
        TM29_filtered$MnCa,
        TM84_filtered$MnCa,
        TS85_filtered$MnCa,
        TSFRS1_filtered$MnCa,
        TSM1_filtered$MnCa,
        SQSA1_filtered$MnCa),
    BaCa = c(PM2_1_filtered$BaCa,
        PM2_2_filtered$BaCa,
        PM3_1_filtered$BaCa,
        PM3_2_filtered$BaCa,
        PM4_filtered$BaCa,
        TM29_filtered$BaCa,
        TM84_filtered$BaCa,
        TS85_filtered$BaCa,
        TSFRS1_filtered$BaCa,
        TSM1_filtered$BaCa,
        SQSA1_filtered$BaCa),
    Specimen = c(PM2_1_filtered$Specimen,
        PM2_2_filtered$Specimen,
        PM3_1_filtered$Specimen,
        PM3_2_filtered$Specimen,
        PM4_filtered$Specimen,
        TM29_filtered$Specimen,
        TM84_filtered$Specimen,
        TS85_filtered$Specimen,
        TSFRS1_filtered$Specimen,
        TSM1_filtered$Specimen,
        SQSA1_filtered$Specimen)
)

combined_loess02 <- data.frame(
    Depth = c(PM2_1_loess02$Depth,
        PM2_2_loess02$Depth,
        PM3_1_loess02$Depth,
        PM3_2_loess02$Depth,
        PM4_loess02$Depth,
        TM29_loess02$depth * 1000,
        TM84_loess02$depth * 1000,
        TS85_loess02$depth * 1000,
        TSFRS1_loess02$depth * 1000,
        TSM1_loess02$depth * 1000,
        SQSA1_loess02$depth * 1000),
    Day = c(PM2_1_loess02$Day,
        PM2_2_loess02$Day,
        PM3_1_loess02$Day,
        PM3_2_loess02$Day,
        PM4_loess02$Day,
        TM29_loess02$Day,
        TM84_loess02$Day,
        TS85_loess02$Day,
        TSFRS1_loess02$Day,
        TSM1_loess02$Day,
        SQSA1_loess02$Day),
    MgCa = c(PM2_1_loess02$MgCa,
        PM2_2_loess02$MgCa,
        PM3_1_loess02$MgCa,
        PM3_2_loess02$MgCa,
        PM4_loess02$MgCa,
        TM29_loess02$MgCa,
        TM84_loess02$MgCa,
        TS85_loess02$MgCa,
        TSFRS1_loess02$MgCa,
        TSM1_loess02$MgCa,
        SQSA1_loess02$MgCa),
    SrCa = c(PM2_1_loess02$SrCa,
        PM2_2_loess02$SrCa,
        PM3_1_loess02$SrCa,
        PM3_2_loess02$SrCa,
        PM4_loess02$SrCa,
        TM29_loess02$SrCa,
        TM84_loess02$SrCa,
        TS85_loess02$SrCa,
        TSFRS1_loess02$SrCa,
        TSM1_loess02$SrCa,
        SQSA1_loess02$SrCa),
    MnCa = c(PM2_1_loess02$MnCa,
        PM2_2_loess02$MnCa,
        PM3_1_loess02$MnCa,
        PM3_2_loess02$MnCa,
        PM4_loess02$MnCa,
        TM29_loess02$MnCa,
        TM84_loess02$MnCa,
        TS85_loess02$MnCa,
        TSFRS1_loess02$MnCa,
        TSM1_loess02$MnCa,
        SQSA1_loess02$MnCa),
    BaCa = c(PM2_1_loess02$BaCa,
        PM2_2_loess02$BaCa,
        PM3_1_loess02$BaCa,
        PM3_2_loess02$BaCa,
        PM4_loess02$BaCa,
        TM29_loess02$BaCa,
        TM84_loess02$BaCa,
        TS85_loess02$BaCa,
        TSFRS1_loess02$BaCa,
        TSM1_loess02$BaCa,
        SQSA1_loess02$BaCa),
    Specimen = c(PM2_1_loess02$Specimen,
        PM2_2_loess02$Specimen,
        PM3_1_loess02$Specimen,
        PM3_2_loess02$Specimen,
        PM4_loess02$Specimen,
        TM29_loess02$Specimen,
        TM84_loess02$Specimen,
        TS85_loess02$Specimen,
        TSFRS1_loess02$Specimen,
        TSM1_loess02$Specimen,
        SQSA1_loess02$Specimen)
)

combined_filtered_offset <- data.frame(
    Depth = c(PM2_1_filtered$Depth,
        PM2_2_filtered$Depth,
        PM3_1_filtered$Depth,
        PM3_2_filtered$Depth,
        PM4_filtered$Depth,
        TM29_filtered$depth * 1000,
        TM84_filtered$depth * 1000,
        TS85_filtered$depth * 1000,
        TSFRS1_filtered$depth * 1000,
        TSM1_filtered$depth * 1000,
        SQSA1_filtered$depth * 1000),
    Day = c(PM2_1_filtered$Day,
        PM2_2_filtered$Day,
        PM3_1_filtered$Day,
        PM3_2_filtered$Day,
        PM4_filtered$Day,
        TM29_filtered$Day,
        TM84_filtered$Day,
        TS85_filtered$Day,
        TSFRS1_filtered$Day,
        TSM1_filtered$Day,
        SQSA1_filtered$Day),
    MgCa = c(PM2_1_filtered$MgCa,
        PM2_2_filtered$MgCa + 3,
        PM3_1_filtered$MgCa + 6,
        PM3_2_filtered$MgCa + 9,
        PM4_filtered$MgCa + 12,
        TM29_filtered$MgCa + 18,
        TM84_filtered$MgCa + 21,
        TS85_filtered$MgCa + 24,
        TSFRS1_filtered$MgCa + 27,
        TSM1_filtered$MgCa + 30,
        SQSA1_filtered$MgCa + 33),
    SrCa = c(PM2_1_filtered$SrCa,
        PM2_2_filtered$SrCa + 1.5,
        PM3_1_filtered$SrCa + 3,
        PM3_2_filtered$SrCa + 4.5,
        PM4_filtered$SrCa + 6,
        TM29_filtered$SrCa + 7.5,
        TM84_filtered$SrCa + 9,
        TS85_filtered$SrCa + 10.5,
        TSFRS1_filtered$SrCa + 12,
        TSM1_filtered$SrCa + 13.5,
        SQSA1_filtered$SrCa + 15),
    MnCa = c(PM2_1_filtered$MnCa,
        PM2_2_filtered$MnCa + .02,
        PM3_1_filtered$MnCa + .04,
        PM3_2_filtered$MnCa + .06,
        PM4_filtered$MnCa + .08,
        TM29_filtered$MnCa + .1,
        TM84_filtered$MnCa + .12,
        TS85_filtered$MnCa + .14,
        TSFRS1_filtered$MnCa + .16,
        TSM1_filtered$MnCa + .18,
        SQSA1_filtered$MnCa + .2),
    BaCa = c(PM2_1_filtered$BaCa,
        PM2_2_filtered$BaCa + .01,
        PM3_1_filtered$BaCa + .02,
        PM3_2_filtered$BaCa + .03,
        PM4_filtered$BaCa + .04,
        TM29_filtered$BaCa + .05,
        TM84_filtered$BaCa + .06,
        TS85_filtered$BaCa + .07,
        TSFRS1_filtered$BaCa + .08,
        TSM1_filtered$BaCa + .09,
        SQSA1_filtered$BaCa + .1),
    Specimen = c(PM2_1_filtered$Specimen,
        PM2_2_filtered$Specimen,
        PM3_1_filtered$Specimen,
        PM3_2_filtered$Specimen,
        PM4_filtered$Specimen,
        TM29_filtered$Specimen,
        TM84_filtered$Specimen,
        TS85_filtered$Specimen,
        TSFRS1_filtered$Specimen,
        TSM1_filtered$Specimen,
        SQSA1_filtered$Specimen)
)

combined_loess02_offset <- data.frame(
    Depth = c(PM2_1_loess02$Depth,
        PM2_2_loess02$Depth,
        PM3_1_loess02$Depth,
        PM3_2_loess02$Depth,
        PM4_loess02$Depth,
        TM29_loess02$depth * 1000,
        TM84_loess02$depth * 1000,
        TS85_loess02$depth * 1000,
        TSFRS1_loess02$depth * 1000,
        TSM1_loess02$depth * 1000,
        SQSA1_loess02$depth * 1000),
    Day = c(PM2_1_loess02$Day,
        PM2_2_loess02$Day,
        PM3_1_loess02$Day,
        PM3_2_loess02$Day,
        PM4_loess02$Day,
        TM29_loess02$Day,
        TM84_loess02$Day,
        TS85_loess02$Day,
        TSFRS1_loess02$Day,
        TSM1_loess02$Day,
        SQSA1_loess02$Day),
    MgCa = c(PM2_1_loess02$MgCa,
        PM2_2_loess02$MgCa + 3,
        PM3_1_loess02$MgCa + 6,
        PM3_2_loess02$MgCa + 9,
        PM4_loess02$MgCa + 12,
        TM29_loess02$MgCa + 18,
        TM84_loess02$MgCa + 21,
        TS85_loess02$MgCa + 24,
        TSFRS1_loess02$MgCa + 27,
        TSM1_loess02$MgCa + 30,
        SQSA1_loess02$MgCa + 33),
    SrCa = c(PM2_1_loess02$SrCa,
        PM2_2_loess02$SrCa + 1.5,
        PM3_1_loess02$SrCa + 3,
        PM3_2_loess02$SrCa + 4.5,
        PM4_loess02$SrCa + 6,
        TM29_loess02$SrCa + 7.5,
        TM84_loess02$SrCa + 9,
        TS85_loess02$SrCa + 10.5,
        TSFRS1_loess02$SrCa + 12,
        TSM1_loess02$SrCa + 13.5,
        SQSA1_loess02$SrCa + 15),
    MnCa = c(PM2_1_loess02$MnCa,
        PM2_2_loess02$MnCa + .02,
        PM3_1_loess02$MnCa + .04,
        PM3_2_loess02$MnCa + .06,
        PM4_loess02$MnCa + .08,
        TM29_loess02$MnCa + .1,
        TM84_loess02$MnCa + .12,
        TS85_loess02$MnCa + .14,
        TSFRS1_loess02$MnCa + .16,
        TSM1_loess02$MnCa + .18,
        SQSA1_loess02$MnCa + .2),
    BaCa = c(PM2_1_loess02$BaCa,
        PM2_2_loess02$BaCa + .01,
        PM3_1_loess02$BaCa + .02,
        PM3_2_loess02$BaCa + .03,
        PM4_loess02$BaCa + .04,
        TM29_loess02$BaCa + .05,
        TM84_loess02$BaCa + .06,
        TS85_loess02$BaCa + .07,
        TSFRS1_loess02$BaCa + .08,
        TSM1_loess02$BaCa + .09,
        SQSA1_loess02$BaCa + .1),
    Specimen = c(PM2_1_loess02$Specimen,
        PM2_2_loess02$Specimen,
        PM3_1_loess02$Specimen,
        PM3_2_loess02$Specimen,
        PM4_loess02$Specimen,
        TM29_loess02$Specimen,
        TM84_loess02$Specimen,
        TS85_loess02$Specimen,
        TSFRS1_loess02$Specimen,
        TSM1_loess02$Specimen,
        SQSA1_loess02$Specimen)
)

# Remove end of Specimen strings
combined_loess02_offset$Specimen <- combined_filtered_offset$Specimen <- combined_loess02$Specimen <- combined_filtered$Specimen <- combined_smoothed$Specimen <- combined_raw$Specimen <- gsub(pattern = " trim", replacement = "", x = combined_raw$Specimen)

# # Plot all records

# # Create colorscale
# Specimencolors <- c(brewer.pal(11, "RdBu")[11:7], "#000000", brewer.pal(11, "RdBu")[5:1])
# names(Specimencolors) <- c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1", "PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")

# SrCa_overview_plot <- ggplot(data = combined_raw) +
# #    geom_point(aes(Day, SrCa, col = Specimen),
# #        size = 0.1,
# #        alpha = 0.01,
# #        ) +
#     geom_line(data = combined_filtered_offset,
#         aes(Day, SrCa, col = Specimen),
#         size = 0.1,
#         alpha = 0.1) +
#     geom_line(data = combined_loess02_offset,
#         aes(Day, SrCa, col = Specimen),
#         size = 0.1,
#         alpha = 1) +
#     scale_y_continuous("Sr/Ca [mmol/mol]",
#         limits = c(0, 17.5),
#         breaks = c(seq(0, 17.5, 0.5)),
#         labels = c(seq(0, 2, 0.5),
#             rep("", 2),
#             seq(0.5, 2, 0.5),
#             rep("", 2),
#             seq(0.5, 2, 0.5),
#             rep("", 3),
#             seq(1, 2.5, 0.5),
#             rep("", 2),
#             seq(1, 2.5, 0.5),
#             rep("", 2),
#             seq(1, 2.5, 0.5)),
#         sec.axis = sec_axis(~.,
#             name = "Sr/Ca [mmol/mol]",
#             breaks = c(seq(2, 3.5, 0.5),
#                 seq(5, 6.5, 0.5),
#                 seq(8.5, 10, 0.5),
#                 seq(11.5, 13, 0.5),
#                 seq(14.5, 16, 0.5)),
#             labels = c(seq(0.5, 2, 0.5),
#                 seq(0.5, 2, 0.5),
#                 seq(1, 2.5, 0.5),
#                 seq(1, 2.5, 0.5),
#                 seq(1, 2.5, 0.5)))) +
#     scale_x_continuous("Age (days)",
#         breaks = seq(0, 1100, 100),
#         sec.axis = sec_axis(~.,
#             name = "Age (years)",
#             breaks = seq(0, 3 * 365, 365 / 2),
#             labels = seq(0, 3, 1 / 2))) +
#     geom_hline(yintercept = c(seq(2, 8, 1.5), seq(8.5, 16, 1.5))) +
#     theme_bw() +
#     scale_colour_manual(values = Specimencolors) +
#     ggtitle("Sr/Ca") +
#     theme(legend.position = "none")

# MgCa_overview_plot <- ggplot(data = combined_raw) +
# #    geom_point(aes(Day, MgCa, col = Specimen),
# #        size = 0.1,
# #        alpha = 0.01,
# #        ) +
#     geom_line(data = combined_filtered_offset,
#         aes(Day, MgCa, col = Specimen),
#         size = 0.1,
#         alpha = 0.1) +
#     geom_line(data = combined_loess02_offset,
#         aes(Day, MgCa, col = Specimen),
#         size = 0.1,
#         alpha = 1) +
#     scale_y_continuous("Mg/Ca [mmol/mol]",
#         limits = c(0, 36),
#         breaks = c(seq(0, 36, 1)),
#         labels = c(seq(0, 5, 1),
#             rep("", 2),
#             seq(2, 5, 1),
#             rep("", 2),
#             seq(2, 5, 1),
#             rep("", 3),
#             seq(0, 3, 1),
#             rep("", 2),
#             seq(0, 3, 1),
#             rep("", 2),
#             seq(0, 3, 1)),
#         sec.axis = sec_axis(~.,
#             name = "Mg/Ca [mmol/mol]",
#             breaks = c(seq(5, 8, 1),
#                 seq(11, 14, 1),
#                 seq(18, 21, 1),
#                 seq(24, 27, 1),
#                 seq(30, 33, 1)),
#             labels = c(seq(2, 5, 1),
#                 seq(2, 5, 1),
#                 seq(0, 3, 1),
#                 seq(0, 3, 1),
#                 seq(0, 3, 1)))) +
#     scale_x_continuous("Age (days)",
#         breaks = seq(0, 1100, 100),
#         sec.axis = sec_axis(~.,
#             name = "Age (years)",
#             breaks = seq(0, 3 * 365, 365 / 2),
#             labels = seq(0, 3, 1 / 2))) +
#     geom_hline(yintercept = c(seq(5, 17, 3), seq(18, 33, 3))) +
#     theme_bw() +
#     scale_colour_manual(values = Specimencolors) +
#     ggtitle("Mg/Ca") +
#     theme(legend.position = "none")

# MnCa_overview_plot <- ggplot(data = combined_raw) +
# #    geom_point(aes(Day, MnCa, col = Specimen),
# #        size = 0.1,
# #        alpha = 0.01,
# #        ) +
#     geom_line(data = combined_filtered_offset,
#         aes(Day, MnCa, col = Specimen),
#         size = 0.1,
#         alpha = 0.1) +
#     geom_line(data = combined_loess02_offset,
#         aes(Day, MnCa, col = Specimen),
#         size = 0.1,
#         alpha = 1) +
#     scale_y_continuous(expression(paste("Mn/Ca [", mu, "mol/mol]")),
#         limits = c(0, 0.22),
#         breaks = c(seq(0, 0.22, .005)),
#         labels = c(seq(0, 20, 5),
#             rep("", 3),
#             seq(0, 20, 5),
#             rep("", 3),
#             seq(0, 20, 5),
#             rep("", 3),
#             seq(0, 20, 5),
#             rep("", 3),
#             seq(0, 20, 5),
#             rep("", 3),
#             seq(0, 20, 5)),
#         sec.axis = sec_axis(~.,
#             name = expression(paste("Mn/Ca [", mu, "mol/mol]")),
#             breaks = c(seq(.02, .04, .005),
#                 seq(.06, .08, .005),
#                 seq(.1, .12, .005),
#                 seq(.14, .16, .005),
#                 seq(.18, .2, .005)),
#             labels = c(seq(0, 20, 5),
#                 seq(0, 20, 5),
#                 seq(0, 20, 5),
#                 seq(0, 20, 5),
#                 seq(0, 20, 5)))) +
#     scale_x_continuous("Age (days)",
#         breaks = seq(0, 1100, 100),
#         sec.axis = sec_axis(~.,
#             name = "Age (years)",
#             breaks = seq(0, 3 * 365, 365 / 2),
#             labels = seq(0, 3, 1 / 2))) +
#     geom_hline(yintercept = seq(.02, .2, .02)) +
#     theme_bw() +            
#     scale_colour_manual(values = Specimencolors) +
#     ggtitle("Mn/Ca") +
#     theme(legend.position = "none")

# BaCa_overview_plot <- ggplot(data = combined_raw) +
# #    geom_point(aes(Day, BaCa, col = Specimen),
# #        size = 0.1,
# #        alpha = 0.01,
# #        ) +
#     geom_line(data = combined_filtered_offset,
#         aes(Day, BaCa, col = Specimen),
#         size = 0.1,
#         alpha = 0.1) +
#     geom_line(data = combined_loess02_offset,
#         aes(Day, BaCa, col = Specimen),
#         size = 0.1,
#         alpha = 1) +
#     scale_y_continuous(expression(paste("Ba/Ca [", mu, "mmol/mol]")),
#         limits = c(0, 0.11),
#         breaks = c(seq(0, 0.11, .0025)),
#         labels = c(seq(0, 10, 2.5),
#             rep("", 3),
#             seq(0, 10, 2.5),
#             rep("", 3),
#             seq(0, 10, 2.5),
#             rep("", 3),
#             seq(0, 10, 2.5),
#             rep("", 3),
#             seq(0, 10, 2.5),
#             rep("", 3),
#             seq(0, 10, 2.5)),
#         sec.axis = sec_axis(~.,
#             name = expression(paste("Ba/Ca [", mu, "mmol/mol]")),
#             breaks = c(seq(.01, .02, .0025),
#                 seq(.03, .04, .0025),
#                 seq(.05, .06, .0025),
#                 seq(.07, .08, .0025),
#                 seq(.09, .1, .0025)),
#             labels = c(seq(0, 10, 2.5),
#                 seq(0, 10, 2.5),
#                 seq(0, 10, 2.5),
#                 seq(0, 10, 2.5),
#                 seq(0, 10, 2.5)))) +
#     scale_x_continuous("Age (days)",
#         breaks = seq(0, 1100, 100),
#         sec.axis = sec_axis(~.,
#             name = "Age (years)",
#             breaks = seq(0, 3 * 365, 365 / 2),
#             labels = seq(0, 3, 1 / 2))) +
#     geom_hline(yintercept = seq(.01, .1, .01)) +
#     theme_bw() +            
#     scale_colour_manual(values = Specimencolors) +
#     ggtitle("Ba/Ca") +
#     theme(legend.position = "none")

# combined_plots <- grid.arrange(SrCa_overview_plot,
#     MgCa_overview_plot,
#     MnCa_overview_plot,
#     BaCa_overview_plot,
#     ncol = 4)

# Move to long format to enable facet grids

combined_filtered_long <- combined_filtered %>%
    gather(key = "parameter", value = "value", MgCa:BaCa)

combined_loess02_long <- combined_loess02 %>%
    gather(key = "parameter", value = "value", MgCa:BaCa)

# Plots per specimen
elementcolors <- c("darkgreen", "blue", "purple", "orange")
names(elementcolors) <- c("MgCa", "SrCa", "MnCa", "BaCa")

# All pectinid plot

# Define scales of y-axes individually for pectinids
scales_y_PM <- list(
  `BaCa` = scale_y_continuous("", limits = c(0, 0.01), breaks = seq(0, 0.01, 0.002), labels = seq(0, 10, 2)),
  `MgCa` = scale_y_continuous("", limits = c(0, 6), breaks = seq(0, 6, 1)),
  `MnCa` = scale_y_continuous("", limits = c(0, 0.02), breaks = seq(0, 0.02, 0.005), labels = seq(0, 20, 5)),
  `SrCa` = scale_y_continuous("", limits = c(0, 2), breaks = seq(0, 2, 0.5))
)

# Define scales of x-axes individually for pectinids
scales_x_PM <- list(
  `PM2_1` = scale_x_continuous("Age (days)",
        limits = c(0, 250),
        breaks = seq(0, 300, 50),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 365, 365 / 10),
            labels = seq(0, 1, 1 / 10))),
  `PM2_2` = scale_x_continuous("Age (days)",
        limits = c(0, 250),
        breaks = seq(0, 300, 50),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 365, 365 / 10),
            labels = seq(0, 1, 1 / 10))),
  `PM3_1` = scale_x_continuous("Age (days)",
        limits = c(0, 250),
        breaks = seq(0, 300, 50),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 365, 365 / 10),
            labels = seq(0, 1, 1 / 10))),
  `PM3_2` = scale_x_continuous("Age (days)",
        limits = c(0, 250),
        breaks = seq(0, 300, 50),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 365, 365 / 10),
            labels = seq(0, 1, 1 / 10))),
  `PM4` = scale_x_continuous("Age (days)",
        limits = c(0, 300),
        breaks = seq(0, 300, 50),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 365, 365 / 10),
            labels = seq(0, 1, 1 / 10)))
)

# Plot pectinid records
PM_facet_plot <- ggplot(data = combined_filtered_long[which(combined_filtered_long$Specimen %in% c("PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")),]) +
    geom_line(data = combined_filtered_long[which(combined_filtered_long$Specimen %in% c("PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")),], 
        aes(Day, value, col = parameter),
        size = 0.1,
        alpha = 0.1) +
    geom_line(data = combined_loess02_long[which(combined_filtered_long$Specimen %in% c("PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")),],
        aes(Day, value, col = parameter),
        size = 1,
        alpha = 1) +
    facet_grid_sc(parameter ~ Specimen,
        scales = list(x = scales_x_PM, y = scales_y_PM),
        switch = "y",
        labeller = as_labeller(c(BaCa = "Ba/Ca [umol/mol]",
            MgCa = "Mg/Ca [mmol/mol]",
            MnCa = "Mn/Ca [umol/mol]",
            SrCa = "Sr/Ca [mmol/mol]",
            PM2_1 = "P. maximus 2 (valley)",
            PM2_2 = "P. maximus 2 (rib)",
            PM3_1 = "P. maximus 3 (valley)",
            PM3_2 = "P. maximus 3 (rib)",
            PM4 = "P. maximus 4 (valley)"))) +
    scale_colour_manual(values = elementcolors) +
    theme_bw() +
    theme(strip.background = element_blank(), # remove the background
        strip.placement = "outside", # put labels to the left of the axis text
        legend.position = "none")

# ------------------------------------------------------------------------------
# Define scales of y-axes individually for tridacnids
scales_y_T <- list(
  `BaCa` = scale_y_continuous("", limits = c(0, 0.01), breaks = seq(0, 0.01, 0.005), labels = seq(0, 10, 5)),
  `MgCa` = scale_y_continuous("", limits = c(0, 4), breaks = seq(0, 4, 1)),
  `MnCa` = scale_y_continuous("", limits = c(0, 0.02), breaks = seq(0, 0.02, 0.005), labels = seq(0, 20, 5)),
  `SrCa` = scale_y_continuous("", limits = c(0, 3), breaks = seq(0, 3, 0.5))
)

# Define scales of x-axes individually for pectinids
scales_x_T <- list(
  `TM29` = scale_x_continuous("Age (days)",
        limits = c(0, 600),
        breaks = seq(0, 1200, 100),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 3 * 365, 365 / 2),
            labels = seq(0, 3, 1 / 2))),
  `TM84` = scale_x_continuous("Age (days)",
        limits = c(0, 500),
        breaks = seq(0, 1200, 100),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 3 * 365, 365 / 2),
            labels = seq(0, 3, 1 / 2))),
  `TS85` = scale_x_continuous("Age (days)",
        limits = c(0, 600),
        breaks = seq(0, 1200, 100),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 3 * 365, 365 / 2),
            labels = seq(0, 3, 1 / 2))),
  `TSFRS1` = scale_x_continuous("Age (days)",
        limits = c(0, 550),
        breaks = seq(0, 1200, 100),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 3 * 365, 365 / 2),
            labels = seq(0, 3, 1 / 2))),
  `TSM1` = scale_x_continuous("Age (days)",
        limits = c(0, 800),
        breaks = seq(0, 1200, 200),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 3 * 365, 365 / 2),
            labels = seq(0, 3, 1 / 2))),
  `SQSA1` = scale_x_continuous("Age (days)",
        limits = c(0, 1200),
        breaks = seq(0, 1200, 200),
        sec.axis = sec_axis(~.,
            name = "Age (years)",
            breaks = seq(0, 3 * 365, 365 / 2),
            labels = seq(0, 3, 1 / 2)))
)

# Plot tridacnid records
Tridacna_facet_plot <- ggplot(data = combined_filtered_long[which(combined_filtered_long$Specimen %in% c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1")),]) +
    geom_line(data = combined_filtered_long[which(combined_filtered_long$Specimen %in% c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1")),], 
        aes(Day, value, col = parameter),
        size = 0.1,
        alpha = 0.1) +
    geom_line(data = combined_loess02_long[which(combined_filtered_long$Specimen %in% c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1")),],
        aes(Day, value, col = parameter),
        size = 1,
        alpha = 1) +
    facet_grid_sc(parameter ~ Specimen,
        scales = list(x = scales_x_T, y = scales_y_T),
        switch = "y",
        labeller = as_labeller(c(BaCa = "Ba/Ca [umol/mol]",
            MgCa = "Mg/Ca [mmol/mol]",
            MnCa = "Mn/Ca [umol/mol]",
            SrCa = "Sr/Ca [mmol/mol]",
            TM29 = "T. maxima 29",
            TM84 = "T. maxima 84",
            TS85 = "T. squamosa 85",
            TSFRS1 = "T. squamosa FRS1",
            TSM1 = "T. squamosa M1",
            SQSA1 = "T. squamosina"))) +
    scale_colour_manual(values = elementcolors) +
    theme_bw() +
    theme(strip.background = element_blank(), # remove the background
        strip.placement = "outside", # put labels to the left of the axis text
        legend.position = "none")

# Combine plots of pectinids and tridacnids
Combined_facet_plot <- ggarrange(
    PM_facet_plot,
    Tridacna_facet_plot,
    ncol = 1
)
