# Create raw data SI

setwd("C:/Users/niels/Dropbox/Research/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Datasets_dated/Increment_based")

# Load and trim data (following previously established cutoffs)

# Pectinids
load("Shell2_1_dated_aligned_cross.rda") # Load Shell2_1 data
PM2_1 <- dat_trim # Rename to avoid conflict
PM2_1_trim <- PM2_1[which(PM2_1$Depth > 7000), ]

load("Shell2_2_dated_cross.rda") # Load Shell2_2 data
PM2_2 <- dat_trim # Rename to avoid conflict
PM2_2_trim <- PM2_2[which(PM2_2$Depth > 3500), ]

load("Shell3_1_dated_aligned_cross.rda") # Load Shell3_1 data
PM3_1 <- dat_trim # Rename to avoid conflict
PM3_1_trim <- PM3_1[which(PM3_1$Depth > 1000), ]

load("Shell3_2_dated_cross.rda") # Load Shell3_2 data
PM3_2 <- dat_trim # Rename to avoid conflict
PM3_2_trim <- PM3_2[which(PM3_2$Depth > 4500), ]

load("Shell4_dated_cross.rda") # Load Shell4 data
PM4 <- dat_trim # Rename to avoid conflict
PM4_trim <- PM4[which(PM4$Depth > 2000), ]

setwd("C:/Users/niels/Dropbox/Research/Side projects/Bivalve photosymbiosis and gigantism/Tridacnids_Dan/LAICPMS data")

# Tridacnids
TM29 <- read.csv("Line_Tridacnid_NdW_29_recalc.csv")
TM29_trim <- TM29[which((TM29$depth < 52) & (TM29$depth > 0.04)), ]
TM29_trim <- TM29_trim[-which((TM29_trim$depth < 33) & (TM29_trim$depth > 31.5)), ]

TM84 <- read.csv("Line_Tridacnid_NdW_84_recalc.csv")
TM84_trim <- TM84[which((TM84$depth < 37) & (TM84$depth > 0.01)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 4.05) & (TM84_trim$depth > 3.8)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 4.7) & (TM84_trim$depth > 4.5)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 6.3) & (TM84_trim$depth > 6.05)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 17.3) & (TM84_trim$depth > 17)), ]
TM84_trim <- TM84_trim[-which((TM84_trim$depth < 29.5) & (TM84_trim$depth > 29)), ]

TS85 <- read.csv("Line_Tridacnid_NdW_85_recalc.csv")
TS85_trim <- TS85[which((TS85$depth < 39.7) & (TS85$depth > 0.6)), ]

TSFRS1 <- read.csv("Line_Tridacnid_NdW_FRS1_recalc.csv")
TSFRS1_trim <- TSFRS1[which((TSFRS1$depth < 32.8) & (TSFRS1$depth > 0.04)), ]

TSM1 <- read.csv("Line_Tridacnid_NdW_M1_recalc.csv")
TSM1_trim <- TSM1[which((TSM1$depth < 42.35) & (TSM1$depth > 0.015) & (TSM1$X25Mg.43Ca < 25) & (TSM1$X137Ba.43Ca < 0.01)), ]
TSM1_trim <- TSM1_trim[-which((TSM1_trim$depth < 3.15) & (TSM1_trim$depth > 2.8)), ]
TSM1_trim <- TSM1_trim[-which((TSM1_trim$depth < 4) & (TSM1_trim$depth > 3.65)), ]

SQSA1 <- read.csv("Line_Tridacnid_NdW_SQSA1_recalc.csv")
SQSA1_trim <- SQSA1[which((SQSA1$depth < 38.75) & (SQSA1$depth > 0.015) & (SQSA1$X25Mg.43Ca < 10) & (SQSA1$X137Ba.43Ca < 0.01)), ]
SQSA1_trim <- SQSA1_trim[-which((SQSA1_trim$depth < 4.05) & (SQSA1_trim$depth > 3.8)), ]
SQSA1_trim <- SQSA1_trim[-which((SQSA1_trim$depth < 26) & (SQSA1_trim$depth > 25.7)), ]
SQSA1_trim <- SQSA1_trim[-which((SQSA1_trim$depth < 25.2) & (SQSA1_trim$depth > 24.5)), ]

# Rename columns for easier reference
colnames(TM29_trim)[c(3:8, 10)] <- colnames(TM84_trim)[c(3:8, 10)] <- colnames(TS85_trim)[c(3:8, 10)] <- colnames(TSFRS1_trim)[c(3:8, 10)] <- colnames(TSM1_trim)[c(3:8, 10)] <- colnames(SQSA1_trim)[c(3:8, 10)] <- c("MgCa", "CaCa", "MnCa", "Sr87Ca", "SrCa", "BaCa", "Day")
colnames(PM2_1_trim)[2:6] <- colnames(PM2_2_trim)[2:6] <- colnames(PM3_1_trim)[2:6] <- colnames(PM3_2_trim)[2:6] <- colnames(PM4_trim)[2:6] <- c("MgCa", "MnCa", "Sr87Ca", "SrCa", "BaCa")
colnames(TM29)[c(3:8, 10)] <- colnames(TM84)[c(3:8, 10)] <- colnames(TS85)[c(3:8, 10)] <- colnames(TSFRS1)[c(3:8, 10)] <- colnames(TSM1)[c(3:8, 10)] <- colnames(SQSA1)[c(3:8, 10)] <- c("MgCa", "CaCa", "MnCa", "Sr87Ca", "SrCa", "BaCa", "Day")
colnames(PM2_1)[2:6] <- colnames(PM2_2)[2:6] <- colnames(PM3_1)[2:6] <- colnames(PM3_2)[2:6] <- colnames(PM4)[2:6] <- c("MgCa", "MnCa", "Sr87Ca", "SrCa", "BaCa")

# Export in one place
write.csv(PM2_1_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/PM2_1_raw.csv", row.names = FALSE)
write.csv(PM2_2_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/PM2_2_raw.csv", row.names = FALSE)
write.csv(PM3_1_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/PM3_1_raw.csv", row.names = FALSE)
write.csv(PM3_2_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/PM3_2_raw.csv", row.names = FALSE)
write.csv(PM4_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/PM4_raw.csv", row.names = FALSE)
write.csv(TM29_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/TM29_raw.csv", row.names = FALSE)
write.csv(TM84_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/TM84_raw.csv", row.names = FALSE)
write.csv(TS85_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/TS85_raw.csv", row.names = FALSE)
write.csv(TSFRS1_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/TSFRS1_raw.csv", row.names = FALSE)
write.csv(TSM1_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/TSM1_raw.csv", row.names = FALSE)
write.csv(SQSA1_trim, "C:/Users/niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/raw data/SQSA1_raw.csv", row.names = FALSE)