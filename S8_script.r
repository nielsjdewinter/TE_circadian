# Script to plot overview of trace element results (S8)

require(tidyverse)
require(RColorBrewer)
require(gridExtra)
require(moments)


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

PM2_1_trim$Specimen <- "PM2_1"
PM2_2_trim$Specimen <- "PM2_2"
PM3_1_trim$Specimen <- "PM3_1"
PM3_2_trim$Specimen <- "PM3_2"
PM4_trim$Specimen <- "PM4"
TM29_trim$Specimen <- "TM29"
TM84_trim$Specimen <- "TM84"
TS85_trim$Specimen <- "TS85"
TSFRS1_trim$Specimen <- "TSFRS1"
TSM1_trim$Specimen <- "TSM1"
SQSA1_trim$Specimen <- "SQSA1"

# Merge data into long format
dat_all <- rbind(
    TM29_trim[c(3, 5, 7, 8, 10, 13)],
    TM84_trim[c(3, 5, 7, 8, 10, 13)],
    TS85_trim[c(3, 5, 7, 8, 10, 13)],
    TSFRS1_trim[c(3, 5, 7, 8, 10, 13)],
    TSM1_trim[c(3, 5, 7, 8, 10, 13)],
    SQSA1_trim[c(3, 5, 7, 8, 10, 13)],
    PM2_1_trim[c(2, 3, 5, 6, 10, 12)],
    PM2_2_trim[c(2, 3, 5, 6, 7, 9)],
    PM3_1_trim[c(2, 3, 5, 6, 10, 12)],
    PM3_2_trim[c(2, 3, 5, 6, 7, 9)],
    PM4_trim[c(2, 3, 5, 6, 7, 9)]
)

# Remove zeroes from dat_all
dat_all[dat_all == 0] <- NA

# Add species names
dat_all$Species <- NA
dat_all$Species[which(dat_all$Specimen %in% c("TM29 trim", "TM84 trim"))] <- "Tridacna_maxima"
dat_all$Species[which(dat_all$Specimen %in% c("TS85 trim", "TSFRS1 trim", "TSM1 trim"))] <- "Tridacna_squamosa"
dat_all$Species[which(dat_all$Specimen == "SQSA1 trim")] <- "Tridacna_squamosina"
dat_all$Species[which(dat_all$Specimen %in% c("PM2_1 trim", "PM2_2 trim", "PM3_1 trim", "PM3_2 trim", "PM4 trim"))] <- "Pecten_maximus"

# Add Genus names
dat_all$Genus <- NA
dat_all$Genus[which(dat_all$Species == "Pecten_maximus")] <- "Pecten"
dat_all$Genus[which(dat_all$Species %in% c("Tridacna_maxima", "Tridacna_squamosa", "Tridacna_squamosina"))] <- "Tridacna"

# Create groups for statistics (section 3.1)
# Group by genus
Genus_groups <- dat_all %>%
group_by(Genus) %>%
summarize(
    N = n(),
    MgCa_mean = mean(MgCa, na.rm = TRUE),
    MgCa_SD = sd(MgCa, na.rm = TRUE),
    MgCa_95CL = qt(0.95, N) * MgCa_SD / sqrt(N),
    MnCa_mean = mean(MnCa, na.rm = TRUE),
    MnCa_SD = sd(MnCa, na.rm = TRUE),
    MnCa_95CL = qt(0.95, N) * MnCa_SD / sqrt(N),
    SrCa_mean = mean(SrCa, na.rm = TRUE),
    SrCa_SD = sd(SrCa, na.rm = TRUE),
    SrCa_95CL = qt(0.95, N) * SrCa_SD / sqrt(N),
    BaCa_mean = mean(BaCa, na.rm = TRUE),
    BaCa_SD = sd(BaCa, na.rm = TRUE),
    BaCa_95CL = qt(0.95, N) * BaCa_SD / sqrt(N)
)

# Group by species
Species_groups <- dat_all %>%
group_by(Species) %>%
summarize(
    Genus = first(Genus),
    N = n(),
    MgCa_mean = mean(MgCa, na.rm = TRUE),
    MgCa_SD = sd(MgCa, na.rm = TRUE),
    MgCa_95CL = qt(0.95, N) * MgCa_SD / sqrt(N),
    MnCa_mean = mean(MnCa, na.rm = TRUE),
    MnCa_SD = sd(MnCa, na.rm = TRUE),
    MnCa_95CL = qt(0.95, N) * MnCa_SD / sqrt(N),
    SrCa_mean = mean(SrCa, na.rm = TRUE),
    SrCa_SD = sd(SrCa, na.rm = TRUE),
    SrCa_95CL = qt(0.95, N) * SrCa_SD / sqrt(N),
    BaCa_mean = mean(BaCa, na.rm = TRUE),
    BaCa_SD = sd(BaCa, na.rm = TRUE),
    BaCa_95CL = qt(0.95, N) * BaCa_SD / sqrt(N)
)

# Group by specimen
Specimen_groups <- dat_all %>%
group_by(Specimen) %>%
summarize(
    Species = first(Species),
    Genus = first(Genus),
    N = n(),
    MgCa_mean = mean(MgCa, na.rm = TRUE),
    MgCa_SD = sd(MgCa, na.rm = TRUE),
    MgCa_95CL = qt(0.95, N) * MgCa_SD / sqrt(N),
    MgCa_skewness = skewness(MgCa, na.rm = TRUE),
    MnCa_mean = mean(MnCa, na.rm = TRUE),
    MnCa_SD = sd(MnCa, na.rm = TRUE),
    MnCa_95CL = qt(0.95, N) * MnCa_SD / sqrt(N),
    MnCa_skewness = skewness(MnCa, na.rm = TRUE),
    SrCa_mean = mean(SrCa, na.rm = TRUE),
    SrCa_SD = sd(SrCa, na.rm = TRUE),
    SrCa_95CL = qt(0.95, N) * SrCa_SD / sqrt(N),
    SrCa_skewness = skewness(SrCa, na.rm = TRUE),
    BaCa_mean = mean(BaCa, na.rm = TRUE),
    BaCa_SD = sd(BaCa, na.rm = TRUE),
    BaCa_95CL = qt(0.95, N) * BaCa_SD / sqrt(N),
    BaCa_skewness = skewness(BaCa, na.rm = TRUE)
)

Specimencolors <- c(brewer.pal(11, "RdBu")[11:7], "#000000", brewer.pal(11, "RdBu")[5:1])
names(Specimencolors) <- c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1", "PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")

MgCa_TEplot <- ggplot(data = dat_all) +
    geom_violin(aes(x = Specimen,
            y = MgCa,
            fill = Specimen),
        kernel = "rectangular",
        scale = "width",
        position = "identity",
        width = .5,
        cex = 0,
        alpha = .3,
        color = NA,
        trim = TRUE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Specimen,
            y = MgCa,
            fill = Specimen),
        width = .2,
        outlier.shape = NA,
        alpha = .5,
        na.rm = TRUE) +
    scale_y_continuous("Mg/Ca [mmol/mol]",
        limits = c(0, 7)) +
    scale_fill_manual(values = Specimencolors) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

SrCa_TEplot <- ggplot(data = dat_all) +
    geom_violin(aes(x = Specimen,
            y = SrCa,
            fill = Specimen),
        kernel = "rectangular",
        scale = "width",
        position = "identity",
        width = .5,
        cex = 0,
        alpha = .3,
        color = NA,
        trim = TRUE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Specimen,
            y = SrCa,
            fill = Specimen),
        width = .2,
        outlier.shape = NA,
        alpha = .5,
        na.rm = TRUE) +
    scale_y_continuous("Sr/Ca [mmol/mol]",
        limits = c(0, 4)) +
    scale_fill_manual(values = Specimencolors) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

MnCa_TEplot <- ggplot(data = dat_all) +
    geom_violin(aes(x = Specimen,
            y = MnCa,
            fill = Specimen),
        kernel = "rectangular",
        scale = "width",
        position = "identity",
        width = .5,
        cex = 0,
        alpha = .3,
        color = NA,
        trim = TRUE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Specimen,
            y = MnCa,
            fill = Specimen),
        width = .2,
        outlier.shape = NA,
        alpha = .5,
        na.rm = TRUE) +
    scale_y_continuous(expression(paste("Mn/Ca [", mu, "mmol/mol]")),
        breaks = seq(0, 0.03, 0.01),
        labels = seq(0, 30, 10),
        limits = c(0, .03)) +
    scale_fill_manual(values = Specimencolors) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

BaCa_TEplot <- ggplot(data = dat_all) +
    geom_violin(aes(x = Specimen,
            y = BaCa,
            fill = Specimen),
        kernel = "rectangular",
        scale = "width",
        position = "identity",
        width = .5,
        cex = 0,
        alpha = .3,
        color = NA,
        trim = TRUE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Specimen,
            y = BaCa,
            fill = Specimen),
        width = .2,
        outlier.shape = NA,
        alpha = .5,
        na.rm = TRUE) +
    scale_y_continuous(expression(paste("Ba/Ca [", mu, "mmol/mol]")),
        breaks = seq(0, .015, .005),
        labels = seq(0, 15, 5),
        limits = c(0, .015)) +
    scale_fill_manual(values = Specimencolors) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

# Combine TE plots
Combined_TE <- grid.arrange(SrCa_TEplot + theme(legend.position = "none") + scale_x_discrete(labels = NULL),
    MgCa_TEplot + theme(legend.position = "none") + scale_x_discrete(labels = NULL),
    MnCa_TEplot + theme(legend.position = "none") + scale_x_discrete(labels = NULL),
    BaCa_TEplot + theme(legend.position = "none"),
    heights = c(10, 10, 10, 15),
    ncol = 1)