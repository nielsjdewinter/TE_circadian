# Script to zoom-in plots of (semi-)diurnal variability

require(tidyverse)
require(signal)
require(ggpubr)

root<- "C:/Users/Niels/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/S0_raw data" #set your root directory
setwd(root)

# Load pectinid data
PM2_2 <- read.csv("PM2_2_raw.csv")
# PM2_2$BaCa[PM2_2$BaCa == 0] <- mean(PM2_2$BaCa[PM2_2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing
PM2_2[is.na(PM2_2)] <- 0

PM2_2_filtered <- as.data.frame(cbind(PM2_2$Depth,
    PM2_2$Day,
    apply(PM2_2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3, n = 51) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(PM2_2_filtered)[c(1, 2)] <- c("Depth", "Day")
PM2_2_filtered$datetime <- PM2_2$datetime

# Plot zoom-in of variability in record

dup_labels  <- function(x) round(PM2_2$Depth[match(x, round(PM2_2$Day, 1))], 0) # Prepare secondary (Depth) axis

# Set plotting window
startday <- 150
endday <- 170

PM2_2_Srplot <- ggplot(PM2_2_filtered) +
geom_line(aes(Day, SrCa),
    col = "blue") +
scale_x_continuous("",
    breaks = seq(startday, endday, 2),
    labels = NULL,
    limits = c(startday, endday),
    sec.axis = dup_axis(name = "Depth (micrometers)",
        labels = dup_labels)
    ) +
scale_y_continuous("Sr/Ca (mmol/mol)",
    limits = c(1.2, 1.6)) +
theme_bw()

PM2_2_Mgplot <- ggplot(PM2_2_filtered) +
geom_line(aes(Day, MgCa),
    col = "darkgreen") +
scale_x_continuous("",
    breaks = seq(startday, endday, 2),
    labels = NULL,
    limits = c(startday, endday)) +
scale_y_continuous("Mg/Ca (mmol/mol)",
    limits = c(1.8, 3.2)) +
theme_bw()

PM2_2_Mnplot <- ggplot(PM2_2_filtered) +
geom_line(aes(Day, MnCa),
    col = "purple") +
scale_x_continuous("",
    breaks = seq(startday, endday, 2),
    labels = NULL,
    limits = c(startday, endday)) +
scale_y_continuous("Mn/Ca (umol/mol)",
    breaks = seq(.006, .012, .002),
    labels = seq(6, 12, 2),
    limits = c(.006, .012)) +
theme_bw()

PM2_2_Baplot <- ggplot(PM2_2_filtered) +
geom_line(aes(Day, BaCa),
    col = "orange") +
scale_x_continuous("Age (days)",
    breaks = seq(startday, endday, 2),
    limits = c(startday, endday)) +
scale_y_continuous("Ba/Ca (umol/mol)",
    breaks = seq(.001, .004, .001),
    labels = seq(1, 4, 1),
    limits = c(.001, .004)) +
theme_bw()

# Combined plots
PM2_2_combined <- ggarrange(
    PM2_2_Srplot,
    PM2_2_Mgplot,
    PM2_2_Mnplot,
    PM2_2_Baplot,
    ncol = 1
)

# Load tridacnid data
TM29 <- read.csv("TM29_raw.csv")
TM29[is.na(TM29)] <- 0

TM29_filtered <- as.data.frame(cbind(TM29$depth,
    TM29$Day,
    apply(TM29[, c(3, 5, 7, 8)], 2, sgolayfilt, p = 3, n = 51) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(TM29_filtered)[c(1, 2)] <- c("depth", "Day")

# Plot zoom-in of variability in record

dup_labels  <- function(x) round(TM29$depth[match(x, round(TM29$Day, 1))] * 1000, 0) # Prepare secondary (Depth) axis

# Set plotting window
startday <- 32
endday <- 40

TM29_Srplot <- ggplot(TM29_filtered) +
geom_line(aes(Day, SrCa),
    col = "blue") +
scale_x_continuous("",
    breaks = seq(startday, endday, 1),
    labels = NULL,
    limits = c(startday, endday),
    sec.axis = dup_axis(name = "Depth (micrometers)",
        labels = dup_labels)
    ) +
scale_y_continuous("Sr/Ca (mmol/mol)",
    limits = c(1, 2)) +
theme_bw()

TM29_Mgplot <- ggplot(TM29_filtered) +
geom_line(aes(Day, MgCa),
    col = "darkgreen") +
scale_x_continuous("",
    breaks = seq(startday, endday, 1),
    labels = NULL,
    limits = c(startday, endday)) +
scale_y_continuous("Mg/Ca (mmol/mol)",
    limits = c(.1, .5)) +
theme_bw()

TM29_Mnplot <- ggplot(TM29_filtered) +
geom_line(aes(Day, MnCa),
    col = "purple") +
scale_x_continuous("",
    breaks = seq(startday, endday, 1),
    labels = NULL,
    limits = c(startday, endday)) +
scale_y_continuous("Mn/Ca (umol/mol)",
    breaks = seq(.00, .0025, .001),
    labels = seq(0, 2.5, 1),
    limits = c(.00, .0025)) +
theme_bw()

TM29_Baplot <- ggplot(TM29_filtered) +
geom_line(aes(Day, BaCa),
    col = "orange") +
scale_x_continuous("Age (days)",
    breaks = seq(startday, endday, 1),
    limits = c(startday, endday)) +
scale_y_continuous("Ba/Ca (umol/mol)",
    breaks = seq(0, .003, .001),
    labels = seq(0, 3, 1),
    limits = c(0, .003)) +
theme_bw()

# Combined plots
TM29_combined <- ggarrange(
    TM29_Srplot,
    TM29_Mgplot,
    TM29_Mnplot,
    TM29_Baplot,
    ncol = 1
)

# Combine both in one figure
zoomin_combined <- ggarrange(
    PM2_2_combined,
    TM29_combined,
    ncol = 2
)