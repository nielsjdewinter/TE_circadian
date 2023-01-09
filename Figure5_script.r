# Script to generate combined MTM plots of tridacnid records (Figure 4)

require(tidyverse)
require(zoo)
require(RColorBrewer)
require(gridExtra)

# Load data
MTM29 <- read.csv("<path>/maxima29_MTM.csv", header = TRUE)
MTM84 <- read.csv("<path>/maxima84_MTM.csv", header = TRUE)
MTM85 <- read.csv("<path>/squamosa85_MTM.csv", header = TRUE)
MTMFRS1 <- read.csv("<path>/squamosaFRS1_MTM.csv", header = TRUE)
MTMM1 <- read.csv("<path>/squamosaM1_MTM.csv", header = TRUE)
MTMSQSA1 <- read.csv("<path>/squamosinaSQSA1_MTM.csv", header = TRUE)

# Normalize powerspectra
MTM29[, -c(1, 2)] <- t(t(MTM29[, -c(1, 2)]) / apply(MTM29[, -c(1, 2)], 2, max))
MTM84[, -c(1, 2)] <- t(t(MTM84[, -c(1, 2)]) / apply(MTM84[, -c(1, 2)], 2, max))
MTM85[, -c(1, 2)] <- t(t(MTM85[, -c(1, 2)]) / apply(MTM85[, -c(1, 2)], 2, max))
MTMFRS1[, -c(1, 2)] <- t(t(MTMFRS1[, -c(1, 2)]) / apply(MTMFRS1[, -c(1, 2)], 2, max))
MTMM1[, -c(1, 2)] <- t(t(MTMM1[, -c(1, 2)]) / apply(MTMM1[, -c(1, 2)], 2, max))
MTMSQSA1[, -c(1, 2)] <- t(t(MTMSQSA1[, -c(1, 2)]) / apply(MTMSQSA1[, -c(1, 2)], 2, max))

# Create colorscale
Specimencolors <- brewer.pal(11, "RdBu")[1:11]
names(Specimencolors) <- c("maxima29", "maxima84", "squamosa85", "squamosaFRS1", "squamosaM1", "squamosinaSQSA1", "maxima29", "maxima84", "squamosa85", "squamosaFRS1", "squamosaM1")

# Plot powerspectra
SrCa_MTM_norm_plot <- ggplot() +
    geom_line(data = MTM29, aes(Period, SrCa, col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, SrCa, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, SrCa, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, SrCa, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, SrCa, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, SrCa, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 1)) +
    ggtitle("MTM of Sr/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme(legend.position = "none")

MgCa_MTM_norm_plot <- ggplot() +
    geom_line(data = MTM29, aes(Period, MgCa, col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, MgCa, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, MgCa, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, MgCa, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, MgCa, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, MgCa, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 1)) +
    ggtitle("MTM of Mg/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme(legend.position = "none")

MnCa_MTM_norm_plot <- ggplot() +
    geom_line(data = MTM29, aes(Period, MnCa, col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, MnCa, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, MnCa, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, MnCa, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, MnCa, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, MnCa, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 1)) +
    ggtitle("MTM of Mn/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme(legend.position = "none")

BaCa_MTM_norm_plot <- ggplot() +
    geom_line(data = MTM29, aes(Period, BaCa, col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, BaCa, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, BaCa, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, BaCa, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, BaCa, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, BaCa, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 1)) +
    ggtitle("MTM of Ba/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme(legend.position = "none")

combined_MTM_plots <- grid.arrange(SrCa_MTM_norm_plot, MgCa_MTM_norm_plot, MnCa_MTM_norm_plot, BaCa_MTM_norm_plot, ncol = 2)

# Offset plots
SrCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, SrCa, col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, SrCa + 0.5, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(SrCa + 0.5, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, SrCa + 1, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(SrCa + 1, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, SrCa + 1.5, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(SrCa + 1.5, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, SrCa + 2, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(SrCa + 2, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, SrCa + 2.5, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(SrCa + 2.5, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 3.5), breaks = c(0.3, 0.8, 1.3, 1.8, 2.3, 2.8), labels = c("T. maxima 29", "T. maxima 84", "T. squamosa 85", "T. squamosa FRS1", "T. squamosa M1", "T. squamosina SQSA1")) +
    ggtitle("MTM of Sr/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MgCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, MgCa, col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, MgCa + 0.5, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(MgCa + 0.5, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, MgCa + 1, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(MgCa + 1, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, MgCa + 1.5, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(MgCa + 1.5, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, MgCa + 2, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(MgCa + 2, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, MgCa + 2.5, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(MgCa + 2.5, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 3.5), breaks = c(0.3, 0.8, 1.3, 1.8, 2.3, 2.8), labels = c("T. maxima 29", "T. maxima 84", "T. squamosa 85", "T. squamosa FRS1", "T. squamosa M1", "T. squamosina SQSA1")) +
    ggtitle("MTM of Mg/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MnCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, MnCa, col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, MnCa + 0.5, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(MnCa + 0.5, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, MnCa + 1, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(MnCa + 1, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, MnCa + 1.5, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(MnCa + 1.5, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, MnCa + 2, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(MnCa + 2, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, MnCa + 2.5, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(MnCa + 2.5, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 3.5), breaks = c(0.3, 0.8, 1.3, 1.8, 2.3, 2.8), labels = c("T. maxima 29", "T. maxima 84", "T. squamosa 85", "T. squamosa FRS1", "T. squamosa M1", "T. squamosina SQSA1")) +
    ggtitle("MTM of Mn/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

BaCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, BaCa, col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, BaCa + 0.5, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(BaCa + 0.5, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, BaCa + 1, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(BaCa + 1, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, BaCa + 1.5, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(BaCa + 1.5, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, BaCa + 2, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(BaCa + 2, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, BaCa + 2.5, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(BaCa + 2.5, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 3.5), breaks = c(0.3, 0.8, 1.3, 1.8, 2.3, 2.8), labels = c("T. maxima 29", "T. maxima 84", "T. squamosa 85", "T. squamosa FRS1", "T. squamosa M1", "T. squamosina SQSA1")) +
    ggtitle("MTM of Ba/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

combined_MTM_offset_plots <- grid.arrange(SrCa_MTM_offset_plot, MgCa_MTM_offset_plot, MnCa_MTM_offset_plot, BaCa_MTM_offset_plot, ncol = 2)

# Offset plots no color
SrCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, SrCa, ), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, SrCa + 0.5, ), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(SrCa + 0.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, SrCa + 1, ), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(SrCa + 1, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, SrCa + 1.5, ), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(SrCa + 1.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, SrCa + 2, ), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(SrCa + 2, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, SrCa + 2.5, ), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(SrCa + 2.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 3.5), breaks = c(0.3, 0.8, 1.3, 1.8, 2.3, 2.8), labels = c("T. maxima 29", "T. maxima 84", "T. squamosa 85", "T. squamosa FRS1", "T. squamosa M1", "T. squamosina SQSA1")) +
    ggtitle("MTM of Sr/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MgCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, MgCa, ), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, MgCa + 0.5, ), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(MgCa + 0.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, MgCa + 1, ), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(MgCa + 1, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, MgCa + 1.5, ), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(MgCa + 1.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, MgCa + 2, ), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(MgCa + 2, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, MgCa + 2.5, ), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(MgCa + 2.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 3.5), breaks = c(0.3, 0.8, 1.3, 1.8, 2.3, 2.8), labels = c("T. maxima 29", "T. maxima 84", "T. squamosa 85", "T. squamosa FRS1", "T. squamosa M1", "T. squamosina SQSA1")) +
    ggtitle("MTM of Mg/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MnCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, MnCa, ), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, MnCa + 0.5, ), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(MnCa + 0.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, MnCa + 1, ), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(MnCa + 1, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, MnCa + 1.5, ), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(MnCa + 1.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, MnCa + 2, ), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(MnCa + 2, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, MnCa + 2.5, ), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(MnCa + 2.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 3.5), breaks = c(0.3, 0.8, 1.3, 1.8, 2.3, 2.8), labels = c("T. maxima 29", "T. maxima 84", "T. squamosa 85", "T. squamosa FRS1", "T. squamosa M1", "T. squamosina SQSA1")) +
    ggtitle("MTM of Mn/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

BaCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, BaCa, ), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, BaCa + 0.5, ), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(BaCa + 0.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, BaCa + 1, ), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(BaCa + 1, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, BaCa + 1.5, ), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(BaCa + 1.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, BaCa + 2, ), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(BaCa + 2, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, BaCa + 2.5, ), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(BaCa + 2.5, 21, na.pad = TRUE), ), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 3.5), breaks = c(0.3, 0.8, 1.3, 1.8, 2.3, 2.8), labels = c("T. maxima 29", "T. maxima 84", "T. squamosa 85", "T. squamosa FRS1", "T. squamosa M1", "T. squamosina SQSA1")) +
    ggtitle("MTM of Ba/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

combined_MTM_offset_plots <- grid.arrange(SrCa_MTM_offset_plot, MgCa_MTM_offset_plot, MnCa_MTM_offset_plot, BaCa_MTM_offset_plot, ncol = 2)

# Log plots
SrCa_MTM_log_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, log(SrCa), col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(log(SrCa), 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, log(SrCa) + 0.5, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(log(SrCa) + 0.5, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, log(SrCa) + 1, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(log(SrCa) + 1, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, log(SrCa) + 1.5, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(log(SrCa) + 1.5, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, log(SrCa) + 2, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(log(SrCa) + 2, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, log(SrCa) + 2.5, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(log(SrCa) + 2.5, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(-8, 2), labels = NULL) +
    ggtitle("MTM of Sr/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MgCa_MTM_log_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, log(MgCa), col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(log(MgCa), 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, log(MgCa) + 0.5, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(log(MgCa) + 0.5, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, log(MgCa) + 1, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(log(MgCa) + 1, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, log(MgCa) + 1.5, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(log(MgCa) + 1.5, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, log(MgCa) + 2, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(log(MgCa) + 2, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, log(MgCa) + 2.5, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(log(MgCa) + 2.5, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(-8, 2), labels = NULL) +
    ggtitle("MTM of Mg/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MnCa_MTM_log_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, log(MnCa), col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(log(MnCa), 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, log(MnCa) + 0.5, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(log(MnCa) + 0.5, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, log(MnCa) + 1, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(log(MnCa) + 1, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, log(MnCa) + 1.5, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(log(MnCa) + 1.5, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, log(MnCa) + 2, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(log(MnCa) + 2, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, log(MnCa) + 2.5, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(log(MnCa) + 2.5, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(-8, 2), labels = NULL) +
    ggtitle("MTM of Mn/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

BaCa_MTM_log_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 3.5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM29, aes(Period, log(BaCa), col = "maxima29"), size = 1, alpha = .1) +
    geom_line(data = MTM29, aes(Period, rollmean(log(BaCa), 21, na.pad = TRUE), col = "maxima29"), size = 1, alpha = .5) +
    geom_line(data = MTM84, aes(Period, log(BaCa) + 0.5, col = "maxima84"), size = 1, alpha = .1) +
    geom_line(data = MTM84, aes(Period, rollmean(log(BaCa) + 0.5, 21, na.pad = TRUE), col = "maxima84"), size = 1, alpha = .5) +
    geom_line(data = MTM85, aes(Period, log(BaCa) + 1, col = "squamosa85"), size = 1, alpha = .1) +
    geom_line(data = MTM85, aes(Period, rollmean(log(BaCa) + 1, 21, na.pad = TRUE), col = "squamosa85"), size = 1, alpha = .5) +
    geom_line(data = MTMFRS1, aes(Period, log(BaCa) + 1.5, col = "squamosaFRS1"), size = 1, alpha = .1) +
    geom_line(data = MTMFRS1, aes(Period, rollmean(log(BaCa) + 1.5, 21, na.pad = TRUE), col = "squamosaFRS1"), size = 1, alpha = .5) +
    geom_line(data = MTMM1, aes(Period, log(BaCa) + 2, col = "squamosaM1"), size = 1, alpha = .1) +
    geom_line(data = MTMM1, aes(Period, rollmean(log(BaCa) + 2, 21, na.pad = TRUE), col = "squamosaM1"), size = 1, alpha = .5) +
    geom_line(data = MTMSQSA1, aes(Period, log(BaCa) + 2.5, col = "squamosinaSQSA1"), size = 1, alpha = .1) +
    geom_line(data = MTMSQSA1, aes(Period, rollmean(log(BaCa) + 2.5, 21, na.pad = TRUE), col = "squamosinaSQSA1"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(-8, 2), labels = NULL) +
    ggtitle("MTM of Ba/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

combined_MTM_log_plots <- grid.arrange(SrCa_MTM_log_plot, MgCa_MTM_log_plot, MnCa_MTM_log_plot, BaCa_MTM_log_plot, ncol = 2)

