# Script to generate combined MTM plots of pectinid records (Figure 3)

require(tidyverse)
require(zoo)
require(RColorBrewer)
require(gridExtra)

# Load data
MTM2_1 <- read.csv("<path>/dat2_1_MTM.csv", header = TRUE)
MTM2_2 <- read.csv("<path>/dat2_2_MTM.csv", header = TRUE)
MTM3_1 <- read.csv("<path>/dat3_1_MTM.csv", header = TRUE)
MTM3_2 <- read.csv("<path>/dat3_2_MTM.csv", header = TRUE)
MTM4 <- read.csv("<path>/dat4_MTM.csv", header = TRUE)

# Normalize powerspectra
MTM2_1[, -c(1, 2)] <- t(t(MTM2_1[, -c(1, 2)]) / apply(MTM2_1[, -c(1, 2)], 2, max))
MTM2_2[, -c(1, 2)] <- t(t(MTM2_2[, -c(1, 2)]) / apply(MTM2_2[, -c(1, 2)], 2, max))
MTM3_1[, -c(1, 2)] <- t(t(MTM3_1[, -c(1, 2)]) / apply(MTM3_1[, -c(1, 2)], 2, max))
MTM3_2[, -c(1, 2)] <- t(t(MTM3_2[, -c(1, 2)]) / apply(MTM3_2[, -c(1, 2)], 2, max))
MTM4[, -c(1, 2)] <- t(t(MTM4[, -c(1, 2)]) / apply(MTM4[, -c(1, 2)], 2, max))

# Create colorscale
Specimencolors <- brewer.pal(11, "RdBu")[1:11]
names(Specimencolors) <- c("maxima29", "maxima84", "squamosa85", "squamosaFRS1", "squamosaM1", "squamosinaSQSA1", "P. maximus 2_1", "P. maximus 2_2", "P. maximus 3_1", "P. maximus 3_2", "P. maximus 4")

# Plot powerspectra
SrCa_MTM_norm_plot <- ggplot() +
    geom_line(data = MTM2_1, aes(Period, SrCa, col = "P. maximus 2_1"), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "P. maximus 2_1"), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, SrCa, col = "P. maximus 2_2"), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "P. maximus 2_2"), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, SrCa, col = "P. maximus 3_1"), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "P. maximus 3_1"), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, SrCa, col = "P. maximus 3_2"), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "P. maximus 3_2"), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, SrCa, col = "P. maximus 4"), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "P. maximus 4"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 1)) +
    ggtitle("MTM of Sr/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme(legend.position = "none")

MgCa_MTM_norm_plot <- ggplot() +
    geom_line(data = MTM2_1, aes(Period, MgCa, col = "P. maximus 2_1"), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "P. maximus 2_1"), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, MgCa, col = "P. maximus 2_2"), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "P. maximus 2_2"), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, MgCa, col = "P. maximus 3_1"), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "P. maximus 3_1"), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, MgCa, col = "P. maximus 3_2"), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "P. maximus 3_2"), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, MgCa, col = "P. maximus 4"), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "P. maximus 4"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 1)) +
    ggtitle("MTM of Mg/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme(legend.position = "none")

MnCa_MTM_norm_plot <- ggplot() +
    geom_line(data = MTM2_1, aes(Period, MnCa, col = "P. maximus 2_1"), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "P. maximus 2_1"), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, MnCa, col = "P. maximus 2_2"), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "P. maximus 2_2"), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, MnCa, col = "P. maximus 3_1"), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "P. maximus 3_1"), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, MnCa, col = "P. maximus 3_2"), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "P. maximus 3_2"), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, MnCa, col = "P. maximus 4"), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "P. maximus 4"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 1)) +
    ggtitle("MTM of Mn/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme(legend.position = "none")

BaCa_MTM_norm_plot <- ggplot() +
    geom_line(data = MTM2_1, aes(Period, BaCa, col = "P. maximus 2_1"), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "P. maximus 2_1"), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, BaCa, col = "P. maximus 2_2"), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "P. maximus 2_2"), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, BaCa, col = "P. maximus 3_1"), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "P. maximus 3_1"), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, BaCa, col = "P. maximus 3_2"), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "P. maximus 3_2"), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, BaCa, col = "P. maximus 4"), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "P. maximus 4"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 1)) +
    ggtitle("MTM of Ba/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme(legend.position = "none")

Tide_MTM_data <- data.frame(Period = 1 / Tides_MTM$Frequency, Tides = Tides_MTM$Power)
Tide_MTM_plot <- ggplot(data = Tide_MTM_data, aes(Period, Tides)) +
    geom_line(size = 1, alpha = .1, color = "black") +
    geom_smooth(size = 1, method = "loess", se = FALSE, span = 0.05, color = "black") +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-1, 2, 1))), limits = c(0.1, 100)) +
    scale_y_log10("Power", breaks = 10 ^ seq(-7, 0, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-7, 0, 1))), limits = c(10 ^ -6, 10 ^ -1)) +
    ggtitle("MTM of Tide data")

combined_MTM_plots <- grid.arrange(SrCa_MTM_norm_plot, MgCa_MTM_norm_plot, MnCa_MTM_norm_plot, BaCa_MTM_norm_plot, ncol = 2)

# Offset plots
SrCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM2_1, aes(Period, SrCa, col = "P. maximus 2_1"), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "P. maximus 2_1"), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, SrCa + 1, col = "P. maximus 2_2"), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(SrCa + 1, 21, na.pad = TRUE), col = "P. maximus 2_2"), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, SrCa + 2, col = "P. maximus 3_1"), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(SrCa + 2, 21, na.pad = TRUE), col = "P. maximus 3_1"), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, SrCa + 3, col = "P. maximus 3_2"), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(SrCa + 3, 21, na.pad = TRUE), col = "P. maximus 3_2"), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, SrCa + 4, col = "P. maximus 4"), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(SrCa + 4, 21, na.pad = TRUE), col = "P. maximus 4"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 5), breaks = c(0.3, 1.3, 2.3, 3.3, 4.3), labels = c("P. maximus 2.1", "P. maximus 2.2", "P. maximus 3.1", "P. maximus 3.2", "P. maximus 4")) +
    ggtitle("MTM of Sr/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MgCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM2_1, aes(Period, MgCa, col = "P. maximus 2_1"), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "P. maximus 2_1"), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, MgCa + 1, col = "P. maximus 2_2"), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(MgCa + 1, 21, na.pad = TRUE), col = "P. maximus 2_2"), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, MgCa + 2, col = "P. maximus 3_1"), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(MgCa + 2, 21, na.pad = TRUE), col = "P. maximus 3_1"), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, MgCa + 3, col = "P. maximus 3_2"), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(MgCa + 3, 21, na.pad = TRUE), col = "P. maximus 3_2"), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, MgCa + 4, col = "P. maximus 4"), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(MgCa + 4, 21, na.pad = TRUE), col = "P. maximus 4"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 5), breaks = c(0.3, 1.3, 2.3, 3.3, 4.3), labels = c("P. maximus 2.1", "P. maximus 2.2", "P. maximus 3.1", "P. maximus 3.2", "P. maximus 4")) +
    ggtitle("MTM of Mg/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MnCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM2_1, aes(Period, MnCa, col = "P. maximus 2_1"), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "P. maximus 2_1"), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, MnCa + 1, col = "P. maximus 2_2"), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(MnCa + 1, 21, na.pad = TRUE), col = "P. maximus 2_2"), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, MnCa + 2, col = "P. maximus 3_1"), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(MnCa + 2, 21, na.pad = TRUE), col = "P. maximus 3_1"), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, MnCa + 3, col = "P. maximus 3_2"), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(MnCa + 3, 21, na.pad = TRUE), col = "P. maximus 3_2"), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, MnCa + 4, col = "P. maximus 4"), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(MnCa + 4, 21, na.pad = TRUE), col = "P. maximus 4"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 5), breaks = c(0.3, 1.3, 2.3, 3.3, 4.3), labels = c("P. maximus 2.1", "P. maximus 2.2", "P. maximus 3.1", "P. maximus 3.2", "P. maximus 4")) +
    ggtitle("MTM of Mn/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

BaCa_MTM_offset_plot <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM2_1, aes(Period, BaCa, col = "P. maximus 2_1"), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "P. maximus 2_1"), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, BaCa + 1, col = "P. maximus 2_2"), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(BaCa + 1, 21, na.pad = TRUE), col = "P. maximus 2_2"), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, BaCa + 2, col = "P. maximus 3_1"), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(BaCa + 2, 21, na.pad = TRUE), col = "P. maximus 3_1"), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, BaCa + 3, col = "P. maximus 3_2"), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(BaCa + 3, 21, na.pad = TRUE), col = "P. maximus 3_2"), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, BaCa + 4, col = "P. maximus 4"), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(BaCa + 4, 21, na.pad = TRUE), col = "P. maximus 4"), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 5), breaks = c(0.3, 1.3, 2.3, 3.3, 4.3), labels = c("P. maximus 2.1", "P. maximus 2.2", "P. maximus 3.1", "P. maximus 3.2", "P. maximus 4")) +
    ggtitle("MTM of Ba/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

combined_MTM_offset_plots <- grid.arrange(SrCa_MTM_offset_plot, MgCa_MTM_offset_plot, MnCa_MTM_offset_plot, BaCa_MTM_offset_plot, ncol = 2)


# Offset plots no color
SrCa_MTM_offset_plot_nocol <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM2_1, aes(Period, SrCa), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(SrCa, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, SrCa + 1), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(SrCa + 1, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, SrCa + 2), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(SrCa + 2, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, SrCa + 3), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(SrCa + 3, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, SrCa + 4), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(SrCa + 4, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 5), breaks = c(0.3, 1.3, 2.3, 3.3, 4.3), labels = c("P. maximus 2.1", "P. maximus 2.2", "P. maximus 3.1", "P. maximus 3.2", "P. maximus 4")) +
    ggtitle("MTM of Sr/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MgCa_MTM_offset_plot_nocol <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM2_1, aes(Period, MgCa), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(MgCa, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, MgCa + 1), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(MgCa + 1, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, MgCa + 2), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(MgCa + 2, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, MgCa + 3), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(MgCa + 3, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, MgCa + 4), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(MgCa + 4, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 5), breaks = c(0.3, 1.3, 2.3, 3.3, 4.3), labels = c("P. maximus 2.1", "P. maximus 2.2", "P. maximus 3.1", "P. maximus 3.2", "P. maximus 4")) +
    ggtitle("MTM of Mg/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

MnCa_MTM_offset_plot_nocol <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM2_1, aes(Period, MnCa), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(MnCa, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, MnCa + 1), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(MnCa + 1, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, MnCa + 2), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(MnCa + 2, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, MnCa + 3), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(MnCa + 3, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, MnCa + 4), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(MnCa + 4, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 5), breaks = c(0.3, 1.3, 2.3, 3.3, 4.3), labels = c("P. maximus 2.1", "P. maximus 2.2", "P. maximus 3.1", "P. maximus 3.2", "P. maximus 4")) +
    ggtitle("MTM of Mn/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

BaCa_MTM_offset_plot_nocol <- ggplot() +
    geom_ribbon(aes(ymin = 0, ymax = 5, x = c(0.4, 0.6)), alpha = 0.1, fill = "red", size = 0) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 7, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 28, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_line(data = MTM2_1, aes(Period, BaCa), size = 1, alpha = .1) +
    geom_line(data = MTM2_1, aes(Period, rollmean(BaCa, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM2_2, aes(Period, BaCa + 1), size = 1, alpha = .1) +
    geom_line(data = MTM2_2, aes(Period, rollmean(BaCa + 1, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM3_1, aes(Period, BaCa + 2), size = 1, alpha = .1) +
    geom_line(data = MTM3_1, aes(Period, rollmean(BaCa + 2, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM3_2, aes(Period, BaCa + 3), size = 1, alpha = .1) +
    geom_line(data = MTM3_2, aes(Period, rollmean(BaCa + 3, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    geom_line(data = MTM4, aes(Period, BaCa + 4), size = 1, alpha = .1) +
    geom_line(data = MTM4, aes(Period, rollmean(BaCa + 4, 21, na.pad = TRUE)), size = 1, alpha = .5) +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 5), breaks = c(0.3, 1.3, 2.3, 3.3, 4.3), labels = c("P. maximus 2.1", "P. maximus 2.2", "P. maximus 3.1", "P. maximus 3.2", "P. maximus 4")) +
    ggtitle("MTM of Ba/Ca data") +
    scale_colour_manual(values = Specimencolors) +
    theme_bw() +
    theme(legend.position = "none")

combined_MTM_offset_plots_nocol <- grid.arrange(SrCa_MTM_offset_plot_nocol, MgCa_MTM_offset_plot_nocol, MnCa_MTM_offset_plot_nocol, BaCa_MTM_offset_plot_nocol, ncol = 2)

