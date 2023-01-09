# Script to plot amplitudes and durations of cycles (Figure 5)

require(tidyverse)
require(RColorBrewer)
require(gridExtra)

stats_overview <- read.csv("<path>/BP_stats.csv", header = TRUE)

# Create colorscale
Specimencolors <- c(rep(brewer.pal(11, "RdBu")[1], 2), # T. maxima
    rep(brewer.pal(11, "RdBu")[2], 3), # T. squamosa
    rep(brewer.pal(11, "RdBu")[3], 1), # T. squamosina
    rep(brewer.pal(11, "RdBu")[10], 5)
)
names(Specimencolors) <- c("29", "84", "85", "FRS1", "M1", "SQSA1", "2_1", "2_2", "3_1", "3_2", "4")

# Create colorscale
Specimencolors <- c(brewer.pal(11, "RdBu")[11:7], "#000000", brewer.pal(11, "RdBu")[5:1])
names(Specimencolors) <- c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1", "PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")

SrCa_ampplot <- ggplot(data = subset(stats_overview, Record == "SrCa")) +
    geom_pointrange(aes(x = mean_duration,
        xmin = mean_duration - sd_duration,
        xmax = mean_duration + sd_duration,
        y = quantile50,
        colour = Specimen,
        alpha = significance)) +
    geom_segment(aes(x = mean_duration,
            xend = mean_duration,
            y = min_amplitude,
            yend = replace(max_amplitude, which(max_amplitude > 0.5), 0.5),
            colour = Specimen
        ),
        arrow = arrow(type = "closed",
            length = unit(.2, "cm")),
        alpha = 0.5) +
    geom_segment(aes(x = mean_duration,
            xend = mean_duration,
            y = quantile25,
            yend = replace(quantile75, which(quantile75 > 0.5), 0.5),
            colour = Specimen),
        alpha = 0.5,
        size = 2) +
    scale_y_continuous("Sr/Ca amplitude [mmol/mol]",
        breaks = seq(0, 0.5, 0.1),
        limits = c(0, 0.5)) +
    scale_x_continuous("Cycle duration [days]",
        breaks = seq(0, 2, 0.5),
        labels = seq(0, 2, 0.5),
        limits = c(0, 2)) +
#    geom_text(aes(x = mean_duration,
#            y = mean_amplitude * 0.8,
#            label = significance,
#            color = Specimen,
#            alpha = significance),
#        size = 3,
#        nudge_x = -0.07,
#        nudge_y = 0.01) +
    scale_colour_manual(values = Specimencolors) +
    theme_bw()

MgCa_ampplot <- ggplot(data = subset(stats_overview, Record == "MgCa")) +
    geom_pointrange(aes(x = mean_duration,
        xmin = mean_duration - sd_duration,
        xmax = mean_duration + sd_duration,
        y = quantile50,
        colour = Specimen,
        alpha = significance)) +
    geom_segment(aes(x = mean_duration,
            xend = mean_duration,
            y = min_amplitude,
            yend = replace(max_amplitude, which(max_amplitude > 0.5), 0.5),
            colour = Specimen
        ),
        arrow = arrow(type = "closed",
            length = unit(.2, "cm")),
        alpha = 0.5) +
    geom_segment(aes(x = mean_duration,
            xend = mean_duration,
            y = quantile25,
            yend = replace(quantile75, which(quantile75 > 0.5), 0.5),
            colour = Specimen),
        alpha = 0.5,
        size = 2) +
    scale_y_continuous("Mg/Ca amplitude [mmol/mol]",
        breaks = seq(0, 0.5, 0.1),
        limits = c(0, 0.5)) +
    scale_x_continuous("Cycle duration [days]",
        breaks = seq(0, 2, 0.5),
        labels = seq(0, 2, 0.5),
        limits = c(0, 2)) +
#    geom_text(aes(x = mean_duration,
#            y = 0.5,
#            label = Specimen,
#            color = Specimen,
#            alpha = significance),
#        size = 3,
#        position = position_dodge2(width = 0.3)) +
#    geom_text(aes(x = mean_duration,
#            y = mean_amplitude * 0.8,
#            label = significance,
#            color = Specimen,
#            alpha = significance),
#        size = 3,
#        nudge_x = -0.07,
#        nudge_y = 0.01) +
    scale_colour_manual(values = Specimencolors) +
    theme_bw()

MnCa_ampplot <- ggplot(data = subset(stats_overview, Record == "MnCa")) +
    geom_pointrange(aes(x = mean_duration,
        xmin = mean_duration - sd_duration,
        xmax = mean_duration + sd_duration,
        y = quantile50,
        colour = Specimen,
        alpha = significance)) +
    geom_segment(aes(x = mean_duration,
            xend = mean_duration,
            y = min_amplitude,
            yend = replace(max_amplitude, which(max_amplitude > 0.003), 0.003),
            colour = Specimen
        ),
        arrow = arrow(type = "closed",
            length = unit(.2, "cm")),
        alpha = 0.5) +
    geom_segment(aes(x = mean_duration,
            xend = mean_duration,
            y = quantile25,
            yend = replace(quantile75, which(quantile75 > 0.003), 0.003),
            colour = Specimen),
        alpha = 0.5,
        size = 2) +
    scale_y_continuous("Mn/Ca amplitude [umol/mol]",
        breaks = seq(0, 0.003, 0.0005),
        labels = seq(0, 3, .5),
        limits = c(0, 0.003)) +
    scale_x_continuous("Cycle duration [days]",
        breaks = seq(0, 2, 0.5),
        labels = seq(0, 2, 0.5),
        limits = c(0, 2)) +
#    geom_text(aes(x = mean_duration,
#            y = mean_amplitude * 0.8,
#            label = significance,
#            color = Specimen,
#            alpha = significance),
#        size = 3,
#        nudge_x = -0.07,
#        nudge_y = 0.01) +
    scale_colour_manual(values = Specimencolors) +
    theme_bw()

BaCa_ampplot <- ggplot(data = subset(stats_overview, Record == "BaCa")) +
    geom_pointrange(aes(x = mean_duration,
        xmin = mean_duration - sd_duration,
        xmax = mean_duration + sd_duration,
        y = quantile50,
        colour = Specimen,
        alpha = significance)) +
    geom_segment(aes(x = mean_duration,
            xend = mean_duration,
            y = min_amplitude,
            yend = replace(max_amplitude, which(max_amplitude > 0.003), 0.003),
            colour = Specimen
        ),
        arrow = arrow(type = "closed",
            length = unit(.2, "cm")),
        alpha = 0.5) +
    geom_segment(aes(x = mean_duration,
            xend = mean_duration,
            y = quantile25,
            yend = replace(quantile75, which(quantile75 > 0.003), 0.003),
            colour = Specimen),
        alpha = 0.5,
        size = 2) +
    scale_y_continuous("Ba/Ca amplitude [umol/mol]",
        breaks = seq(0, 0.003, 0.0005),
        labels = seq(0, 3, .5),
        limits = c(0, 0.003)) +
    scale_x_continuous("Cycle duration [days]",
        breaks = seq(0, 2, 0.5),
        labels = seq(0, 2, 0.5),
        limits = c(0, 2)) +
#    geom_text(aes(x = mean_duration,
#            y = mean_amplitude * 0.8,
#            label = significance,
#            color = Specimen,
#            alpha = significance),
#        size = 3,
#        nudge_x = -0.07,
#        nudge_y = 0.01) +
    scale_colour_manual(values = Specimencolors) +
    theme_bw()

Combined_ampplot <- grid.arrange(SrCa_ampplot + theme(legend.position = "none"),
    MgCa_ampplot + theme(legend.position = "none"),
    MnCa_ampplot + theme(legend.position = "none"),
    BaCa_ampplot + theme(legend.position = "none"),
    ncol = 2)