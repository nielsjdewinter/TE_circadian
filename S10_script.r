# Script to compare daily and tidal stacks Pectinids vs Tridacnids (S10)

# ------------------------------------------------------------------------------
# PREPARE DATA

require(tidyverse)
require(RColorBrewer)
require(gridExtra)
require(signal)

# Load bandpass filter data
PM2_1_bp <- read.csv("<path>/dat2_1_bp.csv", header = TRUE) # Load Shell2_1 bp data
PM2_1_bp$datetime <- NULL # Remove datetime column
PM2_2_bp <- read.csv("<path>/dat2_2_bp.csv", header = TRUE) # Load Shell2_2 bp data
PM2_2_bp$datetime <- NULL # Remove datetime column
PM3_1_bp <- read.csv("<path>/dat3_1_bp.csv", header = TRUE) # Load Shell3_1 bp data
PM3_1_bp$datetime <- NULL # Remove datetime column
PM3_2_bp <- read.csv("<path>/dat3_2_bp.csv", header = TRUE) # Load Shell3_2 bp data
PM3_2_bp$datetime <- NULL # Remove datetime column
PM4_bp <- read.csv("<path>/dat4_bp.csv", header = TRUE) # Load PM2_1 bp data
PM4_bp$datetime <- NULL # Remove datetime column
TM29_bp <- read.csv("<path>/maxima29_bp.csv", header = TRUE) # Load maxima 29 bp data
TM84_bp <- read.csv("<path>/maxima84_bp.csv", header = TRUE) # Load maxima 84 bp data
TS85_bp <- read.csv("<path>/squamosa85_bp.csv", header = TRUE) # Load squamosa 85 bp data
TSFRS1_bp <- read.csv("<path>/squamosaFRS1_bp.csv", header = TRUE) # Load squamosa FRS1 bp data
TSM1_bp <- read.csv("<path>/squamosaM1_bp.csv", header = TRUE) # Load squamosa M1 bp data
SQSA1_bp <- read.csv("<path>/squamosinaSQSA1_bp.csv", header = TRUE) # Load squamosina SQSA1 bp data

# Load overview of bandpass statistics
stats_overview <- read.csv("<path>/BP_stats.csv", header = TRUE)

# Function for the whole bandpass-based binning procedure
bandpass_binning <- function(bandpass_matrix, Nbin){
    # Find derivatives of bandpass filters for stacking
    bandpass_diff <- cbind(data.frame(Day = bandpass_matrix$Day[-1]), bandpass_matrix[-1, -c(1:3)] - bandpass_matrix[-nrow(bandpass_matrix), -c(1:3)])

    # Find maxima in bandpass filter and equalize length with original BP matrix
    maxima <- as.data.frame(rbind(rep(FALSE, ncol(bandpass_diff)),
        (sign(bandpass_diff[-1, ]) < sign(bandpass_diff[-nrow(bandpass_diff), ])),
        rep(FALSE, ncol(bandpass_diff))
    ))

    # Find minima in bandpass filter and equalize length with original BP matrix
    minima <- as.data.frame(rbind(rep(FALSE, ncol(bandpass_diff)),
        (sign(bandpass_diff[-1, ]) > sign(bandpass_diff[-nrow(bandpass_diff), ])),
        rep(FALSE, ncol(bandpass_diff))
    ))
    maxima$Day <- minima$Day <- bandpass_matrix$Day # Restore Day axis in maxima and minima matrices

    count_matrix <- bandpass_matrix # Create matrix for keeping track of cycle positions
    bin_matrix <- bandpass_matrix # Create matrix for keeping track of bins
    binned_data <- data.frame(bin = seq(0, Nbin, 1)) # Seed data.frame for binned results
    firstmin <- apply(minima[, -1], 2, which.max) # Find first occurrence of minima in bandpass
    for(i in 1:(ncol(minima) - 1)){ # Loop through data columns
        # Assign positions relative to bandpass cycles
        count_matrix[, i + 3] <- approx(x = sort(c(bandpass_matrix$Day[which(maxima[, i + 1])], bandpass_matrix$Day[which(minima[, i + 1])])),
        y = seq(1, length(which(maxima[, i + 1])) + length(which(minima[, i + 1])), 1) / 2,
        xout = bandpass_matrix$Day,
        rule = 1)$y

        # Spot instances where the first extreme value was a maximum and add 0.5 to line up minima and maxima in the stacks
        if(which.min(count_matrix[, i + 3]) < firstmin[i]){
            count_matrix[, i + 3] <- count_matrix[, i + 3] + 0.5
        }

        bin_matrix[, i + 3] <- round((count_matrix[, i + 3] %% 1) * Nbin, 0) / Nbin # Store bin assignment in matrix

        # Bin data and calculate statistics for statistically significant periodicities
        summary <- data.frame(record = bandpass_matrix[-which(is.na(bin_matrix[, i + 3])), i + 3], bin = bin_matrix[-which(is.na(bin_matrix[, i + 3])), i + 3]) %>%
        group_by(bin) %>%
        summarize(
            N = n(),
            mean = mean(record, na.rm = TRUE),
            SD = sd(record, na.rm = TRUE),
            CL95 = qt(0.95, N) * SD / sqrt(N)
        )
        summary$mean <- summary$mean - mean(summary$mean) # Subtract mean fr
        colnames(summary) <- paste(colnames(bandpass_matrix)[i + 3], colnames(summary), sep = "_") # Adapt colnames before adding result to matrix

        # Add binned result to matrix
        binned_data <- cbind(binned_data, summary[, -1])
    }
    return(binned_data)
}

# ------------------------------------------------------------------------------

# Create cycle counting for all records and bin
PM2_1_bpbinned <- bandpass_binning(PM2_1_bp, 10)
PM2_2_bpbinned <- bandpass_binning(PM2_2_bp, 10)
PM3_1_bpbinned <- bandpass_binning(PM3_1_bp, 10)
PM3_2_bpbinned <- bandpass_binning(PM3_2_bp, 10)
PM4_bpbinned <- bandpass_binning(PM4_bp, 10)
TM29_bpbinned <- bandpass_binning(TM29_bp, 10)
TM84_bpbinned <- bandpass_binning(TM84_bp, 10)
TS85_bpbinned <- bandpass_binning(TS85_bp, 10)
TSFRS1_bpbinned <- bandpass_binning(TSFRS1_bp, 10)
TSM1_bpbinned <- bandpass_binning(TSM1_bp, 10)
SQSA1_bpbinned <- bandpass_binning(SQSA1_bp, 10)

# Rename binned data columns
colnames(PM2_1_bpbinned) <- paste("PM2_1", colnames(PM2_1_bpbinned), sep = "_")
colnames(PM2_2_bpbinned) <- paste("PM2_2", colnames(PM2_2_bpbinned), sep = "_")
colnames(PM3_1_bpbinned) <- paste("PM3_1", colnames(PM3_1_bpbinned), sep = "_")
colnames(PM3_2_bpbinned) <- paste("PM3_2", colnames(PM3_2_bpbinned), sep = "_")
colnames(PM4_bpbinned) <- paste("PM4", colnames(PM4_bpbinned), sep = "_")
colnames(TM29_bpbinned) <- paste("TM29", colnames(TM29_bpbinned), sep = "_")
colnames(TM84_bpbinned) <- paste("TM84", colnames(TM84_bpbinned), sep = "_")
colnames(TS85_bpbinned) <- paste("TS85", colnames(TS85_bpbinned), sep = "_")
colnames(TSFRS1_bpbinned) <- paste("TSFRS1", colnames(TSFRS1_bpbinned), sep = "_")
colnames(TSM1_bpbinned) <- paste("TSM1", colnames(TSM1_bpbinned), sep = "_")
colnames(SQSA1_bpbinned) <- paste("SQSA1", colnames(SQSA1_bpbinned), sep = "_")

# Combine binned records into one data frame
BPbin <- cbind(PM2_1_bpbinned,
    PM2_2_bpbinned[, -1],
    PM3_1_bpbinned[, -1],
    PM3_2_bpbinned[, -1],
    PM4_bpbinned[, -1],
    TM29_bpbinned[, -1],
    TM84_bpbinned[, -1],
    TS85_bpbinned[, -1],
    TSFRS1_bpbinned[, -1],
    TSM1_bpbinned[, -1],
    SQSA1_bpbinned[, -1]
)
colnames(BPbin)[1] <- "bin"

# ------------------------------------------------------------------------------

# Create colorscale
Specimencolors <- c(brewer.pal(11, "RdBu")[11:7], "#000000", brewer.pal(11, "RdBu")[5:1])
names(Specimencolors) <- c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1", "PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")

# Plot binning results
Specimens <- c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1", "PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")

# Daily stacks
SrCa_daily_plot <- ggplot(data = BPbin) +
    scale_y_continuous(expression(paste("Sr/Ca [", mu, "mol/mol]")),
        limits = c(-0.13, 0.13),
        breaks = seq(-0.12, 0.12, 0.04),
        labels = seq(-0.12, 0.12, 0.04) * 1000) +
    scale_x_continuous("Fraction of cycle [-]",
        breaks = seq(0, 10, 2),
        labels = seq(0, 1, 0.2)) +
    ggtitle("Sr/Ca daily") +
    theme(legend.position = "none")
# Plot 95% CL ribbons
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "SrCa_daily_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_daily_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_daily_CL95", sep = "_"))]
            )
            SrCa_daily_plot <- SrCa_daily_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_daily_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_daily_CL95", sep = "_"))]
            )
            SrCa_daily_plot <- SrCa_daily_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }
    }
}
# Plot mean lines
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "SrCa_daily_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_daily_mean", sep = "_"))]
            )
            SrCa_daily_plot <- SrCa_daily_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record]),
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_daily_mean", sep = "_"))]
            )
            SrCa_daily_plot <- SrCa_daily_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }
    }
}

MgCa_daily_plot <- ggplot(data = BPbin) +
    scale_y_continuous(expression(paste("Mg/Ca [", mu, "mol/mol]")),
        limits = c(-0.06, 0.06),
        breaks = seq(-0.06, 0.06, 0.02),
        labels = seq(-0.06, 0.06, 0.02) * 1000) +
    scale_x_continuous("Fraction of cycle [-]",
        breaks = seq(0, 10, 2),
        labels = seq(0, 1, 0.2)) +
    ggtitle("Mg/Ca daily") +
    theme(legend.position = "none")
# Plot 95% CL ribbons
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "MgCa_daily_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_daily_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_daily_CL95", sep = "_"))]
            )
            MgCa_daily_plot <- MgCa_daily_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_daily_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_daily_CL95", sep = "_"))]
            )
            MgCa_daily_plot <- MgCa_daily_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }
    }
}
# Plot mean lines
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "MgCa_daily_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_daily_mean", sep = "_"))]
            )
            MgCa_daily_plot <- MgCa_daily_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_daily_mean", sep = "_"))]
            )
            MgCa_daily_plot <- MgCa_daily_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }
    }
}

MnCa_daily_plot <- ggplot(data = BPbin) +
    scale_y_continuous(expression(paste("Mn/Ca [", mu, "mol/mol]")),
        limits = c(-0.001, 0.001),
        breaks = seq(-0.001, 0.001, 0.0005),
        labels = seq(-0.001, 0.001, 0.0005) * 1000) +
    scale_x_continuous("Fraction of cycle [-]",
        breaks = seq(0, 10, 2),
        labels = seq(0, 1, 0.2)) +
    ggtitle("Mn/Ca daily") +
    theme(legend.position = "none")
# Plot 95% CL ribbons
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "MnCa_daily_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_daily_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_daily_CL95", sep = "_"))]
            )
            MnCa_daily_plot <- MnCa_daily_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_daily_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_daily_CL95", sep = "_"))]
            )
            MnCa_daily_plot <- MnCa_daily_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }
    }
}
# Plot mean lines
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "MnCa_daily_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_daily_mean", sep = "_"))]
            )
            MnCa_daily_plot <- MnCa_daily_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_daily_mean", sep = "_"))]
            )
            MnCa_daily_plot <- MnCa_daily_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }
    }
}

BaCa_daily_plot <- ggplot(data = BPbin) +
    scale_y_continuous(expression(paste("Ba/Ca [", mu, "mol/mol]")),
        limits = c(-0.001, 0.001),
        breaks = seq(-0.001, 0.001, 0.0005),
        labels = seq(-0.001, 0.001, 0.0005) * 1000) +
    scale_x_continuous("Fraction of cycle [-]",
        breaks = seq(0, 10, 2),
        labels = seq(0, 1, 0.2)) +
    ggtitle("Ba/Ca daily") +
    theme(legend.position = "none")
# Plot 95% CL ribbons
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "BaCa_daily_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_daily_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_daily_CL95", sep = "_"))]
            )
            BaCa_daily_plot <- BaCa_daily_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_daily_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_daily_CL95", sep = "_"))]
            )
            BaCa_daily_plot <- BaCa_daily_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }
    }
}
# Plot mean lines
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "BaCa_daily_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_daily_mean", sep = "_"))]
            )
            BaCa_daily_plot <- BaCa_daily_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_daily_mean", sep = "_"))]
            )
            BaCa_daily_plot <- BaCa_daily_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }
    }
}

# Tidal stacks
SrCa_tidal_plot <- ggplot(data = BPbin) +
    scale_y_continuous(expression(paste("Sr/Ca [", mu, "mol/mol]")),
        limits = c(-0.13, 0.13),
        breaks = seq(-0.12, 0.12, 0.04),
        labels = seq(-0.12, 0.12, 0.04) * 1000) +
    scale_x_continuous("Fraction of cycle [-]",
        breaks = seq(0, 10, 2),
        labels = seq(0, 1, 0.2)) +
    ggtitle("Sr/Ca tidal") +
    theme(legend.position = "none")
# Plot 95% CL ribbons
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "SrCa_tidal_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_tidal_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_tidal_CL95", sep = "_"))]
            )
            SrCa_tidal_plot <- SrCa_tidal_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_tidal_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_tidal_CL95", sep = "_"))]
            )
            SrCa_tidal_plot <- SrCa_tidal_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }
    }
}
# Plot mean lines
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "SrCa_tidal_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_tidal_mean", sep = "_"))]
            )
            SrCa_tidal_plot <- SrCa_tidal_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "SrCa_tidal_mean", sep = "_"))]
            )
            SrCa_tidal_plot <- SrCa_tidal_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }
    }
}

MgCa_tidal_plot <- ggplot(data = BPbin) +
    scale_y_continuous(expression(paste("Mg/Ca [", mu, "mol/mol]")),
        limits = c(-0.06, 0.06),
        breaks = seq(-0.06, 0.06, 0.02),
        labels = seq(-0.06, 0.06, 0.02) * 1000) +
    scale_x_continuous("Fraction of cycle [-]",
        breaks = seq(0, 10, 2),
        labels = seq(0, 1, 0.2)) +
    ggtitle("Mg/Ca tidal") +
    theme(legend.position = "none")
# Plot 95% CL ribbons
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "MgCa_tidal_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_tidal_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_tidal_CL95", sep = "_"))]
            )
            MgCa_tidal_plot <- MgCa_tidal_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_tidal_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_tidal_CL95", sep = "_"))]
            )
            MgCa_tidal_plot <- MgCa_tidal_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }
    }
}
# Plot mean lines
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "MgCa_tidal_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_tidal_mean", sep = "_"))]
            )
            MgCa_tidal_plot <- MgCa_tidal_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MgCa_tidal_mean", sep = "_"))]
            )
            MgCa_tidal_plot <- MgCa_tidal_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }
    }
}

MnCa_tidal_plot <- ggplot(data = BPbin) +
    scale_y_continuous(expression(paste("Mn/Ca [", mu, "mol/mol]")),
        limits = c(-0.001, 0.001),
        breaks = seq(-0.001, 0.001, 0.0005),
        labels = seq(-0.001, 0.001, 0.0005) * 1000) +
    scale_x_continuous("Fraction of cycle [-]",
        breaks = seq(0, 10, 2),
        labels = seq(0, 1, 0.2)) +
    ggtitle("Mn/Ca tidal") +
    theme(legend.position = "none")
# Plot 95% CL ribbons
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "MnCa_tidal_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_tidal_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_tidal_CL95", sep = "_"))]
            )
            MnCa_tidal_plot <- MnCa_tidal_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_tidal_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_tidal_CL95", sep = "_"))]
            )
            MnCa_tidal_plot <- MnCa_tidal_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }
    }
}
# Plot mean lines
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "MnCa_tidal_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_tidal_mean", sep = "_"))]
            )
            MnCa_tidal_plot <- MnCa_tidal_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "MnCa_tidal_mean", sep = "_"))]
            )
            MnCa_tidal_plot <- MnCa_tidal_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }
    }
}

BaCa_tidal_plot <- ggplot(data = BPbin) +
    scale_y_continuous(expression(paste("Ba/Ca [", mu, "mol/mol]")),
        limits = c(-0.001, 0.001),
        breaks = seq(-0.001, 0.001, 0.0005),
        labels = seq(-0.001, 0.001, 0.0005) * 1000) +
    scale_x_continuous("Fraction of cycle [-]",
        breaks = seq(0, 10, 2),
        labels = seq(0, 1, 0.2)) +
    ggtitle("Ba/Ca tidal") +
    theme(legend.position = "none")
# Plot 95% CL ribbons
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "BaCa_tidal_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_tidal_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_tidal_CL95", sep = "_"))]
            )
            BaCa_tidal_plot <- BaCa_tidal_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_tidal_mean", sep = "_"))],
                CL95 = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_tidal_CL95", sep = "_"))]
            )
            BaCa_tidal_plot <- BaCa_tidal_plot +
                geom_ribbon(data = dat,
                    aes(x = bin,
                        ymin = mean - CL95,
                        ymax = mean + CL95,
                        fill = Specimens[record]),
                    color = unname(Specimencolors[record])
                )
        }
    }
}
# Plot mean lines
for(record in 1:length(Specimens)){
    if(paste(Specimens[record], "BaCa_tidal_mean", sep = "_") %in% colnames(BPbin)){
        if(record <= 6){
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_tidal_mean", sep = "_"))]
            )
            BaCa_tidal_plot <- BaCa_tidal_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }else{
            dat <- data.frame(bin = BPbin$bin,
                mean = BPbin[, which(colnames(BPbin) == paste(Specimens[record], "BaCa_tidal_mean", sep = "_"))]
            )
            BaCa_tidal_plot <- BaCa_tidal_plot +
                geom_line(data = dat,
                    aes(x = bin,
                        y = mean),
                    size = 2,
                    color = unname(Specimencolors[record])
                )
        }
    }
}

Combined_stacks <- grid.arrange(SrCa_daily_plot,
    SrCa_tidal_plot,
    MgCa_daily_plot,
    MgCa_tidal_plot,
    MnCa_daily_plot,
    MnCa_tidal_plot,
    BaCa_daily_plot,
    BaCa_tidal_plot,
    ncol = 2)