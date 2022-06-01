# Script used to explore raw data, detect outliers and create plots per dataset

# Load data

require(lubridate)
require(tidyverse)
require(signal)
require(gridExtra)
require(astrochron)
#require(tidyquant)
require(zoo)

# Load time-alighned data
load("E:/Dropbox/Research/postdoc/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Datasets_dated/Increment_based/Shell2_1_dated_aligned_cross.rda") # Load Shell2_1 data
dat2_1 <- dat_trim # Rename to avoid conflict

# Load tide data
load("C:/Users/Niels/Dropbox/Research/postdoc/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Tide data/Tides_Brest_2019.rda")
dat19_trim$Day <- as.numeric(dat19_trim$Date - ymd_hms("2019-01-01 00:00:00")) / 3600 / 24 # Convert datetime to day
Tides <- dat19_trim

# Trim Shell2_1 data
dat2_1_trim <- dat2_1[which(dat2_1$Depth > 7000), ]

# Create trimplots
Shell2_1_MgCa_trimplot <- ggplot(dat2_1) +
    geom_point(aes(Depth, MgCa), size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 7000, color = "red", linetype = "dashed") +
    scale_x_continuous("Length along record [um]") +
    scale_y_continuous("Mg/Ca [mmol/mol]")

Shell2_1_MnCa_trimplot <- ggplot(dat2_1) +
    geom_point(aes(Depth, MnCa), size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 7000, color = "red", linetype = "dashed") +
    scale_x_continuous("Length along record [um]") +
    scale_y_continuous("Mn/Ca [mmol/mol]")

Shell2_1_SrCa_trimplot <- ggplot(dat2_1) +
    geom_point(aes(Depth, Sr88Ca), size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 7000, color = "red", linetype = "dashed") +
    scale_x_continuous("Length along record [um]") +
    scale_y_continuous("Sr/Ca [mmol/mol]")

Shell2_1_BaCa_trimplot <- ggplot(dat2_1) +
    geom_point(aes(Depth, BaCa), size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 7000, color = "red", linetype = "dashed") +
    scale_x_continuous("Length along record [um]") +
    scale_y_continuous("Ba/Ca [mmol/mol]")

Shell2_1_trimplots <- grid.arrange(Shell2_1_MgCa_trimplot, Shell2_1_SrCa_trimplot, Shell2_1_MnCa_trimplot, Shell2_1_BaCa_trimplot, ncol = 1)

# Apply Savitzky-Golay filter
dat2_1_trim2 <- dat2_1_trim
dat2_1_trim2$BaCa[dat2_1_trim2$BaCa == 0] <- mean(dat2_1_trim2$BaCa[dat2_1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

dat2_1_filtered <- as.data.frame(cbind(dat2_1_trim2$Depth,
    dat2_1_trim2$Day,
    apply(dat2_1_trim2[, c(2, 3, 5, 6)], 2, sgolayfilt, p = 3, n = 21) # 21 pts = about 1-2h (should not affect tidal cycles)
    )) # Remove high-res measurement noise
colnames(dat2_1_filtered)[c(1, 2)] <- c("Depth", "Day")
dat2_1_filtered$datetime <- dat2_1_trim2$datetime

# Apply LOESS smoothing
# Loess span of 0.2 is equivalent to 50 days
dat2_1_loess02 <- data.frame(Depth = dat2_1_trim$Depth,
    Day = dat2_1_trim$Day,
    MgCa = predict(loess(dat2_1_trim$MgCa ~ dat2_1_trim$Day, span = 0.2)),
    MnCa = predict(loess(dat2_1_trim$MnCa ~ dat2_1_trim$Day, span = 0.2)),
    Sr88Ca = predict(loess(dat2_1_trim$Sr88Ca ~ dat2_1_trim$Day, span = 0.2)),
    BaCa = predict(loess(dat2_1_trim2$BaCa ~ dat2_1_trim2$Day, span = 0.2))
)
dat2_1_loess02$datetime <- dat2_1_trim$datetime

# Loess span of 0.1 is equivalent to 20 days
dat2_1_loess01 <- data.frame(Depth = dat2_1_trim$Depth,
    Day = dat2_1_trim$Day,
    MgCa = predict(loess(dat2_1_trim$MgCa ~ dat2_1_trim$Day, span = 0.1)),
    MnCa = predict(loess(dat2_1_trim$MnCa ~ dat2_1_trim$Day, span = 0.1)),
    Sr88Ca = predict(loess(dat2_1_trim$Sr88Ca ~ dat2_1_trim$Day, span = 0.1)),
    BaCa = predict(loess(dat2_1_trim2$BaCa ~ dat2_1_trim2$Day, span = 0.1))
)
dat2_1_loess01$datetime <- dat2_1_trim$datetime

# Loess span of 0.05 is equivalent to 10 days
dat2_1_loess005 <- data.frame(Depth = dat2_1_trim$Depth,
    Day = dat2_1_trim$Day,
    MgCa = predict(loess(dat2_1_trim$MgCa ~ dat2_1_trim$Day, span = 0.05)),
    MnCa = predict(loess(dat2_1_trim$MnCa ~ dat2_1_trim$Day, span = 0.05)),
    Sr88Ca = predict(loess(dat2_1_trim$Sr88Ca ~ dat2_1_trim$Day, span = 0.05)),
    BaCa = predict(loess(dat2_1_trim2$BaCa ~ dat2_1_trim2$Day, span = 0.05))
)
dat2_1_loess005$datetime <- dat2_1_trim$datetime

# Plot smoothing and filtering results
Shell2_1_MgCa_filt <- ggplot(dat2_1_trim, aes(datetime, MgCa)) +
    geom_point(size = 0.1, alpha = 0.2) +
    geom_line(data = dat2_1_filtered, aes(datetime, MgCa), col = "red", alpha = .5) +
    geom_line(data = dat2_1_loess02, aes(datetime, MgCa), col = "blue") +
    scale_x_datetime("Date",
        date_breaks = "month",
        date_minor_breaks = "day") +
    scale_y_continuous("Mg/Ca [mmol/mol]",
        limits = c(0, 7)) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Shell2_1_Sr88Ca_filt <- ggplot(dat2_1_trim, aes(datetime, Sr88Ca)) +
    geom_point(size = 0.1, alpha = 0.2) +
    geom_line(data = dat2_1_filtered, aes(datetime, Sr88Ca), col = "red", alpha = .5) +
    geom_line(data = dat2_1_loess02, aes(datetime, Sr88Ca), col = "blue") +
    scale_x_datetime("Date",
        date_breaks = "month",
        date_minor_breaks = "day") +
    scale_y_continuous("Sr/Ca [mmol/mol]",
        limits = c(0, 2.5)) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Shell2_1_MnCa_filt <- ggplot(dat2_1_trim, aes(datetime, MnCa)) +
    geom_point(size = 0.1, alpha = 0.2) +
    geom_line(data = dat2_1_filtered, aes(datetime, MnCa), col = "red", alpha = .5) +
    geom_line(data = dat2_1_loess02, aes(datetime, MnCa), col = "blue") +
    scale_x_datetime("Date",
        date_breaks = "month",
        date_minor_breaks = "day") +
    scale_y_continuous("Mn/Ca [mmol/mol]",
        limits = c(0, 0.02)) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Shell2_1_BaCa_filt <- ggplot(dat2_1_trim, aes(datetime, BaCa)) +
    geom_point(size = 0.1, alpha = 0.2) +
    geom_line(data = dat2_1_filtered, aes(datetime, BaCa), col = "red", alpha = .5) +
    geom_line(data = dat2_1_loess02, aes(datetime, BaCa), col = "blue") +
    scale_x_datetime("Date",
        date_breaks = "month",
        date_minor_breaks = "day") +
    scale_y_continuous("Ba/Ca [mmol/mol]",
        limits = c(0, 0.006)) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Shell2_1_filtplots <- grid.arrange(Shell2_1_MgCa_filt, Shell2_1_Sr88Ca_filt, Shell2_1_MnCa_filt, Shell2_1_BaCa_filt, ncol = 1)

# Apply filtering and smoothing
dat2_1b <- data.frame(
    Depth = dat2_1_filtered$Depth,
    Day = dat2_1_filtered$Day,
    MgCa = dat2_1_filtered$MgCa - dat2_1_loess02$MgCa,
    SrCa = dat2_1_filtered$Sr88Ca - dat2_1_loess02$Sr88Ca,
    MnCa = dat2_1_filtered$MnCa - dat2_1_loess02$MnCa,
    BaCa = dat2_1_filtered$BaCa - dat2_1_loess02$BaCa,
    datetime = dat2_1_filtered$datetime
)

# Calculate MTM powerspectra

SR_2_1 <- diff(range(dat2_1b$Day))/length(dat2_1b$Day) # Find sampling resolution in time domain

dat2_1_SrCa_lin <- linterp(data.frame(Day = dat2_1b$Day,
        SrCa = dat2_1b$SrCa),
    dt = SR_2_1)

# Run MTM fourier transform
SrCa_MTM_2_1 <- mtm(dat = dat2_1_SrCa_lin,
    demean = FALSE,
    detrend = FALSE,
    siglevel = 0.99,
    ar1 = TRUE,
    output = 1,
    xmin = 1/1500,
    xmax = 1/10,
    pl = 1,
    sigID = TRUE
)

SrCa_MTM_2_1$period <- 1 / SrCa_MTM_2_1$Frequency
write.csv(SrCa_MTM_2_1, "C:/Users/Niels/Dropbox/Research/postdoc/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Data renaissance/Spectral analysis 02/Shell2_1_SrCa_MTM_Day_data.csv")

dat2_1_MgCa_lin <- linterp(data.frame(Day = dat2_1b$Day,
        MgCa = dat2_1b$MgCa),
    dt = SR_2_1)

# Run MTM fourier transform
MgCa_MTM_2_1 <- mtm(dat = dat2_1_MgCa_lin,
    demean = FALSE,
    detrend = FALSE,
    siglevel = 0.99,
    ar1 = TRUE,
    output = 1,
    xmin = 1/1500,
    xmax = 1/10,
    pl = 1,
    sigID = TRUE
)

MgCa_MTM_2_1$period <- 1 / MgCa_MTM_2_1$Frequency
write.csv(MgCa_MTM_2_1, "C:/Users/Niels/Dropbox/Research/postdoc/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Data renaissance/Spectral analysis 02/Shell2_1_MgCa_MTM_Day_data.csv")

dat2_1_MnCa_lin <- linterp(data.frame(Day = dat2_1b$Day,
        MnCa = dat2_1b$MnCa),
    dt = SR_2_1)

# Run MTM fourier transform
MnCa_MTM_2_1 <- mtm(dat = dat2_1_MnCa_lin,
    demean = FALSE,
    detrend = FALSE,
    siglevel = 0.99,
    ar1 = TRUE,
    output = 1,
    xmin = 1/1500,
    xmax = 1/10,
    pl = 1,
    sigID = TRUE
)

MnCa_MTM_2_1$period <- 1 / MnCa_MTM_2_1$Frequency
write.csv(MnCa_MTM_2_1, "C:/Users/Niels/Dropbox/Research/postdoc/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Data renaissance/Spectral analysis 02/Shell2_1_MnCa_MTM_Day_data.csv")

dat2_1_BaCa_lin <- linterp(data.frame(Day = dat2_1b$Day,
        BaCa = dat2_1b$BaCa),
    dt = SR_2_1)

# Run MTM fourier transform
BaCa_MTM_2_1 <- mtm(dat = dat2_1_BaCa_lin,
    demean = FALSE,
    detrend = FALSE,
    siglevel = 0.99,
    ar1 = TRUE,
    output = 1,
    xmin = 1/1500,
    xmax = 1/10,
    pl = 1,
    sigID = TRUE
)

BaCa_MTM_2_1$period <- 1 / BaCa_MTM_2_1$Frequency
write.csv(BaCa_MTM_2_1, "C:/Users/Niels/Dropbox/Research/postdoc/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Data renaissance/Spectral analysis 02/Shell2_1_BaCa_MTM_Day_data.csv")

Tides_lin <- linterp(data.frame(Day = Tides$Day,
        Level = Tides$Level),
    dt = SR_2_1)

# Run MTM fourier transform
Level_MTM_tide <- mtm(dat = Tides_lin,
    demean = FALSE,
    detrend = FALSE,
    siglevel = 0.99,
    ar1 = TRUE,
    output = 1,
    xmin = 1/1500,
    xmax = 1/10,
    pl = 1,
    sigID = TRUE
)

Level_MTM_tide$period <- 1 / Level_MTM_tide$Frequency
write.csv(Level_MTM_tide, "C:/Users/Niels/Dropbox/Research/postdoc/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Data renaissance/Spectral analysis 02/Tide_MTM_Day_data.csv")

dat2_1_MTM <- data.frame(Period = 1 / SrCa_MTM_2_1$Frequency,
    SrCa = SrCa_MTM_2_1$Power,
    MgCa = MgCa_MTM_2_1$Power,
    MnCa = MnCa_MTM_2_1$Power,
    BaCa = BaCa_MTM_2_1$Power,
    Tides = approx(x = Level_MTM_tide$period,
        y = Level_MTM_tide$Power,
        xout = SrCa_MTM_2_1$period)$y
)

write.csv(dat2_1_MTM, "C:/Users/Niels/Dropbox/Research/postdoc/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Data renaissance/Spectral analysis 02/dat2_1_MTM.csv")

# Normalize for plotting
dat2_1_MTMnorm <- data.frame(
    Period = dat2_1_MTM$Period,
    SrCa = dat2_1_MTM$MgCa / max(dat2_1_MTM$MgCa),
    MgCa = dat2_1_MTM$SrCa / max(dat2_1_MTM$SrCa),
    MnCa = dat2_1_MTM$MnCa / max(dat2_1_MTM$MnCa),
    BaCa = dat2_1_MTM$BaCa / max(dat2_1_MTM$BaCa),
    Tides = dat2_1_MTM$Tides / max(dat2_1_MTM$Tides)
)

# Plot normalized power
dat2_1_MTM_normplot <- ggplot(dat2_1_MTMnorm) +
    geom_line(aes(Period, SrCa, col = "Sr/Ca"), size = 1, alpha = .1) +
    geom_line(aes(Period, rollmean(SrCa, 21, na.pad = TRUE), col = "Sr/Ca"), size = 1, alpha = .5) +
    geom_line(aes(Period, MgCa, col = "Mg/Ca"), size = 1, alpha = .1) +
    geom_line(aes(Period, rollmean(MgCa, 21, na.pad = TRUE), col = "Mg/Ca"), size = 1, alpha = .5) +
    geom_line(aes(Period, MnCa, col = "Mn/Ca"), size = 1, alpha = .1) +
    geom_line(aes(Period, rollmean(MnCa, 21, na.pad = TRUE), col = "Mn/Ca"), size = 1, alpha = .5) +
    geom_line(aes(Period, BaCa, col = "Ba/Ca"), size = 1, alpha = .1) +
    geom_line(aes(Period, rollmean(BaCa, 21, na.pad = TRUE), col = "Ba/Ca"), size = 1, alpha = .5) +
    geom_vline(xintercept = 1, size = 1, linetype = "dashed") +
    scale_x_log10("Period (day)", breaks = 10 ^ seq(-1, 2, 1), minor_breaks = as.vector(outer(seq(1, 9, 1), 10 ^ seq(-2, 2, 1))), limits = c(0.1, 100)) +
    scale_y_continuous("Normalized power [0 - 1]", limits = c(0, 1)) +
    ggtitle("MTM periodogram Shell 2_1") +
    scale_color_brewer(palette = "Dark2")

# Filter out interesting frequencies
dat2_1_bp <- data.frame(Depth = dat2_1b$Depth,
    Day = dat2_1_SrCa_lin$Day[-1],
    SrCa_tidal = bandpass(dat = data.frame(Day = dat2_1_SrCa_lin$Day, SrCa = dat2_1_SrCa_lin$SrCa),
        flow = 1 / 0.9, # Lower frequency
        fhigh = 1 / 0.6, # Upper frequency
        win = 0, # Rectangular window
        output = 1,
        genplot = FALSE,
        verbose = FALSE
    )$SrCa[-1],
    SrCa_daily = bandpass(dat = data.frame(Day = dat2_1_SrCa_lin$Day, SrCa = dat2_1_SrCa_lin$SrCa),
        flow = 1 / 1.5, # Lower frequency
        fhigh = 1 / 1.25, # Upper frequency
        win = 0, # Rectangular window
        output = 1,
        genplot = FALSE,
        verbose = FALSE
    )$SrCa[-1],
    MgCa_tidal = bandpass(dat = data.frame(Day = dat2_1_MgCa_lin$Day, MgCa = dat2_1_MgCa_lin$MgCa),
        flow = 1 / 0.9, # Lower frequency
        fhigh = 1 / 0.6, # Upper frequency
        win = 0, # Rectangular window
        output = 1,
        genplot = FALSE,
        verbose = FALSE
    )$MgCa[-1],
    MnCa_tidal = bandpass(dat = data.frame(Day = dat2_1_MnCa_lin$Day, MnCa = dat2_1_MnCa_lin$MnCa),
        flow = 1 / 0.7, # Lower frequency
        fhigh = 1 / 0.4, # Upper frequency
        win = 0, # Rectangular window
        output = 1,
        genplot = FALSE,
        verbose = FALSE
    )$MnCa[-1],
    BaCa_tidal = bandpass(dat = data.frame(Day = dat2_1_BaCa_lin$Day, BaCa = dat2_1_BaCa_lin$BaCa),
        flow = 1 / 0.55, # Lower frequency
        fhigh = 1 / 0.35, # Upper frequency
        win = 0, # Rectangular window
        output = 1,
        genplot = FALSE,
        verbose = FALSE
    )$BaCa[-1],
    BaCa_daily = bandpass(dat = data.frame(Day = dat2_1_BaCa_lin$Day, BaCa = dat2_1_BaCa_lin$BaCa),
        flow = 1 / 1.5, # Lower frequency
        fhigh = 1 / 1.0, # Upper frequency
        win = 0, # Rectangular window
        output = 1,
        genplot = FALSE,
        verbose = FALSE
    )$BaCa[-1],
    datetime = dat2_1b$datetime
)

write.csv(dat2_1_bp, "E:/Dropbox/Research/postdoc/Side projects/Bivalve photosymbiosis and gigantism/Pectinids_Lukas/Data renaissance/Spectral analysis 02/dat2_1_bp.csv")

# Plot area of interest to show bandpass result
Shell2_1_MgCa_tidal_AOI <- ggplot(subset(dat2_1b, Depth < 15000), aes(datetime, MgCa)) +
    geom_point(size = 0.1, alpha = 0.3) +
    geom_line(data = subset(dat2_1_bp, Depth < 15000), aes(datetime, MgCa_tidal), col = "red") +
    scale_x_datetime("Date",
        date_breaks = "day") +
    scale_y_continuous("Mg/Ca [mmol/mol]") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Shell2_1_SrCa_tidal_AOI <- ggplot(subset(dat2_1b, Depth < 15000), aes(datetime, SrCa)) +
    geom_point(size = 0.1, alpha = 0.3) +
    geom_line(data = subset(dat2_1_bp, Depth < 15000), aes(datetime, SrCa_tidal), col = "red") +
    scale_x_datetime("Date",
        date_breaks = "day") +
    scale_y_continuous("Sr/Ca [mmol/mol]") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Shell2_1_SrCa_daily_AOI <- ggplot(subset(dat2_1b, Depth < 15000), aes(datetime, SrCa)) +
    geom_point(size = 0.1, alpha = 0.3) +
    geom_line(data = subset(dat2_1_bp, Depth < 15000), aes(datetime, SrCa_daily), col = "red") +
    scale_x_datetime("Date",
        date_breaks = "day") +
    scale_y_continuous("Sr/Ca [mmol/mol]") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Shell2_1_MnCa_tidal_AOI <- ggplot(subset(dat2_1b, Depth < 15000), aes(datetime, MnCa)) +
    geom_point(size = 0.1, alpha = 0.3) +
    geom_line(data = subset(dat2_1_bp, Depth < 15000), aes(datetime, MnCa_tidal), col = "red") +
    scale_x_datetime("Date",
        date_breaks = "day") +
    scale_y_continuous("Sr/Ca [mmol/mol]") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Shell2_1_BaCa_tidal_AOI <- ggplot(subset(dat2_1b, Depth < 15000), aes(datetime, BaCa)) +
    geom_point(size = 0.1, alpha = 0.3) +
    geom_line(data = subset(dat2_1_bp, Depth < 15000), aes(datetime, BaCa_tidal), col = "red") +
    scale_x_datetime("Date",
        date_breaks = "day") +
    scale_y_continuous("Ba/Ca [mmol/mol]") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Shell2_1_BaCa_daily_AOI <- ggplot(subset(dat2_1b, Depth < 15000), aes(datetime, BaCa)) +
    geom_point(size = 0.1, alpha = 0.3) +
    geom_line(data = subset(dat2_1_bp, Depth < 15000), aes(datetime, BaCa_daily), col = "red") +
    scale_x_datetime("Date",
        date_breaks = "day") +
    scale_y_continuous("Ba/Ca [mmol/mol]") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

Combined_AOI <- grid.arrange(Shell2_1_MgCa_tidal_AOI, Shell2_1_SrCa_daily_AOI, Shell2_1_SrCa_tidal_AOI, Shell2_1_MnCa_tidal_AOI, Shell2_1_BaCa_daily_AOI, Shell2_1_BaCa_tidal_AOI, ncol = 1)