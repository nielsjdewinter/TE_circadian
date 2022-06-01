# Script to apply wavelet analysis on trace element data (S9)

library(dplR)
library(RColorBrewer)
library(lubridate)
library(signal)
library(tidyverse)

# Load time-alighned data
load("<path>/Shell2_1_dated_aligned_cross.rda") # Load dat2_1 data
dat2_1 <- dat_trim # Rename to avoid conflict

# Load tide data
load("<path>/Tides_Brest_2019.rda")
dat19_trim$Day <- as.numeric(dat19_trim$Date - ymd_hms("2019-01-01 00:00:00")) / 3600 / 24 # Convert datetime to day
Tides <- dat19_trim

# Trim P. maximus 2_1 data
dat2_1_trim <- dat2_1[which(dat2_1$Depth > 7000), ]
dat2_1_trim2 <- dat2_1_trim
dat2_1_trim2$BaCa[dat2_1_trim2$BaCa == 0] <- mean(dat2_1_trim2$BaCa[dat2_1_trim2$BaCa != 0]) # Remove zeroes from BaCa data for filtering and smoothing

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

# Apply smoothing
dat2_1b <- data.frame(
    Depth = dat2_1_trim$Depth,
    Day = dat2_1_trim$Day,
    MgCa = dat2_1_trim$MgCa - dat2_1_loess02$MgCa,
    SrCa = dat2_1_trim$Sr88Ca - dat2_1_loess02$Sr88Ca,
    MnCa = dat2_1_trim$MnCa - dat2_1_loess02$MnCa,
    BaCa = dat2_1_trim2$BaCa - dat2_1_loess02$BaCa,
    datetime = dat2_1_trim$datetime
)

# Apply Savitzky-Golay filter on dataset
Dres <- diff(range(dat2_1$Depth))/length(dat2_1$Depth)
dat2_1_filtered <- as.data.frame(cbind(dat2_1b$Depth,
                        dat2_1b$Day,
                        apply(dat2_1b[, -c(1, 2, 7)], 2, sgolayfilt, p = 3, n = round(50 / Dres, 0) - 1 + round(50 / Dres, 0) %% 2)
                        )) # Isolate sampling resolution and apply SG filter
colnames(dat2_1_filtered)[c(1,2)] <- c("Depth", "Day")

# Bin data
# Bin data in hourly time bins
dat2_1b$bin <- round(dat2_1b$Day, 1) # Create bins one tenth of a day
dat2_1_bin <- dat2_1b %>%
group_by(bin) %>%
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
) %>%
ungroup()
colnames(dat2_1_bin)[1] <- "Day"

# Calculate morlet wavelet function, including cone of influence and significance levels p < 0.05

# Sr/Ca
wavelet2_1Sr <- morlet(y1= dat2_1b$SrCa, x1 = dat2_1b$Day, dj = 0.1, siglvl = 0.95)
wavelet2_1Sr_filtered <- morlet(y1= dat2_1_filtered$SrCa, x1 = dat2_1_filtered$Day, dj = 0.1, siglvl = 0.95)
wavelet2_1Sr_binned <- morlet(y1= dat2_1_bin$SrCa_mean, x1 = dat2_1_bin$Day, dj = 0.1, siglvl = 0.95)

# Plot wavelet
wavelet.plot(wave.list = wavelet2_1Sr,
    wavelet.levels = quantile(wavelet2_1Sr$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Sr$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Sr$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

wavelet.plot(wave.list = wavelet2_1Sr_filtered,
    wavelet.levels = quantile(wavelet2_1Sr_filtered$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Sr_filtered$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Sr_filtered$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

wavelet.plot(wave.list = wavelet2_1Sr_binned,
    wavelet.levels = quantile(wavelet2_1Sr_binned$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Sr_binned$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Sr_binned$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

# Mg/Ca
wavelet2_1Mg <- morlet(y1= dat2_1b$MgCa, x1 = dat2_1b$Day, dj = 0.1, siglvl = 0.95)
wavelet2_1Mg_filtered <- morlet(y1= dat2_1_filtered$MgCa, x1 = dat2_1_filtered$Day, dj = 0.1, siglvl = 0.95)
wavelet2_1Mg_binned <- morlet(y1= dat2_1_bin$MgCa_mean, x1 = dat2_1_bin$Day, dj = 0.1, siglvl = 0.95)

# Plot wavelet
wavelet.plot(wave.list = wavelet2_1Mg,
    wavelet.levels = quantile(wavelet2_1Mg$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Mg$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Mg$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

wavelet.plot(wave.list = wavelet2_1Mg_filtered,
    wavelet.levels = quantile(wavelet2_1Mg_filtered$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Mg_filtered$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Mg_filtered$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

wavelet.plot(wave.list = wavelet2_1Mg_binned,
    wavelet.levels = quantile(wavelet2_1Mg_binned$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Mg_binned$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Mg_binned$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

# Mn/Ca
wavelet2_1Mn <- morlet(y1= dat2_1b$MnCa, x1 = dat2_1b$Day, dj = 0.1, siglvl = 0.95)
wavelet2_1Mn_filtered <- morlet(y1= dat2_1_filtered$MnCa, x1 = dat2_1_filtered$Day, dj = 0.1, siglvl = 0.95)
wavelet2_1Mn_binned <- morlet(y1= dat2_1_bin$MnCa_mean, x1 = dat2_1_bin$Day, dj = 0.1, siglvl = 0.95)

# Plot wavelet
wavelet.plot(wave.list = wavelet2_1Mn,
    wavelet.levels = quantile(wavelet2_1Mn$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Mn$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Mn$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

wavelet.plot(wave.list = wavelet2_1Mn_filtered,
    wavelet.levels = quantile(wavelet2_1Mn_filtered$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Mn_filtered$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Mn_filtered$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

wavelet.plot(wave.list = wavelet2_1Mn_binned,
    wavelet.levels = quantile(wavelet2_1Mn_binned$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Mn_binned$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Mn_binned$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

# Ba/Ca
wavelet2_1Ba <- morlet(y1= dat2_1b$BaCa, x1 = dat2_1b$Day, dj = 0.1, siglvl = 0.95)
wavelet2_1Ba_filtered <- morlet(y1= dat2_1_filtered$BaCa, x1 = dat2_1_filtered$Day, dj = 0.1, siglvl = 0.95)
wavelet2_1Ba_binned <- morlet(y1= dat2_1_bin$BaCa_mean, x1 = dat2_1_bin$Day, dj = 0.1, siglvl = 0.95)

# Plot wavelet
wavelet.plot(wave.list = wavelet2_1Ba,
    wavelet.levels = quantile(wavelet2_1Ba$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Ba$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Ba$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

wavelet.plot(wave.list = wavelet2_1Ba_filtered,
    wavelet.levels = quantile(wavelet2_1Ba_filtered$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Ba_filtered$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Ba_filtered$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)

wavelet.plot(wave.list = wavelet2_1Ba_binned,
    wavelet.levels = quantile(wavelet2_1Ba_binned$Power, probs = (0:10) / 10),
    add.coi = TRUE,
    add.sig = FALSE,
    x.lab = gettext("Day", domain = "R-dplR"),
    period.lab = gettext("Period", domain = "R-dplR"),
    crn.lab = gettext("RWT", domain = "R-dplR"),
    key.cols = rev(brewer.pal(length(quantile(wavelet2_1Ba_binned$Power, probs = (0:10) / 10)) - 1, "RdBu")),
    key.lab = parse(text=paste0("\"",
        gettext("Power", domain="R-dplR"),
        "\"^2")
    ),
    add.spline = FALSE,
    f = 0.5,
    nyrs = NULL,
    crn.col = "black",
    crn.lwd = 1,
    coi.col = "black",
    crn.ylim = range(wavelet2_1Ba_binned$y) * c(0.95, 1.05),
    side.by.side = FALSE,
    useRaster = FALSE,
    res = 150,
    reverse.y = FALSE
)