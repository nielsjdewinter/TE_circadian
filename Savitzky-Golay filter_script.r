# Script to apply Savitzky-Golay filter on TE data

require(signal)

load("<Name smoothed dataset>")

# Find sampling resolution
SR <- diff(range(smoothed_dataset$Depth)) / length(smoothed_dataset$Depth)

dat_filtered <- as.data.frame(cbind(smoothed_dataset$Depth,
                        smoothed_dataset$Day,
                        apply(smoothed_dataset[, -c(1, 2)], 2, sgolayfilt, p = 3, n = round(50 / SR, 0) - 1 + round(50 / SR, 0) %% 2)
                        )) # Isolate sampling resolution and apply SG filter
colnames(dat_filtered)[c(1,2)] <- c("Depth", "Day")

daystack_filtered <- data.frame(Time = (dat_filtered$Day %% 1),
                        MgCa = dat_filtered$MgCa,
                        MnCa = dat_filtered$MnCa,
                        SrCa = dat_filtered$SrCa,
                        BaCa = dat_filtered$BaCa)

daystack_filtered <- daystack_filtered[order(daystack_filtered$Time), ]