# Function to bin TE data into N equal time bins along cycles found in bandpass filters

bandpass_binning <- function(bandpass_matrix, Nbin){
    # Find derivatives of bandpass filters for stacking
    bandpass_diff <- cbind(data.frame(Day = bandpass_matrix$Day[-1]), bandpass_matrix[-1, -c(1:3, ncol(bandpass_matrix))] - bandpass_matrix[-nrow(bandpass_matrix), -c(1:3, ncol(bandpass_matrix))])

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
        colnames(summary) <- paste(colnames(bandpass_matrix)[i + 3], colnames(summary)) # Adapt colnames before adding result to matrix

        # Add binned result to matrix
        binned_data <- cbind(binned_data, summary[, -1])
    }
    return(binned_data)
}