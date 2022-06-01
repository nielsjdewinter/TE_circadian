# Script to apply LOESS smoothing on TE data
load("<name dataset>")

# Apply LOESS smoothing and filtering

dat_loess <- data.frame(Depth = dataset$Depth,
                        Day = dataset$Day,
                        MgCa = dataset$MgCa - predict(loess(dataset$MgCa ~ dataset$Day, span = 0.1)),
                        MnCa = dataset$MnCa - predict(loess(dataset$MnCa ~ dataset$Day, span = 0.1)),
                        SrCa = dataset$Sr88Ca - predict(loess(dataset$Sr88Ca ~ dataset$Day, span = 0.1)),
                        BaCa = dataset$MgCa - predict(loess(dataset$BaCa ~ dataset$Day, span = 0.1))
                        ) # LOESS span of 0.1 means approximately 50 day period
