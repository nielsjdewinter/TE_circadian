# Script to create figures to compare age models (S5)
require(xlsx)
require(ggplot2)
require(RColorBrewer)

# ---------------------PREPARE PECTINID GROWTH DATA-----------------------------
# Load laminae counting results
setwd("<path>/Age_models")
Out <- read.xlsx("Laminae_counting_comparison.xlsx", sheetName = "Out")
Cross <- read.xlsx("Laminae_counting_comparison.xlsx", sheetName = "Cross")

colnames(Out)[1] <- colnames(Cross)[1] <- "Day"

# Load time-aligned data, convert to mm
load("<path>/Shell2_1_dated_aligned_cross.rda") # Load Shell2_1_aligned data
Shell2_1_aligned <- dat_trim # Rename to avoid conflict
Shell2_1_aligned$Day_inv <- max(Shell2_1_aligned$Day) - Shell2_1_aligned$Day # Invert Day-axis
Shell2_1_aligned$Depth_inv <- (max(Shell2_1_aligned$Depth_aligned) - Shell2_1_aligned$Depth_aligned) / 1000 # Invert Depth-axis and convert to mm
load("<path>/Shell2_2_dated_cross.rda") # Load Shell2_2_aligned data
Shell2_2_aligned <- dat_trim # Rename to avoid conflict
Shell2_2_aligned$Day_inv <- max(Shell2_2_aligned$Day) - Shell2_2_aligned$Day # Invert Day-axis
Shell2_2_aligned$Depth_inv <- (max(Shell2_2_aligned$Depth) - Shell2_2_aligned$Depth) / 1000 # Invert Depth-axis and convert to mm
load("<path>/Shell3_1_dated_aligned_cross.rda") # Load Shell3_1_aligned data
Shell3_1_aligned <- dat_trim # Rename to avoid conflict
Shell3_1_aligned$Day_inv <- max(Shell3_1_aligned$Day) - Shell3_1_aligned$Day # Invert Day-axis
Shell3_1_aligned$Depth_inv <- (max(Shell3_1_aligned$Depth_aligned) - Shell3_1_aligned$Depth_aligned) / 1000 # Invert Depth-axis and convert to mm
load("<path>/Shell3_2_dated_cross.rda") # Load Shell3_2_aligned data
Shell3_2_aligned <- dat_trim # Rename to avoid conflict
Shell3_2_aligned$Day_inv <- max(Shell3_2_aligned$Day) - Shell3_2_aligned$Day # Invert Day-axis
Shell3_2_aligned$Depth_inv <- (max(Shell3_2_aligned$Depth) - Shell3_2_aligned$Depth) / 1000 # Invert Depth-axis and convert to mm
load("<path>/Shell4_dated_cross.rda") # Load Shell4 data
Shell4_aligned <- dat_trim # Rename to avoid conflict
Shell4_aligned$Day_inv <- max(Shell4_aligned$Day) - Shell4_aligned$Day # Invert Day-axis
Shell4_aligned$Depth_inv <- (max(Shell4_aligned$Depth) - Shell4_aligned$Depth) / 1000 # Invert Depth-axis and convert to mm

# Load tide-aligned data
load("<path>/Shell2_1_aligned_dated_tides.rda") # Load Shell2_1_tide data
Shell2_1_tide <- Shell2_1_trim # Rename
Shell2_1_tide$Day_inv <- max(Shell2_1_tide$Day) - Shell2_1_tide$Day # Invert Day-axis
Shell2_1_tide$Depth_inv <- (max(Shell2_1_tide$Depth_aligned) - Shell2_1_tide$Depth_aligned) / 1000 # Invert Depth-axis and convert to mm
#Shell2_1_tide$Tide_inv <- Shell2_1_tide$Tidestack - min(Shell2_1_tide$Tidestack) # Invert Tide counting
load("<path>/Shell2_2_dated_tides.rda") # Load Shell2_2_tide data
Shell2_2_tide <- Shell2_2 # Rename
Shell2_2_tide$Day_inv <- max(Shell2_2_tide$Day) - Shell2_2_tide$Day # Invert Day-axis
Shell2_2_tide$Depth_inv <- (max(Shell2_2_tide$Depth) - Shell2_2_tide$Depth) / 1000 # Invert Depth-axis and convert to mm
Shell2_2_tide$Tide_inv <- Shell2_2_tide$Tidestack - min(Shell2_2_tide$Tidestack) # Invert Tide counting
load("<path>/Shell3_1_aligned_dated_tides.rda") # Load Shell3_1_tide data
Shell3_1_tide <- Shell3_1 # Rename
Shell3_1_tide$Day_inv <- max(Shell3_1_tide$Day) - Shell3_1_tide$Day # Invert Day-axis
Shell3_1_tide$Depth_inv <- (max(Shell3_1_tide$Depth) - Shell3_1_tide$Depth) / 1000 # Invert Depth-axis and convert to mm
Shell3_1_tide$Tide_inv <- Shell3_1_tide$Tidestack - min(Shell3_1_tide$Tidestack) # Invert Tide counting
load("<path>/Shell3_2_dated_tides.rda") # Load Shell3_2_tide data
Shell3_2_tide <- Shell3_2 # Rename
Shell3_2_tide$Day_inv <- max(Shell3_2_tide$Day) - Shell3_2_tide$Day # Invert Day-axis
Shell3_2_tide$Depth_inv <- (max(Shell3_2_tide$Depth) - Shell3_2_tide$Depth) / 1000 # Invert Depth-axis and convert to mm
Shell3_2_tide$Tide_inv <- Shell3_2_tide$Tidestack - min(Shell3_2_tide$Tidestack) # Invert Tide counting
load("<path>/Shell4_dated_tides.rda") # Load Shell4_tide data
Shell4_tide <- Shell4 # Rename
Shell4_tide$Day_inv <- max(Shell4_tide$Day) - Shell4_tide$Day # Invert Day-axis
Shell4_tide$Depth_inv <- (max(Shell4_tide$Depth) - Shell4_tide$Depth) / 1000 # Invert Depth-axis and convert to mm
Shell4_tide$Tide_inv <- Shell4_tide$Tidestack - min(Shell4_tide$Tidestack) # Invert Tide counting

# --------------------PREPARE TRIDACNID GROWTH DATA-----------------------------
# Von Bertalanffy growth models tridacnids

# Load lamina counting data
TM29 <- read.csv("<path>/29p.csv", header = TRUE)
TM68 <- read.csv("<path>/68p.csv", header = TRUE)
TM84 <- read.csv("<path>/84p.csv", header = TRUE)
TS85 <- read.csv("<path>/85p.csv", header = TRUE)
TSFRS1 <- read.csv("<path>/frs1p.csv", header = TRUE)
SQSA1 <- read.csv("<path>/sqsa1p.csv", header = TRUE)

TM68$X <- as.numeric(rownames(TM68))
TS85$X <- as.numeric(rownames(TS85))

# Collect model parameters, estimate data for specimen M1 from the two other T. squamosa specimens
modelpar <- data.frame(specimen = c("TM29", "TM68", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1"),
    species = c(rep("maxima", 3), rep("squamosa", 3), "squamosina"),
    annual_growth_mean = c(mean(TM29$annualgrowth * 2), # Multiply by 2 because of semidiurnal increments
        mean(TM68$annualgrowth * 2), # Multiply by 2 because of semidiurnal increments
        mean(TM84$annualgrowth),
        mean(TS85$annualgrowth),
        mean(TSFRS1$annualgrowth * 2), # Multiply by 2 because of semidiurnal increments
        mean(mean(TSFRS1$annualgrowth * 2), mean(TS85$annualgrowth)),
        mean(SQSA1$annualgrowth)
        ), # Based on lamina counting (see Plot_counting.r)
    annual_growth_se = c(sd(TM29$annualgrowth)/sqrt(length(TM29$annualgrowth)), # Multiply by 2 because of semidiurnal increments
        sd(TM68$annualgrowth * 2)/sqrt(length(TM68$annualgrowth)), # Multiply by 2 because of semidiurnal increments
        sd(TM84$annualgrowth * 2)/sqrt(length(TM84$annualgrowth)),
        sd(TS85$annualgrowth)/sqrt(length(TS85$annualgrowth)),
        sd(TSFRS1$annualgrowth * 2)/sqrt(length(TSFRS1$annualgrowth)), # Multiply by 2 because of semidiurnal increments
        sqrt(sum(c(sd(TSFRS1$annualgrowth * 2)/sqrt(length(TSFRS1$annualgrowth)), sd(TS85$annualgrowth)/sqrt(length(TS85$annualgrowth))) ^ 2) / 2),
        sd(SQSA1$annualgrowth)/sqrt(length(SQSA1$annualgrowth))
        ), # Based on lamina counting (see Plot_counting.r)
    record_length = c(52.62, NA, 37.57, 39.78, 32.90, 42.42, 38.79), # From LAICPMS line scans
    shell_height = c(40.03, 46.05, 32.29, 34.81, 29.01, 42.53, 31.596), # Based on comparison of LAICPMS tracks with maximum height in straight line on cross sections
    Linf_mean = c(174, 174, 174, 280, 280, 280, 190), # infinite growth parameters based on averages of literature values (Mohammed et al., 2019; https://doi.org/10.1016/j.ejar.2019.02.003; Roa-Quiaoit, 2005, Table 5; https://media.suub.uni-bremen.de/handle/elib/2141) and adjustment between shell length and shell height by Dan
    Linf_se = c(17, 17, 17, 11, 11, 11, 0), # infinite growth parameters based on averages of literature values (Mohammed et al., 2019; https://doi.org/10.1016/j.ejar.2019.02.003; Roa-Quiaoit, 2005, Table 5; https://media.suub.uni-bremen.de/handle/elib/2141)
    k_literature = c(.165, .165, .165, .115, .115, .115, NA)
)

# Create function to model von Bertalanffy growth from annual growth and Linf + error propagation

growthcurve <- function(maxyears = 5, t_int = 0.001, annual_growth, annual_growth_se, Linf, Linf_se, specimen_names, sheight = NA, output = "modeldata"){ # Lt is growth in a year, Linf is theoretical max height for that species in Red Sea +/- SE on the value (Lint_se), sheight is measured height of shell
    years <- seq(0, maxyears, t_int) # create a sequence of years in 0.05 year intervals
    k <- (-log(1 - (annual_growth / Linf))) # calculate von bertalanffy's k from the growth in a year
    k_se <- sqrt((1 / (Linf * (1 - (annual_growth / Linf)))) ^ 2 * annual_growth_se ^ 2 + (1 / (-1 * Linf ^ 2 * (1 - (annual_growth / Linf)))) ^ 2 * Linf_se ^ 2) # Propagate uncertainty on k value through the variance formula (Ku, 1966;  doi:10.6028/jres.070c.025)
    growth <- as.data.frame(t(Linf * (1 - exp(-1 * outer(k, years))))) # model shell height from the known Linf and k value for all specimens using outer product to create matrix with columns per specimen
    colnames(growth) <- paste("growth", specimen_names, sep = "_") # Add colnames for growth matrix
    growth_se <- as.data.frame(t(sqrt((1 - exp(-1 * outer(k, years))) ^ 2 * Linf_se ^ 2 + t(t(-1 * Linf * exp(-1 * outer(k, years))) * -1 * years) ^ 2 * k_se ^ 2))) # Propagate errors on Lint and k on growth curve
    colnames(growth_se) <- paste("growth", specimen_names, "se", sep = "_") # Add colnames for growth matrix
    model <- cbind(years * 365, growth, growth_se) # create a growth per day table
    colnames(model)[1] <- "days"
    if(!is.na(sheight[1])){ # If shell height is given, trim records
        for(i in 1:length(sheight)){ # subset out the lifespan of the animal based on its measured shell height
            model[which(model[, i + 1] > sheight[i]), c((i + 1), (length(sheight) + i + 1))] <- NA
        }
    }
    if(output == "modeldata"){
        return(model)
    }else if(output == "kvalues"){
        return(data.frame(k = k,
            k_se = k_se)
        )
    }else{
        print("Output string invalid")
    }
}

# Use function to export full von Bertalanffy models
fullvonBmodels <- growthcurve(maxyears = 10,
    t_int = 0.001,
    annual_growth = modelpar$annual_growth_mean,
    annual_growth_se = modelpar$annual_growth_se,
    Linf = modelpar$Linf_mean,
    Linf_se = modelpar$Linf_se,
    specimen_names = modelpar$specimen,
    sheight = NA,
    output = "modeldata")

# Use function to export trimmed von Bertalanffy models
trimvonBmodels <- growthcurve(maxyears = 10,
    t_int = 0.001,
    annual_growth = modelpar$annual_growth_mean,
    annual_growth_se = modelpar$annual_growth_se,
    Linf = modelpar$Linf_mean,
    Linf_se = modelpar$Linf_se,
    specimen_names = modelpar$specimen,
    sheight = modelpar$shell_height,
    output = "modeldata")

vonBk <- growthcurve(maxyears = 10,
    t_int = 0.001,
    annual_growth = modelpar$annual_growth_mean,
    annual_growth_se = modelpar$annual_growth_se,
    Linf = modelpar$Linf_mean,
    Linf_se = modelpar$Linf_se,
    specimen_names = modelpar$specimen,
    sheight = NA,
    output = "kvalues")

# Add results to modelpar data
modelpar$shellage <- trimvonBmodels[apply(trimvonBmodels[, 2:8], 2, which.max), 1]
modelpar$k_est <- vonBk$k
modelpar$k_se <- vonBk$k_se

# ------------------------------------------------------------------------------
# Plot age models for all specimens together

# Create colorscale
Specimencolors <- c(brewer.pal(11, "RdBu")[11:7], "#000000", brewer.pal(11, "RdBu")[5:1])
names(Specimencolors) <- c("TM29", "TM84", "TS85", "TSFRS1", "TSM1", "SQSA1", "PM2_1", "PM2_2", "PM3_1", "PM3_2", "PM4")

# Prepare tide axis
tidelabels <- seq(0, 500, 100)
tidebreaks <- approx(x = Shell4_tide$Tide_inv,
    y = Shell4_tide$Day_inv,
    xout = tidelabels,
    method = "linear",
    rule = 2)$y

Combined_age_model_plot <- ggplot(data = fullvonBmodels) +
# Add full tridacna growth curves
    geom_ribbon(aes(x = days,
        y = growth_TM29,
        ymin = growth_TM29 - growth_TM29_se,
        ymax = growth_TM29 + growth_TM29_se),
        fill = Specimencolors[names(Specimencolors) == "TM29"],
        alpha = 0.2) +
#    geom_ribbon(aes(x = days,
#        y = growth_TM68,
#        ymin = growth_TM68 - growth_TMse,
#        ymax = growth_TM68 + growth_TMse),
#        fill = Specimencolors[names(Specimencolors) == "TM68"],
#        alpha = 0.2) +
    geom_ribbon(aes(x = days,
        y = growth_TM84,
        ymin = growth_TM84 - growth_TM84_se,
        ymax = growth_TM84 + growth_TM84_se),
        fill = Specimencolors[names(Specimencolors) == "TM84"],
        alpha = 0.2) +
    geom_ribbon(aes(x = days,
        y = growth_TS85,
        ymin = growth_TS85 - growth_TS85_se,
        ymax = growth_TS85 + growth_TS85_se),
        fill = Specimencolors[names(Specimencolors) == "TS85"],
        alpha = 0.2) +
    geom_ribbon(aes(x = days,
        y = growth_TSFRS1,
        ymin = growth_TSFRS1 - growth_TSFRS1_se,
        ymax = growth_TSFRS1 + growth_TSFRS1_se),
        fill = Specimencolors[names(Specimencolors) == "TSFRS1"],
        alpha = 0.2) +
    geom_ribbon(aes(x = days,
        y = growth_SQSA1,
        ymin = growth_SQSA1 - growth_SQSA1_se,
        ymax = growth_SQSA1 + growth_SQSA1_se),
        fill = Specimencolors[names(Specimencolors) == "SQSA1"],
        alpha = 0.2) +
# Add growth curves trimmed to maximum growth of the specimens
    geom_line(data = trimvonBmodels, aes(days, growth_TM29),
        size = 1,
        color = Specimencolors[names(Specimencolors) == "TM29"]
    ) +
#    geom_line(data = trimvonBmodels, aes(days, growth_TM68),
#        size = 1,
#        color = Specimencolors[names(Specimencolors) == "TM68"]
#    ) +
    geom_line(data = trimvonBmodels, aes(days, growth_TM84),
        size = 1,
        color = Specimencolors[names(Specimencolors) == "TM84"]
    ) +
    geom_line(data = trimvonBmodels, aes(days, growth_TS85),
        size = 1,
        color = Specimencolors[names(Specimencolors) == "TS85"]
    ) +
    geom_line(data = trimvonBmodels, aes(days, growth_TSFRS1),
        size = 1,
        color = Specimencolors[names(Specimencolors) == "TSFRS1"]
    ) +
    geom_line(data = trimvonBmodels, aes(days, growth_SQSA1),
        size = 1,
        color = Specimencolors[names(Specimencolors) == "SQSA1"]
    ) +
# Add Pectinid lines
    geom_line(aes(x = Day_inv, y = Depth_inv, col = "PM2_1"), data = Shell2_1_aligned, size = 1) +
    geom_line(aes(x = Day_inv, y = Depth_inv, col = "PM2_2"), data = Shell2_2_aligned, size = 1) +
    geom_line(aes(x = Day_inv, y = Depth_inv, col = "PM3_1"), data = Shell3_1_aligned, size = 1) +
    geom_line(aes(x = Day_inv, y = Depth_inv, col = "PM3_2"), data = Shell3_2_aligned, size = 1) +
    geom_line(aes(x = Day_inv, y = Depth_inv, col = "PM4"), data = Shell4_aligned, size = 1, ) +
# Add plot aesthetics
    scale_x_continuous("Day since start of growth",
        breaks = seq(0, 1100, 100),
        limits = c(0, 1100),
        sec.axis = sec_axis(~. / 365,
            name = "Years of growth",
            breaks = seq(0, 3, .5))
    ) +
    scale_y_continuous("Shell height (mm)",
        breaks = seq(0, 60, 5),
        limits = c(0, 60)) +
    scale_colour_manual(values = Specimencolors) +
    theme_bw()