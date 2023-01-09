# Script to create artificial Sr/Ca curves and sample them using different combinations of scan speed, run cycle and widths of laser spots

library(ggplot2)
library(data.table)
library(tidyverse)
library(patchwork)

root <- "E:/Dropbox/Research/Manuscripts/[Review] Biogeosciences - Circadian_TE/SI/S12 Smoothing by spot size" #set your root directory
setwd(root)

# virtual sampling of clam shell giving growth parameters and LA-ICP-MS settings
SrCafromgrowth <- function(Lt, # growth in a year in mm
    Linf, # theoretical max height for that species in Red Sea in mm
    sheight, # measured height of shell in mm
    scanspd, # Speed of the LA-ICP-MS transect in um/s
    t_runcycle, # Total time of one run cycle, including dead time in ms
    t_element, # Time used for ICP-MS integration of the element in ms
    spotsize, # Size of the spot in scan direction in um
    startpos, # Position (in mm) of the shell where the transect starts
    endpos # Position (in mm) of the shell where the transect ends
  ){
  
  # Generate shell height model
  k <- (-log(1 - (Lt / Linf))) # calculate von bertalanffy's k from the growth in a year
  time <- seq(0 , 10 * 365 , 1 / (24 * 60)) # ten years of time sequence in days to generate synthetic growth data (minute time-step)
  ht <- Linf * (1 - exp(-k * ((time / 365)))) # classical von Bertalanffy function for height (equation was based on years so time/365)
  
  # Generate Sr/Ca curve
  baseline <- 1.5 # baseline Sr value that sinusoid varies around in mmol/mol
  SrCa <- 0.3 * sin((time) * 2 * pi) + baseline # daily sinusoid, based on 0.6 mmol/mol amplitude noted by Sano et al (2012)
  
  # Aggregate original ("true") dataset
  true_dataset <- data.frame(
    time = time,
    height = ht,
    SrCa = SrCa
  )

  # Simulate LA-ICP-MS sampling
  # The laser sweeps over the entire shell height with a velocity of <scanspd> and samples average Sr/Ca ratio during 10 ms of the 109 ms sweep time.
  t_start <- seq(0, max(ht) * 1000 / scanspd, t_runcycle / 1000) # Find starting times (in ms) of Sr/Ca measurements
  t_end <- seq(t_element / 1000, max(ht) * 1000 / scanspd, t_runcycle / 1000) # Find end times (in ms) of Sr/Ca measurements
  h_start <- t_start * scanspd - 0.5 * spotsize # Find start position (in um) of Sr/Ca measurement
  h_end <- t_end * scanspd + 0.5 * spotsize # Find end position (in um) of Sr/Ca measurement

  # Truncate h_start and h_end to prevent overshooting of the laser beyond the shell
  h_start[which(h_start < 0)] <- 0
  h_end[which(h_end > max(ht) * 1000)] <- max(ht) * 1000

  startposh <- which.min(abs(h_start / 1000 - startpos))
  endposh <- which.min(abs(h_end / 1000 - endpos))

  # Create empty vectors to store data
  SrCa_sampled <- rep(NA, endposh - startposh)
  h_mean <- rep(NA, endposh - startposh)

  # Loop through the measurements and find the average Sr/Ca value belonging to the sampled interval
  for(i in startposh:endposh){
    # Find positions of first and last datapoint in the virtual Sr/Ca record
    pos_start <- which.min(abs(ht * 1000 - h_start[i]))
    pos_end <- which.min(abs(ht * 1000 - h_end[i]))

    SrCa_sampled[i - startposh] <- mean(SrCa[pos_start:pos_end]) # Calculate mean Sr/Ca value of sampled interval
    h_mean[i - startposh] <- mean(ht[pos_start:pos_end]) # Calculate mean shell height for sampled interval
    cat(paste("Simulating LA-ICP-MS datapoint ",i - startposh," of ", endposh - startposh),"\r")
  }

  # Aggregate data and export
  sampled_dataset <- data.frame(
    t_start = t_start[startposh:(endposh - 1)],
    t_end = t_end[startposh:(endposh - 1)],
    h_start = h_start[startposh:(endposh - 1)],
    h_end = h_end[startposh:(endposh - 1)],
    h_mean = h_mean,
    SrCa_sampled = SrCa_sampled
  )

  return(list(true_dataset, sampled_dataset))
}

s84 <- SrCafromgrowth(
  Lt = 26.6,
  Linf = 374,
  sheight = 32,
  scanspd = 4,
  t_runcycle = 109,
  t_element = 10,
  spotsize = 20,
  startpos = 2,
  endpos = 3
) # Plugging in our growth values for shell 84, Linf from squamosa, length of shell 84 at death

SQSA1 <- SrCafromgrowth(
  Lt = 14.9,
  Linf = 190,
  sheight = 31.5,
  scanspd = 4,
  t_runcycle = 109,
  t_element = 10,
  spotsize = 20,
  startpos = 2,
  endpos = 3
) #plugging in our growth values for shell SQSA, Linf from squamosina, length of SQSA at death

# plot smoothing in TS84
TS84_true <- s84[[1]]
TS84_sampled <- s84[[2]]

TS84_plot <- ggplot(TS84_sampled) +
  geom_line(aes(h_mean,
    SrCa_sampled,
    col = "20 micron spot size"),
  ) +
  geom_line(data = TS84_true,
    aes(height,
      SrCa,
      col = "true data"),
  ) +
  xlim(c(2, 3)) +
  ylab("Sr/Ca (mmol/mol)") +
  xlab("Length (mm)") +
  theme_bw(base_size = 15) +
  ggtitle("Smoothing of the record in\nTridacna maxima 84")

# plot smoothing in SQSA1
SQSA1_true <- SQSA1[[1]]
SQSA1_sampled <- SQSA1[[2]]

SQSA_plot <- ggplot(SQSA1_sampled) +
  geom_line(aes(h_mean,
    SrCa_sampled,
    col = "20 micron spot size"),
  ) +
  geom_line(data = SQSA1_true,
    aes(height,
      SrCa,
      col = "true data"),
  ) +
  xlim(c(2, 3)) +
  ylab("Sr/Ca (mmol/mol)") +
  xlab("Length (mm)") +
  theme_bw(base_size = 15) +
  ggtitle("Smoothing of the record in\nTridacna squamosina SQSA1")

png("Srmodeled.png",
  width = 8,
  height = 6,
  units = 'in',
  res = 500,
  type = "cairo")
TS84_plot / SQSA_plot +
  plot_layout(guides = 'collect')
dev.off()
  
