
library(ggplot2)
library(Hmisc)
library(dplyr) # easier data wrangling
library(viridis) # colour blind friendly palette, works in B&W also
library(tidyr)
library(tidyverse)
library(patchwork)
library(png)


rm(list=ls())
PIVdata3 <- read.table("data/PIV_data/t3_PIV.txt", sep="\t", header=T)
PIVdata4 <- read.table("data/PIV_data/t4_PIV.txt", sep="\t", header=T)
PIVdata5 <- read.table("data/PIV_data/t5_PIV.txt", sep="\t", header=T)
PIVdata8 <- read.table("data/PIV_data/t8_PIV.txt", sep="\t", header=T)
PIVdata9 <- read.table("data/PIV_data/t9_PIV.txt", sep="\t", header=T)
PIVdata10 <- read.table("data/PIV_data/t10_PIV.txt", sep="\t", header=T)

#make a dataframe with all data in it
PIVdata_all <- rbind(PIVdata3, PIVdata4, PIVdata5, PIVdata8, PIVdata9, PIVdata10)# test if this works once i have some more data
#simplify dataframe by dropping unneeded columns
keep <- c("larva_ID", "time_s","velocity_mms")
PIVdata_all = PIVdata_all[keep]

## de-noise and bin data by averaging flow speed across each second
 #round time UP to nearest whole second (e.g 1.1 = 2 and 1.6 = 2) in new column
PIVdata_all$timebin <- ceiling(PIVdata_all$time_s)

# average (mean + SD) across each second time bin
PIVdata_all_binned <- PIVdata_all %>%
  group_by(timebin, larva_ID) %>%
 # summarise(velocity = mean(velocity_mms)) %>%
 summarise(velocity = mean(velocity_mms), sd = sd(velocity_mms)) %>% # not sure why I wanted it to do a SD?
  as.data.frame()
colnames(PIVdata_all_binned)[1] <- "time_s" # rename column

#calculate mean velocity and SD for all 6 larvae
n_lv <- length(unique(PIVdata_all_binned$larva_ID)) # calculate number of larvae
data_means<- summarise(PIVdata_all_binned, mean_velocity=mean(velocity), sd_velocity=sd(velocity)) 

### ======= PLOT RAW DATA FOR EACH LARVA ======== ###

ID_list <- c(3,4,5,8,9,10)
pivplot <- vector("list", length=length(ID_list)) # collects figures for each larva

for (i in ID_list) {
  
exp_n <- paste0("PIVdata", i)
exp <- get(exp_n)

piv_plot <- ggplot() +
  annotate("rect", xmin=90, xmax=180, ymin=0, ymax=1.3, alpha=0.2, fill="black") +
  annotate("rect", xmin=270, xmax=360, ymin=0, ymax=1.3, alpha=0.2, fill="black") +
  geom_line(data = exp, aes(time_s, y=velocity_mms), colour="black", size=0.5) + 
  scale_x_continuous(name="time (s)", breaks = seq(0,360,45), limits=c(0,368), expand = c(0,0)) +
  scale_y_continuous(name="mean flow velocity (mm/s)", expand = c(0,0)) 
pivplot[[i]] <- piv_plot

}

#plot the PIV data for each larva in a six-panel figure
png("figures/PIV_plots.png", width = 10, height = 7.5, units = 'in', res = 400)
  pivplot[[3]] + theme_classic() + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  pivplot[[4]] + theme_classic() + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  pivplot[[5]] + theme_classic() + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  pivplot[[8]] + theme_classic() + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  pivplot[[9]] + theme_classic() + theme(axis.title.y = element_blank()) +
  pivplot[[10]] + theme_classic() + theme(axis.title.y = element_blank()) +
  plot_layout(ncol=2)
dev.off()

### ======= PLOT BINNED DATA FOR ALL LARVAE ======== ###

blue_colours <- c("#3190C5", "#60B7DE", "#87CEEB", "#B0E1F9", "#5CD1FA","#01BFFF") 
stills <- readPNG("pictures/PIV_grab_start_finish.png")
stills <- cowplot::ggdraw() + cowplot::draw_image(stills, scale = 1)

Fig_all <- ggplot()+
  annotate("rect", xmin=90, xmax=180, ymin=0, ymax=1.2, alpha=0.2, fill="black") +
  annotate("rect", xmin=270, xmax=360, ymin=0, ymax=1.2, alpha=0.2, fill="black") +
  annotate("text", x=45, y = 1.15, label = "white light", size=3) +
  annotate("text", x=135, y = 1.15, label = "dark", size=3) +
  annotate("text", x=225, y = 1.15, label = "white light", size=3) +
  annotate("text", x=315, y = 1.15, label = "dark", size=3) +
  geom_line(data=PIVdata_all_binned, aes(time_s, y=velocity, colour = larva_ID), size=1, alpha = 0.8) + 
  # scale_color_manual(values = blue_colours)
  scale_x_continuous(name="time (s)", breaks = seq(0,360,45), limits=c(0,368), expand = c(0,0)) +
  scale_y_continuous(name="mean flow velocity (mm/s)", expand = c(0,0), limits=c(0,1.2), breaks=seq(0,1.2,0.2) )  # could also plot in µm/s ? 

# PLOT FIGURE 

png("figures/Fig02_PIVdata_ALL.png", width = 220, height = 76, units = 'mm', res = 400)
  stills +
  Fig_all + theme_classic() + theme(legend.position = "none") +
    plot_layout(ncol=2, widths = c(1.5,1)) + 
  plot_annotation(tag_levels = list(c("A","B")))

dev.off()

# ===== statistics ======= #



