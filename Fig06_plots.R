## plotting calculated parameters from MtTack2 data
## This is the fourth step, following on from 3_Mtrack_analyse.R 
## NOTE: 
## make sure you clear the environment before running

library(dplyr)
library(plyr) # for round_any function
library(tidyverse)
library(ggplot2)
library(readr)
library(patchwork)
library(png)
library(stringr)
library(stats)
library(data.table)

rm(list=ls())

#x values for shaded box annotations (corresponding to light treatments)
Left <- 1
on1 <- 0
end <- 50

#define treatments to correspond to the order in folder
st_speed <- data.frame()
st_HD <- data.frame()
treatment <- list()

#create a list of the files
pattern1 <- "CALCULATIONS.*\\.txt$"
file_paths1 <- fs::dir_ls("data/All_results_dc", regexp = pattern1)  
print(file_paths1)
file_list1 <- list()

for (a in seq_along(file_paths1)) {
  file_list1[[a]] <- read.csv(
    file = file_paths1[[a]], 
    sep="\t", header=TRUE) } 
file_list1 <- set_names(file_list1, file_paths1)

# read summary files in
pattern2 <- "RESULTS.*\\.txt$"
file_paths2 <- fs::dir_ls("data/All_results_dc", regexp = pattern2) 
print(file_paths2)
file_list2 <- list()

for (b in seq_along(file_paths2)) {
  file_list2[[b]] <- read.csv(
    file = file_paths2[[b]], 
    sep="\t", header=TRUE) } 
file_list2 <- set_names(file_list2, file_paths2)


#create a List (called plotvec) to save the plots in:
NUM <- length(file_list1)
plotvec <- vector("list", length=NUM*3) # as there are 2 plots per experiment
length(plotvec)  

combined_data <- data.frame()
combined_swim_dir <- data.frame()
all_all <- data.frame()
all_traj <- data.frame()

# ---- Loop to create figures and final tables ---------------------------------

for (i in 1:length(file_list1)) {
  # read in tables 
  CALC <- read.table(file_paths1[[i]], header=T, sep="\t")
  all <- read.table(file_paths2[[i]], header=T, sep="\t")
  setnames(all, "disp", "V_disp")
  
  #save the speed & v-disp data as you go for averaging later 
  select <- data.frame(CALC$time_s, CALC$speed, CALC$sd_speed, CALC$V_disp, CALC$sd_V_disp, CALC$n_larvae)
  select$batch <- i
  combined_data <- rbind(combined_data, select)
  
  select2 <-  data.frame(all[,1:4], all$speed, all$V_disp)
  select2$batch <- i
  all_all <- rbind(all_all, select2)
  
  select4 <- data.frame(all[,1:8])
  select4$batch <- i
  all_traj <- rbind(all_traj, select4)
  
  #sample size
  nmax <- max(CALC$n_larvae)
  
  # create tidy table of swim directions and vertical positions
  time_s <- CALC$time_s
  subset1 <- subset(cbind(CALC[,9:11], time_s))
  
  #rename still to unmoving so that it falls in a nicer order (alphabetically)
  names(subset1)[3] <- "unmoving"
  
  swim_dir <- subset1 %>%
    pivot_longer(!time_s, names_to = "swim_direction", values_to = "count", names_prefix = "swim_")
  
  # calculate the proportion of larvae swimming in each direction
  totals <- aggregate(swim_dir$count, list(swim_dir$time_s), sum, drop = TRUE)
  colnames(totals) <- c("time_s", "n_total")
  w <- rep(totals$time_s, each=3)
  x <- rep(totals$n_total, each=3)
  swim_dir$group_total <- x
  swim_dir$proportion <- swim_dir$count/swim_dir$group_total
  pmax <- max(swim_dir$proportion)
  
  # Collect swim direction data across batches as you go
  
  select3 <- data.frame(swim_dir)
  select3$batch <- i
  combined_swim_dir <- rbind(combined_swim_dir, select3)
  
  combined_swim_dir$time_s <- as.numeric(combined_swim_dir$time_s)
  
  swim_dir_means <- aggregate(combined_swim_dir$proportion,  list(combined_swim_dir$time_s, combined_swim_dir$swim_direction), mean)
  swim_dir_sum <- aggregate(combined_swim_dir[,3:4],  list(combined_swim_dir$time_s, combined_swim_dir$swim_direction), sum)
  swim_dir_means <- cbind(swim_dir_means, swim_dir_sum)
  colnames(swim_dir_means) <- c("time_s", "swim_direction", "proportion", "time_s2" ,"swim_direction2", "count", "group_total")
  
  
  # ============ PLOTS ============== ---------------------------------
  
  col_table1 <- tibble(
    treatments = c("light", "dark"),
    colour = c("white", "black")  )
  place = c("top", "mid\ntop", "mid\nbottom", "bottom")
  
  col_table2 <- tibble(
    place =c("top", "mid\ntop", "mid\nbottom", "bottom"))
  
  # boxplot for speed data
  
  SPEED <- ggplot(data=all, aes(x=time_s, y=speed, group=time_s)) + 
    annotate("text", x =5, y = 2.1, label = paste0("batch ", i)) + 
    annotate("text", x = end*0.92, y = 2.1, label = paste0("n = ", nmax), fontface = 'italic', size=3) +
    geom_boxplot(fill="#48d1ccff", colour="#25948fff", outlier.size =0.4, outlier.shape = NA) + 
    stat_summary(fun=mean, geom="line", aes(group= 1), color="#cd0989ff") +
    scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") + # DOES THIS DO ANYTHING??
    scale_x_continuous(name="time (s)", breaks = seq(0,end,5), expand=c(0,0), limits=c(0,end) ) +
    scale_y_continuous(name="Swim speed (mm/s)", limits=c(0,2.2), expand=c(0,0)) 
  plotvec[[i]] <- SPEED
  
  
  # boxplot for vertical displacement, if light comes from left/bottom, the sign of VD has been reversed.
  VD <- ggplot(data=all, aes(x=time_s, y=V_disp, group=time_s)) + 
    geom_hline(yintercept=0, colour="grey50", size=1) +
    geom_boxplot(fill="#d9b0f4ff", colour="#ab49e6ff", width=1.5, outlier.size =0.4, outlier.shape = NA) +
    annotate("text", x =5, y = 1.7, label = paste0("batch ", i)) +
    annotate("text", x = end*0.92, y = 1.7, label = paste0("n = ", nmax), fontface = 'italic', size=3) +
 #   stat_summary(fun=mean, geom="line", aes(group=1),  color="#0d14bbff") +
    scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
    scale_x_continuous(name="time (s)", breaks = seq(0,end,5), expand=c(0,0), limits=c(-1,end)  ) +
    scale_y_continuous(name="V displacement (mm/s)", limits=c(-1.8,1.8), expand=c(0,0)) 
  plotvec[[(NUM*2)+i]] <- VD
  
  # lineplot for swimming direction
  DIR <- ggplot(swim_dir, aes(x=time_s, y=proportion, group = swim_direction)) +
    geom_smooth(aes(color = swim_direction), method = loess, span=0.1, se=FALSE ) +
    geom_point(aes(color = swim_direction, alpha =0.5)) +
    annotate("text", x =5, y = 1.1, label = paste0("batch ", i)) +
    annotate("text", x = end*0.92, y = 1.1, label = paste0("n = ", nmax), fontface = 'italic', size=3) +
    scale_colour_manual(values=c("grey20", "#d90360ff", "grey60")) + 
    labs(colour="Trajectory") +
    scale_x_continuous(name="time (s)", breaks = seq(0,end,5), expand=c(0,0), limits=c(-1,end) ) +
    scale_y_continuous(name="Proportion of larvae", expand=c(0,0), breaks=seq(0,1,0.25), limits=c(0,1.2)) 
  plotvec[[(NUM*1)+i]] <- DIR
  
  # # statistics not needed
  
} # end of for loop


## combined data
combined_means <- aggregate(combined_data[,2:5],  list(combined_data$CALC.time_s), mean)
combined_sum <- aggregate(combined_data$CALC.n_larvae,  list(combined_data$CALC.time_s), sum)
combined_means <- cbind(combined_means, combined_sum)
colnames(combined_means) <- c("time_s", "speed", "s_std", "V_disp", "v_std", "time_again", "total_n")

summary <- aggregate(list(combined_means$speed, 
                                  combined_means$s_std, 
                                  combined_means$V_disp, 
                                  combined_means$v_std,
                                  combined_means$time_again,
                                  combined_means$total_n), 
                             by = list(combined_means$time_s), mean)

colnames(summary) <- c("time_s", "speed", "s_std","V_disp", "v_std", "time_again", "mean_n")

maxtime <- max(all_all$time_s)
all_all$time_s <- as.numeric(all_all$time_s)


#make summary graphs from all batches

nmaxALL <- max(combined_means$total_n)
nmaxALL <- round(nmaxALL)
max_y <- max(all_all$all.speed)
max_y2 <- max(all_all$all.V_disp)
min_y2 <- min(all_all$all.V_disp)
top_y <- max_y
#top_y <- max_y*1.1
swimdir_nmax <- max(swim_dir_means$group_total)

SPEED_mean <- ggplot(data=all_all, aes(x=time_s, y=all.speed, group=time_s)) + # label
  annotate("rect", xmin=-10, xmax=0, ymin=0, ymax=2, alpha=0.1, fill="blue") +
  annotate("text", x =-5, y = 1, label = "water \n mixed", size=3) +
  # annotate("text", x =(off1-on1)/2, y = max_y+0.1, label = "all data") +
  #  annotate("text", x = end*0.92, y = max_y+0.1, label = paste0("n = ", nmaxALL), fontface = 'italic', size=3) +
  geom_boxplot(fill="#48d1ccff", colour="#25948fff", width = 1.5, outlier.size =0.4, outlier.shape = NA) +
  stat_summary(fun=mean, geom="line", aes(group= 1), color="#cd0989ff") +
  scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,5), expand=c(0,0), limits=c(-10,end)  ) +
  scale_y_continuous(name="Swim speed (mm/s)",  breaks = seq(0,2,0.25), expand=c(0,0), limits=c(0,2))

# speed mean +SD (dot and SD bar)
SPEED_mean_SD <- ggplot()+
  annotate("rect", xmin=-10, xmax=0, ymin=0, ymax=2, alpha=0.1, fill="blue") +
  annotate("text", x =-5, y = 1, label = "water \n mixed", size=3) +
  geom_errorbar(data=summary, mapping=aes(x=time_again, ymin=pmax(speed-s_std,0), ymax=speed+s_std), width=0.2, color="grey50") +
  geom_point(data=summary, aes(x=time_again, y=speed)) +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,5), expand=c(0,0), limits=c(-10,end)  ) +
  scale_y_continuous(name="Swim speed (mm/s)",  breaks = seq(0,2,0.25), expand=c(0,0), limits=c(0,2))

# vertical displacement
VD_mean <- ggplot(data=all_all, aes(x=time_s, y=all.V_disp, group=time_s)) + # label
  annotate("rect", xmin=-10, xmax=0, ymin=-1.5, ymax=1.75, alpha=0.1, fill="blue") +
  annotate("text", x =-5, y = 0, label = "water \n mixed", size=3) +
  geom_hline(yintercept=0, colour="grey50", size=1) +
  geom_boxplot(fill="#d9b0f4ff", colour="#ab49e6ff", width = 1.5, outlier.size =0.4, outlier.shape = NA) +
  scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,5), expand=c(0,0), limits=c(-10,end)  ) +
  scale_y_continuous(name="Vertical displacement (mm/s)", breaks = seq(-1.5,1.75,.5), limits= c(-1.5,1.75), expand=c(0,0))

# VD mean +SD (dot and SD bar)
VD_mean_SD <- ggplot()+
  annotate("rect", xmin=-10, xmax=0, ymin=-1.5, ymax=1.75, alpha=0.1, fill="blue") +
  annotate("text", x =-5, y = 0, label = "water \n mixed", size=3) +
  geom_hline(yintercept=0, colour="grey50", size=1) +
  geom_errorbar(data=summary, mapping=aes(x=time_again, ymin=V_disp-v_std, ymax=V_disp+v_std), width=0.2, color="grey50") +
  geom_point(data=summary, aes(x=time_again, y=V_disp)) +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,5), expand=c(0,0), limits=c(-10,end)  ) +
  scale_y_continuous(name="Vertical displacement (mm/s)", breaks = seq(-1.5,1.75,.5), limits= c(-1.5,1.75), expand=c(0,0))

# swimming directions
DIR_mean <- ggplot(swim_dir_means, aes(x=time_s, y=proportion, group = swim_direction)) +
  annotate("rect", xmin=-10, xmax=0, ymin=-0.05, ymax=1.05, alpha=0.1, fill="blue") +
  annotate("text", x =-5, y = 0.5, label = "water \n mixed", size=3) +
  geom_smooth(aes(color = swim_direction), method = loess, span=0.1, se=FALSE ) +
  geom_point(aes(color = swim_direction)) +
 # annotate("text", x = end*0.92, y = 1, label = paste0("n = ", swimdir_nmax), fontface = 'italic', size=3) +
  scale_colour_manual(values=c("grey20", "#d90360ff", "grey60")) + 
  labs(colour="Trajectory") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,5), expand=c(0,0), limits=c(-10,end) ) +
  scale_y_continuous(name="Proportion of larvae", breaks = seq(0,1,0.2), limits= c(-0.05,1.05), expand=c(0,0))


# # Save combinations of plots ----------------------------------------------

# FIRST PLOT FOR SWIMMING SPEED
# SECOND PLOT FOR SWIMMING DIRECTIONS

#import a legend saved early and scale
legend2 <- readPNG("pictures/swim_direction_legend_V.png")
legend2 <- cowplot::ggdraw() + cowplot::draw_image(legend2, scale = 1)
space <- readPNG("pictures/blank_space.png")
space <- cowplot::ggdraw() + cowplot::draw_image(space, scale = 1)

#

tiff("pictures/decil_Speed.tiff", width = 4, height = 7.5, units = 'in', res = 400)
  plotvec[[1]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[2]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank()) +
  plotvec[[3]] + theme_classic() + theme(legend.position="none", axis.title.y = element_blank()) +
  plot_layout(ncol=1)
dev.off()


tiff("pictures/decil_Swim_direction.tiff", width = 8, height = 7.5, units = 'in', res = 400)
  plotvec[[4]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  space +
  plotvec[[5]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank() ) +
 legend2 + 
  plotvec[[6]]+ theme_classic() + theme(legend.position="none", axis.title.y = element_blank()) +
  space +
  plot_layout(ncol=2)
dev.off()

tiff("pictures/decil_Vertical_displacement.tiff", width = 4, height = 7.5, units = 'in', res = 400)
  plotvec[[7]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[8]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank()) +
  plotvec[[9]] + theme_classic() + theme(legend.position="none", axis.title.y = element_blank()) +
  plot_layout(ncol=1)
dev.off()

# boxplot & dotplot for mean speed data
tiff("pictures/decil_Swim_speed_means.tiff",  width = 8, height = 3, units = 'in', res = 400)
SPEED_mean + theme_classic() +
SPEED_mean_SD +theme_classic() +
plot_layout(ncol=2)
dev.off()

# boxplot & dotplot for mean V_displacement data
tiff("pictures/decil_V_displacement_means.tiff", width = 8, height = 3, units = 'in', res = 400)
VD_mean + theme_classic() +
VD_mean_SD + theme_classic() +
plot_layout(ncol=2)
dev.off()

#lineplot for combined swimming direction data
tiff("pictures/decil_Swimming_direction_means.tiff", width = 5, height = 3, units = 'in', res = 400)
DIR_mean + theme_classic() + theme(legend.position="none") +
legend2 +
plot_layout(ncol=2, widths = c(3, 1))
dev.off()

# panel A = Extra plot to show trajectories coloured by direction
# red / blue = up / down, alpha could show time interval?

#adjust data and filter to show just one batch
tracking_data <- data.frame(
  time_s =  all_traj$time_s,
  subject = all_traj$larva_ID,
  x_start = all_traj$x_start,
  x_end =   all_traj$x_end,
  y_start = all_traj$y_start,
  y_end =   all_traj$y_end,
  batch =   all_traj$batch
)

tracking_data <- tracking_data %>%
  mutate(
    dx = x_end - x_start,
    dy = y_end - y_start,
    angle = atan2(dy, dx), # Angle in radians (-π to π)
    x_change = ifelse(x_end > x_start, "Up", "Down"),
    alpha_scaled = (time_s - min(time_s)) / (max(time_s) - min(time_s))
  ) %>%

      filter(dy >= -40 & dy <= 40)
      
tracking_data_subset <- tracking_data %>% filter(batch == 1)

 #make plot
traj_fig <- ggplot(tracking_data_subset, aes(x = x_start, y = y_start, 
                          xend = x_end, yend = y_end, 
                          color = x_change,
                          alpha = alpha_scaled)) +
  geom_segment(size = 1, lineend = "round") +  # Draw line segments
  scale_color_manual(
    values = c("Up" = "#d90360ff", "Down" = "grey20"),
  ) +
  annotate("text", x =50, y = 50, label = "up", size=3, colour="#d90360ff", fontface = "bold") + # add in legend colour markers
    annotate("text", x =50, y = 200, label = "down", size=3, colour="grey20", fontface = "bold") +
  coord_fixed(ratio = 1150/300) + 
  coord_flip() +
  scale_x_continuous(name="Y", limits=c(0,1150), expand=c(0,0)) +
  scale_y_continuous(name="X", limits= c(0,300), expand=c(0,0)) +
  theme_classic() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 1))

# TO STOP aspect ratio messing up, save it as a png and read it back in...
 png("pictures/Fig04_panelA.png", width = 30, height = 115, units = 'mm', res = 300)
 traj_fig
 dev.off() 
 
 panelA <- readPNG("pictures/Fig04_panelA.png")
 panelA <- cowplot::ggdraw() + cowplot::draw_image(panelA, scale = 1.3)

# figure for the paper
png("figures/Fig06_deciliated_swimming.png", width = 230, height = 76, units = 'mm', res = 400)
  panelA +
  DIR_mean + theme_classic() + theme(legend.position="none") +
  legend2 +  
  VD_mean_SD + theme_classic() +
  plot_layout(ncol=4, widths = c(2,4,1.2,4)) +
  plot_annotation(tag_levels = list(c("A","B","","C")))
dev.off() 

