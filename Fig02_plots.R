## plotting calculated parameters from MtTack2 data
## This is the fourth step, following on from 3_Mtrack_analyse.R 
## NOTE: 
## make sure you clear the environment before running

library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(patchwork)
library(png)
library(stringr)
library(stats)
library(ggpubr) 
library(lme4)
library(data.table)
library(nortest)
library(purrr)

rm(list=ls())

#x values for shaded box annotations (corresponding to light treatments)
Left <- 1
on1 <- 0
off1 <- 90
on2 <- 180
off2 <- 270
end <-360

GLOB_sp_max = 2.23
GLOB_HD_max = 2.23
GLOB_HD_min = -10.5

#define treatments to correspond to the order in folder
st_speed <- data.frame()
st_HD <- data.frame()
treatment <- list()
sp_data_combined <- data.frame()
hd_data_combined <- data.frame()

#create a list of the files
pattern1 <- "CALCULATIONS.*\\.txt$"
file_paths1 <- fs::dir_ls("data/All_results_H", regexp = pattern1) 
print(file_paths1)
file_list1 <- list()

for (a in seq_along(file_paths1)) {
  file_list1[[a]] <- read.csv(
    file = file_paths1[[a]], 
    sep="\t", header=TRUE) } 
file_list1 <- set_names(file_list1, file_paths1)

# read summary files in
pattern2 <- "RESULTS.*\\.txt$"
file_paths2 <- fs::dir_ls("data/All_results_H", regexp = pattern2) 
print(file_paths2)
file_list2 <- list()

for (b in seq_along(file_paths2)) {
  file_list2[[b]] <- read.csv(
    file = file_paths2[[b]], 
    sep="\t", header=TRUE) } 
file_list2 <- set_names(file_list2, file_paths2)

#create a List (called plotvec) to save the plots in:
NUM <- length(file_list1)
plotvec <- vector("list", length=NUM*2) # as there are 2 plots per experiment
length(plotvec)  

combined_data <- data.frame()
combined_swim_dir <- data.frame()
all_all <- data.frame()
comb_swim_dir_counts <- data.frame()
all_traj <- data.frame()

# ---- Loop to create figures and final tables ---------------------------------

for (i in 1:length(file_list1)) {
  # read in tables 
  CALC <- read.table(file_paths1[[i]], header=T, sep="\t")
  all <- read.table(file_paths2[[i]], header=T, sep="\t")
  
  #save the speed & h-disp data as you go for averaging later 
  select <- data.frame(CALC$time_s, CALC$speed, CALC$sd_speed, CALC$H_disp, CALC$sd_H_disp, CALC$n_larvae)
  select$batch <- i
  combined_data <- rbind(combined_data, select)
  
  
  select2 <-  data.frame(all[,1:4], all$speed, all$H_disp)
  select2$batch <- i
  all_all <- rbind(all_all, select2)
  
  select5 <- data.frame(all[,1:8])
  select5$batch <- i
  all_traj <- rbind(all_traj, select5)
  
  #sample size
  nmax <- max(CALC$n_larvae)
  
  # create tidy table of swim directions and horizontal positions
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
  
  #save H-disp left-right converted final tables here (note the calculation file was already transformed)
  write.table(all, file = paste0("data/All_results_H/R_transf_0", i, ".txt"), sep ="\t", row.names = FALSE, quote = FALSE)
  
  # Collect swim direction data across batches as you go
  select3 <- data.frame(swim_dir)
  select3$batch <- i
  combined_swim_dir <- rbind(combined_swim_dir, select3)
  
  # create another combined dataframe of the count data that can be used in stats models later
  select4 <- data.frame(CALC$time_s, CALC$treatment, CALC$n_larvae, CALC$away, CALC$toward, CALC$still)
  select4$batch <- i
  comb_swim_dir_counts <- rbind(comb_swim_dir_counts, select4)

  # Define the set of multiples, Function to find the nearest multiple
  multiples <- seq(5, 360, by = 10)  # This generates multiples: 5, 15, 25, 35, ...
  find_nearest_multiple <- function(x, multiples) {
    multiples[which.min(abs(multiples - x))]
  }
  
  # Apply the function to round 'time_s' values to the nearest multiple
  combined_swim_dir <- combined_swim_dir %>%
    mutate(time_s = map_dbl(time_s, find_nearest_multiple, multiples = multiples))
  
  combined_swim_dir$time_s <- as.numeric(combined_swim_dir$time_s)
  
  swim_dir_means <- aggregate(combined_swim_dir$proportion,  list(combined_swim_dir$time_s, combined_swim_dir$swim_direction), mean)
  swim_dir_sum <- aggregate(combined_swim_dir[,3:4],  list(combined_swim_dir$time_s, combined_swim_dir$swim_direction), sum)
  swim_dir_means <- cbind(swim_dir_means, swim_dir_sum)
  colnames(swim_dir_means) <- c("time_s", "swim_direction", "proportion", "time_s2" ,"swim_direction2", "count", "group_total")
  
  # ============ PLOTS ============== ---------------------------------
  
  col_table1 <- tibble(
    treatments = c("light", "dark"),
    colour = c("white", "black")  )
  
  place =c("furthest", "mid\nfar", "mid\nclose", "closest") 
  
  col_table2 <- tibble(
    place =c("furthest", "mid\nfar", "mid\nclose", "closest"))
  
  # first time you run, find y values for shaded box and text annotations (printed) then input them below
  #speed
  sp_box_max <- max(all$speed)*1.1
  if (sp_box_max > GLOB_sp_max) {
    GLOB_sp_max <- sp_box_max
  }
  #H displacement
  HD_min <- min(all$H_disp)*1.1
  HD_max <- max(all$H_disp)*1.1
  if (HD_max > GLOB_HD_max) {
    GLOB_HD_max <-HD_max
  }
  if (HD_min < GLOB_HD_min) {
    GLOB_HD_min <-HD_min
  }
  
  Hd_min_tx <- GLOB_HD_min/1.1
  Hd_max_tx <- GLOB_HD_max/1.1
  sp_tx <- GLOB_sp_max/1.1
  
  #OPTIONAL: change time_s values to combine values for wider box plots
  
  all_10s <- all
  
  all_10s <- all_10s %>%
    mutate(time_s = map_dbl(time_s, find_nearest_multiple, multiples = multiples))
  
  
  # boxplot for speed data
  
  SPEED <- ggplot(data=all_10s, aes(x=time_s, y=speed, group=time_s)) + 
    annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=GLOB_sp_max, alpha=0.2, fill="black") +
    annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=GLOB_sp_max, alpha=0.2, fill="black") +
    annotate("text", x =(off1-on1)/2, y = sp_tx, label = paste0("batch ", i)) + 
    annotate("text", x = end*0.92, y = sp_tx, label = paste0("n = ", nmax), fontface = 'italic', size=3) +
    geom_boxplot(fill="#48d1ccff", colour="#25948fff", width=7, outlier.size =0.4, outlier.shape = NA) +#, outlier.colour = "grey30") +
    stat_summary(fun=mean, geom="line", aes(group= 1), color="#cd0989ff") +
    scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
    scale_x_continuous(name="time (s)", breaks = seq(0,end,30), expand=c(0,0), limits=c(-1,end)  ) +
    scale_y_continuous(name="Swim speed (mm/s)", limits=c(0,GLOB_sp_max), expand=c(0,0)) 
  
  plotvec[[i]] <- SPEED
  
  # boxplot for horizontal displacement, if light comes from left/bottom, the sign of HD has been reversed.
  HD <- ggplot(data=all_10s, aes(x=time_s, y=H_disp, group=time_s)) + 
    geom_hline(yintercept=0, colour="grey50", size=1) +
    annotate("rect", xmin=off1, xmax=on2, ymin=GLOB_HD_min, ymax=GLOB_HD_max, alpha=0.2, fill="black") +
    annotate("rect", xmin=off2, xmax=end, ymin=GLOB_HD_min, ymax=GLOB_HD_max, alpha=0.2, fill="black") +
    geom_boxplot(fill="#d9b0f4ff", colour="#ab49e6ff", width=7, outlier.size =0.4, outlier.shape = NA) +#, outlier.colour = "grey30") +
    annotate("text", x =(off1-on1)/2, y = Hd_max_tx, label = paste0("batch ", i)) +
    annotate("text", x = end*0.92, y = Hd_max_tx, label = paste0("n = ", nmax), fontface = 'italic', size=3) +
    stat_summary(fun=mean, geom="line", aes(group=1),  color="#0d14bbff") +
    scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
    scale_x_continuous(name="time (s)", breaks = seq(0,end,30), expand=c(0,0), limits=c(-1,end)  ) +
    scale_y_continuous(name="H displacement (mm)", limits=c(GLOB_HD_min,GLOB_HD_max), expand=c(0,0)) 
  plotvec[[(NUM*2)+i]] <- HD
  
  # lineplot for swimming direction
  DIR <- ggplot(swim_dir, aes(x=time_s, y=proportion, group = swim_direction)) +
    annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=1.1, alpha=0.2, fill="black") +
    annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=1.1, alpha=0.2, fill="black") +
    geom_smooth(aes(color = swim_direction), method = loess, span=0.1, se=FALSE ) +
    geom_point(aes(color = swim_direction)) +
    annotate("text", x =(off1-on1)/2, y = 1, label = paste0("batch ", i)) +
    annotate("text", x = end*0.92, y = 1, label = paste0("n = ", nmax), fontface = 'italic', size=3) +
    scale_colour_manual(values=c("grey20", "#d90360ff", "grey60")) + 
    labs(colour="Swimming\ndirection") +
    scale_x_continuous(name="time (s)", breaks = seq(0,end,30), expand=c(0,0), limits=c(-1,end) ) +
    scale_y_continuous(name="Proportion of larvae",  expand=c(0,0)) 
  plotvec[[(NUM*1)+i]] <- DIR
  
  ##### STATISTICAL TESTS #######
  
  # remove rows except those in last 45 sec of each time period  
  all_2ndhalf <- all %>%
    filter((time_s >= 45 & time_s <= 90) |
             (time_s >= 135 & time_s <= 180) | 
             (time_s >= 225 & time_s <= 270) | 
             (time_s >= 315 & time_s <= 360))
  
  # speed statistics
  sp_output <- aggregate(all_2ndhalf$speed, list(all_2ndhalf$treatment), mean) # mean values for all
  
  sp_dark <- subset(all_2ndhalf, treatment=="dark", select = speed)
  sp_dark$treatment <- "dark"
  sp_lightON <- subset(all_2ndhalf, treatment=="light on", select = speed)
  sp_lightON$treatment <- "light on"
  #combine data into one frame
  sp_data <- rbind(sp_dark, sp_lightON)
  sp_data$batch <- i
  
  # collect for each batch
  sp_data_combined <- rbind(sp_data_combined, sp_data)
  
  #test for normality (if p<0.05, data is NOT normal and non-parametric test needed)
  shapiro.test(sp_dark$speed) # not normal
  ggqqplot(sp_dark$speed) 
  shapiro.test(sp_lightON$speed) # not normal
  ggqqplot(sp_lightON$speed) 
  
  # data were not normal so we used a Mann-Whitney-Wilcoxon Test:
  sp_Ttest <- wilcox.test(speed ~ treatment, data = sp_data)
  print(sp_Ttest)
  
  # alterntive t-test for normal data (not used)
  #  sp_Ttest <- t.test(sp_dark[,1], sp_lightON[,1])
  
  sp_prob <- unlist(sp_Ttest[[3]])
  
  if (sp_prob < 0.001) {
    sp_sig <- "***"
  }  else if (between(sp_prob, 0.001, 0.01)) {
    sp_sig <- "**"
  }  else if (between(sp_prob, 0.01, 0.05)) {
    sp_sig <- "*"
  }  else if (sp_prob > 0.05) {
    sp_sig <- "-"
  }
  
  #if using Mann-Whitney test  
  sp_stats <- c(i , median(sp_dark$speed), median(sp_lightON$speed), unlist(sp_Ttest[[1]]), sp_prob, sp_sig)
  
  #if using t-test
  #  sp_stats <- c(sp_output[1,2], sp_output[2,2], unlist(sp_Ttest[[1]]), unlist(sp_Ttest[[2]]), sp_prob, sp_sig)
  
  # collect results for each experiment
  st_speed <- rbind(st_speed, sp_stats)
  
  # Horizontal displacement statistics
  hd_dark <- subset(all_2ndhalf, treatment=="dark", select = H_disp) # All dark   
  hd_dark$treatment <- "dark"
  hd_lightON <- subset(all_2ndhalf, treatment=="light on", select = H_disp) # SELECTS ALL LIGHT ON ####   
  hd_lightON$treatment <- "light on"
  
  #combine data into one frame
  hd_data <- rbind(hd_dark, hd_lightON)
  hd_data$batch <- i
  
  # collect for each batch
  hd_data_combined <-rbind(hd_data_combined, hd_data)
  
  #test for normality (if p<0.05, data is NOT normal and non-parametric test needed)
  shapiro.test(hd_dark$H_disp) # not normal
  ggqqplot(hd_dark$H_disp) 
  shapiro.test(hd_lightON$H_disp) # this one actually IS normally distributed!
  ggqqplot(hd_lightON$H_disp)
  
  # data were not normal so we used a Mann-Whitney-Wilcoxon Test:
  # Add an indicator variable for the group
  hd_dark$group <- "dark"
  hd_lightON$group <- "light"
  
  # Combine the dataframes
  hdstats_df <- rbind(hd_lightON, hd_dark)
  
  # Perform the Mann-Whitney-Wilcoxon test
  hd_Ttest <- wilcox.test(H_disp ~ group, data = hdstats_df)
  print(hd_Ttest)
  
  # p value is large and so the data was found to be not sig. different (there is a large difference in variance, but not median)
  
  # alterntive t-test for normal data (not used)
  # hd_Ttest <- t.test(hd_dark[,1], hd_lightON[,1])
  hd_prob <- unlist(hd_Ttest[[3]])
  
  if (hd_prob < 0.001) {
    hd_sig <- "***"
  }  else if (between(hd_prob, 0.001, 0.01)) {
    hd_sig <- "**"
  }  else if (between(hd_prob, 0.01, 0.05)) {
    hd_sig <- "*"
  }  else if (hd_prob > 0.05) {
    hd_sig <- "-"
  }
  
  hd_stats <- c(i, median(hd_dark$H_disp), median(hd_lightON$H_disp), unlist(hd_Ttest[[1]]), hd_prob, hd_sig)
  # for t-test
  #hd_stats <- c(mean(hd_dark$H_disp), mean(hd_lightON$H_disp), unlist(hd_Ttest[[1]]), unlist(hd_Ttest[[2]]), hd_prob, hd_sig)
  st_HD <- rbind(st_HD, hd_stats)
  
} # end of for loop




#stats on all replicate experiments pooled together
#split into dark and light again
sp_comb_dark <- sp_data_combined[sp_data_combined$treatment == "dark", ]
sp_comb_light <- sp_data_combined[sp_data_combined$treatment == "light on", ]
hd_comb_dark <- hd_data_combined[hd_data_combined$treatment == "dark", ]
hd_comb_light <- hd_data_combined[hd_data_combined$treatment == "light on", ]

#test for normality (if p is very small, p<0.05, data are NOT normal and non-parametric test needed)
shapiro.test(sp_comb_dark$speed)   # not normal
shapiro.test(sp_comb_light$speed)  # not normal
shapiro.test(hd_comb_dark$H_disp)  # not normal
shapiro.test(hd_comb_light$H_disp) # not normal

sp_wilcoxon_pooled <- wilcox.test(speed ~ treatment, data = sp_data_combined)
print(sp_wilcoxon_pooled)
sp_stats_pooled <- c("pooled", median(sp_comb_dark$speed), median(sp_comb_light$speed), unlist(sp_wilcoxon_pooled[[1]]), unlist(sp_wilcoxon_pooled[[3]], "***"))
# ^ overall speed in last 45 seconds of each period is sig different between light treatments

hd_wilcoxon_pooled <- wilcox.test(H_disp ~ treatment, data = hd_data_combined)
print(hd_wilcoxon_pooled)
hd_stats_pooled <- c("pooled", median(hd_comb_dark$H_disp), median(hd_comb_light$H_disp), unlist(hd_wilcoxon_pooled[[1]]), unlist(hd_wilcoxon_pooled[[3]]), "-")
# ^ overall H_DISP is not sig different between light treatments, batch 2 did have a sig difference though

# add pooled result to the individual batch test results
st_speed <- rbind(st_speed, sp_stats_pooled)
st_HD <- rbind(st_HD, hd_stats_pooled)

#save stats tables
colnames(st_speed) <- c("batch", "dark", "light_on", "W-value", "p-value", "signif.") # wilcoxon
#colnames(st_speed) <- c("dark", "light_on", "T-test", "d.f.", "p-value", "signif.") # t-test
#setwd(wdir)
write.table(st_speed, file = "data/All_results_H/speed_stats.txt", sep ="\t", row.names = FALSE, quote = FALSE)
print(st_speed)

colnames(st_HD) <- c("batch", "dark", "light_on", "W-value", "p-value", "signif.") # wilcoxon
#colnames(st_HD) <- c("dark", "light_on", "T-test", "d.f.", "p-value", "signif.") # t-test
write.table(st_HD, file = "data/All_results_H/H_disp_stats.txt", sep ="\t", row.names = FALSE, quote = FALSE)
print(st_HD)

# not using this table anymore
colnames(comb_swim_dir_counts) <- c("time_s", "treatment", "n_larvae", "away", "toward", "still", "batch")

print(paste0("number of plots = ",length(plotvec)))
print(paste0("GLOB_sp_max = ",GLOB_sp_max))
print(paste0("GLOB_HD_max = ",GLOB_HD_max))
print(paste0("GLOB_HD_min = ",GLOB_HD_min))
print("now change the above parameters in the script before running again")

## combined data, average into 10 second intervals
combined_means <- aggregate(combined_data[,2:5],  list(combined_data$CALC.time_s), mean)
combined_sum <- aggregate(combined_data$CALC.n_larvae,  list(combined_data$CALC.time_s), sum)
combined_means <- cbind(combined_means, combined_sum)
colnames(combined_means) <- c("time, s", "speed", "std", "HD", "HD_std", "time_again", "total_n")

#calculate the mean values across each 10 second period
combined_means_10s_int <- combined_means %>% as_tibble() %>% 
  group_by(ceiling(1:n() / 2)) %>% 
  summarise("time, s" = paste("time, s", collapse = "_"),
            speed = mean(speed),
            std = mean(std),
            hd = mean(HD),
            hd_std = mean(HD_std),
            time_again = mean(time_again), 
            total_n = mean(total_n)) %>% 
  select(-1)

all_all_10s_int <- all_all
maxtime <- max(all_all_10s_int$time_s)

#change time_s values to combine values for wider box plots
all_all_10s_int <- all_all_10s_int %>%
  mutate(time_s = map_dbl(time_s, find_nearest_multiple, multiples = multiples))

all_all_10s_int$time_s <- as.numeric(all_all_10s_int$time_s)

#make summary graphs from all batches
nmaxALL <- max(combined_means_10s_int$total_n)
nmaxALL <- round(nmaxALL)
max_y <- max(all_all_10s_int$all.speed)
max_y2 <- max(all_all_10s_int$all.H_disp)
min_y2 <- min(all_all_10s_int$all.H_disp)
top_y <- max_y
#top_y <- max_y*1.1
swimdir_nmax <- max(swim_dir_means$group_total)

#speed in boxplots with mean points overlaid
SPEED_mean <- ggplot(data=all_all_10s_int, aes(x=time_s, y=all.speed, group=time_s)) + # label
  annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=top_y, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=top_y, alpha=0.2, fill="black") +
  annotate("text", x=45, y = max_y-0.1, label = "white light", size=3) +
  annotate("text", x=135, y = max_y-0.1, label = "dark", size=3) +
  annotate("text", x=225, y = max_y-0.1, label = "white light", size=3) +
  annotate("text", x=315, y = max_y-0.1, label = "dark", size=3) +
  geom_boxplot(fill="#A6CEE3", colour="#1F78B4", width=7, outlier.size =0.4, outlier.shape = NA) +#, outlier.colour = "grey30") +
  stat_summary(fun=mean, geom="point", aes(group= 1), color="#E31A1C") +
  scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end)  ) +
  scale_y_continuous(name="Swim speed (mm/s)", expand=c(0,0))

# speed mean +SE (dot and SD bar)
SPEED_mean_SD <- ggplot()+
  annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=1.2, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=1.2, alpha=0.2, fill="black") +
  annotate("text", x=45, y = 1.15, label = "white light", size=3) +
  annotate("text", x=135, y = 1.15, label = "dark", size=3) +
  annotate("text", x=225, y = 1.15, label = "white light", size=3) +
  annotate("text", x=315, y = 1.15, label = "dark", size=3) +
  geom_errorbar(data=combined_means_10s_int, mapping=aes(x=time_again, ymin=pmax(speed-std,0), ymax=speed+std), width=0.2, color="grey50") +
  geom_point(data=combined_means_10s_int, aes(x=time_again, y=speed)) +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end)  ) +
  scale_y_continuous(name="Swim speed (mm/s)", expand=c(0,0), breaks=seq(0,1.2,0.2), limits=c(0,1.2))

#boxplot HORIZONTAL DISPLACEMENT
HD_mean <- ggplot(data=all_all_10s_int, aes(x=time_s, y=all.H_disp, group=time_s)) + # label
  geom_hline(yintercept=0, colour="grey50", size=1) +
  annotate("rect", xmin=off1, xmax=on2, ymin=min_y2, ymax=max_y2, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=min_y2, ymax=max_y2, alpha=0.2, fill="black") +
  geom_boxplot(fill="#d9b0f4ff", colour="#ab49e6ff", width=7, outlier.size =0.4, outlier.shape = NA) +#, outlier.colour = "grey30") +
  stat_summary(fun=mean, geom="line", aes(group= 1), color="#0d14bbff") +
  scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,30), expand=c(0,0), limits=c(-1,end)  ) +
  scale_y_continuous(name="Horizontal displacement (mm/s)", expand=c(0,0))

#dotplot HORIZONTAL DISPLACEMENT
HD_mean_SD <- ggplot()+
  geom_hline(yintercept=0, colour="grey50", size=1) +
  annotate("rect", xmin=off1, xmax=on2, ymin=-4, ymax=4, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=-4, ymax=4, alpha=0.2, fill="black") +
  annotate("text", x =320, y = 3.7, label = "toward light", size=3) +
  annotate("text", x =320, y = -3.7, label = "away from light", size=3) +
  geom_errorbar(data=combined_means_10s_int, mapping=aes(x=time_again, ymin=hd-hd_std, ymax=hd+hd_std), width=0.2, color="#0d14bbff", alpha=0.6) +
  geom_point(data=combined_means_10s_int, aes(x=time_again, y=hd), colour="#0d14bbff") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end)  ) +
  scale_y_continuous(name="Horizontal displacement (mm/s)", expand=c(0,0), breaks=seq(-4,4,1), limits=c(-4,4))

# points +best fit line for swimming direction
DIR_mean <- ggplot(swim_dir_means, aes(x=time_s, y=proportion, group = swim_direction)) +
  annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=1.0, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=1.0, alpha=0.2, fill="black") +
  annotate("text", x=45, y = 0.95, label = "white light", size=3) +
  annotate("text", x=135, y = 0.95, label = "dark", size=3) +
  annotate("text", x=225, y = 0.95, label = "white light", size=3) +
  annotate("text", x=315, y = 0.95, label = "dark", size=3) +
  geom_smooth(aes(color = swim_direction), method = loess, span=0.1, se=FALSE ) +
  geom_point(aes(color = swim_direction)) +
  #  annotate("text", x = end*0.92, y = 1, label = paste0("n = ", swimdir_nmax), fontface = 'italic', size=3) +
  scale_colour_manual(values=c("grey20", "#d90360ff", "grey60")) + 
  labs(colour="Swimming\ndirection") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end) ) +
  scale_y_continuous(name="Proportion of larvae",  expand=c(0,0)) 

## Trajectories plots for 1st dark and following light phases

#adjust data and filter to show just one batch

tracking_data <- read.csv("data/All_results_H/H_trajectory_data.txt", sep="\t", header = T)

chosenbatch <- "R02"
tracking_data_subset <- tracking_data %>% filter(batch == chosenbatch)
tracking_data_subset$x_change = ifelse(x_end > x_start, "Toward", "Away")

#create two data subsets, one for first light off period and the second for the light coming back on
tracking_data_subset_dark <- tracking_data %>% filter(batch == chosenbatch, 
                                                      time_s >= 91 & time_s <= 180 )

tracking_data_subset_light <- tracking_data %>% filter(batch == chosenbatch, 
                                                      time_s >= 181 & time_s <= 270 )

ggplot(
 # geom_path(size = 1) +
  geom_point(size = 1) +
  scale_color_gradient(low = "plum1", high = "violetred4") +
  ) +
  theme_classic()


#make plot 1
traj_fig_dark <- ggplot(tracking_data_subset_dark, aes(x = X, y = Y, group = larva_ID, colour = act_time)) + 
  geom_point(size = 0.7, alpha=0.8) +
  scale_color_gradient(low = "skyblue1", high = "darkblue") +
  scale_x_continuous(limits=c(0,1750), expand=c(0,0)) +
  scale_y_continuous(name="Dark", limits= c(0,810), expand=c(0,0), position = "right") +
  theme_classic() +
  theme(legend.position="none", 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

#make plot 2
traj_fig_light <- ggplot(tracking_data_subset_light, aes(x = X, y = Y, group = larva_ID, colour = act_time)) + 
  geom_point(size = 0.7, alpha=0.8) +
  scale_color_gradient(low = "skyblue1", high = "darkblue") +
  scale_x_continuous(limits=c(0,1750), expand=c(0,0)) +
  scale_y_continuous(name="↓↓↓ Light on ↓↓↓", limits= c(0,810), expand=c(0,0), position = "right") +
  theme_classic() +
  theme(legend.position="none", 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

print(traj_fig_dark + traj_fig_light)

# TO prevent aspect ratio messing up, save it as a png and read it back in...
png("pictures/Fig02_panelC.png", height = 100, width = 105, units = 'mm', res = 300)
traj_fig_dark + traj_fig_light +
plot_layout(ncol = 1)
dev.off() 


# # Save combinations of plots ----------------------------------------------

# FIRST PLOT FOR SWIMMING SPEED
# SECOND PLOT FOR SWIMMING DIRECTIONS

#import a legend saved early and scale to 0.8 size
legend1 <- readPNG("pictures/swim_direction_legend.png")
legend1 <- cowplot::ggdraw() + cowplot::draw_image(legend1, scale = 0.9)
space <- readPNG("pictures/blank_space.png")
space <- cowplot::ggdraw() + cowplot::draw_image(space, scale = 1)

tiff("pictures/H_Speed_fig.tiff", width = 10, height = 7.5, units = 'in', res = 400)
plotvec[[1]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[2]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[3]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank()) +
  plotvec[[4]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[5]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plotvec[[6]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plot_layout(ncol=2)
dev.off()

tiff("pictures/H_Swim_direction_fig.tiff", width = 10, height = 10, units = 'in', res = 400)
plotvec[[7]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[8]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[9]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank()) +
  plotvec[[10]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[11]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plotvec[[12]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  legend1 +
  plot_layout(ncol=2)
dev.off()

tiff("pictures/H_Horizontal_displacement_fig.tiff", width = 10, height = 7.5, units = 'in', res = 400)
plotvec[[13]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[14]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[15]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank()) +
  plotvec[[16]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[17]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plotvec[[18]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plot_layout(ncol=2)
dev.off()

# boxplot for mean H_displacement data (not used)
tiff("pictures/H_displacement_means.tiff", width = 125, height = 76, units = 'mm', res = 400)
HD_mean + theme_classic() + theme(axis.text.x= element_text(colour="black"),
                                  axis.text.y= element_text(colour="black"),
                                  axis.ticks = element_line(colour="black"))
dev.off()

# dotplot for mean H_displacement data 
tiff("pictures/H_disp_means_SD.tiff", width = 125, height = 76, units = 'mm', res = 400)
HD_mean_SD + theme_classic() + theme(axis.text.x= element_text(colour="black"),
                                     axis.text.y= element_text(colour="black"),
                                     axis.ticks = element_line(colour="black"))
dev.off()

# read in trajectory data
panelC <- readPNG("pictures/Fig02_panelC.png")
panelC <- cowplot::ggdraw() + cowplot::draw_image(panelC, scale = 1.1)

# figure for paper
png("figures/Fig02_Swim_speed_direction_means.png", width = 255, height = 75, units = 'mm', res = 400)
SPEED_mean + theme_classic() + theme(axis.text.x= element_text(colour="black"),
                                        axis.text.y= element_text(colour="black"),
                                        axis.ticks = element_line(colour="black")) +
  DIR_mean + theme_classic() + theme(axis.text.x= element_text(colour="black"),
                                     axis.text.y= element_text(colour="black"), legend.position="none",
                                     axis.ticks = element_line(colour="black")) +
  legend1 +
  panelC +
  plot_layout(ncol=4, widths = c(4,4,1,4)) +
  plot_annotation(tag_levels = list(c("A","B","","C")))
dev.off()  



