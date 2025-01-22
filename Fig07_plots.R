## plotting calculated parameters from MtTack2 data
## This is the fourth step, following on from 3_Mtrack_analyse.R 
## Updated on 7 March 2024

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
library(ggpubr) 
library(nortest)

rm(list=ls()) ## clear the environment

#x values for shaded box annotations (corresponding to light treatments)
on1 <- 0
off1 <- 90
on2 <- 180
off2 <- 270
end <-360

# define treatments to correspond to the order in folder
st_speed <- data.frame()
st_VD <- data.frame()
treatment <- list()
sp_data_combined <- data.frame()
vd_data_combined <- data.frame()

#create a list of the files
pattern1 <- "CALCULATIONS.*\\.txt$"
file_paths1 <- fs::dir_ls("data/All_results_V", regexp = pattern1)  
print(file_paths1)
file_list1 <- list()

for (a in seq_along(file_paths1)) {
  file_list1[[a]] <- read.csv(
    file = file_paths1[[a]], 
    sep="\t", header=TRUE) } 
file_list1 <- set_names(file_list1, file_paths1)

# read summary files in
pattern2 <- "RESULTS.*\\.txt$"
file_paths2 <- fs::dir_ls("data/All_results_V", regexp = pattern2) 
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
comb_swim_dir_counts <- data.frame()
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
  
  select4 <- data.frame(all[,1:9])
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
  
   #change time_s values into 10 sec bins to combine values for wider box plots 
  combined_swim_dir$time_s <- round_any(combined_swim_dir$time_s, 10, f = ceiling)-5
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
  
  #OPTIONAL: change time_s values to combine values for wider box plots
  all_10s <- all
  all_10s$time_s <- round_any(all_10s$time_s, 10, f = ceiling)-5
  all_10s$time_s <- as.numeric(all_10s$time_s)  
  
  # boxplot for speed data
  SPEED <- ggplot(data=all_10s, aes(x=time_s, y=speed, group=time_s)) +
    annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=2.3, alpha=0.2, fill="black") +
    annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=2.3, alpha=0.2, fill="black") +
    annotate("text", x =(off1-on1)/2, y = 2.2, label = paste0("batch ", i)) +
    annotate("text", x = end*0.92, y = 2.2, label = paste0("n = ", nmax), fontface = 'italic', size=3) +
    geom_boxplot(fill="#48d1ccff", colour="#25948fff", width=7, outlier.size =0.4, outlier.shape = NA) +#, outlier.colour = "grey30") +
    stat_summary(fun=mean, geom="line", aes(group= 1), color="#cd0989ff") +
    scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
    scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end)  ) +
    scale_y_continuous(name="Swim speed (mm/s)", limits=c(0,2.4), expand=c(0,0))
  
  plotvec[[i]] <- SPEED
  
  # boxplot for vertical displacement, if light comes from left/bottom, the sign of VD has been reversed.
  VD <- ggplot(data=all_10s, aes(x=time_s, y=V_disp, group=time_s)) + 
    geom_hline(yintercept=0, colour="grey50", size=1) +
    annotate("rect", xmin=off1, xmax=on2, ymin=-2, ymax=2, alpha=0.2, fill="black") +
    annotate("rect", xmin=off2, xmax=end, ymin=-2, ymax=2, alpha=0.2, fill="black") +
    geom_boxplot(fill="#d9b0f4ff", colour="#ab49e6ff", width=7, outlier.size =0.4, outlier.shape = NA) +#, outlier.colour = "grey30") +
    annotate("text", x =(off1-on1)/2, y = 1.8, label = paste0("batch ", i)) +
    annotate("text", x = end*0.92, y = 1.8, label = paste0("n = ", nmax), fontface = 'italic', size=3) +
    stat_summary(fun=mean, geom="line", aes(group=1),  color="#0d14bbff") +
    scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
    scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end)  ) +
    scale_y_continuous(name="V displacement (mm/s)", limits=c(-2,2), expand=c(0,0)) 
  plotvec[[(NUM*2)+i]] <- VD
  
  # lineplot for swimming direction
  DIR <- ggplot(swim_dir, aes(x=time_s, y=proportion, group = swim_direction)) +
    annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=0.9, alpha=0.2, fill="black") +
    annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=0.9, alpha=0.2, fill="black") +
    geom_smooth(aes(color = swim_direction), method = loess, span=0.1, se=FALSE ) +
    geom_point(aes(color = swim_direction, alpha=0.5)) +
    annotate("text", x =(off1-on1)/2, y = 0.85, label = paste0("batch ", i)) +
    annotate("text", x = end*0.92, y = 0.85, label = paste0("n = ", nmax), fontface = 'italic', size=3) +
    scale_colour_manual(values=c("grey20", "#d90360ff", "grey60")) + 
    labs(colour="Swimming\ndirection") +
    scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end) ) +
    scale_y_continuous(name="Proportion of larvae",  expand=c(0,0)) 
  plotvec[[(NUM*1)+i]] <- DIR

#### STATISTICS #####
  
  # remove rows except those in middle 30 sec of each time period (this is when there is most difference in VD)
  # excluded the first bright light period from analysis
  
  all_mid30s <- all %>%
    filter(  (time_s >= 120 & time_s <= 150) | 
             (time_s >= 210 & time_s <= 240) | 
             (time_s >= 300 & time_s <= 330))
  
  # speed statistics
  sp_output <- aggregate(all_mid30s$speed, list(all_mid30s$treatment), mean) # mean values for all
  
  sp_dark <- subset(all_mid30s, treatment=="dark", select = speed)
  sp_dark$treatment <- "dark"
  sp_lightON <- subset(all_mid30s, treatment=="light on", select = speed)
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
  
  
    # Vertical displacement statistics
  vd_dark <- subset(all_mid30s, treatment=="dark", select = V_disp) # All dark   
  vd_dark$treatment <- "dark"
  vd_lightON <- subset(all_mid30s, treatment=="light on", select = V_disp) # SELECTS ALL LIGHT ON ####   
  vd_lightON$treatment <- "light on"
  
  #combine data into one frame
  vd_data <- rbind(vd_dark, vd_lightON)
  vd_data$batch <- i
  
  # collect for each batch
  vd_data_combined <-rbind(vd_data_combined, vd_data)
  
  # mean(vd_dark$V_disp)
  # mean(vd_lightON$V_disp)
  
  #test for normality (if p<0.05, data is NOT normal and non-parametric test needed)
  shapiro.test(vd_dark$V_disp) # NOT NORMAL
  ggqqplot(vd_dark$V_disp) 
  shapiro.test(vd_lightON$V_disp) # NOT NORMAL
  ggqqplot(vd_lightON$V_disp)
  
  # data were not normal so we used a Mann-Whitney-Wilcoxon Test:
  # Add an indicator variable for the group
  vd_dark$group <- "dark"
  vd_lightON$group <- "light"
  
  # Combine the dataframes
  vdstats_df <- rbind(vd_lightON, vd_dark)
  
  # Perform the Mann-Whitney-Wilcoxon test
  vd_Ttest <- wilcox.test(V_disp ~ group, data = vdstats_df)
  print(vd_Ttest)
  
  # p value is large and so the data was found to be not sig. different (there is a large difference in variance, but not median)
  
  # alterntive t-test for normal data (not used)
  # vd_Ttest <- t.test(vd_dark[,1], vd_lightON[,1])
  vd_prob <- unlist(vd_Ttest[[3]])
  
  if (vd_prob < 0.001) {
    vd_sig <- "***"
  }  else if (between(vd_prob, 0.001, 0.01)) {
    vd_sig <- "**"
  }  else if (between(vd_prob, 0.01, 0.05)) {
    vd_sig <- "*"
  }  else if (vd_prob > 0.05) {
    vd_sig <- "-"
  }
  
  vd_stats <- c(i, median(vd_dark$V_disp), median(vd_lightON$V_disp), unlist(vd_Ttest[[1]]), vd_prob, vd_sig)
  # for t-test
  #vd_stats <- c(mean(vd_dark$V_disp), mean(vd_lightON$V_disp), unlist(vd_Ttest[[1]]), unlist(vd_Ttest[[2]]), vd_prob, vd_sig)
  st_VD <- rbind(st_VD, vd_stats)

} # end of for loop


#stats on all replicate experiments pooled together
#split into dark and light again
sp_comb_dark <- sp_data_combined[sp_data_combined$treatment == "dark", ]
sp_comb_light <- sp_data_combined[sp_data_combined$treatment == "light on", ]
vd_comb_dark <- vd_data_combined[vd_data_combined$treatment == "dark", ]
vd_comb_light <- vd_data_combined[vd_data_combined$treatment == "light on", ]

#test for normality 
# shapiro-wilk test cannot be done as there are >5000 data points to compare, so I used the Anderson-Darling normality test instead
#(if p is very small, p<0.05, data are NOT normal and non-parametric test needed)

ad.test(sp_comb_dark$speed)   # not normal
ad.test(sp_comb_light$speed)  # not normal
ad.test(vd_comb_dark$V_disp)  # not normal
ad.test(vd_comb_light$V_disp) # not normal

sp_wilcoxon_pooled <- wilcox.test(speed ~ treatment, data = sp_data_combined)
print(sp_wilcoxon_pooled)
sp_stats_pooled <- c("pooled", median(sp_comb_dark$speed), median(sp_comb_light$speed), unlist(sp_wilcoxon_pooled[[1]]), unlist(sp_wilcoxon_pooled[[3]], "***"))
# ^ overall speed in last 45 seconds of each period is sig different between light treatments

vd_wilcoxon_pooled <- wilcox.test(V_disp ~ treatment, data = vd_data_combined)
print(vd_wilcoxon_pooled)
vd_stats_pooled <- c("pooled", median(vd_comb_dark$V_disp), median(vd_comb_light$V_disp), unlist(vd_wilcoxon_pooled[[1]]), unlist(vd_wilcoxon_pooled[[3]]), "***")
# ^ overall H_DISP is not sig different between light treatments, batch 2 did have a sig difference though

# add pooled result to the individual batch test results
st_speed <- rbind(st_speed, sp_stats_pooled)
st_VD <- rbind(st_VD, vd_stats_pooled)

#save stats tables
colnames(st_speed) <- c("batch", "dark", "light_on", "W-value", "p-value", "signif.") # wilcoxon
#colnames(st_speed) <- c("dark", "light_on", "T-test", "d.f.", "p-value", "signif.") # t-test
#setwd(wdir)
write.table(st_speed, file = "data/All_results_V/speed_stats.txt", sep ="\t", row.names = FALSE, quote = FALSE)
print(st_speed)

colnames(st_VD) <- c("batch", "dark", "light_on", "W-value", "p-value", "signif.") # wilcoxon
#colnames(st_HD) <- c("dark", "light_on", "T-test", "d.f.", "p-value", "signif.") # t-test
write.table(st_VD, file = "data/All_results_V/V_disp_stats.txt", sep ="\t", row.names = FALSE, quote = FALSE)
print(st_VD)




print(paste0("number of plots = ",length(plotvec)))

## combined data

combined_means <- aggregate(combined_data[,2:5],  list(combined_data$CALC.time_s), mean)
combined_sum <- aggregate(combined_data$CALC.n_larvae,  list(combined_data$CALC.time_s), sum)
combined_means <- cbind(combined_means, combined_sum)
colnames(combined_means) <- c("time_s", "speed", "s_std", "V_disp", "v_std", "time_again", "total_n")

#calculate the mean values across each 10 second period 
combined_means_10s_int <- combined_means %>% as_tibble()
combined_means_10s_int$time_s <- round_any(combined_means_10s_int$time_s, 10, f = ceiling)-5
combined_means_10s_int$time_s <- as.numeric(combined_means_10s_int$time_s)  

summary_10s_int <- aggregate(list(combined_means_10s_int$speed, 
                       combined_means_10s_int$s_std, 
                       combined_means_10s_int$V_disp, 
                       combined_means_10s_int$v_std, 
                       combined_means_10s_int$time_again,
                       combined_means_10s_int$total_n), 
                  by = list(combined_means_10s_int$time_s), mean)

colnames(summary_10s_int) <- c("time_s", "speed", "s_std", "V_disp", "v_std", "time_again", "total_n")


all_all_10s_int <- all_all
maxtime <- max(all_all_10s_int$time_s)

#change time_s values to combine values for wider box plots
all_all_10s_int$time_s <- round_any(all_all_10s_int$time_s, 10, f = ceiling)-5
all_all_10s_int$time_s <- as.numeric(all_all_10s_int$time_s)  

#make summary graphs from all batches

nmaxALL <- max(combined_means_10s_int$total_n)
nmaxALL <- round(nmaxALL)
max_y <- max(all_all_10s_int$all.speed)
max_y2 <- max(all_all_10s_int$all.V_disp)
min_y2 <- min(all_all_10s_int$all.V_disp)
top_y <- max_y
#top_y <- max_y*1.1
swimdir_nmax <- max(swim_dir_means$group_total)

SPEED_mean <- ggplot(data=all_all_10s_int, aes(x=time_s, y=all.speed, group=time_s)) + # label
  annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=1.8, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=1.8, alpha=0.2, fill="black") +
  geom_boxplot(fill="#48d1ccff", colour="#25948fff", width=7, outlier.size =0.4, outlier.shape = NA) +#, outlier.colour = "grey30") +
  stat_summary(fun=mean, geom="line", aes(group= 1), color="#cd0989ff") +
  scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end)  ) +
  scale_y_continuous(name="Swim speed (mm/s)", breaks = seq(0,1.8,0.25), limits = c(0,1.8), expand=c(0,0))

# speed mean +SE (dot and SD bar)
SPEED_mean_SD <- ggplot()+
  annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=1.2, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=1.2, alpha=0.2, fill="black") +
  annotate("text", x=45, y = 1.15, label = "white light", size=3) +
  annotate("text", x=135, y = 1.15, label = "dark", size=3) +
  annotate("text", x=225, y = 1.15, label = "white light", size=3) +
  annotate("text", x=315, y = 1.15, label = "dark", size=3) +
  geom_errorbar(data=summary_10s_int, mapping=aes(x=time_again, ymin=pmax(speed-s_std,0), ymax=speed+s_std), width=0.2, color="grey50") +
  geom_point(data=summary_10s_int, aes(x=time_again, y=speed)) +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end)  ) +
  scale_y_continuous(name="Swim speed (mm/s)", expand=c(0,0), breaks=seq(0,1.2,0.2), limits=c(0,1.2))

VD_mean <- ggplot(data=all_all_10s_int, aes(x=time_s, y=all.V_disp, group=time_s)) + # label
  geom_hline(yintercept=0, colour="grey50", size=1) +
  annotate("rect", xmin=off1, xmax=on2, ymin=-1, ymax=1, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=-1, ymax=1, alpha=0.2, fill="black") +
  geom_boxplot(fill="#d9b0f4ff", colour="#ab49e6ff", width=7, outlier.size =0.4, outlier.shape = NA) +#, outlier.colour = "grey30") +
  stat_summary(fun=mean, geom="line", aes(group= 1), color="#0d14bbff") +
  scale_fill_manual(values = col_table1$colour, name="Lighting\ncondition") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end)  ) +
  scale_y_continuous(name="Vertical displacement (mm/s)", limits=c(-1,1), breaks = seq(-1,1,.2), expand=c(0,0))

# VD mean +SD (dot and SD bar)
VD_mean_SD <- ggplot()+
  geom_hline(yintercept=0, colour="grey50", size=1) +
  annotate("rect", xmin=off1, xmax=on2, ymin=-0.8, ymax=0.8, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=-0.8, ymax=0.8, alpha=0.2, fill="black") +
  annotate("text", x=45, y = 0.75, label = "white light", size=3) +
  annotate("text", x=135, y = 0.75, label = "dark", size=3) +
  annotate("text", x=225, y = 0.75, label = "white light", size=3) +
  annotate("text", x=315, y = 0.75, label = "dark", size=3) +
  geom_errorbar(data=summary_10s_int, mapping=aes(x=time_again, ymin=V_disp-v_std, ymax=V_disp+v_std), width=0.2, color="grey50") +
  geom_point(data=summary_10s_int, aes(x=time_again, y=V_disp)) +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,45),  expand=c(0,0), limits=c(-1,end)  ) +
  scale_y_continuous(name="Vertical displacement (mm/s)", breaks = seq(-0.8,0.8,.2), limits= c(-0.8,0.8), expand=c(0,0))

DIR_mean <- ggplot(swim_dir_means, aes(x=time_s, y=proportion, group = swim_direction)) +
  annotate("rect", xmin=off1, xmax=on2, ymin=0, ymax=0.8, alpha=0.2, fill="black") +
  annotate("rect", xmin=off2, xmax=end, ymin=0, ymax=0.8, alpha=0.2, fill="black") +
  annotate("text", x=45, y = 0.77, label = "white light", size=3) +
  annotate("text", x=135, y = 0.77, label = "dark", size=3) +
  annotate("text", x=225, y = 0.77, label = "white light", size=3) +
  annotate("text", x=315, y = 0.77, label = "dark", size=3) +
#  annotate("text", x = end*0.92, y = 0.75, label = paste0("n = ", swimdir_nmax), fontface = 'italic', size=3) +
  geom_smooth(aes(color = swim_direction), method = loess, span=0.01, se=FALSE ) +
  geom_point(aes(color = swim_direction)) +
  scale_colour_manual(values=c("grey20", "#d90360ff", "grey60")) + 
  labs(colour="Swimming\ndirection") +
  scale_x_continuous(name="time (s)", breaks = seq(0,end,45), expand=c(0,0), limits=c(-1,end) ) +
  scale_y_continuous(name="Proportion of larvae",  expand=c(0,0)) 
  
  
# # Save combinations of plots ----------------------------------------------

# FIRST PLOT FOR SWIMMING SPEED
# SECOND PLOT FOR SWIMMING DIRECTIONS

#import a legend saved early and scale
legend2 <- readPNG("pictures/swim_direction_legend_V.png")
legend2 <- cowplot::ggdraw() + cowplot::draw_image(legend2, scale = 1)
space <- readPNG("pictures/blank_space.png")
space <- cowplot::ggdraw() + cowplot::draw_image(space, scale = 1)

#
tiff("pictures/V_Speed.tiff", width = 10, height = 7.5, units = 'in', res = 400)
plotvec[[1]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[2]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[3]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank()) +
  plotvec[[4]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[5]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plotvec[[6]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plot_layout(ncol=2)
dev.off()

tiff("pictures/V_Swim_direction.tiff", width = 10, height = 10, units = 'in', res = 400)
plotvec[[7]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[8]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
 plotvec[[9]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank()) +
  plotvec[[10]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[11]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plotvec[[12]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  legend2 +
  plot_layout(ncol=2)
dev.off()

tiff("pictures/V_Vertical_displacement.tiff", width = 10, height = 7.5, units = 'in', res = 400)
plotvec[[13]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[14]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[15]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank()) +
  plotvec[[16]] + theme_classic() + theme(axis.text.x = element_blank(), legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  plotvec[[17]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plotvec[[18]] + theme_classic() + theme(axis.title.y = element_blank(), legend.position="none") +
  plot_layout(ncol=2)
dev.off()


# boxplot & dotplot for mean speed data
tiff("pictures/V_Swim_speed_means.tiff",  width = 8, height = 3, units = 'in', res = 400)
SPEED_mean + theme_classic() +
  SPEED_mean_SD + theme_classic() +
  plot_layout(ncol=2)
dev.off()

# boxplot & dotplot  for mean V_displacement data
tiff("pictures/V_V_displacement_means.tiff", width = 8, height = 3, units = 'in', res = 400)
VD_mean + theme_classic() +
  VD_mean_SD + theme_classic() +
  plot_layout(ncol=2)
dev.off()

#lineplot for combined swimming direction data
tiff("pictures/V_Swimming_direction_means.tiff", width = 5, height = 3, units = 'in', res = 400)
DIR_mean + theme_classic() + theme(legend.position="none") +
  legend2 +
  plot_layout(ncol=2, widths = c(3, 1))
dev.off()

# panel B, show example trajectories coloured by direction
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
  
  filter(dy >= -40 & dy <= 40) # remove outliers

#create two data subsets, one for first light off period and the second for the light coming back on
tracking_data_subset_dark <- tracking_data %>% filter(batch == 2, 
                                                 time_s >= 91 & time_s <= 180 )

tracking_data_subset_light <- tracking_data %>% filter(batch == 2, 
                                                      time_s >= 181 & time_s <= 270 )

#make plot 1
traj_fig_dark <- ggplot(tracking_data_subset_dark, aes(x = x_start, y = y_start, 
                                             xend = x_end, yend = y_end, 
                                             color = x_change,
                                             alpha = alpha_scaled)) +
  geom_segment(size = 1, lineend = "round") +  # Draw line segments
  scale_color_manual(values = c("Up" = "#d90360ff", "Down" = "grey20")) +
  coord_fixed(ratio = 1230/300) + 
  coord_flip() +
  scale_x_continuous(limits=c(0,1230), expand=c(0,0)) +
  scale_y_continuous(name="Dark", limits= c(0,300), expand=c(0,0)) +
  theme_classic() +
  theme(legend.position="none", 
        axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 1))

#make plot 2
traj_fig_light <- ggplot(tracking_data_subset_light, aes(x = x_start, y = y_start, 
                                                       xend = x_end, yend = y_end, 
                                                       color = x_change,
                                                       alpha = alpha_scaled)) +
  geom_segment(size = 1, lineend = "round") +  # Draw line segments
  scale_color_manual(values = c("Up" = "#d90360ff", "Down" = "grey20")) +
  coord_fixed(ratio = 1230/300) + 
  coord_flip() +
  scale_x_continuous(limits=c(0,1230), expand=c(0,0)) +
  scale_y_continuous(name="Light on", limits= c(0,300), expand=c(0,0)) +
  theme_classic() +
  theme(legend.position="none", 
        axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 1))

print(traj_fig_dark + traj_fig_light)

# TO STOP aspect ratio messing up, save it as a png and read it back in...
png("pictures/Fig05_panelB.png", height = 124, width = 70, units = 'mm', res = 300)
traj_fig_dark + traj_fig_light
dev.off() 

panelB <- readPNG("pictures/Fig05_panelB.png")
panelB <- cowplot::ggdraw() + cowplot::draw_image(panelB, scale = 1.3)

# figure for the paper
png("figures/Fig07_Vertical_swimming.png", width = 260, height = 76, units = 'mm', res = 400)
  DIR_mean + theme_classic() + theme(legend.position="none") +
  legend2 +  
  panelB +
  VD_mean_SD + theme_classic() +
  plot_layout(ncol=4, widths = c(4,1.2,2.4,4)) +
  plot_annotation(tag_levels = list(c("A","","B", "C")))
dev.off() 
