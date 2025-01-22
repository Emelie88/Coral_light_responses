#setwd("//arc.ex.ac.uk/share/Emelie/R/R Acropora project") ## WINDOWS LAPTOP
#setwd("/ebio/ag-jekely/share/Emelie/R/R Acropora project")  ## LINUX COMPUTER

library("ggplot2")
library("Hmisc")
library(dplyr) # easier data wrangling
library(viridis) # colour blind friendly palette, works in B&W also
library(tidyr)
library(tidyverse)
library(patchwork)
library(showtext)
library(png)
library(ggpubr)

rm(list=ls())
data <- read.table("data/shapechanges_pooled.txt", sep="\t", header=T)

# compile 2021 data
data2021 <- data.frame()
data2021 <- data[data$Year == "2021",]
data2021_grouped <- data2021 %>% group_by(time_s) 
n_2021 <- length(unique(data2021$coral_ID)) # calculate number of larvae
data2021_means<- summarise(data2021_grouped, mean_length=mean(length_um), sd_length=sd(length_um), se_length = sqrt(sd_length/n_2021)) 

ratio_means2021 <- summarise(
  data2021_grouped, 
  mean_ratio = mean(ratio), 
  se_ratio = sd(ratio) / sqrt(n_2021), 
  lower_ci_ratio = mean_ratio - 1.96 * se_ratio, 
  upper_ci_ratio = mean_ratio + 1.96 * se_ratio
)

area2021 <- summarise(
  data2021_grouped, 
  mean_area = mean(area_mm), 
  se_area = sd(area_mm) / sqrt(n_2021), 
  lower_ci_area = mean_area - 1.96 * se_area, 
  upper_ci_area = mean_area + 1.96 * se_area
)

# compile 2022 data
data2022 <- data.frame()
data2022 <- data[data$Year == "2022",]
data2022_grouped <- data2022 %>% group_by(time_s) 
n_2022 <- length(unique(data2022$coral_ID))
data2022_means<- summarise(data2022_grouped, mean_length=mean(length_um), sd_length=sd(length_um), se_length = sqrt(sd_length/n_2022))

ratio_means2022 <- summarise(
  data2022_grouped, 
  mean_ratio = mean(ratio), 
  se_ratio = sd(ratio) / sqrt(n_2022), 
  lower_ci_ratio = mean_ratio - 1.96 * se_ratio, 
  upper_ci_ratio = mean_ratio + 1.96 * se_ratio
)

area2022 <- summarise(
  data2022_grouped, 
  mean_area = mean(area_mm), 
  se_area = sd(area_mm) / sqrt(n_2022), 
  lower_ci_area = mean_area - 1.96 * se_area, 
  upper_ci_area = mean_area + 1.96 * se_area
)


####### MAKE PLOTS ####### 
# coral ratios # with confidence intervals instead (using geom_smooth) NOTE  - SOME DATA HIDDEN FOR 2021 
Fig_ratio <- ggplot() +
  annotate("rect", xmin=90, xmax=180, ymin=0, ymax=3.5, alpha=0.2, fill="black") +
  annotate("rect", xmin=270, xmax=360, ymin=0, ymax=3.5, alpha=0.2, fill="black") +
  annotate("text", x=45, y = 3.4, label = "white light", size=2.5) +
  annotate("text", x=135, y = 3.4, label = "dark", size=2.5) +
  annotate("text", x=225, y = 3.4, label = "white light", size=2.5) +
  annotate("text", x=315, y = 3.4, label = "dark", size=2.5) +
  geom_ribbon(data = ratio_means2021, aes(time_s, y = mean_ratio, ymin = lower_ci_ratio, ymax = upper_ci_ratio, fill = "2021", alpha = 0.1)) +
  geom_ribbon(data = ratio_means2022, aes(time_s, y = mean_ratio, ymin = lower_ci_ratio, ymax = upper_ci_ratio, fill = "2022", alpha = 0.1)) +
  geom_line(data = ratio_means2021, aes(time_s, y = mean_ratio), colour = "#df180bff", size = 1) +
  geom_line(data = ratio_means2022, aes(time_s, y = mean_ratio), colour = "#007c80ff", size = 1) +
  geom_point(data = ratio_means2021, aes(time_s, y = mean_ratio), colour = "#df180bff", size = 1, pch = 16) +
  geom_point(data = ratio_means2022, aes(time_s, y = mean_ratio), colour = "#007c80ff", size = 1, pch = 16) +
  scale_x_continuous(name = "time (s)", breaks = seq(0, 360, 90), limits = c(0, 365), expand = c(0, 0)) +
  scale_y_continuous(name = "length/width ratio", limits = c(0, 3.5), breaks = seq(0, 3.5, 0.5), expand = c(0, 0))


# coral 2d areas NOTE  - SOME DATA HIDDEN FOR 2021
Fig_area <- ggplot()+
  annotate("rect", xmin=90, xmax=180, ymin=0, ymax=350, alpha=0.2, fill="black") +
  annotate("rect", xmin=270, xmax=360, ymin=0, ymax=350, alpha=0.2, fill="black") +
  annotate("text", x=45,  y = 340, label = "white light", size=2.5) +
  annotate("text", x=135, y = 340, label = "dark", size=2.5) +
  annotate("text", x=225, y = 340, label = "white light", size=2.5) +
  annotate("text", x=315, y = 340, label = "dark", size=2.5) +
  geom_ribbon(data = area2021, aes(time_s, y = mean_area, ymin = lower_ci_area, ymax = upper_ci_area, fill = "2021", alpha = 0.1)) +
  geom_ribbon(data = area2022, aes(time_s, y = mean_area, ymin = lower_ci_area, ymax = upper_ci_area, fill = "2022", alpha = 0.1)) +
  geom_line(data=area2021, aes(time_s, y=mean_area), colour="#df180bff", size=1) +
  geom_line(data=area2022, aes(time_s, y=mean_area), colour="#007c80ff", size=1) +
  geom_point(data=area2021, aes(time_s, y=mean_area), colour="#df180bff", size=1, pch=16) +
  geom_point(data=area2022, aes(time_s, y=mean_area), colour="#007c80ff", size=1, pch=16) +
  scale_x_continuous(name="time (s)", limits=c(0,365), breaks = seq(0,360,90), expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks=seq(0,350,50)) +
  labs(y=expression(paste('Larval profile area (', mm^{2},')')))


# read in legend and photo examples
legend2 <- readPNG("pictures/legend2.png")
legend2 <- cowplot::ggdraw() + cowplot::draw_image(legend2, scale = 1.2)

imageexamples <- readPNG("pictures/shape_changes.png")
imageexamples <- cowplot::ggdraw() + cowplot::draw_image(imageexamples, scale = 1.2)

# compile paneled figure
png("figures/Fig05_shapechanges.png", width = 220, height = 76, units = 'mm', res = 400)
Fig_ratio + theme_classic() + theme(legend.position = "none") +
Fig_area + theme_classic() + theme(legend.position = "none") +
legend2 +
imageexamples +
plot_layout(ncol=4, widths = c(3,3,1,2.5)) + # design =
  plot_annotation(tag_levels = list(c("A","B","","C")))
dev.off()

# STATISTICS FOR RATIO CHANGES COMPARING LAST 45s OF EACH LIGHT PHASE

# 2021
# remove rows except those in last 45 sec of each time period  
sechalf_21 <- data2021 %>%
  filter((time_s >= 60 & time_s <= 90) |
           (time_s >= 135 & time_s <= 180) | 
           (time_s >= 225 & time_s <= 270) | 
           (time_s >= 315 & time_s <= 360))

dark21 <- subset(sechalf_21, light=="off", select = ratio)
dark21$treatment <- "dark"
lightON21 <- subset(sechalf_21, light=="on", select = ratio)
lightON21$treatment <- "light on"
#combine data into one frame
statdata21 <- rbind(dark21, lightON21)

#test for normality (if p<0.05, data is NOT normal and non-parametric test needed)
shapiro.test(dark21$ratio) # not normal
shapiro.test(lightON21$ratio) # not normal
#figures - if points align along line, it indicates normality
ggqqplot(dark21$ratio) 
ggqqplot(lightON21$ratio) 

# data were not normal so we used a Mann-Whitney-Wilcoxon Test:
wilc_test21 <- wilcox.test(ratio ~ treatment, data = statdata21)
print(wilc_test21)

# create significance ID
prob21 <- unlist(wilc_test21[[3]])

if (prob21 < 0.001) {
  sig21 <- "***"
}  else if (between(prob21, 0.001, 0.01)) {
  sig21 <- "**"
}  else if (between(prob21, 0.01, 0.05)) {
  sig21 <- "*"
}  else if (prob21 > 0.05) {
  sig21 <- "-"
}

# 2022 
# remove rows except those in last 45 sec of each time period  
sechalf_22 <- data2022 %>%
  filter((time_s >= 45 & time_s <= 90) |
           (time_s >= 135 & time_s <= 180) | 
           (time_s >= 225 & time_s <= 270) | 
           (time_s >= 315 & time_s <= 360))

dark22 <- subset(sechalf_22, light=="off", select = ratio)
dark22$treatment <- "dark"
lightON22 <- subset(sechalf_22, light=="on", select = ratio)
lightON22$treatment <- "light on"
#combine data into one frame
statdata22 <- rbind(dark22, lightON22)

#test for normality (if p<0.05, data is NOT normal and non-parametric test needed)
shapiro.test(dark22$ratio) # not normal
shapiro.test(lightON22$ratio) # not normal

#figures - if points align along line, it indicates normality
ggqqplot(dark22$ratio) 
ggqqplot(lightON22$ratio) 

# data were not normal so we used a Mann-Whitney-Wilcoxon Test:
wilc_test22 <- wilcox.test(ratio ~ treatment, data = statdata22)
print(wilc_test22)

# create significance ID
prob22 <- unlist(wilc_test22[[3]])

if (prob22 < 0.001) {
  sig22 <- "***"
}  else if (between(prob22, 0.001, 0.01)) {
  sig22 <- "**"
}  else if (between(prob22, 0.01, 0.05)) {
  sig22 <- "*"
}  else if (prob22 > 0.05) {
  sig22 <- "-"
}

# Mann-Whitney test results
stats21 <- c("ratio 2021", median(dark21$ratio), median(lightON21$ratio), unlist(wilc_test21[[1]]), prob21, sig21)
stats22 <- c("ratio 2022", median(dark22$ratio), median(lightON22$ratio), unlist(wilc_test22[[1]]), prob22, sig22)
as.data.frame(shape_change_stats[1,1]<-"ratio")
shape_change_stats <- rbind(stats21, stats22)
colnames(shape_change_stats) <- c("year", "dark_med", "light_on_med", "W-value", "p-value", "signif.")
print(shape_change_stats)


# STATISTICS FOR (2D) AREA CHANGES COMPARING LAST 45s OF EACH LIGHT PHASE

# 2021

dark21_a <- subset(sechalf_21, light=="off", select = area_mm)
dark21_a$treatment <- "dark"
lightON21_a <- subset(sechalf_21, light=="on", select = area_mm)
lightON21_a$treatment <- "light on"
#combine data into one frame
statdata21_a <- rbind(dark21_a, lightON21_a)

#test for normality (if p<0.05, data is NOT normal and non-parametric test needed)
shapiro.test(dark21_a$area_mm) # not normal
shapiro.test(lightON21_a$area_mm) # not normal
#figures - if points align along line, it indicates normality
ggqqplot(dark21_a$area_mm) 
ggqqplot(lightON21_a$area_mm) 

# data were not normal so we used a Mann-Whitney-Wilcoxon Test:
wilc_test21_a <- wilcox.test(area_mm ~ treatment, data = statdata21_a)
print(wilc_test21_a)

# create significance ID
prob21a <- unlist(wilc_test21_a[[3]])

if (prob21a < 0.001) {
  sig21a <- "***"
}  else if (between(prob21a, 0.001, 0.01)) {
  sig21a <- "**"
}  else if (between(prob21a, 0.01, 0.05)) {
  sig21a <- "*"
}  else if (prob21a > 0.05) {
  sig21a <- "-"
}

# 2022 

dark22_a <- subset(sechalf_22, light=="off", select = area_mm)
dark22_a$treatment <- "dark"
lightON22_a <- subset(sechalf_22, light=="on", select = area_mm)
lightON22_a$treatment <- "light on"
#combine data into one frame
statdata22_a <- rbind(dark22_a, lightON22_a)

#test for normality (if p<0.05, data is NOT normal and non-parametric test needed)
shapiro.test(dark22_a$area_mm) # not normal
shapiro.test(lightON22_a$area_mm) # not normal
#figures - if points align along line, it indicates normality
ggqqplot(dark22_a$area_mm) 
ggqqplot(lightON22_a$area_mm) 

# data were not normal so we used a Mann-Whitney-Wilcoxon Test:
wilc_test22_a <- wilcox.test(area_mm ~ treatment, data = statdata22_a)
print(wilc_test22_a)

# create significance ID
prob22a <- unlist(wilc_test22_a[[3]])

if (prob22a < 0.001) {
  sig22a <- "***"
}  else if (between(prob22a, 0.001, 0.01)) {
  sig22a <- "**"
}  else if (between(prob22a, 0.01, 0.05)) {
  sig22a <- "*"
}  else if (prob22a > 0.05) {
  sig22a <- "-"
}

# Mann-Whitney test results - create table and save
stats21_a <- c("area 2021", median(dark21_a$area_mm), median(lightON21_a$area_mm), unlist(wilc_test21_a[[1]]), prob21a, sig21a)
stats22_a <- c("area 2022", median(dark22_a$area_mm), median(lightON22_a$area_mm), unlist(wilc_test22_a[[1]]), prob22a, sig22a)
shape_change_stats <- rbind(shape_change_stats, stats21_a)
shape_change_stats <- rbind(shape_change_stats, stats22_a)

colnames(shape_change_stats) <- c("metric / year", "dark_med", "light_on_med", "W-value", "p-value", "signif.")
print(shape_change_stats)

write.table(shape_change_stats, file = "data/shape_change_stats.txt", sep ="\t", row.names = FALSE, quote = FALSE)
