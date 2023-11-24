library(ggplot2)
library(dplyr)
library(reshape)
library(xtable)

theme_set(theme_light())
setwd("C:/Users/u19i96/Documents/MastersThesis/simulations")

sim_tag.diversity = read.csv('evaluation/results/sim_1_tag_diversity.csv')
sim_def.diversity = read.csv('evaluation/results/sim_1_default_diversity.csv')

# TODO merge


# generate mean line chart with confidence interval
sim_tag.diversity$simulation <- "Optimized"
sim_def.diversity$simulation <- "Default"

sim <- bind_rows(sim_tag.diversity, sim_def.diversity)
sim["simulation"] <- lapply(sim["simulation"] , factor)
sim["repetition"] <- lapply(sim["repetition"] , factor)


plot.line <- ggplot(sim, aes(x=generation, y=diversity, group=simulation, color=simulation)) + 
  geom_point() + 
  stat_summary(fun.data = "mean_se", geom = "line", size = 1)
print(plot.line)


plot.ribbon <- ggplot(sim, aes(x=generation, y=diversity, group=simulation, color=simulation)) + 
  stat_summary(fun.data = "mean_se", geom = "line", size = 1) + 
  stat_summary(fun.max = function(y) max(y), fun.min = function(y) min(y), geom = "ribbon", alpha = 0.1, aes(fill = simulation)) +
  labs(x = "Generation", y = "Diversity", color = "Simulations", fill = "Simulations")
print(plot.ribbon)
ggsave("evaluation/plots/sim_1_diversity.jpg", plot = plot.ribbon, width = 12, height = 8, units = "cm", dpi = 600)
