library(ggplot2)
library(dplyr)
library(reshape)
library(xtable)

theme_set(theme_light())
setwd("C:/Users/u19i96/Documents/MastersThesis/simulations")


#### functions ####
parse_csv_min_data <- function(raw_csv_results) {
  temp_data = raw_csv_results[0, ]
  for(i in 1:nrow(raw_csv_results)-1) {       # for-loop over columns
    if ((i + 1) %% (num_of_gen + 1) == 0) {
      temp_data <- rbind(temp_data, raw_csv_results[i+1, ])
    }
  }
  temp_data <- temp_data[, res_col_names]
  rownames(temp_data) <- 1:res_row_size
  return (temp_data)
} 



#### settings ####
simulation_name = "sim_1"
num_of_gen = 30
res_row_size = 1
res_column_size = 45
res_col_names <- c("min_0", "min_1", "min_2", "min_3", "min_4", "min_5", "min_6", "min_7", "min_8", "min_9")

##### Load Data ####
sim.opt_min = read.csv(paste0('evaluation/results/', simulation_name, '_tag.csv'))[,0:res_column_size]
sim.def_min = read.csv(paste0('evaluation/results/', simulation_name, '_default.csv'))[,0:res_column_size]
sim.rand = read.csv(paste0('evaluation/results/', simulation_name, '_random.csv'))
sim.opt_diversity = read.csv(paste0('evaluation/results/', simulation_name, '_tag_diversity.csv'))
sim.def_diversity = read.csv(paste0('evaluation/results/', simulation_name, '_default_diversity.csv'))

#### merge data
# merge best results with random results
threshold <- quantile(sim.rand$results, 0.2)
sim.rand_best <- subset(sim.rand, sim.rand$results <= threshold)
sim.rand_t <- data.frame(t(sim.rand_best))
sim.res <- rbind(parse_csv_min_data(sim.opt_min), parse_csv_min_data(sim.def_min))
sim.res <- bind_rows(sim.res, sim.rand_t)
sim.res$simulation <- c("Optimized", "Default", "Random")
sim.res["simulation"] <- lapply(sim.res["simulation"] , factor)
sim.res_column_names <- colnames(sim.res)
sim.res_column_names <- sim.res_column_names[sim.res_column_names != "simulation"]
sim.res.melted<-melt(sim.res, id = c("simulation"), measured = sim.res_column_names)
sim.res.melted$value <- ((-sim.res.melted$value + 3500) / 100)

# merge diversity
sim.opt_diversity$simulation <- "Optimized"
sim.def_diversity$simulation <- "Default"

sim.diversity <- bind_rows(sim.opt_diversity, sim.def_diversity)
sim.diversity["simulation"] <- lapply(sim.diversity["simulation"] , factor)
sim.diversity["repetition"] <- lapply(sim.diversity["repetition"] , factor)


# merge all ga generations
sim.all_ga.opt_min <- sim.opt_min
sim.all_ga.def_min <- sim.def_min

sim.all_ga.opt_min$simulation <- "Optimized"
sim.all_ga.def_min$simulation <- "Default"
sim.all_ga <- bind_rows(sim.all_ga.opt_min, sim.all_ga.def_min)
sim.all_ga["simulation"] <- lapply(sim.all_ga["simulation"] , factor)
sim.all_ga.melted<-melt(sim.all_ga[, c("gen", "simulation", res_col_names)], 
                         id = c("gen", "simulation"), 
                         measured = res_col_names)
sim.all_ga.melted$value <- ((3500 -sim.all_ga.melted$value) / 100)

##### plot data ####
# plot performance comparison
desired_levels <- c("Optimized", "Default")

plot.bp <- ggplot(sim.res.melted, aes(simulation, value)) + 
  geom_boxplot(data = subset(sim.res.melted, simulation %in% desired_levels), color="#457b9d") + 
  geom_point(color="#457b9d")  +
  labs(x = "", y = "Cummulated Emergency Break Duration (s)")
print(plot.bp)
ggsave(paste0('evaluation/plots/', simulation_name, '_comparison.jpg'), plot = plot.bp, width = 12, height = 6, units = "cm", dpi = 600)


# plot diversity comparison
plot.diversity_comparison <- ggplot(sim.diversity, aes(x=generation, y=diversity, group=simulation, color=simulation)) + 
  stat_summary(fun.data = "mean_se", geom = "line", linewidth = 1) + 
  stat_summary(fun.max = function(y) max(y), fun.min = function(y) min(y), geom = "ribbon", alpha = 0.1, aes(fill = simulation)) +
  labs(x = "Generations", y = "Diversity", color = "Simulations", fill = "Simulations")
print(plot.diversity_comparison)
ggsave(paste0('evaluation/plots/', simulation_name, '_ga_diversity.jpg'), plot = plot.diversity_comparison, width = 12, height = 8, units = "cm", dpi = 600)


# plot generation comparison
plot.generation_comparison <- ggplot(sim.all_ga.melted, aes(x=gen, y=value, group=simulation, color=simulation)) + 
  stat_summary(fun.data = "mean_se", geom = "line", linewidth = 1) + 
  stat_summary(fun.max = function(y) max(y), fun.min = function(y) min(y), geom = "ribbon", alpha = 0.1, aes(fill = simulation)) +
  labs(x = "Generations", y = "Cummulated Emergency Break Duration (s)", color = "Simulations", fill = "Simulations")
plot.generation_comparison <- plot.generation_comparison + guides(color = FALSE, fill = FALSE)
print(plot.generation_comparison)
ggsave(paste0('evaluation/plots/', simulation_name, '_ga_generations.jpg'), plot = plot.generation_comparison, width = 12, height = 8, units = "cm", dpi = 600)


##### t - test and effect size comparing default with optimized #####
sim.res.melted.only_ga <- subset(sim.res.melted, simulation != "Random")
sim.res.melted.only_ga <- subset(sim.res.melted.only_ga, !is.na(value))

t_test<-t.test(value ~ simulation, data = sim.res.melted.only_ga)
print(t_test)
t<-t_test$statistic[[1]]
df<-t_test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))

print("Effect Size:")
print(round(r, 3))


#### plot random distribution ####
rand_with_removed_value <- sim.rand
rand_with_removed_value$results <- ((-rand_with_removed_value$results + 3500) / 100)

test <- ggplot(rand_with_removed_value, aes(x = results)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram", x = "Values", y = "Frequency")

print(test)




install.packages("fitdistrplus")
library(fitdistrplus)

# Assuming skewed_data is your vector of skewed data
fit <- fitdist(rand_with_removed_value$results, "lnorm")  # Fit a log-normal distribution

# Summary of the fitted distribution
summary(fit)

# Plot the histogram and fitted distribution
plot(fit)

