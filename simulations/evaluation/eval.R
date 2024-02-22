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
simulation_name = "sim_4"
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
#threshold <- quantile(sim.rand$results, 0.2)
#sim.rand_best <- subset(sim.rand, sim.rand$results <= threshold)
sim.rand_t <- data.frame(t(sim.rand))


rand_fitness <- sim.rand
rand_fitness$name <- rep(0:9, each = 96 * 31)
rand_fitness$name <- as.factor(rand_fitness$name)
rand_top_results <- rand_fitness %>%
  group_by(name) %>%
  summarize(max_result = min(results))
sim.rand_t <- data.frame(t(rand_top_results$max_result))
colnames(sim.rand_t) <- res_col_names

sim.res <- rbind(parse_csv_min_data(sim.opt_min), parse_csv_min_data(sim.def_min))
sim.res <- bind_rows(sim.res, sim.rand_t)
sim.res$simulation <- c("Optimized", "Default", "Random")
sim.res["simulation"] <- lapply(sim.res["simulation"] , factor)
sim.res_column_names <- colnames(sim.res)
sim.res_column_names <- sim.res_column_names[sim.res_column_names != "simulation"]
sim.res.melted<-melt(sim.res, id = c("simulation"), measured = sim.res_column_names)
sim.res.melted$value <- ((-sim.res.melted$value + 3500) / 100)

sim.res.translated <- sim.res
sim.res.translated[, res_col_names] <- (3500 - sim.res.translated[, res_col_names]) / 100
x <- xtable(sim.res.translated)
#digits(x) <- 0
print(x)

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

plot.bp <- ggplot(sim.res.melted, aes(value, simulation)) + 
  geom_boxplot(color="#457b9d") + 
  labs(x = "Cumulated Emergency Brake Duration [s]", y = "")
print(plot.bp)
ggsave(paste0('evaluation/plots/', simulation_name, '_comparison.jpg'), plot = plot.bp, width = 16, height = 5, units = "cm", dpi = 1000)

# Same plot, but horizontal
plot.bp <- ggplot(sim.res.melted, aes(simulation, value)) + 
  geom_boxplot(color="#457b9d") + 
  labs(y = "Cumulated Emergency Brake Duration [s]", x = "")
print(plot.bp)
ggsave(paste0('evaluation/plots/', simulation_name, '_comparison_horizontal.jpg'), plot = plot.bp, width = 16, height = 5, units = "cm", dpi = 1000)


plot.bp <- ggplot(sim.res.melted, aes(value, simulation)) + 
  geom_boxplot(color="#457b9d") + 
  labs(x = "Cumulated Emergency Brake Duration [s]", y = "")  + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
print(plot.bp)
ggsave(paste0('evaluation/plots/', simulation_name, '_comparison_presentation.jpg'), plot = plot.bp, width = 16, height = 8, units = "cm", dpi = 800)

#plot.bp.left <- ggplot(sim.res.melted, aes(value, simulation)) + 
#  geom_boxplot(color="#457b9d") + coord_cartesian(xlim = c(-200, 0)) + scale_x_continuous(expand = c(0, 0)) + 
#  labs(y = "", x = "")
#print(plot.bp.left)

#plot.bp.right <- ggplot(sim.res.melted, aes(value, simulation)) + 
#  geom_boxplot(color="#457b9d") + coord_cartesian(xlim = c(0, 12)) + scale_x_continuous(expand = c(0, 0)) + 
#  labs(y = "", x = "") + theme(axis.text.y = element_blank(),
#                               axis.title.y = element_blank())
#print(plot.bp.right)
#ggsave(paste0('evaluation/plots/', simulation_name, '_comparison_left.jpg'), plot = plot.bp.left, width = 12, height = 6, units = "cm", dpi = 600)
#ggsave(paste0('evaluation/plots/', simulation_name, '_comparison_right.jpg'), plot = plot.bp.right, width = 12, height = 6, units = "cm", dpi = 600)



# plot diversity comparison
plot.diversity_comparison <- ggplot(sim.diversity, aes(x=generation, y=diversity, group=simulation, color=simulation)) + 
  stat_summary(fun.data = "mean_se", geom = "line", linewidth = 1) + 
  stat_summary(fun.max = function(y) quantile(y, 0.75), fun.min = function(y) quantile(y, 0.25), geom = "ribbon", alpha = 0.1, aes(fill = simulation)) +
  labs(x = "Generations", y = "Diversity", color = "Simulations", fill = "Simulations")
print(plot.diversity_comparison)
ggsave(paste0('evaluation/plots/', simulation_name, '_ga_diversity.jpg'), plot = plot.diversity_comparison, width = 12, height = 8.15, units = "cm", dpi = 600)

# plot generation comparison
plot.generation_comparison <- ggplot(sim.all_ga.melted, aes(x=gen, y=value, group=simulation, color=simulation)) + 
  stat_summary(fun.data = "mean_se", geom = "line", linewidth = 1) + 
  stat_summary(fun.max = function(y) quantile(y, 0.75), fun.min = function(y) quantile(y, 0.25), geom = "ribbon", alpha = 0.1, aes(fill = simulation)) +
  labs(x = "Generations", y = "Cumulated Emergency Brake Duration [s]", color = "Simulations", fill = "Simulations")
plot.generation_comparison <- plot.generation_comparison + guides(color = FALSE, fill = FALSE)
print(plot.generation_comparison)
ggsave(paste0('evaluation/plots/', simulation_name, '_ga_generations.jpg'), plot = plot.generation_comparison, width = 12, height = 8.15, units = "cm", dpi = 600)




# plot diversity comparison presentation
plot.diversity_comparison <- ggplot(sim.diversity, aes(x=generation, y=diversity, group=simulation, color=simulation)) + 
  stat_summary(fun.data = "mean_se", geom = "line", linewidth = 1) + 
  stat_summary(fun.max = function(y) quantile(y, 0.75), fun.min = function(y) quantile(y, 0.25), geom = "ribbon", alpha = 0.1, aes(fill = simulation)) +
  labs(x = "Generations", y = "Diversity", color = "Simulations", fill = "Simulations")   + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
print(plot.diversity_comparison)
ggsave(paste0('evaluation/plots/', simulation_name, '_ga_diversity_present.jpg'), plot = plot.diversity_comparison, width = 12, height = 8.15, units = "cm", dpi = 600)

# plot generation comparison presentation
plot.generation_comparison <- ggplot(sim.all_ga.melted, aes(x=gen, y=value, group=simulation, color=simulation)) + 
  stat_summary(fun.data = "mean_se", geom = "line", linewidth = 1) + 
  stat_summary(fun.max = function(y) quantile(y, 0.75), fun.min = function(y) quantile(y, 0.25), geom = "ribbon", alpha = 0.1, aes(fill = simulation)) +
  labs(x = "Generations", y = "Cumulated Emergency\nBrake Duration [s]", color = "Simulations", fill = "Simulations")
plot.generation_comparison <- plot.generation_comparison + guides(color = FALSE, fill = FALSE)   + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
print(plot.generation_comparison)
ggsave(paste0('evaluation/plots/', simulation_name, '_ga_generations_present.jpg'), plot = plot.generation_comparison, width = 12, height = 8.15, units = "cm", dpi = 600)



##### t - test and effect size #####
print("comparing Optimized with Default")
sim.res.melted.only_ga <- subset(sim.res.melted, simulation != "Random")
#sim.res.melted.only_ga <- subset(sim.res.melted.only_ga, !is.na(value))
sim.res.melted.only_ga$simulation <- relevel(sim.res.melted.only_ga$simulation, ref = "Optimized")

t_test<-t.test(value ~ simulation, data = sim.res.melted.only_ga)
print(t_test)
t<-t_test$statistic[[1]]
df<-t_test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))

group_summary <- sim.res.melted.only_ga %>%
  group_by(simulation) %>%
  summarise(std = sd(value))

group_size <- length(res_col_names)
group_summary$se <- group_summary$std / sqrt(group_size)
print('SE:')
print(group_summary)



print("Effect Size:")
print(round(r, 3))

print("comparing Optimized with Random")
sim.res.melted.rand_with_opt <- subset(sim.res.melted, simulation != "Default")
#sim.res.melted.only_ga <- subset(sim.res.melted.only_ga, !is.na(value))

t_test<-t.test(value ~ simulation, data = sim.res.melted.rand_with_opt)
print(t_test)
t<-t_test$statistic[[1]]
df<-t_test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))


group_summary <- sim.res.melted.rand_with_opt %>%
  group_by(simulation) %>%
  summarise(std = sd(value))

group_size <- length(res_col_names)
group_summary$se <- group_summary$std / sqrt(group_size)
print('SE:')
print(group_summary)


print("Effect Size:")
print(round(r, 3))



#### plot random distribution ####

#rand_fitness$name_index <- rep(0:(nrow(rand_fitness) / (96 * 31) - 1), each = 96 * 31)

#rand_fitness$name_index <- ave(rep(1, nrow(rand_fitness)), rand_fitness$name, FUN = seq_along) - 1
#test <- ggplot(rand_fitness, aes(x = results)) +
#  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
#  labs(title = "Histogram", x = "Values", y = "Frequency")

#print(test)


#plot.rand_to_mean <- ggplot(rand_fitness, aes(x = name_index, y = results)) +
#  geom_point() +
#  facet_wrap(~name, scales = "free_x", ncol=10) +
#  geom_hline(yintercept = 8.52, color = "red") +
#  coord_cartesian(ylim = c(-180, 15)) +
#  labs(x = "",
#       y = "Cummulated Emergency Break Duration")
#print(plot.rand_to_mean)

#ggsave(paste0('evaluation/plots/', simulation_name, '_rand_to_mean.jpg'), plot = plot.rand_to_mean, width = 20, height = 8, units = "cm", dpi = 600)


#rand_top_results <- rand_fitness %>%
#  group_by(name) %>%
#  summarize(max_result = max(results))

# Print the new dataframe
#print(rand_top_results)
