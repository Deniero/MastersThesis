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
num_of_gen = 30
res_row_size = 1
res_column_size = 45
res_col_names <- c("min_0", "min_1", "min_2", "min_3", "min_4", "min_5", "min_6", "min_7", "min_8", "min_9")


##### Sim 1 #####
# load data #
sim_1.opt_min = read.csv('evaluation/results/sim_1_tag.csv')[,0:res_column_size]
sim_1.def_min = read.csv('evaluation/results/sim_1_tag_no_elite.csv')[,0:res_column_size]
sim_1.rand = read.csv('evaluation/results/sim_1_random.csv')
threshold <- quantile(sim_1.rand$results, 0.2)
sim_1.rand_best <- subset(sim_1.rand, sim_1.rand$results <= threshold)
sim_1.rand_t <- data.frame(t(sim_1.rand_best))


sim_1.res <- rbind(parse_csv_min_data(sim_1.opt_min), parse_csv_min_data(sim_1.def_min))


sim_1.res <- bind_rows(sim_1.res, sim_1.rand_t)


sim_1.res$simulation <- c("Optimized", "Default", "Random")
sim_1.res["simulation"] <- lapply(sim_1.res["simulation"] , factor)


# plot data #
sim_1.res_column_names <- colnames(sim_1.res)
sim_1.res_column_names <- sim_1.res_column_names[sim_1.res_column_names != "simulation"]
melted<-melt(sim_1.res, id = c("simulation"), measured = sim_1.res_column_names)

desired_levels <- c("Optimized", "Default")

plot.bp <- ggplot(melted, aes(simulation, value)) + 
  geom_boxplot(data = subset(melted, simulation %in% desired_levels), color="#457b9d") + 
  geom_point(color="#457b9d")  +
  labs(x = "", y = "Cost")
print(plot.bp)
ggsave("evaluation/plots/sim_1_comparison.jpg", plot = plot.bp, width = 12, height = 8, units = "cm", dpi = 600)

