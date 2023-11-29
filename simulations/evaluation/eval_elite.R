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



##### load data #####
sim_1.elite_min = read.csv('evaluation/results/sim_1_tag.csv')[,0:res_column_size]
sim_1.no_elite_min = read.csv('evaluation/results/sim_1_tag_no_elite.csv')[,0:res_column_size]

sim_1.res <- rbind(parse_csv_min_data(sim_1.elite_min), parse_csv_min_data(sim_1.no_elite_min))


sim_1.res$simulation <- c("Elite", "NoElite")
sim_1.res["simulation"] <- lapply(sim_1.res["simulation"] , factor)


#### plot boxplot ####
sim_1.res_column_names <- colnames(sim_1.res)
sim_1.res_column_names <- sim_1.res_column_names[sim_1.res_column_names != "simulation"]
melted<-melt(sim_1.res, id = c("simulation"), measured = sim_1.res_column_names)


plot.bp <- ggplot(melted, aes(simulation, value)) + 
  geom_boxplot(color="#457b9d") + 
  geom_point(color="#457b9d")  +
  labs(x = "", y = "Cost")
print(plot.bp)
ggsave("evaluation/plots/elite_vs_no_elite.jpg", plot = plot.bp, width = 12, height = 8, units = "cm", dpi = 600)


#### plot over generations ####
selected_repetitions = c("min_2", "min_3", "min_8")
melted_generations<-melt(sim_1.elite_min[, c("gen", selected_repetitions)], 
                         id = c("gen"), 
                         measured = selected_repetitions)
levels(melted_generations$variable) <- c("Rep 1", "Rep 2", "Rep 3")
plot.line <- ggplot(melted_generations, aes(gen, value, color=variable)) + geom_line()  +
  labs(x = "Generations", y = "Cost", color = "Repetitions")
print(plot.line)
ggsave("evaluation/plots/ga_elite_generations.jpg", plot = plot.line, width = 15, height = 7, units = "cm", dpi = 300)


selected_repetitions = c("min_2", "min_3", "min_8")
melted_generations<-melt(sim_1.no_elite_min[, c("gen", selected_repetitions)], 
                         id = c("gen"), 
                         measured = selected_repetitions)
levels(melted_generations$variable) <- c("Rep 1", "Rep 2", "Rep 3")
plot.line <- ggplot(melted_generations, aes(gen, value, color=variable)) + geom_line()  +
  labs(x = "Generations", y = "Cost", color = "Repetitions")
print(plot.line)
ggsave("evaluation/plots/ga_no_elite_generations.jpg", plot = plot.line, width = 15, height = 7, units = "cm", dpi = 300)

#### T test ####

t_test<-t.test(value ~ simulation, data = melted)
print(t_test)
t<-t_test$statistic[[1]]
df<-t_test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))

print("Effect Size:")
print(round(r, 3))
