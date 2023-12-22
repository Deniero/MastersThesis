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
melted$value <- ((-melted$value + 3500) / 100)


plot.bp <- ggplot(melted, aes(value, simulation)) + 
  geom_boxplot(color="#457b9d") + 
  labs(x = "Cummulated Emergency Break Duration", y = "")
print(plot.bp)
ggsave("evaluation/plots/elite_vs_no_elite.jpg", plot = plot.bp, width = 16, height = 4, units = "cm", dpi = 1000)


#### plot over generations ####
selected_repetitions = c("min_2", "min_3", "min_8")
melted_generations_elite<-melt(sim_1.elite_min[, c("gen", selected_repetitions)], 
                         id = c("gen"), 
                         measured = selected_repetitions)
melted_generations_elite$value <- ((-melted_generations_elite$value + 3500) / 100)

levels(melted_generations_elite$variable) <- c("Rep 1", "Rep 2", "Rep 3")
plot.line.elite <- ggplot(melted_generations_elite, aes(gen, value, color=variable)) + geom_line()  +
  labs(x = "Generations", y = "Cummulated Emergency Break Duration", color = "Repetitions")
print(plot.line.elite)
#ggsave("evaluation/plots/ga_elite_generations.jpg", plot = plot.line.elite, width = 15, height = 4, units = "cm", dpi = 300)


selected_repetitions = c("min_2", "min_3", "min_8")
melted_generations_no_elite<-melt(sim_1.no_elite_min[, c("gen", selected_repetitions)], 
                         id = c("gen"), 
                         measured = selected_repetitions)
melted_generations_no_elite$value <- ((-melted_generations_no_elite$value + 3500) / 100)

levels(melted_generations_no_elite$variable) <- c("Rep 1", "Rep 2", "Rep 3")
plot.line.no_elite <- ggplot(melted_generations_no_elite, aes(gen, value, color=variable)) + geom_line()  +
  labs(x = "Generations", y = "Cummulated Emergency Break Duration", color = "Repetitions")
print(plot.line.no_elite)
#ggsave("evaluation/plots/ga_no_elite_generations.jpg", plot = plot.line.no_elite, width = 15, height = 7, units = "cm", dpi = 300)


melted_generations_no_elite$Source <- "Elite = 0"
melted_generations_elite$Source <- "Elite = 2"

melted_generations <- bind_rows(melted_generations_no_elite, melted_generations_elite)
melted_generations["Source"] <- lapply(melted_generations["Source"] , factor)

plot.line.combined <- ggplot(melted_generations, aes(gen, value, color=variable)) + geom_line()  + facet_wrap(~Source, scales = "free_x",) +
  labs(x = "Generations", y = "Cummulated Emergency Break Duration", color = "Repetitions")
print(plot.line.combined)
ggsave("evaluation/plots/elite_vs_no_elite_generations.jpg", plot = plot.line.combined, width = 16, height = 8, units = "cm", dpi = 1000)
#### T test ####

t_test<-t.test(value ~ simulation, data = melted)
print(t_test)

group_summary <- melted %>%
  group_by(simulation) %>%
  summarise(std = sd(value))

group_size <- length(res_col_names)
group_summary$se <- group_summary$std / sqrt(group_size)
print('SE:')
print(group_summary)


t<-t_test$statistic[[1]]
df<-t_test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))



print("Effect Size:")
print(round(r, 3))


