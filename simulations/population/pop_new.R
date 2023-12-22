library(here)
library(ggplot2)
library(reshape)
library(xtable)
library(tidyr)

theme_set(theme_light())
setwd("C:/Users/u19i96/Documents/MastersThesis/simulations")

#### Settings ####
num_of_gen = 30
res_column_size = 30
res_row_size = 8


#### load data ####
raw_csv_results.32 = read.csv('population/32.csv')[,0:res_column_size]
raw_csv_results.48 = read.csv('population/48.csv')[,0:res_column_size]
raw_csv_results.64 = read.csv('population/64.csv')[,0:res_column_size]
raw_csv_results.96 = read.csv('population/96.csv')[,0:res_column_size]

parse_csv_pop_data <- function(raw_csv_results) {
  temp_data = raw_csv_results[0, ]
  for(i in 1:nrow(raw_csv_results)-1) {       # for-loop over columns
    if ((i + 1) %% (num_of_gen + 1) == 0) {
      temp_data <- rbind(temp_data, raw_csv_results[i+1, ])
    }
  }
  temp_data <- temp_data[, c("min_0", "min_1", "min_2", "min_3", "min_4")]
  rownames(temp_data) <- 1:res_row_size
  
  temp_data <- cbind(temp_data, "setting" = c("A", "B", "C", "D", "E", "F", "G", "H"))
  temp_data["setting"] <- lapply(temp_data["setting"] , factor)
  return (temp_data)
} 

results.pop32 <- parse_csv_pop_data(raw_csv_results.32)
results.pop48 <- parse_csv_pop_data(raw_csv_results.48)
results.pop64 <- parse_csv_pop_data(raw_csv_results.64)
results.pop96 <- parse_csv_pop_data(raw_csv_results.96)

# combine data
results.all <- rbind(results.pop32, results.pop48, results.pop64, results.pop96)
results.all <- cbind(results.all, "population" = c(replicate(8, "32"), replicate(8, "48"), replicate(8, "64"), replicate(8, "96")))
results.all["population"] <- lapply(results.all["population"] , factor)


#### Write means for table ####
mean_table <- results.all[c("population", "setting")]
mean_table$min_mean <- rowMeans(results.all[c("min_0", "min_1", "min_2", "min_3", "min_4")])
mean_table$min_mean <- ((-mean_table$min_mean + 3500) / 100)
wide_df <- pivot_wider(mean_table, id_cols = "setting", names_from = "population", values_from = "min_mean")
x <- xtable(wide_df)
print(x)

#### Generate Plots ####
melted<-melt(results.all, id = c("setting", "population"), measured = c("min_0", "min_1", "min_2", "min_3", "min_4"))
melted$value <- ((-melted$value + 3500) / 100)

scatter <- ggplot(melted, aes(setting, value)) + geom_point(aes(color=population)) + geom_smooth()
print(scatter)


plot.points.32 <- ggplot(melted[melted$population=="32",], aes(x=setting, y=value, color=population, group=population)) + 
  geom_point()
print(plot.points.32)


plot.points.all <- ggplot(melted, aes(x=setting, y=value, color=population, group=population)) + 
  geom_point()
print(plot.points.all)


plot.points.line <- ggplot(melted, aes(x=setting, y=value, color=population, group=population)) + 
  geom_point() + 
  stat_summary(fun.data = "mean_se", geom = "line", size = 1)
print(plot.points.line)


plot.pop_comp <- ggplot(melted, aes(x=setting, y=value, group=population)) + 
  stat_summary(fun.max = function(y) max(y), fun.min = function(y) min(y), geom = "errorbar", size = 0.5, color="#457b9d") +
  stat_summary(fun.y = "mean", geom = "line", size = 1.1, color="#457b9d")  +
  facet_wrap(~population, ncol = 4) +
  labs(x = "Settings", y = "Cummulated Emergency Break Duration")
print(plot.pop_comp)
#ggsave("population/plots/comparison.jpg", plot = plot.pop_comp, width = 25, height = 10, units = "cm", dpi = 600)
ggsave("population/plots/comparison.jpg", plot = plot.pop_comp, width = 25, height = 10.5, units = "cm", dpi = 600)

