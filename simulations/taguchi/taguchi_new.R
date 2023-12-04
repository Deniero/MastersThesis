library(ggplot2)
library(dplyr)
library(reshape)
library(xtable)

theme_set(theme_light())
setwd("C:/Users/u19i96/Documents/MastersThesis/simulations")



#### functions ####
calc_effects <- function (factor_array, results) {
  combined <- cbind(factor_array, results)
  effects <- data.frame(factor_name = c(), level = c(), effect = c())
  for (factor_name in names(factor_array)) {
    effect <- combined[, c(factor_name, res_col_names)] %>%
      group_by(across(all_of(factor_name))) %>%
      summarise_all(mean)
    
    effect <- effect %>%
      rowwise() %>%
      mutate(mean_value = mean(c_across(where(is.numeric))))
    
    tmp <- data.frame(factor_name = c(replicate(nlevels(combined[, factor_name]), factor_name)), 
                      level = effect[, factor_name], 
                      mean_value = effect$mean_value)
    colnames(tmp) <- c("factor_name", "level", "effect")
    effects <- rbind(effects, tmp)
  }
  
  effects["factor_name"] <- lapply(effects["factor_name"] , factor)
  effects["level"] <- lapply(effects["level"] , factor)
  return(effects)
}



get_test_of_interaction <- function (combined_res_factor, interaction_factor_1, interaction_factor_2) {
  interaction <- combined_res_factor[, c(interaction_factor_1, interaction_factor_2, res_col_names)] %>%
    group_by(across(all_of(c(interaction_factor_1, interaction_factor_2)))) %>%
    summarise_all(mean)
  
  interaction <- interaction %>%
    rowwise() %>%
    mutate(mean_value = mean(c_across(where(is.numeric))))
  
  tmp <- data.frame(interaction = c(replicate(nlevels(combined_res_factor[, interaction_factor_1]), paste(interaction_factor_1, interaction_factor_2))),
                    factor_1_level = interaction[, interaction_factor_1], 
                    factor_2_level = interaction[, interaction_factor_2], 
                    mean_value = interaction$mean_value)
  tmp["interaction"] <- lapply(tmp["interaction"] , factor)
  colnames(tmp) <- c("interaction", "factor_1_level", "factor_2_level", "interaction_effect")
  return (tmp)
}



#### settings ####
num_of_gen = 30
res_row_size = 2
res_column_size = 39
res_col_names <- c("min_0", "min_1", "min_2", "min_3", "min_4", "min_5", "min_6", "min_7")


#### load data ####
# Possible to import dataset from ~/MastersThesis/simulations/taguchi/results/dataset.RData
raw_0102 = read.csv('taguchi/results/0102.csv')[,0:res_column_size]
raw_0304 = read.csv('taguchi/results/0304.csv')[,0:res_column_size]
raw_0506 = read.csv('taguchi/results/0506.csv')[,0:res_column_size]
raw_0708 = read.csv('taguchi/results/0708.csv')[,0:res_column_size]
raw_0910 = read.csv('taguchi/results/0910.csv')[,0:res_column_size]
raw_1112 = read.csv('taguchi/results/1112.csv')[,0:res_column_size]
raw_1314 = read.csv('taguchi/results/1314.csv')[,0:res_column_size]
raw_1516 = read.csv('taguchi/results/1516.csv')[,0:res_column_size]


parse_csv_tag_data <- function(raw_csv_results) {
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


tag.res <- rbind(parse_csv_tag_data(raw_0102), parse_csv_tag_data(raw_0304),
                 parse_csv_tag_data(raw_0506), parse_csv_tag_data(raw_0708),
                 parse_csv_tag_data(raw_0910), parse_csv_tag_data(raw_1112),
                 parse_csv_tag_data(raw_1314), parse_csv_tag_data(raw_1516))

head(tag.res)
tag.res <- (3500 - tag.res) / 100
head(tag.res)

mean_of_results <- mean(as.matrix(tag.res), na.rm = TRUE)

# define taguchi array
tag.factor_array <- data.frame(
  A = as.factor(c (1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)), 
  B = as.factor(c (1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)),
  C = as.factor(c (1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1)),
  D = as.factor(c (1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2)),
  E = as.factor(c (1,2,1,2,1,2,1,2,2,1,2,1,2,1,2,1)),
  F = as.factor(c (1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1)),
  G = as.factor(c (1,2,1,2,2,1,2,1,2,1,2,1,1,2,1,2))
)

tag.interaction_array <- data.frame(
  DE = as.factor(c (1,2,2,1,2,1,1,2,1,2,2,1,2,1,1,2))
)
tag.res.factor <- cbind(tag.factor_array, tag.res)

# print Table
x <- xtable(tag.res)
#digits(x) <- 0
print(x)


#### main effects ####
main_effects <- calc_effects(tag.factor_array, tag.res)

plot.main_effects <- ggplot(main_effects, aes(x=level, y=effect, group=factor_name)) + 
  geom_line(linewidth=1.0, color="#457b9d") +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  facet_wrap(~factor_name, scales = "free", ncol = 4) +
  labs(x = "", y = "")

print(plot.main_effects)
ggsave("taguchi/plots/main_effects.jpg", plot = plot.main_effects, width = 18, height = 10, units = "cm", dpi = 600)

# calc optimum performance without interaction DE:
main_effects.best_effects <- main_effects %>%
  group_by(factor_name) %>%
  summarize(max_effect = max(effect))
print("Predicted Optiumum performance without interaction DE")
print(sum(main_effects.best_effects$max_effect) - (mean_of_results * nrow(main_effects.best_effects)) + mean_of_results)


#### interaction effects ####
interaction_effects <- calc_effects(tag.interaction_array, tag.res)

#plot.interaction_effects <- ggplot(interaction_effects, aes(x=level, y=effect, group=factor_name)) + 
#  geom_line(linewidth=1.0, color="#457b9d") +
#  scale_x_discrete(expand = c(0.1, 0.1)) +
#  facet_wrap(~factor_name, scales = "free", ncol = 4) +
#  labs(x = "", y = "")

#print(plot.interaction_effects)
#ggsave("taguchi/plots/interaction_effects.jpg", plot = plot.interaction_effects, width = 18, height = 6, units = "cm", dpi = 600)

# Calc Test of interaction
test_of_interaction <- get_test_of_interaction(tag.res.factor, "D", "E")

# TODO: define chart like in primer page 158(164)
plot.test_of_interaction <- ggplot(test_of_interaction, aes(x=factor_2_level, y=interaction_effect, group=factor_1_level, color=factor_1_level)) + 
  geom_line(linewidth=1.0) +
  facet_wrap(~interaction, scales = "free", ncol = 2) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  labs(x = "E", y = "", color = "D")
print(plot.test_of_interaction)
ggsave("taguchi/plots/test_of_interaction.jpg", plot = plot.test_of_interaction, width = 18, height = 4, units = "cm", dpi = 600)


# calc Predicted Optiumum performance with interaction DE
combined_effects = rbind(main_effects, interaction_effects)

combined_effects_filtered <- combined_effects %>%
  filter(factor_name != "E" | level != 2) %>% filter(factor_name != "FG")

combined_effects.best_effects <- combined_effects_filtered %>%
  group_by(factor_name) %>%
  summarize(max_effect = max(effect))
print("Predicted Optiumum performance with interaction DE")
print(sum(combined_effects.best_effects$max_effect) - (mean_of_results * nrow(combined_effects.best_effects)) + mean_of_results)


#### ANOVA ####
# change to long format
tag.res.factor.melted<-melt(tag.res.factor, id = c(names(tag.factor_array)), measured = c(names(tag.res)))

anova <- aov(value ~ A + B + C + D * E + F + G, data = tag.res.factor.melted)
summary(anova)
x <- xtable(anova)
print(x)


LM <- lm(value ~ A + B + C + D * E + F + G, data = tag.res.factor.melted)
summary(LM)


percentage_contribution = anova %>% broom::tidy() %>% mutate(percentage_contribution = sumsq/sum(sumsq) * 100)
percentage_contribution["Factors"] <- lapply(percentage_contribution["term"] , factor)


plot.percentage_contribution <- ggplot(percentage_contribution, aes(y = reorder(Factors, percentage_contribution), x=percentage_contribution)) + 
  geom_bar(stat = "identity", fill="#457b9d") +
  labs(x = "", y = "Factors")

print(plot.percentage_contribution)
ggsave("taguchi/plots/percentage_contribution.jpg", plot = plot.percentage_contribution, width = 12, height = 6, units = "cm", dpi = 600)


#plot(anova)
##### WORK in progress
residuals_df <- data.frame(residuals = residuals(anova))
studen_residuals_df <- data.frame(residuals = rstudent(anova))
test3 <- ggplot(residuals_df, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ Plot of ANOVA Residuals")
#print(test3)

hist <- ggplot(residuals_df, aes(x = residuals)) +
  geom_histogram(binwidth = 1, fill = "#457b9d", color = "black", alpha = 0.7) +
  ggtitle("Histogram of ANOVA Residuals") +
  xlab("Residuals")
#print(hist)
test3 <- ggplot(studen_residuals_df, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ Plot of ANOVA Student Residuals")
#print(test3)

hist <- ggplot(studen_residuals_df, aes(x = residuals)) +
  geom_histogram(binwidth = 1, fill = "#457b9d", color = "black", alpha = 0.7) +
  ggtitle("Histogram of ANOVA Student Residuals") +
  xlab("Residuals")
#print(hist)


#### S/N ####
combined <- cbind(tag.factor_array, tag.res)
combined$msd <- apply(combined[,res_col_names], 1, function(row) sum(row^2) / length(row))
combined$s_n <- apply(combined, 1, function(row) -10 * log10(as.numeric(row[c('msd')])))


anova.s_n <- aov(s_n ~ A + B + C + D * E + F + G, data = combined)
summary(anova.s_n)
x <- xtable(anova.s_n)
print(x)
LM.s_n <- lm(s_n ~ A + B + C + D * E + F + G, data = combined)
summary(LM.s_n)
# did not result in good performance and was not further investigated.