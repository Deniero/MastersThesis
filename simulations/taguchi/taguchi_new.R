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
raw_0102 = read.csv('taguchi/results/0102.csv')[,0:res_column_size]
raw_0304 = read.csv('taguchi/results/0304.csv')[,0:res_column_size]
raw_0506 = read.csv('taguchi/results/0506.csv')[,0:res_column_size]
raw_0708 = read.csv('taguchi/results/0708.csv')[,0:res_column_size]
raw_0910 = read.csv('taguchi/results/0910.csv')[,0:res_column_size]
raw_1112 = read.csv('taguchi/results/1112.csv')[,0:res_column_size]
raw_1314 = read.csv('taguchi/results/1314.csv')[,0:res_column_size]
raw_1516 = read.csv('taguchi/results/0102.csv')[,0:res_column_size]


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
  FG = as.factor(c (1,2,2,1,1,2,2,1,2,1,1,2,2,1,1,2)), 
  DE = as.factor(c (1,2,2,1,2,1,1,2,1,2,2,1,2,1,1,2))
)
tag.res.factor <- cbind(tag.factor_array, tag.res)


#### main effects ####
main_effects <- calc_effects(tag.factor_array, tag.res)

plot.main_effects <- ggplot(main_effects, aes(x=level, y=effect, group=factor_name)) + 
  geom_line(linewidth=1.0, color="#457b9d") +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  facet_wrap(~factor_name, scales = "free", ncol = 4) +
  labs(x = "", y = "")

print(plot.main_effects)
ggsave("taguchi/plots/main_effects.jpg", plot = plot.main_effects, width = 18, height = 10, units = "cm", dpi = 600)



#### interaction effects ####
interaction_effects <- calc_effects(tag.interaction_array, tag.res)

plot.interaction_effects <- ggplot(interaction_effects, aes(x=level, y=effect, group=factor_name)) + 
  geom_line(linewidth=1.0, color="#457b9d") +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  facet_wrap(~factor_name, scales = "free", ncol = 4) +
  labs(x = "", y = "")

print(plot.interaction_effects)
ggsave("taguchi/plots/interaction_effects.jpg", plot = plot.interaction_effects, width = 18, height = 6, units = "cm", dpi = 600)

# Calc Test of interaction
test_of_interaction <- rbind(get_test_of_interaction(tag.res.factor, "D", "E"), get_test_of_interaction(tag.res.factor, "F", "G"))

plot.test_of_interaction <- ggplot(test_of_interaction, aes(x=factor_2_level, y=interaction_effect, group=factor_1_level, color=factor_1_level)) + 
  geom_line(linewidth=1.0) +
  facet_wrap(~interaction, scales = "free", ncol = 2) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  labs(x = "", y = "", color = "Interactions")
print(plot.test_of_interaction)
ggsave("taguchi/plots/test_of_interaction.jpg", plot = plot.test_of_interaction, width = 18, height = 6, units = "cm", dpi = 600)


#### ANOVA ####
# change to long format
tag.res.factor.melted<-melt(tag.res.factor, id = c(names(tag.factor_array)), measured = c(names(tag.res)))

anova <- aov(value ~ A + B + C + D * E + F * G, data = tag.res.factor.melted)
summary(anova)
# TODO: Pool unimportant Factors
anova <- aov(value ~ A + B + C + D * E + F * G, data = tag.res.factor.melted)
summary(anova) # Use only this table, combined with pooled factors (Residuals can be also called All other/error (this is done in "A Primer ..."))



#### More stuff ####
# print Table
x <- xtable(tag.res)
digits(x) <- 0
print(x)
