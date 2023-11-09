library(ggplot2)
library(dplyr)
library(reshape)
library(xtable)


setwd("C:/Users/u19i96/Documents/MastersThesis/simulations")


raw_csv_results.32 = read.csv('population/32.csv')[,0:30]
raw_csv_results.64 = read.csv('population/64.csv')[,0:30]

num_of_gen = 30
res_col_names <- c("min_0", "min_1", "min_2", "min_3", "min_4")
res_row_size <- 8


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


tag.res <- rbind(parse_csv_tag_data(raw_csv_results.32), parse_csv_tag_data(raw_csv_results.64))


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
tag.factor_array.col_names <- names(tag.factor_array)


# Calc Main Effects
main_effects <- data.frame(factor_name = c(), level = c(), main_effect = c())
for (factor_name in tag.factor_array.col_names) {
  main_effect <- tag.res.factor[, c(factor_name, res_col_names)] %>%
    group_by(across(all_of(factor_name))) %>%
    summarise_all(mean)
  
  main_effect <- main_effect %>%
    rowwise() %>%
    mutate(mean_value = mean(c_across(where(is.numeric))))
  
  tmp <- data.frame(factor_name = c(replicate(nlevels(tag.res.factor[, factor_name]), factor_name)), 
                                    level = main_effect[, factor_name], 
                                    mean_value = main_effect$mean_value)
  colnames(tmp) <- c("factor_name", "level", "main_effect")
  main_effects <- rbind(main_effects, tmp)
}
main_effects["factor_name"] <- lapply(main_effects["factor_name"] , factor)
main_effects["level"] <- lapply(main_effects["level"] , factor)


theme_set(theme_light())
plot.main_effects <- ggplot(main_effects, aes(x=level, y=main_effect, group=factor_name)) + 
  geom_line(size=1.0, color="#457b9d") +
  facet_wrap(~factor_name, scales = "free", ncol = 4) +
  labs(x = "", y = "")

print(plot.main_effects)
ggsave("taguchi/plots/main_effects.jpg", plot = plot.main_effects, width = 18, height = 10, units = "cm", dpi = 600)



# Calc Interaction Effects
get_interaction_effect <- function (interaction_factor_1, interaction_factor_2) {
  interaction <- tag.res.factor[, c(interaction_factor_1, interaction_factor_2, res_col_names)] %>%
    group_by(across(all_of(c(interaction_factor_1, interaction_factor_2)))) %>%
    summarise_all(mean)
  
  interaction <- interaction %>%
    rowwise() %>%
    mutate(mean_value = mean(c_across(where(is.numeric))))
  
  tmp <- data.frame(interaction = c(replicate(nlevels(tag.res.factor[, interaction_factor_1]), paste(interaction_factor_1, interaction_factor_2))),
                    factor_1_level = interaction[, interaction_factor_1], 
                    factor_2_level = interaction[, interaction_factor_2], 
                    mean_value = interaction$mean_value)
  tmp["interaction"] <- lapply(tmp["interaction"] , factor)
  colnames(tmp) <- c("interaction", "factor_1_level", "factor_2_level", "interaction_effect")
  return (tmp)
}

interaction_effects <- rbind(get_interaction_effect("D", "E"), get_interaction_effect("F", "G"))


plot.interaction_effects <- ggplot(interaction_effects, aes(x=factor_2_level, y=interaction_effect, group=factor_1_level, color=factor_1_level)) + 
  geom_line(size=1.0) +
  facet_wrap(~interaction, scales = "free", ncol = 2) +
  labs(x = "", y = "", color = "Interactions")
print(plot.interaction_effects)
ggsave("taguchi/plots/interaction_effects.jpg", plot = plot.interaction_effects, width = 18, height = 8, units = "cm", dpi = 600)


# ANOVA
tag.combined <- cbind(tag.factor_array, tag.res)
# change to long format
tag.combined<-melt(tag.combined, id = c(names(tag.factor_array)), measured = c(names(tag.res)))

anova <- aov(value ~ A + B + C + D * E + F * G, data = tag.combined)
summary(anova)
# TODO: Pool unimportant Factors
anova <- aov(value ~ A + B + C + D * E + F * G, data = tag.combined)
summary(anova) # Use only this table, combined with pooled factors (Residuals can be also called All other/error (this is done in "A Primer ..."))


# print Table
x <- xtable(tag.res)
digits(x) <- 0
print(x)
