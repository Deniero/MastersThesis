library(here)
library(ggplot2)

# Example 6-5 


taguchi.results <- taguchi.factor_array <- data.frame(
  min_1 = c (42, 50, 36, 45, 35, 55, 30, 54),
  min_2 = c (42, 50, 36, 45, 35, 55, 30, 54) 
)
row.names(taguchi.results) <- c(1:8)


taguchi.results <- taguchi.factor_array <- data.frame(
  min_1 = c (38, 45, 38, 55, 30, 65, 40, 58),
  min_2 = c (42, 50, 36, 45, 35, 55, 30, 54),
  min_3 = c (46, 55, 34, 35, 40, 45, 20, 50) 
)
row.names(taguchi.results) <- c(1:8)



taguchi.factor_array <- data.frame(
  A = c (1,1,1,1,2,2,2,2), 
  B = c (1,2,1,2,1,2,1,2),
  C = c (1,1,2,2,1,1,2,2),
  D = c (1,2,1,2,2,1,2,1),
  E = c (1,2,2,1,2,1,1,2)
)

taguchi.interaction_array <- data.frame(
  AC = c (1,1,2,2,2,2,1,1), 
  BC = c (1,2,2,1,1,2,2,1)
)


# Main Effects (taken from design for six sigma (starting p 426))
taguchi.main_effects <- t(data.frame(
  A = rep(NA, 4),
  B = rep(NA, 4),
  C = rep(NA, 4),
  D = rep(NA, 4),
  E = rep(NA, 4)
))
colnames(taguchi.main_effects) <- c(1,2,3,4)

for (col_name in colnames(taguchi.factor_array)) {
  column <- taguchi.factor_array[[col_name]]
  level_array <- c(1,2)
  if (is.element(4, column)) {
    level_array <- c(1,2,3,4)
  }
  
  for (value in level_array) {
    rows <- which(taguchi.factor_array[[col_name]] == value)
    
    for (i in 1:length(rows)) {
      row_number <- rows[i]
      row_data <- taguchi.results[row_number, ]
      if(is.na(taguchi.main_effects[col_name,value])) {
        taguchi.main_effects[col_name,value] <- rowMeans(row_data)
      }
      else {
        taguchi.main_effects[col_name,value] <- taguchi.main_effects[col_name,value] + rowMeans(row_data)
      }
    }
    taguchi.main_effects[col_name,value] <- taguchi.main_effects[col_name,value] / (length(column) / max(column))
  }
}

# Create line charts for rows with all 4 columns filled
for (i in 1:nrow(taguchi.main_effects)) {
  row_data <- taguchi.main_effects[i, ]
  row_name <- rownames(taguchi.main_effects)[i]
  
  # Create a data frame for the plot
  if (all(!is.na(row_data))) {
    df <- data.frame(x = 1:4, y = row_data)
    
    gg <- ggplot(df, aes(x, y)) +
      geom_line() +
      scale_x_continuous(breaks = 1:4, labels = as.character(1:4)) +
      ggtitle(paste("Factor", row_name))
    
    print(gg)  # Use print to display the plot
  }
  else {
    df <- data.frame(x = 1:2, y = row_data[1:2])
    
    gg <- ggplot(df, aes(x, y)) +
      geom_line() +
      scale_x_continuous(breaks = 1:2, labels = as.character(1:2)) +
      ggtitle(paste("Factor", row_name))
    
    print(gg)  # Use print to display the plot
  }
}


mean_of_interaction <- function(level1, level2, interaction_de) {
  filtered_rows <- taguchi.factor_array[taguchi.factor_array$A == level1 & taguchi.factor_array$C == level2, ]
  if(!interaction_de) {
    filtered_rows <- taguchi.factor_array[taguchi.factor_array$B == level1 & taguchi.factor_array$C == level2, ]
  }
  print(filtered_rows)
  row_mean <- 0
  
  for (i in 1:nrow(filtered_rows)) {
    row_number <- as.integer(rownames(filtered_rows)[i])
    row_data <- taguchi.results[row_number, ]
    row_mean <- row_mean + rowMeans(row_data)
  }
  row_mean <- row_mean/nrow(filtered_rows)
}


##### Main Effects (taken from design for six sigma (starting p 426))
taguchi.interaction_effect_DE <- data.frame(
  D1E12 = c(mean_of_interaction(1, 1, TRUE), mean_of_interaction(1, 2, TRUE)),
  D2E12 = c(mean_of_interaction(2, 1, TRUE), mean_of_interaction(2, 2, TRUE))
)

gg <- ggplot(taguchi.interaction_effect_DE, aes(x = 1:2)) +
  geom_line(aes(y = D1E12), color = "blue") +
  geom_line(aes(y = D2E12), color = "red") +
  scale_x_continuous(breaks = 1:2, labels = as.character(1:2)) +
  ggtitle("Interaction DE")
print(gg)

taguchi.interaction_effect_FG <- data.frame(
  F1G12 = c(mean_of_interaction(1, 1, FALSE), mean_of_interaction(1, 2, FALSE)),
  F2G12 = c(mean_of_interaction(2, 1, FALSE), mean_of_interaction(2, 2, FALSE))
)

gg <- ggplot(taguchi.interaction_effect_FG, aes(x = 1:2)) +
  geom_line(aes(y = F1G12), color = "blue") +
  geom_line(aes(y = F2G12), color = "red") +
  scale_x_continuous(breaks = 1:2, labels = as.character(1:2)) +
  ggtitle("Interaction FG")
print(gg)


# ANOVA
# Create a new dataframe by combining df1 and the calculated means
#taguchi.results_means <- rowMeans(taguchi.results)
#taguchi.combined <- cbind(taguchi.factor_array, results = taguchi.results_means)


taguchi.combined <- cbind(taguchi.factor_array, taguchi.results)
taguchi.combined_pivoted <- taguchi.combined %>% pivot_longer(cols = starts_with("min_"), values_to = "results")


anova <- aov(results ~ factor(A) * factor(C) + factor(B) * factor(C) + factor(D) + factor(E), data = taguchi.combined_pivoted)
summary(anova)
# TODO: Pool unimportant Factors
anova <- aov(results ~factor(C) + factor(B) + factor(D), data = taguchi.combined_pivoted)
summary(anova) # Use only this table, combined with pooled factors (Residuals can be also called All other/error (this is done in "A Primer ..."))





##### OTHER TESTING
##### TESTING
# F is pooled, thus not used
# ANOVA TESTING - Example 9-3 from Primer on Taguchi (p.241)
# Percenate is calculated using Sum_Sq_a * 100/Sum Sq
testing.taguchi <- data.frame(
  A = c("1","1","2","2","3","3","4","4"),
  D = c("1","2","1","2","1","2","1","2"),
  E = c("1","2","1","2","2","1","2","1"),
  F = c("1","2","2","1","1","2","2","1"),
  G = c("1","2","2","1","2","1","1","2"),
  result = c(50.0, 62.0, 70.0, 75.0, 68.0, 65.0, 65.0, 74.0)
)
anova <- aov(result ~ A + D + E + F + G, data = testing.taguchi)
summary(anova)

# F is pooled, thus not used
anova <- aov(result ~ A + D + E + G, data = testing.taguchi)
summary(anova)
