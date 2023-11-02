library(here)
library(ggplot2)


# Settings
num_of_gen = 30

#taguchi_results_part_1_data = read.csv(here('MastersThesis/simulations/population/32.csv'))[,0:30]
#taguchi_results_part_2_data = read.csv(here('MastersThesis/simulations/population/64.csv'))[,0:30]


taguchi.results = taguchi_results_part_1_data[0, ]
for(i in 1:nrow(taguchi_results_part_1_data)-1) {       # for-loop over columns
  if ((i + 1) %% (num_of_gen + 1) == 0) {
    taguchi.results <- rbind(taguchi.results, taguchi_results_part_1_data[i+1, ])
  }
}


taguchi.results <- rbind(taguchi.results, taguchi_results_part_2_data[0, ])
for(i in 1:nrow(taguchi_results_part_2_data)-1) {       # for-loop over columns
  if ((i + 1) %% (num_of_gen + 1) == 0) {
    taguchi.results <- rbind(taguchi.results, taguchi_results_part_2_data[i+1, ])
  }
}

columns_to_select <- c("min_0", "min_1", "min_2", "min_3", "min_4")  # Replace with the column names you want
taguchi.results <- taguchi.results[, columns_to_select]
row.names(taguchi.results) <- c(1:16)

taguchi.factor_array <- data.frame(
  A = c (1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 
  B = c (1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4),
  C = c (1,2,3,4,2,1,4,3,3,4,1,2,4,3,2,1),
  D = c (1,1,2,2,2,2,1,1,2,2,1,1,1,1,2,2),
  E = c (1,2,1,2,1,2,1,2,2,1,2,1,2,1,2,1),
  F = c (1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1),
  G = c (1,2,1,2,2,1,2,1,2,1,2,1,1,2,1,2)
)

taguchi.interaction_array <- data.frame(
  FG = c (1,2,2,1,1,2,2,1,2,1,1,2,2,1,1,2), 
  DE = c (1,2,2,1,2,1,1,2,1,2,2,1,2,1,1,2)
)


# Main Effects (taken from design for six sigma (starting p 426))
taguchi.main_effects <- t(data.frame(
  A = rep(NA, 4),
  B = rep(NA, 4),
  C = rep(NA, 4),
  D = rep(NA, 4),
  E = rep(NA, 4),
  F = rep(NA, 4),
  G = rep(NA, 4)
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
  filtered_rows <- taguchi.factor_array[taguchi.factor_array$D == level1 & taguchi.factor_array$E == level2, ]
  if(!interaction_de) {
    filtered_rows <- taguchi.factor_array[taguchi.factor_array$F == level1 & taguchi.factor_array$G == level2, ]
  }
  print(filtered_rows)
  row_mean <- 0
  
  for (i in 1:nrow(filtered_rows)) {
    row_number <- as.integer(rownames(filtered_rows)[i])
    row_data <- taguchi.results[row_number, ]
    row_mean <- row_mean + rowMeans(row_data)
  }
  row_mean <- row_mean/4
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
# Calculate the row-wise mean of df2
taguchi.results_means <- rowMeans(taguchi.results)

# Create a new dataframe by combining df1 and the calculated means
taguchi.combined <- cbind(taguchi.factor_array, results = taguchi.results_means)

# Print the new dataframe
print(taguchi.combined)


anova <- aov(results ~ factor(A) + factor(B) + factor(C) + factor(D) * factor(E) + factor(F) * factor(G), data = taguchi.combined)
summary(anova)
# TODO: Pool unimportant Factors
anova <- aov(results ~ factor(A) + factor(B) + factor(C) + factor(D) * factor(E) + factor(F) * factor(G), data = taguchi.combined)
summary(anova) # Use only this table, combined with pooled factors (Residuals can be also called All other/error (this is done in "A Primer ..."))

##### TESTING
# F is pooled, thus not used
# ANOVA TESTING - Example 9-3 from Primer on Taguchi (p.241)
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


