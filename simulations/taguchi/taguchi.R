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
  row_name <- rownames(t_main_effects)[i]
    
  # Create a data frame for the plot
  if (all(!is.na(row_data))) {
    df <- data.frame(x = 1:4, y = row_data)
    
    gg <- ggplot(df, aes(x, y)) +
      geom_line() +
      scale_x_continuous(breaks = 1:4, labels = as.character(1:4)) +
      ggtitle(paste("Row", row_name))
    
    print(gg)  # Use print to display the plot
  }
  else {
    df <- data.frame(x = 1:2, y = row_data[1:2])
    
    gg <- ggplot(df, aes(x, y)) +
      geom_line() +
      scale_x_continuous(breaks = 1:2, labels = as.character(1:2)) +
      ggtitle(paste("Row", row_name))
    
    print(gg)  # Use print to display the plot
  }
}

