library(here)
library(ggplot2)

#pop_32 = read.csv(here('MastersThesis/simulations/population/32.csv'))[,0:30]

#print(head(pop_32))

# get the last generation
num_of_gen = 30
final_results_of_reps = pop_32[0, ]
for(i in 1:nrow(pop_32)-1) {       # for-loop over columns
  if ((i + 1) %% (num_of_gen + 1) == 0) {
    final_results_of_reps <- rbind(final_results_of_reps, pop_32[i+1, ])
  }
}

# select only minimum values
columns_to_select <- c("min_0", "min_1", "min_2", "min_3", "min_4")  # Replace with the column names you want
results <- final_results_of_reps[, columns_to_select]

# transpose
transposed_results <- data.frame(t(results))

# get best setting
mean_values <- colMeans(transposed_results)
best_setting <- which.min(mean_values)


# Plot
matplot(1:ncol(transposed_results), t(transposed_results), type = 'p', pch = 16, col = 'blue',
        xlab = 'Hyperparameter Settings', ylab = 'Cost',
        main = 'Results: Popultation 32')

points(1:ncol(transposed_results), mean_values, pch = 19, col = 'red')
abline(h = mean_values[best_setting], col = 'grey', lty = 2)

# Customize the x-axis labels
axis(1, at = 1:ncol(transposed_results), labels = colnames(transposed_results), las = 2)

# Rotate x-axis labels for better readability
par(las = 2)

