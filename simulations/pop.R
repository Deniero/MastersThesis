library(here)
library(ggplot2)


# Settings
num_of_gen = 30


# gobal variables
best_settings <- data.frame()


# load all pops
pop_32 = read.csv(here('MastersThesis/simulations/population/32.csv'))[,0:30]
pop_48 = read.csv(here('MastersThesis/simulations/population/32.csv'))[,0:30]
pop_64 = read.csv(here('MastersThesis/simulations/population/32.csv'))[,0:30]
pop_96 = read.csv(here('MastersThesis/simulations/population/32.csv'))[,0:30]


#### POP 32 ####
# get the last generation
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
colnames(transposed_results) <- c("A", "B", "C", "D", "E", "F", "G", "H")

# get best setting
mean_values <- colMeans(transposed_results)
best_setting <- which.min(mean_values)
best_settings <- data.frame(POP32 = transposed_results[, best_setting])

# Plot
matplot(1:ncol(transposed_results), t(transposed_results), type = 'p', pch = 16, col = 'blue',
        xlab = 'Hyperparameter Settings', ylab = 'Cost',
        main = 'Results: Popultation 32', xaxt = "n")

points(1:ncol(transposed_results), mean_values, pch = 19, col = 'red')
abline(h = mean_values[best_setting], col = 'grey', lty = 2)

# Customize the x-axis labels
axis(1, at = 1:ncol(transposed_results), labels = colnames(transposed_results), las = 1)

# Rotate x-axis labels for better readability
par(las = 1)



#### POP 48 ####
# get the last generation
final_results_of_reps = pop_48[0, ]
for(i in 1:nrow(pop_48)-1) {       # for-loop over columns
  if ((i + 1) %% (num_of_gen + 1) == 0) {
    final_results_of_reps <- rbind(final_results_of_reps, pop_48[i+1, ])
  }
}

# select only minimum values
columns_to_select <- c("min_0", "min_1", "min_2", "min_3", "min_4")  # Replace with the column names you want
results <- final_results_of_reps[, columns_to_select]

# transpose
transposed_results <- data.frame(t(results))
colnames(transposed_results) <- c("A", "B", "C", "D", "E", "F", "G", "H")

# get best setting
mean_values <- colMeans(transposed_results)
best_setting <- which.min(mean_values)
best_settings$POP48 <- transposed_results[, best_setting]

# Plot
matplot(1:ncol(transposed_results), t(transposed_results), type = 'p', pch = 16, col = 'blue',
        xlab = 'Hyperparameter Settings', ylab = 'Cost',
        main = 'Results: Popultation 48', xaxt = "n")

points(1:ncol(transposed_results), mean_values, pch = 19, col = 'red')
abline(h = mean_values[best_setting], col = 'grey', lty = 2)

# Customize the x-axis labels
axis(1, at = 1:ncol(transposed_results), labels = colnames(transposed_results), las = 1)

# Rotate x-axis labels for better readability
par(las = 1)


#### POP 64 ####
# get the last generation
final_results_of_reps = pop_64[0, ]
for(i in 1:nrow(pop_64)-1) {       # for-loop over columns
  if ((i + 1) %% (num_of_gen + 1) == 0) {
    final_results_of_reps <- rbind(final_results_of_reps, pop_64[i+1, ])
  }
}

# select only minimum values
columns_to_select <- c("min_0", "min_1", "min_2", "min_3", "min_4")  # Replace with the column names you want
results <- final_results_of_reps[, columns_to_select]

# transpose
transposed_results <- data.frame(t(results))
colnames(transposed_results) <- c("A", "B", "C", "D", "E", "F", "G", "H")

# get best setting
mean_values <- colMeans(transposed_results)
best_setting <- which.min(mean_values)
best_settings$POP64 <- transposed_results[, best_setting]

# Plot
matplot(1:ncol(transposed_results), t(transposed_results), type = 'p', pch = 16, col = 'blue',
        xlab = 'Hyperparameter Settings', ylab = 'Cost',
        main = 'Results: Popultation 64', xaxt = "n")

points(1:ncol(transposed_results), mean_values, pch = 19, col = 'red')
abline(h = mean_values[best_setting], col = 'grey', lty = 2)

# Customize the x-axis labels
axis(1, at = 1:ncol(transposed_results), labels = colnames(transposed_results), las = 1)

# Rotate x-axis labels for better readability
par(las = 1)


#### POP 96 ####
# get the last generation
final_results_of_reps = pop_96[0, ]
for(i in 1:nrow(pop_96)-1) {       # for-loop over columns
  if ((i + 1) %% (num_of_gen + 1) == 0) {
    final_results_of_reps <- rbind(final_results_of_reps, pop_96[i+1, ])
  }
}

# select only minimum values
columns_to_select <- c("min_0", "min_1", "min_2", "min_3", "min_4")  # Replace with the column names you want
results <- final_results_of_reps[, columns_to_select]

# transpose
transposed_results <- data.frame(t(results))
colnames(transposed_results) <- c("A", "B", "C", "D", "E", "F", "G", "H")

# get best setting
mean_values <- colMeans(transposed_results)
best_setting <- which.min(mean_values)
best_settings$POP96 <- transposed_results[, best_setting]

# Plot
matplot(1:ncol(transposed_results), t(transposed_results), type = 'p', pch = 16, col = 'blue',
        xlab = 'Hyperparameter Settings', ylab = 'Cost',
        main = 'Results: Popultation 96', xaxt = "n")

points(1:ncol(transposed_results), mean_values, pch = 19, col = 'red')
abline(h = mean_values[best_setting], col = 'grey', lty = 2)

# Customize the x-axis labels
axis(1, at = 1:ncol(transposed_results), labels = colnames(transposed_results), las = 1)

# Rotate x-axis labels for better readability
par(las = 1)



##### Best Setting ####
# get best setting
mean_values <- colMeans(best_settings)
best_setting <- which.min(mean_values)

# Plot
matplot(1:ncol(best_settings), t(best_settings), type = 'p', pch = 16, col = 'blue',
        xlab = 'Populations', ylab = 'Cost',
        main = 'Best result per Population', xaxt = "n")

points(1:ncol(best_settings), mean_values, pch = 19, col = 'red')
abline(h = mean_values[best_setting], col = 'grey', lty = 2)

# Customize the x-axis labels
axis(1, at = 1:ncol(best_settings), labels = colnames(best_settings), las = 1)

# Rotate x-axis labels for better readability
par(las = 1)


