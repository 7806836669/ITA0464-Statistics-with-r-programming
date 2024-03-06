# Set a seed for reproducibility
set.seed(123)

# Create a random sample of LETTERS and convert it to a factor
letters_factor <- as.factor(sample(LETTERS, 100, replace = TRUE))

# Display the levels of the factor
levels_before <- levels(letters_factor)
cat("Levels before extraction:", levels_before, "\n")

# Extract five levels from the factor
selected_levels <- sample(levels_before, 5)

# Create a new factor with only the selected levels
letters_factor_extracted <- factor(letters_factor, levels = selected_levels)

# Display the levels of the new factor
levels_after <- levels(letters_factor_extracted)
cat("Levels after extraction:", levels_after, "\n")