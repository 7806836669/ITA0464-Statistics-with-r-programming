# Set seed for reproducibility
set.seed(123)

# Generate a list of random numbers with a mean of 0 and standard deviation of 1
random_numbers <- rnorm(100, mean = 0, sd = 1)

# Count occurrences of each value
occurrences <- table(random_numbers)

# Display the list of random numbers and their occurrences
print("Random Numbers:")
print(random_numbers)

print("\nOccurrences:")
print(occurrences)