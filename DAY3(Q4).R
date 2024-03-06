# Create a sequence of even integers greater than 50
sequence <- seq(from = 52, by = 2, length.out = 5*3)

# Reshape the sequence into a 5x3 array
array <- matrix(sequence, nrow = 5, ncol = 3, byrow = TRUE)

# Display the content of the array
print(array)
