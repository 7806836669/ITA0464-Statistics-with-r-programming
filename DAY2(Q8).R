# Create two vectors
vec1 <- c(1, 2, 3)
vec2 <- c(4, 5, 6)

# Create an array with three columns, three rows, and two "tables" using the two vectors
arr <- array(c(vec1, vec2), dim = c(3, 3, 2))

# Print the array
print(arr)