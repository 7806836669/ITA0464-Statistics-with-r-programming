# Create vectors
vector1 <- c(1, 2, 3)
vector2 <- c(4, 5, 6)

# Create a matrix
matrix1 <- matrix(1:9, nrow = 3)

# Define a function
multiply_by_2 <- function(x) {
  return(x * 2)
}

# Apply the function to one of the vectors
result <- multiply_by_2(vector1)

# Create a list containing the vectors, matrix, and result of the function
my_list <- list(
  "Vector 1" = vector1,
  "Vector 2" = vector2,
  "Matrix 1" = matrix1,
  "Result of Function" = result
)

# Print the content of the list
print(my_list)
