# Given two vectors
vector1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
vector2 <- c(9, 8, 7, 6, 5, 4, 3, 2, 1)

# Reshape vectors into matrices
matrix1 <- matrix(vector1, nrow = 3, ncol = 3, byrow = TRUE)
matrix2 <- matrix(vector2, nrow = 3, ncol = 3, byrow = TRUE)

# Print the second row of the second matrix
cat("Second row of the second matrix:\n")
print(matrix2[2, ])


# Print the element in the 3rd row and 3rd column of the 1st matrix
cat("\nElement in the 3rd row and 3rd column of the 1st matrix:", matrix1[3, 3])