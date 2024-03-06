# Create a 5 x 4 matrix with no labels and fill it by rows
mat5x4 <- matrix(data = c(1:20), nrow = 5, ncol = 4)

# Create a 3 x 3 matrix with labels and fill it by rows
mat3x3 <- matrix(data = c(110, 190, 330, 410, 550, 630, 770, 850, 990), nrow = 3, ncol = 3, dimnames = list(c("Row 1", "Row 2", "Row 3"), c("Col 1", "Col 2", "Col 3")))

# Create a 2 x 2 matrix with labels and fill it by columns
mat2x2 <- matrix(data = c(1, 3, 2, 4), nrow = 2, ncol = 2, dimnames = list(c("Row 1", "Row 2"), c("Col 1", "Col 2")))
mat2x2[,1] <- c(1, 2)
mat2x2[,2] <- c(3, 4)

# Print the matrices
print(mat5x4)
print(mat3x3)
print(mat2x2)