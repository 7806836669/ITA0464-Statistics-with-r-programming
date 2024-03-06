# Given columns, rows, and tables
columns <- 4
rows <- 3
tables <- 2

# Create sample data
data1 <- matrix(1:12, nrow = rows, ncol = columns)
data2 <- matrix(13:24, nrow = rows, ncol = columns)

# Combine data into an array
array <- array(c(data1, data2), dim = c(rows, columns, tables))

# Display the content of the array
print(array)
