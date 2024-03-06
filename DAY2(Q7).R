# Create a vector of values
values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

# Create a vector of dimensions
dimensions <- c(2, 3, 2)  # This will create a 3D array with dimensions 2x3x2

# Provide names for each dimension
dimnames_list <- list(
  c("Row1", "Row2"),
  c("Col1", "Col2", "Col3"),
  c("Depth1", "Depth2")
)

# Create the array
my_array <- array(values, dim = dimensions, dimnames = dimnames_list)

# Display the created array
print("Created Array:")
print(my_array)