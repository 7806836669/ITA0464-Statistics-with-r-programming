# Function to get unique elements of a string
get_unique_characters <- function(string) {
  unique_chars <- unique(strsplit(string, "")[[1]])
  return(unique_chars)
}

# Function to get unique numbers of a vector
get_unique_numbers <- function(vector) {
  unique_numbers <- unique(vector)
  return(unique_numbers)
}

# Example usage
input_string <- "hello world"
unique_chars <- get_unique_characters(input_string)
print(paste("Unique characters in the string:", paste(unique_chars, collapse = ", ")))

input_vector <- c(1, 2, 3, 4, 1, 2, 5, 6, 3, 7, 8, 9, 4, 5)
unique_numbers <- get_unique_numbers(input_vector)
print(paste("Unique numbers in the vector:", paste(unique_numbers, collapse = ", ")))
