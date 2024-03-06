# Function to find factors of a given number
find_factors <- function(number) {
  factors <- c()
  for (i in 1:number) {
    if (number %% i == 0) {
      factors <- c(factors, i)
    }
  }
  return(factors)
}

# Input number
input_number <- 36  # You can change this to any number you want

# Find factors of the input number
factors <- find_factors(input_number)

# Print the factors
cat("Factors of", input_number, "are:", factors)
