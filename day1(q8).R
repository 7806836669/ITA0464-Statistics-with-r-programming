# R program to extract specified letters

# Extract first 10 letters in lower case
first_10_lower <- letters[1:10]

# Extract last 10 letters in upper case
last_10_upper <- toupper(letters[17:26])

# Extract letters between 22nd to 24th in upper case
letters_22_to_24_upper <- toupper(letters[22:24])

# Print the results
cat("First 10 letters in lower case:", paste(first_10_lower, collapse = " "), "\n")
cat("Last 10 letters in upper case:", paste(last_10_upper, collapse = " "), "\n")
cat("Letters between 22nd to 24th in upper case:", letters_22_to_24_upper, "\n")