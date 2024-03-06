# Load the women dataset
data(women)

# Display the structure of the women dataset
str(women)

# Create a factor corresponding to the height variable
height_factor <- cut(women$height, breaks = c(50, 60, 70, 80), labels = c("Short", "Average", "Tall"))

# Display the first few rows of the factor
head(height_factor)