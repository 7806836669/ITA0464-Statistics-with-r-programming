# Call the built-in dataset 'airquality'
data(airquality)

# Check if it is a data frame
is_data_frame <- is.data.frame(airquality)
cat("Is 'airquality' a data frame? ", is_data_frame, "\n")

# Order the entire data frame by the first and second column
airquality_ordered <- airquality[order(airquality$Ozone, airquality$Solar.R), ]

# Display the ordered data frame
cat("Ordered Data Frame by 'Ozone' and 'Solar.R':\n")
print(airquality_ordered)

# Remove the variables 'Solar.R' and 'Wind'
airquality_subset <- airquality[, !(names(airquality) %in% c('Solar.R', 'Wind'))]

# Display the modified data framea
cat("\nData Frame after removing 'Solar.R' and 'Wind':\n")
print(airquality_subset)