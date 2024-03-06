# Existing data frame
exam_data <- data.frame(
  name = c('John', 'Jane', 'Mike'),
  score = c(8, 7, 9),
  attempts = c(2, 1, 3),
  qualify = c('yes', 'no', 'yes')
)

# Display existing data frame
cat("Existing Data Frame:\n")
print(exam_data)

# (a) Add new row(s) to the existing data frame
new_exam_data <- data.frame(
  name = c('Robert', 'Sophia'),
  score = c(10.5, 9),
  attempts = c(1, 3),
  qualify = c('yes', 'no')
)

exam_data <- rbind(exam_data, new_exam_data)

# Display data frame after adding new row(s)
cat("\nData Frame after adding new row(s):\n")
print(exam_data)

# (b) Sort the data frame by name and score
exam_data_sorted <- exam_data[order(exam_data$name, exam_data$score), ]

# Display sorted data frame
cat("\nData Frame Sorted by 'name' and 'score':\n")
print(exam_data_sorted)

# (c) Save the information of a data frame to a file
write.csv(exam_data_sorted, file = "exam_data_sorted.csv", row.names = FALSE)

# (d) Display the information of the saved file
read_exam_data <- read.csv("exam_data_sorted.csv")
cat("\nData Frame Information from the Saved File:\n")
print(read_exam_data)