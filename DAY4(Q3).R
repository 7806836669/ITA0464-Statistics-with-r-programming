# Load the airquality dataset
data(airquality)

# (i) Find any missing values (NA) in features and handle them accordingly
missing_values <- colSums(is.na(airquality))
na_threshold <- 0.1 * nrow(airquality)

for (feature in names(airquality)) {
  if (missing_values[feature] > 0) {
    if (missing_values[feature] < na_threshold) {
      airquality <- na.omit(airquality)
      cat("Dropped missing values for feature:", feature, "\n")
    } else {
      mean_value <- mean(airquality[[feature]], na.rm = TRUE)
      airquality[[feature]][is.na(airquality[[feature]])] <- mean_value
      cat("Replaced missing values with mean for feature:", feature, "\n")
    }
  }
}

# (ii) Apply a linear regression algorithm using Least Squares Method on “Ozone” and “Solar.R”
linear_model <- lm(Ozone ~ Solar.R, data = airquality)

# (iii) Plot Scatter plot between Ozone and Solar and add regression line created by above model
plot(airquality$Solar.R, airquality$Ozone, xlab = "Solar Radiation (Langley)", ylab = "Ozone Concentration (ppb)", main = "Scatter plot between Ozone and Solar.R with Regression Line")
abline(linear_model, col = "red")
