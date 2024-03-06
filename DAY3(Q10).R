# Load the Iris dataset
data(iris)

# (i) Find dimension, Structure, Summary statistics, Standard Deviation of all features
# Dimension
dim(iris)

# Structure
str(iris)

# Summary statistics
summary(iris)

# Standard Deviation of all features
apply(iris[, 1:4], 2, sd)

# (ii) Find mean and standard deviation of features grouped by three species of Iris flowers
aggregate(. ~ Species, data = iris, mean)
aggregate(. ~ Species, data = iris, sd)

# (iii) Find quantile value of sepal width and length
quantile(iris$Sepal.Width)
quantile(iris$Sepal.Length)

# (iv) Create new data frame named iris1 with a new column named Sepal.Length.Cate
iris1 <- iris
iris1$Sepal.Length.Cate <- cut(iris$Sepal.Length, breaks = quantile(iris$Sepal.Length), labels = c("Q1", "Q2", "Q3", "Q4"))

# (v) Average value of numerical variables by two categorical variables: Species and Sepal.Length.Cate
aggregate(. ~ Species + Sepal.Length.Cate, data = iris1, mean)

# (vi) Average mean value of numerical variables by Species and Sepal.Length.Cate
aggregate(. ~ Species + Sepal.Length.Cate, data = iris1, mean)

# (vii) Create Pivot Table based on Species and Sepal.Length.Cate
library(tidyr)
pivot_table <- spread(iris1, key = Sepal.Length.Cate, value = Sepal.Length)