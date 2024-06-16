# Load required libraries
library(recommenderlab)
library(reshape2)

# Load the Movielens dataset
ratings <- read.csv("ml-latest-small/ratings.csv")

# Create a sparse rating matrix
ratings_matrix <- dcast(ratings, userId ~ movieId, value.var = "rating")
ratings_matrix <- as.matrix(ratings_matrix[,-1])
ratings_matrix[is.na(ratings_matrix)] <- 0
ratings_matrix <- as(ratings_matrix, "realRatingMatrix")

# Split the dataset into training and test sets
set.seed(42)
train_indices <- sample(x = c(TRUE, FALSE), size = nrow(ratings_matrix), replace = TRUE, prob = c(0.75, 0.25))
train_data <- ratings_matrix[train_indices, ]
test_data <- ratings_matrix[!train_indices, ]

# Model training using User-Based Collaborative Filtering
model <- Recommender(train_data, method = "UBCF")

# Make predictions on the test set
predictions <- predict(model, test_data, type = "ratings")

# Evaluate the model
calcRMSE <- function(real, predicted) {
  sqrt(mean((real - predicted) ^ 2, na.rm = TRUE))
}

# Get the real and predicted ratings
real_ratings <- as(test_data, "matrix")
predicted_ratings <- as(predictions, "matrix")

# Calculate RMSE
rmse <- calcRMSE(real_ratings, predicted_ratings)
print(paste("RMSE:", round(rmse, 4)))

# Example of making predictions for a specific user
user_index <- 1  # Example user index
user_predictions <- as(predict(model, ratings_matrix[user_index, ], type = "ratings"), "list")
print(paste("Predicted ratings for user", user_index, ":", user_predictions))
