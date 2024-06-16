library(tidyverse)

# Read the dataset
ratings <- read_csv('C:/Projects/University/DataUnit2/ml-latest-small/ml-latest-small/ratings.csv')
movies <- read_csv('C:/Projects/University/DataUnit2/ml-latest-small/ml-latest-small/movies.csv')

print(head(ratings))
print(head(movies))

print(summary(ratings))
print(summary(movies))

num_users <- n_distinct(ratings$userId)
num_movies <- n_distinct(ratings$movieId)

cat("Number of unique users:", num_users, "\n")
cat("Number of unique movies:", num_movies, "\n")

theme_set(theme_minimal())

ggplot(ratings, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "grey", color = "black", boundary = 0) +
  labs(title = "Distribution of Movie Ratings", 
       x = "Movie Rating", 
       y = "Number of Ratings") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  theme(panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', color = 'gray70'))
