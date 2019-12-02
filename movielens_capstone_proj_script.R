# load libraries
library(ggplot2)
library(car)
library(tidyverse)
library(caret)
library(lubridate)
library(stringr)

# load dataset
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- read.table(
  text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
  col.names = c("userId", "movieId", "rating", "timestamp")
)

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(
  movieId = as.numeric(levels(movieId))[movieId],
  title = as.character(title),
  genres = as.character(genres)
)

movielens <- left_join(ratings, movies, by = "movieId")
# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# preparing dataset

# test N/A

colSums(is.na(edx))
# converting timestamp to year

edx <- mutate(edx, UserTag_year = year(as_datetime(timestamp)))

# extracting "year" from "title" data field

edx <- mutate(edx, movie_year = as.numeric(str_sub(title, -5, -2)))


# exploring dataset

str(edx)
# check for duplicates/double elements and comparing with metadata

# metadata option. Users were selected at random for inclusion. All users selected had rated at least 20 movies. Each user is represented by an id, and no other information is provided.
# Checking dataset whether it includes users below metadata limit option or not
uid <- edx %>%
  group_by(userId) %>%
  summarize(number = n(), min = min(rating), max = max(rating)) %>%
  arrange(desc(number)) %>%
  filter(number <= 19)
nrow(uid)
# this part of dataset does not accord the metadata option (more than 20 ratings per user).
# checking for doubles. Users who wathched same film several times.


double_watching <- edx %>%
  group_by(userId, title) %>%
  summarize(number = n(), min = min(rating), max = max(rating)) %>%
  arrange(desc(number)) %>%
  filter(number >= 2)
nrow(double_watching)

# test for duplicate movieId


dupl_title_movieId <- edx %>%
  group_by(title) %>%
  summarize(min = min(movieId), max = max(movieId)) %>%
  filter(min != max)
head(dupl_title_movieId)
# check timestamp inconsistencies

edx <- edx %>% mutate(Tag_year_delay = UserTag_year - movie_year)
nrow(filter(edx, edx$Tag_year_delay <= -1))
head(filter(edx, edx$Tag_year_delay <= -1))
# observing dataset

head(edx)
summary(edx)
str(edx)
# user activity characteristics

head(edx %>% group_by(userId) %>% summarize(number = n(), min = min(rating), max = max(rating)) %>% arrange(desc(number)))

edx %>% ggplot(aes(rating)) +
  geom_histogram() +
  labs(
    title = "Rating distribution",
    x = "Rating",
    y = "number_of_rating"
  )

edx %>%
  count(userId) %>%
  ggplot(aes(x = n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of users") +
  ylab("Number of ratings") +
  ggtitle("Number of ratings per user")

edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")

# Unique elements of dataset
edx %>%
  summarize(
    dist_Users = n_distinct(userId),
    dist_Movies = n_distinct(movieId),
    dist_Genres = n_distinct(genres)
  )

edx %>% ggplot(aes(movie_year)) +
  geom_histogram(fill = "black") +
  labs(
    title = "Year distribution",
    subtitle = "Rates by year",
    x = "Year",
    y = "Number of ratings"
  )

edx %>% ggplot(aes(Tag_year_delay)) +
  geom_histogram(fill = "black") +
  labs(
    title = "Delay distribution",
    subtitle = "Rates by delay",
    x = "Tag_year_delay",
    y = "Number of ratings"
  )

# Regression model

# avg movie effect

# overall mean rating
mu <- mean(edx$rating)

#  b_i on the training set
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# predicted ratings
predicted_ratings_bi <- mu + validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  .$b_i


# avg movie + user effect

#  b_u on the training set
user_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# predicted ratings
predicted_ratings_bu <- validation %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred


# avg movie + user + time effect

# extracting year of given rating from timestamp on validation dataset


validation1 <- validation %>%
  mutate(UserTag_year = year(as_datetime(timestamp)))

# Time effects ( b_t) on the training set
UserTag_year_avgs <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(UserTag_year) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u))

# predicted ratings
predicted_ratings_bt <- validation1 %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(UserTag_year_avgs, by = "UserTag_year") %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  .$pred

# calculating RMSE for current models

rmse_1 <- RMSE(validation$rating, predicted_ratings_bi)
rmse_1


rmse_2 <- RMSE(validation$rating, predicted_ratings_bu)
rmse_2


rmse_3 <- RMSE(validation$rating, predicted_ratings_bt)
rmse_3


# regularization

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l) {
  mu_reg <- mean(edx$rating)

  b_i_reg <- edx %>%
    group_by(movieId) %>%
    summarize(b_i_reg = sum(rating - mu_reg) / (n() + l))

  b_u_reg <- edx %>%
    left_join(b_i_reg, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - b_i_reg - mu_reg) / (n() + l))

  predicted_ratings_b_i_u <-
    validation %>%
    left_join(b_i_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(pred = mu_reg + b_i_reg + b_u_reg) %>%
    .$pred

  return(RMSE(validation$rating, predicted_ratings_b_i_u))
})


qplot(lambdas, rmses)

rmse_model4 <- min(rmses)
rmse_model4
# Conclusion.This MovieLens project was examined  to predict movie rating. The model evaluation performance through the RMSE ( root mean squared error) showed that the Linear regression model with regularized effects on users and movies are useful to predict ratings on the validation set. Current model effeciency approximately 0.86499
