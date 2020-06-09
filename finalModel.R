# compute the RMSE of our final model on the validation set
# THIS IS NOT MY TRAINING SCRIPT. My report shows how I trained and tested the model. This script just uses the final methods in the model amd computes the final RMSE.
library(tidyverse)
library(lubridate)
library(recosystem)
library(caret)
library(dplyr)
edx <- readRDS('./edx.rds')
validation <- readRDS('./validation.rds')

set.seed(500)
# create test set and train set out of edx
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# only include observations in the test set with movie and user in the train set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2))
}

# function that takes lambda as an argument and computes the regularized movie effect for each movie
get_movie_effect_reg <- function(l) {
  train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
}

# find best lambda for movie effect
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l) {
  movie_avgs <- get_movie_effect_reg(l)
  predicted_ratings <- test_set %>%
    left_join(movie_avgs, by="movieId") %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  RMSE(predicted_ratings, test_set$rating)
})
# best lambda for calculating movie effect
b_i_lambda <- lambdas[which.min(rmses)]

# save regularized movie biases
movie_avgs_reg <- get_movie_effect_reg(b_i_lambda)


# given lambda, compute regularized user effect for each user
get_user_avgs_reg <- function(l) {
  train_set %>%
    left_join(movie_avgs_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
}
# find best lambda for user effect regularization
rmses <- sapply(lambdas, function(l) {
  user_avgs <- get_user_avgs_reg(l)
  predicted_ratings <- test_set %>%
    left_join(movie_avgs_reg, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  RMSE(predicted_ratings, test_set$rating)
})

# save best lambda
b_u_lambda <- lambdas[which.min(rmses)]
# save regularized user effects using the best lambda
user_avgs_reg <- get_user_avgs_reg(b_u_lambda)

# compute residuals for test set
train_set <- train_set %>%
  left_join(user_avgs_reg, by="userId") %>%
  left_join(movie_avgs_reg, by="movieId") %>%
  mutate(res = rating - mu - b_i - b_u)

# specify data set for recosystem's DataSource
train_data <- data_memory(user_index = train_set$userId, item_index = train_set$movieId, 
                          rating = train_set$res, index1 = TRUE)

# create model object
r = Reco()

# select best tuning parameters
set.seed(123)
opts = r$tune(train_data, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 1, niter = 10))
# train
set.seed(123)
r$train(train_data, opts = c(opts$min, nthread = 1, niter = 10))

# add b_i's and b_u's to the validation set
validation <- validation %>%
  left_join(user_avgs_reg, by="userId") %>%
  left_join(movie_avgs_reg, by="movieId") %>%
  setNames(gsub('\\.x$', '', names(.))) %>%
  mutate(b_i = ifelse(is.na(b_i), 0, b_i), b_u = ifelse(is.na(b_u), 0, b_u))

# make validation data a DataSource for recosystem
validation_data <- data_memory(user_index = validation$userId, item_index = validation$movieId, index1 = TRUE)

# predict residuals for validation and add them to the mean and user and movie effects
predicted_ratings <- r$predict(validation_data, out_memory()) + mu + validation$b_i + validation$b_u

# change predictions over max to the max
ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5

# change predictions under min to the min
ind <- which(predicted_ratings < 1 & validation$timestamp < half_rating_start)
predicted_ratings[ind] <- 1

ind <- which(predicted_ratings < 0.5 & validation$timestamp >= half_rating_start)
predicted_ratings[ind] <- 0.5

# compute final RMSE
RMSE(predicted_ratings, validation$rating)
