library(tidyverse)
library(lubridate)
library(recosystem)
library(caret)
library(dplyr)

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

# naive model that predicts average rating everytime
mu <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu)
rmse_results <-data_frame(method = "Just the average", RMSE = naive_rmse)

# model that includes the average bias of each movie
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
# model with average user bias
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results


# regularization
train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(movieId, title) %>%
  summarize(b_i = first(b_i), n = n()) %>%
  arrange(desc(abs(b_i))) %>%
  head()



get_movie_effect_reg <- function(l) {
  train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
}

# find best lambda for movie effect using train set
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l) {
  movie_avgs <- get_movie_effect_reg(l)
  predicted_ratings <- test_set %>%
    left_join(movie_avgs, by="movieId") %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  RMSE(predicted_ratings, test_set$rating)
})
qplot(lambdas, rmses)
b_i_lambda <- lambdas[which.min(rmses)]

movie_avgs_reg <- get_movie_effect_reg(b_i_lambda)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",
                                     RMSE = min(rmses)))

# regularized user effect
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
qplot(lambdas, rmses)
b_u_lambda <- lambdas[which.min(rmses)]

user_avgs_reg <- get_user_avgs_reg(b_u_lambda)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",
                                     RMSE = min(rmses)))

# matrix factorization
set.seed(100)
userIds <- sample(unique(train_set$userId), 100)

set.seed(100)
m <- train_set %>%
  filter(userId %in% userIds) %>%
  select(userId, movieId, res) %>%
  spread(movieId, res) %>%
  select(-userId) %>%
  select(sample(ncol(.), 100)) %>%
  as.matrix()

image(m, xlab = "Movies", ylab = "Users", axes = FALSE)
axis(1, at=seq(0,1,0.2), labels=seq(0,100,length = 6))
axis(2, at=seq(0,1,0.2), labels=seq(0,100,length = 6))
grid(100,100, lty = 1)


# compute residuals for test and training set
train_set <- train_set %>%
  left_join(user_avgs_reg, by="userId") %>%
  left_join(movie_avgs_reg, by="movieId") %>%
  mutate(res = rating - mu - b_i - b_u)

test_set <- test_set %>%
  left_join(user_avgs_reg, by="userId") %>%
  left_join(movie_avgs_reg, by="movieId") %>%
  mutate(res = rating - mu - b_i - b_u)

# specify data set for recosystem's DataSource
train_data <- data_memory(user_index = train_set$userId, item_index = train_set$movieId, 
                          rating = train_set$res, index1 = TRUE)

test_data <- data_memory(user_index = test_set$userId, item_index = test_set$movieId, index1 = TRUE)

# create model object
r = Reco()

# select best tuning parameters
set.seed(123)
opts = r$tune(train_data, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))

# train
set.seed(123)
r$train(train_data,  opts = c(opts$min, nthread = 1, niter = 10))

# predict
predicted_ratings <- r$predict(test_data, out_memory()) + mu + test_set$b_i + test_set$b_u 

# change ratings over max to max
ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5

# find first half rating
half_rating_start <- train_set %>%
  filter(rating %% 1 == 0.5) %>%
  pull(timestamp) %>%
  min()

# change values below one from dates before half ratings to a one.
ind <- which(predicted_ratings < 1 & test_set$timestamp < half_rating_start)
predicted_ratings[ind] <- 1

# change values below 0.5 from dates after half ratings to a 0.5
ind <- which(predicted_ratings < 0.5 & test_set$timestamp >= half_rating_start)
predicted_ratings[ind] <- 0.5

# compute RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect + Matrix Factorization Model",
                                     RMSE = RMSE(predicted_ratings, test_set$rating)))

rmse_results %>% knitr::kable()

# add b_i's and b_u's to the validation set
validation <- validation %>%
  left_join(user_avgs_reg, by="userId") %>%
  left_join(movie_avgs_reg, by="movieId") %>%
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

#  mutate(res = rating - mu - ifelse(is.na(b_i), 0, b_i) - ifelse(is.na(b_u), 0, b_u))

# residuals <- train_set %>%
#   left_join(user_avgs_reg, by="userId") %>%
#   left_join(movie_avgs_reg, by="movieId") %>%
#   mutate(resid = rating - mu - b_i - b_u, date = as_datetime(timestamp))
# 
# residuals %>%
#   mutate(date = round_date(date, unit = "week")) %>%
#   group_by(date) %>%
#   summarize(mean_resid = mean(resid)) %>%
#   ggplot(aes(date, mean_resid)) +
#   geom_point() +
#   geom_smooth(method="loess", span=0.1) +
#   geom_vline(xintercept = as.POSIXct("1996-05-01"), color = "red")
# 
# residuals %>%
#   mutate(year = year(date)) %>%
#   group_by(year) %>%
#   summarize(n = n()) %>%
#   ggplot(aes(year, n)) +
#   geom_bar(stat="identity") +
#   scale_x_continuous(breaks = seq(1995, 2010)) 
# 
# # we don't have significant amount of reviews until april 1996
# residuals %>%
#   filter(year(date) < 1997) %>%
#   mutate(date = round_date(date, unit = "month")) %>%
#   group_by(date) %>%
#   summarize(count = n(), r = mean(resid))
# 
# residuals %>%
#   filter(date < as.POSIXct("1996-06-01")) %>%
#   mutate(date = round_date(date, unit = "week")) %>%
#   group_by(date) %>%
#   summarize(count = n(), r = mean(resid))
# 
# residuals %>%
#   filter(date > as.POSIXct("1996-03-01") & date < as.POSIXct("1996-05-28")) %>%
#   mutate(week = round_date(date, unit = "week")) %>%
#   group_by(week) %>%
#   summarize(count = n(), r = mean(resid), se = sd(resid)/sqrt(count)) %>%
#   ggplot(aes(week, r)) +
#   geom_line() +
#   geom_errorbar(aes(ymin = r - 1.96*se, ymax = r + 1.96*se)) +
#   geom_vline(xintercept = as.POSIXct("1996-04-21"))
# 
# residuals %>%
#   filter(date > as.POSIXct("1996-03-01") & date < as.POSIXct("1996-05-28")) %>%
#   mutate(day = round_date(date, unit = "day")) %>%
#   group_by(day) %>%
#   summarize(count = n(), r = mean(resid), se = sd(resid)/sqrt(count)) %>%
#   print(n= 80)
# 
# # new model = mu + b_i + b_u + beta_d*X where (X = 1 if date before 1996-04-22)
# residuals %>%
#   filter(date < as.POSIXct("1996-04-22")) %>%
#   summarize(N = n(), r = mean(resid), se = sd(resid)/sqrt(N), lower = r - 1.96 *se, upper = r + 1.96*se)
# 
# p_value <- (1 - pnorm(0.1101301, 0, 0.006219189)) * 2
# 
# beta_d <- 0.1101301
# 
# predicted_ratings <- test_set %>%
#   left_join(movie_avgs_reg, by="movieId") %>%
#   left_join(user_avgs_reg, by="userId") %>%
#   mutate(date = as_datetime(timestamp), is_old = date < as.POSIXct("1996-04-22")) %>%
#   mutate(pred = mu + b_i + b_u + ifelse(is_old, 1, 0) * beta_d) %>%
#   .$pred
# RMSE(predicted_ratings, test_set$rating)
# 
# residuals %>%
#   mutate(month = month(date)) %>%
#   group_by(month) %>%
#   summarize(r = mean(resid)) %>%
#   ggplot(aes(month, r)) +
#   geom_bar(stat = "identity")
# 
# residuals %>%
#   summarize(mean(resid), mean(b_u), mean(b_i))
# 
# residuals %>%
#   mutate(day = yday(date)) %>%
#   group_by(day) %>%
#   summarize(r = mean(resid)) %>%
#   ggplot(aes(day, r)) +
#   geom_point() +
#   geom_smooth()
# 
# fit <- residuals %>%
#   mutate(day = round_date(date, unit="day")) %>%
#   group_by(day) %>%
#   summarize(r = mean(resid)) %>%
#   train(r ~ day, data = ., method = "gamLoess", tuneGrid = expand.grid(span = seq(0.1, 0.65, len = 10), degree = 1))
# 
# ggplot(fit, highlight = TRUE)
# 
# 
# f_d <- as_datetime(test_set$timestamp) %>%
#   predict(fit, data = .)
# 
# predicted_ratings <- test_set %>%
#   left_join(movie_avgs_reg, by="movieId") %>%
#   left_join(user_avgs_reg, by="userId") %>%
#   mutate(pred = mu + b_i + b_u + f_d) %>%
#   .$pred
# RMSE(predicted_ratings, test_set$rating)
# 
# 
# residuals %>%
#   group_by(genres) %>%
#   summarize(n = n(), mean = mean(resid)) %>%
#   arrange(desc(mean)) %>%
#   print(n = 100)
# 
# 
# regularized genres effect
# get_genres_avgs_reg <- function(l) {
#   train_set %>%
#     group_by(genres) %>%
#     summarize(b_g = sum(res)/(n() + l))
# }
# 
# rmses <- sapply(lambdas, function(l) {
#   genres_avgs <- get_genres_avgs_reg(l)
#   predicted_ratings <- test_set %>%
#     left_join(movie_avgs_reg, by="movieId") %>%
#     left_join(user_avgs_reg, by="userId") %>%
#     left_join(genres_avgs, by="genres")
#     mutate(pred = mu + b_i + b_u + b_g) %>%
#     .$pred
#   RMSE(predicted_ratings, test_set$rating)
# })
# qplot(lambdas, rmses)
# b_g_lambda <- lambdas[which.min(rmses)]
# 
# user_avgs_reg <- get_genres_avgs_reg(b_g_lambda)
# 
# rmse_results <- bind_rows(rmse_results,
#                           data_frame(method="Regularized Movie + User + Genres Effect Model",
#                                      RMSE = min(rmses)))
# rmse_results

# popular_genres <- train_set %>%
#   group_by(genres) %>%
#   summarize(n = n(), mean = mean(res), lower = mean - sd(res)/sqrt(n), upper = mean + sd(res)/sqrt(n)) %>%
#   top_n( 20, n) %>%
#   arrange(desc(n))
# 
# popular_genres %>%
#   ggplot(aes(genres, mean, ymin = lower, ymax = upper)) +
#   geom_errorbar() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))



