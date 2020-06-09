library("irlba")

pop_movies <- train_set %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  top_n(100, n) %>%
  .$movieId

pop_users <- train_set %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  top_n(500, n) %>%
  .$userId

sub_train_set <- train_set %>%
  filter(userId %in% pop_users & movieId %in% pop_movies)

head(sub_train_set)

# m <- train_set %>%
#   select(userId, movieId, rating) %>%
#   spread(movieId, rating) %>%
#   as.matrix()

# nrow(m)

train_sparse <- sparseMatrix(train_set$userId, train_set$movieId, x = train_set$rating)
train_rating_matrix <- new("realRatingMatrix", data = train_sparse)

test_sparse <- sparseMatrix(test_set$userId, test_set$movieId, x = test_set$rating)
test_rating_matrix <- new("realRatingMatrix", data = test_sparse)


r_m <- normalize(train_rating_matrix)
image(r_m)

S <- irlba(train_sparse, nu=5, nv=5)
nrow(train_sparse)
nrow(S$v)
max(train_set$movieId)

ud <- S$u %*% diag(S$d)

ud[351,] %*% S$v[1500,]

residuals[341243,]

train_sparse <- sparseMatrix(residuals$userId, residuals$movieId, x = residuals$resid)
S <- irlba(train_sparse, nu=5, nv=5)

ud <- S$u %*% diag(S$d)
ud[3624,] %*% S$v[19,]
