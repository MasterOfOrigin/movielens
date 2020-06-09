edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ggtitle("Movies")

edx %>%
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  xlab("Number of ratings") +
  ggtitle("Users")


edx %>%
  ggplot(aes(rating)) +
  geom_bar() +
  ggtitle("Ratings") +
  geom_vline(xintercept = mean(edx$rating), color = "blue") 