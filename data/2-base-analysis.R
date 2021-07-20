load("./rda/edx-validation-modified.RData")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#First model: Y_u,i = mu + e_u,i

mu_hat <- mean(train$rating)
naive_rmse <- RMSE(test$rating, mu_hat)