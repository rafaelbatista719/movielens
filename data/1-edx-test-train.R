library(tidyverse)
library(caret)

load("./rda/edx-validation.RData")

set.seed(1, sample.kind = "Rounding")

test_index <- createDataPartition(edx$rating, times = 1, p = 0.5, list = FALSE)
edx %>% mutate(rating = as.factor(rating))
test <- edx[test_index, ]
train <- edx[-test_index, ]