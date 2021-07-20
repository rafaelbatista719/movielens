library(tidyverse)
library(caret)

load("./rda/edx-validation.RData")

set.seed(1, sample.kind = "Rounding")

#test_index <- createDataPartition(edx$rating, times = 1, p = 0.5, list = FALSE)

#test <- edx[test_index, ]
#train <- edx[-test_index, ]
#rm(test_index)