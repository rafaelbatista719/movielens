library(tidyverse)
library(caret)

load("./rda/edx-validation.RData")

set.seed(1, sample.kind = "Rounding")


edx <- edx %>% extract(title, c("title", "year"), "^(.*)\\s\\((\\d{4})\\)$") %>% mutate(year = as.integer(year))




#test_index <- createDataPartition(edx$rating, times = 1, p = 0.5, list = FALSE)

#test <- edx[test_index, ]
#train <- edx[-test_index, ]
#rm(test_index)