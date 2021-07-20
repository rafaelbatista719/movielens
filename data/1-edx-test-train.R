library(tidyverse)
library(caret)

load("./rda/edx-validation.RData")

set.seed(1, sample.kind = "Rounding")

edx <- edx %>% extract(title, c("title", "year"), "^(.*)\\s\\((\\d{4})\\)$") %>% mutate(year = as.integer(year)) %>%
  mutate (isAction = str_detect(genres, "Action"),
          isAdventure = str_detect(genres, "Adventure"),
          isAnimation = str_detect(genres, "Animation"),
          isChildren = str_detect(genres, "Children"),
          isComedy = str_detect(genres, "Comedy"),
          isCrime = str_detect(genres, "Crime"),
          isDocumentary = str_detect(genres, "Documentary"),
          isDrama = str_detect(genres, "Drama"),
          isFantasy = str_detect(genres, "Fantasy"),
          isFilmNoir = str_detect(genres, "Film-Noir"),
          isHorror = str_detect(genres, "Horror"),
          isIMAX = str_detect(genres, "IMAX"),
          isMusical = str_detect(genres, "Musical"),
          isMystery = str_detect(genres, "Mystery"),
          isRomance = str_detect(genres, "Romance"),
          isSciFi = str_detect(genres, "Sci-Fi"),
          isThriller = str_detect(genres, "Thriller"),
          isWar = str_detect(genres, "War"),
          isWestern = str_detect(genres, "Western"))

validation <- validation %>% extract(title, c("title", "year"), "^(.*)\\s\\((\\d{4})\\)$") %>% mutate(year = as.integer(year)) %>%
  mutate (isAction = str_detect(genres, "Action"),
          isAdventure = str_detect(genres, "Adventure"),
          isAnimation = str_detect(genres, "Animation"),
          isChildren = str_detect(genres, "Children"),
          isComedy = str_detect(genres, "Comedy"),
          isCrime = str_detect(genres, "Crime"),
          isDocumentary = str_detect(genres, "Documentary"),
          isDrama = str_detect(genres, "Drama"),
          isFantasy = str_detect(genres, "Fantasy"),
          isFilmNoir = str_detect(genres, "Film-Noir"),
          isHorror = str_detect(genres, "Horror"),
          isIMAX = str_detect(genres, "IMAX"),
          isMusical = str_detect(genres, "Musical"),
          isMystery = str_detect(genres, "Mystery"),
          isRomance = str_detect(genres, "Romance"),
          isSciFi = str_detect(genres, "Sci-Fi"),
          isThriller = str_detect(genres, "Thriller"),
          isWar = str_detect(genres, "War"),
          isWestern = str_detect(genres, "Western"))

test_index <- createDataPartition(edx$rating, times = 1, p = 0.2, list = FALSE)

test_set <- edx[test_index, ]
train_set <- edx[-test_index, ]
rm(test_index)
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

save.image(file = "./rda/edx-validation-modified.RData")