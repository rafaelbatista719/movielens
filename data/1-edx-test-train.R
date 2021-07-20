library(tidyverse)
library(caret)

load("./rda/edx-validation.RData")

set.seed(1, sample.kind = "Rounding")

edx <- edx %>% extract(title, c("title", "year"), "^(.*)\\s\\((\\d{4})\\)$") %>% mutate(year = as.integer(year)) %>%
  mutate (isAction = as.numeric(str_detect(genres, "Action")),
          isAdventure = as.numeric(str_detect(genres, "Adventure")),
          isAnimation = as.numeric(str_detect(genres, "Animation")),
          isChildren = as.numeric(str_detect(genres, "Children")),
          isComedy = as.numeric(str_detect(genres, "Comedy")),
          isCrime = as.numeric(str_detect(genres, "Crime")),
          isDocumentary = as.numeric(str_detect(genres, "Documentary")),
          isDrama = as.numeric(str_detect(genres, "Drama")),
          isFantasy = as.numeric(str_detect(genres, "Fantasy")),
          isFilmNoir = as.numeric(str_detect(genres, "Film-Noir")),
          isHorror = as.numeric(str_detect(genres, "Horror")),
          isIMAX = as.numeric(str_detect(genres, "IMAX")),
          isMusical = as.numeric(str_detect(genres, "Musical")),
          isMystery = as.numeric(str_detect(genres, "Mystery")),
          isRomance = as.numeric(str_detect(genres, "Romance")),
          isSciFi = as.numeric(str_detect(genres, "Sci-Fi")),
          isThriller = as.numeric(str_detect(genres, "Thriller")),
          isWar = as.numeric(str_detect(genres, "War")),
          isWestern = as.numeric(str_detect(genres, "Western")))

validation <- validation %>% extract(title, c("title", "year"), "^(.*)\\s\\((\\d{4})\\)$") %>% mutate(year = as.integer(year)) %>%
  mutate (isAction = as.numeric(str_detect(genres, "Action")),
          isAdventure = as.numeric(str_detect(genres, "Adventure")),
          isAnimation = as.numeric(str_detect(genres, "Animation")),
          isChildren = as.numeric(str_detect(genres, "Children")),
          isComedy = as.numeric(str_detect(genres, "Comedy")),
          isCrime = as.numeric(str_detect(genres, "Crime")),
          isDocumentary = as.numeric(str_detect(genres, "Documentary")),
          isDrama = as.numeric(str_detect(genres, "Drama")),
          isFantasy = as.numeric(str_detect(genres, "Fantasy")),
          isFilmNoir = as.numeric(str_detect(genres, "Film-Noir")),
          isHorror = as.numeric(str_detect(genres, "Horror")),
          isIMAX = as.numeric(str_detect(genres, "IMAX")),
          isMusical = as.numeric(str_detect(genres, "Musical")),
          isMystery = as.numeric(str_detect(genres, "Mystery")),
          isRomance = as.numeric(str_detect(genres, "Romance")),
          isSciFi = as.numeric(str_detect(genres, "Sci-Fi")),
          isThriller = as.numeric(str_detect(genres, "Thriller")),
          isWar = as.numeric(str_detect(genres, "War")),
          isWestern = as.numeric(str_detect(genres, "Western")))

test_index <- createDataPartition(edx$rating, times = 1, p = 0.2, list = FALSE)

test_set <- edx[test_index, ]
train_set <- edx[-test_index, ]
rm(test_index)
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

save.image(file = "./rda/edx-validation-modified.RData")