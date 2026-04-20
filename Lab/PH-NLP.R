# ============================================================
# Sentiment & Emotion Analysis of PH Ludwigsburg (English site)
# Prof. Dr. Olawale Awe
# ============================================================
getwd()
# ---- 0. Install Required Packages (Run only once) ----
# Uncomment these lines and run them if packages are not yet installed on your computer.
# install.packages("rvest")
# install.packages("dplyr")
# install.packages("tidytext")
# install.packages("tibble")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("ggwordcloud")
# install.packages("wordcloud2")
# install.packages("scales")
# install.packages("tidyr")
# install.packages("forcats")

# ---- 1. Load packages ----
library(rvest)
library(dplyr)
library(tidytext)
library(tibble)
library(stringr)
library(ggplot2)
library(ggwordcloud)
library(wordcloud2)
library(scales)
library(tidyr)
library(forcats)

# ---- 2. Scrape website text ----
url <- "https://www.ph-ludwigsburg.de/en"

page <- read_html(url)

text_content <- page |>
  html_nodes("p, h1, h2, h3, li, a") |>
  html_text2() |>
  paste(collapse = " ") |>
  str_squish()

cat(substr(text_content, 1, 300), "...\n")  # preview first 300 chars

# ---- 3. Clean & tokenize ----
custom_stop <- c("ludwigsburg", "ph", "university", "education",
                 "study", "students", "home", "apply", "learn",
                 "programme", "program", "admission", "read", "more")

tokens <- tibble(text = text_content) |>
  unnest_tokens(word, text) |>
  filter(str_detect(word, "^[a-z]+$")) |>
  anti_join(stop_words, by = "word") |>
  filter(!word %in% custom_stop, nchar(word) > 2)

# ---- 4. Word frequency ----
word_counts <- tokens |>
  count(word, sort = TRUE)

# ---- 5. Sentiment analysis (Bing lexicon) ----
bing_sent <- tokens |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(sentiment) |>
  mutate(percent = n / sum(n) * 100)

# ---- 6. Emotion analysis (NRC lexicon) ----
nrc_sent <- tokens |>
  inner_join(get_sentiments("nrc"), by = "word") |>
  count(sentiment) |>
  filter(!sentiment %in% c("positive", "negative")) |>
  mutate(percent = n / sum(n) * 100)

# ---- 7. AFINN numeric sentiment ----
afinn_score <- tokens |>
  inner_join(get_sentiments("afinn"), by = "word") |>
  summarise(mean_score = mean(value, na.rm = TRUE))

cat("Average Sentiment Score (AFINN):", round(afinn_score$mean_score, 2), "\n")

# ============================================================
# VISUALIZATIONS
# ============================================================

# ---- 8. Sentiment Bar Chart ----
ggplot(bing_sent, aes(x = sentiment, y = percent, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Sentiment Distribution — PH Ludwigsburg (English Site)",
    x = "Sentiment",
    y = "Percentage of Words"
  ) +
  scale_fill_manual(values = c("positive" = "#2E8B57", "negative" = "#B22222")) +
  theme_minimal(base_size = 14)

# ---- 9. Emotion Breakdown (NRC) ----
ggplot(nrc_sent, aes(x = fct_reorder(sentiment, percent), y = percent, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Emotions Expressed — PH Ludwigsburg (English Site)",
    x = "Emotion",
    y = "Percentage of Words"
  ) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  theme_minimal(base_size = 14)

# ---- 10. Static Word Cloud (ggwordcloud) ----
top_words <- word_counts |> slice_max(n, n = 150)

set.seed(42)
ggplot(top_words, aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(area_corr = TRUE, rm_outside = TRUE) +
  scale_size_area(max_size = 20) +
  scale_color_viridis_c(option = "C", end = 0.9) +
  theme_minimal() +
  labs(title = "Word Cloud — PH Ludwigsburg (English Site)",
       size = "Frequency", color = "Frequency")

# ---- 11. Interactive Word Cloud ----
wordcloud2(top_words,
           size = 1.0,
           minRotation = -pi/6,
           maxRotation =  pi/6,
           rotateRatio = 0.25,
           color = "random-dark",
           backgroundColor = "white")
