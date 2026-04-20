# ============================================================
# Sentiment Analysis + WordCloud for "pdf Files"
# AUTHOR: Professor O. Olawale Awe 
# Miva Open University Executive in Residence Course
# ============================================================

# ---- Packages ----
required <- c("pdftools", "tidyverse", "tidytext", "wordcloud",
              "RColorBrewer", "textdata", "ggplot2", "forcats")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)

library(pdftools)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(textdata)
library(ggplot2)
library(forcats)

# ---- File path ----
pdf_path <- "C:/Users/olawa/OneDrive/Desktop/Codes/CV_Awe5.pdf"

# ---- Extract text ----
cat("\nReading PDF...\n")
text_data  <- pdftools::pdf_text(pdf_path)
essay_text <- paste(text_data, collapse = " ")

# ---- Tokenize text ----
tokens <- tibble(text = essay_text) |>
  unnest_tokens(word, text) |>
  filter(str_detect(word, "^[a-z']+$")) |>
  anti_join(stop_words, by = "word")

cat("Total tokens:", nrow(tokens), "\n")

# ---- Word frequencies ----
word_freq <- tokens |>
  count(word, sort = TRUE)

# ---- Bing sentiment ----
suppressWarnings(try(textdata::lexicon_bing(), silent = TRUE))
bing <- get_sentiments("bing")

sentiment_bing <- tokens |>
  inner_join(bing, by = "word")

sentiment_summary <- sentiment_bing |>
  count(sentiment, sort = TRUE)

# ---- NRC emotions ----
suppressWarnings(try(textdata::lexicon_nrc(), silent = TRUE))
nrc <- get_sentiments("nrc")

nrc_summary <- tokens |>
  inner_join(nrc, by = "word") |>
  count(sentiment, sort = TRUE)

# ---- WordCloud ----
cat("\nGenerating WordCloud...\n")
set.seed(123)
wordcloud(
  words       = word_freq$word,
  freq        = word_freq$n,
  min.freq    = 3,
  max.words   = 200,
  random.order= FALSE,
  colors      = brewer.pal(8, "Dark2")
)

# ---- Bing sentiment bar plot ----
cat("\nPlotting Bing Sentiment...\n")
ggplot(sentiment_summary, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Sentiment Distribution (Bing)",
    x = NULL,
    y = "Word Count"
  ) +
  theme_minimal(base_size = 12)

# ---- NRC emotion analysis ----
cat("\nPlotting NRC Emotions...\n")
nrc_summary |>
  mutate(sentiment = fct_reorder(sentiment, n)) |>
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Emotion Analysis (NRC)",
    x = NULL,
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12)

# ---- Display summaries ----
cat("\nTop 25 frequent words:\n")
print(head(word_freq, 25))

cat("\nBing sentiment summary:\n")
print(sentiment_summary)

cat("\nNRC emotions summary:\n")
print(nrc_summary)

