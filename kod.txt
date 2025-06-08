# ---- 1. Instalacja pakietów (jednorazowo) ----
packages <- c("tidyverse", "tidytext", "textdata", "wordcloud", "ggplot2", "RColorBrewer")
install_if_missing <- function(pkg) if (!require(pkg, character.only = TRUE)) install.packages(pkg)
invisible(lapply(packages, install_if_missing))

# ---- 2. Ładowanie bibliotek ----
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(textdata)

# ---- 3. Wczytanie danych ----
file_path <- "biblia.txt"
if (!file.exists(file_path)) stop("Plik biblia.txt nie został znaleziony w katalogu roboczym!")

raw_data <- tibble(text = readLines(file_path))

# ---- 4. Przygotowanie danych ----
data("stop_words")
tidy_data <- raw_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_to_lower(word))

word_frequencies <- tidy_data %>%
  count(word, sort = TRUE)

# ---- 5. Analiza sentymentu ----
sentiment_bing <- get_sentiments("bing")
text_sentiment <- tidy_data %>%
  inner_join(sentiment_bing, by = "word") %>%
  count(word, sentiment, sort = TRUE)

total_sentiment <- text_sentiment %>%
  group_by(sentiment) %>%
  summarise(total_words = sum(n), .groups = "drop")

# ---- 6. Wizualizacje ----
# Wykres: 20 najczęstszych słów
ggplot(word_frequencies %>% slice_max(n, n = 20), aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "20 Najczęściej występujących słów", x = "Słowo", y = "Częstość")

# Wykres: sentyment
ggplot(total_sentiment, aes(x = sentiment, y = total_words, fill = sentiment)) +
  geom_col() +
  labs(title = "Ogólny sentyment tekstu", x = "Sentyment", y = "Liczba słów") +
  theme_minimal()

# Chmura słów
set.seed(1234)
wordcloud(
  words = word_frequencies$word,
  freq = word_frequencies$n,
  min.freq = 2,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
