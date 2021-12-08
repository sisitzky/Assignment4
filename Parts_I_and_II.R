library(gutenbergr)
library(dplyr)
library(stringr)
library(tidytext)
library(textdata)
library(tidyverse)
library(wordcloud)
library(reshape2)


gutenberg_metadata %>%
  filter(title == "The Scarlet Letter")

scarlet_letter <- gutenberg_download(25344)

scarlet_letter <- scarlet_letter %>%  
                        mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc <- get_sentiments("nrc")

nrc_fear <- get_sentiments("nrc") %>% 
                    filter(sentiment == "fear")

scarlet_fear <- scarlet_letter %>%
                    inner_join(nrc_fear) %>%
                    count(word, sort = TRUE)


scarlet_letter_sentiment <- scarlet_letter %>%
                                inner_join(get_sentiments("bing")) %>%
                                count(index = linenumber %/% 100, sentiment) %>%
                                pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
                                mutate(sentiment = positive - negative)

ggplot(scarlet_letter_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)


afinn <- scarlet_letter %>% 
                inner_join(get_sentiments("afinn")) %>% 
                group_by(index = linenumber %/% 100) %>% 
                summarise(sentiment = sum(value)) %>% 
                mutate(method = "AFINN")

bing_and_nrc <- bind_rows(scarlet_letter %>% 
                              inner_join(get_sentiments("bing")) %>%
                                    mutate(method = "Bing et al."),
                          scarlet_letter %>% 
                              inner_join(get_sentiments("nrc") %>% 
                                    filter(sentiment %in% c("positive", "negative"))) %>%
                                      mutate(method = "NRC")) %>%
                                      count(method, index = linenumber %/% 100, sentiment) %>%
                                      pivot_wider(names_from = sentiment,
                                      values_from = n,
                                      values_fill = 0) %>% 
                                      mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


bing_word_counts <- scarlet_letter %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


scarlet_letter %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


scarlet_letter %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)













