library(tidyverse)
library(readr)
library(stringr)
library(ggplot2)
library(extrafont)
library(viridis)
library(ggpubr)
midterm <- read_csv("midterm/Midterm - Attempt Details (3).csv")

# taken from Andrew Heiss' website
library(ggtext)
theme_custom <- function(){
  theme_minimal(base_size = 19,
                base_family = "Fira Sans") %+replace%
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          plot.title = element_markdown(face = "bold", size = rel(1.7)),
          plot.subtitle = element_markdown(face = "plain", size = rel(1.3)),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
          axis.title.y = element_text(margin = margin(r = 10), hjust = 1, angle = 90))
}

# important midterm answers
midterm <- midterm %>% 
  mutate(word_count = str_count(Answer, "\\w+"),
         q_type = ifelse(`Q #` %in% c(1, 2), "Essay", "Medium-length")) %>% 
  rename(question = `Q #`)

# plotting distribution of number of words per question
essay_plot <- ggplot(filter(midterm, question %in% 1:2),
       aes(x = word_count, y = factor(question))) +
  geom_violin(aes(fill = factor(question))) +
  geom_boxplot(width = 0.15, col = "grey", alpha = 0.4) +
  theme_bw(base_size = 13) +
  scale_x_continuous(breaks = seq(0, 1400, 200)) +
  scale_fill_manual(values = viridis(7)[1:2]) +
  labs(y = "Question",
       x = "Number of words") +
  guides(fill = "none") +
  theme_custom()

medium_plot <- ggplot(filter(midterm, question %in% 3:7), 
       aes(x = word_count, y = factor(question))) +
  geom_violin(aes(fill = factor(question))) +
  geom_boxplot(width = 0.15, col = "grey", alpha = 0.4) +
  theme_bw(base_size = 13) +
  scale_x_continuous(breaks = seq(0, 1400, 200)) +
  scale_fill_manual(values = viridis(7)[3:7]) +
  labs(x = "",
       y = "Question") +
  theme_custom()

ggarrange(medium_plot, essay_plot, nrow = 2,
          heights = c(1, 0.7),
          labels = c("Medium-length answers", "Essay answers"))

ggsave("midterm/words_stats.png", height = 7, width = 11)

#------------------------------------------
# text analysis
#------------------------------------------
library(tidytext)

# See here: https://rpubs.com/andrew-donnelly/589746

# a dataset at the word level for Q1
q1_words <- midterm %>%  
  unnest_tokens(word, Answer)

# most frequent non-stop-words per question
q1_words %>% 
  anti_join(stop_words) %>% 
  group_by(question) %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = n, y = word, fill = factor(question))) +
  geom_col() +
  scale_fill_viridis_d() +
  facet_wrap(~question, scales = "free") +
  guides(fill = "none")

midterm_words_counts <- midterm %>%
  unnest_tokens(word, Answer) %>%
  count(question, word, sort = TRUE) 

total_words <- midterm_words_counts %>%
  group_by(question) %>% 
  summarize(total = sum(n))

midterm_words_counts <- left_join(midterm_words_counts, total_words)

midterms_tf_idf <- midterm_words_counts %>%             # Calculates tf-idf
  bind_tf_idf(word, question, n)


midterms_tf_idf %>%
  arrange(-tf_idf) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(question) %>% 
  top_n(5) %>% 
  ggplot(aes(x = word, y = tf_idf, fill = factor(question))) +
  geom_col(show.legend = FALSE) +
  labs(x = "", y = "tf-idf") +
  facet_wrap(~question, scales = "free") +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "Most distinctive words in each question")


##----------------------------------------------------------------
midterm %>% 
  filter(!is.na(Answer)) %>% 
  group_by(question) %>% 
  summarise(mean_grade = mean(Score/`Out Of`),
            n = n())

midterm %>% 
  filter(question %in% 1:2) %>% 
  filter(Score > 5) %>% View()
