
# Text processing ---------------------------------------------------------

library(tidytext)
library(tm)

glimpse(mass_shootings)

mass_shootings$summary

## Regex pattern to detect names and potential middle names (M. or full name) - acounts for del, -, III, Mc, etc

name_pattern <- "(?<!the )[A-Z][a-z]+ [A-Z]?[']?[A-Z][a-z]+ [A-Z][a-z]+\\, |^[A-Z|a-z]+ ([A-Z]\\.? )?[A-Z][a-z]+\\, |^[A-Z][a-z]+ [A-Z][a-z]+ (del)? [A-Z][a-z]+\\,? |[A-Z|a-z]+ ([A-Z]\\.? )?[A-Z][a-z]+\\, |[A-Z|a-z]+ [A-Z][a-z]+ III\\, |[A-Z|a-z]+ Mc[A-Z][a-z]+\\, |[A-Z][a-z]+\\-[A-Z][a-z]+ [A-Z][a-z]+\\, |[A-Z][a-z]+ [A-Z][a-z]+ [A-Z][a-z]+\\,? " # perl=TRUE

names <- str_extract_all(mass_shootings$summary, pattern = name_pattern)

## Entry 61 has a weird character in it - manual entry Sulejman Talović grepl(mass_shootings$summary, pattern = "\U{0107}")

names[61] <- "Sulejman Talović"

mass_shootings$name <- names %>% map(1) %>% unlist()# sapply(test, function(x) x[1])

# Delete all white spaces and commas in names

mass_shootings %<>% mutate(name = str_remove_all(name, pattern = ", $"))



# Text analysis -----------------------------------------------------------

## Normalize and clean text

# Remove white space in the beginning and end

mass_shootings %<>% mutate(summary_clean = str_replace(summary, pattern = "(^[:space:]?)", replacement = ""))

mass_shootings %<>% mutate(summary_clean = str_replace(summary, pattern = "([:space:]?$)", replacement = ""))


mass_shootings %<>% mutate(summary_clean = gsub("\r?\n|\r", " ", summary_clean))  # get rid of line brakes


mass_shootings %<>% mutate(summary_clean = str_replace_all(summary_clean, pattern = "([:space:]+)", replacement = " "))


## Split into sentences
## 

library(stringr); library(tidytext); library(stringi)


sentences <- mass_shootings %>% unnest_tokens(output = sentence, input = summary_clean, token = "sentences")

str(sentences)

mass_sentences <- sentences %>% group_by(year) %>% mutate(linenumber = row_number()) %>% ungroup


## Creating tidy text one row per word data frame

tidy_mass <- mass_sentences %>% unnest_tokens(output = word, input = sentence)


data("stop_words")

library(tm)

# Also get stopwords from tm package

new_stops <- bind_rows(data.frame(word = stopwords("en"), lexicon = c("custom")), stop_words)

tidy_mass <- tidy_mass %>% anti_join(new_stops)

tidy_mass %>% count(word, sort = TRUE) %>% filter(n > 10) %>% mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()



tidy_mass %>% count(word, sort = TRUE) %>% filter(n > 5) %>% mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() + 
        scale_fill_gradient(guide = FALSE, low = "#E68415", high = "#C94024") +
        theme_tufte() +
        theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
        ggtitle("Most frequent words in summary") +
        labs(y = "Count")


# Psychological descriptions ----------------------------------------------


## Normalize and clean text

# Remove white space in the beginning and end

mass_shootings %<>% mutate(mental_health_clean = str_replace(mental_health_details, pattern = "(^[:space:]?)", replacement = ""))

mass_shootings %<>% mutate(mental_health_clean = str_replace(mental_health_clean, pattern = "([:space:]?$)", replacement = ""))


mass_shootings %<>% mutate(mental_health_clean = gsub("\r?\n|\r", " ", mental_health_clean))  # get rid of line brakes


mass_shootings %<>% mutate(mental_health_clean = str_replace_all(mental_health_clean, pattern = "([:space:]+)", replacement = " "))

## Split into sentences

sentences_health <- mass_shootings %>% unnest_tokens(output = sentence, input = mental_health_clean, token = "sentences")


mass_sentences_health <- sentences_health %>% group_by(year) %>% mutate(linenumber = row_number()) %>% ungroup


## Creating tidy text one row per word data frame

tidy_mass_health <- mass_sentences_health %>% unnest_tokens(output = word, input = sentence)


## Removing stop words

tidy_mass_health <- tidy_mass_health %>% anti_join(new_stops)

tidy_mass_health %>% count(word, sort = TRUE) %>% filter(n > 5) %>% mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() +
        scale_fill_gradient(guide = FALSE, low = "#E68415", high = "#C94024") +
        theme_tufte() +
        theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
        ggtitle("Most frequent words in details about psychological health") +
        labs(y = "Count")



## Correlations



## TFID weighting




# Topic modeling ----------------------------------------------------------


corpus_test <- VCorpus(DataframeSource(text2))

# Alter the function code to match the instructions
clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
        corpus <- tm_map(corpus, removeNumbers)
        return(corpus)
}

# Apply your customized function to the tweet_corp: clean_corp
clean_corp <- clean_corpus(corpus_test)

dtm <- DocumentTermMatrix(clean_corp)

## LDA topic modeling tests

library(topicmodels)

# Needs document-term-matrix

lda <- LDA(dtm[1:2,], k = 2, method = "Gibbs")

tidy(lda, matrix = "beta") %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% 
        arrange(topic, -beta) %>% mutate(term = reorder(term, beta)) %>% 
        ggplot(aes(term, beta, fill = topic)) +
        geom_col(show.legend = FALSE) + coord_flip() +
        facet_wrap(~topic, scales = "free_y")