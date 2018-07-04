
# Text processing ---------------------------------------------------------

library(tidytext)
library(tm)

glimpse(mass_shootings)

mass_shootings$summary

## Regex pattern to detect names and potential middle names (M. or full name) - acounts for del, -, III, Mc, etc

pattern <- "(?<!the )[A-Z][a-z]+ [A-Z]?[']?[A-Z][a-z]+ [A-Z][a-z]+\\, |^[A-Z|a-z]+ ([A-Z]\\.? )?[A-Z][a-z]+\\, |^[A-Z][a-z]+ [A-Z][a-z]+ (del)? [A-Z][a-z]+\\,? | [A-Z|a-z]+ ([A-Z]\\.? )?[A-Z][a-z]+\\, " # perl=TRUE

pattern2 <- " [A-Z|a-z]+ [A-Z][a-z]+ III\\, | [A-Z|a-z]+ Mc[A-Z][a-z]+\\, | [A-Z][a-z]+\\-[A-Z][a-z]+ [A-Z][a-z]+\\," # perl=TRUE

name_pattern <- "(?<!the )[A-Z][a-z]+ [A-Z]?[']?[A-Z][a-z]+ [A-Z][a-z]+\\, |^[A-Z|a-z]+ ([A-Z]\\.? )?[A-Z][a-z]+\\, |^[A-Z][a-z]+ [A-Z][a-z]+ (del)? [A-Z][a-z]+\\,? |[A-Z|a-z]+ ([A-Z]\\.? )?[A-Z][a-z]+\\, |[A-Z|a-z]+ [A-Z][a-z]+ III\\, |[A-Z|a-z]+ Mc[A-Z][a-z]+\\, |[A-Z][a-z]+\\-[A-Z][a-z]+ [A-Z][a-z]+\\, |[A-Z][a-z]+ [A-Z][a-z]+ [A-Z][a-z]+\\,? " # perl=TRUE

names <- str_extract_all(mass_shootings$summary, pattern = name_pattern)

## Entry 61 has a weird character in it - manual entry Sulejman Talović

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


diaries_sentences <- diaries_combined %>% unnest_tokens(output = sentence, input = text, token = "sentences")

str(diaries_sentences)

diaries_sentences <- diaries_sentences %>% group_by(part_id) %>% mutate(linenumber = row_number()) %>% ungroup


## Creating tidy text one row per word data frame

tidy_diaries <- diaries_sentences %>% unnest_tokens(output = word, input = sentence)


data("stop_words")

library(tm)

# Also get stopwords from tm package

new_stops <- bind_rows(data.frame(word = stopwords("en"), lexicon = c("custom")), stop_words)

tidy_diaries <- tidy_diaries %>% anti_join(new_stops)

tidy_diaries %>% count(word, sort = TRUE) %>% filter(n > 100) %>% mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

## Remove numbers

diaries_combined[,4] <- gsub('[[:digit:]]+', '', diaries_combined[,4])

## Split in sentences again

diaries_sentences <- diaries_combined %>% unnest_tokens(output = sentence, input = text, token = "sentences")


diaries_sentences <- diaries_sentences %>% group_by(part_id, diary_num) %>% mutate(linenumber = row_number()) %>% ungroup

saveRDS(diaries_sentences, file = "diaries_sentences.rds")

## Make tidy diaries with line numbers

tidy_diaries <- diaries_sentences %>% unnest_tokens(word, sentence)

tidy_diaries <- tidy_diaries %>% anti_join(new_stops)

tidy_diaries %>% count(word, sort = TRUE) %>% filter(n > 100) %>% mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()


## Remove words like it's i'm, don't

new_stops2 <- bind_rows(data.frame(word = c("it’s", "i’m", "I’m", "i’ve", "i’ll", "c’s", "she’s", "he’s", "don’t"), 
                                   lexicon = c("custom")), new_stops)


tidy_diaries <- tidy_diaries %>% anti_join(new_stops2)

tidy_diaries %>% count(word, sort = TRUE) %>% filter(n > 30) %>% mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() + 
        scale_fill_gradient(guide = FALSE, low = "blue", high = "red")


tidy_diaries %>% count(word, age_range, sex, date_created, sort = TRUE) %>% filter(n > 30, age_range %in% c("20-30", "30-40")) %>% 
        mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() + 
        scale_fill_gradient(guide = FALSE, low = "blue", high = "red") + 
        facet_grid(sex~date_created, scales = "free")

setwd("./dsl_elm_app/data")
saveRDS(object = tidy_diaries, file = "tidy_diaries.rds")


stop_no_day_time <- bind_rows(data.frame(word = c("time", "day", "pm"), 
                                         lexicon = c("custom")), new_stops2)

tidy_diaries_no_daytime <- tidy_diaries %>% anti_join(stop_no_day_time)

saveRDS(tidy_diaries_no_daytime, file = "tidy_diaries_no_daytime.rds")


tidy_diaries_no_daytime %>% count(word, sort = TRUE) %>% filter(n > 50) %>% mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n, fill = n)) + geom_col() + xlab(NULL) + coord_flip() + 
        scale_fill_gradient(guide = FALSE, low = "blue", high = "red")

setwd("./dsl_elm_app/data")

saveRDS(object = tidy_diaries_no_daytime, file = "tidy_diaries_no_daytime.rds")

setwd("../..") # move back two directories

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