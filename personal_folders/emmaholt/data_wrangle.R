
library(tidyverse)
library(dplyr)
library(stringr)

raw <- read_csv("https://raw.githubusercontent.com/BYUIDSconsulting/woodruff_stories/master/data/raw/2022-12-12-wwp-pages-export.csv")
View(raw)



all_journals <- raw %>% 
  filter(`Document Type` == "Journals")
View(all_journals)



# trying to make new collapsed string

sorted <- all_journals %>% 
  arrange(`Internal ID`)
View(sorted)


## paste method
n_full_text <- paste(sorted$`Text Only Transcript`, collapse = "")
n_full_text
View(n_full_text)


sub_text1 <- substring(n_full_text, 0, 25000000)
sub_text1

sub_text2 <- substring(n_full_text, 25000001, -1)
sub_text2


pattern_trey <- "(([J|F|M|A|J|S|O|N|D][a-z]{3,8}\\s){1,2}\\d{1,2}(th|st|rd|nd)?,?(\\s\\d{4})?|[J|F|M|A|J|S|O|N|D][a-z]{3,8}\\s\\d{1,2}(th|st|rd|nd)?) ~"

matches <- str_extract_all(n_full_text, pattern=pattern_trey) %>% unlist()

matches2 <- append(matches, NA, after = 0)
matches2

text <- strsplit(n_full_text, split = pattern_trey) %>% unlist()
text

# length(matches) <- length(text)

papers <- data.frame(
  date = matches2,
  text = text
)
View(papers)

people_list <- paste(raw$People, collapse = ";") %>% unlist()
people_list2 <- people_list[!is.na(people_list)]


papers2 <- papers %>%
  mutate(date = str_sub(papers$date, end= -2))
View(papers2)

## write out to files

write.table(n_full_text, file = "../../data/derived/full_text.txt")

write.table(papers2, file = "../../data/derived/papers.csv", row.names = TRUE)






