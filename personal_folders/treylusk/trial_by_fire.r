
library(tidyverse)
library(dplyr)
library(stringr)

raw <- read_csv("https://raw.githubusercontent.com/BYUIDSconsulting/woodruff_stories/master/data/raw/2022-12-12-wwp-pages-export.csv")
View(raw)

transcripts <- raw %>% 
  drop_na(`Text Only Transcript`)
View(transcripts)

table(transcripts$Name)


page1 <- transcripts %>% 
  filter(Name == "page_0001")
View(page1)
# maybe page_0001 means first page in that entry


journals <- transcripts %>% 
  filter(`Document Type` == "Journals")
View(journals)



journals2 <- journals %>% 
  select(Name, `Parent Name`, `Parent ID`,`Original Transcript`)%>%
  group_by(`Parent Name`)
View(journals2)


table(journal$Name)

sjournal <- journal %>% 
  filter(`Parent ID` == "5157")
View(sjournal)

table(sjournal$Name)

sj <- sjournal %>% 
  select(`Internal ID`, Name, `Original Transcript`, `Text Only Transcript`)
View(sj)
sj$new_date <- NA

## make UDF for extracting dates

s <- "I have this: 180-4"

# e_sj <- extract(
#   sj,
#   col = "Original Transcript",
#   into = "new_date",
#   regex = "(\\d)"
#   )
# View(e_sj)

f_sj <- sj %>% 
  mutate(new_date = str_extract(
    string = `Original Transcript`,
    pattern = "when='\d+-\d+-\d+'"
  ))
View(f_sj)

bob <- paste(sj$`Text Only Transcript`, collapse = ';')
bob




all_journals <- raw %>% 
  filter(`Document Type` == "Journals")
View(all_j)

# external text file

txtf <- read.delim("https://raw.githubusercontent.com/BYUIDSconsulting/woodruff_stories/master/data/raw/WWJ_Final_djvu.txt")
View(txtf)

df <- read.table("https://raw.githubusercontent.com/BYUIDSconsulting/woodruff_stories/master/data/raw/WWJ_Final_djvu.txt", sep = "\t")
View(df)

# df$open[grepl("^\\d", df$V1)] <- 1
# View(df)

df2 <- df %>% 
  mutate(start = ifelse(
    grepl("^\\d", df$V1), "yes", "no"
  ))
View(df2)



# trying to make new collpsed string

View(raw)

sorted <- raw %>% 
  arrange(`Internal ID`)
View(sorted)

full_text <- str_c(sorted$`Text Only Transcript`, sep = "", collapse=NULL)
View(full_text)
full_text

sub_text <- substring(full_text, 0, 5)
sub_text

str_view_all(full_text, pattern="\\d")

str_extract_all()
