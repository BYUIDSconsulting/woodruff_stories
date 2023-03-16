
library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)

# Loading in the data
raw <- read_csv("/Users/luskenterprises/consulting/woodruff_stories/personal_folders/treylusk/with_internal_id.csv")
View(raw)
# Filtering out just the journals
all_journals <- raw %>% 
  filter(`Document Type` == "Journals")
View(all_journals)
# Sorting the journals so they are more or less in order of when they were written
sorted <- all_journals %>% 
  arrange(`Parent ID`)
view(sorted)
# This paste turns it into one long string 
n_full_text <- paste(sorted$`Text Only Transcript`, collapse = "")
# This is the pattern that removes all posible ways that WW wrote his dates
pattern_trey <- "(([J|F|M|A|J|S|O|N|D][a-z]{2,8}\\s){1,2}\\d{1,2}(th|st|rd|nd)?,?(\\s\\d{4})?|[J|F|M|A|J|S|O|N|D][a-z]{3,8}\\s\\d{1,2}(th|st|rd|nd)?) ~"
# Removing of the dates
matches <- str_extract_all(n_full_text, pattern=pattern_trey) %>% unlist()
# Emma wrote this, i think that this removies the first NA
#matches2 <- append(matches, NA, after = 0)
# This then resplits the data by date as opposed by page
text <- strsplit(n_full_text, split = pattern_trey) %>% unlist()
# This creates our database which is huge for us
papers <- data.frame(
  date = matches2,
  text = text
)
# Creating People Column
calvin <-raw%>%
  filter(People != 'NA')
# Creating the list 
real_people_list <- calvin$People
# creating one string
colpasts_people <- paste(real_people_list, collapse = "|")
# Splitting it by one person
franklin <-str_split(colpasts_people,'\\|')
# This unlisted it though im not sure it did anything
carl <- franklin %>% unlist()
# This creates unique list
new <-unique(carl)
# This extracts from the text all things that match any names in the list and put it into a new column
papers$names <- sapply(str_extract_all(papers$text, paste(new, collapse = "|")), 
                        paste, collapse = ", ")



# This whole thing is the exact same code but done for the place 
micheal <-raw%>%
  filter(Places != 'NA')
view(micheal$Places)
real_place_list <- micheal$Places
view(real_place_list)

colpasts_place <- paste(real_place_list, collapse = "|")
delenore <-str_split(colpasts_place,'\\|')
rose <- delenore %>% unlist()
length(unique(carl))
newer <-unique(rose)

papers$places <- sapply(str_extract_all(papers$text, paste(newer, collapse = "|")), 
                        paste, collapse = ", ")

# tilda removing 
papers$date <- substring(papers$date, 1, nchar(papers$date) - 1)

papers%>%mutate(date_new = unlist(format(mdy(papers$date), "%m/%d/%Y")))

view(papers)

papers()

write.csv(papers, "/Users/luskenterprises/consulting/woodruff_stories/personal_folders/treylusk/WW_by_date_with_may.csv", row.names=FALSE)

contains_string <- grepl("Leaves from my Journal", raw$`Parent Name`)

journal_leafs <- raw[contains_string, ]
view(journal_leafs)

leaves_from_my_journal <- raw%>% filter(grepl("Leaves [Ff]rom [Mm]y Journal",raw$`Parent Name`))

view(leaves_from_my_journal)
clara <- raw%>%filter(`Document Type`=="Additional")%>%
  mutate(grepl("Religious",raw$`Parent Name`))
view(clara)
#try <-raw%>% filter(`Parent Name` == str_contains("Autobiography Leaves from my Journal"))
#view(try)
