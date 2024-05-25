library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

library(lubridate)
library(httr)

library(jsonlite)
NYTIMES_KEY <- "ZbsRjAMerhNV6yxJgj1pEFnqiGXqEmW5"

term <- "crypto"
begin_date <- "20220916"
end_date <- "20230101"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

pages <- vector("list",length=maxPages)

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages[[i+1]] <- nytSearch 
  Sys.sleep(5) #I was getting errors more often when I waited only 1 second between calls. 5 seconds seems to work better.
}


crypto_articles_22 <- rbind_pages(pages)

getwd()
setwd('/Users/judithlong/Downloads/Text Analytics')
save(crypto_articles_22,file="crypto_articles_22.Rdata")

term <- "crypto"
begin_date <- "20230102"
end_date <- "20230402"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

pages_23 <- vector("list",length=maxPages)

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame()
  pages_23[[i+1]] <- nytSearch
  Sys.sleep(5)
}
crypto_articles_23 <- rbind_pages(pages_23)

save(crypto_articles_23,file="crypto_articles_23.Rdata")

# explore data
colnames(crypto_articles_22) <- str_replace(colnames(crypto_articles_22),
                                                pattern='response\\.',replace='')
colnames(crypto_articles_22) <- str_replace(colnames(crypto_articles_22),
                                                pattern='docs\\.',replace='')

colnames(crypto_articles_23) <- str_replace(colnames(crypto_articles_23),
                                                pattern='response\\.',replace='')
colnames(crypto_articles_23) <- str_replace(colnames(crypto_articles_23),
                                                pattern='docs\\.',replace='')

colnames(crypto_articles_22)

typeof(crypto_articles_22)

for(i in 1:ncol(crypto_articles_22))
{
  if(class(crypto_articles_22[,i]) == "list"){
    print(colnames(crypto_articles_22)[i])
    head(crypto_articles_22[,i],n=3)
  }
}




num_rows_keywords <- unlist(lapply(crypto_22_articles$keywords,function(x)nrow(x)))

for(article_num in which(num_rows_keywords > 0)){
  this_article_keywords <- data.frame(Article.index = article_num,
                                      crypto_22_articles$keywords[[article_num]],
                                      stringsAsFactors=FALSE)
  if(exists("crypto_22_keywords_collapsed") == FALSE){crypto_22_keywords_collapsed <- this_article_keywords;next}
  crypto_22_keywords_collapsed <- rbind(crypto_22_keywords_collapsed,this_article_keywords)
}

head(crypto_22_keywords_collapsed);tail(crypto_22_keywords_collapsed)


class_keywords <- unlist(lapply(crypto_23_articles$keywords,function(x)class(x)))
num_rows_keywords <- vector("numeric",length=nrow(crypto_23_articles))
num_rows_keywords[which(class_keywords == "data.frame")] <- unlist(lapply(crypto_23_articles$keywords,function(x)nrow(x)))

for(article_num in which(num_rows_keywords > 0)){
  this_article_keywords <- data.frame(Article.index = article_num,
                                      crypto_23_articles$keywords[[article_num]],
                                      stringsAsFactors=FALSE)
  if(exists("crypto_23_keywords_collapsed") == FALSE){crypto_23_keywords_collapsed <- this_article_keywords;next}
  crypto_23_keywords_collapsed <- rbind(crypto_23_keywords_collapsed,this_article_keywords)
}

head(crypto_23_keywords_collapsed);tail(crypto_23_keywords_collapsed)


crypto_22_articles <- crypto_22_articles %>% select(setdiff(colnames(crypto_22_articles),"keywords"))
crypto_23_articles <- crypto_23_articles %>% select(setdiff(colnames(crypto_23_articles),"keywords"))

head(crypto_22_articles);tail(crypto_22_articles)

crypto_22_articles <- data.frame(Targeted.topic = "2022 crypto",
                                     Article.index = 1:nrow(crypto_22_articles),
                                     crypto_22_articles,
                                     stringsAsFactors=FALSE)

crypto_23_articles <- data.frame(Targeted.topic = "2023 crypto",
                                     Article.index = 1:nrow(crypto_23_articles),
                                     crypto_23_articles,
                                     stringsAsFactors=FALSE)

crypto_articles <- rbind(crypto_22_articles,crypto_23_articles)

crypto_22_keywords_collapsed <- data.frame(Targeted.topic = "2014 Facebook psychological experiment scandal",
                                               crypto_22_keywords_collapsed,
                                               stringsAsFactors=FALSE)

crypto_23_keywords_collapsed <- data.frame(Targeted.topic = "2018 Facebook Cambridge Analytica scandal",
                                               crypto_23_keywords_collapsed,
                                               stringsAsFactors=FALSE)

crypto_keywords <- rbind(crypto_22_keywords_collapsed,crypto_23_keywords_collapsed)


articles_matching_keywords_info <- crypto_keywords %>% filter(value == "crypto") %>% select(c("Targeted.topic","Article.index"))
articles_matching_keywords_info <- articles_matching_keywords_info[!duplicated(articles_matching_keywords_info),]

merged_articles <- merge(crypto_articles,articles_matching_keywords_info,by=c("Targeted.topic","Article.index"))

crypto_articles <- apply(crypto_articles,2,as.character)

write.csv(crypto_articles,"crypto articles from nyt.csv")


# parse full text
# load all required packages
library(newsanchor) # download newspaper articles
library(robotstxt)  # get robots.txt
library(httr)       # http requests
library(rvest)      # web scraping tools
library(dplyr)      # easy data frame manipulation
library(stringr)    # string/character manipulation 
library(tidytext)   # tidy text analysis

get_article_body <- function (url) {
  
  # download article page
  response <- GET(url)
  
  # check if request was successful
  if (response$status_code != 200) return(NA)
  
  # extract html
  html <- content(x        = response, 
                  type     = "text", 
                  encoding = "UTF-8")
  
  # parse html
  parsed_html <- read_html(html)                   
  
  # define paragraph DOM selector
  selector <- "article#story div.StoryBodyCompanionColumn div p"
  
  # parse content
  parsed_html %>% 
    html_nodes(selector) %>%      # extract all paragraphs within class 'article-section'
    html_text() %>%               # extract content of the <p> tags
    str_replace_all("\n", "") %>% # replace all line breaks
    paste(collapse = " ")         # join all paragraphs into one string
}

library(readxl)

articles <- read_excel("crypto articles from nyt.xlsx")

allowed <- paths_allowed(articles$web_url)
all(allowed)
# create new text column
articles$body <- NA

# initialize progress bar
pb <- txtProgressBar(min     = 1, 
                     max     = nrow(articles), 
                     initial = 1, 
                     style   = 3)

# loop through articles and "apply" function
for (i in 1:nrow(articles)) {
  
  # "apply" function to i url
  articles$body[i] <- get_article_body(articles$web_url[i])
  
  # update progress bar
  setTxtProgressBar(pb, i)
  
  # sleep for 1 sec
  Sys.sleep(1)
}


write.csv(articles,'nyt articles.csv')
