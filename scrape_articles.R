##############################################################
#### Scrapes all articles by Soo Ewe Jin from The Star website
#### Author: Kevin Soo
##############################################################

# load libraries
library(tidyverse)
library(stringr)
library(rvest)

# for each result page
for (i in 1:37) {
    # scrape name, date, url
    pages <- paste("http://www.thestar.com.my/authors/?q=%22Soo+Ewe+Jin%22&pgno=", i, sep="")
    article <- read_html(pages) %>% html_nodes("#slcontent3_3_sleft_0_pnlAuthorsList a") %>% html_text()
    date <- read_html(pages) %>% html_nodes(".date") %>% html_text()
    link <- read_html(pages) %>% html_nodes("#slcontent3_3_sleft_0_pnlAuthorsList a") %>% html_attr('href')
    
    # combine into data frame
    date <- date[2:length(date)]
    temp <- data.frame(article, date, link)
    if (i==1) { ej <- temp }
    else { ej <- rbind(ej, temp)}
}

# format links and titles
ej$url <- NA
ej$title <- NA
for (i in 1:nrow(ej)) {
    ej$url[i] <- paste("http://www.thestar.com.my", ej$link[i], sep="")
    # clean html
    ej$title[i] <- gsub("â\u0080\u0098", "'", 
                        gsub("â\u0080\u0099", "'", 
                             gsub("â\u0080\u0094", "-", 
                                  gsub("â\u0080\u0093", "-", 
                                       gsub(" &#8212;", "; ",
                                            gsub("&#8216;", "'",
                                                 gsub("&#8217;", "'",
                                                      gsub("â\u0080\u0098", "'",
                                                                ej$article[i]))))))))
}
ej <- ej %>% select(-link, -article)

# categorize each article
ej$category <- NA
for (i in 1:nrow(ej)) {
    if(str_detect(ej$url[i], "/columnists/")==TRUE) { ej$category[i] <- "Column" }
    else if(str_detect(ej$url[i], "/business-news/")==TRUE) { ej$category[i] <- "Business" }
    else { ej$category[i] <- "Other" }
}

# scrape text of each article
ej$text <- NA
ej$paragraphs <- NA
for (i in 1:nrow(ej)) {
    txt <- read_html(ej$url[i]) %>% html_nodes("#slcontent3_5_sleft_0_storyDiv p") %>% html_text()
    tmp <- paste(txt, collapse=" ")
    ej$paragraphs[i] <- length(txt)
    # clean html
    ej$text[i] <- gsub("\n", "", 
                       gsub("Â", "",
                            gsub("â\u0080\u0098", "'", 
                                 gsub("â\u0080\u0099", "'", 
                                      gsub("â\u0080\u0094", "-", 
                                           gsub("â\u0080\u0093", "-", 
                                                gsub(" &#8212;", "; ",
                                                     gsub("&#8216;", "'",
                                                          gsub("&#8217;", "'",
                                                               gsub("â\u0080\u0098", "'",
                                                                    tmp))))))))))
}

# format columns
ej$category <- as.factor(ej$category)
ej$title <- as.factor(ej$title)
ej$date <- as.Date(ej$date, format="%d %B %Y")

# save date
save(ej, file="ejStar.Rda")
