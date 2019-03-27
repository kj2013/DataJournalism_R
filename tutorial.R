---
### title: "NYT Puller"
### author: '@kj2013'
### date: "March 17, 2019"
### output: 
###  html_document: 
###    toc: yes
###  html_notebook: 
###    number_sections: yes
###    toc: yes
###---

## Data Journalism project

### This is an example of R for a Data Journalism project involving New York Times data, and also for publishing analysis on a webpage.
### Builds on the great tutorial at http://www.storybench.org/working-with-the-new-york-times-api-in-r/


### Prerequisites

### Make sure you've installed the nyt package!

install.packages("devtools")
devtools::install_github("mkearney/nytimes")
install.packages("ggplot2")
install.packages("jsonlite")
install.packages("jsonlite")
install.packages("tm")
install.packages("SnowballC")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("nipals")

### Load libraries

###```{r setup2}
###knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(nytimes)
###```

### This is where my key was. Your key goes here!
###```{r set_key}
key=""
secret = ""



### Choose the keywords and the time period you want!

term <- "trump+kim+summit" # Need to use + to string together separate words
begin_date <- "20180601"
end_date <- "20180930"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",key, sep="")

### Pull the first 50 articles...

#need to know the number of pages of results.

initialQuery <- tryCatch({jsonlite::fromJSON(baseurl)},warning=function(){print("There was an error.")})
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) -1
 Sys.sleep(4) 
#Now, set up the loop!
pages <- list()
nytSearch=data.frame(matrix(ncol=32,nrow=0))
for(i in 0:3){
  nytS<-tryCatch({jsonlite::fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE)%>% data.frame() },warning=function(){print("There was an error.")})
 Sys.sleep(5) 
  message("Retrieving page ", i)
  if(!is.null(nytSearch) & length(nytSearch)>0)
    #append to the master list of articles
{  
    if(!is.null(nytS)){nytSearch<-rbind(nytSearch,nytS)}
   Sys.sleep(1) 
  }
  else 
{   if(!is.null(nytS)) 
  {nytSearch <-nytS
}
}}



### Pull the comments of the first 10 articles...


nytComments<-data.frame(matrix(ncol=24,nrow=0))

for(i in 1:10)
{
  maxComments=0
  url<-nytSearch[i,"response.docs.web_url"]
commenturl<-paste0("http://api.nytimes.com/svc/community/v3/user-content/url.json?api-key=",key,"&url=",url)

### need to know the number of pages of comments.
initialQuery<-tryCatch({jsonlite::stream_in(url(commenturl),flatten = TRUE) %>% data.frame() },error= function(cond){print("Error."); return(NULL)})
maxComments <- round((initialQuery$results.totalCommentsReturned / 10)-1) 
Sys.sleep(5) 
### if there are comments, pull them
if(!is.null(maxComments) & length(maxComments) >0) 
  {if(maxComments>-1)
{for(j in 0:maxComments)
{
nytC <- tryCatch({jsonlite::stream_in(url(paste0(commenturl, "&page=", j)),flatten = TRUE) %>% data.frame()},error= function(cond) return(NULL))
#make sure we can map each set of comments to the article url
   if(!is.null(nytC) & length(nytComments)>0)
      #append to the master list of comments
    {  
     nytC$url<-url
     nytComments<-rbind(nytComments,nytC)
    }
    else
      {if(!is.null(nytC))
{ 
  nytC$url<-url
  nytComments<-nytC

      }}
    Sys.sleep(4) 

}
}}

}


### The comments data frame has all the comments in a list of lists. Now, time to deconstruct it...


allcomments<-data.frame(matrix(nrow=0,ncol=0))
for(g in 1:nrow(nytComments))
{
x<-do.call(rbind, nytComments$results.comments[g])
x$url<-nytComments$url[g]
if(!is.null(allcomments) & length(allcomments)>0)
{
  allcomments<-rbind(allcomments,x)
}
else
  allcomments<-x
}

### Write the data

library(readr)
#write.csv(allcomments[c(4,5,6,8,9,10,11,12,14,15,17,21,22,23,25,27,29)],"nytComments.csv",row.names=FALSE)
#write.csv(nytSearch[c(3,4,5,6,9,11,12,13,14,16,17,29,21,22,25,26,27,29,30,31)],"nytArticles.csv",row.names=FALSE)
write_rds(nytSearch,"nytSearch.rds")
write_rds(allcomments,"ntyComments.rds")


### Visualize the data

### Use `ggplot2` and `dplyr` functions to select and summarise features.



detach("package:plyr", unload=TRUE) #dplyr and plyr both have functions called summarise

nytSearch %>% 
  group_by(response.docs.type_of_material) %>%
  summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=response.docs.type_of_material, fill=response.docs.type_of_material), stat = "identity") + coord_flip()


nytSearch %>%
  mutate(pubDay=gsub("T.*","",response.docs.pub_date)) %>%
  group_by(pubDay) %>%
  summarise(count=n()) %>%
  #filter(count >= 2) %>%
  ggplot() +
  geom_bar(aes(x=pubDay, y=count), stat="identity") + coord_flip()


### Word Clouds

### Use `tm` `SnowballC` `RColorBrewer` `wordcloud` to generate word clouds.


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


### Load the data as a corpus
articles <- Corpus(VectorSource(nytSearch$response.docs.lead_paragraph))
comments <- Corpus(VectorSource(allcomments$commentBody))

### Text transformation
### Transformation is performed using tm_map() function to replace, for example, special characters from the text.

### Replacing "/", "@" and "|" with space:
  
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

articles <- tm_map(articles, toSpace, "/")
articles <- tm_map(articles, toSpace, "@")
articles <- tm_map(articles, toSpace, "\\|")
articles <- tm_map(articles, toSpace, "<")
articles <- tm_map(articles, toSpace, ">")


comments <- tm_map(comments, toSpace, "/")
comments <- tm_map(comments, toSpace, "@")
comments <- tm_map(comments, toSpace, "\\|")
comments <- tm_map(comments, toSpace, "<")
comments <- tm_map(comments, toSpace, ">")


#text cleaning
# Convert the text to lower case
articles <- tm_map(articles, content_transformer(tolower))
# Remove numbers
articles <- tm_map(articles, removeNumbers)
# Remove english common stopwords
articles <- tm_map(articles, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
articles <- tm_map(articles, removeWords, c("br", "https","http","www")) 
# Remove punctuations
articles <- tm_map(articles, removePunctuation)
# Eliminate extra white spaces
articles <- tm_map(articles, stripWhitespace)
# Text stemming
articles <- tm_map(articles, stemDocument)


#text cleaning
# Convert the text to lower case
comments <- tm_map(comments, content_transformer(tolower))
# Remove numbers
comments <- tm_map(comments, removeNumbers)
# Remove english common stopwords
comments <- tm_map(comments, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
comments <- tm_map(comments, removeWords, c("br", "https","http","www")) 
# Remove punctuations
comments <- tm_map(comments, removePunctuation)
# Eliminate extra white spaces
comments <- tm_map(comments, stripWhitespace)
# Text stemming
comments <- tm_map(comments, stemDocument)




### term document matrix

dtm <- TermDocumentMatrix(articles)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

dtm_articles<-dtm
d_articles<-d

dtm <- TermDocumentMatrix(comments)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

dtm_comments<-dtm
d_comments<-d

set.seed(1234)
wordcloud(words = d_articles$word, freq = d_articles$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))





###Trajectory

Use the NIPALS algorithm. 

library(nipals)
res_articles<-nipals(dtm_articles,ncomp=2)
res_comments<-nipals(dtm_comments,ncomp=2)

###Try it yourself!
### Next:
### Standardize scores and loadings

### Map scores to days and loadings to words

### Visualize, connect days by lines

### see traj1.png, traj2.png