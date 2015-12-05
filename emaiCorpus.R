#install.packages("topicmodels")
#install.packages("tm.plugin.mail")

library(stringr)
library(plyr)
library(tm)
library(tm.plugin.mail)


library(SnowballC)
library(topicmodels)




# get date of email
get.date <- function(msg.vec) {
  date.grep <- grepl("^Date: ", msg.vec)
  date.grepl <- which(date.grep == TRUE)
  date <- msg.vec[date.grep]
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  date <- gsub("^\\s+|\\s+$", "", date)
  #return(date)
  return(strtrim(date, 25))
}

# get subject of email
get.subject <- function(msg.vec) {
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if(length(subj) > 0) {
    return(strsplit(subj, "Subject: ")[[1]][2])
    #return(subj)
  }
  else {
    return("")
  }
}

#get the message of email
get.msg <- function(msg.vec) {
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
  return(paste(msg, collapse="\n"))
}

#get the sender email 
get.from <- function(msg.vec) {
  from <- msg.vec[grepl("From: ", msg.vec)]
  from <- strsplit(from, '[":<> ]')[[1]]
  from <- from[which(from !="" & from !=" ")]
  return(from[grepl("@", from)][1])
}
         
#fully message of email        
msg.full <- function(path) {
  con <- file(path, open="rt", encoding="latin1")
  msg <- readLines(con)
  close(con)
  return(msg)
}

parse.email <- function(path) {
  full.msg <- msg.full(path)
  date <- get.date(full.msg)
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  #return(c(date, from, subj, msg, path))
  return(c(date,from, subj, msg, path, full.msg))
}

#email_txts
email_txts[200]
#parse.email(paste(data.path,email_txts[1]))
parse.email("/Users/naru/Documents/R_workshop/SpamFilter/data/1423436694.12029_41.ivanova")
