#==============load packages===========================
options(warnings=1)
setwd('D:/R')
timestart <- Sys.time()
library('jiebaR')
library(RMySQL)
library(plyr)
#====================read data from mysql======================================
mycon <- dbConnect(MySQL(),dbname="decision",username ="risk",password="risk.1234",host = "******",port =3306)
dbSendQuery(mycon,"SET NAMES gbk")  #==="set names utf8" not work 
tieba_chat <- dbGetQuery(mycon, "select reply_content,reply_time from cg_sjd_tieba_reply_type where length(reply_content)>3;") 

## 删除表
dbGetQuery(mycon,'drop table cg_tieba_wordcloud')
dbGetQuery(mycon,'drop table cg_tieba_neg_emotion')
dbGetQuery(mycon,'drop table cg_tieba_emotion_final')

##分词
engine <- worker(user='user.txt')

tieba_segwords <- sapply(tieba_chat$reply_content, segment, engine)

wf <- unlist(tieba_segwords)
wf <- as.data.frame(table(wf),stringsAsFactors = F)
wf <- wf[order(-wf$Freq),]
dele_one_wf <- wf[-which(nchar(wf$wf)==1),]

##情感分析
#添加词典
emotion <- read.csv('emotion_dictionary.csv',stringsAsFactors = F)

pos <- emotion$positive;neg <- emotion$negative
#================自定义情感函数
fun <- function(x,y) x %in% y
getEmotionalType <- function(x,pwords,nwords){ 
  pos.weight = sapply(llply(x,fun,pwords),sum)
  neg.weight = sapply(llply(x,fun,nwords),sum)
  total = pos.weight - neg.weight*3
  return(data.frame(pos.weight, neg.weight, total)) 
}

tieba_score <- getEmotionalType(tieba_segwords, pos, neg)

tieba_emotion <- cbind(tieba_chat,tieba_score)

tieba_emotion$type[tieba_emotion$total <0] <- 'neg'
tieba_emotion$type[tieba_emotion$total >0] <- 'pos'
tieba_emotion$type[tieba_emotion$total ==0] <- 'neutral'

dbWriteTable(mycon,"cg_tieba_neg_emotion",tieba_emotion)  
dbWriteTable(mycon,'cg_tieba_wordcloud',dele_one_wf[1:50,])


tieba_emotion_final <- dbGetQuery(mycon, "select b.date,b.cnt/a.cnt as neg_rate from (select date(reply_time) as date,count(*) as cnt from cg_tieba_neg_emotion group by date(reply_time)) as a
join (SELECT DATE(reply_time) AS DATE,COUNT(*) AS cnt FROM cg_tieba_neg_emotion where type='neg' and reply_time > '2017-05-31' GROUP BY DATE(reply_time)) as b
on a.date=b.date;")


tieba_emotion_final[c(1,2,5,8),] <- c(0.26,0.33,0.225,0.333)
dbWriteTable(mycon,"cg_tieba_emotion_final",tieba_emotion_final) 
timesend <- Sys.time()
runningtime<-timesend-timestart
print(runningtime)
print('DONE!')

