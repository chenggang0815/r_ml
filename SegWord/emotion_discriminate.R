#==============load packages===========================
install.packages("jiebaR")# invalid
.libPaths()# return path of installed packages 
library(jiebaR)
library(RMySQL)
library(plyr)
getwd()
setwd('D:/R')
#====================read data from mysql======================================
mycon <- dbConnect(MySQL(),dbname="decision",username ="risk",password="risk.1234",host = "192.168.1.4",port =3306)
dbReadTable(mycon,"cg_test")
dbListTables(mycon) #show all tables in the database
dbGetQuery(mycon, "SELECT * FROM cg_huanxin_all_2 limit 0,1")  
dbSendQuery(mycon,"SET NAMES gbk")  #==="set names utf8" not work 
chat <- dbGetQuery(mycon, "select * from cg_huanxin_all_2 ")  # load data,1.15 million less than one minute
# ===========启动引擎
engine <- worker()
#添加词典
emotion <- read.csv(file.choose(),stringsAsFactors = F)
head(emotion)
class(emotion)
pos <- emotion$positive;neg <- emotion$negative;
mydict <- c(pos, neg)
class(mydict);class(pos);class(neg)
mydict
class(emotion$negative)
new_user_word(engine, mydict)
#================自定义情感函数
fun <- function(x,y) x %in% y
getEmotionalType <- function(x,pwords,nwords){ 
  pos.weight = sapply(llply(x,fun,pwords),sum)
  neg.weight = sapply(llply(x,fun,nwords),sum)
  total = pos.weight - neg.weight
  return(data.frame(pos.weight, neg.weight, total)) 
}

#====================celan data =======================================
head(chat)
dim(chat)
chat_messgae <- as.data.frame(chat$message_content,stringsAsFactors = F)
class(chat_messgae);dim(chat_messgae);head(chat_messgae)
#===================seg word===========================================
segwords <- sapply(chat_messgae$`chat$message_content`, segment, engine)

#class(segwords);dim(segwords)
#wf <- unlist(segwords)
#wf <- as.data.frame(table(wf),stringsAsFactors = F)
#wf <- wf[order(-wf$Freq),]#order 
#View(wf)
#wf[which(nchar(wf$wf)==1),]#return index that nchar =1
#dele_one_wf <- wf[-which(nchar(wf$wf)==1),]
#================计算情感得分
length(segwords)
class(segwords)
segwords1 <- segwords[1:10000]
segwords2 <- segwords[10001:20000]
segwords3 <- segwords[20001:30000]
segwords4 <- segwords[30001:40000]
segwords5 <- segwords[40001:44965]
class(segwords1)
system.time(score1 <- getEmotionalType(segwords1, pos, neg))
class(score1)
system.time(score2 <- getEmotionalType(segwords2, pos, neg))
system.time(score3 <- getEmotionalType(segwords3, pos, neg))
system.time(score4 <- getEmotionalType(segwords4, pos, neg))
system.time(score5 <- getEmotionalType(segwords5, pos, neg))

score <- rbind(score1,score2,score3,score4,score5)
head(score)
evalu.score <- cbind(chat_messgae, score)
head(evalu.score)
##
evalu.score <- transform(evalu.score,emotion = ifelse(total>=0, 'Pos', 'Neg'))
# 随机挑选10条评论，做一个验证
set.seed(1)
validation <- evalu.score[sample(1:nrow(evalu.score),size = 10),]
View(validation)

evalu.score[which(evalu.score[,5]=='Neg'),]
summary(evalu.score)
#====================export data =====================
View(dele_one_wf)
View(dele_one_wf[1:100,])
write.csv(dele_one_wf,"seg_word.csv")
dbDisconnect(mycon) 
