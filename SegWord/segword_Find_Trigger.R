library(RMySQL)
library(jiebaR)
mycon <- dbConnect(MySQL(),dbname="decision",username ="chenggang",password="*****",host = "***",port =3306)
dbSendQuery(mycon,"SET NAMES gbk")  #==="set names utf8" not work 
dbGetQuery(mycon, "SELECT * FROM Invalid_Chat_Record limit 0,3")  
# chat1 <- dbGetQuery(mycon, "select message_content from Invalid_Chat_Record limit 1,100000;")  # load data,0.1 million every time
# chat2 <- dbGetQuery(mycon, "select message_content from Invalid_Chat_Record limit 100001,200000;")  # load data,0.1 million every time
# chat3 <- dbGetQuery(mycon, "select message_content from Invalid_Chat_Record limit 300001,300000;")  # load data,0.1 million every time
# chat4 <- dbGetQuery(mycon, "select message_content from Invalid_Chat_Record limit 600001,400000;")  # load data,0.1 million every time
# chat5 <- dbGetQuery(mycon, "select message_content from Invalid_Chat_Record limit 1000001,600000;")  # load data,0.1 million every time
# chat6 <- dbGetQuery(mycon, "select message_content from Invalid_Chat_Record limit 1600001,610000;")  # load data,0.1 million every time
# chat <- rband(chat1,chat2,chat3,chat4,cha5,cha6)
chat <- dbGetQuery(mycon, "select message_content from Invalid_Chat_Record limit 0,2208765;")  # load data,2.2 million take 45s
dim(chat);tail(chat,10);class(chat)
#===============================找出触发句=============================
index <- grep("您咨询", chat$message_content)-1
length(index)#344967

trigger_Invalid <- as.data.frame(chat[index,],stringsAsFactors = F)
names(trigger_Invalid) <- "message_content"
#===========================delete =================================
index2 <- grep("您咨询", trigger_Invalid$message_content)
trigger_Invalid2 <- as.data.frame(trigger_Invalid[-index2,],stringsAsFactors = F)
names(trigger_Invalid2) <- "message_content"
class(trigger_Invalid2);dim(trigger_Invalid2)
#================================对触发局分词======================================
engine <- worker()
segwords <- sapply(trigger_Invalid2, segment, engine)
wf <- unlist(segwords)
wf <- as.data.frame(table(wf),stringsAsFactors = F)
wf <- wf[order(-wf$Freq),]#order 
View(wf)
wf[which(nchar(wf$wf)==1),]#return index that nchar =1
dele_one_wf <- wf[-which(nchar(wf$wf)==1),]
#===================导出=============
View(dele_one_wf[1:100,])
write.csv(dele_one_wf,"trigger_Invalid.csv")
dbDisconnect(mycon) 
#=======================查看原话===========================
i <- grep("信息", chat$message_content)
chat[i[1:10],]

