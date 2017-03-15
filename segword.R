#==============load packages===========================
install.packages("jiebaR")# invalid
.libPaths()# return path about installed packages 
library(jiebaR)
#====================read data from mysql======================================
mycon <- dbConnect(MySQL(),dbname="decision",username ="chenggang",password="*Cheng2017",host = "10.139.101.142",port =3306)
dbReadTable(mycon,"cg_test")
dbListTables(mycon) #show all tables in the database
dbGetQuery(mycon, "SELECT * FROM cg_test limit 0,1")  
dbSendQuery(mycon,"SET NAMES gbk")  #==="set names utf8" not work 
dbGetQuery(mycon, "desc zrx_gap_6;")  
chat1 <- dbGetQuery(mycon, "select message_content,addtime from zrx_gap_6 limit 600000,100000;")  # load data,0.1 million every time
chat2 <- dbGetQuery(mycon, "select message_content,addtime from zrx_gap_6 limit 700001,100000;")  
chat3 <- dbGetQuery(mycon, "select message_content,addtime from zrx_gap_6 limit 800001,100000;")  
chat4 <- dbGetQuery(mycon, "select message_content,addtime from zrx_gap_6 limit 900001,100000;") 
chat5 <- dbGetQuery(mycon, "select message_content,addtime from zrx_gap_6 limit 1000001,100000;") 
chat6 <- dbGetQuery(mycon, "select message_content,addtime from zrx_gap_6 limit 1100001,100000;")  
#====================celan data =======================================
chat <- rbind(chat1,chat2,chat3,chat4,chat5,chat6)
head(chat)
dim(chat)
chat_messgae <- as.data.frame(chat$message_content,stringsAsFactors = F)
class(chat_messgae);dim(chat_messgae);names(chat_messgae) <- c("content");head(chat_messgae)
#===================seg word===========================================
engine <- worker()
segwords <- sapply(chat_messgae, segment, engine)
wf <- unlist(segwords)
wf <- as.data.frame(table(wf),stringsAsFactors = F)
wf <- wf[order(-wf$Freq),]#order 
View(wf)
wf[which(nchar(wf$wf)==1),]#return index that nchar =1
dele_one_wf <- wf[-which(nchar(wf$wf)==1),]
#====================export data =====================
View(dele_one_wf)
View(dele_one_wf[1:100,])
write.csv(dele_one_wf,"seg_word.csv")

