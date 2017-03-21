library(RMySQL)
library(jiebaR)
mycon <- dbConnect(MySQL(),dbname="decision",username ="chenggang",password="*******8",host = "********",port =3306)
dbSendQuery(mycon,"SET NAMES gbk")  #==="set names utf8" not work 
dbGetQuery(mycon, "SELECT * FROM Invalid_Chat_Record limit 0,3")  
dbGetQuery(mycon,"select FROM_UNIXTIME(addtime,'%Y年%m月%d ') as addtime  from Invalid_Chat_Record")
chat <- dbGetQuery(mycon, "select message_content,addtime from Invalid_Chat_Record limit 0,2208765;")  # load data,2.2 million take 45s
dim(chat);tail(chat,10);class(chat)
#==============================================================
index <- grep("您咨询", chat$message_content)-1
index[1:10]
length(index)#344967
trigger_Invalid <- as.data.frame(chat[index,],stringsAsFactors = F)
names(trigger_Invalid) <- c("message_content","datetime")
#========================delete one word ===========================
dele_one_word <- function(data){
  data <- data[-which(nchar(data[,1])==1),]
  trigger_Invalid <<- data
}
#========================delete two word ===========================
dele_two_word <- function(data){
  data <- data[-which(nchar(data[,1])==2),]
  trigger_Invalid <<- data
}

#===========================delete invalid word =================================
dele_invalid_word <- function(data,word) {
     index <- grep(word, data[,1])
     if(length(index) ==0){
       print("这个词语已被删除")
     }else{
     trigger_Invalid <<- as.data.frame(data[-index,],stringsAsFactors = F)
     n <- length(trigger_Invalid[,1])
     print(paste("已删除，删除后数据量为:",n))
     }
     }
Test_data <- function(data,word){
  index <- grep(word, data[,1])
  if(length(index) == 0){
    print("不包含这个单词!")
  }
  else{
    w <- "包含这个词语,且数据量为："
    print(paste(w,length(index)))
  }
  n <- length(data[,1])
  print(paste("数据总量为:",n))
}
#================================================================================
dele_one_word(trigger_Invalid)#delete one word
head(trigger_Invalid)
class(trigger_Invalid);dim(trigger_Invalid)

#=============================================delete invalid word ===============
dele_invalid_word(trigger_Invalid,"马上为您转接")
Test_data(trigger_Invalid,"马上为您转接")

dele_invalid_word(trigger_Invalid,"1.")
Test_data(trigger_Invalid,"1.")
dele_invalid_word(trigger_Invalid,"audio")
Test_data(trigger_Invalid,"audio")

dele_two_word(trigger_Invalid)
dim(trigger_Invalid)

View(trigger_Invalid[200:2000,])
#=================================seg word===============================
engine <- worker()
segwords <- sapply(trigger_Invalid$message_content, segment, engine)
segwords[1:10]

#==============================removewords===============================
removewords <- function(target_words,stop_words){
  target_words = target_words[target_words%in%stop_words==FALSE]
  return(target_words)
}

stop <- c("是","么")
stopwords <- as.data.frame(stop)
stopwords
segwords2 <- sapply(segwords, removewords, stopwords)
segwords2[1:5]
a <- segwords2[[4]]
class(a)
#============================================






