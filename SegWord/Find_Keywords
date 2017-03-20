#==========================load data=========================================
word_weigth <- read.csv(file.choose(),header = T,stringsAsFactors = F)  #加载语料库与权重
trigger_Invalid <- read.csv(file.choose(),header = T,stringsAsFactors = F) 
names(trigger_Invalid) <- c("word","Freq")
head(trigger_Invalid);head(word_weigth)
dim(word_weigth);dim(trigger_Invalid)
View(trigger_Invalid)
#==========================define function==========================
FindKeyword <- function(weigth_data, trigger_data) {
  lentrigger <- length(trigger_data[, 1])
  lenweight <- length(weigth_data[, 1])
  for (i in 1:lentrigger) {
    for (j in 1:lenweight) {
      
      if (trigger_data[i, 1] == weigth_data[j, 1]) {
        trigger_data$wei[i] <- trigger_data[i, 2] * weigth_data[j, 3]
        print(i)
      } else {
        trigger_data$weight[i] <- 0
      }
    }
  }
  
  trigger_key_word <<- trigger_data  #<<- define global variable
}
#============================test function==========================
head(trigger_Invalid);head(word_weigth)
trigger_Invalid1 <- trigger_Invalid[1:8,]
word_weigth1 <- word_weigth[1:10,]
trigger_Invalid1;word_weigth1
FindKeyword(word_weigth1,trigger_Invalid1)
rm(trigger_key_word)
View(trigger_key_word)
#=====================clean trigger_key_word==============================================
FindKeyword(word_weigth,trigger_Invalid)
trigger_key_word <- trigger_key_word[,c(1,2,4)]
dim(trigger_key_word)
head(trigger_key_word)
View(trigger_key_word)
#========================verification===============================
Varification <- function(word){
  var1 <- trigger_key_word[which(trigger_key_word[,1]==word),3]
  if(
    var1 ==0
  ){
    print("这个词语不在语料库中")
  }else{
    var <- word_weigth[which(word_weigth$word==word),3]*trigger_Invalid[which(trigger_Invalid$word==word),2]
    if(var == var1){
      print("验证成功")
    }else{
      print("验证失败")
    } 
  }
}
Varification("申请")
#===========================sort======================================
trigger_key_word <- trigger_key_word[order(-trigger_key_word$wei),]#order 
View(trigger_key_word)
