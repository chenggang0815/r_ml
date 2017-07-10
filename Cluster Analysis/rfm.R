#====================RFM==========================
data2 <- read.csv(file.choose())

head(data2);dim(data2)
summary(data2)
# data clean 
names(data2)
quantile(data2$borrow_times,probs = c(0.01,0.99))
data2[data2$borrow_times>33,2] = 33
quantile(data2$recent_day,probs = c(0.01,0.99))
data2[data2$recent_day>631,4] = 631
quantile(data2$total_xf,probs = c(0.01,0.99))
data2[data2$total_xf >302,3] = 302
s <- summary(data2)
s
write.csv(s,'smmary2.csv')
hist()
hist(data$borrow_times,freq=T)
#==================================visual
install.packages("scatterD3")  
library(scatterD3)  
scatterD3(data=result,x=tm_intrvl,y=avg_amt,lab=cluster,col_var=cluster,symbol_var=cluster,size_var= cnt)  
#=============3
getwd()
names(data2)
m3 <- kmeans(data2[,2:4],3)
c3 <- m3$centers;c3
plot(data$recent_day,data$total_xf,col=m3$cluster)
b3<- table(m3$cluster);b3
p3 <- prop.table(b3);p3
table3 <- rbind(b3,p3)
write.csv(table3,'t3.csv')
#================4
m4 <- kmeans(data2[,2:4],4)
c4 <- m4$centers;c4
b4 <- table(m4$cluster);b4
p4 <- prop.table(b4);p4
table4 <- rbind(b4,p4)
write.csv(table4,'t4.csv')
head(data2,100)
c4 <-   as.data.frame((m4$cluster))
names(c4) <- 'type'
dim(c4);head(c4,100)
user_type <- cbind(data2,c4)
write.csv(user_type,'user_type_A2.csv')

#================5
m5 <- kmeans(data2[,2:4],5)
c5 <- m5$centers;c5
b5 <- table(m5$cluster);b5
p5 <- prop.table(b5);p5
table5 <- rbind(b5,p5)
write.csv(table5,'t5.csv')

  #================6
m6 <- kmeans(data2[,2:4],6)
c6 <- m6$centers;c6
b6 <- table(m6$cluster);b6
p6 <- prop.table(b6)
table6 <- rbind(b6,p6)
write.csv(table6,'t6.csv')
  #================7
m7 <- kmeans(data2[,2:4],7)
c7 <- m7$centers
b7 <- table(m7$cluster);b7
p7 <- prop.table(b7)
table7 <- rbind(b7,p7)
write.csv(table7,'t7.csv')
  # ==============8
m8 <- kmeans(data2[,2:4],8)
c8 <- m8$centers
b8 <- table(m8$cluster);b8
p8 <- prop.table(b8)
table8 <- rbind(b8,p8)
write.csv(table8,'t8.csv')
c <- rbind(c3,c4,c5,c6,c7,c8)
write.csv(c,'center.csv')
  
  
  