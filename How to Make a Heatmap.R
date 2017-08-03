#How to Make a Heatmap – a Quick and Easy Solution
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")
head(nba)
str(nba)
View(nba)
nba <- nba[order(nba$PTS),]
row.names(nba) <- nba$Name
head(nba)
nba <- nba[,2:20]
nba_matrix <- data.matrix(nba)
nba_heatmap <- heatmap(nba_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
#Maybe you want a different color scheme. Just change the argument to col, which is cm.colors(256) in the line of code we just executed. Type ?cm.colors for help on what colors R offers. 
#For example, you could use more heat-looking colors:
nba_heatmap <- heatmap(nba_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))
