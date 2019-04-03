library(dplyr)
library(text2vec)
library(tm)
library(wordcloud2)
##資料處理 
stop=gsub(pattern = "[A-Za-z]", "", text2) ###利用這行找出不需要的東西

text = feder[,4]
text2 = unlist(text)
text2 = gsub("\\s", " ", text2)



text2 = gsub("[^[:alpha:][:space:]']", " ", text2)
text2 = gsub("'+"," ",text2)
text3 = gsub("\\s{2,}","",text2)
text4 = strsplit(text3,split = " ")
text5 = unlist(text4)

text5[which(nchar(text5)>15)] %>% View()




##tf矩陣
tf = as.data.frame(matrix(ncol = 1,nrow=length(unique(text5))))
colnames(tf) = c("word")
tf[,1] = unique(text5)

tt <- paste0("text", 1:length(text4))
for (i in 1:length(text4)){
  w = data.frame(table(text4[i]))
  colnames(w) = c("word","freq")
  tf = merge(tf,w,all.x = T)
  colnames(tf) <- c("word", tt[1:i])
}
tf[is.na(tf)] = 0
rownames(tf) = sort(unique(text5))
tf = tf[,-1]
finaltf = data.frame()
finaltf = as.data.frame(t(apply(tf,1,function(x) x/sum(x))))

###idf矩陣
q = data.frame()
q = ifelse(tf[1:nrow(tf),2:ncol(tf)] > 0,1,0)
e = as.data.frame(matrix(ncol = 1,nrow=length(unique(text5))))
rownames(e) = sort(unique(text5))
idf = cbind(e,q)[,-1]
finalidf = data.frame(apply(idf,1,function(x) log(ncol(finaltf)/sum(x))))
for (i in 1:nrow(finalidf)){
  finalidf[i,1] = ifelse(finalidf[i,1] == Inf , log(86/(sum(idf[i,]+0.5))),finalidf[i,1])
}
colnames(finalidf) = "idfv"



##### df矩陣
r = as.data.frame(matrix(ncol = 1,nrow=length(unique(text5))))
rownames(e) = sort(unique(text5))
df = cbind(e,q)[,-1]
finaldf = data.frame(apply(df,1,function(x) sum(x)/log(ncol(finaltf))))
colnames(finaldf) = "dfv"




###tf-idf矩陣
tfidf = data.frame()
tfidf = sweep(finaltf,1,finalidf[,1],'*')
rownames(tfidf) = sort(unique(text5))
colnames(tfidf) = tt



###tf-df矩陣
tfdf = data.frame()
tfdf = sweep(finaltf,1,finaldf[,1],'*')
rownames(tfdf) = sort(unique(text5))
colnames(tfdf) = tt

##擷取前n大的字tfidf
tmp01 <- tfidf
tt1 <- tmp01$text18 %>% order(., decreasing = T)
tmp02 <- data.frame(t18 = tmp01$text18[tt1])
rownames(tmp02) <- rownames(tmp01)[tt1]
tmp02 %>% head(10)


##擷取前n大的字tfdf
tmp01 <- tfdf
tt1 <- tmp01$text15 %>% order(., decreasing = T)
tmp02 <- data.frame(t15 = tmp01$text15[tt1])
rownames(tmp02) <- rownames(tmp01)[tt1]
tmp02 %>% head(10)


#輸出成ecxel

write.csv(tfidf , file = "tfidf.csv")
write.csv(tfdf , file = "tfdf.csv")

##歐式距離

##標準化
w = tfidf[1:3,1:3]
w = apply(w,2,function(x) sum(x)/sqrt(var(x)))
dist(w,p = 2)


##文字雲

length(finaldf[,1])
e = data.frame()
x = data.frame(c(sort(unique(text5))))
df = cbind(x,finaldf)
wordcloud2(df)



