###ngram函數 test為你要分析的文本(文字向量) n為要以幾個字分一次(預設兩個字)
ngram = function(test,n=2){
  if (length(test) == 1 & is.character(test) == TRUE){
    
             a = sapply(1:(nchar(test)-(n-1)),function(s) substr(x = test,start = s,stop = (s+n-1)))
    
       return(a)
             
  }else if(length(test) > 1 & is.character(test) == TRUE){
    
          a <- vector("list",length(test))
          
  for (i in 1:length(test)){
      
         a[[i]] = sapply(1:(nchar(test[i])-(n-1)),function(s) substr(x = test[i],start = s,stop = (s+n-1)))
       
      } 
          return(a)
  }else{
        print("格式錯誤")
  }
}

##範例一
txt =  c("台北大晴天","新北好天氣","abcde")
ngram(txt)

##示範何時錯誤
error_ex = c(1,2,3)
ngram(error_ex)
