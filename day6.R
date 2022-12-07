# Donnees ----
s<-read_lines("data6.txt")
x<-strsplit(s,'') %>% unlist

# A ----
search<-function(x,size){
  for(i in size:length(x)){
    if(length(unique(x[(i-size+1):i]))==size) return (i)
  }
}
search(x,4)
# 1802

# B ----
search(x,14)
# 3551