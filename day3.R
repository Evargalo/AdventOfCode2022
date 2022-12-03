# Donnees ----

d <- read_delim("data3.txt",col_names = FALSE, delim=' ')
v<-d$X1
allLetters<-c(letters,LETTERS)

# A ----

score<-0
for(a in v){
  cutString(a,2)->b
  b1<-str_split(b[1],'') %>% unlist 
  b2<-str_split(b[2],'') %>% unlist 
  c<-b1[b1 %in% b2]
  score<-score+which(allLetters==c)
}

score

# 7793


# B ----

l<-length(v)
score<-0
for(i in 1:(l/3)){
  b1<-str_split(v[3*i-2],'') %>% unlist 
  b2<-str_split(v[3*i-1],'') %>% unlist 
  b3<-str_split(v[3*i],'') %>% unlist 
  c<-b1[b1 %in% b2 & b1 %in% b3]
  score<-score+which(allLetters==c)
}
score

# 2499