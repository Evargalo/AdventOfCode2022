# Donnees ----

d <- read_delim("data3.txt",col_names = FALSE, delim=' ')
v<-d$X1

# A ----

a<-(v[1])

sol<-c()
score<-0
for(a in v){
  cutString(a,2)->b
  b1<-str_split(b[1],'') %>% unlist 
  b2<-str_split(b[2],'') %>% unlist 
  c<-b1[b1 %in% b2]
  sol<-c(sol,c)
  if(c %in% letters){
    score<-score+which(letters==c)
  } else {
    score<-score+which(LETTERS==c)+26
  }
}

sum(score)

# 7793


# B ----

l<-length(v)
sol<-c()
score<-0
for(i in 1:(l/3)){
  cutString(a,2)->b
  b1<-str_split(v[3*i-2],'') %>% unlist 
  b2<-str_split(v[3*i-1],'') %>% unlist 
  b3<-str_split(v[3*i],'') %>% unlist 
  c<-b1[b1 %in% b2 & b1 %in% b3]
  sol<-c(sol,c)
  if(c %in% letters){
    score<-score+which(letters==c)
  } else {
    score<-score+which(LETTERS==c)+26
  }
}
score

# 2499