# Donnees ----

d <- read_delim("data/data13.txt",col_names = FALSE, delim=',')
# d <- read_delim("data/data13test.txt",col_names = FALSE, delim=',')
v<-read_lines("data/data13test.txt")
v<-read_lines("data/data13.txt")
v<-c("",v)
n<-length(v)/3
d<-data.frame(X1=v[2+3*(0:(n-1))],X2=v[3*(1:n)])

# A ----

# Tous les "10" ont été remplacés par "X" pour être représentés sur un seul caractère.
numbers<-c(as.character(0:9),"X")
isNumber<-function(s1) return(s1 %in% numbers)
compareNumbers<-function(s1,s2){
  if(which(numbers==s1)==which(numbers==s2)) return(0)
  if(which(numbers==s1)<which(numbers==s2)) return(1)
  if(which(numbers==s1)>which(numbers==s2)) return(-1)
}

# Deux chaînes de caractères sont-elles bien rangées ?
compareItems<-function(s1,s2){ # 1: bien rangées, s1<s2 / -1: mal rangées, s1>s2 
  if(s1==s2) return(0) # just in case
  c1<-firstChar(s1)
  c2<-firstChar(s2)
  if(c1==c2) return(compareItems(cutFirstChar(s1),cutFirstChar(s2)))
  if(isNumber(c1) & isNumber(c2)) return(compareNumbers(c1,c2))
  if(isNumber(c1) & c2=="[") return(compareItems(paste0("[",c1,"]",cutFirstChar(s1)),s2))
  if(c1=="]") return(1)
  return(-compareItems(s2,s1))  
}

pmap(d %>% select(s1=X1,s2=X2),compareItems) %>% unlist->res
# Nombre de paires bien rangées
sum(which(res==1))
# 5252

# B ----

pmap(d %>% select(s1=X1),compareItems,s2="[[2]]") %>% unlist->res1
pmap(d %>% select(s1=X2),compareItems,s2="[[2]]") %>% unlist->res11
pmap(d %>% select(s1=X1),compareItems,s2="[[6]]") %>% unlist->res2
pmap(d %>% select(s1=X2),compareItems,s2="[[6]]") %>% unlist->res22

(sum(res1==1)+sum(res11==1)+1)*(sum(res2==1)+sum(res22==1)+2)
# 20592
