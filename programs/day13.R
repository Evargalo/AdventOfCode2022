# Donnees ----

# v<-read_lines("data/data13test.txt")
v<-read_lines("data/data13.txt")
v<-c("",str_replace_all(v,"10","X")) # tous les nombres sur un seul caractère
n<-length(v)/3
d<-data.frame(X1=v[3*(1:n)-1],
              X2=v[3*(1:n)])

# A ----

# Tous les "10" ont été remplacés par "X".
numbers<-c(as.character(0:9),"X")
isNumber<-function(s1) return(s1 %in% numbers)
compareNumbers<-function(s1,s2){
  if(which(numbers==s1)<which(numbers==s2)) return(1)
  if(which(numbers==s1)>which(numbers==s2)) return(-1)
}

# Deux chaînes de caractères sont-elles bien rangées ?
compareItems<-function(s1,s2){ # 1: bien rangées, s1<s2 / -1: mal rangées, s1>s2 
  if(s1==s2) return(0) # just in case
  c1<-firstChar(s1)
  c2<-firstChar(s2)
  if(c1==c2) return(compareItems(rmFirstChar(s1),rmFirstChar(s2)))
  if(isNumber(c1) & isNumber(c2)) return(compareNumbers(c1,c2))
  if(isNumber(c1) & c2=="[") return(compareItems(paste0("[",c1,"]",rmFirstChar(s1)),s2))
  if(c1=="]") return(1)
  return(-compareItems(s2,s1)) 
}
d %<>% rowwise %>% mutate(r=compareItems(X1,X2)) %>% ungroup

# Nombre de paires bien rangées
sum(which(d$r==1))
# 5252

# B ----

map(.x = c(d$X1,d$X2),.f=compareItems,s2="[[2]]") %>% unlist->res1
map(.x = c(d$X1,d$X2),.f=compareItems,s2="[[6]]") %>% unlist->res2

# rang de [[2]] * rang de [[6]]
(sum(res1==1)+1)*(sum(res2==1)+2)
# 20592
