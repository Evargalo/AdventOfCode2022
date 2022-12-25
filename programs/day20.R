# Donnees ----

d <- read_delim("data/data20.txt",col_names = FALSE, delim=' ')
# d <- read_delim("data/data20test.txt",col_names = FALSE, delim=' ')

v<-(d$X1)
length(v)->l
duplicated(v) %>% sum # 1352: careful, some values are duplicated
sum(v==0) # 1: But only one 0

# A ----

d %<>% rename(val=X1) %>% 
  mutate(pos=as.double(row_number()-1))
dd<-d
print(d %>% arrange(pos) %>% pull(val))

for(i in 1:l){
  val<-d[i,1] %>% unlist %>% unname
  pp<-d[i,2] %>% unlist %>% unname
  newpos<-(val+pp)
  while(newpos<0) newpos<-newpos+l-1
  while(newpos>l-1) newpos<-newpos-l+1
  d %<>% mutate(pos=if_else(
    pos %in% (pp:newpos),
    (pos-sign(newpos-pp))%%l,
    pos)) 
  d[i,2]<-newpos
  if(i%%500==0) print(i)
  # print(d %>% arrange(pos) %>% pull(val))
}

# sum of values 1000, 2000 and 3000 places after 0
pos0<-d %>% filter(val==0) %>% pull(pos) 
(pos0+1000*(1:3))%%l -> popo
d %>% filter(pos %in% popo) %>% summarise(sum(val))
# 15297


# B ----
dKey<-811589153
d<-dd
d %<>% mutate(val=dKey*val) %>% 
  mutate(nbFwd=val%%l+val%/%l)
calcNbFwd<-function(x){
  if(x %in% 0:l) return(x)
  return(calcNbFwd(x%%l + x%/%l))
}
# calcNbFwd(3500000)
# calcNbFwd(-3500000)
d %<>% rowwise %>% mutate(nbFwd=calcNbFwd(val)) %>% ungroup

for(j in 1:10){
  print("#######"); print(j)
  for(i in 1:l){
    val<-d[i,1] %>% unlist %>% unname
    pp<-d[i,2] %>% unlist %>% unname
    mf<-d[i,3] %>% unlist %>% unname
    newpos<-(pp+mf)
    while(newpos<0) newpos<-newpos+l-1
    while(newpos>l-1) newpos<-newpos-l+1
    d %<>% mutate(pos=if_else(pos %in% (pp:newpos),(pos-sign(newpos-pp))%%l,pos)) 
    d[i,2]<-newpos
    if(i%%500==0) print(i)
  }
  # print(d %>% arrange(pos) %>% pull(val))
}

# sum of values 1000, 2000 and 3000 places after 0
pos0<-d %>% filter(val==0) %>% pull(pos) 
(pos0+1000*(1:3))%%l -> popo
# popo[popo==0]<-l
d %>% filter(pos %in% popo) %>% summarise(sum(val))

# 2897373276210















# Bad data model ----
d %<>% mutate(X2=lag(X1)) %>% mutate(X2=coalesce(X2,last(X1))) %>% rename(val=X2,nex=X1) %>% mutate(nbFwd=val%%l)
d

getNext<-function(d,vv){
  d %>% filter(val==vv) %>% pull(nex)
}
moveFwd<-function(d,vv,n){
  if(n==0) return(d)
  getNext(d,vv)->nene
  getNext(d,nene)->tai
  d %<>% mutate(nex=case_when(
    val==vv~tai,
    val==nene~vv,
    TRUE ~ nex))
  return(moveFwd(d,vv,n-1))
} 
dd<-d
for(vv in v)  d<-moveFwd(d,vv,vv%%l)

p<-0
vv<-0
for(i in 1:3000){
  vv<-getNext(d,vv)
  if(i %% 1000 == 0) p<-p+vv
}
p

