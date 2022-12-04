# Donnees ----

d <- read_delim("data4.txt",col_names = FALSE, delim=',')
d <- read_delim("data4test.txt",col_names = FALSE, delim=',')


# A ----
d %<>% mutate(overlap1= X1<=X3 & X2>=X4, # second range included in the first one
              overlap2= X1>=X3 & X2<=X4, # first range included in the second one
              overlap= overlap1 | overlap2)

sum(d$overlap)
# 556


# B ----

d %>% summarise(sum(X1<=X4 & X3<=X2))
# 876


# B en moins efficace ----
d %<>% mutate(overlap3= X1<=X3 & X3<=X2 & X2<=X4, # end of first range overlaps the beginning of the second one
              overlap4= X3<=X1 & X1<=X4 & X4<=X2, # end of second range overlaps the beginning of the first one
              overlap= overlap1 | overlap2 | overlap3 | overlap4)
sum(d$overlap)
# 876



# Trucs inutiles parce que j'ai mal lu l'énoncé... ----

# amplitude des chevauchements ----
d %<>% mutate(
  sizeOverlap=case_when(
    !overlap ~ 0,
    overlap1 ~ X4-X3+1,
    overlap2 ~ X2-X1+1,
    overlap3 ~ X2-X3+1,
    overlap4 ~ X4-X1+1
  )
)
sum(d$sizeOverlap)

# Nombre de tâches inclues dans des chevauchements ----
d <- read_delim("data4.txt",col_names = FALSE, delim=',')
min(d$X1,d$X2,d$X3,d$X4)
max(d$X1,d$X2,d$X3,d$X4)

v<-1:99
res<-rep(FALSE,99)
buildOverlaps<-function(X1,X2,X3,X4){
 res<<-res | (v>=X1 & v>=X3 & v<=X2 & v<=X4)
}
pmap(d,buildOverlaps)
sum(res)

# Deuxième méthode...
d <- read_delim("data4.txt",col_names = FALSE, delim=',')
v<-1:99
res<-rep(FALSE,99)

for(x in v){
  d %<>% mutate(inside=(x>=X1 & x>=X3 & x<=X2 & x<=X4))
  res[x]<-(any(d$inside))
}
sum(res)

# 