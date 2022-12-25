# Donnees ----

source("packages.R")

d <- read_delim("data/data21.txt",col_names = FALSE, delim=' ')
# d <- read_delim("data/data21test.txt",col_names = FALSE, delim=' ')

d %<>% mutate(X1=cutAfterLast(X1,":")) 
numbers<-d %>% filter(is.na(X3)) %>% 
  mutate(X2=as.numeric(X2)) %>% 
  select(name=X1,val=X2)
ope<-d %>% filter(!is.na(X3))

# A ----
nbSvg<-numbers
opeSvg<-ope
searchOpe<-function(X1,X2,X3,X4){
  if(X2 %in% numbers$name & X4 %in% numbers$name){
    v1<-numbers %>% filter(name==X2) %>% pull(val)
    v2<-numbers %>% filter(name==X4) %>% pull(val)
    if(X3=="+") res<-v1+v2
    if(X3=="-") res<-v1-v2
    if(X3=="*") res<-v1*v2
    if(X3=="/") res<-v1%/%v2
    x<-X1
    ope<<-ope %>% filter(X1!=x)
    numbers<<-numbers %>% add_row(name=x,val=res)
  }
}
n<-nrow(ope)
while("root" %in% ope$X1){
  pmap(ope,searchOpe)
  if(nrow(ope)==n) break
  n<-nrow(ope)
  print(n)
}

numbers %>% filter(name=="root") %>% pull(val)
# 157714751182692

# B ----

# Simplify data by first calculating everything that doesn't need "humn"

ope<-opeSvg
numbers2<-nbSvg %>% filter(name!="humn")

searchOpe2<-function(X1,X2,X3,X4){
  if(X2 %in% numbers2$name & X4 %in% numbers2$name){
    v1<-numbers2 %>% filter(name==X2) %>% pull(val)
    v2<-numbers2 %>% filter(name==X4) %>% pull(val)
    if(X3=="+") res<-v1+v2
    if(X3=="-") res<-v1-v2
    if(X3=="*") res<-v1*v2
    if(X3=="/") res<-v1%/%v2
    x<-X1
    ope<<-ope %>% filter(X1!=x)
    numbers2<<-numbers2 %>% add_row(name=x,val=res)
  }
}
n<-nrow(ope)
while("root" %in% ope$X1){
  pmap(ope,searchOpe2)
  if(nrow(ope)==n) break
  n<-nrow(ope)
  print(n)
}
# 66 ope instead of 1484 !
nbSvg2<-numbers2
opeSvg2<-ope 


# Reverse search: what goal do we wanna reach ?
opeSvg %>% filter(X1=="root") 
nbSvg2 %>% filter(name %in% c("rjmz","nfct"))
opeSvg2 %>% filter(X1 %in% c("rjmz","nfct"))
# One value already known, we set a goal for the other one
goal<-nbSvg2 %>% filter(name=="nfct") %>% pull(val)         
goals<-tibble(nam="rjmz",goa=goal)

nom<-"rjmz"
oldName<-""

while(nom!=oldName & nom!="humn"){
  oldName<-nom
  opeSvg2 %>% filter(X1 == nom) ->myLine
  myLine %>% pull(X2)->n1
  myLine %>% pull(X4)->n2
  # each time one operand is already known, we can update our goal by one step:
  if(n1 %in% nbSvg2$name){
    nom<-n2
    v1<-nbSvg2 %>% filter(name==n1) %>% pull(val)
    if(myLine$X3=="+") goal<-goal-v1
    if(myLine$X3=="-") goal<-v1-goal
    if(myLine$X3=="*") goal<-goal%/%v1
    if(myLine$X3=="/") goal<-v1%/%goal
  }
  if(n2 %in% nbSvg2$name){
    nom<-n1
    v1<-nbSvg2 %>% filter(name==n2) %>% pull(val)
    if(myLine$X3=="+") goal<-goal-v1
    if(myLine$X3=="-") goal<-v1+goal
    if(myLine$X3=="*") goal<-goal%/%v1
    if(myLine$X3=="/") goal<-v1*goal
  }
  goals %<>% add_row(nam=nom,goa=goal)
}
goals %>% tail
goals %>% filter(nam=="humn") %>% pull(goa)
# 3373767893067



# Naive approach ----
# OK for test, too slow for real data

# h<-301
for(h in 5856:100000){
  if(h %%100 ==0) print(h)
  ope<-opeSvg2 
  numbers<-nbSvg2 %>% add_row(name="humn",val=as.double(h))
  n<-nrow(ope)
  while("rjmz" %in% ope$X1 & "nfct" %in% ope$X1){
    # while("pppw" %in% ope$X1 & "sjmn" %in% ope$X1){
    pmap(ope,searchOpe)
    if(nrow(ope)==n) break
    n<-nrow(ope)
  }
  if(numbers %>% filter(name %in% c("rjmz","nfct")) %>% summarise(n_distinct(val))==1) break
  # if(numbers %>% filter(name %in% c("pppw","sjmn")) %>% summarise(n_distinct(val))==1) break
}
h
# h>=25085
# Test ok