# Donnees ----

d <- read_delim("data/data11.txt",col_names = FALSE, delim=':')
# d <- read_delim("data/data11test.txt",col_names = FALSE, delim=':')

d %<>% mutate_all(trim) %>% 
  mutate(X1=rmFirstWord(X1))

# importance and situation of each item
start<-data.frame(value=numeric(),monkey=numeric())
# rules of monkeys
monkeys<-data.frame(monkey=numeric(),operator=character(),operande=numeric(),
                    test=numeric(),iftrue=numeric(),iffalse=numeric(),insp=numeric())
# initialize start and monkeys
for(i in 0:(nrow(d)%/%6 -1)){
  monk<-d[(i*6+1):(i*6+6),]
  m<-as.numeric(monk[1,1])
  items<-str_split(monk[2,2],", ") %>% unlist %>% as.numeric
  start %<>% bind_rows(data.frame(value=items,monkey=m))
  op<-str_sub(monk[3,2],11,11)
  ope<-cutBefore(monk[3,2]," ")
  if(ope=="old"){op<-"sq"; ope<-"0"}
  ope<-as.numeric(ope)
  tes<-cutBefore(monk[4,2]," ") %>% as.numeric
  ift<-cutBefore(monk[5,2]," ") %>% as.numeric
  iff<-cutBefore(monk[6,2]," ") %>% as.numeric
  monkeys %<>% add_row(monkey=m,
                       operator=op,
                       operande=ope,
                       test=tes,
                       iftrue=ift,
                       iffalse=iff,
                       insp=0)
}
start
monkeys

# A ----

# A monkey inspects an item
insp<-function(value,monkey){
  m<-monkey
  monk<-monkeys %>% filter(monkey==m)
  if(monk$operator =="sq") value<-value*value
  if(monk$operator =="*") value<-value*monk$operande 
  if(monk$operator =="+") value<-value+monk$operande 
  value<-value%/%3 
  if(value%%monk$test==0) monkey<-monk$iftrue else monkey<-monk$iffalse 
  monkeys<<-monkeys %>% mutate(insp=if_else(monkey==m,insp+1,insp))
  data.frame(value,monkey)
}
# A monkey inspects all his items
turn<-function(m){
  items<-start %>% filter(monkey==m)
  newitems<-pmap_dfr(items,insp)
  start<<-start %>% filter(monkey!=m) %>% 
    bind_rows(newitems)
}

# All monkeys have their turn
round<-function(){
  for(i in 0:7) turn(i)
}

# 20 rounds of monkeyness
for(i in 0:19) {print(i);round()}

# product of the number of inspections by the two most active monkeys
monkeys %>% arrange(desc(insp)) %>% head(2) %>% pull(insp) %>% prod()
# 110264

# B ----

# nb = number of items of each worry level x monkey : to save some iterations
start %<>% group_by(value,monkey) %>% summarise(nb=n()) %>% ungroup
# limit for my worry level
monkeys %>% pull(test) %>% prod -> modulo
# Storing nb of inspections in a vector save some time vs mutating the df monkeys
nbInsp<-index_from_0(rep(0,8))

insp<-function(value,monkey,nb){
  m<-monkey
  monk<-monkeys %>% filter(monkey==m)
  if(monk$operator =="sq") value<-value*value
  if(monk$operator =="*") value<-value*monk$operande 
  if(monk$operator =="+") value<-value+monk$operande 
  if(value%%monk$test==0) monkey<-monk$iftrue else monkey<-monk$iffalse 
  nbInsp[m]<<-nbInsp[m]+nb
  data.frame(value,monkey,nb)
}

turn<-function(m){
  newitems<-pmap_dfr(start %>% filter(monkey==m),insp) %>% 
    # keep worry levels manageable 
    mutate(value=value %% modulo) %>% 
    mutate(value=if_else(value==0,modulo,value))
  start<<-start %>% filter(monkey!=m) %>% 
    bind_rows(newitems) %>% 
    # save time in case there are two identical objects
    group_by(value,monkey) %>% 
    summarise(nb=sum(nb)) %>% 
    ungroup
}

round<-function(){
  for(i in 0:7) turn(i)
}

for(i in 1:1000) {
  suppressMessages(round())
  if(i%%20==0) {print(i);print(monkeys$insp)}
}
s1000<-start
m1000<-monkeys

# Be patient or go for your morning run 
for(i in 1001:10000) {
  suppressMessages(round())
  if(i%%100==0) {print(i);print(monkeys$insp)}
  if(i%%1000==0) {
    assign(paste0("s",i),start)
    assign(paste0("m",i),monkeys)
  }
}

monkeys %>% arrange(desc(insp)) %>% head(2) %>% pull(insp) %>% prod()
# 23612457316

# B bis ----
# This strategy gains time but it is only feasible because there are only 8 monkeys.
# Strategy in section B is slower because it manipulates more df, but it can work even with a huge tribe of monkeys

nbInsp<-index_from_0(rep(0,8))
insp2<-function(value,monkey,nb){
  nbInsp[monkey]<<-nbInsp[monkey]+nb
  if(monkey==0){
    value<-3*value
    if(value%%5==0) {monkey<-2} else {monkey<-3}
    return(data.frame(value,monkey,nb))
  }
  if(monkey==1){
    value<-8+value
    if(value%%11==0) {monkey<-4} else {monkey<-7}
    return(data.frame(value,monkey,nb))
  }
  if(monkey==2){
    value<-2+value
    if(value%%2==0) {monkey<-5} else {monkey<-3}
    return(data.frame(value,monkey,nb))
  }
  if(monkey==3){
    value<-4+value
    if(value%%13==0) {monkey<-1} else {monkey<-5}
    return(data.frame(value,monkey,nb))
  }
  if(monkey==4){
    value<-19*value
    if(value%%7==0) {monkey<-7} else {monkey<-6}
    return(data.frame(value,monkey,nb))
  }
  if(monkey==5){
    value<-5+value
    if(value%%3==0) {monkey<-4} else {monkey<-1}
    return(data.frame(value,monkey,nb))
  }
  if(monkey==6){
    value<-value*value
    if(value%%17==0) {monkey<-0} else {monkey<-2}
    return(data.frame(value,monkey,nb))
  }
  if(monkey==7){
    value<-1+value
    if(value%%19==0) {monkey<-6} else {monkey<-0}
    return(data.frame(value,monkey,nb))
  }
}
turn2<-function(m){
  newitems<-pmap_dfr(start %>% filter(monkey==m),insp2) %>% 
    # keep worry levels manageable 
    mutate(value=value %% modulo) %>% 
    mutate(value=if_else(value==0,modulo,value))
  start<<-start %>% filter(monkey!=m) %>% 
    bind_rows(newitems) %>% 
    group_by(value,monkey) %>% 
    summarise(nb=sum(nb)) %>% 
    ungroup
}
round2<-function(){
  for(i in 0:7) turn2(i)
}

# benchmark
start<-s10000
monkeys<-m10000
nbInsp<-index_from_0(monkeys$insp)
Sys.time()
for(i in 1:20) suppressMessages(round())
Sys.time() # 26s
start<-s10000
Sys.time()
for(i in 1:20) suppressMessages(round2())
Sys.time() # 9s
