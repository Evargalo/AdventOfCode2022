# Donnees ----


# v<-read_lines("data/data19test.txt")
v<-read_lines("data/data19.txt")
i<-1
blueprint<-v[i:i+3]
keepOnly(blueprint,charac = " ",numbers = TRUE) %>% str_split(" ") %>% unlist->bP
bP<-bP[!""==bP]

bPs<-as.numeric(bP)
while(i<length(v)-4){
  i<-i+4
  blueprint<-v[i:i+3]
  keepOnly(blueprint,charac = " ",numbers = TRUE) %>% str_split(" ") %>% unlist->bP
  bP<-bP[!""==bP]
  bPs<-bPs %>% rbind(bP)
}

i<-1
blueprint<-v[i:i+3]


d <- read_delim("data/data19.txt",col_names = FALSE, delim=' ')
# d <- read_delim("data/data19test.txt",col_names = FALSE, delim=' ')




d %<>% mutate(X2=cutAfter(X2,":")) %>% 
  select(2,7,13,19,22,28,31) %>% 
  mutate_all(as.numeric)
names(d)<-paste0("X",0:6)
d

# A ----
time<-24
situationInit<-tibble(oreRobot=1,
                      clayRobot=0,
                      obsidianRobot=0,
                      geodeRobot=0,
                      ore=0,
                      clay=0,
                      obsidian=0,
                      geode=0,
                      time=24)
situ<-situationInit

calcProd<-function(X0,X1,X2,X3,X4,X5,X6,oreRobot,
                   clayRobot,obsidianRobot,geodeRobot,ore,clay,obsidian,geode,time){
  if(time==0){return("the end")}
  canBuildClayRobot<-ore>=X2
  canBuildObsRobot<-ore>=X3 & clay>=X4
  canBuildGeodeRobot<-ore>=X5 & obsidian>=X6
  canBuildOreRobot<-ore>=X1
  
  ore<-ore+oreRobot
  clay<-clay+clayRobot
  obsidian<-obsidian+obsidianRobot
  geode<-geode+geodeRobot
  
  situation<-tibble(time=time-1,ore=ore,clay=clay,obsidian=obsidian,geode=geode,oreRobot=oreRobot,clayRobot=clayRobot,obsidianRobot=obsidianRobot,geodeRobot=geodeRobot)

  if(canBuildOreRobot){
    situ<<-situ %>% bind_rows(situation %>% mutate(ore=ore-X1,oreRobot=oreRobot+1))
  }  
  if(canBuildClayRobot){
    situ<<-situ %>% bind_rows(situation %>% mutate(ore=ore-X2,clayRobot=clayRobot+1))
  }
  if(canBuildObsRobot){
    situ<<-situ %>% bind_rows(situation %>% mutate(ore=ore-X3,clay=clay-X4,obsidianRobot=obsidianRobot+1))
  }
  if(canBuildGeodeRobot){
    situ<<-situ %>% bind_rows(situation %>% mutate(ore=ore-X5,obsidian=obsidian-X6,geodeRobot=geodeRobot+1))
  }
  if(ore<=max(X5,X3,X2,X1)+oreRobot+1) {
    situ<<-situ %>% bind_rows(situation)
    }
  return(as.character(time))
}

res<-tibble(blueP=d$X0,score=0)
bP<-1
calcBP<-function(bP){
  X0<-bP
  X1<-d[bP,2] %>% unlist
  X2<-d[bP,3] %>% unlist
  X3<-d[bP,4] %>% unlist
  X4<-d[bP,5] %>% unlist
  X5<-d[bP,6] %>% unlist
  X6<-d[bP,7] %>% unlist
  situ<<-situationInit
  time<-situ$time
  for(tt in time:1){
    print(tt)
    situ<<-situ %>% filter(time==tt) %>% unique
    if(tt<15) {situ<<-situ %>% arrange(desc(geodeRobot*tt*tt+geode*tt+obsidian)) %>% head(1000)}
    if(tt<5) {situ<<-situ %>% arrange(desc(geodeRobot*tt+geode)) %>% head(100)} 
    pmap(situ,calcProd,X0=X0,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)
  }
  nbGeodes<-max(situ$geode)
  res<<-res %>% mutate(score=if_else(blueP==bP,nbGeodes,score))
  return("done")
}

calcBP(1)
res %>% head
res %>% filter(score>0)
for(bp in 2:nrow(d)){
  print(bp)
  calcBP(bp)
}
res %>% summarise(sum(blueP*score))

# 1115


# B ----
situationInit %<>% mutate(time=32)

for(bp in 1:3){
  print(bp)
  calcBP(bp)
}
res %>% head(3) %>% summarise(prod(score))

# 