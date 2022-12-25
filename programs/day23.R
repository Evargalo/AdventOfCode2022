# Donnees ----

source("packages.R")


# v<-read_lines("data/data23test.txt")
v<-read_lines("data/data23.txt")


buildMaze(v)->maze
drawMaze(maze$Maze)

elves<-maze$Maze %>% filter(t=="#") %>% 
  mutate(xD=0,yD=0) %>% 
  select(-t) %>% 
  mutate_all(as.double) %>% 
  mutate(prop="?")

dir<-c("N","S","W","E")

# A ----

tryDir<-function(x,y,dir){
  xx<-x;yy<-y
  if(dir=="N") poss<-elves %>% summarise(sum(x==xx-1 & abs(yy-y)<2))  
  if(dir=="S") poss<-elves %>% summarise(sum(x==xx+1 & abs(yy-y)<2))  
  if(dir=="E") poss<-elves %>% summarise(sum(y==yy+1 & abs(xx-x)<2))  
  if(dir=="W") poss<-elves %>% summarise(sum(y==yy-1 & abs(xx-x)<2))
  if(poss==0){
    elves<<-elves %>% mutate(
      prop=if_else(x==xx & y==yy,dir,prop)
    )
  }
  return("")
}
allFree<-function(x,y){
  xx<-x;yy<-y
  elves %>% summarise(sum(abs(xx-x)<2 & abs(yy-y)<2)) %>% unlist %>% unname->nbNei
  return(nbNei<2)
}

for(i in 1:10){
  print(i)
  elves %<>% rowwise %>%  
    mutate(allFree=allFree(x,y)) %>% ungroup
  for(j in 1:4){
    pmap(elves %>% filter(!allFree & prop=="?") %>% select(x,y),
         tryDir,dir=dir[j])
    print(dir[j])
  }
  elves %<>% mutate(xD=case_when(
    prop=="N" ~ x-1,
    prop=="S" ~ x+1,
    TRUE ~ x
  ),yD=case_when(
    prop=="W" ~ y-1,
    prop=="E" ~ y+1,
    TRUE ~ y
  )
  )
  elves %<>% arrange(xD,yD) %>% mutate(coord=10000*xD+yD)
  stables<-elves %>% filter(allFree | duplicated(coord) | duplicated(coord,fromLast = TRUE))
  print(stables %>% nrow)
  movers<-elves %>% filter(!allFree & !duplicated(coord) & !duplicated(coord,fromLast = TRUE))
  stables %>% select(x,y) %>% 
    bind_rows(movers %>% select(x=xD,y=yD)) %>% 
    mutate(prop="?",
           xD=as.double(0),
           yD=as.double(0)
           )->elves
  
  dir<-c(dir[2:4],dir[1])
}

drawMaze(elves)

xMax<-max(elves$x)
yMin<-min(elves$y)
xMin<-min(elves$x)
yMax<-max(elves$y)
(yMax-yMin+1)*(xMax-xMin+1)-nrow(elves)
# 4116


# B ----

elves  %>%
  mutate(allFree=allFree(x,y))

for(i in 11:1000){
  print(i)
  elves %<>% rowwise %>% 
    mutate(allFree=allFree(x,y)) %>% 
    ungroup
  for(j in 1:4){
    pmap(elves %>% filter(!allFree & prop=="?") %>% select(x,y),
         tryDir,dir=dir[j])
    print(dir[j])
  }
  elves %<>% mutate(xD=case_when(
    prop=="N" ~ x-1,
    prop=="S" ~ x+1,
    TRUE ~ x
  ),yD=case_when(
    prop=="W" ~ y-1,
    prop=="E" ~ y+1,
    TRUE ~ y
  )
  )
  elves %<>% arrange(xD,yD) %>% mutate(coord=10000*xD+yD)
  stables<-elves %>% filter(allFree | duplicated(coord) | duplicated(coord,fromLast = TRUE))
  print(stables %>% nrow)
  if(nrow(stables)==nrow(elves)){print("gagné!") ; print(i) ; break }
  movers<-elves %>% filter(!allFree & !duplicated(coord) & !duplicated(coord,fromLast = TRUE))
  stables %>% select(x,y) %>% 
    bind_rows(movers %>% select(x=xD,y=yD)) %>% 
    mutate(prop="?",
           xD=as.double(0),
           yD=as.double(0)
    )->elves
  
  dir<-c(dir[2:4],dir[1])
  if(i%%100==0){
    assign(paste0("elves",i),elves)
    assign(paste0("dir",i),dir)
    }
}

i
# 984
