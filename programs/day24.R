# Donnees ----

source("packages.R")

# v<-read_lines("data/data24test.txt")
v<-read_lines("data/data24.txt")

buildMaze(v)->maze
drawMaze(maze$Maze)
maze$Maze->m
max(m$x)->xMax
max(m$y)->yMax
maze$foundChar
dir<-c("<",">","v","^")
m %>% filter(t %in% dir)->winds
# A ----
winds %>% filter(y==yMax-1) %>% group_by(t) %>% count
winds %>% filter(y==2) %>% group_by(t) %>% count
# no v nor ^ : startpoint and endpoint are always accessible
winds %<>% mutate(xx=x-2,yy=y-2) %>% select(-x,-y)
yM<-yMax-2
xM<-xMax-2

# Where can we go after reaching x,y at time t ? 
canMove<-function(x,y,time){
  x0<-x;y0<-y
  time<-time+1
  w<-winds %>% 
    filter(abs(xx-x)<2 | abs(yy-y)<2) %>% 
    mutate(x=case_when(
      t=="v" ~ (xx+time) %% xM,
      t=="^" ~ (xx-time) %% xM,
      TRUE ~ xx
    ),y=case_when(
      t==">" ~ (yy+time) %% yM,
      t=="<" ~ (yy-time) %% yM,
      TRUE ~ yy
    )) %>% 
    select(x,y) %>% 
    filter(abs(x-x0) + abs(y-y0)<2)
  return(tibble(x=c(x,x+1,x-1,x,x),
                y=c(y,y,y,y+1,y-1),
                time=time) %>% 
           anti_join(w) %>% 
           filter(x>=0,y>=0,x<xM,y<yM)
  )
}

pos<-tibble(x=-1,y=0,time=0)
stop<-FALSE
while(!stop){
  newPos<-suppressMessages(pmap_dfr(pos,canMove)) %>% unique
  tt<-max(newPos$time)
  if(tt %% 10 ==0) {
    print(tt)
    print(Sys.time())
  }
  if(tt %% 100 ==0) {
    assign(paste0("pos",tt),newPos)
  }
  if(any(newPos$x==xM-1 & newPos$y==yM-1)) {
    print("gagné")
    print(tt+1)
    stop<-TRUE
  }
  pos<-newPos %>% arrange(desc(x+y),desc(y)) %>% head(100)
  if(tt==2000) stop<-TRUE
}
max(pos$time+1)
# 373
# test ok 


# B ----

t1<-tt+1
pos<-tibble(x=xM,y=yM-1,time=t1)

# Back to start
stop<-FALSE
while(!stop){
  newPos<-suppressMessages(pmap_dfr(pos,canMove)) %>% unique
  tt<-max(newPos$time)
  if(tt %% 10 ==0) {
    print(tt)
    print(Sys.time())
  }
  if(tt %% 100 ==0) {
    assign(paste0("pos",tt),newPos)
  }
  if(any(newPos$x==0 & newPos$y==0)) {
    print("gagné")
    print(tt+1)
    t2<-tt+1
    stop<-TRUE
  }
  pos<-newPos %>% arrange(x+y,y) %>% head(60)
  if(tt==2000) stop<-TRUE
}

# To the end again
tt<-t2
while(nrow(canMove(x=-1,y=0,time=tt))==0){tt<-tt+1}
pos<-tibble(x=-1,y=0,time=tt)
stop<-FALSE
while(!stop){
  newPos<-suppressMessages(pmap_dfr(pos,canMove)) %>% unique
  tt<-max(newPos$time)
  if(tt %% 10 ==0) {
    print(tt)
    print(Sys.time())
  }
  if(tt %% 100 ==0) {
    assign(paste0("pos",tt),newPos)
  }
  if(any(newPos$x==xM-1 & newPos$y==yM-1)) {
    print("gagné")
    print(tt+1)
    t3<-tt+1
    stop<-TRUE
  }
  pos<-newPos %>% arrange(desc(x+y),desc(y)) %>% head(60)
  if(tt==2000) stop<-TRUE
}
t3
# 997