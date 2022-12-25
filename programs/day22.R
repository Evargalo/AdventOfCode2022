# Donnees ----

source("packages.R")

#  v<-read_lines("data/data22test.txt")
v<-read_lines("data/data22.txt")

# buildMaze(c("################",v))->maze
buildMaze(v)->maze
drawMaze(maze$Maze)
maze$Maze->m
m %>% head
# m %<>% filter(x>1) %>% mutate(x=x-1)
m %<>% mutate(t=coalesce(t," "))
drawMaze(m)
m %>% filter(x==1 & t=='.') %>% filter(y==min(y)) ->startP

# A ----
instr<-read_lines("data/data22instr.txt")
instr<-"10R5L5R10L4R5L5"
 # directions: 0:> 1:v 2:< 3:^
str_replace_all(instr,"R"," ")->ins
str_replace_all(ins,"L"," ")->ins
str_split(ins," ")->ins
ins %<>% unlist %>% as.numeric 
keepOnly(instr,charac = "RL") -> turns
turns %>% str_split("") %>% unlist -> turns
length(turns)
length(ins)

pos<-startP
dir<-0

xMax<-max(m$x)
yMax<-max(m$y)  

changeDir<-function(dir,x){
  if(x=="R") return((dir+1)%%4)
  if(x=="L") return((dir-1)%%4)
}

nextPos<-function(pos,dir){
  xx<-pos$x
  yy<-pos$y
  if(dir==0) yy<-(yy+1)
  if(dir==2) yy<-(yy-1)
  if(dir==1) xx<-(xx+1)
  if(dir==3) xx<-(xx-1)
  if(xx==0 | yy==0 | xx==xMax+1 | yy==yMax+1){
    t<-" "
  } else{
    t<-m %>% filter(x==xx,y==yy) %>% pull(t)
  }
  if(t==" " & dir==0){
    m %>% filter(x==xx,y<=yy-1,t==" ") -> prevBlank
    if(nrow(prevBlank)==0) {yy<-1} else{
      yy<-max(prevBlank$y)+1
    }
  }
  if(t==" " & dir==2){
    m %>% filter(x==xx,y>=yy+1,t==" ") -> prevBlank
    if(nrow(prevBlank)==0) {yy<-yMax} else{
      yy<-min(prevBlank$y)-1
    }
  }
  if(t==" " & dir==1){
    m %>% filter(x<=xx-1,y==yy,t==" ") -> prevBlank
    if(nrow(prevBlank)==0) {xx<-1} else{
      xx<-max(prevBlank$x)+1
    }
  }
  if(t==" " & dir==3){
    m %>% filter(x>=xx+1,y==yy,t==" ") -> prevBlank
    if(nrow(prevBlank)==0) {xx<-xMax} else{
      xx<-min(prevBlank$x)-1
    }
  }
  return(m %>% filter(x==xx,y==yy))
}

moveFwd<-function(pos,dir,k){
  if(k==0) return(pos)
  newPos<-nextPos(pos,dir)
  if(newPos$t == "#") return(pos)
  if(newPos$t == ".") return(moveFwd(newPos,dir,k-1))
} 

pos<-startP
dir<-0
pos<-moveFwd(pos,dir,ins[1])

for(i in 1:(length(turns))){
  if(i%%100==0) print(i)
  dir<-changeDir(dir,turns[i])
  print(pos)
  print(dir)
  pos<-moveFwd(pos,dir,ins[i+1])
}

1000*pos$x + 4*pos$y + dir
#13566

# B ----

nextPos<-function(pos){
  xx<-pos$x
  yy<-pos$y
  dir<-pos$d
  if(dir==0) yy<-(yy+1)
  if(dir==2) yy<-(yy-1)
  if(dir==1) xx<-(xx+1)
  if(dir==3) xx<-(xx-1)
  if(xx==0 | yy==0 | xx==xMax+1 | yy==yMax+1){
    t<-" "
  } else{
    t<-m %>% filter(x==xx,y==yy) %>% pull(t)
  }
  if(t!=" ") return(m %>% filter(x==xx,y==yy) %>% mutate(d=dir))
  if(yy>150 & dir==0){
    dir<-2
    xx<-151-xx
    yy<-100
  }
  if(yy==101 & xx>50 & xx<=100 & dir==0){
    dir<-3
    yy<-50+xx
    xx<-50
  }
  if(yy==101 & xx>100 & xx<=150 & dir==0){
    dir<-2
    xx<-151-xx
    yy<-150
  }
  if(yy==51 & xx>150 & dir==0){
    dir<-3
    yy<-xx-100
    xx<-150
  }
  if(xx==51 & yy>100 & dir==1){
    dir<-2
    xx<-yy-50
    yy<-100
  }
  if(xx==151 & yy>50 & yy<=100 & dir==1){
    dir<-2
    xx<-yy+100
    yy<-50
  }
  if(xx==201 & dir==1){
    dir<-1
    yy<-yy+100
    xx<-1
  }
  if(xx==0 & yy>100 & dir==3){
    dir<-3
    yy<-yy-100
    xx<-200
  }
  if(xx==0 & yy>50 & yy<=100 & dir==3){
    dir<-0
    xx<-yy+100
    yy<-1
  }
  if(xx==100 & yy<=50 & dir==3){
    dir<-0
    xx<-yy+50
    yy<-51
  }
  if(yy==50 & xx<=50 & dir==2){
    dir<-0
    xx<-151-xx
    yy<-1
  }
  if(yy==50 & xx>50 & xx<=100 & dir==2){
    dir<-1
    yy<-xx-50
    xx<-101
  }
  if(yy==0 & xx>100 & xx<=150 & dir==2){
    dir<-0
    xx<-151-xx
    yy<-51
  }
  if(yy==0 & xx>150 & dir==2){
    dir<-1
    yy<-xx-100
    xx<-1
  }
  return(m %>% filter(x==xx,y==yy) %>% mutate(d=dir))
}

moveFwd<-function(pos,k,i){
  # just for visualization purpose
  m<<- m %>% mutate(path=if_else(x==pos$x & y==pos$y,1000+i,path))
  if(k==0) return(pos)
  newPos<-nextPos(pos)
  if(newPos$t == "#") return(pos)
  if(newPos$t == ".") return(moveFwd(newPos,k-1,i))
} 

pos<-startP
dir<-0
pos$d<-dir
m$path<-0

pos<-moveFwd(pos,ins[1],0)

for(i in 1:(length(turns))){
  if(i%%100==0) print(i)
  pos$d<-changeDir(pos$d,turns[i])
  pos<-moveFwd(pos,ins[i+1],i)
}

1000*pos$x + 4*pos$y + pos$d
# 11451 

gf_tile(data = m,gformula = x~y,fill=~path) %>% 
  gf_refine(theme=theme_void())
