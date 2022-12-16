# Donnees ----

#  d <- read_delim("data/data16test.txt",col_names = FALSE, delim=' ')
d <- read_delim("data/data16.txt",col_names = FALSE, delim=' ')

d %<>% select(V=X2,rate=X5,lead=X10) %>% 
  mutate(rate=keepOnly(rate,numbers=TRUE)) %>% 
  mutate(rate=as.numeric(rate)) 

maze<-data.frame(fr=character(),to=character())

buildMaze<-function(V,lead){
  for(vv in str_split(lead,',') %>% unlist){
    maze<<-maze %>% add_row(fr=V,to=vv)
  }
}
pmap(d %>% select(V,lead),buildMaze)

valves<-unique(maze$fr)

rates<-d %>% select(-lead)
getRate<-function(v){
  rates %>% filter(V==v) %>% pull(rate) %>% unlist
}

closed<-d %>% filter(rate!=0) %>% pull(V) # the only valves that really interest us
realValves<-c("AA",closed) %>% unique # plus the starting point

# how long to travel between valves
g<-graph_from_data_frame(maze) ; plot(g)
distances(g,v=realValves,to=realValves)->matDist
getDist<-function(v,vv){
  matDist[v,vv]
}

# A ----

# Our starting point
paths<-data.frame(fr='AA', # where we are
                  path='AA', # valves we have already opened
                  gain=0, # total pressure released by open valves (till the end of the 30')
                  time=30) # time left before eruption

continuePath<-function(fr,path,gain,time){ # go open one more  
  prev<-str_split(path,',') %>% unlist # valves already visited
  poss<-closed[closed %notin% prev] # next valves to visit
  res<-data.frame(fr,path,gain,time) # to store path
  for(possib in poss){
    tt<-getDist(fr,possib) # how long to go there
    if(tt<time){
      newPath<-paste0(path,',',possib)
      newTime<-time-tt-1 # 1 minute to open it
      newGain<-gain+getRate(possib)*newTime
      res %<>% add_row(fr=possib,path=newPath,gain=newGain,time=newTime)
    }
  }
  res[-1,] # remove the input from the answer
}
#test
pmap_df(paths,continuePath)

newPaths<-paths
while(newPaths %>% nrow > 0){
  # print(paths %>% nrow);print(newPaths %>% summarise(max(time)))
  newPaths<-pmap_dfr(newPaths,continuePath) 
  paths %<>% bind_rows(newPaths) %>% unique()
  if(newPaths %>% nrow >100){ # investigate only promising paths
    newPaths %<>% arrange(desc(gain)) %>% head(100)
  }
}

paths %>% summarise(max(gain))

# 1651 test ok
# 1474


# B ----

# same with two actors
paths<-data.frame(fr='AA',path='AA',gain=0,time=26, # me
                  frE='AA',pathE='AA',timeE=26 # the elephant
) 

continuePathE<-function(fr,path,gain,time,frE,pathE,timeE){ 
  prev<-c(str_split(path,',') %>% unlist,str_split(pathE,',') %>% unlist)
  poss<-closed[closed %notin% prev] # which valves remain to be opened
  res<-data.frame(fr,path,gain,time,frE,pathE,timeE)
  for(possib in poss){ 
    tt<-getDist(fr,possib) # either I go to open a valve
    if(tt<time){  
      newPath<-paste0(path,',',possib)
      newTime<-time-tt-1
      newGain<-gain+getRate(possib)*newTime
      res %<>% add_row(fr=possib,path=newPath,gain=newGain,time=newTime,frE,pathE,timeE)
    } # or the elephant goes to open a valve
    if(fr!=frE | time!=timeE){ # I and the elephant are not equivalent
      tt<-getDist(frE,possib)
      if(tt<timeE){
        newPath<-paste0(pathE,',',possib)
        newTime<-timeE-tt-1
        newGain<-gain+getRate(possib)*newTime
        res %<>% add_row(fr,path,gain=newGain,time,frE=possib,pathE=newPath,timeE=newTime)
      }
    }
  }
  res[-1,] 
}

# test 
pmap_df(paths,continuePathE)

tracePath<-function(fr,time,frE,timeE){ # sumup the open valves, the location and time remaining of either actor
  if(time<timeE){
    a<-fr;fr<-frE;frE<-a
    b<-time;time<-timeE;timeE<-b
  }
  paste0(time,fr,timeE,frE)
}

paths<-data.frame(fr='AA',path='AA',gain=0,time=26,frE='AA',pathE='AA',timeE=26) # initial situation
newPaths<-paths

while(newPaths %>% nrow > 0){
  print(newPaths %>% nrow)
  timeleft<-newPaths %>% summarise(max(time+timeE)) %>% unlist
  print(timeleft)
  # do one more valve for each path found
  newPaths<-pmap_dfr(newPaths,continuePathE)
  # keep only the best score among paths leading to the same positions and time
  newPaths %<>% rowwise %>% mutate(trace=tracePath(fr,time,frE,timeE)) %>% ungroup %>% 
    arrange(desc(gain)) %>% filter(!duplicated(trace)) %>% select(-trace)
  paths %<>% bind_rows(newPaths %>% filter(gain > 2000)) %>% unique() # keep a trace of promising results !
  # reduce the size of newPaths to gain time
  bound<-10000
  if(newPaths %>% nrow > bound & newPaths %>% nrow > 0){
    print("nb de nouveaux chemins calculés: "); print(newPaths %>% nrow)
    newPaths %<>% arrange(desc(gain+gainE+3*time*timeE)) %>% head(bound)
  }
}

paths %>% arrange(desc(gain)) %>% head

paths %>% summarise(max(gain))
# 2100 

