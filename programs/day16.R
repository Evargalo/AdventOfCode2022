# Donnees ----

d <- read_delim("data/data16test.txt",col_names = FALSE, delim=' ')
d <- read_delim("data/data16.txt",col_names = FALSE, delim=' ')

d %<>% select(V=X2,rate=X5,lead=X10) %>% 
  mutate(rate=keepOnly(rate,numbers=TRUE)) %>% 
  mutate(rate=as.numeric(rate)) 

maze<-data.frame(fr=character(),to=character())
V<-d[1,1]
lead<-d[1,3]
buildMaze<-function(V,lead){
  for(vv in str_split(lead,',') %>% unlist){
    maze<<-maze %>% add_row(fr=V,to=vv)
  }
}
pmap(d %>% select(V,lead),buildMaze)

valves<-unique(maze$fr)

rates<-d %>% select(-lead)
open<-d %>% filter(rate==0) %>% pull(V)
closed<-d %>% filter(rate!=0) %>% pull(V)
realValves<-c("AA",closed) %>% unique

g<-graph_from_data_frame(maze)
plot(g)

distance_table(g)
distances(g,v=realValves,to=realValves)->matDist

getDist<-function(v,vv){
  matDist[v,vv]
}
getRate<-function(v){
  rates %>% filter(V==v) %>% pull(rate) %>% unlist
}
# 
# for(v in V(h)){
#   for(vv in V(h)){
#     if(v!=vv) {
#       h %<>% add.edges(edges = c(v,vv),weight=getDist(v,vv)) 
#     }
#   }
# }
# 
# h %>% get.edgelist(names = )
# h %>% get.edge.attribute(name = "weight")
# h %>% plot.igraph()
# h %>% get.all.shortest.paths(from = "AA") ->sP
# h %>% shortest_paths(from = "AA",output="both")
# 
# h %>% get.adjedgelist()
# 
# h %>% distances ->distH
# #distH[distH==0]<-1



# A ----

paths<-data.frame(fr='AA',path='AA',gain=0,time=30)
#fr='PL';path='AA,PL';gain=4*28;time=28
fr='AA';path='AA';gain=0;time=30

continuePath<-function(fr,path,gain,time){
  prev<-str_split(path,',') %>% unlist
  poss<-closed[closed %notin% prev]
  res<-data.frame(fr,path,gain,time)
  # possib<-poss[1]
  for(possib in poss){
    tt<-getDist(fr,possib)
    if(tt<time){
      newPath<-paste0(path,',',possib)
      newTime<-time-tt-1
      newGain<-gain+getRate(possib)*newTime
      res %<>% add_row(fr=possib,path=newPath,gain=newGain,time=newTime)
    }
  }
  res[-1,]
}
continuePath(fr,path,gain,time)
pmap_df(paths,continuePath)

newPaths<-paths
while(newPaths %>% nrow > 0){
  print(paths %>% nrow)
  print(newPaths %>% summarise(max(time)))
  
  newPaths<-pmap_dfr(newPaths,continuePath) 
  paths %<>% bind_rows(newPaths) %>% unique()
}

paths %>% tail
newPaths %>% tail
paths %>% summarise(max(gain))

# 1651 test ok
# 


# B ----


# 