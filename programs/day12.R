# Donnees ----

f<- fileToDF("data/data12.txt")
# f<- fileToDF("data/data12test.txt")

f %<>% unname 

lettersToNum<-function(l){
  if(l=="E") return(26)
  if(l=="S") return(1)
  which(letters==l)
}

df<-expand.grid(i=1:nrow(f),j=1:ncol(f))
df %<>% rowwise %>% mutate(l=f[i,j]) %>% 
  mutate(h=lettersToNum(l), # elevation
         d=if_else(l=="E",0,-1) # distance to E, -1 when not calculated) 
  ) %>% ungroup

df %>% group_by(h) %>% count
df %>% group_by(d) %>% count

# A ----

dd<-0 # last distance to E having been explored

# spots from which we can move to ii,jj
find4Neighbours<-function(ii,jj,hh,dd,ll){
  df %>% filter (
    (h>=hh-1)   # possible movement
    &
      (d==-1)     # spot not reached earlier
    &
      (((ii-i==0) & ((jj-j) %in% c(-1,1))) | ((jj-j==0) & ((ii-i) %in% c(-1,1))))
  ) %>% mutate(d=dd+1)    # close enough
}
# one step further away from E
walk<-function(dd){
  toDo<-df %>% filter(d==dd) # spots with distance dd, previously calculated
  nei<-pmap_dfr(toDo,find4Neighbours) # spots with distance dd
  change<-nei %>% nrow # how many spots will be updated
  df<<-nei %>% bind_rows(df) %>% group_by(i,j) %>% filter(row_number()==1) %>% ungroup # replace data for new spots in df
  change
}

# map the area
stop<-FALSE
while(!stop){
  if(dd%%20==0) {print(dd);print(df %>% filter(d==-1) %>% nrow)}
  stop<-(walk(dd)==0 | df %>% filter(l=='S') %>% pull(d)>-1) # if we are stuck or if we have finished
  dd<-dd+1
}

# Distance from S to E
df %>% filter(l=="S") %>% pull(d)
# 472

# 31 for dataTest : ok

# B ----

# shortest path from elevation 1 to E
df %>% filter(h==1 & d!=-1) %>% pull(d) %>% min
# 465

# Maps ----
# elevation map
gf_tile(df %>% arrange(l %in% c("S","E")),(j)~i,fill = ~h,
        color = ~(l %in% c("S","E"))) %>% 
  gf_refine(scale_color_manual(values = c(alpha("lightblue",0),"red")),
            theme=theme_void())

# distance map
gf_tile(df %>% mutate(d=if_else(d==-1,800,d)) %>% arrange(l %in% c("S","E")),
        (j)~i,fill = ~d,color = ~(l %in% c("S","E"))) %>% 
  gf_refine(scale_color_manual(values = c(alpha("lightblue",0),"red")),
            theme=theme_void())

# path
path<-df %>% filter(l=="S")
last<-path
while(last$d>0){
  df %>% filter (
      (h<=last$h+1)   # possible movement
    &
      (d==last$d-1)     # closer to goal
    &
      (((last$i-i==0) & ((last$j-j) %in% c(-1,1))) | ((last$j-j==0) & ((last$i-i) %in% c(-1,1)))) # neighbour
  ) %>% head(1) -> last
  path %<>% add_row(last) 
}

df %>% bind_rows(path) %>% mutate(onTheWay=duplicated(df%>% bind_rows(path))) %>% unique %>% 
  gf_tile(gformula=j~i,fill = ~h,color = ~onTheWay) %>% 
  gf_refine(scale_color_manual(values = c(alpha("lightblue",0),"red")),
            theme=theme_void())
ggsave("programs/day12_plot.jpg")
