# Donnees ----

f<- fileToDF("data/data12.txt")
# f<- fileToDF("data/data12test.txt")

c<-as.matrix(f)
nrow(c)->nR
ncol(c)->nC

m<-matrix(data = numeric(),ncol = nC,nrow = nR)
for(j in 1:nC){
  for(i in 1:nR){
    if(c[i,j]=="E") m[i,j]<-26 else 
      if(c[i,j]=="S") m[i,j]<-1 else 
        m[i,j]<-which(letters==c[i,j])
  }
}

df<-expand.grid(i=1:nR,j=1:nC)
df %<>% rowwise %>% mutate(h=m[i,j],  # elevation
                           l=c[i,j]   # letter
                           ) %>% ungroup
df %>% group_by(h) %>% count
df %<>% mutate(d=if_else(l=="E",0,-1)) # distance to E, -1 when not calculated


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
  toDo<-df %>% filter(d==0) # spots with distance dd, previously calculated
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
gf_tile(df %>% arrange(l %in% c("S","E")),(-j)~i,fill = ~h,
        color = ~(l %in% c("S","E"))) %>% 
  gf_refine(scale_color_manual(values = c(alpha("lightblue",0),"red")))

# distance map
gf_tile(df %>% mutate(d=if_else(d==-1,500,d)) %>% arrange(l %in% c("S","E")),
        (-j)~i,fill = ~d,color = ~(l %in% c("S","E"))) %>% 
  gf_refine(scale_color_manual(values = c(alpha("lightblue",0),"red")))
