# Donnees ----

d <- read_delim("data/data15test.txt",col_names = FALSE, delim='=')
d <- read_delim("data/data15.txt",col_names = FALSE, delim='=')

d %<>% select(-X1) %>% 
  mutate(X2=cutAfter(X2,','),X4=cutAfter(X4,','),X3=cutAfter(X3,':')) %>% 
  mutate_all(as.numeric)

minX<-min(c(d$X2,d$X4))-max(abs(d$X2-d$X4))
maxX<-max(c(d$X2,d$X4))+max(abs(d$X2-d$X4))
minY<-min(c(d$X3,d$X5))-max(abs(d$X3-d$X5))
maxY<-max(c(d$X3,d$X5))+max(abs(d$X3-d$X5))
manDist<-function(a,b,c,d)  abs(c-a)+abs(d-b)
d %<>% mutate(mD=manDist(X2,X3,X4,X5))

# A ----
yR<-10 
yR<-2000000

d %>% filter(abs(X3-yR)<=mD) %>% 
  mutate(fr=X2-mD+abs(X3-yR),to=X2+mD-abs(X3-yR)) ->dd

dd %<>% mutate(mDfr=manDist(X2,X3,fr,yR))

imposs<-d %>% filter(X5==yR) %>% pull(X4) %>% unique 
length(imposs)->beacOnLine

addImposs<-function(fr,to){
  imposs<<-c(fr:to,imposs) %>% unique
}
pmap(dd %>% select(fr,to),addImposs)

length(imposs) - beacOnLine
# 5176944


# B ----

# x,y in 0: 4000000
bSup<-20
bSup<-4000000

# Fastest strategy: by collapsing intervals ----
findHoles<-function(df){ # given a df of intervals, find any non-included value
  df %<>% arrange(fr,to) %>% mutate(gap=fr-lag(to))
  df %>% filter(gap>1) %>% pull(fr)-1
}
# Test
df<-data.frame(fr=c(0,25,36),to=c(28,34,bSup))
df<-data.frame(fr=c(0,25,36),to=c(28,35,bSup))
findHoles(df)

X<-0
while(X < bSup){ # given X, find any possible Y
  print(X)
  #  calculate forbidden intervals for Y 
  d %>% mutate(dX=abs(X-X2), # horizontal distance to the sensor  
               dir=sign(X-X2) # are we getting closer or further from the sensor when X increase ?
               ) -> d1 # will be reused
  d1 %>% filter(mD>=dX) %>% # Only for sensors not too far away, otherwise they don't interfere
    rowwise() %>% mutate(fr=max(X3-mD+dX,0),to=min(X3+mD-dX,bSup)) %>% # Y can't be in [fr;to]
    ungroup->dd
  # remove any interval inside another one
  # dd %>% rowwise %>% 
  #   mutate(inside=sum((dd$fr<=fr & dd$to>to) | (dd$fr<fr & dd$to>=to))) %>% # We don't want to remove duplicated intervals
  #   ungroup %>% filter(inside<1) -> ddd
  
  dd %>% select(fr,to) %>% unique %>% rowwise %>% 
    mutate(inside=sum(dd$fr<=fr & dd$to>=to)) %>% ungroup %>% 
    filter(inside==1) -> ddd
  
  # Does any Y feat ?
  # ddd %<>% arrange(fr,to) %>% mutate(gap=fr-lag(to))
  # l<- ddd %>% filter(gap>1) %>% pull(fr)-1
  
  l<-findHoles(ddd %>% select(fr,to)) 
  if(length(l)>0) {print("gagné!");break}
  
  # how far can we jump forward ? Let's see by comparing intervals
  dd %>% select(dX,fr,to,dir) %>% mutate(one=1) -> ddd
  ddd %>% full_join(ddd,by=c("one")) %>% 
    filter(to.x!=to.y | fr.y!=fr.x)%>% 
    filter(!(to.x+1<=fr.y | to.y+1<=fr.x)) %>% 
    rowwise %>% 
    mutate(dir=dir.x*dir.y,overlap=min(to.x-fr.y,to.y-fr.x)) %>%
    ungroup  %>% filter(dir!=-1) %>% 
    pull(overlap) ->ovl
  move<-min(d1$dX,ovl)%/%2 
  X<-X+max(1,move-1)
}

X*bSup+l
#13350458933732




# First strategy, ~brute force ----
v<-0:bSup
rmRange<-function(fr,to){
  v<<-v[v%notin%(fr:to)]
}

for(X in 0:bSup){
  if(X%%100==0) print(X)
  d %>% mutate(dX=abs(X-X2)) %>% filter(mD>=dX) %>% rowwise() %>% 
    mutate(fr=max(X3-mD+dX,0),to=min(X3+mD-dX,bSup)) %>% 
    ungroup->dd
  v<-0:bSup
  v<-v[v %notin% (d %>% filter(X4==X) %>% pull(X5) %>% unique)] 
  suppressMessages(pmap(dd %>% select(fr,to),rmRange))
  if(length(v)>0){break}
}

X*4000000+v

# ok for test, too slow for real data

# Refinement ----
# Faster by jumping if we have margin
X<-0
# X<-2175528
while(X < bSup){
  print(X)
  d %>% mutate(dX=abs(X-X2)) %>% filter(mD>=dX) %>% rowwise() %>% 
    mutate(fr=max(X3-mD+dX,0),to=min(X3+mD-dX,bSup)) %>% 
    ungroup->dd
  dd %<>% rowwise %>% 
    mutate(inside=sum((dd$fr<=fr & dd$to>to) | (dd$fr<fr & dd$to>=to))) %>% ungroup %>% 
    filter(inside<1)
  v<-0:bSup
  v<-v[v %notin% (d %>% filter(X4==X) %>% pull(X5) %>% unique)] 
  suppressMessages(pmap(dd %>% select(fr,to),rmRange))
  if(length(v)>0){break}
  diff<-expand_grid(fr=unique(dd$fr),to=unique(dd$to)) %>% mutate(delta=abs(to-fr))
  X<-X+max(min(diff$delta)%/%2,1)
}
X*bSup+v

# Still too slow because at some point we don't have any margin, intervals don't overlap
