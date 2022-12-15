# Donnees ----

# d <- read_delim("data/data15test.txt",col_names = FALSE, delim='=')
d <- read_delim("data/data15.txt",col_names = FALSE, delim='=')

d %<>% select(-X1) %>% 
  mutate(X2=cutAfter(X2,','),X4=cutAfter(X4,','),X3=cutAfter(X3,':')) %>% 
  mutate_all(as.numeric) # X2,X3 : sensor's coordinates / X4,X5 : beacon's coordinates

minX<-min(c(d$X2,d$X4))-max(abs(d$X2-d$X4))
maxX<-max(c(d$X2,d$X4))+max(abs(d$X2-d$X4))
minY<-min(c(d$X3,d$X5))-max(abs(d$X3-d$X5))
maxY<-max(c(d$X3,d$X5))+max(abs(d$X3-d$X5))

manDist<-function(a,b,c,d)  abs(c-a)+abs(d-b)

d %<>% mutate(mD=manDist(X2,X3,X4,X5)) # manDist between sensor and beacon

# A ----
# yR<-10 
yR<-2000000

d %>% filter(abs(X3-yR)<=mD) %>% # if a sensor is too far it doesn't interfere
  mutate(fr=X2-mD+abs(X3-yR),to=X2+mD-abs(X3-yR) # X-values in the interval [fr;to] are too close from the sensor
         ) ->dd

imposs<-d %>% filter(X5==yR) %>% pull(X4) %>% unique # impossible values for Y: initialized where there is already a beacon 
length(imposs)->beacOnLine # how many beacons on this row ?

addImposs<-function(fr,to){ # aggregate impossible intervals
  imposs<<-c(fr:to,imposs) %>% unique 
}
suppressMessages(pmap(dd %>% select(fr,to),addImposs))

length(imposs) - beacOnLine # nb of impossible spots
# 5176944


# B ----

# x,y in 0:4000000

#bSup<-20
bSup<-4000000

# Fastest strategy: by collapsing intervals ----

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
  dd %>% select(fr,to) %>% unique %>% 
    rowwise %>% mutate(inside=sum(dd$fr<=fr & dd$to>=to)) %>% ungroup %>% 
    filter(inside==1) -> ddd
  
  # Does any Y feat ?
  ddd %<>% arrange(fr,to) %>% # sort intervals
    mutate(gap=fr-lag(to)) # gap between intervals. A negative gap means that the intervals overlaps with the previous one 
  l<- ddd %>% filter(gap>1) %>% 
    pull(fr)-1 # If any gap is above 1, the value in between is the possible Y
  if(length(l)>0) {print("gagné!");break}
  
  # Otherwise we want to try another column (i.e. increase X, but not just by 1 because that's too slow)
  # how far can we jump forward ? Let's see by comparing intervals
  suppressMessages(
    ddd %>% left_join(dd %>% select(fr,to,dir)) %>% mutate(dir=dir*lag(dir)) %>% 
    filter(dir %in% c(0,1)) %>% # if X is not on the same side of both sensors, the intervals move in different directions and we need to be careful before jumping
    summarise(min(abs(gap))) %>%
    unlist %>% unname()->ovl
    ) # smallest overlap that isn't stable
 
  # jump carefully... 
  move<-min(d1$dX,   # ... not to byPass a sensor
            ovl%/%2) # ... and to let overlaps vanish
  X<-X+max(1,move-1)
}

X*bSup+l
#13350458933732


######################################################################

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
