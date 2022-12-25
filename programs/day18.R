source("packages.R")

# Donnees ----

d <- read_delim("data/data18.txt",col_names = FALSE, delim=',')
# d <- read_delim("data/data18test.txt",col_names = FALSE, delim=',')

# A ----
surfTot<-function(dd){
  dd %<>% unique
  dd %<>% mutate(one=1)
  dd %>% left_join(dd,by="one")->doublons
  doublons %<>% mutate(dist=abs(X1.y-X1.x)+abs(X2.y-X2.x)+abs(X3.y-X3.x)) %>% 
    mutate(doub=dist==1)
  6*nrow(dd)-sum(doublons$doub)
}
surfTot(d)
# 4308


# B ----

surfLava<-surfTot(d)
minX1<-min(d$X1)
minX2<-min(d$X2)
minX3<-min(d$X3)
maxX1<-max(d$X1)
maxX2<-max(d$X2)
maxX3<-max(d$X3)
# Modelize a cube of air and lava
cube<-expand.grid(X1=(minX1):(maxX1),
                  X2=(minX2):(maxX2),
                  X3=(minX3):(maxX3)
)
d %>% mutate(t="lava") %>%  bind_rows(cube %>% mutate(t="air")) %>% filter(!duplicated(X1,X2,X3)) ->cube
cube %>% group_by(t) %>% count
# Outside : direct contact with steam
isOutside<-function(X1,X2,X3,t){
  if(t=="lava") return(FALSE)
  x<-X1;y<-X2;z<-X3
  if(nrow(d %>% filter(X1==x,X2==y,X3>z))==0) return(TRUE)
  if(nrow(d %>% filter(X1==x,X2==y,X3<z))==0) return(TRUE)
  if(nrow(d %>% filter(X1==x,X2<y,X3==z))==0) return(TRUE)
  if(nrow(d %>% filter(X1==x,X2>y,X3==z))==0) return(TRUE)
  if(nrow(d %>% filter(X1<x,X2==y,X3==z))==0) return(TRUE)
  if(nrow(d %>% filter(X1>x,X2==y,X3==z))==0) return(TRUE)
  return(FALSE)
}

cube %<>% rowwise %>% mutate(steam=isOutside(X1,X2,X3,t)) %>% ungroup

cube %>% group_by(t,steam) %>% count

# any block of air that touches steam from outside also fills of steam
air<-cube %>% filter(t=="air") %>% select(-t)
nbOutside<-nrow(air %>% filter(steam))
newN<-nrow(air)

while(newN>nbOutside){
  nbOutside<-nrow(air %>% filter(steam))
  air %>% left_join(air,by="X1") %>% mutate(X1.x=X1) %>% rename(X1.y=X1) %>% mutate(dist=abs(X1.y-X1.x)+abs(X2.y-X2.x)+abs(X3.y-X3.x)) %>% filter(dist<2) ->cro1
  air %>% left_join(air,by="X2") %>% mutate(X2.x=X2) %>% rename(X2.y=X2) %>% mutate(dist=abs(X1.y-X1.x)+abs(X2.y-X2.x)+abs(X3.y-X3.x)) %>% filter(dist<2)->cro2
  bind_rows(cro1,cro2) %>% unique %>% mutate(neighbour=dist==1)->cro
  cro %>% group_by(X1=X1.x,X2=X2.x,X3=X3.x) %>% 
    summarise(steam=any(steam.x) | any(steam.y & neighbour)) %>% ungroup->air
  newN<-nrow(air %>% filter(steam))
}

# surface of air out of reach from steam
surfAirC<-surfTot(air %>% filter(!steam))

surfLava-surfAirC
# 2540
# ok for test










# lava
inD<-function(X1,X2,X3){
  x<-X1;y<-X2;z<-X3
  if(nrow(d %>% filter(X1==x,X2==y,X3==z))==0) return(FALSE)
  return(TRUE)
}
# no obvious way outside but might still be reached by steam
trapped<-function(X1,X2,X3){
  x<-X1;y<-X2;z<-X3
  if(nrow(d %>% filter(X1==x,X2==y,X3>z))==0) return(FALSE)
  if(nrow(d %>% filter(X1==x,X2==y,X3<z))==0) return(FALSE)
  if(nrow(d %>% filter(X1==x,X2<y,X3==z))==0) return(FALSE)
  if(nrow(d %>% filter(X1==x,X2>y,X3==z))==0) return(FALSE)
  if(nrow(d %>% filter(X1<x,X2==y,X3==z))==0) return(FALSE)
  if(nrow(d %>% filter(X1>x,X2==y,X3==z))==0) return(FALSE)
  return(TRUE)
}

cube %<>% rowwise %>% mutate(inside=trapped(X1,X2,X3),inD=inD(X1,X2,X3)) %>% ungroup

cube %>% group_by(inside,inD) %>% count

air<-cube %>% filter(!inD)
# any block of air that touches air from outside is also outside
nbInside<-nrow(air %>% filter(inside))
newN<-0
while(newN<nbInside){
  nbInside<-nrow(air %>% filter(inside))
  ins<-air %>% mutate(one=1)
  # out<-air %>% filter(!inside) %>% mutate(one=1)
  ins %>% left_join(ins,by="one")->crois
  crois %<>% mutate(dist=abs(X1.y-X1.x)+abs(X2.y-X2.x)+abs(X3.y-X3.x)) %>% 
    mutate(doub=dist==1)
  crois %>% group_by(X1=X1.x,X2=X2.x,X3=X3.x) %>% 
    summarise(inside=any(inside.x) & all(inside.y | !doub)) %>% 
    ungroup->air
  newN<-nrow(air %>% filter(inside))
}

# surface de l'air coincé
surfAirC<-surfTot(air %>% filter(inside))

surfLava-surfAirC
# 2540
# ok for test
