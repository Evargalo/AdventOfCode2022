# Donnees ----

# v<-read_lines("data/data14test.txt")
v<-read_lines("data/data14.txt")
d <- str_split(v,c(' -> '))
# df with rocks' positions
rock<-data.frame(X1 = numeric(),X2 = numeric())
for(i in 1:length(v)){
  d[i] %>% keepOnly(numbers = TRUE,charac=',')
  e <- str_split(d[i] %>% keepOnly(numbers = TRUE,charac=','),' , ') %>% unlist
  m<-matrix(str_split(e[1],',')%>% unlist%>% as.numeric
            ,ncol=2,byrow = TRUE)
  for(j in 1:(nrow(m)-1)){
    rock %<>% bind_rows(expand_grid(X1=m[j,1]:m[j+1,1],X2=m[j,2]:m[j+1,2]))
  }
}
rock %<>% unique
# 
# minX1<-min(rock$X1)
# maxX1<-max(rock$X1)
# minX2<-min(rock$X2)
# maxX2<-max(rock$X2)
# 
# df<-expand.grid(X1=(minX1-1):(maxX1+1),X2=0:maxX2,t=".")
# rock %>% mutate(t="#") %>% bind_rows(df) ->temp  
# temp %>% filter(!duplicated(temp %>% select(X1,X2))) -> df
# 
# df %<>% filter(X2>=abs(X1-500))
# df2<-df %>% filter(t!=".") 

df2<-rock # all obstacles

# A ----

sand3<-function(X1S=500,X2S=0,prevX1=500,prevX2=0){
  # if sand is out of bounds it is over 
  if(X2S>=maxX2 | X1S>maxX1 | X1S<minX1) return(list(10000,10000,10000,10000))
  # Obstacles right under sand
  temp<-df2 %>% filter(X1==X1S,X2>X2S) %>% arrange(X2) %>% first
  # if not obstacle it is over
  if(nrow(temp)==0) return(list(10000,10000,10000,10000))
  # If the first obstacle is far sand reaches the spot just above it
  if(temp$X2>X2S+1) return(sand3(X1S,temp$X2-1,X1S,temp$X2-2))
  # Otherwise an obstacle is just below 
  # Sand tries to go left
  if(df2 %>% summarise(sum(X1==X1S-1 & X2==X2S+1))==0) return (sand3(X1S-1,X2S+1,X1S,X2S)) 
  # Sand tries to go right
  if(df2 %>% summarise(sum(X1==X1S+1 & X2==X2S+1))==0) return (sand3(X1S+1,X2S+1,X1S,X2S)) 
  # If nothing has worked sand stays on spot
  return(list(X1S,X2S,prevX1,prevX2))
}

stop<-FALSE
X1<-500
X2<-0
while(!stop){
  s<-sand3(X1,X2)
  if(s[[1]]==10000 & s[[2]]==10000){ # sand falls out of bounds, it is over
    stop<-TRUE
  } else{
    df2 %<>% add_row(X1=s[[1]],X2=s[[2]]) # new obstacle found
    # To save time, next step will start from the penultimate position
    X1<-s[[3]]
    X2<-s[[4]]
  }
}
nrow(df2)-nrow(rock) # number of obstacles that are not a rock
# 1199



# B ----

# B Direct approach: ----
# the only unreachable points are the ones with 3 rocks or unreachable points above them
# Said otherwise, any point with a reachable spot in either of the three positions above will be reached.

bottom<-maxX2+2

nR<-bottom
nC<-2*nR-1
m<-matrix("-",nrow = nR,ncol = nC)
m[1,nR]<-"o" # The source of all sand

rock %<>% mutate(X3=nR-500+X1) # col_number in m

for(i in 2:nR){
  for(j in 1:nC){
    if(any((rock$X3==j)&(rock$X2==i-1))) { # This spot is a rock
      m[i,j]<-"#"
    }else { # This spot will eventually be reached if any of the three spots above have is reached
      jj<-max(j-1,1)
      jjj<-min(j+1,nC)
      if(any(m[i-1,jj:jjj]=="o")) m[i,j]<-"o"   
    }
  }
}
sum(m=="o")
# 23925

# Visualization
df2 %<>% mutate(t=if_else(row_number()<=nrow(rock),"#","o"))
gf_tile(object = df2,gformula = (-X2)~X1,fill = ~t) %>% 
  gf_refine(theme=theme_void())


# B Old version like in A: works fine for test, quite slow for real data ----

df2<-rock %>% mutate(t="#") # type of obstacle, added for graph

sand3<-function(X1S=500,X2S=0,prevX1=500,prevX2=0){
  # At floor level sand can't go further
  if(X2S+1==bottom) return(list(X1S,X2S,prevX1,prevX2))
  # Obstacles right under sand
  temp<-df2 %>% filter(X1==X1S,X2>X2S) %>% arrange(X2) %>% first
  # No obstacle: sand reaches the floor
  if(nrow(temp)==0) return(list(X1S,bottom-1,X1S,X2S))
  # Obstacle far below: sand arrives above the obstacle
  if(temp$X2>X2S+1) return(sand3(X1S,temp$X2-1,X1S,temp$X2-2))
  # No obstacle on the left : sand goes there
  if(df2 %>% summarise(sum(X1==X1S-1 & X2==X2S+1))==0) return (sand3(X1S-1,X2S+1,X1S,X2S)) 
  # No obstacle on the right : sand goes there
  if(df2 %>% summarise(sum(X1==X1S+1 & X2==X2S+1))==0) return (sand3(X1S+1,X2S+1,X1S,X2S))
  # Otherwise sand can't go anywhere anymore, it lays there.
  # print(X1S); print(X2S)
  return(list(X1S,X2S,prevX1,prevX2))
}

stop<-FALSE
nbSand<-0
X1<-500
X2<-0
while(!stop){
  s<-sand3(X1,X2)
  if(s[[1]]==500 & s[[2]]==0){stop<-TRUE} # We stop if some sand stays on (500,0)
  df2 %<>% add_row(X1=s[[1]],X2=s[[2]],t="o") # A new spot is occupied by sand
  nbSand<-nbSand+1
  if(nbSand%%1000==0){print(nbSand);assign(paste0("df2_",nbSand),df2)}
  # To save time, next step will start from the penultimate position
  X1<-s[[3]]
  X2<-s[[4]]
}

df2 %>% unique %>% group_by(t) %>% count()
nbSand
# Works for test, slow for real data

# Still a nice graph though
gf_tile(object = df2,gformula = (-X2)~X1,fill = ~t)

