# Donnees ----

# v<-read_lines("data/data17test.txt")
v<-read_lines("data/data17.txt")
str_split_1(v,'')->s

cave<-matrix(0,3,7) # Empty space: 0 ; block : 2 ; unit falling : 1
caveInit<-cave

buffer<-matrix(data = c( # 3 empty rows
  0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,
  0,0,0,0,0,0,0
),3,7) 

newUnit<-function(i){
  i<-i%%5
  if(i==1) m<-matrix(data = c(
    0,0,1,1,1,1,0
  ),1,7) # -
  if(i==2) m<-matrix(data = c( # +
    0,0,0,1,0,0,0,
    0,0,1,1,1,0,0,
    0,0,0,1,0,0,0
  ),3,7,byrow = TRUE) 
  if(i==3) m<-matrix(data = c( # L
    0,0,0,0,1,0,0, 
    0,0,0,0,1,0,0,
    0,0,1,1,1,0,0
  ),3,7,byrow = TRUE) 
  if(i==4) m<-matrix(data = c( # I
    0,0,1,0,0,0,0,
    0,0,1,0,0,0,0,
    0,0,1,0,0,0,0,
    0,0,1,0,0,0,0
  ),4,7,byrow = TRUE) 
  if(i==0) m<-matrix(data = c( # #
    0,0,1,1,0,0,0,
    0,0,1,1,0,0,0
  ),2,7,byrow = TRUE) 
  return(m)
}

# A ----
k<-0 # index in the wind input
length(s)->l 

push<-function(){ # anyway the wind blows...
  k<<-k%%l + 1 # we loop on the wind input
  wind<-s[k]
  tC<-t(cave) # using the transposed matrix to read values row-wise
  lim<-length(c(tC))
  piece<-which(tC==1)
  if(wind =='<'){
    if(1 %in% piece){return(FALSE)}
    prev<-tC[piece-1]
    if(any(prev>=2 | # block
           piece %% 7 ==1) # wall
    ){
      return(FALSE)
    } else{
      tC[piece]<-0
      tC[piece-1]<-1
      cave<<-t(tC)
    }
  }
  if(wind =='>'){
    if(lim %in% piece){return(FALSE)}
    foll<-tC[piece+1]
    if(any(foll>=2 | piece %% 7 ==0)){
      return(FALSE)
    } else{
      tC[piece]<-0
      tC[piece+1]<-1
      cave<<-t(tC)
    }
  }
  return(TRUE)
}

push()
cave

goDown<-function(){ # let the unit fall
  piece<-which(cave==1)
  nR<-nrow(cave)
  if(any(piece%%nR==0)) { # already touching the floor
    cave[piece]<<-5+i%%5 # then we freeze
    return(FALSE)
  }
  foll<-piece+1
  if(any(cave[foll]>=2)) { # one block on the way
    cave[piece]<<-5+i%%5
    return(FALSE)
  }
  cave[piece]<<-0
  cave[foll]<<-1
  return(TRUE) # we could go down
}
goDown()

trimCave<-function(cave){ # remove empty rows on top
  fR<-cave[1,] # first row
  if(any(fR!=0)) return(cave)
  if(nrow(cave)==2) return(t(as.matrix(cave[-1,]))) # careful because R changes 1-row matrices into vectors
  return(trimCave(cave[-1,]))
}
trimCave(cave)

fall<-function(){
  push() # first the wind
  while(goDown()){ # if we go down
    push() # the wind again
  }
}

k<-0
i<-1
cave<-rbind(newUnit(i),caveInit)
fall()
cave<-trimCave(cave)
# let's go
for(i in 2:2022){
  cave<-rbind(newUnit(i),buffer,cave)
  fall()
  cave<-trimCave(cave)
}
nrow(cave)

# 3173



# B ----

saveCave<-function(){ # keep previous states
  nRC<-nrow(cave)
  cols<-rep(nRC,7) # highest block on each row
  for(j in 1:7){
    cols[j]=min(nRC,first(which(cave[,j]>=2)))
  }
  nR<-max(cols) # nb rows to keep
  skippedRows<<-skippedRows+nRC-nR
  cave<<-cave[1:nR,]
  state<-data.frame(C1=cols[1],C2=cols[2],C3=cols[3],C4=cols[4],C5=cols[5],C6=cols[6],C7=cols[7],k=k,i=i%%5,ii=i,sR=skippedRows)
  if(nrow(merge(previousStates[,1:9],state[,1:9]))>0){ # already had the same situation
    return(TRUE)
  }
  previousStates <<- previousStates %>% bind_rows(state)
  return(FALSE)
}

skippedRows<-0
previousStates<-data.frame(C1=1,C2=1,C3=1,C4=1,C5=1,C6=1,C7=1,k=0,i=0,ii=0,sR=0)
k<-0
i<-1
cave<-rbind(newUnit(i),caveInit)
fall()
cave<-trimCave(cave)

# Could use a while(), but well...
for(i in 2:2022){
  cave<-rbind(newUnit(i),buffer,cave)
  fall()
  cave<-trimCave(cave)
  if(suppressMessages(saveCave())) {break}
}
i<2022
# TRUE : Duplicated situation found !
merge(previousStates[,1:11],state[,1:9])->firstDup

path<-state$ii-firstDup$ii # how long before situation repeats
addedRows<-state$sR-firstDup$sR # how many rows have been added during one cycle

nRep<-1000000000000
nbCycles<-(nRep-96)%/%path # total number of cycles

skippedRows<-skippedRows+(nbCycles-1)*addedRows # simply skip them

j<-i+(nbCycles-1)*path+1 # and restart after the last cycle

for(i in j:nRep){
  cave<-rbind(newUnit(i),buffer,cave)
  fall()
  cave<-trimCave(cave)
}
nrow(cave)+skippedRows
# 1570930232582

# Viz ----

cavedf<-expand.grid(r=1:nrow(cave),co=1:7) %>% rowwise %>% mutate(t=cave[r,co]) %>% ungroup

cavedf %>% group_by(t) %>% count
gf_tile(cavedf %>% filter(r<50),-r~co,fill=~as.factor(t)) %>% 
  gf_refine(scale_fill_brewer(palette="Paired"),
    theme=theme_void(),
    theme(legend.position="none")
  )

require(esquisse)
ggplot(cavedf%>% filter(r<50)) +
  aes(x = co, y = -r, fill = as.factor(t)) +
  geom_tile() +
  scale_fill_brewer(palette="Paired")+
  theme_void() +
  theme(legend.position="none")  -> tetris
ggsave(filename = "programs/day17_viz.png",plot=tetris,width = 7,height = 49)
