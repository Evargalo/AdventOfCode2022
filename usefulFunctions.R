options(digits = 22)

# Création opérateur Not in  ----
'%notin%' <- Negate('%in%')

############
# Data ----


# One character per column in a df
fileToDF<-function(file) read_lines(file) %>% strsplit('') %>% reduce(rbind)

df_from_vector_of_strings<-function(v){
  n<-nchar(v[1])
  data.frame(v) %>% separate(v,into=paste0('x',0:n),sep='') %>% select(-x0)
}
# ex:
# v<-c("azerty","qsdfgh","wxcvbn")
# df_from_vector_of_strings(v)


###############
# Vectors----

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#################
# Arithmetics----

pgcd<-function(a,b){
  if(b>a) return (pgcd(b,a))
  if(b<0) return (pgcd(-b,a))
  if(b==0) return (a)
  a<-a%%b
  if(0==a) return (b)
  pgcd(b,a)
}
# pgcd(-250,75)

ppcm<-function(a,b){
  (a*b%/%pgcd(b,a)) %>% abs
}
# ppcm(-24,90)

###############
# Strings----

firstChar<-function(s) substr(s,1,1)
rmFirstChar<-function(s) {
  if(nchar(s)==0) return("")
  substr(s,2,nchar(s))
}

cutBefore<- function (text,pattern) sub(pattern=paste0(".*",pattern),"",text)
cutAfter<- function (text,pattern) sub(pattern=paste0(pattern,".*"),"",text)

cutAfter("a/df/b/jh","/")
cutBefore("a/df/b/jh","/")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

rmLastWord<-function(x) sub(pattern = "\\s*\\w*$",x = x,replacement = "")
rmFirstWord<-function(x) sub(pattern = "^.*?(\\s)",x = x,replacement = "")

cutAfterLast<- function (text,pattern) {
  gsub(pattern," ",text) %>% rmLastWord->t
  gsub(" ",pattern,t)
}
cutAfterLast("a/df/b/jh","/")

cutString<-function(s,nbParts=10){
  step<-nchar(s)%/%nbParts
  tibble(x=s) %>% separate(x,into = paste0("V",1:nbParts),sep=(1:(nbParts-1))*step) %>% unlist %>% unname()
}
# cutString("azertyuiopqsdfghjklmwxcvbnazer",7)

stringToVector<-function(s) cutString(s,nchar(s))
# stringToVector("azertyuiopqsdfghjklmwxcvbnazer")


keepOnly<-function(s,numbers=FALSE,letters=FALSE,charac=""){
  pat<-"[^"
  if(numbers) pat<-paste0(pat,"0-9")
  if(letters) pat<-paste0(pat,"a-zA-Z")
  pat<-paste0(pat,charac,"]")
  gsub(s,pattern=pat,replacement = '')
}
# keepOnly("aze zAZLKJsd g1 5676 15 , ;  : ;",letters=TRUE,charac = ",")

reduceString<-function(s,pattern="[(][)]|[[][]]|[{][}]|[<][>]"){
  n<-nchar(s)+1
  while(nchar(s)<n){
    n<-nchar(s)
    gsub(pattern,"",s)->s
  }
  s
}
#reduceString("[({(<(())[]>[[{[]{<()<>>")


###############
# Binary ----

BinToDec <- function(x, charactersForOnes="1") sum(2^(which(rev(unlist(strsplit(as.character(x), "")) %in% charactersForOnes))-1))
# ones<-c("R","B")
# x<-"BBFFBBFLRL"
# BinToDec(x,ones)

DecToBin <- function(n) {
  ifelse(n > 1, 10*DecToBin(as.integer(n/2)) + n %% 2 ,n %% 2) 
}

# DecToBin(1:16)

nb1InBinary<-c(0,1)
for(a in 1:15){nb1InBinary<-c(nb1InBinary,1+nb1InBinary)}
nb1InBinary[5:15] # Starts at zero !

###############
# Mazes----

# rawData         = vecteur de string
# charOfInterests = vecteur de char
buildMaze<-function(rawData){
  Maze<-data.frame(x=c(),y=c(),t=c(),stringsAsFactors = FALSE)
  n<-length(rawData)
  p<-nchar(rawData[1])
  # Data
  for(k in 1:n){
    line<-unlist(strsplit(rawData[k],split = "")) 
    for (l in 1:p){
      newline<-data.frame(x=k,y=l,t=line[l],stringsAsFactors = FALSE)
      Maze<-Maze %>% bind_rows(newline)
    }
  }
  res<-list("Maze"=Maze,"foundChar"=unique(Maze$t))
  for (c in unique(Maze$t)){
    assign(x = paste0("Char",c,"Spots"),
           Maze %>% filter(t==c) %>% 
             select(x,y))
    res[[paste0("Char",c,"Spots")]]<-get(x = paste0("Char",c,"Spots"))
  }
  res
}


# Example
# rawData <- X3DecembreInput$X1
# rawData %>% sapply(function(x) str_replace_all(string = x, pattern = "#", replacement = "B")) %>% 
#   unname() -> rawData
# mazeInfo<-buildMaze(rawData)

find4Neighbours<-function(xx,yy,spots){
  spots %>% filter (((xx-x==0) & ((yy-y) %in% c(-1,1))) | ((yy-y==0) & ((xx-x) %in% c(-1,1))) )
}

find8Neighbours<-function(xx,yy,spots){
  spots %>% filter ( ((xx-x) %in% (-1):1) & ((yy-y) %in% (-1):1) & ((xx!=x) | (yy!=y)) )
}

find9Neighbours<-function(xx,yy,spots){
  spots %>% filter ( ((xx-x) %in% (-1):1) & ((yy-y) %in% (-1):1) )
}

countNeighbours<-function(xx,yy,spots){
  find4Neighbours(xx,yy,spots) %>% nrow()
}

directions<-list(c(1,0),c(0,1),c(1,1),c(-1,1),c(1,-1),c(-1,-1),c(-1,0),c(0,-1))

buildTableNeighbour<-function(maze){
  limx<-max(maze$x)
  limy<-max(maze$y)
  tableNeighbour1<-data.frame(a=c(0),b=c(0),c=c(0),d=c(0))
  for(j in 1:nrow(maze)){
    a<-maze$x[j]
    b<-maze$y[j]
    for (dir in directions){
      place<-c(a,b)+dir
      tableNeighbour1 %>% add_row(a=a,b=b,c=place[1],d=place[2]) ->tableNeighbour1
    }
    if(j%%50 == 0){
      print(j)
    }
  }
  tableNeighbour1 %>% filter(c>0 & d>0 & c<limx+1 & d<limy+1) ->tableNeighbour1
  tableNeighbour1
}
# walls<-c("B")
# ways<-c(".")

crossRoads<-function(mazeInfo,walls,ways){
  mazeInfo$Maze %>% filter(t %in% ways) %>% select(x,y) -> canPass
  canPass %>% mutate(xx=x,yy=y) %>% select(xx,yy) -> crossRoads
  crossRoads$pw=pmap_int(crossRoads,countNeighbours,spots=canPass)
  crossRoads %>% filter(pw>2)
}

# countNeighbours(316,3,canPass)
# crossRoads(mazeInfo,walls,ways)

fillDeadEnds<-function(mazeInfo,walls,ways,keys){
  w<-walls[1]
  mazeInfo$Maze -> newMaze
  mazeInfo$Maze %>% filter(t == "B") %>% select(x,y) -> blocks
  mazeInfo$Maze %>% filter(t %in% walls) %>% select(x,y) -> blocks
  mazeInfo$Maze %>% filter(t %in% ways) -> canPass
  canPass$bw=pmap_int(canPass %>% select(x,y),countNeighbours,spots=blocks)
  canPass %>% filter((bw>2) & !(t %in% keys) ) %>% select(x,y) %>% mutate(t=w) -> deadEnds
  while (nrow(deadEnds)>0) {
    newMaze %>% anti_join(deadEnds,by=c("x","y")) %>% bind_rows(deadEnds) -> newMaze
    newMaze %>% filter(t %in% walls) %>% select(x,y) -> blocks
    newMaze %>% filter(t %in% ways) -> canPass
    canPass$bw=pmap_int(canPass %>% select(x,y),countNeighbours,spots=blocks)
    canPass %>% filter((bw>2)& !(t %in% keys)) %>% select(x,y) %>% mutate(t=w) -> deadEnds
  }
  res<-list("Maze"=newMaze,"foundChar"=unique(newMaze$t),"deadEnds"=deadEnds)
  for (c in unique(newMaze$t)){
    assign(x = paste0("Char",c,"Spots"),
           newMaze %>% filter(t==c) %>% 
             select(x,y))
    res[[paste0("Char",c,"Spots")]]<-get(x = paste0("Char",c,"Spots"))
  }
  res
}

# fillDeadEnds(smallMaze,walls,ways,c(""))->newSmallMaze


drawMaze<-function(maze){
  x<-maze$y 
  y<-(-maze$x)
  t<-maze$t
  gf_tile(y~x,fill=~maze$t)
}
# drawMaze(mazeInfo$Maze)

# smallMaze<-list(Maze=mazeInfo$Maze %>% filter(x<15 & y<15))
# drawMaze(smallMaze$Maze)

distanceInMaze<-function(mazeInfo,origin,walls,ways,keys){
  endPointChar<-"£"
  keys<-c(keys,endPointChar)
  ways<-c(ways,endPointChar)
  mazeInfo$Maze %>% mutate(t=ifelse(test = (x==origin$x & y==origin$y),
                                    endPointChar,t))->mazeInfo$Maze
  #fillDeadEnds(mazeInfo,walls,ways,keys)->newSmallMaze
  mazeInfo->newSmallMaze
  newSmallMaze$Maze %>% filter(t %in% ways) -> canPass
  current_step<-0
  canPass %>% mutate(dist=ifelse(test = (t==endPointChar),current_step,-1)) -> canPass
  canPass %>% filter(dist==current_step) %>% mutate(xx=x,yy=y)->newCalc
  while(nrow(newCalc)>0){
    current_step<-current_step+1
    pmap_dfr(.l=newCalc %>% select(xx,yy),.f=findNeighbours,spots=(canPass %>% filter(dist==-1))) -> newCalc
    newCalc %>% mutate(dist=current_step) ->newCalc
    canPass %>% anti_join(newCalc,by=c("x","y")) %>% bind_rows(newCalc) %>% unique ->canPass
    newCalc %>% mutate(xx=x,yy=y)->newCalc
  }
  return(canPass)
}
# origin<-list(x=5,y=5)
# keys<-c("")
# distanceInMaze(smallMaze,origin,walls,ways,keys)->newMaze
# gf_tile(newMaze,-x~y,fill=~dist)

###############
# Matrices ----
x<-c(1:20)
M<-matrix(data = x,nrow = 4,byrow = TRUE)
t(M)
N<- M %*% t(M)
eigen(N)
diag(x = 1:5)
N^3 #terme à terme
# M matrice carrée, calcule M^p
matrixPow<-function(M,p){
  if(p==0) return (diag(x = 1,nrow = nrow(M)))
  if(p%%2==0) {
    N<-matrixPow(M,p/2)
    return(N %*% N)
  } 
  return(M %*% matrixPow(M,p-1))
}
matrixPow(N/5,3)

###############
# Graphs ----

# https://igraph.org/r/#docs
t <- read_delim("start-A
start-b
A-c
A-b
b-d
A-end
b-end", col_names = FALSE, delim='-')

g <- graph_from_data_frame(t,directed = FALSE)
g <- graph_from_data_frame(t,directed = TRUE)
plot(g)
g %>% get.vertex.attribute()
g %>% get.diameter()
g %>% gsize
g %>% 
  add.vertices(1,name="e") %>% 
  add.edges(c("start","e", "e","A", "end","e")) ->g 
g %>% plot
g %>% delete_vertices(c("b","end")) %>% plot

get.all.shortest.paths(g,"start","end")
get.all.shortest.paths(g,"start","end",mode = "all")
get.all.shortest.paths(g,"end","start")
get.all.shortest.paths(g,"end","start",mode = "all")

g %>% get.adjacency()->adj
adj %>% as.matrix
adj %>% matrixPow(10)

g %>% all_simple_paths("start","end","all")
g %>% all_shortest_paths("start")

###############
# Trees ----

make_tree(15, 2, mode = "out") %>% plot
make_tree(11, 3, mode = "undirected") %>% plot

fillTree<-function(container,inside){
  aTree<-data.frame(parent=c(),child=c(),nb=c())
  children<-rmLastWord(trim(unlist(strsplit(inside,","))))
  for (child  in children){
    nb<-sub(pattern = "\\s.*",x = child,replacement = "")
    if(!(nb%in%c("0","no"))) {
      nb<-as.numeric(nb)
      child<-rmFirstWord(child)
      newRow<-data.frame(parent=container,child=child,nb=nb)
      aTree<-aTree %>% bind_rows(newRow)
    }
  }
  aTree
}

# treeDF<-pmap_dfr(.l = Day07A,.f = fillTree)

addChildren<-function(xNode,treeDF){
  xNode$Get("name")->x
  treeDF %>% filter(parent==x) -> subDF
  if(nrow(subDF)>0){
    for(i in 1:nrow(subDF)){
      child<-subDF$child[i]
      nb<-subDF$nb[i]
      xNode$AddChild(name=child,nb=nb)
      addChildren(xNode$Climb(name=child))
    }
  }
}
# colorsTree<-Node$new(name = "shiny gold")
# colorsTree$nb<-1
# addChildren(colorsTree)

###############
# BootCode ----

# colnames(bootCode)<-(c("ope","arg"))
execBootCode<-function(bootCode){
  accum<-0
  index<-1
  nbInstr<-nrow(bootCode)
  positions<-c()
  while(!(index%in%positions) & (index<nbInstr+1)){
    positions<-c(positions,index)
    action<-bootCode$ope[index]
    x<-bootCode$arg[index]
    if(action== "nop") {
      index<-index+1
    }
    if(action== "acc"){
      accum<-accum+x
      index<-index+1
    }
    if(action== "jmp"){
      index<-index+x
    }
  }
  if(index>nbInstr) return(c(1,accum))
  else return(c(0,accum))
}



###############
# Permutations----
# https://www.rdocumentation.org/packages/permutations/versions/1.0-9
#library(permutations)
troisPerm<-rperm(3,10)
as.word(troisPerm)
as.cycle(troisPerm)
inverse(troisPerm)
permorder(troisPerm)
unePerm<-troisPerm[1]
unePerm
as.word(unePerm)
?as.word
permutation(unePerm)
troisPerm[1]*troisPerm[2]
# troisPerm[1]+troisPerm[2] seulement pour des cycles disjoints
troisPerm[3]^4
f<-as.word(c(5,1,6,9,2,4,7,8,3))
f
options(print_word_as_cycle=FALSE)
f
as.matrix(as.word(troisPerm))
###############
??rperm
