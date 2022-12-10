# Donnees ----

d <- read_delim("data/data9.txt",col_names = FALSE, delim=' ')

# d <- read_delim("data/data9test.txt",col_names = FALSE, delim=' ')
# d <- read_delim("data/data9test2.txt",col_names = FALSE, delim=' ')

# A ----

xH<-0
xT<-0
yH<-0  
yT<-0
# positions of the tail
trace<-data.frame(xT,yT)

move<-function(X1,X2){
  if(X2>0){ # move head
    if(X1=="R"){
      xH<<-xH+1
    }
    if(X1=="L"){
      xH<<-xH-1
    }
    if(X1=="D"){
      yH<<-yH-1
    }
    if(X1=="U"){
      yH<<-yH+1
    }
    if(abs(xH-xT)>1 | abs(yH-yT)>1) { # move tail
      if(xH!=xT) xT<<-xT+sign(xH-xT)
      if(yH!=yT) yT<<-yT+sign(yH-yT)
      trace<<-trace %>% add_row(xT,yT) 
    }
    move(X1,X2-1)  # next move
  }
}

pmap(d,move)

# Different positions of the tail
trace %<>% unique
trace %>% nrow
# 6269


# B ----

# current state of the chain
rope<-data.frame(x=rep(0,10),y=rep(0,10))
# Positions of the last link
trace<-data.frame(xT=0,yT=0)

# One link of the chain follows the previous one
follow<-function(i){
  xH<-rope[i,1]
  yH<-rope[i,2]
  xT<-rope[i+1,1]
  yT<-rope[i+1,2]
  if(abs(xH-xT)>1 | abs(yH-yT)>1) { # need to move the next link ?
    if(xH!=xT) xT<-xT+sign(xH-xT)
    if(yH!=yT) yT<-yT+sign(yH-yT)
    # save positions
    rope[i,1]<<-xH
    rope[i,2]<<-yH
    rope[i+1,1]<<-xT
    rope[i+1,2]<<- yT
    if(i==9) {
      trace<<-trace %>% add_row(xT,yT) # save new tail position
      } else {follow(i+1)} # next link
  }
}

move<-function(X1,X2){
  if(X2>0){ # move the head
    if(X1=="R"){
      rope[1,1]<<-rope[1,1]+1
    }
    if(X1=="L"){
      rope[1,1]<<-rope[1,1]-1
    }
    if(X1=="D"){
      rope[1,2]<<-rope[1,2]-1
    }
    if(X1=="U"){
      rope[1,2]<<-rope[1,2]+1
    }
    follow(1) # the chain follows the head
    move(X1,X2-1)  # next move
  }
}

pmap(d,move)

# Different positions of the tail
trace %<>% unique
trace %>% nrow

# 2557
