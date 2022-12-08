# Donnees ----

d <- read_delim("data8.txt",col_names = FALSE, delim=' ')

# d <- read_delim("data8test.txt",col_names = FALSE, delim=' ')

dat<-df_from_vector_of_strings(d$X1) %>% mutate_all(as.numeric) %>% as.matrix
size<-dat %>% ncol 

# A ----

res<-dat
res[,]<-1 # arbre visible sauf si caché dans les 4 directions

for(i in 2:(size-1)){
  for(j in 2:(size-1)){
    if(any(dat[1:(i-1),j]>=dat[i,j]) &
       any(dat[i,1:(j-1)]>=dat[i,j]) &
       any(dat[(i+1):size,j]>=dat[i,j]) &
       any(dat[i,(j+1):size]>=dat[i,j])
       ) res[i,j]<-0
  }
}

sum(res)

# 1669


# B ----

res[,]<-1 # scenic score

for(i in 2:(size-1)){
  for(j in 2:(size-1)){
    val<-dat[i,j] # hauteur de l'arbre
    
    if(all(dat[1:(i-1),j]<val)){
      k<-(i-1)
    } else{
      w<-which(dat[(i-1):1,j]>=val)
      k<-first(w)
    } 
    res[i,j]<-res[i,j]*k
    
    if(all(dat[size:(i+1),j]<val)){
      k<-(size-i)
    } else{
      w<-which(dat[(i+1):size,j]>=val)
      k<-first(w)
    } 
    res[i,j]<-res[i,j]*k
    
    if(all(dat[i,1:(j-1)]<val)){
      k<-(j-1)
    } else{
      w<-which(dat[i,(j-1):1]>=val)
      k<-first(w)
    } 
    res[i,j]<-res[i,j]*k
    
    if(all(dat[i,size:(j+1)]<val)){
      k<-(size-j)
    } else{
      w<-which(dat[i,(j+1):size]>=val)
      k<-first(w)
    } 
    res[i,j]<-res[i,j]*k
  }
}
max(res)

# 331344
