# Donnees ----

d <- read_delim("data/data8.txt",col_names = FALSE, delim=' ')

# d <- read_delim("data/data8test.txt",col_names = FALSE, delim=' ')

dat<-df_from_vector_of_strings(d$X1) %>% mutate_all(as.numeric) %>% as.matrix
size<-dat %>% ncol 

# A ----

res<-dat
res[,]<-1 # arbre visible sauf si caché dans les 4 directions

mat<-res %>% as.logical
for(i in 2:(size-1)){
  for(j in 2:(size-1)){
    val<-dat[i,j] # hauteur de l'arbre
    mat<-dat>=val # arbres au moins aussi hauts ?
    if(any(mat[1:(i-1),j]) &
       any(mat[i,1:(j-1)]) &
       any(mat[(i+1):size,j]) &
       any(mat[i,(j+1):size])
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
    mat<-dat>=val
    
    if(!any(mat[1:(i-1),j])){
      k<-(i-1)
    } else{
      w<-which(mat[(i-1):1,j])
      k<-first(w)
    } 
    res[i,j]<-res[i,j]*k
    
    if(!any(mat[size:(i+1),j])){
      k<-(size-i)
    } else{
      w<-which(mat[(i+1):size,j])
      k<-first(w)
    } 
    res[i,j]<-res[i,j]*k
    
    if(!any(mat[i,1:(j-1)])){
      k<-(j-1)
    } else{
      w<-which(mat[i,(j-1):1])
      k<-first(w)
    } 
    res[i,j]<-res[i,j]*k
    
    if(!any(mat[i,size:(j+1)])){
      k<-(size-j)
    } else{
      w<-which(mat[i,(j+1):size])
      k<-first(w)
    } 
    res[i,j]<-res[i,j]*k
  }
}
max(res)

# 331344
