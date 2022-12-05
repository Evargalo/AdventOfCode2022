# Donnees ----

d <- read_delim("data5instr.txt",col_names = FALSE, delim=',')
# d <- read_delim("data5test.txt",col_names = FALSE, delim=',')


s <- read_delim("data5.txt",col_names = FALSE, delim='] [')
s
s %<>% mutate(X1=substr(X1,2,2), X9=substr(X9,1,1))
for(i in 1:9){
  v<-s[,i]%>% unlist %>% unname
  assign(paste0("C",i),v[v!=" "]) 
}

# A ----
applyInstr<-function(X1,X2,X3){
  if(X1>0){
    giver<-get(paste0("C",X2))
    receiver<-get(paste0("C",X3))
    receiver<-c(giver[1],receiver)
    giver<-giver[-1]
    assign(paste0("C",X2),giver, envir = .GlobalEnv)
    assign(paste0("C",X3),receiver, envir = .GlobalEnv)
    applyInstr(X1-1,X2,X3)
  }
}

pmap(d,applyInstr)

paste0(C1[1],C2[1],C3[1],C4[1],C5[1],C6[1],C7[1],C8[1],C9[1])

# WSFTMRHPP

# 


# B ----

for(i in 1:9){
  v<-s[,i]%>% unlist %>% unname
  assign(paste0("C",i),v[v!=" "]) 
}

applyInstr<-function(X1,X2,X3){
  if(X1>0){
    giver<-get(paste0("C",X2))
    receiver<-get(paste0("C",X3))
    receiver<-c(giver[1:X1],receiver)
    giver<-giver[-(1:X1)]
    assign(paste0("C",X2),giver, envir = .GlobalEnv)
    assign(paste0("C",X3),receiver, envir = .GlobalEnv)
  }
}

pmap(d,applyInstr)

paste0(C1[1],C2[1],C3[1],C4[1],C5[1],C6[1],C7[1],C8[1],C9[1])

# 