# Donnees ----

d <- read_delim("data7.txt",col_names = FALSE, delim=' ')

# d <- read_delim("data7test.txt",col_names = FALSE, delim=' ')

# Architecture des dossiers et fichiers
# NB : en réalité on ne l'utilise pas par la suite, construire juste la liste des dossiers suffirait !
aTree<-data.frame(parent=character(),child=character())
# Base des fichiers
files<-data.frame(size=numeric(),name=character(),folder=character())

parentFolder<-function(path){
  if(path=="/") return("")
  path<-substr(path,1,nchar(path)-1)
  path<-paste0(cutAfterLast(path,"/"),"/")
  path
}

pos<-"/"
# Applique une ligne d'instuction pour complèter l'arbre et la base des fichiers
read<-function(X1,X2,X3){
  if(X1=="$"){ # instruction
    if(X2=="cd"){
      if(X3==".."){
        pos<<-parentFolder(pos)
      }
      if(X3=="/"){
        pos<-"/"
      }
      if(! X3 %in% c("/","..")){
        pos<<-paste0(pos,X3,"/")
      }
    }
    if(X2=="ls"){ # rien à faire
    }
  }
  if(X1=="dir"){ # subfolder
    aTree<<-aTree %>% add_row(parent=pos,child=paste0(pos,X2))
  }
  if(! X1 %in% c("dir","$")){ # file
    files<<-files %>% add_row(size=as.numeric(X1),name=X2,folder=pos)
    aTree<<-aTree %>% add_row(parent=pos,child=X2)
  }
}

# Construit l'arbre et la base des fichiers
pmap(d,read)


# A ----

# Table des dossiers non vides, avec taille initialisée à 0
folders<-aTree %>% select(f=parent) %>% unique %>% mutate(s=0)

# Ajoute la taille d'un fichier à celle de tous les dossiers qui le contiennent
doFile<-function(size,folder){
  if(folder != ""){
    folders<<-folders %>% mutate(s=if_else(f==folder,size+s,s))
    doFile(size,parentFolder(folder))
  }
}
# Calcul des tailles de dossiers
pmap(.l = files %>% select(size,folder),.f = doFile)

# Somme des tailles des dossiers de taille au plus 100000
folders %>% filter(s<=100000) %>% summarise(sum(s))

# 1391690


# B ----

folders %>% filter(f=="/") %>% pull(s)->occupied
sizeNeeded<-occupied-40000000
# taille du plus petit dossier plus grand que sizeNeeded
folders %>% filter(s>sizeNeeded) %>% filter(s==min(s)) %>% pull(s)

# 5469168
