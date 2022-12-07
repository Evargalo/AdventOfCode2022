# Donnees ----

d <- read_delim("data7.txt",col_names = FALSE, delim=' ')

# d <- read_delim("data7test.txt",col_names = FALSE, delim=' ')

# Architecture des dossiers et fichier
aTree<-data.frame(parent=character(),child=character())
# Base des fichiers
files<-data.frame(size=numeric(),name=character(),folder=character())

parentFolder<-function(path){ 
  path<-substr(path,1,nchar(path)-1)
  path<-paste0(cutAfterLast(path,"/"),"/")
  path
}

pos<-"/"
# Applique une ligne d'instuction pour complèter l'arbre et la base des fichiers
read<-function(X1,X2,X3){
  if(X1=="$"){
    if(X2=="cd"){
      if(X3==".."){
        pos<<-parentFolder(pos)
      } else{
        pos<<-paste0(pos,X3,"/")
      }
    }
    if(X2=="ls"){ # rien à faire
    }
  } else{
    if(X1=="dir"){
      aTree<<-aTree %>% add_row(parent=pos,child=paste0(pos,X2))
    } else{
      files<<-files %>% add_row(size=as.numeric(X1),name=X2,folder=pos)
      aTree<<-aTree %>% add_row(parent=pos,child=X2)
    }
  }
}

# Construit l'arbre et la base des fichiers
pmap(d %>% filter(row_number()>1),read)

# Table des dossiers avec taille initialisée à 0
folders<-aTree %>% select(f=parent) %>% unique %>% mutate(s=0)

aTree %>% head(20)

# A ----

# Ajoute la taille d'un fichier à celle de tous les dossiers qui le contiennent
doFile<-function(size,folder){
  # print(folder)
  if(!folder %in% c("","/")){
    folders<<-folders %>% mutate(s=if_else(f==folder,size+s,s))
    doFile(size,parentFolder(folder))
  }
}
# Calcul des tailles de dossiers
pmap(.l = files %>% select(size,folder),.f = doFile)
# Ajout du dossier racine
folders<<-folders %>% mutate(s=if_else(f=="/",sum(files$size),s))

folders %>% arrange(desc(s)) %>% head

folders %>% filter(s<=100000) %>% summarise(sum(s))

# 1391690


# B ----

folders %>% filter(f=="/") %>% pull(s)->occupied
sizeNeeded<-occupied-40000000
folders %>% filter(s>sizeNeeded) %>% filter(s==min(s)) %>% pull(s)

# 5469168
