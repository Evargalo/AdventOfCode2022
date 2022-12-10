# Donnees ----

d <- read_delim("data/data10.txt",col_names = FALSE, delim=' ')
# d <- read_delim("data/data10test.txt",col_names = FALSE, delim=' ')

# A ----
X<-1  # registre
vals<-c() # valeurs du registre à chaque cycle

exec<-function(X1,X2){
  vals<<-c(vals,X) # dans tous les cas, 1 cycle sans changer la valeur du registre
  if(X1=="addx"){  # si addx, 1 cycle supplémentaire et nouvelle valeur
    vals<<-c(vals,X)
    X<<-X+X2
  }
}
pmap(d,exec)

# somme des valeurs après 20,60,100,140,180,220 cycles
(20+40*(0:5))->cycles
sum(cycles*vals[cycles])
# 13740

# B ----
pos<-data.frame(CRT=0:239,stripe=vals)
pos %<>% rowwise %>% 
  mutate(ink=CRT%%40 %in% (stripe-1):(stripe+1),
         pixel=if_else(ink,"#",".")) %>% 
  ungroup

# Affichage
pos %>% 
  mutate(x=CRT%%40,y=CRT%/%40) %>%
  gf_tile((-y)~x,fill = ~pixel)%>%
  gf_refine(theme=theme_bw(),scale_fill_manual(values=c("black","lightgrey")))
# ZUPRFECL

# Sans package graphique :
matrix(data = pos$pixel,nrow = 6,ncol = 40,byrow = TRUE)
# ZUPRFECL

 

