# Donnees ----

d <- read_delim("data2.txt",col_names = FALSE, delim=' ')

# A ----

d %<>% mutate(
  sc1=case_when( # score for shape
    X2=="X"~1,
    X2=="Y"~2,
    X2=="Z"~3,),
  sc2=case_when( # score for win or draw
    X1=="A" & X2=="Y" ~ 6,
    X1=="B" & X2=="Z" ~ 6,
    X1=="C" & X2=="X" ~ 6,
    X1=="A" & X2=="X" ~ 3,
    X1=="B" & X2=="Y" ~ 3,
    X1=="C" & X2=="Z" ~ 3,
    TRUE ~ 0)) %>% 
  mutate(sc=sc1+sc2)

sum(d$sc)

# 14827


# B ----

d %<>% mutate(
  sc1=case_when( # score for win or draw
    X2=="X"~0,
    X2=="Y"~3,
    X2=="Z"~6,),
  scX1=case_when( # opponent's score for shape
    X1=="A"~1,
    X1=="B"~2,
    X1=="C"~3,),
  sc2=case_when( # our score for shape 1,2,0
    X2=="X"~scX1-1,
    X2=="Y"~scX1,
    X2=="Z"~scX1%%3+1) ) %>% 
  mutate(
    sc2=if_else(sc2==0,3,sc2), # our score for shape 1,2,3
    sc=sc1+sc2) # total score

sum(d$sc)

# 13889