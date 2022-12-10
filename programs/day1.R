# Donnees ----
read_lines("data/data1b.txt") %>% paste0(collapse = "") %>% strsplit(',,') %>% unlist ->v

# A ----
s<-c()
for(k in 1:250){
  vv<-v[k] %>% strsplit(',') %>% unlist %>% as.numeric %>% sum
  s<-c(s,vv)
}
max(s)

# Réponse
# 69693

# B ----
s %>% sort %>% tail(3) %>% sum

# Réponse
# 200945