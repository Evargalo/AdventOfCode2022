# Donnees ----

source("packages.R")

# v<-read_lines("data/data25test.txt")
v<-read_lines("data/data25.txt")

# A ----
SNAFUToDec <- function(x) {
  if(nchar(x)==0) return(0)
  digit<-substr(x,nchar(x),nchar(x))
  val<-case_when(
    digit=="=" ~ -2,
    digit=="-" ~ -1,
    digit=="0" ~ 0,
    digit=="1" ~ 1,
    digit=="2" ~ 2
  )
  val+5*SNAFUToDec(substr(x,1,nchar(x)-1))
}
tot<-map(v,SNAFUToDec) %>% unlist %>% sum

DecTo5 <- function(n) {
  ifelse(n > 4, 10*DecTo5(as.integer(n/5)) + n %% 5 ,n %% 5) 
}
# Test
DecTo5(4890)
124030
SNAFUToDec("2=-1=0")

DecTo5 <- function(n) {
  ifelse(n > 4, paste0(DecTo5(n %/% 5), n %% 5) ,n %% 5) 
}

DecTo5(tot)
# 13333423411212430021
# Conversion by hand :
# 2----0=--1122=0=0021

# Conversion by algorithm :
FivetoSNAFU<-function(x){
  # print(x)
  if(x==0) return("")
  rest<-x%/%10
  digit<-x%%10
  if(digit%in%5:9){
    rest<-rest+1
    digit<-digit-5
  }
  digit<-as.character(digit)
  if(digit=="4"){
    rest<-rest+1
    digit<-"-"
  }
  if(digit=="3"){
    rest<-rest+1
    digit<-"="
  }
  paste0(FivetoSNAFU(rest),digit)
}
FivetoSNAFU(124030)
FivetoSNAFU(as.numeric(DecTo5(tot)))
FivetoSNAFU(13333423411212430021)
# "2----0=--1122=0-==02"

# B ----

# Merry Christmas ! 