##### Import Library
library(dplyr)
library(readr)
library(ggplot2)
library(writexl)
library(stringr)
library(tidyr)
library(arules)
library(arulesSequences)

##### import data
setwd("D:/Project/2019 Lpoint/Code/Data")
dataset <- read_csv("./merged_onilne_df.csv") 

##### 5 제외 / 6 포함된 session만
##### 구매횟수 테이블(1이상)
purchase_one <- dataset %>% select(clnt_id, sess_id, trans_id)
purchase_one <- purchase_one %>% filter(!is.na(trans_id))
purchase_one <- unique(purchase_one)
purchase_one <- purchase_one %>% group_by(clnt_id) %>% summarise(purchase_count = n())

############### 1. 1회 구매자

#### 1회 구매자 list
purchase_one_list <- purchase_one %>% filter(purchase_count==1)
purchase_one_list <- purchase_one_list$clnt_id

##### row
action_trans <- dataset %>% select(clnt_id, action_type, sess_id, hit_seq)
action_trans <- action_trans %>% filter(clnt_id %in% purchase_one_list) # 여기서 1회 구매자 filtering
action_trans$clnt_sess <- paste(action_trans$clnt_id, action_trans$sess_id, sep="_")
action_trans$size <- 1
action_trans <- action_trans %>% select(clnt_sess, hit_seq, size, action_type)
action_trans <- unique(action_trans)
action_trans <- action_trans %>% arrange(clnt_sess, hit_seq)

##### Action type에 6(구매) 포함된 session만 filtering
include_purchase <- action_trans %>% filter(action_type==6)
include_purchase <- unique(include_purchase$clnt_sess)

action_trans_purchase <- action_trans %>% filter(clnt_sess %in% include_purchase)

##### 5 제외
action_trans_purchase_exclude_1purchase <- action_trans_purchase %>% filter(action_type!=5)

##### 저장
write.table(action_trans_1purchase, "action_trans_1purchase.txt", sep=" ", row.names=FALSE, quote=FALSE, col.names=FALSE)

##### R cONSOLE에서 실행 #####
setwd("D:/Project/2019 Lpoint/Code/Data")
action_trans_baskets <- read_baskets(con = "action_trans_1purchase.txt", info = c("sequenceID", "eventID", "SIZE"))
seq_rule <- cspade(action_trans_baskets, parameter = list(support=0.2), control= list(verbose = TRUE))
result <- as(seq_rule, "data.frame")
result <- result %>% arrange(desc(support))
write.csv(result, "action_trans_1purchase.csv")
##### R cONSOLE에서 실행 #####

############### 2. 2회 이상 구매자

#### 2회 구매자 list
purchase_more2_list <- purchase_one %>% filter(purchase_count>1)
purchase_more2_list <- purchase_one_list$clnt_id

##### row
action_trans <- dataset %>% select(clnt_id, action_type, sess_id, hit_seq)
action_trans <- action_trans %>% filter(clnt_id %in% purchase_more2_list) # 여기서 2회 이상 구매자 filtering 
action_trans$clnt_sess <- paste(action_trans$clnt_id, action_trans$sess_id, sep="_")
action_trans$size <- 1
action_trans <- action_trans %>% select(clnt_sess, hit_seq, size, action_type)
action_trans <- unique(action_trans)
action_trans <- action_trans %>% arrange(clnt_sess, hit_seq)

##### Action type에 6(구매) 포함된 session만 filtering
include_purchase <- action_trans %>% filter(action_type==6)
include_purchase <- unique(include_purchase$clnt_sess)

action_trans_purchase <- action_trans %>% filter(clnt_sess %in% include_purchase)

##### 5 제외
action_trans_more2purchase <- action_trans_purchase %>% filter(action_type!=5)

##### 저장
write.table(action_trans_more2purchase, "action_trans_more2purchase.txt", sep=" ", row.names=FALSE, quote=FALSE, col.names=FALSE)

##### R cONSOLE에서 실행 #####
setwd("D:/Project/2019 Lpoint/Code/Data")
action_trans_baskets <- read_baskets(con = "action_trans_more2purchase.txt", info = c("sequenceID", "eventID", "SIZE"))
seq_rule <- cspade(action_trans_baskets, parameter = list(support=0.2), control= list(verbose = TRUE))
result <- as(seq_rule, "data.frame")
result <- result %>% arrange(desc(support))
write.csv(result, "action_trans_more2purchase.csv")
##### R cONSOLE에서 실행 #####

############### 3. 전체 대상(1회 이상 구매자) <- 6 포함된 session이므로

##### row
action_trans <- dataset %>% select(clnt_id, action_type, sess_id, hit_seq)
action_trans$clnt_sess <- paste(action_trans$clnt_id, action_trans$sess_id, sep="_")
action_trans$size <- 1
action_trans <- action_trans %>% select(clnt_sess, hit_seq, size, action_type)
action_trans <- unique(action_trans)
action_trans <- action_trans %>% arrange(clnt_sess, hit_seq)

##### Action type에 6(구매) 포함된 session만 filtering
include_purchase <- action_trans %>% filter(action_type==6)
include_purchase <- unique(include_purchase$clnt_sess)

action_trans_purchase <- action_trans %>% filter(clnt_sess %in% include_purchase)

##### 5 제외
action_trans_all <- action_trans_purchase %>% filter(action_type!=5)

##### 저장
write.table(action_trans_all, "action_trans_all.txt", sep=" ", row.names=FALSE, quote=FALSE, col.names=FALSE)

##### R cONSOLE에서 실행 #####
setwd("D:/Project/2019 Lpoint/Code/Data")
action_trans_baskets <- read_baskets(con = "action_trans_all.txt", info = c("sequenceID", "eventID", "SIZE"))
seq_rule <- cspade(action_trans_baskets, parameter = list(support=0.2), control= list(verbose = TRUE))
result <- as(seq_rule, "data.frame")
result <- result %>% arrange(desc(support))
write.csv(result, "action_trans_all.csv")
##### R cONSOLE에서 실행 #####

