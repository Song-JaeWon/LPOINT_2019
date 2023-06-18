##### Import Library
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(arules)
library(arulesSequences)

##### Load Data
setwd("D:/Project/2019 Lpoint/Code/Data")
dataset <- read_csv("./merged_onilne_df.csv")

purchase <- dataset %>% select(clnt_id, sess_id, clac_nm3)
purchase$clnt_sess <- paste(purchase$clnt_id, purchase$sess_id, sep="_")
purchase <- purchase %>% select(clnt_sess, clac_nm3)
purchase <- purchase %>% filter(!is.na(clac_nm3))

# transaction data 생성
purchase_list <- split(purchase$clac_nm3, purchase$clnt_sess)
purchase_transaction <- as(purchase_list, "transactions")
purchase_transaction

# 연관분석 적정 파라미터 유추
ecl <- eclat(purchase_transaction, parameter=list(support=0.001,minlen=2 ,maxlen=10))
ecl
inspect(sort(ecl)[1:50])

# 연관분석
rules <- apriori(purchase_transaction,
                 parameter=list(support=0.001, confidence=0.4, minlen=1, maxlen=10, smax=1))
summary(rules)
rules_list <- as.data.frame(inspect(rules))
rules_list <- rules_list[order(rules_list$confidence, decreasing=TRUE),]

write.csv(rules_list, "clac_nm3_rules_list.csv", row.names=FALSE)
