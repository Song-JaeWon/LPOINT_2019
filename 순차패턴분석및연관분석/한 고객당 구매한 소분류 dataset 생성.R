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
#buying <- read_csv("./MERGE/buying_sess_df.csv")
#buying <- buying %>% filter(buying_sess>0)

# 상품분류 확인
table <- as.data.frame(table(dataset$clac_nm1))
table <- table %>% arrange(desc(Freq))
write.csv(table, "product_table.csv", row.names=FALSE)

# 구매한 세션만
#dataset <- left_join(dataset, buying, by=c("clnt_id", "sess_id"))
#dataset <- dataset %>% filter(!is.na(dataset$buying_sess))
# 필요한 변수만
dataset_product <- dataset %>% select(clnt_id, sess_id, clac_nm1, clac_nm2, clac_nm3)
dataset_product <- dataset_product %>% filter(!is.na(clac_nm1))
# 소분류 기준으로 펼치기
dataset_product <- dataset_product %>% select(clnt_id, clac_nm3)
dataset_product <- dataset_product %>% group_by(clnt_id, clac_nm3) %>% mutate(item_sum=n())
dataset_product <- dataset_product %>% select(clnt_id, clac_nm3, item_sum)
dataset_product <- unique(dataset_product)
dataset_product <- as.data.frame(spread(dataset_product, clac_nm3 ,item_sum,fill=0)) # 펼치기
# 행이 7421밖에 안되는 이유: clac_nm의 na 때문 -> 데이터 있는 것에 대해서만 학습시키기.
#colnames(dataset_product) <- str_replace_all(colnames(dataset_product), " ","") # 띄어쓰기 없애
#colnames(dataset_product) <- str_replace_all(colnames(dataset_product), "/", "_")
#colnames(dataset_product) <- str_replace_all(colnames(dataset_product), "'", "")
dataset_product <- dataset_product[,1:ncol(dataset_product)-1]
dataset_product_column <- as.data.frame(colnames(dataset_product))

# kelp만 맨 뒤로 옮기기
kelp <- dataset_product %>% select(kelp)
dataset_product <- dataset_product %>% select(-kelp)
dataset_product$kelp <- kelp
dataset_product_column <- as.data.frame(colnames(dataset_product))

write.csv(dataset_product, "dataset_product.csv", row.names=FALSE, fileEncoding = "UTF-8")

head(dataset_product)
