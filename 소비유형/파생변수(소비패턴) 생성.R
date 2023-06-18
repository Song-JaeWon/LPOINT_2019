library(dplyr)
library(readr)
library(tidyr)
setwd("D:/Project/2019 Lpoint/Code/Data/MERGE")
dataset <- read.csv("jangbaguni_pattern.csv")
using <- read.csv("mean_transaction_count.csv")
using$clnt_id <- unique(dataset$clnt_id)
using <- using %>% select(clnt_id, item_quantities, transaction_count)

##### Load Data
sess_df <- read.csv("buying_sess_df.csv")
setwd("D:/Project/2019 Lpoint/Code/Data/")
online_df <- read_csv("./merged_onilne_df.csv")

online_df <- online_df %>% arrange(clnt_id, sess_id)
##### 3번: 장바구니 추가, 삭제 횟수 평균
dataset$action <- dataset$add_counts + dataset$delete_counts
dataset_temp <- dataset %>% group_by(clnt_id) %>% summarise(add_or_delete = mean(action))
using$add_or_delete <- dataset_temp$add_or_delete

##### 1번: 단순하게 구매한 세션 중에서 장바구니 이용한 비율(구매세션 중 장바구니이용 수 / 구매세션수)
dataset$use_tf <- ifelse(dataset$action>0, 1, 0)
dataset_temp <- dataset %>% group_by(clnt_id) %>% summarise(use_rate = mean(use_tf))
#using$use_rate <- dataset_temp$use_rate
using <- left_join(using, dataset_temp, by="clnt_id")

##### 2번: 구매 직전 actiontype
online_df <- left_join(online_df, sess_df, by=c("clnt_id", "sess_id"))
online_df <- online_df %>% select(clnt_id, sess_id, action_type, buying_sess)
online_df <- online_df %>% filter(buying_sess==1)
online_df$six <- ifelse(online_df$action_type==6, 1, 0)
next_clnt_id <- online_df$clnt_id
next_clnt_id <- c(next_clnt_id[2:length(next_clnt_id)], next_clnt_id[length(next_clnt_id)])
online_df$next_clnt_id <- next_clnt_id
next_sess_id <- online_df$sess_id
next_sess_id <- c(next_sess_id[2:length(next_sess_id)], next_sess_id[length(next_sess_id)])
online_df$next_sess_id <- next_sess_id
next_action_id <- online_df$action_type
next_action_id <- c(next_action_id[2:length(next_action_id)], next_action_id[length(next_action_id)])
online_df$next_action_id <- next_action_id
online_df$pre_action <- ifelse(online_df$clnt_id==online_df$next_clnt_id&online_df$sess_id==online_df$sess_id&next_action_id==6, online_df$action_type, -1)
online_df_six <- online_df %>% filter(pre_action!=-1)
online_df_six <- online_df_six %>% select(clnt_id, pre_action)
online_df_six_new <- online_df_six
# 빈도 수 펼치기
online_df_six <- online_df_six %>% select(clnt_id, pre_action)
online_df_six <- online_df_six %>% group_by(clnt_id, pre_action) %>% mutate(item_sum=n())
online_df_six <- online_df_six %>% select(clnt_id, pre_action, item_sum)
online_df_six <- unique(online_df_six)
online_df_six <- as.data.frame(spread(online_df_six, pre_action ,item_sum,fill=0)) # 펼치기
colnames(online_df_six) <- c("clnt_id", "action_0", "action_1", "action_2", "action_3", "action_4", "action_5", "action_6", "action_7")
using <- left_join(using, online_df_six, by="clnt_id")
# 바로 6인경우 NA -> 0으로
using$action_0 <- ifelse(is.na(using$action_0), 0, using$action_0)
using$action_1 <- ifelse(is.na(using$action_1), 0, using$action_1)
using$action_2 <- ifelse(is.na(using$action_2), 0, using$action_2)
using$action_3 <- ifelse(is.na(using$action_3), 0, using$action_3)
using$action_4 <- ifelse(is.na(using$action_4), 0, using$action_4)
using$action_5 <- ifelse(is.na(using$action_5), 0, using$action_5)
using$action_6 <- ifelse(is.na(using$action_6), 0, using$action_6)
using$action_7 <- ifelse(is.na(using$action_7), 0, using$action_7)

##### 4번: 장바구니에서 일부만 산 경우 횟수
dataset$part <- ifelse(dataset$item_quantities < dataset$quantity_fluctuation, 1, 0)
dataset_temp <- dataset %>% group_by(clnt_id) %>% summarise(part_purchase = sum(part))
using <- left_join(using, dataset_temp, by="clnt_id")
#using$part_purchase <- dataset_temp$part_purchase

##### 구매까지 걸린 seq
#next_clnt, next_sess 같고 next_num이 1이고 next_actio이 6 이면 num 가져오기
#new_num이 1이고actiontype 6이면 0
#dataset_row <- online_df %>% select(clnt_id, sess_id, action_type, six)
dataset_row <- online_df
dataset_row <- dataset_row %>% group_by(clnt_id, sess_id, six) %>% mutate(num = row_number())
next_num <- dataset_row$num
next_num <- c(next_num[2:length(next_num)], next_num[length(next_num)])
dataset_row$next_num <- next_num
dataset_row <- dataset_row %>% group_by(clnt_id, sess_id) %>% mutate(new_num=row_number())

dataset_row$before_seq <- ifelse(dataset_row$clnt_id==dataset_row$next_clnt_id&dataset_row$sess_id==dataset_row$next_sess_id&dataset_row$next_num==1&dataset_row$next_action_id==6, dataset_row$num, -1)
dataset_row$before_seq <- ifelse(dataset_row$new_num==1&dataset_row$action_type==6, 0, dataset_row$before_seq)
dataset_row_new <- dataset_row %>% filter(before_seq!=-1)
dataset_row_new <- dataset_row_new %>% group_by(clnt_id) %>% summarise(mean_before_seq=mean(before_seq))
using <- left_join(using, dataset_row_new, by="clnt_id")
table(is.na(using))

# 저장
write.csv(using, "jangbaguni_using_dataset.csv", row.names=FALSE)
