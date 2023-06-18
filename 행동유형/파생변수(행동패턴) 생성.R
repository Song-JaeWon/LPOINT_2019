##### Import Library
library(dplyr)
library(readr)
library(ggplot2)
library(writexl)
library(stringr)
library(tidyr)

##### Load Data
setwd("D:/Project/2019 Lpoint/Code/Data/")
#customer <- read_csv("./Customer.csv")
online <- read_csv("./Online.csv")
#product <- read_csv("./Product.csv")
#transaction <- read_csv("./Transaction.csv")
dataset <- read_csv("./merged_onilne_df.csv")

##### 0. check
colnames(dataset)
head(dataset)
dataset_agg <- dataset

########## 1. clnt_id 기준
##### sess_id 횟수
clnt_sess <- dataset %>% select(clnt_id, sess_id)
clnt_sess <- clnt_sess %>% group_by(clnt_id) %>% summarise(num_sess = max(sess_id))
dataset_agg <- clnt_sess

##### action type 펼치기
clnt_action <- dataset %>% select(clnt_id, action_type)
clnt_action <- clnt_action %>% group_by(clnt_id, action_type) %>% mutate(item_sum=n())
clnt_action <- unique(clnt_action) #중복제거
clnt_action <- as.data.frame(spread(clnt_action, action_type ,item_sum,fill=0)) # 펼치기
# 비율로 변경
colnames(clnt_action) <- c("clnt_id", "action_0", "action_1", "action_2", "action_3", "action_4", "action_5", "action_6", "action_7")
sum <- clnt_action$action_0 + clnt_action$action_1 + clnt_action$action_2 + clnt_action$action_3 + clnt_action$action_4 + clnt_action$action_5 + clnt_action$action_6 + clnt_action$action_7
clnt_action$action_0 <- round(clnt_action$action_0 / sum,3)
clnt_action$action_1 <- round(clnt_action$action_1 / sum,3)
clnt_action$action_2 <- round(clnt_action$action_2 / sum,3)
clnt_action$action_3 <- round(clnt_action$action_3 / sum,3)
clnt_action$action_4 <- round(clnt_action$action_4 / sum,3)
clnt_action$action_5 <- round(clnt_action$action_5 / sum,3)
clnt_action$action_6 <- round(clnt_action$action_6 / sum,3)
clnt_action$action_7 <- round(clnt_action$action_7 / sum,3)
dataset_agg <- left_join(dataset_agg, clnt_action, by="clnt_id")

##### action_type의 다양성

##### biz_unit 펼치기(sess 내에서 동일하므로 sess당 하나로 간주)
# 한 사람당 한 biz_unit만 가짐
clnt_biz <- dataset %>% select(clnt_id, biz_unit)
clnt_biz <- unique(clnt_biz)
dataset_agg <- left_join(dataset_agg, clnt_biz, by="clnt_id")

##### tot_pag_view_ct 평균
clnt_pag_view <- dataset %>% select(clnt_id, sess_id, tot_pag_view_ct)
clnt_pag_view <- unique(clnt_pag_view)
clnt_pag_view <- clnt_pag_view %>% group_by(clnt_id) %>% summarise(pag_view_mean = mean(tot_pag_view_ct))
dataset_agg <- left_join(dataset_agg, clnt_pag_view, by="clnt_id")
dataset_agg$pag_view_mean <- round(dataset_agg$pag_view_mean, 3)

##### tot_sess_hr_v 평균
clnt_sess_hr <- dataset %>% select(clnt_id, sess_id, tot_sess_hr_v)
clnt_sess_hr <- unique(clnt_sess_hr)
clnt_sess_hr <- clnt_sess_hr %>% group_by(clnt_id) %>% summarise(sess_hr_mean = mean(tot_sess_hr_v))
dataset_agg <- left_join(dataset_agg, clnt_sess_hr, by="clnt_id")
dataset_agg$sess_hr_mean <- round(dataset_agg$sess_hr_mean, 3)
# NA를 0으로
dataset_agg$sess_hr_mean <- ifelse(is.na(dataset_agg$sess_hr_mean), 0, dataset_agg$sess_hr_mean)

##### trfc_src 펼치기(한 session 내에서 동일하므로, session 당 하나로 간주)
clnt_src <- dataset %>% select(clnt_id, sess_id, trfc_src)
clnt_src <- unique(clnt_src) #중복제거
clnt_src <- clnt_src %>% group_by(clnt_id, trfc_src) %>% mutate(item_sum=n())
clnt_src <- clnt_src %>% select(clnt_id, trfc_src, item_sum)
clnt_src <- unique(clnt_src)
clnt_src <- as.data.frame(spread(clnt_src, trfc_src ,item_sum,fill=0)) # 펼치기
# 비율로 변경
colnames(clnt_src) <- c("clnt_id", "DIRECT", "PORTAL_1", "PORTAL_2", "PORTAL_3", "PUSH", "unknown", "WEBSITE")
sum <- clnt_src$DIRECT + clnt_src$PORTAL_1 + clnt_src$PORTAL_2 + clnt_src$PORTAL_3 + clnt_src$PUSH + clnt_src$unknown + clnt_src$WEBSITE
clnt_src$DIRECT <- round(clnt_src$DIRECT / sum,3)
clnt_src$PORTAL_1 <- round(clnt_src$PORTAL_1 / sum,3)
clnt_src$PORTAL_2 <- round(clnt_src$PORTAL_2 / sum,3)
clnt_src$PORTAL_3 <- round(clnt_src$PORTAL_3 / sum,3)
clnt_src$PUSH <- round(clnt_src$PUSH / sum,3)
clnt_src$unknown <- round(clnt_src$unknown / sum,3)
clnt_src$WEBSITE <- round(clnt_src$WEBSITE / sum,3)
colnames(clnt_src) <- c("clnt_id", "src_DIRECT", "src_PORTAL_1", "src_PORTAL_2", "src_PORTAL_3", "src_PUSH", "src_unknown", "src_WEBSITE")
dataset_agg <- left_join(dataset_agg, clnt_src, by="clnt_id")

##### dvc_ctg_nm 펼치기(한 session 내에서 동일하므로, session 당 하나로 간주)
# 한사람당 한 dvc_ctg_nm만 가짐
clnt_dvc <- dataset %>% select(clnt_id, dvc_ctg_nm)
clnt_dvc <- unique(clnt_dvc)
dataset_agg <- left_join(dataset_agg, clnt_dvc, by="clnt_id")

##### clnt 별 구매횟수
clnt_purchase <- dataset %>% select(clnt_id, sess_id, trans_id)
clnt_purchase <- unique(clnt_purchase)
clnt_purchase$trans_id <- ifelse(is.na(clnt_purchase$trans_id),0,1)
clnt_purchase <- clnt_purchase %>% group_by(clnt_id) %>% summarise(purchase_count = sum(trans_id))
table(clnt_purchase$purchase_count)
dataset_agg <- left_join(dataset_agg, clnt_purchase, by="clnt_id")

##### 한 번 구매시 몇 개의 물건 구매하는지
# product code 없는 경우 trans_id 있어도 buy_ct가 -1 로 표시됨
# -1을 0으로 처리하고 buy_ct 합/구매횟수
clnt_purchase_num <- dataset %>% select(clnt_id, trans_id, buy_ct)
clnt_purchase_num <- unique(clnt_purchase_num)
clnt_purchase_num$buy_ct <- ifelse(!is.na(clnt_purchase_num$trans_id)&clnt_purchase_num$buy_ct==-1, 1, clnt_purchase_num$buy_ct)
clnt_purchase_num$buy_ct <- ifelse(clnt_purchase_num$buy_ct==-1, 0, clnt_purchase_num$buy_ct)
clnt_purchase_num <- clnt_purchase_num %>% group_by(clnt_id) %>% summarise(sum_buy_ct = sum(buy_ct))
clnt_purchase_num <- left_join(clnt_purchase_num, clnt_purchase, by="clnt_id")
clnt_purchase_num$mean_buy_ct <- ifelse(clnt_purchase_num$purchase_count!=0, round(clnt_purchase_num$sum_buy_ct / clnt_purchase_num$purchase_count, 3), 0)
clnt_purchase_num <- clnt_purchase_num %>% select(clnt_id, mean_buy_ct)
dataset_agg <- left_join(dataset_agg, clnt_purchase_num, by="clnt_id")

##### 요일 별 접속비율
clnt_week <- dataset %>% select(clnt_id, sess_id, weekday)
clnt_week <- unique(clnt_week) #중복제거
clnt_week <- clnt_week %>% group_by(clnt_id, weekday) %>% mutate(item_sum=n())
clnt_week <- clnt_week %>% select(clnt_id, weekday, item_sum)
clnt_week <- unique(clnt_week)
clnt_week <- as.data.frame(spread(clnt_week, weekday ,item_sum,fill=0)) # 펼치기
# 비율로 변경
colnames(clnt_week) <- c("clnt_id", "weekday_0", "weekday_1", "weekday_2", "weekday_3", "weekday_4", "weekday_5", "weekday_6")
sum <- clnt_week$weekday_0 + clnt_week$weekday_1 + clnt_week$weekday_2 + clnt_week$weekday_3 + clnt_week$weekday_4 + clnt_week$weekday_5 + clnt_week$weekday_6
clnt_week$weekday_0 <- round(clnt_week$weekday_0 / sum,3)
clnt_week$weekday_1 <- round(clnt_week$weekday_1 / sum,3)
clnt_week$weekday_2 <- round(clnt_week$weekday_2 / sum,3)
clnt_week$weekday_3 <- round(clnt_week$weekday_3 / sum,3)
clnt_week$weekday_4 <- round(clnt_week$weekday_4 / sum,3)
clnt_week$weekday_5 <- round(clnt_week$weekday_5 / sum,3)
clnt_week$weekday_6 <- round(clnt_week$weekday_6 / sum,3)
dataset_agg <- left_join(dataset_agg, clnt_week, by="clnt_id")

##### 성별, 나이 정보
clnt_info <- dataset %>% select(clnt_id, clnt_gender, clnt_age)
clnt_info <- unique(clnt_info)
dataset_agg <- left_join(dataset_agg, clnt_info, by="clnt_id")

#####
write.csv(dataset_agg, "dataset_agg_EB.csv", row.names = FALSE)
##### dvc 중복문제 -> 삭제하고 unique
dataset_agg <- dataset_agg %>% select(-dvc_ctg_nm)
dataset_agg <- unique(dataset_agg)
write.csv(dataset_agg, "dataset_agg_EB.csv", row.names = FALSE)

###################################
# 구매한 상품에 대한 dataset 생성(class label)
dataset_purchase <- dataset %>% select(clnt_id, clac_nm1, clac_nm2, clac_nm3) %>% filter(!is.na(clac_nm1))

###################################
setwd("D:/Project/2019 Lpoint/Code/Data/MERGE")
dataset_agg1 <- dataset_agg
dataset_agg2 <- read.csv("df_by_clnt_id.csv")

dataset_agg <- left_join(dataset_agg1, dataset_agg2, by="clnt_id")
write.csv(dataset_agg, "dataset_agg.csv")


################################### onehot encoding 된 것 가져와서 데이터셋 분리 및 정규화
#install.packages("DescTools")
library(DescTools)
mean_pag_view <- read.csv("mean_pag_view.csv")
dataset_agg <- read.csv("ohe_df.csv")
dataset_agg_ <- dataset_agg

##### 못다한 전처리
# pag_view_mean na 평균값으로 채우기
#dataset_agg$pag_view_mean <- ifelse(is.na(dataset_agg$pag_view_mean), mean(dataset_agg$pag_view_mean, na.rm=TRUE), dataset_agg$pag_view_mean)
# mean_pag_view에서 pag_view_mean과 sess_hr_mean 가져와서 대체
colnames(mean_pag_view) <- c("clnt_id", "pag_view_mean", "sess_hr_mean")
dataset_agg <- dataset_agg %>% select(-c("pag_view_mean", "sess_hr_mean"))
dataset_agg <- left_join(dataset_agg, mean_pag_view, by="clnt_id")
# mean_pass_time이 0인 것(한번 들어온사람)에 last access 값 넣어주기
dataset_agg$mean_pss_time <- ifelse(dataset_agg$mean_pss_time==0, dataset_agg$last_access, dataset_agg$mean_pss_time)

# 필요없는 column 제외 및 잠재고객과 아닌사람 분리하기
#colnames(dataset_agg)
dataset_agg <- dataset_agg %>% select(-c(purchase_count, mean_buy_ct, action_6, action_7))

dataset_audience <- dataset_agg %>% filter(is_latent==1)
dataset_normal <- dataset_agg %>% filter(is_latent==0)
dataset_audience <- dataset_audience %>% select(-is_latent)
dataset_normal <- dataset_normal %>% select(-is_latent)

# 잠재고객: 정규화 필요한 columns 분리
dataset_audience_scaler <- dataset_audience %>% select(clnt_id, time_to_last, mean_pss_time, last_access, tot_sess_cnt, return_counts, pag_view_mean, sess_hr_mean)
dataset_audience_no_scaler <- dataset_audience %>% select(-c(time_to_last, mean_pss_time, last_access, tot_sess_cnt, return_counts, pag_view_mean, sess_hr_mean))

# 정규화 전에 log scaling
for (i in (2:ncol(dataset_audience_scaler))){
  dataset_audience_scaler[,i] <- log(dataset_audience_scaler[,i] +1)
}
# 정규화
for (i in (2:ncol(dataset_audience_scaler))){
  dataset_audience_scaler[,i] <- scale(dataset_audience_scaler[,i], center = TRUE, scale = TRUE)
}
# column들 합치기
dataset_audience_new <- left_join(dataset_audience_scaler, dataset_audience_no_scaler, by="clnt_id")

# 잠재고객x: 정규화 필요한 columns 분리
dataset_normal_scaler <- dataset_normal %>% select(clnt_id, time_to_last, mean_pss_time, last_access, tot_sess_cnt, return_counts, pag_view_mean, sess_hr_mean)
dataset_normal_no_scaler <- dataset_normal %>% select(-c(time_to_last, mean_pss_time, last_access, tot_sess_cnt, return_counts, pag_view_mean, sess_hr_mean))
# 정규화 전에 log scaling
for (i in (2:ncol(dataset_normal_scaler))){
  dataset_normal_scaler[,i] <- log(dataset_normal_scaler[,i] +1)
}
# 정규화
for (i in (2:ncol(dataset_normal_scaler))){
  dataset_normal_scaler[,i] <- scale(dataset_normal_scaler[,i], center = TRUE, scale = TRUE)
}
# column들 합치기
dataset_normal_new <- left_join(dataset_normal_scaler, dataset_normal_no_scaler, by="clnt_id")

### 저장
write.csv(dataset_audience_new, "dataset_audience.csv", row.names=FALSE)
write.csv(dataset_normal_new, "dataset_normal.csv", row.names=FALSE)
