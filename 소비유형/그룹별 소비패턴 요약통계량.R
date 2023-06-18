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
dataset <- read.csv("jangbaguni_using_dataset.csv")
grouping <- read.csv("normal_with_group.csv")
grouping <- group %>% select(clnt_id, group)
dataset <- left_join(dataset, grouping, by = "clnt_id")
dataset_group <- dataset %>% group_by(group) %>% summarise_all(list(mean = mean))
dataset_group <- dataset_group %>% select(-clnt_id_mean)

write.csv(dataset_group, "normal_jangbaguni_group_info.csv", row.names = FALSE)
