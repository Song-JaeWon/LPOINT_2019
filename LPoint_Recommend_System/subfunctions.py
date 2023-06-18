import os
import json
import pickle
import urllib.request
import itertools
import time

import numpy as np
import pandas as pd

import tensorflow as tf
# from tensorflow.keras.models import model
# from keras.models import load_model

os.chdir("../제6회 L.POINT Big Data Competition")

audience = pd.read_csv("audience_with_group.csv")
base = pd.read_csv("normal_with_group.csv")
naver_cat_matching = pd.read_csv("product_table.csv", encoding="euc-kr")
dataset_purchase = pd.read_csv("buying_list_clac3.csv")


asso_df = pd.read_csv("association_rule_df.csv")
# asso_df.drop(asso_df.columns[1], axis=1, inplace=True)


def get_pickle_files():
    with open("naver_shopping_insight.pickle", "rb") as f:
        naver_shopping_insight = pickle.load(f)
    with open("lstm_trend_dict.pickle", "rb") as f:
        trend_dict = pickle.load(f)

    return naver_shopping_insight, trend_dict


def getTrend(date="2019-10-01", client_id="", client_secret=""):
    client_id = client_id
    client_secret = client_secret

    body1 = '{"startDate":"' + str(date) + '","endDate":"' + str(
        date) + '","timeUnit":"date","category":[{"name":"패션의류","param":["50000000"]},{"name":"패션잡화","param":["50000001"]},{"name":"화장품/미용","param":["50000002"]}]}'
    body2 = '{"startDate":"' + str(date) + '","endDate":"' + str(
        date) + '","timeUnit":"date","category":[{"name":"디지털/가전","param":["50000003"]},{"name":"가구/인테리어","param":["50000004"]},{"name":"출산/육아","param":["50000005"]}]}'
    body3 = '{"startDate":"' + str(date) + '","endDate":"' + str(
        date) + '","timeUnit":"date","category":[{"name":"식품","param":["50000006"]},{"name":"스포츠/레저","param":["50000007"]},{"name":"생활/건강","param":["50000008"]}]}'
    body4 = '{"startDate":"' + str(date) + '","endDate":"' + str(
        date) + '","timeUnit":"date","category":[{"name":"패션의류","param":["50000000"]},{"name":"디지털/가전","param":["50000003"]},{"name":"생활/건강","param":["50000008"]}]}'

    url = "https://openapi.naver.com/v1/datalab/shopping/categories"
    request = urllib.request.Request(url)
    request.add_header("X-Naver-Client-Id", client_id)
    request.add_header("X-Naver-Client-Secret", client_secret)
    request.add_header("Content-Type", "application/json")

    response1 = urllib.request.urlopen(request, data=body1.encode("utf-8"))
    response2 = urllib.request.urlopen(request, data=body2.encode("utf-8"))
    response3 = urllib.request.urlopen(request, data=body3.encode("utf-8"))
    response4 = urllib.request.urlopen(request, data=body4.encode("utf-8"))
    response_list = [response1, response2, response3]

    # 각 body 별 비율 계산
    body_rate = []
    rescode = response4.getcode()
    response_body = json.loads(response4.read().decode('utf-8'))
    for i in range(0, 3):
        body_rate.append(response_body["results"][i]["data"][0]["ratio"])
    body_rate = [i / max(body_rate) for i in body_rate]

    search_rate_dict = {}
    num = 0
    for res in response_list:
        rescode = res.getcode()
        if (rescode == 200):
            response_body = res.read().decode('utf-8')
            response_body = json.loads(response_body)
            rate = body_rate[num]
            for i in range(0, 3):
                search_rate = response_body["results"][i]["data"][0]["ratio"] * rate
                search_rate_dict[response_body["results"][i]["title"]] = search_rate
        else:
            print("Error Code:" + rescode)
        num += 1
    search_rate_dict["기타"] = 0

    with open("naver_shopping_insight.pickle", "wb") as f:
        pickle.dump(search_rate_dict, f, protocol=pickle.HIGHEST_PROTOCOL)

    return search_rate_dict


def naver_scaler(x):
    return 1/250*x + 1/5


def get_recent_sales():
    with open("sales_by_day.pickle", "rb") as f:
        sales = pickle.load(f)
    recent_sales = pd.DataFrame(sales).iloc[:, -7:]
    return recent_sales


def get_recent_time_info(start_time='20190924', now_time='20191001'):
    with open("holiday_list.pickle", "rb") as f:
        holiday_list = pickle.load(f)
    time_info = pd.DataFrame({"time": pd.date_range(start=start_time, end=now_time)})

    time_info["is_holiday"] = np.where(time_info.isin(pd.to_datetime(holiday_list)), 1, 0)
    time_info["is_weekend"] = np.where(time_info.time.dt.weekday.isin(range(0, 5)), 0, 1)

    time_info["holiday_tomorrow"] = time_info.is_holiday.shift(-1, fill_value=0)
    time_info["weekend_tomorrow"] = time_info.is_weekend.shift(-1, fill_value=0)

    time_info.drop(["is_holiday", "is_weekend"], axis=1, inplace=True)

    return time_info


def predict_trend_by_lstm():
    import gc
    with open("clac_nm_list.pickle", 'rb') as f:
        clac_nm_list = pickle.load(f)

    recent_sales = get_recent_sales()
    time_info = get_recent_time_info()
    yesterday_sales = get_recent_sales().iloc[:, -1].values
    expected_sales_list = []

    for i in range(recent_sales.shape[0]):
        # print("Expecting Today's {} Sales ".format(clac_nm_list[i]))
        sales = pd.concat([recent_sales.iloc[i:i + 1].T.reset_index(drop=True), time_info.iloc[:-1, 1:]],
                          axis=1).values.reshape(1, 7, 3)
        modelname = os.curdir + "/models/lstm_model_{}.h5".format(clac_nm_list[i].replace("/", "_"))
        model = tf.keras.load_model(modelname)
        expected_sales = np.round(model.predict(sales)[0][0])
        expected_sales_list.append(expected_sales)

    sales_diff = yesterday_sales - np.array(expected_sales_list)

    del [recent_sales, time_info]
    gc.collect()

    trend_dict = pd.DataFrame(
        {"clac_nm1": clac_nm_list,
         "sales": np.where(sales_diff > 0, 1.1, np.where(sales_diff == 0, 1, 0.9))}).set_index(
        "clac_nm1").to_dict().get("sales")

    with open("lstm_trend_dict.pickle", "wb") as f:
        pickle.dump(trend_dict, f, protocol=pickle.HIGHEST_PROTOCOL)

    return sales_diff, expected_sales_list

def knn_weight_by_group():
    col = list(audience.columns[1:-1])
    g1_weight = g2_weight = g3_weight = g4_weight = g5_weight = g6_weight = pd.DataFrame({"col" : col, "weight":1})
    """
    for i in range(1, 7):
        globals()["g{}_weight".format(i)] = pd.DataFrame({"col": col, "weight": 1})
    """
    g1_weight.iloc[[col.index("time_to_last"), col.index("last_access")], 1] = 0.8
    g2_weight.iloc[[col.index("return_rate"), col.index("src_PUSH")], 1] = 0.8
    g3_weight.iloc[[col.index("action_0"), col.index("action_2"), col.index("return_rate")], 1] = 0.8
    g4_weight.iloc[[col.index("tot_sess_cnt"), col.index("pag_view_mean"), col.index("src_WEBSITE")], 1] = 0.8
    g5_weight.iloc[[col.index("tot_sess_cnt"), col.index("return_rate"), col.index("action_0")], 1] = 0.8
    g6_weight.iloc[[col.index("action_2"), col.index("tot_sess_cnt")], 1] = 0.8

    weight_list_by_group = {1: list(g1_weight.weight),
                            2: list(g2_weight.weight),
                            3: list(g3_weight.weight),
                            4: list(g4_weight.weight),
                            5: list(g5_weight.weight),
                            6: list(g6_weight.weight)}

    return weight_list_by_group


def similar_user(user_info, user_group, n_neighbor):
    # user_info, user_group => 함수내에서 자동으로 input

    weight_list = knn_weight_by_group().get(user_group)
    calc_dist = base.iloc[:, 1:-1].apply(lambda x: np.linalg.norm(np.multiply(weight_list, (x - user_info))),
                                         axis=1).reset_index(name="dist")
    top_n_clnt = calc_dist.sort_values(by="dist").iloc[:n_neighbor, 0]
    top_n_similarity = calc_dist.sort_values(by="dist").iloc[:n_neighbor, 1]
    top_n = pd.DataFrame({"clnt_id": base.iloc[top_n_clnt, 0], "dist": top_n_similarity}).reset_index(drop=True)
    return top_n


def multiplier(weight, array):
    return weight*array


def similar_user_weight(top_n):
    # dataset_purchase에 붙여서 가중치 계산
    dataset_purchase_top = dataset_purchase[dataset_purchase["clnt_id"].isin(list(top_n.clnt_id))].reset_index(drop=True)
    top_n["weight"] = 1 / (top_n.dist+(max(list(top_n.dist))-min(list(top_n.dist)))*2)
    top_n = top_n.drop("dist", axis=1)
    top_n_df = pd.merge(top_n, dataset_purchase_top, on="clnt_id", how="left")
    top_n_df = top_n_df.apply(lambda x : multiplier(x['weight'], x[2:]),axis=1)
    weight_knn = top_n_df.iloc[:, :-1].sum(axis=0)
    return weight_knn


def find_asso(recommend):
    if (type(recommend)!=list):
        recommend = recommend.to_list()
    asso_list = []
    asso_conf = []
    for i in range(len(recommend)):
        for j in range(len(asso_df[asso_df.lhs_length == 1])):
            if (recommend[i] == asso_df.lhs[j][0]):
                asso_list.append(asso_df.rhs[j][0])
                asso_conf.append(asso_df.confidence[j])
    recommend_permu = list(map(','.join, itertools.permutations(recommend, 2)))
    for i in range(len(recommend_permu)):
        for j in range(len(asso_df[asso_df.lhs_length>1])):
            if recommend_permu[i].split(',')[0] in asso_df.lhs[j] and recommend_permu[i].split(',')[1] in asso_df.lhs[j]:
                asso_list.append(asso_df.rhs[j][0])
                asso_conf.append(asso_df.confidence[j])
    asso_result = pd.DataFrame({"product": asso_list, "confidence": asso_conf}).sort_values(by="confidence", ascending=False)
    asso_result = asso_result.drop_duplicates(['product'], keep='first').reset_index(drop=True)
    return asso_result


with open("mappers.pickle", "rb") as f:
    mappers = pickle.load(f)

# clac_nm3 = sorted(online_df.clac_nm3.dropna().unique().tolist())