import subfunctions
import pandas as pd
import numpy as np
import os
import pickle

from sklearn.preprocessing import MinMaxScaler
import tensorflow as tf

os.chdir("../제6회 L.POINT Big Data Competition")


class Recommend:

    def __init__(self, naver_shopping_insight=None, trend_dict=None):
        self.item_list = []
        self.naver_shopping_insight = naver_shopping_insight
        self.trend_dict = trend_dict
        self.cat3_to_cat1 = subfunctions.mappers.get('cat3_to_cat1')
        self.cat1_to_naver = subfunctions.mappers.get('cat1_to_naver')

    def recommend(self, clnt_id, n_neighbor=30):
        clac_nm3 = subfunctions.mappers.get("clac_nm3")
        user_log_summary = get_user_info(clnt_id)
        # weight = np.zeros(len(clac_nm3))
        # weight = pd.Series([0] * len(clac_nm3))
        min_max_scaler = MinMaxScaler()
        # -------------------- 고객정보가 필요하지 않은 부분 ---------------
        # 1. 네이버 API를 통해 쇼핑인사이트의 각 카테고리별 상대적 검색비중을 확인
        # naver_search_cat = [x[0] for x in sorted(naver_shopping_insight.items(), key=(lambda x : x[1]), reverse=True)]
        # naver_search_val = [x[1] for x in sorted(naver_shopping_insight.items(), key=(lambda x : x[1]), reverse=True)]
        print("1) Naver Trend", end="")
        weight_naver = pd.Series(clac_nm3).map(self.cat3_to_cat1).map(self.cat1_to_naver).map(naver_shopping_insight)
        weight_naver_scaling = subfunctions.naver_scaler(weight_naver)
        print("\t...Success!")

        # 2. 대분류 lstm
        print("2) LSTM", end="")
        weight_lstm = pd.Series(clac_nm3).map(self.cat3_to_cat1).map(trend_dict)
        print("\t...Success!")

        # -------------------- 고객정보 필요한 부분 ------------------------
        # 3. knn - 비슷한 사람 - 소분류 가중치
        print("3) KNN", end="")
        user_info = user_log_summary[0][1:-1]
        user_group = user_log_summary[0][-1].astype(int)
        weight_knn = subfunctions.similar_user_weight(subfunctions.similar_user(user_info, user_group, n_neighbor))
        weight_knn_scaling = pd.DataFrame(min_max_scaler.fit_transform(pd.DataFrame(weight_knn)))[0]
        print("\t...Success!")

        # 4. dnn - 행동패턴을 통해 구매할 소분류 예측
        print("4) DNN", end="")
        dnn = tf.keras.models.load_model("dnn_model.h5")
        weight_dnn = pd.Series(dnn.predict(np.expand_dims(list(user_info), axis=0))[0])
        weight_dnn_scaling = pd.DataFrame(min_max_scaler.fit_transform(pd.DataFrame(weight_dnn)))[0]
        print("\t...Success!")

        # 최종 가중치 계산 및 추천상품 선정
        final_weight = weight_lstm * (weight_naver_scaling + weight_knn_scaling + weight_dnn_scaling)
        final_result_df = pd.DataFrame({"product": clac_nm3, "weight": final_weight}).sort_values(by="weight",
                                                                                                  ascending=False)
        final_result = final_result_df.iloc[0:9, 0].reset_index(drop=True)
        print(final_result)
        # 5. Association Rule Discovery - 추천된 상품과 함께사면 좋을 음식 추천
        # 음식 아닌 상품 + 음식 + 함께사면 좋을 음식
        # not_food = final_result[final_result.map(cat3_to_cat1).map(cat1_to_naver)!="식품"]
        asso_list = subfunctions.find_asso(final_result)["product"]
        for prod in asso_list:
            if prod not in list(final_result):
                final_result[9] = prod
                break
        if len(final_result) == 9:
            final_result[9] = final_result_df.iloc[10, 0]
        return final_result


def get_user_info(clnt_id):
    try:
        path = "./grouped_info_by_clnt_id/"
        file = "grouped_clnt_info_{}.pickle".format(clnt_id)
        with open(os.path.join(path, file), "rb") as f:
            user_log_summary = pickle.load(f)

        return user_log_summary
    except FileNotFoundError:
        return 0


if __name__ == "__main__":
    # subfunctions.getTrend() # naver_shopping_insight pickle 파일 갱신
    # subfunctions.predict_trend_by_lstm() # trend_dict 갱신
    naver_shopping_insight, trend_dict = subfunctions.get_pickle_files()
    rc = Recommend(naver_shopping_insight, trend_dict)
    clnt_id = 2
    rc.recommend(clnt_id)
