# libraryの読み込み
library(readr)
library(changepoint)
library(forecast)


# 1. 前処理
# 1.1 年号データの読み込み
dat_nengo <- read_csv("nengo.csv")
dat_nengo

# 1.2 時系列型に変換して可視化、定常系列を探す。
n_years <- as.ts(dat_nengo$years)
plot(n_years)    # 時系列のプロット
plot(diff(n_years))    # 差分系列のプロット

# 1.3 差分系列のオブジェクトを作る。
diff_nyears <- diff(n_years)


# 2. 対象となる時系列の抽出
# 分散が変化すると仮定して、変化点を検出する。
res_cpts <- cpt.var(diff_nyears, method = "PELT")
param.est(res_cpts)    # 推定されたパラメータの確認
plot(res_cpts)    # 変化点の可視化
cpts(res_cpts)    # 変化点


# 3. train_test_splitとその可視化
# 3.1 train_test_split
n_years_term <- as.ts(dat_nengo[30:172, ]$years)
train <- window(n_years_term, 1, 134)
test <- window(n_years_term, 135, 143)

# 3.2 原系列のプロットと差分系列のプロット
plot(train)    # 原系列のプロット
plot(diff(train))    # 差分系列のプロット

# 3.3 延長から明徳までの差分系列のオブジェクト
train_diff <- diff(train)
acf(train_diff)    # コレログラム


# 4. auto.arima
# 4.1 ARIMAモデルを探索する。
res <- auto.arima(train, ic = "aic",
                  stepwise = FALSE, approximation = FALSE,
                  max.p = 10, max.q = 10, max.order = 20, 
                  parallel = TRUE, num.cores = 4)
res

# 4.2 残差分析
tsdiag(res)


# 5. モデル評価
# 5.1 ARIMA(0, 1, 3)モデルを推定して、h時点後までを予測する関数を作る。
pred_func <- function(x, h){
  res <- Arima(x, order = c(0,1,3))
  return(forecast(res, h = h))
}

# 5.2 time series cross validation
error_all <- tsCV(n_years_term, pred_func, h = 1)
error_test <- window(error_all, 135, 143)
var(error_test)    # 残差の分散
c(mean(abs(error_test)), mean(abs(error_test/test)))    # MAE, MAPE


