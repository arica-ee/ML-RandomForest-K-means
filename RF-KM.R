### Random Forest ###
library(datasets)
library(MASS)
library(tidyverse)

# 載入資料、切訓練集測試集
setwd("xxxxx")
data <- read.csv("airline_survey.csv")
data <- na.omit(data)
new_data <- data %>% 
  mutate(satisfaction = as.factor(satisfaction))
set.seed(1)

i <- sample(nrow(data), size = 1000, replace = FALSE)
train <- sample(i, size = 750, replace = FALSE)
test <- sample(i, size = 250, replace = FALSE)

trained <- new_data[train,]
tested <- new_data[test, ]

# 隨機森林模型
# install.packages("randomForest")
library(randomForest)
rf <- randomForest(satisfaction ~ ., data = trained, importance=TRUE) 
rf 

# 畫出模型各分類的錯誤率（OOB:未訓練數據的錯誤率）-> satisfied類別的錯誤率較高
plot(rf)
legend("topright", colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4)

# 各變數對模型的影響
### MeanDecreaseAccuracy：（移除後）影響模型準確率
### MeanDecreaseGini：（移除後）影響樹節點分裂
importance(rf) 
varImpPlot(rf)

# 預測
pred=predict(rf, newdata = tested)
table(Real = tested$satisfaction, Predict = pred)
accuracy <- (290+195)/(290+195+5+10)
accuracy

### Kmeans ###
setwd("xxxxx")
data <- read.csv("airline_survey.csv")
library(factoextra)

# 調整資料格式（數值型、統一量級）
new_data <- data %>% 
  mutate(
    gender = ifelse(Gender=="Male",1,0),
    travel_type = ifelse(Type.of.Travel=="Business travel",1,0),
    satisfaction = ifelse(satisfaction=="satisfied",1,0),
    flight_distance = (Flight.Distance-min(Flight.Distance))/ (max(Flight.Distance)-min(Flight.Distance)),
    clean = (Cleanliness-min(Cleanliness))/(max(Cleanliness)-min(Cleanliness))
  )
head(new_data)
new_data <- new_data[,-c(1:24)]
i <- sample(nrow(new_data), size = 1000, replace = FALSE)
sample_data <- new_data[i, ]
head(sample_data)

ggplot(sample_data, aes(x=flight_distance, y=clean)) +
  geom_point()

# 使用手肘法選擇模型群數
fviz_nbclust(sample_data[,c(4,5)],
             FUNcluster = kmeans,
             method = "wss", # 用組內平方和作為指標
             k.max = 20)+ # 最多分幾組
  labs(title = "Elbow Method of Kmeans")+
  geom_vline(xintercept = 3, linetype = 2) # 標出拐點

# K-means模型（使用清潔滿意度跟飛行距離作為分群依據）
k = kmeans(sample_data[,c(4,5)], centers = 3, nstart = 20)
k

cc = k$cluster
fviz_cluster(k, # 分群結果
             data = sample_data[,c(4,5)], 
             geom = c("point"), 
             ellipse.type = "norm") 
