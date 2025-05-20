data= read.csv("~/MLR_Project/TourismData.csv", sep = ";")
head(data)
summary(data) #checked the NA rows etc.


# 2020_2 verisini ayırma (Leave-One-Out)
data_test = subset(data, TimeIndex == "2020_2")
data_train = subset(data, !(TimeIndex == "2020_2"))


##### TOURISM INCOME PREDICTION

install.packages("caret")
install.packages("xgboost")

##### DATA_TRAIN - DATA_TEST: NUMERIC VERILER HEPSİ
#### DATA_TRAIN2 - DATA_TEST2: YEAR VE QUARTER FACTOR KATEGORIK, TEST2 INCOME LINEAR, CATEGORY RATIO
### DATA_TEST3: KATEGORILER VE INCOME MODELSİZ SADECE TRAINDEN TURIST/ X ORANIYLA BULUNDU. 

### year ve quarter factor olan diğer verisetlerini oluştur
data_train2 = data_train
data_test2 = data_test
data_train2$Quarter <- as.factor(data_train$Quarter)
data_test2$Quarter <- as.factor(data_test$Quarter)

data_train2$Year <- as.factor(data_train$Year)
data_test2$Year <- as.factor(data_test$Year)

str(data_train2)
str(data_test)

# Faktör seviyelerini eşitleme
data_train2$Year <- factor(data_train2$Year, levels = unique(data_train2$Year))
data_train2$Quarter <- factor(data_train2$Quarter, levels = unique(data_train2$Quarter))

data_test2$Year <- factor(data_test2$Year, levels = levels(data_train2$Year))
data_test2$Quarter <- factor(data_test2$Quarter, levels = levels(data_train2$Quarter))


### MULTIPLE LINEAR numeric
lm_model <- lm(Total_Income ~ TouristNumber + Year + Quarter, data = data_train)
lm_pred <- predict(lm_model, newdata = data_test)

# Multiple Linear Regression (Kategorik)
lm_factor_model <- lm(Total_Income ~ TouristNumber + Year + Quarter, data = data_train2)
lm_factor_pred <- predict(lm_factor_model, newdata = data_test2)

# Random Forest
library(randomForest)
rf_model <- randomForest(Total_Income ~ TouristNumber + Year + Quarter, data = data_train2, ntree = 500)
rf_pred <- predict(rf_model, newdata = data_test2)

# Tahmin Sonuçları
cat("Multiple Linear Model (Kategorik) Tahmini:", lm_factor_pred, "\n" , 
    "Random Forest Tahmini:", rf_pred, "\n",
    "Multiple Linear Model (numeric) Tahmini:", lm_pred, "\n")  ## random forest best pred.


# Performance Metric Calculation Function
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))}

lm_mae <- mae(data_train$Total_Income, lm_pred)

rf_mae <- mae(data_train2$Total_Income, rf_pred)

lm_fac_mae <- mae(data_train2$Total_Income,lm_factor_pred)

# Sonuçları Karşılaştırma Tablosu
results <- data.frame(
  Model = c("Linear Regression", "Random Forest", "Lin. Reg. Factor"),
  MAE = c(lm_mae, rf_mae, lm_fac_mae))
print(results)

# Multiple lin reg kategorik ile en iyi modelden Total_Income tahminini ekleme
data_test$Total_Income <- lm_factor_pred 

# Total Income değerini Factor olan verisetine de ekleyelim
data_test2$Total_Income <- lm_factor_pred


### 🔹 Veriyi Hazırlama
input_features <- c("TouristNumber", "Total_Income")
output_features <- c("Ind_Expenditure", "Food_Beverage", "Accommodation", 
                     "Health", "Clothes", "Carpet_Rug", "GSM_Expenditure", "Souvenirs")

# 📌 Diğer Kategorilerin Income üzerinden Oransal Tahmini ------ kullanılmayan kısım olailir
category_ratios <- colMeans(data_train2[, output_features] / data_train2$Total_Income, na.rm = TRUE)
# 📌 Oranlara Göre Tahmin
for (feature in output_features) {
    data_test2[[feature]] <- data_test2$Total_Income * category_ratios[feature]
  }


#### MODELLER TAHMİNİ YERİNE TURİZM INCOME'I TURİST SAYISINA ORANININ AVG DEĞERİ İLE 240K BULDUM.
data_test3= data_test2

category_ratios <-data_train2$Total_Income / data_train2$TouristNumber

data_test3$Total_Income <- data_test3$TouristNumber * mean(category_ratios)
View(data_test3)


### VE BU RATIO ORANIYLA BULUNAN TOURISM INCOME ILE DİĞERLERİ İÇİN CATEGORY RATIO YAPIYORUZ
category_ratios <- sapply(output_features, function(feature) {
  mean(data_train2[[feature]] / data_train2$Total_Income, na.rm = TRUE)
})

for (feature in output_features) {
  data_test3[[feature]] <- data_test3$Total_Income * category_ratios[feature]
}

View(data_test3)

plot(Total_Income ~ TouristNumber, data = data_test3)



#### DATA_TOURISM VERİSETİYLE TÜM DATAYI BİRLEŞTİRDİM 2020_2 KISMINI EKLEDİM

targets2 <- c("Total_Income","Food_Beverage", "Accommodation", 
             "Health", "Clothes", "Carpet_Rug", 
             "GSM_Expenditure", "Souvenirs", "Ind_Expenditure")

data_tourism = data
data_tourism[which(data_tourism$TimeIndex == "2020_2"), targets2] <- data_test3[which(data_test3$TimeIndex == "2020_2"), targets2]

View(data_tourism)


#### HER SATIR İÇİN INCOME'A ORANLARINA GÖRE KATEGORİLERİ KENDİ ARASINDA HIGH-LOW LABELLADIK

targets <- c("Food_Beverage", "Accommodation", 
             "Health", "Clothes", "Carpet_Rug", 
             "GSM_Expenditure", "Souvenirs", "Ind_Expenditure")

data_tourism_hl <- data_tourism

# Kategori oranlarına göre High/Low belirleme (Her satır için)
for (col in targets) {
  # Her satırda kategoriyi Total_Income'a bölerek oran hesapla
  ratio_values <- data_tourism[[col]] / data_tourism$Total_Income
  
  # Oranların genel ortalama ve standart sapmasını hesapla
  mean_ratio <- mean(ratio_values, na.rm = TRUE)
  sd_ratio <- sd(ratio_values, na.rm = TRUE)
  
  # Z-score hesapla ve High/Low etiketle
  z_scores <- (ratio_values - mean_ratio) / sd_ratio
  data_tourism_hl[[paste0(col, "_Level")]] <- ifelse(z_scores > 0, "High", "Low") }

head(data_tourism_hl[, grepl("_high$", names(data_tourism_hl))])
View(data_tourism_hl)


#### DATA_TOURISM_HL VERİSETİYLE PREDICTIONLAR 

# Gerekli kütüphaneler
library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(pROC)


set.seed(1071)

# ✔️ 1. Veriyi Train ve Test olarak ayırma (Stratified Sampling)
index <- createDataPartition(data_tourism_hl$Food_Beverage_Level, p = 0.7, list = FALSE)
train_data <- data_tourism_hl[index, ]
test_data <- data_tourism_hl[-index, ]


# 🔧 Performans Değerlendirme Fonksiyonu
evaluate_model <- function(model, test_data, target) {
  if (inherits(model, "train")) {
    predictions <- predict(model, test_data, type = "raw")
  } else if (inherits(model, "glm")) {
    predictions <- ifelse(predict(model, test_data, type = "response") > 0.5, "High", "Low")
  } else if (inherits(model, "randomForest")) {
    predictions <- predict(model, test_data)
  } else if (inherits(model, "xgb.Booster")) {
    predictions <- ifelse(predict(model, as.matrix(test_data[, c("TouristNumber", "Total_Income", "Year", "Quarter")])) > 0.5, "High", "Low")
  } else {
    predictions <- predict(model, test_data)
  }
  
  actuals <- test_data[[target]]
  conf_matrix <- confusionMatrix(factor(predictions, levels = c("High", "Low")), 
                                 factor(actuals, levels = c("High", "Low")))
  
  accuracy <- conf_matrix$overall["Accuracy"]
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  return(c(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score))
}

# 🔧 Tüm Kategoriler ve Modeller için Otomatik Eğitim ve Değerlendirme
targets <- c("Food_Beverage", "Accommodation", "Health", "Clothes", 
             "Carpet_Rug", "GSM_Expenditure", "Souvenirs", "Ind_Expenditure")

results <- data.frame(Category = character(), Model = character(),
                      Accuracy = numeric(), Precision = numeric(), 
                      Recall = numeric(), F1_Score = numeric())

for (col in targets) {
  target_col <- paste0(col, "_Level")
  print(paste("Training for:", target_col))
  
  # ✅ Logistic Regression (Factor to Numeric)
  train_data[[target_col]] <- factor(train_data[[target_col]], levels = c("Low", "High"))
  test_data[[target_col]] <- factor(test_data[[target_col]], levels = c("Low", "High"))
  
  log_model <- glm(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")), 
                   data = train_data, family = "binomial")
  log_res <- evaluate_model(log_model, test_data, target_col)
  results <- rbind(results, data.frame(Category = col, Model = "Logistic Regression", t(log_res)))

  #  Decision Tree
  tree_model <- train(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")), 
                      data = train_data, method = "rpart")
  tree_res <- evaluate_model(tree_model, test_data, target_col)
  results <- rbind(results, data.frame(Category = col, Model = "Decision Tree", t(tree_res)))
  
  #  Random Forest
  rf_model <- randomForest(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")), 
                           data = train_data, ntree = 500)
  rf_res <- evaluate_model(rf_model, test_data, target_col)
  results <- rbind(results, data.frame(Category = col, Model = "Random Forest", t(rf_res)))
  
  #  XGBoost
  dtrain <- xgb.DMatrix(as.matrix(train_data[, c("TouristNumber", "Total_Income", "Year", "Quarter")]), 
                        label = as.numeric(train_data[[target_col]] == "High"))
  dtest <- xgb.DMatrix(as.matrix(test_data[, c("TouristNumber", "Total_Income", "Year", "Quarter")]))
  xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "binary:logistic", verbose = 0)
  xgb_pred <- ifelse(predict(xgb_model, dtest) > 0.5, "High", "Low")
  xgb_res <- evaluate_model(xgb_model, test_data, target_col)
  results <- rbind(results, data.frame(Category = col, Model = "XGBoost", t(xgb_res)))
  
  #  Naive Bayes
  nb_model <- naiveBayes(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")), 
                         data = train_data)
  nb_res <- evaluate_model(nb_model, test_data, target_col)
  results <- rbind(results, data.frame(Category = col, Model = "Naive Bayes", t(nb_res)))
  
  #  KNN
  knn_model <- train(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")), 
                     data = train_data, method = "knn", tuneLength = 5)
  knn_res <- evaluate_model(knn_model, test_data, target_col)
  results <- rbind(results, data.frame(Category = col, Model = "KNN", t(knn_res)))
}

View(results)

#### TEST VERİSİNDE TAHMİNLER

# Test Verisinde Tahmin Yapma
log_pred_prob <- predict(log_model, test_data, type = "response")  # Probability values
log_pred <- ifelse(log_pred_prob > 0.5, "High", "Low")  # High/Low tahmini

# Gerçek Değerler ile Tahmin Sonuçlarını Karşılaştırma
actuals <- test_data$Food_Beverage_Level

# Sonuçları Görselleştirme (Confusion Matrix)
conf_matrix <- confusionMatrix(factor(log_pred, levels = c("Low", "High")), 
                               factor(actuals, levels = c("Low", "High")))

# Accuracy, Precision, Recall, F1-Score'ları Görüntüleme
print(conf_matrix)

#  Logistic Regression Modelini Eğitme
log_model <- glm(Food_Beverage_Level ~ TouristNumber + Total_Income + Year + Quarter, 
                 data = train_data, family = "binomial")

#  Gerçek Değerler
actuals <- test_data$Food_Beverage_Level

#  Tahmin Sonuçlarını Test Verisine Ekleyelim
test_data$Food_Beverage_Pred <- log_pred

#  Confusion Matrix Hesaplama
conf_matrix <- confusionMatrix(factor(log_pred, levels = c("Low", "High")), 
                               factor(test_data$Food_Beverage_Level, levels = c("Low", "High")))

#  TP, TN, FP, FN Hesaplama ve DataFrame Olarak Gösterme
confusion_df <- data.frame(
  Actual_Low = c(conf_matrix$table[1, 1], conf_matrix$table[1, 2]),
  Actual_High = c(conf_matrix$table[2, 1], conf_matrix$table[2, 2])
)
rownames(confusion_df) <- c("Predicted_Low", "Predicted_High")

# ✔️ Performans Metriklerini DataFrame Olarak Gösterme
performance_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1-Score"),
  Value = c(
    round(conf_matrix$overall['Accuracy'], 4),
    round(conf_matrix$byClass['Pos Pred Value'], 4),
    round(conf_matrix$byClass['Sensitivity'], 4),
    round(2 * ((conf_matrix$byClass['Pos Pred Value'] * conf_matrix$byClass['Sensitivity']) /
                 (conf_matrix$byClass['Pos Pred Value'] + conf_matrix$byClass['Sensitivity'])), 4)
  )
)




#  Her Kategori için En Yüksek Doğruluğa Sahip Modeli Seçme
best_models <- results %>%
  group_by(Category) %>%
  slice_max(Accuracy.Accuracy, n = 1, with_ties = FALSE) %>%
  ungroup()

View(best_models)




library(caret)  # Confusion matrix için gerekli
library(dplyr)  # Verileri gruplamak için

#  Performans Sonuçları Tablosu
all_predictions <- data.frame(Category = character(),
                              Actual = character(),
                              Predicted = character())

confusion_matrices <- list()  # Her kategori için confusion matrixleri kaydet

for (i in 1:nrow(best_models)) {
  category <- best_models$Category[i]
  best_model <- best_models$Model[i]
  
  target_col <- paste0(category, "_Level")
  
  print(paste("Testing with Best Model for:", category, "-", best_model))
  
  # Modeli seçme ve tahmin yapma
  if (best_model == "Logistic Regression") {
    model <- glm(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")),
                 data = train_data, family = "binomial")
    pred_probs <- predict(model, test_data, type = "response")
    predictions <- ifelse(pred_probs > 0.5, "High", "Low")
    
  } else if (best_model == "Decision Tree") {
    model <- train(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")),
                   data = train_data, method = "rpart")
    predictions <- predict(model, test_data)
    
  } else if (best_model == "Random Forest") {
    model <- randomForest(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")),
                          data = train_data, ntree = 500)
    predictions <- predict(model, test_data)
    
  } else if (best_model == "XGBoost") {
    dtrain <- xgb.DMatrix(as.matrix(train_data[, c("TouristNumber", "Total_Income", "Year", "Quarter")]),
                          label = as.numeric(train_data[[target_col]] == "High"))
    dtest <- xgb.DMatrix(as.matrix(test_data[, c("TouristNumber", "Total_Income", "Year", "Quarter")]))
    model <- xgboost(data = dtrain, nrounds = 100, objective = "binary:logistic", verbose = 0)
    predictions <- ifelse(predict(model, dtest) > 0.5, "High", "Low")
    
  } else if (best_model == "Naive Bayes") {
    model <- naiveBayes(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")),
                        data = train_data)
    predictions <- predict(model, test_data)
    
  } else if (best_model == "KNN") {
    model <- train(as.formula(paste(target_col, "~ TouristNumber + Total_Income + Year + Quarter")),
                   data = train_data, method = "knn", tuneLength = 5)
    predictions <- predict(model, test_data)
  }
  
  # Test verisine tahminleri ekleme
  test_data[[paste0(category, "_Pred")]] <- predictions
  
  # Confusion Matrix Hesaplama
  actuals <- test_data[[target_col]]
  conf_matrix <- confusionMatrix(factor(predictions, levels = c("Low", "High")), 
                                 factor(actuals, levels = c("Low", "High")))
  confusion_matrices[[category]] <- conf_matrix
  
  # Sonuçları kaydetme
  all_predictions <- rbind(all_predictions, 
                           data.frame(Category = category,
                                      Actual = actuals,
                                      Predicted = predictions))
}
View(test_data) #### tüm veriler leveller ve predler dahil olan dataset

View(all_predictions)

# Confusion Matrices'i Görüntüleme
confusion_matrices  # Her kategori için Confusion Matrix



###### VISUALIZATIOINS

# Turist Sayısı ve Toplam Gelir İlişkisi
library(ggplot2)
ggplot(data_tourism, aes(x = TouristNumber, y = Total_Income)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Turist Sayısı ve Toplam Gelir İlişkisi", x = "Turist Sayısı", y = "Toplam Gelir") +
  theme_minimal()



# Turist Sayısı ve Gıda-İçecek Harcaması İlişkisi
ggplot(data_tourism, aes(x = TouristNumber, y = Food_Beverage)) +
  geom_point(aes(color = Year)) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Turist Sayısı ve Gıda-İçecek Harcaması İlişkisi", x = "Turist Sayısı", y = "Gıda-İçecek Harcaması") +
  theme_minimal()

# Turist Sayısı ve Konaklama Harcaması İlişkisi
ggplot(data_tourism, aes(x = TouristNumber, y = Accommodation)) +
  geom_point(aes(color = Year)) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Turist Sayısı ve Konaklama Harcaması İlişkisi", x = "Turist Sayısı", y = "Konaklama Harcaması") +
  theme_minimal()


# Turist Sayısı ve Ind_Expenditure İlişkisi
ggplot(data_tourism, aes(x = TouristNumber, y = Ind_Expenditure)) +
  geom_point(aes(color = Year)) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Turist Sayısı ve Gıda-İçecek Harcaması İlişkisi", x = "Turist Sayısı", y = "Ind_ExpenditureHarcaması") +
  theme_minimal()


# Turist Sayısı ve Health Harcaması İlişkisi
ggplot(data_tourism, aes(x = TouristNumber, y =Health)) +
  geom_point(aes(color = Year)) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Turist Sayısı ve Gıda-İçecek Harcaması İlişkisi", x = "Turist Sayısı", y = "Gıda-İçecek Harcaması") +
  theme_minimal()


# Turist Sayısı ve Carpet_Rug Harcaması İlişkisi
ggplot(data_tourism, aes(x = TouristNumber, y =Carpet_Rug)) +
  geom_point(aes(color = Year)) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Turist Sayısı ve Gıda-İçecek Harcaması İlişkisi", x = "Turist Sayısı", y = "Carpet_Rug") +
  theme_minimal()


# Turist Sayısı ve GSM_Expenditure Harcaması İlişkisi
ggplot(data_tourism, aes(x = TouristNumber, y =GSM_Expenditure)) +
  geom_point(aes(color = Year)) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Turist Sayısı ve GSM_Expenditure İlişkisi", x = "Turist Sayısı", y = "GSM_Expenditure") +
  theme_minimal()



# Yıl Bazında Kategori Oranları --- bunda IndExpend çıkar
category_ratios <- colMeans(data_train2[, output_features] / data_train2$Total_Income, na.rm = TRUE)
category_ratios_df <- data.frame(Category = names(category_ratios), Ratio = category_ratios)

ggplot(category_ratios_df, aes(x = Category, y = Ratio, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Yıl Bazında Kategori Oranları", x = "Kategori", y = "Oran") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Z-Skorları ile Kategorik Etiketler *----- bunu diğerleri için de yap
ggplot(data_tourism_hl, aes(x = Food_Beverage_Level, fill = Food_Beverage_Level)) +
  geom_bar() +
  labs(title = "Gıda-İçecek Harcaması Yüksek/Düşük Dağılımı", x = "Kategori Seviye", y = "Frekans") +
  theme_minimal()



###### CONFUSION MATRIX GÖRSELLEŞTİRMESİ BUNU HEPSİ İÇİN YAPP
# Confusion Matrix Sonuçları
conf_matrix <- confusionMatrix(factor(test_data$Food_Beverage_Pred, levels = c("Low", "High")), 
                               factor(test_data$Food_Beverage_Level, levels = c("Low", "High")))
# Confusion Matrix Görselleştirmesi
library(caret)
library(ggplot2)

# Confusion Matrix Tablosunu DataFrame'e Dönüştürme
confusion_df <- as.data.frame(conf_matrix$table)
colnames(confusion_df) <- c("Actual", "Predicted", "Freq")

# Görselleştirme
ggplot(confusion_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1.5, color = "black", size = 5) +
  labs(
    title = "Confusion Matrix: Food & Beverage Predicted vs Actual",
    x = "Predicted", 
    y = "Actual"
  ) +
  theme_minimal()



# Performans Sonuçlarını Bar Plot Olarak Görselleştirme
ggplot(results, aes(x = Category, y = Accuracy.Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performansı: Kategorik Sınıflandırma", x = "Kategori", y = "Doğruluk") + 
  scale_fill_brewer(palette = "RdYlBu") + 
  theme_minimal()



data_train5 = data_tourism

# Yıl ve Çeyrek Bilgilerini Birleştiriyoruz
data_train5$Date <- as.Date(paste(data_train5$Year, (data_train5$Quarter - 1) * 3 + 1, "01", sep = "-"))

# Tarihe Göre Sıralama
data_train5 <- data_train5 %>% arrange(Date)
View(data_train5)



# Zaman Serisi Grafiği: Turist Sayısı ve Gelir
ggplot(data_train5, aes(x = Date)) +
  geom_line(aes(y = TouristNumber, color = "Turist Sayısı"), size = 1) +
  geom_line(aes(y = Total_Income / 1000, color = "Turizm Geliri (Bin TL)"), size = 1) +
  labs(title = "Turist Sayısı ve Turizm Geliri: Zaman Serisi", 
       x = "Yıl", 
       y = "Değer") +
  scale_color_manual(values = c("Turist Sayısı" = "darkgreen", "Turizm Geliri (Thousand Dollar)" = "darkblue")) +
  theme_minimal()

# Çeyrek Bazlı Boxplot
ggplot(data_train5, aes(x = as.factor(Quarter), y = Total_Income / 1000, fill = as.factor(Quarter))) +
  geom_boxplot() +
  labs(title = "Çeyreklere Göre Turizm Geliri Dağılımı", 
       x = "Çeyrek (Quarter)", 
       y = "Turizm Geliri (Bin TL)") +
  theme_minimal()

install.packages("stl")
# Turizm Gelirleri İçin Zaman Serisi Nesnesi
income_ts <- ts(data_train5$Total_Income, start = c(min(data$Year), min(data$Quarter)), frequency = 4)

# Sezonluk ve Trend Analizi
decomp <- stl(income_ts, s.window = "periodic")
autoplot(decomp) + 
  labs(title = "Turizm Geliri Sezonluk ve Trend Analizi") +
  theme_minimal()
