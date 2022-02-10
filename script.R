library(caret)
library(tidyverse)

# nacitavanie dat
data <- read.csv("\\Student_Performance_new.csv")
head(data,10)

# mozme odstranit prvy stlpec
data[1] <- NULL

# analyzovanie dat
data[is.na(data)]
nrow(data)
ncol(data)
sapply(data, typeof)
table(data$race.ethnicity)
table(data$parental.level.of.education)
data <- mutate(data,avg.score.percentage = rowMeans(data[,5:7]))

# Vysledky testov na zaklade pohlavia
data %>%
  group_by(sex) %>%
  summarise(avg.math = mean(math.percentage),avg.read = mean(reading.score.percentage),write.avg = mean(writing.score.percentage))

# priemer z testov na zaklade vzdelania rodicov
data %>%
  group_by(parental.level.of.education) %>%
  summarise(avg.score = mean(avg.score.percentage))

# priemer z testov na zaklade ucastnenia sa pripravy
data %>%
  group_by(test.preparation.course) %>%
  summarise(avg.score = mean(avg.score.percentage))


# priemer z testov na zaklade druhu obeda
data %>%
  group_by(lunch) %>%
  summarise(avg.score = mean(avg.score.percentage))
boxplot(math.percentage~lunch,data=data)

# uprave dat pre cross-validaciu
data$lunch_num <- 1
data$lunch_num[data$lunch == "free/reduced"] <- 0

# ukazka cross validacie s grafom
teacher <- data[1:800,]
tester <- data[801:1000,]
# vytvarame model podla ucitela
model <- lm(math.percentage ~ writing.score.percentage + reading.score.percentage + lunch_num, data = teacher)
plot(math.percentage ~ writing.score.percentage, data = teacher)
abline(a=model[[1]], b=coef(model)[[2]]+coef(model)[[3]]+coef(model)[[4]])
# predikujeme data pre testera
predictions <- model %>%
  predict(tester)
pred <- data.frame(predictions)
# vykreslujeme data do grafu
plot(pred$predictions ~ tester$writing.score.percentage,col="red",ylim = c(0,1))
points(x=tester$writing.score.percentage,y=tester$math.percentage,col="green")




# K-fold cross validacia, k = 5
# Rozdelujeme data do 5 skupin
testers <- split(data, rep(1:5, nrow(data)/5))

# funkcia pre vypocitanie odchyliek modela obsahujuceho obed
reg_model_lunch <- function(akt_tester){
  teacher_test <- anti_join(data,akt_tester)
  model <- lm(math.percentage ~ writing.score.percentage + reading.score.percentage + lunch_num, data = teacher_test)
  predictions <- model %>%
    predict(akt_tester)
  pred <- data.frame(predictions)
  return(data.frame( R2 = R2(predictions, akt_tester$math.percentage),
                     RMSE = RMSE(predictions, akt_tester$math.percentage),
                     MAE = MAE(predictions, akt_tester$math.percentage)))
}

# funkcia pre vypocet odchyliek modela bez obedu
reg_model_no_lunch <- function(akt_tester){
  teacher_test <- anti_join(data,akt_tester)
  model <- lm(math.percentage ~ writing.score.percentage + reading.score.percentage, data = teacher_test)
  predictions <- model %>%
    predict(akt_tester)
  pred <- data.frame(predictions)
  return(data.frame( R2 = R2(predictions, akt_tester$math.percentage),
                     RMSE = RMSE(predictions, akt_tester$math.percentage),
                     MAE = MAE(predictions, akt_tester$math.percentage)))
}

# mapovanie funkcii na vsetkych 5 skupin
map(testers,reg_model_no_lunch)
map(testers,reg_model_lunch)