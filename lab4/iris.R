#UTF-8 encoding

install.packages("RClickhouse")
install.packages("ellipse")
con <- DBI::dbConnect(RClickhouse::clickhouse(), host="", password="")
res <- DBI::dbGetQuery(con, "Select 2+2 as result")

#Найс название пакета бтв
install.packages("e1071")
library(caret)

#Пикаем таблицу с базы
dataset <- DBI::dbGetQuery(con, "Select * from bomba409.iris")
dataset$Class <- as.factor(dataset$Class)


class(dataset)
summary(dataset)
levels(dataset$Class)
sapply(dataset, class)

#Раскидываем значения и факторы
x <- dataset[,1:4]
y <- dataset[,5]


#Делаем диаграммки для того чтобы нас уважали
par(mfrow=c(1,4))
for(i in 1:4)
  boxplot(x[,i], main=names(dataset)[i])

featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")

#Погнал машин лернинг
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
set.seed(13)
fit.lda <- train(Class~., data=dataset, method="lda", metric=metric, trControl=control)

set.seed(13)
fit.cart <- train(Class~., data=dataset, method="rpart", metric=metric, trControl=control)

set.seed(13)
fit.knn <- train(Class~., data=dataset, method="knn", metric=metric, trControl=control)

set.seed(13)
fit.svm <- train(Class~., data=dataset, method="svmRadial", metric=metric, trControl=control)

set.seed(13)
fit.rf <- train(Class~., data=dataset, method="rf", metric=metric, trControl=control)

#Изучаем аутпут с умным видом
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

#Тестики
predictions <- predict(fit.lda, x)
confusionMatrix(predictions, y)
#Все красивенько
