# Необходимые пакеты
library(reshape2)

# Загрузка данных ----
setwd('~/git/zsuo/2018/')
zsuo.prop <- read.csv("prop.csv", sep = ";", dec = ",")

# подсчёт результатов
results <- data.frame(party = colnames(zsuo.prop[, 4:12]),
                      stringsAsFactors = FALSE)
results$votes <- colSums(zsuo.prop[, 4:12]) # подсчитывает голоса
results$percents <- results$votes / sum(results$votes) # вычисляет доли
results <- results[order(results$percents, 
                         decreasing = T), ] # упорядочивает по убыванию

# распределение мандатов ----
threshold <- .05 # проходной барьер (в долях, а не процентах!)
seats <- 18 # количество распределяемых мандатов
divisor <- 2 # первый делитель (смотреть в законе о выборах)

# преодолели проходной барьер
elected <- results[results$percents >= threshold, c(1, 2)] 
parties <- levels(as.factor(elected$party)) # названия партий

# пустой массив с коэффициентами
ratio <- matrix(data = 1/ divisor:seats, nrow = nrow(elected), 
                ncol = length(seq(divisor:seats)), byrow = TRUE)

# умножение голосов на коэффициенты
ratio <- data.frame(ratio) * elected$votes

# добавляем названия партий
ratio <- cbind(party = elected$party, ratio)

# трансформация таблицы
ratio <- melt(ratio, id.vars = "party")
# распределение мандатов
seats <- ratio[order(ratio$value, decreasing = TRUE), ]$party[c(1:seats)]

# проверка: все ли преодолевшие барьер партии получили мандат
check <- setdiff(parties, levels(as.factor(seats)))
# если не все, то присуждаем им мандаты
if (length(check) != 0){
  seats[c(length(seats) - length(check) + 1):length(seats)] <- check
}

# итоговое распределение
table(seats)
