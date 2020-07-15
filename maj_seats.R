library(data.table)

setwd("~/git/zsuo/2018/districts/")
districts <- list.files(".", pattern = ".csv")

zsuo.prop <- read.csv("prop.csv", sep = ";", dec = ",")

results <- read.csv(districts[1])
results$`Доля` <- results$Голоса / sum(results$Голоса)
results <- results[order(results$Голоса, decreasing = TRUE), ]
elected <- results[1, ]

GetElected <- function(x){
  results <- read.csv(x)
  results$`Доля` <- results$Голоса / sum(results$Голоса)
  results <- results[order(results$Голоса, decreasing = TRUE), ]
  elected <- results[1, c(1, 2, 4)]
  elected[, 3] <- round(elected[, 3], 2)
  return(elected)
}

elected <- lapply(districts, GetElected)
elected <- data.frame(rbindlist(elected))
elected <- cbind("Округ" = c(1:18), elected)

# количество мандатов
table(elected$Партия)
