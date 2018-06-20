library("sandwich")
library("memisc")  
library("psych")  
library("lmtest")  
library("sjPlot")  
library("sgof")
library("foreign")  
library("car")
library("hexbin")  
library("tidyverse") 
library("rlms")
library("haven")
# Этот скрипт загружает данные 22 волны базы РЛМС
# отбирает 6 переменных и создает дамми-переменные
# для последующего построения регрессионной модели
# описание переменных:https://www.hse.ru/data/2014/09/01/1313533595/R22ind_cb.pdf
# загрузка данных: https://www.hse.ru/rlms/spss
h<-rlms_read("r22i_os26a.sav")

h2 <- select(h, wage = rj13.2,
                age = rh6,
                sex = rh5,
                edu = r_diplom,
                type = status,
                sat = rj1.1.1)%>% 
      mutate(age = 2013 - age)  
glimpse(h2)
l2 <- h2[h2$type %in% c("областной центр","город"),]
l3 <- l2[l2$sat %in% c(NA, "ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ","СКОРЕЕ УДОВЛЕТВОРЕНЫ"), ]
l4 <- l3[l3$edu %in% c(NA,"окончил 0 - 6 классов",
               "незаконченное среднее образование (7 - 8 кл)",
               "незаконченное среднее образование (7 - 8 кл) + что-то еще",
               "законченное среднее образование",
               "законченное среднее специальное образование",
               "законченное высшее образование и выше"),]

l4$type <- memisc::recode(l4$type, 1 <- "город", 0 <- "областной центр")
l4$sat <- memisc::recode(l4$sat, 1<- "ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ",
                         0 <- "СКОРЕЕ УДОВЛЕТВОРЕНЫ")
l4$sex <- memisc::recode(l4$sex, 1 <- "МУЖСКОЙ",0 <- "ЖЕНСКИЙ")
l4$edu1 <- factor ( with ( l4, ifelse ( ( edu == "окончил 0 - 6 классов" |
                            edu == "незаконченное среднее образование (7 - 8 кл)" |
                            edu == "незаконченное среднее образование (7 - 8 кл) + что-то еще" ), 1, 0 ) ) )
l4$edu2 <- factor ( with ( l4, ifelse ( ( edu == "законченное среднее образование" ), 1, 0 ) ) )
l4$edu3 <- factor ( with ( l4, ifelse ( ( edu == "законченное среднее специальное образование" ), 1, 0 ) ) )
l4$edu4 <- factor ( with ( l4, ifelse ( ( edu == "законченное высшее образование и выше" ), 1, 0 ) ) )
l5<-na.omit(l4)
summary(l4)
hist(l5$wage)
model<-lm(data=l5,wage~age+sex+edu2+edu3+edu4+type+sat)
coeftest(model, vcov. = vcovHC(model))
summary(model)
