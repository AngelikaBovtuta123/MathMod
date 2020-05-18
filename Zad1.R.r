#Подключение нужных пакетов
library(rnoaa)
library(tidyverse)
library(lubridate)

#Cоздадим векторы с данными для расчета
#Константы из таблицы
ai = c(0.00,32.11,26.31,25.64,23.20,18.73,16.30,13.83,0.00)
bi = c(0.00,11.30,9.26,9.03,8.16,6.59,5.73,4.87,0.00)
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент суммы частей основной и побочной продукции
Lj = 2.2
#Коэффициент "Стандартная влажность культуры"
Ej = 25
#Коэффициент для экпозиции склона
y = 1.0

#Скачивание  списка метеостанций
station_data = ghcnd_stations() 
station_data = read.csv("station_data.csv")

#Формируем список метеостанций
#После получения списка всех станций, выбираем из него список 12 станций ближайших к 
#столице региона,создав таблицу с именем региона и координатами его столицы
#координаторы в десятых градусов
Ulan-Ude = data.frame(id = " ULAN-UDE", latitude = 51.8272,  longitude = 107.606)
#Выбираем конечное число станций, которые имеют необходимые данные
#в заданный временной период, и выбрать переменные, которые обязательно должны быть в наличии
Ulan-Ude_around = meteo_nearby_stations(lat_lon_df = Ulan-Ude, station_data = station_data,
                                       limit = 12, var = c("PRCP", "TAVG"),
                                       year_min = 1999, year_max = 2003)
# Ulan-Ude_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Ulan-Ude.

#Первым элементом таблицы будет идентификатор метеостанци, 
#попытаемся его получить
Ulan-Ude _id = bryansk_around %>% select(id)
#теперь из таблицы в вектор
Ulan-Ude _id = bryansk_id[[1]]
Ulan-Ude _id2 = bryansk_id[1]
#Работа с данными метеостанций
#Создаем датафрейм, куда будут скачаны все данные
all_data = data.frame()
di = data.frame()
#Создаем цикл для скачивания данных с 12 метеостанций

for (v in 1:12)
{
  Ulan-Ude_id = Ulan-Ude_around[["ULAN-UDE"]][["id"]][v]
  #
  data = meteo_tidy_ghcnd(stationid = Ulan-Ude_id,
                          var="TAVG",
                          date_min="1999-01-01",
                          date_max="2003-12-31")
  all_data = bind_rows(all_data, data%>%
                      mutate(year = year(date), month = month(date), day= day (date)) %>%
                      group_by(month, year,day))
                                     
}

write.csv(all_data, file="all_data.csv")

#Вычислим di для каждого месяца
#Произведем обработку полученных данных
#обнулим значение температуры в невегетативный период
#Создадим колонки year, month для группировки
all_data = read.csv("all_data.csv")
#обнулим значение температуры в невегетационный период
all_data[(all_data$month < 4),"tavg"] = 0
all_data[(all_data$month > 8),"tavg"] = 0
all_data[(all_data$month == 4 & all_data$day < 15),"tavg"] = 0
all_data[(all_data$month == 8 & all_data$day > 15),"tavg"] = 0
#Сгруппируем по годам и месяцам 
all_data = all_data %>% group_by(month)
#Вычислим di для каждого месяца
di1 = summarise(all_data, di = length(tavg[tavg>70])/length(tavg))[-12,][-2,][-1,]
di=di1[[2]]
all_data2 = read.csv("all_data.csv")
all_bryansk = all_data2 %>% 
  mutate(year = year(date), month = month(date), day = day(date)) %>% 
  #Сгруппируем с учетом id метеостанций
  group_by(month, id) %>% 
  #Выберем активные температуры (более 5 градусов Цельсия)
  mutate(tavg=tavg/10) %>% filter (tavg>5)%>%
  #Сгруппируем по годам и месяцам 
  summarise(sum = sum (tavg, na.rm = TRUE)) %>%
  group_by(month) %>%
  summarise(S = mean(sum,na.rm = TRUE))%>%
  #Создадим колонку для результатов расчета урожайности
  mutate(Y = ((ai + bi * y * S * di) * Kf) / (Qj * Lj * (100 - Ej)))
#Вычислим суммарную урожайность
Yield = sum(all_ Ulan-Ude$Y); Yield
#Урожайность для Ulan-Ude области в 1999 году составила 16.62 ц/га
