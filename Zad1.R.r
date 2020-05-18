#����������� ������ �������
library(rnoaa)
library(tidyverse)
library(lubridate)

#C������� ������� � ������� ��� �������
#��������� �� �������
ai = c(0.00,32.11,26.31,25.64,23.20,18.73,16.30,13.83,0.00)
bi = c(0.00,11.30,9.26,9.03,8.16,6.59,5.73,4.87,0.00)
#����������� ������������� ��� �������
Kf = 300
#������������ ������ ��������
Qj = 1600
#����������� ����� ������ �������� � �������� ���������
Lj = 2.2
#����������� "����������� ��������� ��������"
Ej = 25
#����������� ��� ��������� ������
y = 1.0

#����������  ������ ������������
station_data = ghcnd_stations() 
station_data = read.csv("station_data.csv")

#��������� ������ ������������
#����� ��������� ������ ���� �������, �������� �� ���� ������ 12 ������� ��������� � 
#������� �������,������ ������� � ������ ������� � ������������ ��� �������
#������������ � ������� ��������
Ulan-Ude = data.frame(id = " ULAN-UDE", latitude = 51.8272,  longitude = 107.606)
#�������� �������� ����� �������, ������� ����� ����������� ������
#� �������� ��������� ������, � ������� ����������, ������� ����������� ������ ���� � �������
Ulan-Ude_around = meteo_nearby_stations(lat_lon_df = Ulan-Ude, station_data = station_data,
                                       limit = 12, var = c("PRCP", "TAVG"),
                                       year_min = 1999, year_max = 2003)
# Ulan-Ude_around ��� ������ ������������ ��������� �������� �������� �������, ���������� �������������� ������������ ��������������� �� �� 
# ������������ �� Ulan-Ude.

#������ ��������� ������� ����� ������������� �����������, 
#���������� ��� ��������
Ulan-Ude _id = bryansk_around %>% select(id)
#������ �� ������� � ������
Ulan-Ude _id = bryansk_id[[1]]
Ulan-Ude _id2 = bryansk_id[1]
#������ � ������� ������������
#������� ���������, ���� ����� ������� ��� ������
all_data = data.frame()
di = data.frame()
#������� ���� ��� ���������� ������ � 12 ������������

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

#�������� di ��� ������� ������
#���������� ��������� ���������� ������
#������� �������� ����������� � �������������� ������
#�������� ������� year, month ��� �����������
all_data = read.csv("all_data.csv")
#������� �������� ����������� � ��������������� ������
all_data[(all_data$month < 4),"tavg"] = 0
all_data[(all_data$month > 8),"tavg"] = 0
all_data[(all_data$month == 4 & all_data$day < 15),"tavg"] = 0
all_data[(all_data$month == 8 & all_data$day > 15),"tavg"] = 0
#����������� �� ����� � ������� 
all_data = all_data %>% group_by(month)
#�������� di ��� ������� ������
di1 = summarise(all_data, di = length(tavg[tavg>70])/length(tavg))[-12,][-2,][-1,]
di=di1[[2]]
all_data2 = read.csv("all_data.csv")
all_bryansk = all_data2 %>% 
  mutate(year = year(date), month = month(date), day = day(date)) %>% 
  #����������� � ������ id ������������
  group_by(month, id) %>% 
  #������� �������� ����������� (����� 5 �������� �������)
  mutate(tavg=tavg/10) %>% filter (tavg>5)%>%
  #����������� �� ����� � ������� 
  summarise(sum = sum (tavg, na.rm = TRUE)) %>%
  group_by(month) %>%
  summarise(S = mean(sum,na.rm = TRUE))%>%
  #�������� ������� ��� ����������� ������� �����������
  mutate(Y = ((ai + bi * y * S * di) * Kf) / (Qj * Lj * (100 - Ej)))
#�������� ��������� �����������
Yield = sum(all_ Ulan-Ude$Y); Yield
#����������� ��� Ulan-Ude ������� � 1999 ���� ��������� 16.62 �/��
