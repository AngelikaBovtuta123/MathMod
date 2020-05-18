## дневных потоков углекислого газа за весенний период 2013 года по данным измерений методом турбулентной пульсации
# проверка рабочей директории
getwd()


library("tidyverse")# Всё обо всем
library("readr")# функция read_csv()
library("stringr")# функция str_replace_all
библиотека ("dplyr") # функции: Filter (), arrange (), select (), mutate (), summary (), group_by_n()
library("ggplot2")# графики функций qplot()

#считываем файл
eddypro  = читать.csv ("eddypro.csv", skip  =  1, na  = c ( "" , " NA " , "-9999" , "-9999.0" ), комментарий  = c( " [ "))
#готовим данные
# Удаляем ненужную пустую первую строку
eddypro  =  эддипро [ - 1,]
# Удаляем ненужный пустой столбец "roll"
eddypro  = select (eddypro, - (roll))
# Преобразуем в факторы (factor) столбы типа char(символ)
eddypro  =  eddypro % > > % mutate_if( is.характер, фактор)
#Заменим специальные символы в названии стобцов на допустимые для переменных имена
имена (eddypro) = имена (eddypro ) %>>%
 str_replace_all (" [!] ", "_exclam_") %>>%
 str_replace_all("[?] ", "_quest_") %>>% 
 str_replace_all ( " [ * ]", "_star") %>>% 
 str_replace_all ( " [ + ]", "_plus_") %>>%
 str_replace_all ( " [ - ]", "_minus_") %>>%
 str_replace_all ("[ @ ]", "_at_" ) %>>%
 str_replace_all ( " [ $ ]", "_dollar_") %>>%
 str_replace_all ( " [ # ]", "_hash_") %>>%
 str_replace_all ( " [ / ]", "_slash_") %>>%
 str_replace_all( "[%] " , " __pecent_") %>>%
 str_replace_all ( " [ & ]", "_amp_") %>>%
 str_replace_all( "[ \\ ^] " , "_power_") %>>%
 str_replace_all( "[()] " , " _ ")
#Возвратим столбцы таблицы в виде векторов для проверки,посмотрим
беглый взгляд( eddypro)
#Удалим строки в которых содержится NA
eddypro = drop_na(eddypro)
# Отфильтруем по заданию данные только за весенний период. 
eddypro  = фильтр (eddypro, DOY > > =  151  &  DOY <  242)
# Отфильтруем данные по заданию только за дневное время
eddypro  = фильтр (eddypro, дневное  время = = TRUE)
# Получим таблицу, состоящую только из чисел для работы с ней
eddypro_numeric  =  eddypro [, sapply( eddypro, is.числовой)]
# Получим таблицу, содержащую остальные колонки
eddypro_non_numeric  =  eddypro [,!sapply( eddypro, is.числовой)]
# Создадим обучающую и тестирующую непересекающиеся выборки с помощью базового функционала
row_numbers  =  1: длина (eddypro_numeric $ co2_flux)
teach  = sample( row_numbers , floor(length( eddypro_numeric $ co2_flux))*.7))
test  =  row_numbers [- учить]
#Обучающая выборка
teaching_tbl  =  eddypro_numeric [ учить,]
#Тестирующая выборка
testing_tbl  =  eddypro_numeric [ тест,]
# Создадим модель добавив в нее все переменные с помощью "(.)" и используя обучающую выборку
mod  = lm (co2_flux~ (.), data  =  teaching_tbl)
