library(prophet)
library(tidyverse)
library(lubridate)
library(scales)


# import data
train <- read_csv("D:/UNI/Development of Data Products/exam project/rossmann-store-sales/train.csv", guess_max = 100000)
store = read_csv("D:/UNI/Development of Data Products/exam project/rossmann-store-sales/store.csv",  guess_max = 100000)
# remove closed stores and those with no sales
storepromo = store$Store[store$Promo2 ==1]
train = train %>% filter( Open!= 0 )  %>% arrange(Store, Date) 
train$StateHoliday = as_factor(train$StateHoliday)

year(min(train$Date))
yearmax = year(max(train$Date))
# def function

ggpromof = function(storetemp,storecod ) {
  
  PromoMonths = storetemp %>% select(PromoInterval) %>%as.character()
  PromoMonths = unlist(strsplit(PromoMonths, ",") )
  
  yearmin = storetemp$Promo2SinceYear[storetemp$Store==storecod] 
  
  ggpromo = as.Date(character(0))
  for (year in seq(yearmin, yearmax,1) )  {
    for (mese in PromoMonths){
    init_m = as.Date(paste0('1/',mese, '/', year),format='%d/%b/%Y')  
    fin_m = as.Date(paste( 1, month(init_m)+1, year, sep = "/"),format='%d/%m/%Y')-1
    ggpromo = c(ggpromo, seq(init_m, fin_m,1) )
    }
  }
  
  return(ggpromo[ggpromo >= ymd( paste0(yearmin, "-01-01" )) + weeks( store$Promo2SinceWeek - 1 )])
}

storecod = 2
gg = if(sum(storecod== storepromo) ==1) ggpromof(store %>% filter(Store ==storecod), storecod )

# sales for the store number 1 (StoreType C)
df = train %>% filter(Store ==storecod) %>% transmute( ds= Date, y = Sales)
holidays <- train %>% filter(Store ==storecod) %>% transmute(ds = Date,  # lower_window = 0, upper_window = 1,
      StateHoliday= if_else(StateHoliday ==0, 0, 1) , SchoolHoliday    ) %>% gather("holiday",n,  -ds ) %>% 
  bind_rows(tibble(ds = gg, holiday = 'promo', n = 0) ) %>% 
                   transmute(ds = ds, holiday = holiday, lower_window = 0, upper_window = 1)
# filter (holiday >0) 
# plot daily sales
# plot(df$ds, df$y, 'l')

m <- prophet(growth='linear', daily.seasonality = T, interval_width = 0.95, holidays = holidays)
# m <- add_country_holidays(m, country_name = 'Germany') 
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
m <- fit.prophet(m, df)

# prophet:::plot_yearly(m)
# prophet_plot_components(m, forecast)

future <- make_future_dataframe(m, periods = 6*7)
# future$cap <- round(max(df$y)/1e3)*1e3 
# future$cap <- 8.5
fcst <- predict(m, future)
# tail(fcst[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], 6*7 ) %>% View()

# fcst %>%   select(ds, StateHoliday, SchoolHoliday) %>% 
#  filter(abs(StateHoliday+ SchoolHoliday) > 0) %>%   tail(10)
 plot(m, fcst )  +  xlim(max(fcst$ds) + days(-90)  ,max(fcst$ds))+ labs(title = 'historical data and forecast')
# prophet_plot_components(m, fcst)

# gather(type, val, -ds) %>%
#  ggplot(aes(ds, val, colour = type)) + geom_line()

df.cv <- cross_validation(m, initial = round( nrow(df)/4*3), period = round( nrow(df)/4), horizon = 6*7, units = 'days')
head(df.cv)

df.p <- performance_metrics(df.cv)
head(df.p)
plot_cross_validation_metric(df.cv, metric='mape')  + scale_y_continuous(labels=percent) +
  labs (  title = paste0("mean abs perc error: ", round( mean(abs(df.cv$y - df.cv$yhat)/df.cv$y), 3)*100, "%" ))


###################
setwd("d:/UNI/FinEc/LCRcoredeposits")

dataset_lcr <- read_csv("dataset_lcr.csv")
#View(dataset_lcr)

code = dataset_lcr$cod[1]
code = 238
for (code in unique(dataset_lcr$cod)) {
  df = dataset_lcr %>% filter(cod == code) %>% transmute(ds = mdy(dat), y = vol, cap = 1.5, floor = 0)
  # plot(df$ds, df$y)
  # m <- prophet(growth='logistic', daily.seasonality = F, interval_width = 0.95)
  # m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
  # m <- fit.prophet(m, df)
  
  m = prophet(growth = 'linear', changepoint.prior.scale =.8, changepoint.range = .75,
              daily.seasonality = F, weekly.seasonality = F, yearly.seasonality = F)
  m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=30)
  #m <- add_seasonality(m, name='yearly', period=365.25, fourier.order=5)
  m <- fit.prophet(m, df)
  
  future <- make_future_dataframe(m, periods = 6, freq = 'month')
  fcst <- predict(m, future)
  future['cap'] = 1.5; future['floor'] = 0
  fcst = predict(m,future )
  #prophet_plot_components(m, fcst)
  plot(m, fcst )  + labs(title = code )
  
  ggsave(paste0(code, '.jpg'))
  
}
