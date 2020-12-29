utils.add_aqi <- function(df){
  # Capping at 500
  # Using this: https://www.mee.gov.cn/ywgz/fgbz/bz/bzwb/jcffbz/201203/W020120410332725219541.pdf
  df %>% mutate(
    aqi_pm25=replace_na(approx(x=c(0, 35, 75, 115, 150, 250, 350, 500, Inf),
                               y=c(0, 50, 100, 150, 200, 300, 400, 500, 500),
                               xout=pm25)$y,0),
    aqi_pm10=replace_na(approx(x=c(0, 50, 150, 250, 350, 420, 500, 600, Inf),
                               y=c(0, 50, 100, 150, 200, 300, 400, 500, 500),
                               xout=pm10)$y,0),
    aqi_no2=replace_na(approx(x=c(0, 40, 80, 180, 280, 565, 750, 940, Inf),
                              y=c(0, 50, 100, 150, 200, 300, 400, 500, 500),
                              xout=no2)$y,0),
    aqi_so2=replace_na(approx(x=c(0, 50, 150, 475, 800, 1600, 2100, 2620, Inf),
                              y=c(0, 50, 100, 150, 200, 300, 400, 500, 500),
                              xout=so2)$y,0),
    aqi=pmax(aqi_pm25, aqi_pm10, aqi_no2, aqi_so2, na.rm=T),
    aqi_cat=as.integer(cut(aqi,
                           breaks=c(0,50,100,150,200,300,400,500,Inf),
                           labels=c(1,2,3,4,5,6,6,6)))
  ) %>%
    mutate(aqi_argmax=factor(max.col(.[c("aqi_pm25","aqi_pm10","aqi_no2", "aqi_so2")]),
                             seq(1,4),
                             labels=c("pm25","pm10","no2","so2"),
                             ordered=F)) %>%
    left_join(tibble(aqi_cat=seq(1,6),
                     aqi_cat_zh=factor(seq(1,6), seq(1,6), labels=c("优","良", "轻度污染", "中度污染", "重度污染", "严重污染"), ordered=T),
                     aqi_cat_en=factor(seq(1,6), seq(1,6), labels=c("Excellent", "Good", "Mildly polluted", "Moderately polluted", "Heavily polluted", "Severely polluted"), ordered=T)))
}


utils.fill_city_dates <- function(m, min_date="2018-01-01"){
  # To ensure comparability across years,
  # we fill empty cityxdate combinations with NA

  date <- seq(lubridate::date(min_date), lubridate::today(), by="day")
  m %>% right_join(
    m %>% distinct(
      location_id, location_name, province_en, gadm1_id, process_id
    ) %>%
      tidyr::crossing(date)
  )
}
