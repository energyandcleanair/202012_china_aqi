utils.add_aqi <- function(df){
  # Capped at 300
  #TODO confirm definition
  df %>% mutate(
    aqi_pm25=replace_na(approx(x=c(0, 35, 75, 115, 150, 250, Inf),
                    y=c(0, 50, 100, 150, 200, 300, 300),
                    xout=pm25)$y,0),

    aqi_pm10=replace_na(approx(x=c(0, 50, 150, 250, 350, 420, Inf),
                    y=c(0, 50, 100, 150, 200, 300, 300),
                    xout=pm10)$y,0),
    aqi=pmax(aqi_pm25, aqi_pm10, na.rm=T),
    aqi_argmax=ifelse(aqi_pm25>aqi_pm10 | is.na(aqi),"pm25","pm10"),
    aqi_cat=cut(aqi,
                breaks=c(0,50,100,150,200,300,Inf),
                labels=c("Excellent","Good","Slightly Polluted","Lightly Polluted",
                         "Moderately Polluted", "Heavily Polluted"))
  )
}
