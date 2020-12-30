remotes::install_github('energyandcleanair/rcrea')
library(rcrea)
library(dplyr)
library(tidyr)



m <- rcrea::measurements(source="mee",
                         process_id = "city_day_mad",
                         poll=c(
                           rcrea::PM25,
                           rcrea::PM10,
                           rcrea::NO2,
                           rcrea::SO2),
                         date_from="2016-01-01",
                         date_to="2020-12-31",
                         with_metadata=T) %>%
  spread("poll","value") %>%
  mutate(date=lubridate::date(date))

# Add province name
m <- m %>% left_join(read.csv("data/gadm1.csv") %>% mutate(gadm1_id=tolower(GID_1)) %>%
                  select(gadm1_id, province_en=NAME_1, province_zh=NL_NAME_1)) %>%
   filter(!is.na(province_en))

# Add sand storm
m <- m %>% utils.add_sandstorm()

# Add AQI
m.aqi <- m %>% utils.add_aqi() %>% filter(!is.na(province_en))
saveRDS(m.aqi,  file.path("results","m.aqi.RDS"))

# Fill missing dates with NA to ensure comparability across years
m.aqi <- m.aqi %>% utils.fill_city_dates(min_date=min(m$date))


# Daily heatmap ----------------------------------------------------------------
(plt <- ggplot(m.aqi %>% filter(date>="2018-01-01"), aes(x=date, y=location_name, fill=aqi_cat_en)) +
  geom_tile() +
   facet_wrap(~province_en, scales="free_y") +
   rcrea::theme_crea() +
    theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_x_date(date_labels="%Y", date_breaks = "1 year") +
  # scale_fill_brewer(palette="RdYlGn", direction=-1, name=NULL) +
  scale_fill_manual(values=c("#50C878","#FCF4A3","orange","red","purple","brown","white"),
                    name=NULL) +
  labs(y=NULL, x=NULL,
       title="AQI Categoty in Chinese cities",
       subtitle="Each row corresponds to a city",
       caption="Source: CREA based on MEE"))

ggsave(file.path("results","plots","heatmap.png"), width=12, height=8)



# Responsible pollutant per category ------------------------------------------
(plt <- ggplot(m.aqi %>% filter(date>="2018-01-01", !sandstorm)) +
   geom_bar(stat="count", aes(x=aqi_argmax,
                              group=aqi_cat_en,
                              y=..prop..,
                              fill=factor(..x..)),
            show.legend = F) +
   facet_wrap(~aqi_cat_en, scales="free_x") +
   theme(axis.text.y=element_blank(),
         axis.ticks.y=element_blank()) +
   scale_x_discrete(drop=FALSE) +
   scale_y_continuous(labels=scales::percent) +
   rcrea::theme_crea() +
   labs(x=NULL,y=NULL,
        title="Pollutant determining the daily AQI",
        subtitle="2018-2020",
        caption="Source: CREA based on MEE"))

ggsave(file.path("results","plots","aqi_pollutants.png"), width=8, height=5)


# Responsible pollutant per province ------------------------------------------
(plt <- ggplot(m.aqi %>% filter(aqi_cat %in% c(5,6), date>="2018-01-01")) +
   geom_bar(stat="count", aes(x=aqi_argmax,
                              group=province_en,
                              y=..prop..,
                              fill=factor(..x..)),
            show.legend = F) +
   facet_wrap(~province_en, scales="free_x") +
   theme(axis.text.y=element_blank(),
         axis.ticks.y=element_blank()) +
   scale_x_discrete(drop=FALSE) +
   scale_y_continuous(labels=scales::percent) +
   rcrea::theme_crea() +
   labs(x=NULL,y=city x day,
        title="Pollutant responsible for heavy polluted days",
        subtitle="AQI > 200 during 2018-2020",
        caption="Source: CREA based on MEE"))

ggsave(file.path("results","plots","aqi_pollutants_province.png"), width=12, height=8)


# Number of days with AQI>200 per province ------------------------------------------
(plt <- ggplot(m.aqi %>%
                  filter(!sandstorm) %>%
                  group_by(province_en, aqi_cat, aqi_cat_en, year=lubridate::year(date)) %>%
                  summarise(count=n()) %>%
                  mutate(aqi_cat_en=factor(aqi_cat_en, levels=rev(levels(.$aqi_cat_en))))
                 ) +
   geom_area(stat="identity", aes(x=year,
                               y=count,
                               fill=factor(aqi_cat_en))) +
   # guides(fill = guide_legend(reverse=TRUE)) +
   # geom_area(stat="count", aes(x=year,
   #                            group=province_en,
   #                            y=..prop..,
   #                            fill=factor(..x..)),
   #          show.legend = F) +
   facet_wrap(~province_en, scales="free_y") +
   theme(axis.text.y=element_blank(),
         axis.ticks.y=element_blank()) +
   scale_x_discrete(drop=FALSE) +
   # scale_y_continuous(labels=scales::percent) +
   scale_fill_brewer(palette="RdYlGn", direction=1, name=NULL, na.value="grey") +
   rcrea::theme_crea() +
   labs(x=NULL,y=NULL,
        title="Air quality levels in Chinese regions",
        caption="Source: CREA based on MEE"))

ggsave(file.path("results","plots","aqi_ts_province.png"), width=12, height=8)


# Number of days with AQI>200 in selected cities ------------------------------------------
cities <- c("Beijing","Shanghai","Guangzhou","Shenzhen","Chengdu","Hangzhou","Wuhan","Chongqing","Nanjing","Tianjin","Suzhou","Xi'an","Changsha","Shenyang","Qingdao","Zhengzhou","Dalian","Dongguan","Ningbo")
(plt <- ggplot(m.aqi %>% filter(location_name %in% cities,
                                aqi_cat %in% c(5,6)) %>%
                  group_by(location_name, year=lubridate::year(date)) %>%
                  summarise(count=n())) +
   geom_line(aes(year, count)) +
   facet_wrap(~location_name) +
   rcrea::theme_crea() +
   scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0,0.2))) +
   labs(x=NULL,y=NULL,
        title="Number of heavy polluted days in selected Cities",
        subtitle="AQI > 200",
        caption="Source: CREA based on MEE"))

ggsave(file.path("results","plots","aqi_ts_province.png"), width=12, height=8)


# Number of days with sandstorm ------------------------------------------

(plt <- ggplot(m.aqi %>%
                  group_by(location_name, province_en, year=lubridate::year(date)) %>%
                  summarise(count=sum(as.integer(sandstorm))) %>%
                  mutate(count=tidyr::replace_na(count,0)) %>%
                  group_by(province_en, year) %>%
                  summarise(count=mean(count))) +
      geom_bar(stat="identity", aes(year, count)) +
      facet_wrap(~province_en) +
      rcrea::theme_crea() +
      scale_y_continuous(limits=c(0,NA), expand = expansion(mult = c(0,0.2))) +
      labs(x=NULL,y=NULL,
           title="Average number of sandstorm days in China provinces",
           caption="Storm-day defined as (PM10 > 300 µg/m3) and (PM2.5/PM10 < 0.5). Source: CREA based on MEE"))

ggsave(file.path("results","plots","sandstorm_bar_province.png"), width=12, height=8)


