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
                         date_from="2018-01-01",
                         date_to="2020-12-31",
                         with_metadata=T) %>%
  spread("poll","value")

# Add province name
m <- m %>% left_join(read.csv("data/gadm1.csv") %>% mutate(gadm1_id=tolower(GID_1)) %>%
                  select(gadm1_id, province_en=NAME_1, province_zh=NL_NAME_1))

# Add AQI
m.aqi <- m %>% utils.add_aqi() %>% filter(!is.na(province_en))
saveRDS(m.aqi,  file.path("results","m.aqi.RDS"))
write.csv(m.aqi, file.path("results","m.aqi.csv"), row.names=F)


# Daily heatmap ----------------------------------------------------------------
(plt <- ggplot(m.aqi, aes(x=date, y=location_name, fill=aqi_cat_en)) +
  geom_tile() +
   facet_wrap(~province_en, scales="free_y") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_x_datetime(date_labels="%Y", date_breaks = "1 year") +
  scale_fill_brewer(palette="RdYlGn", direction=-1, name=NULL) +
  # scale_fill_manual(values=c("green","yellow","orange","red","purple","brown"), name=NULL) +
  labs(y=NULL, x=NULL,
       title="AQI Categoty in Chinese cities",
       subtitle="Each row corresponds to a city",
       caption="Source: CREA based on MEE"))

ggsave(file.path("results","plots","heatmap.png"), width=12, height=8)


# Responsible pollutant per category ------------------------------------------
(plt <- ggplot(m.aqi) +
   geom_bar(stat="count", aes(x=aqi_argmax,
                              group=aqi_cat_en,
                              y=..prop..,
                              fill=..x..),
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
(plt <- ggplot(m.aqi %>% filter(aqi_cat >= 5)) +
   geom_bar(stat="count", aes(x=factor(aqi_argmax),
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
   labs(x=NULL,y=NULL,
        title="Pollutant responsible for heavy polluted days",
        subtitle="AQI > 200 during 2018-2020",
        caption="Source: CREA based on MEE"))

ggsave(file.path("results","plots","aqi_pollutants_province.png"), width=12, height=8)
