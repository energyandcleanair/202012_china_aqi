remotes::install_github('energyandcleanair/rcrea')
library(rcrea)
library(dplyr)


# We join in db to reduce amount to download and
# thereby accelerate download
m <- rcrea::measurements(source="mee",
                         process_id = "city_day_mad",
                         poll=c(rcrea::PM10),
                         date_from="2018-01-01",
                         date_to="2020-12-31",
                         collect=F,
                         with_metadata=T) %>%
  rename(pm10=value) %>%
  select(-c(id, poll, geometry)) %>%
  dplyr::full_join(
    rcrea::measurements(source="mee",
                        process_id = "city_day_mad",
                        poll=c(rcrea::PM25),
                        date_from="2018-01-01",
                        date_to="2020-12-31",
                        collect=F,
                        with_metadata=F) %>%
      select(process_id, date, location_id, unit, pm25=value)
  ) %>%
  collect() %>%
  ungroup()

# Add province name
m <- m %>% left_join(read.csv("data/gadm1.csv") %>% mutate(gadm1_id=tolower(GID_1)) %>%
                  select(gadm1_id, province_en=NAME_1, province_zh=NL_NAME_1))

# Add AQI
m.aqi <- m %>% utils.add_aqi() %>% filter(!is.na(province_en))
saveRDS(m.aqi,  file.path("results","m.aqi.RDS"))
write.csv(m.aqi, file.path("results","m.aqi.csv"), row.names=F)


# Daily heatmap ----------------------------------------------------------------
(plt <- ggplot(m.aqi, aes(x=date, y=location_name, fill=aqi_cat)) +
  geom_tile() +
   facet_wrap(~province_en, scales="free_y") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
   scale_x_datetime(date_labels="%Y") +
  scale_fill_brewer(palette="RdYlGn", direction=-1, name=NULL) +
  labs(y=NULL, x=NULL,
       title="AQI Categoty in Chinese cities",
       subtitle="Each row corresponds to a city",
       caption="Source: CREA based on MEE"))

ggsave(file.path("results","plots","heatmap.png"), width=12, height=8)
