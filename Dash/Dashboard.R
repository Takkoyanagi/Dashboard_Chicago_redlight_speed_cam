#Import Necessary packages

library(tidyverse)
library(janitor)
library(lubridate)
library(ggthemes)
library(plotly)
library(leaflet)

red_viol <- read_csv('red-light-camera-violations.csv')
speed_viol <- read_csv('speed-camera-violations.csv')


redv_year <- red_viol %>% clean_names() %>% 
                    select(address, violation_date, violations) %>%
                                      mutate(month = month(violation_date), year = year(violation_date)) %>%
                                              group_by(year) %>%
                                                    summarise(sum_year_v = sum(violations)) %>%
                                                        as.data.frame()
                                                              
ggplot(redv, aes(year, sum_month_v, fill = 'blue', alpha =0.5)) + geom_bar(stat = 'identity')




redv_month <- red_viol %>% clean_names() %>% 
              select(address, violation_date, violations) %>%
                          mutate(month = as.factor(month(violation_date)), year = (year(violation_date))) %>%
                                  filter(violation_date>= as.Date('2015-01-01')) %>% 
                                          group_by(month) %>%
                                                  summarise(total_viol = sum(violations))

                                                    

ggplotly(ggplot(redv_month, aes(month, total_viol, alpha =0.5)) + 
           geom_bar(stat = 'identity', fill="blue", color='black') + 
           theme_bw() +
           ggtitle('Total Number of Redlight Camera Violations per Month (from 2015-Present)') +
           ylab("Total number of violations")+
           coord_cartesian(ylim = c(100000,225000))
         )
        



speedv_year <- speed_viol %>% clean_names() %>% 
      select(address, violation_date, violations) %>%
            mutate(month = month(violation_date), year = year(violation_date)) %>%
                  group_by(year) %>%
                      summarise(sum_year_v = sum(violations))

ggplot(redv, aes(year, sum_month_v, fill = 'blue', alpha =0.5)) + geom_bar(stat = 'identity')



speedv_month <- speed_viol %>% clean_names() %>% 
            select(address, violation_date, violations) %>%
                    mutate(month = as.factor(month(violation_date)), year = (year(violation_date))) %>%
                            filter(violation_date>= as.Date('2015-01-01')) %>% 
                                  group_by(month) %>%
                                          summarise(total_viol = sum(violations))

ggplotly(ggplot(speedv_month, aes(month, total_viol, alpha =0.5)) + 
           geom_bar(stat = 'identity', fill="green", color='black') + 
           theme_bw() +
           ggtitle('Total Number of Speed Camera Violations per Month in Chicago (from 2015-Present)') +
           ylab("Total number of violations") +
           coord_cartesian(ylim = c(200000,425000))
        )



rownames(redv_year) <- redv_year$year
redv_year['2018','sum_year_v']


rownames(speedv_year) <- speedv_year$year
speedv_year['2018','sum_year_v']




speed_loc <- read_csv('speed-camera-locations.csv')

speedl <- speed_loc %>% clean_names() %>%
      select('latitude', 'longitude','address','go_live_date')

speedl$year = as.factor(year(as.Date(speedl$go_live_date, format = "%m/%d/%Y")))


speed_cam_map <- leaflet() %>% 
      addTiles() %>%
      addMarkers(lat = filter(speedl, year == '2014')$latitude, 
                 lng = filter(speedl, year == '2014')$longitude, 
                 popup = filter(speedl, year == '2014')$address,
                 group = '2014') %>%
      addMarkers(lat = filter(speedl, year == '2015')$latitude, 
                 lng = filter(speedl, year == '2015')$longitude, 
                 popup = filter(speedl, year == '2015')$address,
                 group = '2015') %>%
      addMarkers(lat = filter(speedl, year == '2013')$latitude, 
                 lng = filter(speedl, year == '2013')$longitude, 
                 popup = filter(speedl, year == '2013')$address,
                 group = '2013') %>%
      addMarkers(lat = filter(speedl, year == '2018')$latitude, 
                 lng = filter(speedl, year == '2018')$longitude, 
                 popup = filter(speedl, year == '2018')$address,
                 group = '2018') %>%
      addLayersControl(
                      overlayGroups = c('2013','2014','2015','2018'),
                      options = layersControlOptions(collapsed = FALSE)
                      )
speed_cam_map

ggplotly(ggplot(speedl, aes(year)) + 
           geom_histogram(stat='count', color = 'red') +
           theme_bw() +
           ggtitle('Number of Speed Cameras Deployed per Year in Chicago') +
           ylab('Number of Speed Cameras Deployed')
        )





red_loc <- read_csv('red-light-camera-locations.csv')

redl <- red_loc %>% clean_names() %>%
  select('latitude', 'longitude','intersection','go_live_date') %>%
  mutate(year = as.factor(year(go_live_date)))


red_cam_map <- leaflet() %>% 
      addTiles() %>%
      addMarkers(lat = filter(redl, year == '2003')$latitude, 
                 lng = filter(redl, year == '2003')$longitude, 
                 popup = filter(redl, year == '2003')$intersection,
                 group = '2003') %>%
      addMarkers(lat = filter(redl, year == '2004')$latitude, 
                 lng = filter(redl, year == '2004')$longitude, 
                 popup = filter(redl, year == '2004')$intersection,
                 group = '2004') %>%
      addMarkers(lat = filter(redl, year == '2006')$latitude, 
                 lng = filter(redl, year == '2006')$longitude, 
                 popup = filter(redl, year == '2006')$intersection,
                 group = '2006') %>%
      addMarkers(lat = filter(redl, year == '2017')$latitude, 
                 lng = filter(redl, year == '2017')$longitude, 
                 popup = filter(redl, year == '2017')$intersection,
                 group = '2017') %>%
      addMarkers(lat = filter(redl, year == '2018')$latitude, 
                 lng = filter(redl, year == '2018')$longitude, 
                 popup = filter(redl, year == '2018')$intersection,
                 group = '2018') %>%
      addMarkers(lat = filter(redl, year == '2007')$latitude, 
                 lng = filter(redl, year == '2007')$longitude, 
                 popup = filter(redl, year == '2007')$intersection,
                 group = '2007') %>%
      addMarkers(lat = filter(redl, year == '2008')$latitude, 
                 lng = filter(redl, year == '2008')$longitude, 
                 popup = filter(redl, year == '2008')$intersection,
                 group = '2008') %>%
      addMarkers(lat = filter(redl, year == '2009')$latitude, 
                 lng = filter(redl, year == '2009')$longitude, 
                 popup = filter(redl, year == '2009')$intersection,
                 group = '2009') %>%
      addMarkers(lat = filter(redl, year == '2010')$latitude, 
                 lng = filter(redl, year == '2010')$longitude, 
                 popup = filter(redl, year == '2010')$intersection,
                 group = '2010') %>%
      addMarkers(lat = filter(redl, year == '2011')$latitude, 
                 lng = filter(redl, year == '2011')$longitude, 
                 popup = filter(redl, year == '2011')$intersection,
                 group = '2011') %>%
      addLayersControl(
        overlayGroups = c('2003','2004','2006', '2007', '2008', '2009', '2010', '2011', '2017','2018'),
        options = layersControlOptions(collapsed = FALSE)
      )


red_cam_map

ggplotly(ggplot(redl, aes(year)) + 
          geom_histogram(stat='count', color = 'blue') +
          theme_bw() +
          ggtitle('Number of Red Light Cameras Deployed per Year in Chicago') +
          ylab('Number of Red Light Cameras Deployed')
        )