library(nycflights13)
library(dplyr)
library(ggiraph)
View(flights)
View(airports)
visualize_airport_delays <- function(){
  trimmed_flights <- dplyr::distinct(select(flights, origin, dest))
  return(trimmed_flights)
}
visualize_airport_delays()

#Removes rows with NA values based on column names
flights <- flights[!is.na(flights$dep_delay), ]
flights <- flights[!is.na(flights$arr_delay), ]

#Calculating mean delay of destination airports
flights_destination <-  flights %>%
  dplyr::group_by(flights$dest) %>%
  summarise(mean_delay = mean(arr_delay+dep_delay))
final_fl
View(flights_destination)

#Calculating mean delay of origin airports
flights_origin <-  flights %>%
  dplyr::group_by(flights$origin) %>%
  summarise(mean_delay = mean(dep_delay))
View(flights_origin)

#Changing names of coulmns of respective data frame
colnames(flights_destination) <- c('Airports', 'Mean Delay')
colnames(flights_origin) <- c('Airports', 'Mean Delay')

#binding rows of origin airports data frame with destination airports data frame
flights_ori_dest <- flights_destination %>%
  dplyr::bind_rows(flights_origin)
View(flights_ori_dest)

#Joining the Airport with mean delays data frame with Airports data frame containing
#longitude and latitude information
flights_ori_dest <- airports %>%
  dplyr::left_join(flights_ori_dest, airports$lat, by = c('faa' = 'Airports'))
View(flights_ori_dest)

#dropping unnecessary columns from data frame
flights_ori_dest <- flights_ori_dest[!is.na(flights_ori_dest$'Mean Delay'), ]
flights_ori_dest <- dplyr::select(flights_ori_dest, -c(5,6,7,8))
View(flights_ori_dest)

#creating a data frame of values for hover-box text
faa_data = flights_ori_dest$faa
meandelay_flight <- flights_ori_dest$`Mean Delay`
tooltip_df <- data.frame(faa_data,meandelay_flight)

tooltip_df$tooltip <- c(paste0("Airport : ", tooltip_df$faa_data,
                               "\n Mean Delay : ", tooltip_df$meandelay_flight))

#plotting longitude vs latitude for mean delay each airport
#Note: Hover over each point in the plot to find Airport faa code and mean delay
plot_delay <- ggplot(data = flights_ori_dest)+
  geom_point_interactive(aes(x = lon, y = lat, colour = faa,
                             tooltip =tooltip_df$tooltip ,
                             data_id = tooltip_df$tooltip))+
  theme(legend.position = 'None',
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'))+
  xlab('Longitude')+
  ylab('Latitude')+
  ggtitle('Longitude vs. Latitude', subtitle = 'Visualizing Mean Delay of Airports')

girafe(ggobj = plot_delay)

