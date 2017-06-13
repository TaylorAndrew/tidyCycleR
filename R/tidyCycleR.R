library(dplyr)
library(anytime)
library(cycleRtools)
#' read_rides
#'
#' read_rides will read a list of ride data and bind them into a single rides tibble
#'
#' @param file_list list of .tcx, .fit, .pwx, or .srm files
#'
#' @return tibble with all ride data
#' @export
#'
#' @examples
#' ## No run
#' # ride_list <- c('ride1.tcx', 'ride2.tcx')
#' # rides <- read_rides(ride_list)
read_rides <- function(file_list) {
    extensions <- substr(file_list, nchar(file_list)-3, nchar(file_list))
    if(sum(!extensions%in%c('.tcx', '.srm', '.pwx', '.fit')>0)) return(print('Not all files are .tcx. Only .tcx files are supported'))
    get_one_file <- function(file) {
        ride = strsplit(file, '\\/')
        ride = gsub('\\.tcx', '', ride[[1]][length(ride[[1]])])
        mydf <- cycleRtools::read_ride(file)
        mydf <- tibble::as_tibble(data.frame(ride = ride, mydf))
        return(mydf)
    }
    rides <- do.call(rbind, lapply(file_list, get_one_file))
    class(rides) <- c(class(rides), 'rides_df')
    return(rides)
}

#' remove_pause_data
#'
#' @param data rides tibble
#'
#' @return filtered tibble with all paused time removed
#' @export
#'
#' @examples
#' ## No run
#' # active_ride_df <- remove_pause_data(rides)
remove_pause_data <- function(data) {
    data <-
        data %>%
        group_by(ride) %>%
        mutate(delta.distance = distance.km - lag(distance.km)) %>%
        filter(delta.distance!=0) %>%
        select(-delta.distance)
    class(data) <- c(class(data), 'rides_df')
    data
}

#' by_mile
#'
#' by_mile summarizes all rides data information into a tidy per mile summary tibble
#'
#' @param data rides tibble
#'
#' @return tibble of summarized rides
#' @export
#'
#' @examples
#' ## No Run
#' by_mile(rides)
by_mile <- function(data) {
    if(!'rides_df'%in%class(data)) return('Must use a rides_df produced via read_rides')
    data$mile <- floor(as.numeric(as.character(data$distance.km)) * 0.621371)
    data %>%
        group_by(ride, mile) %>%
        mutate(pace_minutes = as.numeric(timer.min[row_number()==length(timer.min)]) - (timer.min[row_number()==1]),
               average_mph = (1 / (as.numeric(pace_minutes)/60)),
               alt = as.numeric(as.character(elevation.m)) * 3.28084,
               total_elevation_change_feet = (alt[row_number()==length(alt)] - alt[row_number()==1]),
               instantaneous_alt_change = c(0, diff(alt)),
               elevation_gain_feet = sum(instantaneous_alt_change[instantaneous_alt_change>0]),
               elevation_loss_feet = sum(instantaneous_alt_change[instantaneous_alt_change<0])) %>%
        filter(row_number()==length(pace_minutes)) %>%
        ungroup() %>%
        group_by(ride) %>%
        mutate(cum_average_mph = cummean(average_mph),
               cum_elevation_gain_feet = cumsum(elevation_gain_feet),
               cum_elevation_loss_feet = cumsum(elevation_loss_feet),
               cum_total_elevation_change_feet = cumsum(total_elevation_change_feet),
               cum_pace_minutes = cummean(pace_minutes)) %>%
        select(ride, mile, pace_minutes, cum_pace_minutes,
               average_mph, cum_average_mph,
               elevation_gain_feet, cum_elevation_gain_feet,
               elevation_loss_feet, cum_elevation_loss_feet,
               total_elevation_change_feet, cum_total_elevation_change_feet)
}

#' by_kilometer
#'
#' by_kilometer summarizes all rides data information into a tidy per kilometer summary tibble
#'
#' @param data rides tibble
#'
#' @return tibble of summarized rides
#' @export
#'
#' @examples
#' ## No Run
#' by_kilometer(rides)
by_kilometer <- function(data) {
    if(!'rides_df'%in%class(data)) return('Must use a rides_df produced via read_rides')
    data$kilometer <- floor(as.numeric(as.character(data$distance.km)))
    data %>%
        group_by(ride, kilometer) %>%
        mutate(pace_minutes = as.numeric(timer.min[row_number()==length(timer.min)]) - as.numeric(timer.min[row_number()==1]),
               average_kph = (1 / (as.numeric(pace_minutes)/60)),
               alt = as.numeric(as.character(elevation.m)),
               total_elevation_change_meters = (alt[row_number()==length(alt)] - alt[row_number()==1]),
               instantaneous_alt_change = c(0, diff(alt)),
               elevation_gain_meters = sum(instantaneous_alt_change[instantaneous_alt_change>0]),
               elevation_loss_meters = sum(instantaneous_alt_change[instantaneous_alt_change<0])) %>%
        filter(row_number()==length(pace_minutes)) %>%
        ungroup() %>%
        group_by(ride) %>%
        mutate(cum_average_kph = cummean(average_kph),
               cum_elevation_gain_meters = cumsum(elevation_gain_meters),
               cum_elevation_loss_meters = cumsum(elevation_loss_meters),
               cum_total_elevation_change_meters = cumsum(total_elevation_change_meters),
               cum_pace_minutes = cummean(pace_minutes)) %>%
        select(ride, kilometer, pace_minutes, cum_pace_minutes,
               average_kph, cum_average_kph,
               elevation_gain_meters, cum_elevation_gain_meters,
               elevation_loss_meters, cum_elevation_loss_meters,
               total_elevation_change_meters, cum_total_elevation_change_meters)
}

#' ride_summary
#'
#' ride_summary provides overall summaries of rides
#'
#' @param data rides tibble
#' @param unit either 'imperial' or 'metric'
#'
#' @return tibble
#' @export
#'
#' @examples
#' ## No run
#' # ride_summary(rides, 'metric')
ride_summary <- function(data, unit = 'metric') {
    if(!'rides_df'%in%class(data)) return('Must use a rides_df produced via read_rides')
    if(unit == 'metric') {
    data %>%
        group_by(ride) %>%
        mutate(next_distance = lag(distance.km),
               is_active = ifelse(distance.km == next_distance, 0, 1),
               delta.distance = distance.km - lag(distance.km),
               kph = delta.distance/(delta.t/60/60),
               kph = ifelse(is.infinite(kph) | kph == 0, NA, kph),
               pace = (delta.t/60)/ delta.distance,
               pace = ifelse(is.infinite(pace) | pace == 0, NA, pace)) %>%
        summarise(start_time = min(anytime(timestamp)),
               end_time = max(anytime(timestamp)),
               active_time_minutes = sum(delta.t[is_active==1], na.rm=T)/60,
               pause_time_minutes = sum(delta.t[is_active==0], na.rm=T)/60,
               total_time_minutes = sum(delta.t/60),
               start_elevation_meters = elevation.m[1],
               end_elevation_meters = elevation.m[length(elevation.m)],
               max_elevation_meters = max(elevation.m),
               min_elevation_meters = min(elevation.m),
               total_elevation_change_meters = elevation.m[1] - elevation.m[length(elevation.m)],
               total_elevation_gain_meters = sum(elevation.m[delta.elev>0]),
               total_elevation_loss_meters = sum(elevation.m[delta.elev<0]),
               accent_time_minutes = sum(delta.t[delta.elev>0])/60,
               descent_time_minutes = sum(delta.t[delta.elev<0])/60 ,
               accent_distance_kilometers = sum(delta.distance[delta.elev>0], na.rm = T),
               descent_distance_kilometers = sum(delta.distance[delta.elev<0], na.rm = T),
               total_distance_kilometers = max(distance.km),
               min_pace = min(pace, na.rm = T),
               max_pace = max(pace, na.rm = T),
               avg_pace = as.numeric(timer.min[row_number()==length(timer.min)]) - as.numeric(timer.min[row_number()==1])/max(distance.km),
               min_kph = min(kph, na.rm = T),
               max_kph = max(kph, na.rm = T),
               avg_kph = max(distance.km) / (max(timer.min)/60),
               avg_accent_pace = sum(delta.t[delta.elev>0]/60) / sum(delta.distance[delta.elev>0]),
               avg_descent_pace = sum((delta.t[delta.elev<0]/60)) / sum(delta.distance[delta.elev<0]),
               avg_accent_kph = sum(delta.distance[delta.elev>0]) / sum((delta.t[delta.elev>0]/60/60)),
               avg_descent_kph = sum(delta.distance[delta.elev<0]) / sum((delta.t[delta.elev>0]/60/60))) %>%
        select(ride, start_time, end_time, active_time_minutes,
               pause_time_minutes, total_time_minutes, start_elevation_meters,
               end_elevation_meters, max_elevation_meters, min_elevation_meters,
               total_elevation_change_meters, total_elevation_gain_meters,
               total_elevation_loss_meters, accent_time_minutes, descent_time_minutes,
               accent_distance_kilometers, descent_distance_kilometers,
               total_distance_kilometers, min_pace, max_pace, avg_pace,
               min_kph, max_kph, avg_kph, avg_accent_pace, avg_descent_pace,
               avg_accent_kph, avg_descent_kph)
  } else if(unit == 'imperial') {
    data %>%
        group_by(ride) %>%
        mutate(next_distance = lag(distance.km),
               is_active = ifelse(distance.km == next_distance, 0, 1),
               delta.distance = (distance.km - lag(distance.km)) * 0.621371,
               mph = delta.distance/(delta.t/60/60),
               mph = ifelse(is.infinite(mph) | mph == 0, NA, mph),
               pace = (delta.t/60)/ delta.distance,
               pace = ifelse(is.infinite(pace) | pace == 0, NA, pace)) %>%
        summarise(start_time = min(anytime(timestamp)),
               end_time = max(anytime(timestamp)),
               active_time_minutes = sum(delta.t[is_active==1], na.rm=T)/60,
               pause_time_minutes = sum(delta.t[is_active==0], na.rm=T)/60,
               total_time_minutes = sum(delta.t/60),
               start_elevation_feet = elevation.m[1] * 3.28084,
               end_elevation_feet = elevation.m[length(elevation.m)] * 3.28084,
               max_elevation_feet = max(elevation.m) * 3.28084,
               min_elevation_feet = min(elevation.m) * 3.28084,
               total_elevation_change_feet = (elevation.m[1] - elevation.m[length(elevation.m)]) * 3.28084,
               total_elevation_gain_feet = sum(elevation.m[delta.elev>0]) * 3.28084,
               total_elevation_loss_feet = sum(elevation.m[delta.elev<0]) * 3.28084,
               accent_time_minutes = sum(delta.t[delta.elev>0])/60,
               descent_time_minutes = sum(delta.t[delta.elev<0])/60 ,
               accent_distance_miles = sum(delta.distance[delta.elev>0], na.rm = T),
               descent_distance_miles = sum(delta.distance[delta.elev<0], na.rm = T),
               total_distance_miles = max(distance.km) * 0.621371,
               min_pace = min(pace, na.rm = T),
               max_pace = max(pace, na.rm = T),
               avg_pace = as.numeric(timer.min[row_number()==length(timer.min)]) - as.numeric(timer.min[row_number()==1])/(max(distance.km) * 3.28084),
               min_mph = min(mph, na.rm = T),
               max_mph = max(mph, na.rm = T),
               avg_mph = (max(distance.km) * 3.28084) / (max(timer.min)/60),
               avg_accent_pace = sum(delta.t[delta.elev>0]/60) / sum(delta.distance[delta.elev>0]),
               avg_descent_pace = sum((delta.t[delta.elev<0]/60)) / sum(delta.distance[delta.elev<0]),
               avg_accent_mph = sum(delta.distance[delta.elev>0]) / sum((delta.t[delta.elev>0]/60/60)),
               avg_descent_mph = sum(delta.distance[delta.elev<0]) / sum((delta.t[delta.elev>0]/60/60))) %>%
        select(ride, start_time, end_time, active_time_minutes,
               pause_time_minutes, total_time_minutes, start_elevation_feet,
               end_elevation_feet, max_elevation_feet, min_elevation_feet,
               total_elevation_change_feet, total_elevation_gain_feet,
               total_elevation_loss_feet, accent_time_minutes, descent_time_minutes,
               accent_distance_miles, descent_distance_miles,
               total_distance_miles, min_pace, max_pace, avg_pace,
               min_mph, max_mph, avg_mph, avg_accent_pace, avg_descent_pace,
               avg_accent_mph, avg_descent_mph)
  } else return(print('unit must be either metric or imperial'))
}


# file_list <- paste0('C:/Users/ataylor9/Downloads/',
#                     c('03_24_17.tcx',
#                       '04_26_17.tcx'))
# rides <- read_ride_data(file_list)
#
#
# kilo_df <- by_kilometer(rides)
# mile_df <- by_mile(rides)
# summaries <- ride_summary(rides)
# summaries_imperial <- ride_summary(rides, 'imperial')
# # Accessing API Calls:
# library(httr)
# library(jsonlite)
#
# token <- GET(url = 'https://ridewithgps.com/api',
#              path = '/users/current.json?email={taylor.andrew.r@gmail.com}&password={eu7m8EHZl0%Ihn}&apikey=testkey1&version=2')
# url  <- "https://ridewithgps.com/api"
# path <- "/USERS/current.JSON"
# raw.result <- GET(url = 'https://ridewithgps.com/api/users/current.json?auth_token={3d699ed410b9ebc91538884af32d0605}')
