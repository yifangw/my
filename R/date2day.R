#' Date2Day function
#'
#' This function allows you to calculate the day based on date.
#' The formula used in the function is kim Larsson calculation formula.
#' 
#' @param year Enter the year in an int format, eg. 1989
#' @param month Enter the month in an int format, eg. 8
#' @param day Enter the day in an int format, eg. 25
#' @keywords Date2Day
#' @export
#' @examples 
#' date2day_fun(2018, 5, 28)
#' 
date2day_fun <- function(year, month, day){
  y <- year
  m <- month
  if (month == 1){
    m <- 13
    y <- year - 1
  }
  if (month == 2){
    m <- 14
    y <- year -1
  }
  w <- ceiling((day + 2*m + 3*(m + 1)/5 + y + y/4 - y/100 + y/400) %% 7)
  if (w == 0){
    print(paste0(year,"-",month,"-", day," is : ", "Sunday"))
  }
  if (w == 1){
    print(paste0(year,"-",month,"-", day," is : ", "Monday"))
  }
  if (w == 2){
    print(paste0(year,"-",month,"-", day," is : ", "Tuesday"))
  }
  if (w == 3){
    print(paste0(year,"-",month,"-", day," is : ", "Wednesday"))
  }
  if (w == 4){
    print(paste0(year,"-",month,"-", day," is : ", "Thursday"))
  }
  if (w == 5){
    print(paste0(year,"-",month,"-", day," is : ", "Friday"))
  }
  if (w == 6){
    print(paste0(year,"-",month,"-", day," is : ", "Saturday"))
  }
}



