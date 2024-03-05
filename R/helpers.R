save_object <- function(object) {
   # Capture the name of the object
   object_name <- deparse(substitute(object))
   
   # Create the directory path with the current date
   dir_path <- here("data", today(), object_name)
   
   # Create the directory if it doesn't exist
   if (!dir.exists(dirname(dir_path))) {
      dir.create(dirname(dir_path), recursive = TRUE)
   }
   
   # Construct the file name with the object name
   # file_name <- paste0(dir_path, ".rds")
   
   # Save the object using write_rds
   write_rds(object, dir_path)
}

get_utc_month_range <- function(year, month) {
   # Define the start and end times in local time (Eastern Time)
   start_local <- ymd_hms(
      paste(year, month, "01 00:00:00", sep = "-"), 
      tz = "America/New_York"
   )
   end_local <- ymd_hms(
      paste(
         year, 
         month, 
         days_in_month(as.Date(paste(year, month, "01", sep = "-"))), 
         "23:59:59", sep = "-"
      ), 
      tz = "America/New_York"
   )
   
   # Convert the local times to UTC
   start_utc <- with_tz(start_local, "UTC")
   end_utc <- with_tz(end_local, "UTC")
   
   # Assign the start and end times in UTC to the global environment
   assign("start_time", format(start_utc, "%Y-%m-%dT%H:%M:%SZ"), envir = .GlobalEnv)
   assign("end_time", format(end_utc, "%Y-%m-%dT%H:%M:%SZ"), envir = .GlobalEnv)
}

format_custom <- function(x) {
   sapply(x, function(num) {
      if (num >= 1e6) {
         paste0(format(round(num / 1e6, 1), nsmall = 1, big.mark = ","), "M")
      } else {
         format(round(num), big.mark = ",")
      }
   })
}

round_to_nearest_thousand <- function(x) {
   ifelse(x > 1000, round(x / 1000) * 1000, x)
}
