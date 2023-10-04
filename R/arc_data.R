#' Copy files from data directory to archive directory
#'
#' @param ... arguments passed to fs::dir_info() i.e. regexp, glob
#'
#' @param asmbdat_path path to projects asmbdat directory. Defaults to working
#'   directory
#'
#' @param archive_path path to projects archive directory. Will create a new
#'   archive directory if one doesn't exists as well as dated folders inside
#'   archive directory. Dated archive directory defaults to System Date
#'
#' @return files moved to data archive directory.
#'
#' @export


arc_data <- function(asmbdat_path=getwd(),archive_path=NULL,...){
  
  
  # check if asmbdat_path is valid
  if (!file.exists(asmbdat_path)) {
    cli::cli_abort(message="{.arg {asmbdat_path}} is not a valid path")
  }
  
  # check if asmbdat_path is to asmbdat directory
  if (asmbdat_path==getwd() && !grepl("asmbdat",getwd())) {
    cli::cli_abort(message="{.file {asmbdat_path}} set your wd to asmbdat directory,
 or provide a valid path directly to the projects data directory")
  }
  
  
  # If working in asmbdat get data directory
  if (basename(asmbdat_path)=="asmbdat") {
    data_dir <- paste0(dirname(asmbdat_path),"/data")
  } else {
    data_dir <- asmbdat_path
  }
  
  # is data directory actually "data" directory
  if (!basename(data_dir)=="data" ) {
    cli::cli_abort(message="{.file {data_dir}} is not a valid data directory")
  } else{
    
    data_dir_info <- fs::dir_info(data_dir,
                                  type="file",...) 
    
    cli::cli_alert_info("data from {.file {data_dir}} to archive")  
    cli::cat_print(suppressMessages(data_dir_info %>% 
                                      transmute(File_extentions=tools::file_ext(path))%>%
                                      group_by(File_extentions)%>%
                                      tally())
    )
  }
  
  if(nrow(data_dir_info)==0){
    cli::cli_abort(message="No files found meeting conditions")
    
  }
  
  # archive directory
  new_path <- paste0(data_dir,"/archive/")
  
  
  # check if archive exists. Create archive in data directory if it doesn't yet exist
  if (!file.exists(new_path)) {
    cli::cli_alert_info("created archive directory{.file {new_path}}")
    fs::dir_create(new_path) 
  }
  
  
  folder_date <- lubridate::date(Sys.Date())
  
  # Default to creating a sys date folder
  if (is.null(archive_path)) {
    to_path <- paste0(new_path,folder_date)
  } else{
    to_path <- archive_path
  }
  
  
  
  # Force archive to save to the archive directory 
  if(!is.null(archive_path) && !basename(dirname(to_path))=="archive"){
    cli::cli_abort(message=c(
      "{.file {to_path}} is not a valid archive directory",
      "1. Set {.arg archive_path} NULL to archive to new directory or",
      "2. Set {.arg archive_path} to an existing archive directory e.g. 2023-10-27"))
  }
  
  # Are to and from the same project?
  if (!utilscognigen::path_project(path = dirname(to_path))== utilscognigen::path_project(path = data_dir)){
    cli::cli_abort(message="{.file {to_path}} and {.file {data_dir}} 
       are different projects!")
  } 
  
  # check if the provided archive_path can be parsed as ymd by lubridate
  if (!is.null(archive_path) && !file.exists(to_path) && is.na(suppressWarnings(lubridate::parse_date_time(basename(to_path),orders = "ymd")))) {
    cli::cli_alert_warning(message=c("The new directory name {.file {basename(to_path)}}",
                                     "should be a date formatted as year-month-day e.g. 2023-10-27"))
  } 
  
  
  if (!file.exists(to_path)) {
    fs::dir_create(to_path)
    cli::cli_alert_success("You created new directory {.file {to_path}}")
  }
  
  
  
  data_dir_info %>%
    purrr::pull(path) #%>%
    purrr::walk( ~ utilscognigen::file_copy(from = .x, to = to_path))
  
  
  cli::cli_alert_success(
    "You archived {.value {nrow(fs::dir_info(to_path))}} files to \n{.file {to_path}}")  
  
  
}
