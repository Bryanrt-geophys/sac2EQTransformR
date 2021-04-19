#' file_paths
#'
#' @param training_files character list of the names of file types being used to provide training data
#' @param dat_path path to the root directory for all of the files being pulled for creation of the training model
#' @param station_list_path path to the station list that was created by EQTransformer
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
file_paths <- function(training_files, dat_path, station_list_path){

  progressr::with_progress({
    p <- progressr::progressor(steps = (length(training_files) + 2))


    paths <- furrr::future_map(training_files, function(x){
      p()
      Sys.sleep(.2)

      if (x == "picfil") {
        fs::dir_ls(path =  dat_path,
                   recurse = TRUE,
                   all = TRUE,
                   regexp = sprintf("%s*", x)
        ) %>%
          stringr::str_subset(pattern = "bak$", negate = TRUE) %>%
          stringr::str_subset(pattern = "temp*", negate = TRUE)
      } else {
        fs::dir_ls(path = if (x == "json") {
          station_list_path
        } else {
          dat_path
        },
        recurse = TRUE,
        all = TRUE,
        regexp = sprintf("%s*", x)
        )
      }

    })

    gc()
    p()
    Sys.sleep(.2)
    names(paths) <- training_files
    p()
    Sys.sleep(.2)
    paths
  })
}
