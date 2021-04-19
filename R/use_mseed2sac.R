#' use_mseed2sac
#'
#' @param mseed_path root path where the mseed files where downloaded from EQTransformer
#' @param sac_path path to the directory where you plan to store the SAC files
#' @param exicutable_path path to the directory that contains the mseed2sac executable from IRIS
#' @param wrangled_SEISMOS output from wrangle_SEISMOS
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
use_mseed2sac <- function(mseed_path, sac_path, executable_path, wrangled_SEISMOS) {

  stations <- unique(wrangled_SEISMOS$reciever_code)
  rm(wrangled_SEISMOS)

  progressr::with_progress({
    p <- progressr::progressor(steps = (length(stations) + 1))

    # command doesn't seem to work on windows. look into
    system(command = sprintf("cp %s %s", executable_path, here::here()))
    p()
    Sys.sleep(.2)

    furrr::future_walk(stations, function(x){
      p()
      Sys.sleep(.2)
      station_path <- fs::dir_ls(path =  mseed_path,
                                 recurse = TRUE,
                                 all = TRUE,
                                 regexp = sprintf("%s.0*", x)
      ) %>%
        stringr::str_subset(pattern = "SAC$", negate = TRUE) %>%
        stringr::str_subset(pattern = ".DS_Store", negate = TRUE)

      if (length(station_path) > 0) {
        fs::dir_create(path = sac_path, x)
        sac_dir_paths <- sprintf("%s/%s",sac_path,x)
      }

      if (length(station_path) > 0) {
        for (i in 1:length(station_path)) {
          # gc()
          system(command = sprintf("./mseed2sac %s", station_path[i]))
          local_sac <- fs::dir_ls(path =  here::here(),
                                  recurse = FALSE,
                                  all = TRUE,
                                  regexp = print(".SAC$"))
          for (i in 1:length(local_sac)) {
            system(command = sprintf("mv %s %s", local_sac[i], sac_dir_paths))
          }
        }
      }
    })
  })
  rm(mseed_path)
  rm(sac_path)
  rm(executable_path)
}
