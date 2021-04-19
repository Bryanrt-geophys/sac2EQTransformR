#' header_data
#'
#' @param sac_path path to the directory where you plan to store the SAC files
#' @param wrangled_SEISMOS output from wrangle_SEISMOS
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
header_data <- function(sac_path, wrangled_SEISMOS){



    stations <- unique(wrangled_SEISMOS$reciever_code)

    rm(wrangled_SEISMOS)

    progressr::with_progress({
      p <- progressr::progressor(steps = (length(stations) + 6))

    sac_files <- furrr::future_map(stations, function(x){
      p()
      Sys.sleep(.2)
      fs::dir_ls(path =  sac_path,
                 recurse = TRUE,
                 all = TRUE,
                 regexp = sprintf("%s", x))  %>%
        .[-1] %>%
        stringr::str_subset(pattern = "DS_Store", negate = TRUE)
    })

    names(sac_files) <- stations

    sac_files_trimmed <- purrr::discard(sac_files, ~ length(.x) == 0)
    p()
    Sys.sleep(.2)
    rm(sac_files)

    possibly_read <- purrr::possibly(RSEIS::read1sac, otherwise = NA)
    p()
    Sys.sleep(.2)
    sac_headers <- furrr::future_map(sac_files_trimmed, function(x){

      furrr::future_map(x, possibly_read) %>%
        furrr::future_map(1)

    })
    p()
    Sys.sleep(.2)
    header_data <- furrr::future_map(sac_headers, function(x){


      furrr::future_imap(x, function(z, y){
        z <- z %>% as.data.frame()

        z %>% dplyr::summarize(
          sampling_rate = 1/z$delta,
          points = z$npts,
          start_time = (z$nzhour * 60 * 60) + (z$nzmin * 60) + (z$nzsec) + (z$nzmsec/1000),
          end_time = z$npts/sampling_rate + start_time,
          day = z$nzjday,
          year = z$nzyear,
          channel = stringr::str_trim(string = z$kcmpnm, side = "both"),
          network = stringr::str_trim(string = z$knetwk, side = "both")
          # SAC = stringr::str_trim(string = basename(y), side = "both")
        )
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(day) %>%
        dplyr::mutate(
          merge_time_dif = dplyr::if_else(condition = start_time != dplyr::lag(end_time), true = start_time - dplyr::lag(end_time), false = 0),
          start_time_correction = dplyr::if_else(condition = start_time < 1 , true = start_time - 0, false = 0)
        ) %>%
        dplyr::ungroup()
    })
    p()
    Sys.sleep(.2)
    rm(sac_headers)

    sac_data <- furrr::future_map(sac_files_trimmed, function(x){

      furrr::future_map(x, possibly_read) %>%
        furrr::future_map(2)

    })
    p()
    Sys.sleep(.2)
    rm(sac_files_trimmed)

    # error appears to be in map_dfr() passing x to rep(), x seems to not have names
    # preserved so I don't know if the variables defined in pmap() hold any values
    # passed to rep(). If they are empty then this will likely trigger the invalid
    # times arguement in rep()
    for (i in 1:length(header_data)) {
      header_data[[i]]$sac <- sac_data[[i]]

      header_data[[i]] <- header_data[[i]] %>%
        dplyr::mutate(
          lost_recordings = sampling_rate * merge_time_dif,
          lost_recordings = dplyr::if_else(is.na(lost_recordings), 0, lost_recordings),
          lost_recordings_beg = sampling_rate * start_time_correction
        ) %>%

        dplyr::group_split(day) %>%

        furrr::future_map_dfr(function(x){
          x %>%
            dplyr::mutate(
              sac = furrr::future_pmap(list(lost_beg = lost_recordings_beg, lost = lost_recordings, sac = sac), function(lost_beg, lost, sac){
                c(
                  rep(0, length(floor(lost_beg))),
                  rep(0, length(floor(lost))),
                  sac
                )
              })
            )
        })
    }
    p()
    Sys.sleep(.2)
    rm(sac_data)

    header_data
  })
}
