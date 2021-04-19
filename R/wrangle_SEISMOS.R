#' wrangle_SEISMOS
#'
#' @param file_paths list of paths to the dat files used for training
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
wrangle_SEISMOS <- function(file_paths){

  progressr::with_progress({
    p <- progressr::progressor(steps = 15)

    # raw name of each event in file_data
    events <- sort(unique(basename(dirname(file_paths[["picfil"]]))))
    # all stations available to pick up events in file_data
    json_data <- jsonlite::fromJSON(file_paths[["json"]])
    stations <- names(json_data)
    p()
    Sys.sleep(.2)
    # all the attribute data for each event, stations data embedded
    file_data <- furrr::future_map(file_paths, function(y){
      furrr::future_map(events, ~ stringr::str_subset(string = y, pattern = .x)) %>%
        furrr::future_map(~ furrr::future_map(.x, readr::read_lines))
    })
    p()
    Sys.sleep(.2)
    file_data[["picfil"]] <- furrr::future_map(file_data[["picfil"]], function(x){
      dplyr::if_else(condition = (length(x) == 1), true = x[1], false = x[1])
    })
    p()
    Sys.sleep(.2)
    wave_ps_coda <- furrr::future_map2(file_data[["picfil"]] , events, function(x, y){
      x %>%
        unlist() %>%
        .[-1] %>%
        stringr::str_trim() %>%
        stringr::str_split(pattern = "\\s+") %>%
        furrr::future_map(~ as.data.frame(t(.x))) %>%
        dplyr::bind_rows() %>%
        stats::setNames(c("type", "reciever_code", "uncertanty_multiply_quarter_sec", "obs_p_arrival", "sec")) %>%
        dplyr::mutate(
          dplyr::across(.cols = c(type, uncertanty_multiply_quarter_sec, obs_p_arrival), .fns = as.numeric),
          event = y
        )
    })
    p()
    Sys.sleep(.2)
    network_data <- furrr::future_imap_dfr(json_data, function(x, y){
      x[-2] %>%
        unlist() %>%
        t() %>%
        tibble::as_tibble() %>%
        stats::setNames(c("network_code",
                          "reciever_latitude",
                          "reciever_longitude",
                          "reciever_elevation_m")) %>%
        dplyr::mutate(
          dplyr::across(c(reciever_latitude, reciever_longitude, reciever_elevation_m), as.numeric),
          reciever_code = y
        )
    })
    p()
    Sys.sleep(.2)
    origin_data <- furrr::future_map2(file_data[["hypo71"]], events, function(x, y){

      # x <- file_data[[3]][[1]]
      # y = events[[1]]

      x %>% unlist() %>%
        .[3] %>%
        stringr::str_trim(side = "left") %>%
        stringr::str_remove(pattern = "\\**") %>%
        stringr::str_replace_all(pattern = "   ", " ") %>%
        stringr::str_replace_all(pattern = "  ", " ") %>%
        tibble::as_tibble() %>%
        tidyr::separate(col = value,
                        into = c("DATE",
                                 "Time",
                                 "ORIGIN",
                                 "LAT_deg",
                                 "LAT_min",
                                 "LONG_deg",
                                 "LONG_min",
                                 "DEPTH",
                                 "MAG",
                                 "NO_DM",
                                 "GAP_M",
                                 "RMS",
                                 "ERH",
                                 "ERZ"),
                        sep = " ")  %>%
        dplyr::mutate(dplyr::across(.fns = as.numeric),
                      event = y)
    })
    p()
    Sys.sleep(.2)
    hypo71_data <- furrr::future_map2(file_data[["hypo71"]], events, function(x, y){
      l <- x %>% unlist() %>%
        stringr::str_subset(paste(stations, collapse = "|")) %>%
        paste0(collapse = "\n")
      if (stringr::str_count(l,paste(stations, collapse = "|")) > 1) {
        l %>% readr::read_delim(delim = " ",
                                col_names = c("reciever_code",
                                              "DIST",
                                              "AZM",
                                              "AIN",
                                              "PRMK",
                                              "HRMN",
                                              "P_RES")) %>%
          dplyr::mutate(
            dplyr::across(c(DIST,AZM,AIN,HRMN,P_RES),as.double),
            event = y,
            reciever_code = stringr::str_trim(reciever_code,"left"))
      }
    })
    p()
    Sys.sleep(.2)
    wave_data <- furrr::future_map2(file_data[["refloc"]], events, function(x, y){
      x %>% unlist() %>%
        stringr::str_subset(paste(stations, collapse = "|")) %>%
        stringr::str_subset(pattern = paste(... = c("p", "s"),collapse =  "|")) %>%
        paste0(collapse="\n") %>%
        readr::read_delim(delim=" ",
                          col_names = c("reciever_code",
                                        "phase",
                                        "fm_qual",
                                        "wt",
                                        "delta",
                                        "azm",
                                        "ain",
                                        "obs_arv",
                                        "obs_trv",
                                        "theo_trv",
                                        "resid",
                                        "sta_cor")) %>%
        dplyr::mutate(
          dplyr::across(c(fm_qual,wt,delta,azm,ain,obs_arv,obs_trv,theo_trv, resid, sta_cor),as.double),
          event = y,
          reciever_code = stringr::str_trim(reciever_code,"left"),
          phase = stringr::str_trim(phase,"left"),
          type = dplyr::if_else(condition = phase == "p", true = 0, false = 1)
        ) %>%
        dplyr::select(-(phase))
    })
    p()
    Sys.sleep(.2)
    wave_ps_coda_df <- dplyr::bind_rows(wave_ps_coda) %>% tibble::as_tibble()
    wave_data_df <- dplyr::bind_rows(wave_data)
    hypo71_data_df <- dplyr::bind_rows(hypo71_data)
    origin_data_df <- dplyr::bind_rows(origin_data)
    p()
    Sys.sleep(.2)
    pre_stead <- dplyr::full_join(wave_ps_coda_df, wave_data_df, by = c("type", "event", "reciever_code")) %>%
      dplyr::full_join(hypo71_data_df, by = c("reciever_code", "event")) %>%
      dplyr::full_join(origin_data_df, by = c("event")) %>%
      dplyr::full_join(network_data, by = c("reciever_code")) %>%
      dplyr::arrange(event)
    p()
    Sys.sleep(.2)
    pre_stead$sec <- as.numeric(pre_stead$sec)
    pre_stead <- tidyr::pivot_longer(pre_stead, cols = c("uncertanty_multiply_quarter_sec",
                                                         "obs_p_arrival",
                                                         "sec",
                                                         "fm_qual",
                                                         "wt",
                                                         "delta",
                                                         "azm",
                                                         "ain",
                                                         "obs_arv",
                                                         "obs_trv",
                                                         "theo_trv",
                                                         "resid",
                                                         "sta_cor",
                                                         "DIST",
                                                         "AZM",
                                                         "AIN",
                                                         "HRMN",
                                                         "P_RES",
                                                         "DATE",
                                                         "Time",
                                                         "ORIGIN",
                                                         "LAT_deg",
                                                         "LAT_min",
                                                         "LONG_deg",
                                                         "LONG_min",
                                                         "DEPTH",
                                                         "MAG",
                                                         "NO_DM",
                                                         "GAP_M",
                                                         "RMS",
                                                         "ERH",
                                                         "ERZ",
                                                         "reciever_latitude",
                                                         "reciever_longitude",
                                                         "reciever_elevation_m"))
    pre_stead <- tidyr::pivot_wider(pre_stead, names_from = c("name", "type"), values_from = value)
    p()
    Sys.sleep(.2)
    pre_stead <- pre_stead[-c((ncol(pre_stead) -35):ncol(pre_stead),
                              41,
                              43,
                              45,
                              46,
                              51:76,
                              78:108)]
    names(pre_stead) <- c("reciever_code",
                          "event",
                          "PRMK",
                          "network_code",
                          "uncertanty_multiply_quarter_sec_0",
                          "obs_p_arrival",
                          "sec_0",
                          "fm_qual",
                          "wt_0",
                          "delta",
                          "azm",
                          "ain_0",
                          "obs_arv_0",
                          "obs_trv_0",
                          "theo_trv_0",
                          "resid_0",
                          "sta_cor_0",
                          "DIST",
                          "AZM",
                          "AIN",
                          "HRMN",
                          "P_RES",
                          "DATE",
                          "Time",
                          "ORIGIN",
                          "LAT_deg",
                          "LAT_min",
                          "LONG_deg",
                          "LONG_min",
                          "DEPTH",
                          "MAG",
                          "NO_DM",
                          "GAP_M",
                          "RMS",
                          "ERH",
                          "ERZ",
                          "reciever_latitude",
                          "reciever_longitude",
                          "reciever_elevation_m",
                          "uncertanty_multiply_quarter_sec_1",
                          "sec_1",
                          "wt_1",
                          "ain_1",
                          "obs_arv_1",
                          "obs_trv_1",
                          "theo_trv_1",
                          "sec_31")
    p()
    Sys.sleep(.2)
    # this would likely be done after the summery to be most efficient
    pre_stead <- pre_stead %>%
      dplyr::mutate(
        sec_add2split = sec_0,
        pre_split_points = (as.numeric(substr(x = event, start = 10, 11)) * 60 * 60) +
          (as.numeric(substr(x = event, start = 12, 13)) * 60) +
          (as.numeric(substr(x = event, start = 14, 15))),
        sac_split_point_s = pre_split_points + sec_add2split
      )
    p()
    Sys.sleep(.2)
    # get day for sac join
    pre_stead <- pre_stead %>%
      dplyr::mutate(
        date = substr(x = event, start = 1, 8) %>%
          as.Date(format = "%Y%m%d"),
        date_start = sprintf("%s%s%s", substr(x = event, start = 1, 4), "01", "01") %>%
          as.Date(format = "%Y%m%d"),
        day = as.numeric(date) - as.numeric(date_start) + 1
      )
    p()
    Sys.sleep(.2)
    # fix this summary table to left_merge with the sax data to cut segments
    pre_stead <- pre_stead %>%
      dplyr::mutate(
        source_latitude = LAT_deg + (LAT_min / 60),
        source_longitute = LONG_deg + (LONG_min / 60),
        source_depth = DEPTH
      )
    p()
    Sys.sleep(.2)
    pre_stead
  })
}
