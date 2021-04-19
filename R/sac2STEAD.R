#' sac2STEAD
#'
#' Cuts, resamples, and properly names the SAC data and signal labels
#'
#' @param sac2preStead Dataframe containing the data from the initial mseed files, converted to SAC, merged into 24 hr segments and data needed for merging with signal labels.
#' @param wrangle_SEISMOS tidy data from wrangle_SEISMOS
#'
#' @importFrom dplyr %>%
#'
#' @export

sac2STEAD <- function(sac2preStead, wrangle_SEISMOS){

  progressr::with_progress({
    p <- progressr::progressor(steps = (8))

    sac2preStead <- dplyr::bind_rows(sac2preStead)
    p()
    Sys.sleep(.2)

    pre_hdf5 <- dplyr::full_join(sac2preStead, wrangle_SEISMOS, by = c("reciever_code", "day")) %>%
      dplyr::mutate(
        sac_p_index = ((as.numeric(substr(x = event, start = 10, 11)) * 60 * 60) +
                         (as.numeric(substr(x = event, start = 12, 13)) * 60) +
                         (as.numeric(substr(x = event, start = 14, 15))) + sec_0 - 7) * sampling_rate,
        sac_s_index = ((as.numeric(substr(x = event, start = 10, 11)) * 60 * 60) +
                         (as.numeric(substr(x = event, start = 12, 13)) * 60) +
                         (as.numeric(substr(x = event, start = 14, 15))) + sec_1) * sampling_rate,
        sac_coda_index = ((as.numeric(substr(x = event, start = 10, 11)) * 60 * 60) +
                            (as.numeric(substr(x = event, start = 12, 13)) * 60) +
                            (as.numeric(substr(x = event, start = 14, 15))) + sec_31 + sec_0 - 7) * sampling_rate,
        sac_index_end = (sac_split_point_s - 7 + 60) * sampling_rate,
      ) %>%
      tidyr::drop_na()
    p()
    Sys.sleep(.2)

    rm(sac2preStead)

    # Works but not in the most ideal way. From here I need to clean up the attributes to match
    # with STEAD format and remove the SAC column as well as the non-STEAD columns before
    # converting to hdf5 and csv.
    for (i in 1:length(pre_hdf5$sac_p_index)) {

      gc()

      pre_hdf5$sac_EV[[i]] <- pre_hdf5$sac[[i]][pre_hdf5$sac_p_index[i]:(pre_hdf5$sac_index_end[i] - 1)]
    }
    p()
    Sys.sleep(.2)

    #removes the SAC values to minimize the pre_hdf5 file size.
    pre_hdf5 <- pre_hdf5[-6]

    pre_hdf5_unnested <- pre_hdf5 %>%
      tidyr::unnest(cols = c(sac_EV))
    p()
    Sys.sleep(.2)

    rm(pre_hdf5)

    # This is meant to remove every other row from signal data with sampling rate of 200 Hz
    pre_hdf5_unnested_filtered <- pre_hdf5_unnested %>%
      dplyr::group_by(event, reciever_code) %>%
      dplyr::slice(
        if(as.integer(sampling_rate[1]) == 200) {
          seq(1, dplyr::n(), 2)
        } else 1:dplyr::n()
      )
    p()
    Sys.sleep(.2)

    rm(pre_hdf5_unnested)

    # re-sampling the index does not work for some reason.
    pre_hdf5_filtered <- pre_hdf5_unnested_filtered %>%
      tidyr::nest(sac_EV = sac_EV) %>%                   # the nested data no longer has the name "sac_EV". Need to rename.
      dplyr::mutate(
        sac_p_index = dplyr::if_else(as.integer(sampling_rate) == 200, sac_p_index/2, sac_p_index),
        sac_s_index = dplyr::if_else(as.integer(sampling_rate) == 200, sac_s_index/2, sac_s_index),
        sac_coda_index = sac_p_index + 4800,
        # sac_coda_index = dplyr::if_else(as.integer(sampling_rate) == 200, sac_coda_index/2, sac_coda_index),
        sac_index_end = dplyr::if_else(as.integer(sampling_rate) == 200, sac_index_end/2, sac_index_end),
        sampling_rate = dplyr::if_else(as.integer(sampling_rate) == 200, 100, 100)
      )
    p()
    Sys.sleep(.2)

    rm(pre_hdf5_unnested_filtered)

    pre_hdf5_filtered <- pre_hdf5_filtered %>%
      dplyr::mutate(
        sac_EV = list(unlist(sac_EV) - mean(unlist(sac_EV)))
      )
    p()
    Sys.sleep(.2)

    for (i in 1:length(pre_hdf5_filtered$sac_EV)) {
      gc()
      pre_hdf5_filtered$sac_EV[i] <- eseis::signal_filter(pre_hdf5_filtered$sac_EV[i],
                                                          f = c(1, 45),
                                                          fft = F,
                                                          # fft = T,
                                                          type = "BP",
                                                          order = 2)
    }
    p()
    Sys.sleep(.2)
    for (i in 1:length(pre_hdf5_filtered$sac_p_index)) {

      gc()

      x <- pre_hdf5_filtered$sac_EV[[i]] %>% unlist()

      y <- x[500:700]

      z <- y %>%
        abs() %>%
        stats::quantile(., 0.95)

      pre_hdf5_filtered$snr_N[i] <- sum(abs(y[abs(y) > z]) ^ 2)

      y2 <- x[(pre_hdf5_filtered$sac_s_index[i] - (pre_hdf5_filtered$sac_p_index[i])):(pre_hdf5_filtered$sac_s_index[i] - (pre_hdf5_filtered$sac_p_index[i] + 700) + 200)]

      z2 <- y2 %>%
        abs() %>%
        stats::quantile(., 0.95)

      pre_hdf5_filtered$snr_S[i] <- sum(abs(y2[abs(y2) > z2]) ^ 2)
    }
    p()
    Sys.sleep(.2)

    rm(i)
    rm(x)
    rm(y)
    rm(y2)
    rm(z)
    rm(z2)

    pre_hdf5_filtered
  })
}


