#' form_STEAD
#'
#' Reformats attributes for HDF5 and csv format
#'
#' @param pre_hdf5_filtered output from joining_sac2STEAD
#'
#' @importFrom dplyr %>%
#'
#' @export


form_STEAD <- function(STEAD_data){
  progressr::with_progress({
    p <- progressr::progressor(steps = 4)

    STEAD <- STEAD_data %>%
      dplyr::summarise(
        network_code = network,
        receiver_code = reciever_code,
        receiver_type = "BH",      # manually defined, not sure if BH is correct though.
        receiver_latitude = reciever_latitude,
        receiver_longitude = reciever_longitude,
        receiver_elevation_m = reciever_elevation_m,
        p_arrival_sample = sampling_rate * 7,
        p_status = "manual",
        p_weight = wt_0,
        p_travel_sec = obs_trv_0,
        s_arrival_sample = (sac_s_index - sac_p_index) + (7 * sampling_rate),
        s_status = "manual",
        s_weight = wt_1,
        source_id = NA,     # is this a user defined index, or does this come from IRIS?
        source_origin_time = NA,
        source_origin_uncertainty_sec = uncertanty_multiply_quarter_sec_0 * 0.25,
        source_latitude = source_latitude,
        source_longitude = source_longitute,
        source_error_sec = NA,
        source_gap_deg = GAP_M,          # check this variables source to see the unit. deg or m?
        source_horizontal_uncertainty_km = NA,   # is this an std_1 value from statistical eval? May be in the dat files.
        source_depth_km = source_depth,
        source_depth_uncertainty_km = NA,     # is this an std_1 value from statistical eval? May be in the dat files.
        source_magnitude = MAG,
        source_magnitude_type = "ml",
        source_magnitude_author = "None",
        source_mechanism_strike_dip_rake = "None",
        source_distance_deg = NA,
        source_distance_km = DIST,
        back_azimuth_deg = dplyr::if_else(condition = AZM < 180,
                                          true = AZM + 180,
                                          false = AZM - 180),
        snr_db = 10 * log10(snr_S/snr_N),     # calculate from the STEAD paper
        coda_end_sample = (sac_coda_index - sac_p_index) + (7 * sampling_rate),
        trace_start_time = NA,
        trace_category = "earthquake_local",
        trace_name = sprintf("%s.%s_%s_EV",
                             reciever_code,
                             network_code,
                             substr(event, start = 1, stop = 15) %>%
                               stringr::str_remove(pattern = "_"))
      )
    p()
    Sys.sleep(.2)
    STEAD <- STEAD %>%
      dplyr::mutate(
        coda_end_sample = if(coda_end_sample > 0) {
          coda_end_sample
        } else(NA)
      )
    p()
    Sys.sleep(.2)
    STEAD <- STEAD[,-c(1:2)]
    p()
    Sys.sleep(.2)
    gc()
    p()
    Sys.sleep(.2)
    STEAD
  })
}
