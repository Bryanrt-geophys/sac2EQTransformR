#' sac_2preSTEAD
#'
#' @param header_data output from header_data
#'
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
sac_2preSTEAD <- function(header_data){
  progressr::with_progress({
    p <- progressr::progressor(steps = (length(header_data) + 1))
    sac2preStead <- furrr::future_map(header_data, function(x){
      p()
      Sys.sleep(.2)
      x %>%
        dplyr::group_by(day, year, channel, network, sampling_rate) %>%
        dplyr::summarize(
          sac = list(unlist(.$sac))
        ) %>%
        dplyr::ungroup()
    })

    p()
    Sys.sleep(.2)
    sac2preStead <- purrr::imap(sac2preStead, function(x, y){
      x %>% dplyr::mutate(
        reciever_code = y
      )
    })

  })
}
