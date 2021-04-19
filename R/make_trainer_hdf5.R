#' make_trainer_hdf5
#'
#' @param h5_file_path path, with new file name included, where h5 file to be created
#' @param meta_data output from form_STEAD
#' @param signal sac_EV column values from sac2STEAD
#'
#' @import hdf5r
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
make_trainer_hdf5 <- function(h5_file_path, meta_data, signal){
  progressr::with_progress({
    p <- progressr::progressor(steps = 5)
    # both mode "a" and mode "w" output files that are read only to EqT
    NMTSO_trainer.h5 <- H5File$new(filename = sprintf("%s/NMTSO_trainer.h5", h5_file_path), mode = "a")
    p()
    Sys.sleep(.2)
    data.grp <- NMTSO_trainer.h5$create_group("data")

    # need to redo this so as to account for three components
    # Use substr(signal$channel,start=3,stop=3) to find out what component
    x = list()
    for (i in 1:length(signal$sac_EV)){
      x[[i]] <- signal$sac_EV[i] %>% unlist() %>% matrix(nrow = 1)
      # hacky fix for the three columns. Need to download SAC files with three components to fix this
      x[[i]] <- rep(0,(2 * length(signal$sac_EV[[1]]))) %>% matrix(nrow = 2) %>% rbind(x[[i]])
    }
    p()
    Sys.sleep(.2)
    rm(i)

    furrr::future_map2(meta_data$trace_name, x,function(trace_name, x){
      data.grp[[trace_name]] <- x
    })
    p()
    Sys.sleep(.2)
    rm(x)

    col_names <- colnames(meta_data)

    info <- tidyr::expand_grid(trace_name = meta_data$trace_name, col_names) %>%
      dplyr::mutate(value = furrr::future_map2(trace_name, col_names, function(x, y){
        dplyr::pull(meta_data[meta_data$trace_name == x, y])
      }))
    p()
    Sys.sleep(.2)
    info <- info[-c(which(info$col_names == "trace_name")),]

    furrr::future_pwalk(as.list(info), function(trace_name, col_names, value){
      h5attr(data.grp[[trace_name]], col_names) <- value
    })
    p()
    Sys.sleep(.2)
    rm(col_names)

    rm(info)

    NMTSO_trainer.h5$close_all(close_self = TRUE)
  })
}
