# look into pre_filtering the station_list with the .dat receiver codes to minimize
# data storage
plan(multicore)

fp <- file_paths(training_files = c("picfil", "json", "hypo71", "refloc"),
           dat_path ="~/Bryan/ML_work/nmtso_data/dat_files",
           station_list_path = "~/Bryan/ML_work/ml-seismic-application/spyder/EQTransformer_test/project/json")

# look into way to hide the warnings
ws <- wrangle_SEISMOS(file_paths = fp)

# consider the command line inputs are operating system specific. Mac and Windows
use_mseed2sac(mseed_path = "~/Bryan/ML_work/ml-seismic-application/spyder/EQTransformer_test/project/downloads_mseeds",
              sac_path = "~/Bryan/ML_work/sac2eqtransformr/sample_data/signal_data",
              executable_path = "~/Bryan/ML_work/mseed2sac-2.3/mseed2sac",
              wrangled_SEISMOS = ws)


headerData <- header_data(sac_path = "~/Bryan/ML_work/sac2eqtransformr/sample_data/signal_data",
                          wrangled_SEISMOS = ws)


preSTEAD <- sac_2preSTEAD(header_data = headerData)


STEAD_data <- sac2STEAD(sac2preStead = preSTEAD,
                   wrangle_SEISMOS = ws)

# change snr_db to a 3x1 matrix and add bracket
# add double brackets to code_end_sample with a period
meta_data <- form_STEAD(STEAD_data = STEAD_data)

# possibly add two zero values to the SNR column
make_trainer_hdf5(h5_file_path = "~/Bryan/ML_work/sac2eqtransformr/sample_data",
                  meta_data = meta_data,
                  signal = STEAD_data)




