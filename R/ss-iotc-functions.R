# functions created by the ss-iotc team for analyzing the YFT datasets

# dependencies 


run_1a_analysis <- function() {
  data("YFT_SRD_1A_4")
  # convert data format from SS 3.24 5o SS 3.30
  dat_1A_4_330 <- convert_data(dat = dat_1A_4)
}

convert_dat_SS324_to_SS330 <- function(dat_3.24) {
  if(!is.list(dat_3.24)) {
    dat_3.24 <- r4ss::SS_readdat(file = dat_3.24, version = "3.24",
                                 verbose = FALSE)
  }
  dat_3.30 <- dat_3.24
  dat_3.30$SSversion <- NULL
  dat_3.30$ReadVersion <- "3.30"
  dat_3.30$Nsubseasons <- 
  dat_3.30$spawn_month <- dat_3.24$spawn_seas # note: may need something more sophisticated 
  dat_3.30$spawn_seas <- NULL
  dat_3.30$Nfleets <- dat_3.24$Nfleet + dat_3.24$Nsurveys
  dat_3.30$Nfleet <- NULL
  dat_3.30$Nsurveys <- NULL
  dat_3.30$Nsexes <- dat_3.24$Ngenders
  dat_3.30$fleetinfo <- data.frame(type = dat_3.24$fleetinfo$type, 
                                   surveytiming = dat_3.24$fleetinfo$surveytiming,
                                   area = dat_3.24$fleetinfo$areas, 
                                   units = dat_3.24$fleetinfo$units,
                                   fleetname = rownames(dat_3.24$fleetinfo))
  fleet_key <- data.frame(fleet = seq_len(nrow(dat_3.30$fleetinfo)), 
                          fleetname = dat_3.30$fleetinfo$fleetname)
  tmp_catch <- tidyr::pivot_longer(dat_3.30$catch, cols = seq_len(dat_3.24$Nfleet), 
                      names_to = "fleetname")
  tmp_catch <- merge(x = tmp_catch, y = fleet_key)
  tmp_catch <- tmp_catch[, c("year", "seas", "fleet", "value")]
  colnames(tmp_catch)[4] <- "catch"
  tmp_catch$catch_se <- 0.01 # is this correct? I think this info is missing from SS 3.24.
  dat_3.30$catch <- tmp_catch
}
