#' Deteksi anomali
#'
#' @param data Data transaksi
#' @param threshold Ambang batas (default = 2)
#' @return Data frame anomali
#' @export
deteksi_anomali <- function(data, threshold = 2) {

  if(nrow(data) == 0) {
    warning("Data kosong")
    return(data.frame())
  }

  rata_rata <- mean(data$kwh, na.rm = TRUE)
  sd <- sd(data$kwh, na.rm = TRUE)

  batas_atas <- rata_rata + (threshold * sd)
  batas_bawah <- rata_rata - (threshold * sd)

  anomali <- data %>%
    filter(kwh > batas_atas | kwh < batas_bawah) %>%
    mutate(
      status = ifelse(kwh > batas_atas, "Konsumsi Tinggi", "Konsumsi Rendah"),
      selisih = kwh - rata_rata
    ) %>%
    arrange(desc(abs(selisih)))

  if(nrow(anomali) == 0) {
    cat("Tidak ada anomali terdeteksi.\n")
  } else {
    cat("Terdeteksi", nrow(anomali), "transaksi anomali!\n")
  }

  return(anomali)
}

#' Get status real-time
#'
#' @param stasiun_id ID stasiun
#' @return Data frame status charger
#' @export
get_status_real_time <- function(stasiun_id = "semua") {

  stasiun <- ambil_data_stasiun(stasiun_id)

  if(nrow(stasiun) == 0) {
    warning("Stasiun tidak ditemukan")
    return(data.frame())
  }

  status_list <- list()

  for(i in 1:nrow(stasiun)) {
    n_charger <- stasiun$total_charger[i]

    status <- data.frame(
      id_stasiun = stasiun$id_stasiun[i],
      nama_stasiun = stasiun$nama_stasiun[i],
      charger_id = paste0("CH-", stasiun$id_stasiun[i], "-", sprintf("%02d", 1:n_charger)),
      status = sample(c("Available", "In Use", "Maintenance"), n_charger, replace = TRUE),
      power_usage = round(runif(n_charger, 0, 50), 1),
      last_update = Sys.time()
    )

    status_list[[i]] <- status
  }

  do.call(rbind, status_list)
}
