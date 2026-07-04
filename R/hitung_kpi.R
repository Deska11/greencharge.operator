#' Hitung tingkat pemanfaatan
#'
#' @param data Data transaksi
#' @param total_charger Jumlah charger (default = 5)
#' @return Data frame pemanfaatan
#' @export
hitung_pemanfaatan <- function(data, total_charger = 5) {

  if(nrow(data) == 0) {
    warning("Data kosong")
    return(data.frame())
  }

  # Hitung per stasiun
  hasil <- data %>%
    group_by(id_stasiun) %>%
    summarise(
      total_jam = sum(durasi_menit, na.rm = TRUE) / 60,
      total_kwh = sum(kwh, na.rm = TRUE),
      jumlah_transaksi = n(),
      .groups = "drop"
    ) %>%
    mutate(
      kapasitas = total_charger * 24 * 30,
      pemanfaatan_persen = round((total_jam / kapasitas) * 100, 1),
      rata_jam_per_hari = round(total_jam / 30, 1)
    ) %>%
    arrange(desc(pemanfaatan_persen))

  # Tambah nama stasiun
  stasiun <- ambil_data_stasiun()
  hasil <- hasil %>%
    left_join(stasiun %>% select(id_stasiun, nama_stasiun), by = "id_stasiun")

  return(hasil)
}

#' Hitung total pendapatan
#'
#' @param data Data transaksi
#' @param by_stasiun Jika TRUE, hitung per stasiun
#' @return Data frame atau angka pendapatan
#' @export
hitung_pendapatan <- function(data, by_stasiun = FALSE) {

  if(nrow(data) == 0) {
    warning("Data kosong")
    return(data.frame())
  }

  if(by_stasiun) {
    hasil <- data %>%
      group_by(id_stasiun) %>%
      summarise(
        total_pendapatan = sum(biaya, na.rm = TRUE),
        jumlah_transaksi = n(),
        rata_rata = mean(biaya, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_pendapatan))

    stasiun <- ambil_data_stasiun()
    hasil <- hasil %>%
      left_join(stasiun %>% select(id_stasiun, nama_stasiun), by = "id_stasiun")

    return(hasil)
  }

  # Total keseluruhan
  hasil <- data %>%
    summarise(
      total_pendapatan = sum(biaya, na.rm = TRUE),
      jumlah_transaksi = n(),
      rata_rata = mean(biaya, na.rm = TRUE)
    )

  return(hasil)
}

#' Hitung total konsumsi
#'
#' @param data Data transaksi
#' @param by_stasiun Jika TRUE, hitung per stasiun
#' @return Data frame atau angka konsumsi
#' @export
hitung_konsumsi <- function(data, by_stasiun = FALSE) {

  if(nrow(data) == 0) {
    warning("Data kosong")
    return(data.frame())
  }

  if(by_stasiun) {
    hasil <- data %>%
      group_by(id_stasiun) %>%
      summarise(
        total_kwh = sum(kwh, na.rm = TRUE),
        jumlah_transaksi = n(),
        rata_rata = mean(kwh, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_kwh))

    stasiun <- ambil_data_stasiun()
    hasil <- hasil %>%
      left_join(stasiun %>% select(id_stasiun, nama_stasiun), by = "id_stasiun")

    return(hasil)
  }

  hasil <- data %>%
    summarise(
      total_kwh = sum(kwh, na.rm = TRUE),
      jumlah_transaksi = n(),
      rata_rata = mean(kwh, na.rm = TRUE)
    )

  return(hasil)
}

#' Hitung semua KPI lengkap
#'
#' @param data Data transaksi
#' @param total_charger Jumlah charger
#' @return List berisi semua KPI
#' @export
hitung_kpi_lengkap <- function(data, total_charger = 5) {

  if(nrow(data) == 0) {
    warning("Data kosong")
    return(NULL)
  }

  list(
    ringkasan = list(
      total_kwh = sum(data$kwh),
      total_biaya = sum(data$biaya),
      total_transaksi = nrow(data),
      rata_kwh = mean(data$kwh),
      rata_biaya = mean(data$biaya)
    ),
    per_stasiun = hitung_pendapatan(data, by_stasiun = TRUE),
    pemanfaatan = hitung_pemanfaatan(data, total_charger)
  )
}
