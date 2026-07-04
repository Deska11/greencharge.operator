#' Ambil data transaksi
#'
#' @param tanggal_awal Tanggal mulai (format "YYYY-MM-DD")
#' @param tanggal_akhir Tanggal selesai (format "YYYY-MM-DD")
#' @param stasiun_id ID stasiun (default = "semua")
#' @return Data frame transaksi
#' @export
ambil_data_transaksi <- function(tanggal_awal, tanggal_akhir, stasiun_id = "semua") {

  # Validasi tanggal
  if(!validasi_tanggal(tanggal_awal) || !validasi_tanggal(tanggal_akhir)) {
    stop("Tanggal harus dalam format 'YYYY-MM-DD'")
  }

  if(tanggal_awal > tanggal_akhir) {
    stop("tanggal_awal harus lebih kecil dari tanggal_akhir")
  }

  # Cek apakah ada file data
  file_data <- "data/transaksi_sample.csv"

  if(file.exists(file_data)) {
    data <- read.csv(file_data)
    data$tanggal <- as.Date(data$tanggal)
  } else {
    # Buat data simulasi jika file tidak ada
    set.seed(123)
    tanggal_seq <- seq(as.Date(tanggal_awal), as.Date(tanggal_akhir), by = "day")

    data <- data.frame(
      id_transaksi = 1:100,
      tanggal = sample(tanggal_seq, 100, replace = TRUE),
      id_stasiun = sample(c("A", "B", "C"), 100, replace = TRUE),
      id_charger = sample(c("CH-01", "CH-02", "CH-03", "CH-04", "CH-05"), 100, replace = TRUE),
      kwh = round(runif(100, 10, 50), 1),
      durasi_menit = round(runif(100, 30, 120)),
      biaya = round(runif(100, 40000, 200000), 0)
    )
  }

  # Filter data
  data <- data[data$tanggal >= as.Date(tanggal_awal) &
                 data$tanggal <= as.Date(tanggal_akhir), ]

  if(stasiun_id != "semua") {
    data <- data[data$id_stasiun == stasiun_id, ]
  }

  return(data)
}

#' Ambil data stasiun
#'
#' @param stasiun_id ID stasiun (default = "semua")
#' @return Data frame stasiun
#' @export
ambil_data_stasiun <- function(stasiun_id = "semua") {

  if(file.exists("data/data_stasiun.csv")) {
    data <- read.csv("data/data_stasiun.csv")
  } else {
    data <- data.frame(
      id_stasiun = c("A", "B", "C"),
      nama_stasiun = c("Stasiun Sudirman", "Stasiun Gatot Subroto", "Stasiun Kuningan"),
      alamat = c("Jl. Sudirman No. 1", "Jl. Gatot Subroto No. 2", "Jl. Kuningan No. 3"),
      kota = c("Jakarta", "Jakarta", "Jakarta"),
      total_charger = c(5, 4, 3)
    )
  }

  if(stasiun_id != "semua") {
    data <- data[data$id_stasiun == stasiun_id, ]
  }

  return(data)
}
