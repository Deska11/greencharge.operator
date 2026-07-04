#' Format angka menjadi Rupiah
#'
#' @param x Angka yang akan diformat
#' @return String dengan format Rupiah
#' @export
format_rupiah <- function(x) {
  paste0("Rp", prettyNum(round(x), big.mark = ".", decimal.mark = ","))
}

#' Format angka dengan pemisah ribuan
#'
#' @param x Angka yang akan diformat
#' @return String dengan pemisah ribuan
#' @export
format_angka <- function(x) {
  prettyNum(round(x), big.mark = ".", decimal.mark = ",")
}

#' Format angka menjadi kWh
#'
#' @param x Angka yang akan diformat
#' @return String dengan format kWh
#' @export
format_kwh <- function(x) {
  paste0(prettyNum(round(x), big.mark = ".", decimal.mark = ","), " kWh")
}

#' Validasi tanggal
#'
#' @param tanggal String tanggal dalam format YYYY-MM-DD
#' @return TRUE jika valid, FALSE jika tidak
validasi_tanggal <- function(tanggal) {
  tryCatch({
    as.Date(tanggal)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}
