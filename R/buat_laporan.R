#' Buat laporan operator
#'
#' @param data Data transaksi
#' @param output_file Nama file output
#' @param format Format: "html", "pdf", "word"
#' @param open_report Buka laporan setelah dibuat
#' @export
buat_laporan_operator <- function(
    data,
    output_file = "laporan_operator.html",
    format = "html",
    open_report = TRUE
) {

  # Buat template laporan (menggunakan paste() untuk multi-line)
  template <- paste(
    "---",
    "title: Laporan Operasional Harian",
    "author: Operator Stasiun",
    paste0("date: '", Sys.Date(), "'"),
    "output: html_document",
    "params:",
    "  data: NULL",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(knitr)",
    "library(ggplot2)",
    "library(DT)",
    "data <- params$data",
    "```",
    "",
    "# Ringkasan",
    "",
    "```{r ringkasan, echo=FALSE}",
    "total_kwh <- sum(data$kwh)",
    "total_biaya <- sum(data$biaya)",
    "total_transaksi <- nrow(data)",
    "",
    "cat('Total Konsumsi:', total_kwh, 'kWh\\n')",
    "cat('Total Pendapatan: Rp', format(total_biaya, big.mark = '.'), '\\n')",
    "cat('Jumlah Transaksi:', total_transaksi, '\\n')",
    "```",
    "",
    "# Grafik Konsumsi per Stasiun",
    "",
    "```{r grafik, echo=FALSE, fig.height=5}",
    "ggplot(data, aes(x = id_stasiun, y = kwh, fill = id_stasiun)) +",
    "  geom_boxplot() +",
    "  labs(title = 'Distribusi Konsumsi per Stasiun')",
    "```",
    "",
    "# Data Transaksi",
    "",
    "```{r tabel, echo=FALSE}",
    "datatable(data, options = list(pageLength = 10))",
    "```",
    sep = "\n"
  )

  # Simpan template ke file temporary
  temp_file <- tempfile(fileext = ".Rmd")
  writeLines(template, temp_file)

  # Render laporan
  rmarkdown::render(
    input = temp_file,
    output_file = output_file,
    params = list(data = data),
    quiet = TRUE
  )

  # Hapus file temporary
  file.remove(temp_file)

  if(open_report) {
    browseURL(output_file)
  }

  cat("Laporan berhasil dibuat:", output_file, "\n")
}
