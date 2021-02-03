
#' Extract the font information from an article in pdf
#'
#' @inheritParams extract_section_from_pdf
#' @return A dataframe with the text box (font), name of the font and the size of the font.
#' @export
prepare_poppler_output <- function(pdf_name) {
  res <- pdftools::poppler_config()
  stopifnot(res$has_local_font_info)

  poppler_output <- pdftools::pdf_data(pdf_name, font_info = TRUE)
  poppler_output <- do.call(rbind, poppler_output)
  poppler_output <- poppler_output[c("text", "font_name", "font_size")]
  poppler_output <- as.data.frame(poppler_output)
  colnames(poppler_output) <- c("Word", "Font", "Size")

  return(poppler_output)
}
