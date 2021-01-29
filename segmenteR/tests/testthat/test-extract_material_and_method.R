test_that("Extraction of material and methods remain the same", {
  if (!file.exists("Abrams, M T et al 2010.pdf")) {
    url <- ("https://www.cell.com/action/showPdf?pii=S1525-0016%2816%2931594-5")
    download.file(url, "Abrams, M T et al 2010.pdf")
  }
  section_test <- readRDS("section_test.RDS")
  section_aliases <- c("material", "method", "experimental", "experiment", "methodology")
  expect_identical(extract_section_from_pdf(
    pdf_name = "Abrams, M T et al 2010.pdf",
    udpipe_model = "english-gum-ud-2.4-190531.udpipe", section_aliases = section_aliases
  ), section_test)
})
