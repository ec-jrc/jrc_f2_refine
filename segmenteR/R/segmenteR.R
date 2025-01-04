#' Download the model, Tokenize and make the Dependency Parsing of the extracted text
#'
#' @param txt_pdf Raw text read from the article in pdf, extracted using the
#' tabulapdf::extract_text() function.
#' @param udpipe_model An object of class udpipe_model, as returned by udpipe_load_model()
#' from the package udpipe. If left void, the function will download the model
#' english-gum annotation model from the treebanks available at
#' https://universaldependencies.org and load it to annotate the raw text.
#'
#' @return A dataframe of the raw text of the article annotated using the model, in the CoNLL-U format.
#' Please refer to https://bnosac.github.io/udpipe/en/index.html to know more about it.
#' @export
#'
#'
annotate_txt_pdf <- function(txt_pdf, udpipe_model) {

  # Following line need a refactor with two things :
  # the try catch does not work properly
  # the try catch should be in a other function to make it clearer?
  
  # # Trying to load the model in case user forgot to provide one :
  # ud_model_gum <- tryCatch(
  #   {
  #     ud_model_gum <- udpipe::udpipe_load_model(file = udpipe_model)
  #   },
  #   error = function(cond) {
  #     message("Failed to load the model for annotation.")
  #     message("Trying to download the model.")
  #     # res object to test for res$download_failed
  #     res <- udpipe::udpipe_download_model(language = "english-gum")
  #     if (res$download_failed == TRUE) {
  #       stop("Download of udpipe model failed")
  #     }
  #     # Use res$file_model that contain the name of the file to load the model
  #     ud_model_gum <- udpipe::udpipe_load_model(file = res$file_model)
  #     return(ud_model_gum)
  #   }
  # ) # endtryCatch

  # conllu_df is the standard name of the dataframe in the documentation of udpipe
  # annotate the txt file
  conllu_df <- udpipe::udpipe_annotate(object = udpipe_model, x = txt_pdf)
  conllu_df <- as.data.frame(conllu_df)

  return(conllu_df)
}

#' Identify the font used for the section titles
#'
#' This function identify the most likely font used to write the sections title
#' inside the article in pdf, by looking first at the font used in Acknowledgment
#' and References.
#' The function first try to identify where are the words References and
#' Acknowledgements in the poppler output. After that function try to identify if
#' there is a section named References, and it not, Acknowledgement
#'
#' @param poppler_output A dataframe with the text box (font), name of the font
#' and the size of the font, produced by prepare_poppler_output.
#'
#' @return The name of the font used for the sections titles.
#' @export
identify_font <- function(poppler_output) {

  # Need of addition of capitalize() to be able to identify "REFERENCES"
  # So the function can send something back if there is nothing :
  font_sections <- "void"

  reference_df <- poppler_output[which(capitalize_first_letter(poppler_output$Word) %in% c("References")), ]
  ack_df <- poppler_output[which(capitalize_first_letter(poppler_output$Word) %in%
    c(
      "Acknowledgements", "Acknowledgments",
      "Acknowledgement", "Acknowledgment"
    )), ]

  if (dim(reference_df)[1] > 0) { # if Reference exist
    reference_df <- reference_df[which(reference_df$Size == max(reference_df$Size)), ]
    font_sections <- unique(reference_df$Font)
  } else { # if Acknowledgement exist
    if (dim(ack_df)[1] > 0) {
      font_sections <- unique(ack_df$Font)
    }
  }

  return(font_sections)
}

#' Merge sections name into one, such as "Discussion and Results"
#'
#'
#' This function aims to merge really close section in positions_section_df
#' The problem arise from section title like "results and discussion", or
#' "materials and methods".
#' The way the results of the poppler is read does not discriminate for section
#' in the same sentence. A section "results and discussion" would create two
#' entries in positions_sections_df. This function gather together sections in
#' positions_sections_df that occurred at a distance of two words or so.
#' @inheritParams extract_section_from_conllu
#'
#' @return The position of the section in the Conllu data structure.
merging_section <- function(positions_sections_df) {
  positions_sections_df$section <- as.character(positions_sections_df$section)
  for (i in 1:(length(positions_sections_df$occurrences) - 1)) {
    gap <- positions_sections_df$occurrences[[i + 1]] - positions_sections_df$occurrences[[i]]
    if (gap < 4) {
      new_section <- paste(positions_sections_df[i, ]$section, "and", positions_sections_df[i + 1, ]$section)
      positions_sections_df[i, ]$section <- new_section
      positions_sections_df <- positions_sections_df[-(i + 1), ]
      return(merging_section(positions_sections_df))
    }
  }
  return(positions_sections_df)
}

#' Preprocess the section titles
#'
#' This function preprocess the section title to help the extractions functions.
#' For example, "aterial" cannot be find in "MATERIAL".
#' Called in extract_section_from_conllu().
#' @inheritParams extract_section_from_conllu
#'
#' @return
clean_section_title <- function(positions_sections_df) {
  positions_sections_df$section <- as.character(positions_sections_df$section)
  positions_sections_df$section <- tolower(positions_sections_df$section)
  return(positions_sections_df)
}

#' Remove the most abundant font, usually the font of the text
#'
#' In "Baker, G L et al 2008.pdf", the word of the section's titles had a smaller
#' size than the text of the article. This seems to happened in old articles/for
#' different fonts that have a different scale.
#' Removing the font of the text is necessary in this case, and, in general way,
#' calling this function once help processing the text.
#' This function is called in the main function of the package and
#' find_section_tile().
#'
#' @inheritParams identify_font
#'
#' @return
#' @export
#'
#'
clean_font_txt <- function(poppler_output) {
  # Following line make a frequency of the fonts :
  fonts <- as.data.frame(table(poppler_output$Font)) # dataframe of fonts freq
  # Fond the most abundant font, the one of the text
  font_text <- fonts$Var1[which(fonts$Freq == max(fonts$Freq))] # font the most used
  clean_df_poppler <- poppler_output[-which(poppler_output$Font == font_text), ]
  return(clean_df_poppler)
}

#' Preprocess the text of the article using regex expression
#'
#' This function is a succession of regex to correct/preprocess the text extracted
#' from the pdf. Since the raw text is extracted directly from the pdf using
#' tabulapdf, there is some typos or recurrent character so remove. For example,
#' non graphical caracter, or "-\" and newline character in the middle of a word,
#' or concatenated at the beginning of the word.
#'
#' @param txt_pdf The text of the article, into a single string, extracted from
#' the pdf using tabulapdf
#' @export
#'
#' @return
preprocess_article_txt <- function(txt_pdf) {
  # "To gather all these points, iodine-based nano-\nemulsions, have recently been developed showing huge
  # stabil-\nity, high biocompatibility and great potential in medical ap-\nplications, such as image-guided
  # surgery, advanced diagnosis\n(e.g., to recognize tumor regions), personalized medicine or\ntheragnostics."

  # "To gather all these points, iodine-based nanoemulsions, have recently been developed showing huge stability,
  # high biocompatibility and great potential in medical applications, such as image-guided surgery, advanced
  # diagnosis\n(e.g., to recognize tumor regions), personalized medicine or\ntheragnostics."
  txt_pdf <- gsub("\\b\\-\n", "", txt_pdf, perl = TRUE)

  # As shown with "Chung, E J et al 2015.pdf", the section title can be merge with the dot and the previous section.
  # This problem was already partially addressed by regex_correction()
  # Here the section Result is not found because merge with the end of the previous section as following :
  # "significant.Results".
  # Instead of solving this issue in the NLP structure when the occurrences of the section is zero,
  # This function repair the text to avoid any other downstrean trouble.
  # https://stackoverflow.com/questions/58936991/how-to-split-two-words-connected-by-a-dot-in-r
  # Example of the regex :
  # > txt<-"significant.Results"
  # > gsub("(?<=\\p{L})\\.(?=\\p{L})", ". ", txt, perl=TRUE)
  # [1] "significant. Results"

  txt_pdf <- gsub("\\b\\.\\b", ". ", txt_pdf, perl = TRUE)

  # "Elsabahy, M et al 2013.pdf" show a similar problem, but with a coma
  # Example of the regex :
  # "SCKs,Results and discussion" ->  "SCKs. Results"
  txt_pdf <- gsub("\\b\\,\\b", ", ", txt_pdf, perl = TRUE)
  # txt_pdf<-gsub("\\b[[:graph:]]\\b", ". ", txt_pdf, perl=TRUE)

  # https://stackoverflow.com/questions/26896971/add-space-between-two-letters-in-a-string-in-r
  # https://stringr.tidyverse.org/articles/regular-expressions.html
  # "methods2.1." -> "methods 2.1"
  # don't cut "2014"
  txt_pdf <- gsub("([A-z])([1-9])", "\\1 \\2", txt_pdf)

  # "31443144References" ->  "31443144 References"
  # "Yu, J et al 2014.pdf"
  txt_pdf <- gsub("([1-9])([A-z])", "\\1 \\2", txt_pdf, perl = TRUE)

  # remove non graphical caracter :
  # https://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
  txt_pdf <- stringr::str_replace_all(txt_pdf, "[^[:graph:]]", " ")

  # "Zhang, J et al 2013.pdf"
  # View Article OnlineResults and discussion Characterization of nanoTiO 2 As shown in Fig.
  txt_pdf <- stringr::str_replace_all(txt_pdf, "Online", "Online ")

  return(txt_pdf)
}

#' Clean title of the journal from the section_title_df.
#'
#' An article, Guo, J et al 2014.pdf highlight a the following problem :
#' the title of the journal can be written with the same font that the sections
#' titles. The simplest solution to handle this problem is to correct the
#' section_title_df().
#' This function check if there is a section repeated in the dataframe a number
#' of time equal to the number of pages of the pdf or equal to the number of
#' page less 1.
#'
#'
#' @inheritParams extract_section_from_pdf
#' @inheritParams reduce_occurrences
#'
#' @return
#' @export
#'
#'
#'
#'
clean_title_journal <- function(pdf_name, section_title_df) {
  res <- table(section_title_df$Word)
  res <- as.data.frame(res)

  nb_page <- tabulapdf::get_n_pages(pdf_name)
  title_mistaken <- which(res$Freq == nb_page | res$Freq == (nb_page - 1))

  if (length(title_mistaken) > 0) { # if exist
    title_journal <- res[title_mistaken, ]$Var1
    section_title_df <- section_title_df[-(which(section_title_df$Word == title_journal)), ]
    message("Clean_title_journal has been called")
  }
  return(section_title_df)
}

#' Grep the lemma corresponding to the head token of the token at index
#'
#' Grep the lemma corresponding to the head_token_id of the token at the entry
#' at index in the conllu_df. Function called by eliminate_cf_occurrences().
#'
#' @inheritParams extract_section_from_conllu
#' @param index
#'
#' @return
#'
#'
grep_head_token <- function(conllu_df, index) {
  # conllu_df[index,] return a token and all the associated data :
  # lemma, but also sentence and doc_id
  # conllu_df[index,], where conllu_df is the dataframe of annotation generated by udpipe
  occurrence <- conllu_df[index, ]
  head_token_id <- occurrence$head_token_id
  head_token_id <- as.numeric(head_token_id)
  if (head_token_id == 0) {
    lemma_head_token_id <- conllu_df[index, ]$lemma
    return(lemma_head_token_id)
  }
  sentence_id <- occurrence$sentence_id
  # the following line query the lemma of the head_token_id based on the previous parameters
  lemma_head_token_id <- conllu_df[which(conllu_df$sentence_id == sentence_id)[head_token_id], ]$token
  return(lemma_head_token_id)
}

#' Reorder the sections inside section_title_df
#'
#' If the position of the section "Acknowledgments" is set before "Conclusion",
#' reorder the sections titles to position "Acknowledgments" after "Conclusion".
#' This would otherwise lead to crash downstream since Conclusion is placed
#' before for a human reader AND for tabulapdf, which handle(s) double column
#' writing style better than poppler.
#'
#' In "Smulders, S et al 2015.pdf" the problem was encountered :
#' > section_title_df
#' Word                   Font   Size
#' 543      Introduction CLNCIJ+AdvOT18499c10.B 7.9702
#' 1089        Materials CLNCIJ+AdvOT18499c10.B 7.9702
#' 1091          Methods CLNCIJ+AdvOT18499c10.B 7.9702
#' 1787          Results CLNCIJ+AdvOT18499c10.B 7.9702
#' 2263       Discussion CLNCIJ+AdvOT18499c10.B 7.9702
#' 3447 Acknowledgements CLNCIJ+AdvOT18499c10.B 7.9702
#' 3449       Conclusion CLNCIJ+AdvOT18499c10.B 7.9702
#' 3624    Supplementary CLNCIJ+AdvOT18499c10.B 7.9702
#' 3642       References CLNCIJ+AdvOT18499c10.B 7.9702
#'
#'
#' @inheritParams reduce_occurrences
#'
#' @return
#' @export
#'
#'
ad_hoc_reorder <- function(section_title_df) {
  pos_conclusion <- which(capitalize_first_letter(section_title_df$Word) %in% c("Conclusion", "Conclusions"))
  pos_ack <- which(capitalize_first_letter(section_title_df$Word) %in% c("Acknowledgements", "Acknowledgments"))


  if (length(pos_conclusion) > 0 & length(pos_ack > 0)) {
    if (pos_ack < pos_conclusion) {
      # it is swapping time
      section_title_df$Word[c(pos_conclusion, pos_ack)] <- section_title_df$Word[c(pos_ack, pos_conclusion)]
      message("ad_hoc_reorder(section_title_df) has been called")
    }
  }
  return(section_title_df)
}

#' Message if a section appeared in duplicate in positions_sections_df
#'
#' Send a message if there is duplicate in positions_sections_df.
#'
#' @inheritParams extract_section_from_conllu
#'
#' @export
#'
#'
check_sections_df <- function(positions_sections_df) {
  df <- as.data.frame(table(positions_sections_df$section))
  if (max(df$Freq) > 1) {
    message("One section appeared at least in duplicate when calling check_section_df()")
  }
}

#' Remove the references section
#'
#' This function remove the "references" section and the ones that came after in
#' section_title_df. This is to prevent failure due to have sections aliases
#' located in the references section. This can cause the whole script to fail,
#' since no other section would be found after this falsely located section.
#'
#' @inheritParams reduce_occurrences
#'
#' @export
#' @return section_title_df without the reference and the following section
remove_reference_section_from_titles <- function(section_title_df) {
  idx <- which(lower_but_first_letter(section_title_df$Word) %in% c("References"))
  if (length(idx) == 1) {
    section_title_df <- section_title_df[1:(idx - 1), ]
  }
  return(section_title_df)
}

#' Remove the bibliography from conllu.
#'
#' This function must be called before remove_reference_section_from_titles.
#' This function remove the bibliography from the article.
#' This is to prevent failure due to have sections aliases located in the
#' references section, such as material in a title of an article.
#' This can cause the whole script to fail, since no other section would be found
#' after this falsely located section.
#' @inheritParams extract_section_from_conllu
#' @inheritParams reduce_occurrences
#'
#' @export
#' @return the conllu_df without the whole bibliography
remove_bibliography_from_conllu <- function(conllu_df, section_title_df) {
  occurrences <- which(lower_but_first_letter(conllu_df$token) %in% c("References"))
  idx <- which(lower_but_first_letter(section_title_df$Word) %in% c("References"))

  if (length(occurrences) == 1 & length(idx) == 1) {
    conllu_df <- conllu_df[1:(occurrences + 1), ]
  }
  return(conllu_df)
}


#' Extract the section from the ConllU dataframe
#'
#' Extract the section specified by the vector of section aliases.
#' The function use a for loop to parse the different sections in the
#' position_section_df to find a match. Earlier build stopped at the first match
#' and returned everything between the start of the section and the next one
#' inside position_section_df. Now it continues to look for match after the
#' first one, in case of, for example, "Material" and "Methods" are split into
#' different sections.
#'
#' @param conllu_df A dataframe of the raw text of the article annotated using
#' the model, in the CoNLL-U format.
#' @param positions_sections_df A dataframe with the positions of the sections
#' currently localized inside the conllu_df.
#' @inheritParams extract_section_from_pdf
#'
#' @return The section extracted in a dataframe in the CoNLL-U format.
#' @export
#'
#'
extract_section_from_conllu <- function(conllu_df, positions_sections_df, section_aliases) {
  positions_sections_df <- clean_section_title(positions_sections_df)

  # pattern is meant to use the operator or in the grepl
  pattern <- paste(section_aliases, collapse = "|")

  for (i in 1:(length(positions_sections_df$section))) {
    if (grepl(pattern, positions_sections_df$section[i])) {
      idx <- i
      beginning_section <- positions_sections_df[idx, ]$occurrences
      # if not the last section of the article
      if (length(positions_sections_df$occurrences) >= idx + 1) {
        end_section <- positions_sections_df[idx + 1, ]$occurrences
      } else { # if the section is the last one of the article, just go until the end of conllu_df
        end_section <- dim(conllu_df)[1]
      }
      # following line if for material and methods split in different sections :
      if (exists("section")) {
        section <- rbind(section, conllu_df[beginning_section:(end_section - 1), ])
      } else {
        section <- conllu_df[beginning_section:(end_section - 1), ]
      }
    }
  }
  return(section)
}

#' Extract a specified section from a scientific article in pdf format.
#'
#' The main function of the package. Taking the name of a pdf, it extract the
#' section that have a section title that match the section_aliases provided by
#' the user. Please refer to the vignette of the package for a more detailed
#' description of the internal working of this function.
#'
#' @inheritParams annotate_txt_pdf
#' @param pdf_name A string, the name of the article in pdf format, for which we
#' want to extract a specific section specified by section_aliases.
#' @param section_aliases A vector of strings, which are the different names that
#' the section extracted by extract_section_from_pdf() can have.
#' @param remove_bibliography Boolean. If the references of the article must be
#' removed during the extraction, to avoid error due to aliases of the section
#' names inside the references. Default to TRUE (references of the articles are removed).
#'
#' @return A dataframe of the section of the article in the CoNLL-U format.
#' Please refer to https://bnosac.github.io/udpipe/en/index.html to know more about this specific format.
#' @export
extract_section_from_pdf <- function(pdf_name, udpipe_model, section_aliases, remove_bibliography = TRUE) {
  txt_pdf <- tabulapdf::extract_text(pdf_name) # read the text from the pdf
  txt_pdf <- preprocess_article_txt(txt_pdf)

  conllu_df <- annotate_txt_pdf(txt_pdf, udpipe_model) # create the dataframe for NLP using udpipe

  poppler_output <- prepare_poppler_output(pdf_name)
  font_section <- identify_font(poppler_output)

  # the sections what the script will try to identify in the doppler output
  list_of_sections <- list(
    c("Introduction", "INTRODUCTION"),
    c("Materials", "Material", "materials", "material", "MATERIALS", "MATERIAL"),
    c("Methods", "Method", "methods", "method", "METHODS", "METHOD"),
    c(
      "Acknowledgements", "Acknowledgments", "ACKNOWLEDGEMENTS", "ACKNOWLEDGMENTS",
      "Acknowledgement", "Acknowledgment", "ACKNOWLEDGEMENT", "ACKNOWLEDGMENT"
    ),
    c("References", "REFERENCES"),
    c("Results", "RESULTS"),
    c("Discussion", "DISCUSSION", "discussion"),
    c("Abstract", "ABSTRACT"),
    c("Conclusions", "Conclusion", "CONCLUSION", "CONCLUSIONS"),
    c("Background", "BACKGROUND"),
    c("Experimental", "EXPERIMENTAL", "Experiment"), # Experiment :Yu, Z et al 2013.pdf
    c("Supplementary", "SUPPLEMENTARY"),
    c("Methodology"), # "Meng, H et al 2007.pdf"
    c("Appendix"),
    c("Section", "SECTION")
  )


  poppler_output <- clean_font_txt(poppler_output)
  section_title_df <- create_section_title_df(font_section, list_of_sections, poppler_output)
  section_title_df <- clean_title_journal(pdf_name, section_title_df)
  section_title_df <- ad_hoc_reorder(section_title_df)

  if (remove_bibliography == TRUE) {
    conllu_df <- remove_bibliography_from_conllu(conllu_df, section_title_df)
    section_title_df <- remove_reference_section_from_titles(section_title_df)
  }

  positions_sections_df <- locate_sections_position_in_conllu(conllu_df, section_title_df)
  check_sections_df(positions_sections_df)

  section <- extract_section_from_conllu(conllu_df, positions_sections_df, section_aliases)

  return(section)
}
