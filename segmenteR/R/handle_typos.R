#' Solve problems due to typo inside the ConLL-U dataframe
#'
#' This function call various functions to solve a list of problems encountered
#' in locate_sections_position_in_conllu() due to typos generated by the extraction of the
#' text directly from the pdf. NB : this functions only work on the NLP dataframe,
#' in ConLL-U format. This function call :
#' - missing_first_letter_section() (when "Acknowledgements" became
#' "cknowledgements", and "Reference", "eference")
#' - capitalize_first_letter() ("MAterIAls"->"Materials", correct strange
#' behavior of tabulizer for section title)
#' - regex_correction() (when "Acknowledgements" became "group.Acknowledgments")
#'
#' @inheritParams extract_section_from_conllu
#' @param section A string, a section name.
#' @param occurrences A int, an index inside the conllu_df dataframe. This index
#' refer to a row, which correspond to a token, and all the information related
#' to it such as : lemma, sentence, head_token, etc.
#'
#' @return
#'
#'
handle_typos <- function(conllu_df, section, occurrences) {
  if (length(occurrences) == 0) { # is first caps is missing
    # When "Acknowledgements" became "cknowledgements", and "Reference", "eference"
    occurrences <- missing_first_letter_section(conllu_df, section)
  }
  if (length(occurrences) == 0) {
    # "MAterIAls"->"Materials", correct strange behavior of tabulizer for section title
    occurrences <- which(capitalize_first_letter(conllu_df$token) %in% section)
  }
  if (length(occurrences) == 0) {
    # in "Attia, AB et al 2013.pdf" "Acknowledgements" became "group.Acknowledgments"
    occurrences <- regex_correction(conllu_df, section)
  }
  if (length(occurrences) == 0) {
    message("warning in handle_typo()")
  }

  return(occurrences)
}

#' Look for token that are a substring of section.
#'
#' This function is called in handle_typos(), after adding_missing_first_letter_section().
#' In one of article of Elsevier, "Al-Bairuty, G et al 2013.pdf",
#' "Acknowledgements" became "cknowledgements", and "Reference", "eference"
#' but this only occurred in the conllu_df, Conll-U datastructure from UDpipe.
#' @inheritParams capitalize_first_letter
#' @inheritParams extract_section_from_conllu
#' @return occurences of truncated section such as "eference"
missing_first_letter_section <- function(conllu_df, section) {
  section <- tolower(substring(section, 2))
  occurrences <- which(conllu_df$token %in% section)
  if (length(occurrences) > 0) { # send back only if it exist
    return(occurrences)
  }
}

#' Capitalize the first letter and lower the other letters
#'
#' Used to correct strange behavior of tabulizer for section title.
#' For example, "MAterIAls"->"Materials".
#' Must be used on must on conllu_df$token.
#' @inheritParams handle_typos
#'
#' @return A string with the first letter capitalized and the other one lowered.
capitalize_first_letter <- function(section) {
  # "MAterIAls"->"Materials"
  # use to correct strange behavior of tabulizer for section title
  # must be used on conllu_df$token !
  section <- paste0(toupper(substring(section, 1, 1)), tolower(substring(section, 2)))
  return(section)
}

#' Search for token that comprise a section inside it
#'
#' Called by handle_typos(). In "Attia, AB et al 2013.pdf", "Acknowledgements"
#' became "group.Acknowledgments". This function look for section inside tokens.
#' This only occurs in the Conll-U dataframe.
#'
#'
#' @inheritParams capitalize_first_letter
#' @inheritParams extract_section_from_conllu
#'
#' @return
#'
#'
regex_correction <- function(conllu_df, section) {
  occurrences <- which(!is.na(stringr::str_extract((conllu_df$token), section)))
  if (length(occurrences) > 0) { # send back only if it exist
    return(occurrences)
  }
}
