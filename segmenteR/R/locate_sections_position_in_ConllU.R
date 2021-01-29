#' Localize the position of the sections inside the Conll-U dataframe
#'
#' This function create and return the a dataframe with the name of the section
#' and its starting position inside conllu_df. Simple structure of the function :
#'
#' locate_sections_position_in_conllu() For section name in list of section (identify by the fonts information)
#' lower_but_first_letter(token of conllu_df in section)
#' lower_but_first_letter(token of conllu_df in lower_but_first_letter(section))
#' eliminate_cf_occurrences()
#' adding_missing_first_letter_section()
#' multiple_section_fix()
#' subset_occurrences()
#' handle_typos()
#' is_summary_box()
#'
#' @inheritParams extract_section_from_conllu
#' @inheritParams reduce_occurrences
#'
#' @export
#' @return The position of the section in the Conllu data structure.
locate_sections_position_in_conllu <- function(conllu_df, section_title_df) {
  # reduce_occurrences() use the order of the sections inside the document and NLP approach to reduce the number
  # of occurrences to one, i.e. to select among the different occurrences of a section title which one
  # correspond to the section title. More description in their documentation.
  # Update tabulizer : after using tabulizer to read the pdf, some gotchas seem to appear in some section title :
  # in Abrams et al 2010, "Introduction" became "IntroductIon". Token was impossible to find.
  # Tolower() is added to keep using token. Lemma would require playing with the plurals.

  positions_sections_df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("section", "occurrences"))

  for (section in section_title_df$Word) {
    occurrences <- which(lower_but_first_letter(conllu_df$token) %in% section)
    if (length(occurrences) == 0) { # if several time the section name in the article
      occurrences <- which(lower_but_first_letter(conllu_df$token) %in% lower_but_first_letter(section))
    }
    occurrences <- eliminate_cf_occurrences(conllu_df, occurrences)
    occurrences <- adding_missing_first_letter_section(conllu_df, section, occurrences)
    occurrences <- subset_occurrences(occurrences, positions_sections_df)
    occurrences <- handle_typos(conllu_df, section, occurrences)
    occurrences <- is_summary_box(conllu_df, section, occurrences, section_title_df)
    if (length(occurrences) > 1) { # if several time the section name in the article
      occurrences <- subset_occurrences(occurrences, positions_sections_df)
    }
    if (length(occurrences) > 1) { # if several time the section name in the article
      occurrences <- reduce_occurrences(conllu_df, occurrences, positions_sections_df, section_title_df)
    }
    positions_sections_df <- rbind(positions_sections_df, data.frame(section, occurrences))
    positions_sections_df <- multiple_section_fix(positions_sections_df)
  }
  return(merging_section(positions_sections_df))
}

#' Lower all the letter except the first one
#'
#' Correct strange behavior of tabulizer for section titles, by lowering all the
#' letters except the first one which is left uncorrected.
#' "MAterIAls" become "Materials". This function allow to find tokens such as
#' "MAterIAls" when searching for "Material", while avoiding to find token such
#' as "material".
#'
#' @param token A string of character, which is a token inside the ConLL-U
#' dataframe.
#'
#' @return A string of character corrected.
#'
#'
lower_but_first_letter <- function(token) {
  first_letter <- substring(token, 1, 1)
  body_word <- tolower(substring(token, 2))
  token <- paste0(first_letter, body_word)
  return(token)
}

#' Remove the occurrences of section title that are preceded by a cf occurrence
#'
#' This function remove the occurrences of a section title that are tokens
#' preceded by tokens that indicate a confer context (i.e. "compare" or "consult")
#' such as "see", "cf", "discussed", "described".
#' Simply speaking, it allow the calling function, locate_sections_position_in_conllu(),
#' to ignore sentence such as "For more details, please see material and method
#' section".
#' Note created during the documentation : this function could be improved :
#' - by extending the range to look for the tokens, to handle sentence such as
#' "please see the material and method section".
#' - including word such as "refer".
#'
#' @inheritParams extract_section_from_conllu
#' @inheritParams handle_typos
#'
#' @return
#'
#'
eliminate_cf_occurrences <- function(conllu_df, occurrences) {
  #' Note created during the documentation : this function could be improved :
  #' - by extending the range to look fot the tokens, to handle sentence such as
  #' "please see the material and method section".
  #' - including word such as "refer".
  occurrences_without_cf <- c()
  for (occur in occurrences) {
    word_before <- conllu_df[(occur - 1), ]$token
    if (word_before == "see") {
      next
    }
    if (word_before == "cf") {
      next
    }
    if (word_before == "in" | word_before == "the") {
      head_token <- grep_head_token(conllu_df, occur)
      if (head_token == "discussed") {
        next
      }
      if (head_token == "described") {
        next
      }
    }
    # if not next addition to the occurrences without the
    # entries that are just references to a section
    occurrences_without_cf <- c(occurrences_without_cf, occur)
  }
  occurrences <- occurrences_without_cf
  return(occurrences)
}

#' Send back occurrences of a truncated name of the section title
#'
#' Please also refer to the function missing_first_letter_section().
#' adding_missing_first_letter_section() is called before
#' missing_first_letter_section() in handle_typos().
#' #' In one of article of Elsevier, "Al-Bairuty, G et al 2013.pdf",
#' "Acknowledgements" became "cknowledgements" and "Reference", "eference".
#' This function search for tokens that correspond to truncated name of
#' the section title. If there is occurrences for truncated versions, the function
#' send back to the main functions the new occurrences.
#'
#' @inheritParams capitalize_first_letter
#' @inheritParams extract_section_from_conllu
#' @inheritParams handle_typos
#'
#' @return Occurrences
#'
#'
adding_missing_first_letter_section <- function(conllu_df, section, occurrences) {
  section <- tolower(substring(section, 2))
  new_occurrences <- which(conllu_df$token %in% section)
  if (length(new_occurrences) > 0) { # send back only if it exist
    occurrences <- c(occurrences, new_occurrences)
    return(occurrences)
  }
  return(occurrences)
}

#' Reduce the search space of section titles
#'
#' Simply speaking, it reduce the search of section names to portion of the
#' article after the already annotated section titles. In Abrams et al 2010,
#' "Introduction" is at position 338, "Results" at 1501, "Discussion" at 5975 and
#' there is occurrences of "Materials" at 1799, 2747, 3816, 7055.
#' The three firsts ones are likely part of the "Results" section.
#'
#' @inheritParams handle_typos
#' @inheritParams extract_section_from_conllu
#'
#' @return
#'
subset_occurrences <- function(occurrences, positions_sections_df) {
  # Following : if it is not the first iteration of the loop
  # i.e. if a section has already been positioned
  if (dim(positions_sections_df)[1] > 0) {
    occurrences <- occurrences[which(occurrences > max(positions_sections_df$occurrences))]
    return(occurrences)
  }
  else {
    return(occurrences)
  }
}

#' Reduce the number of potential localization of section titles
#'
#' This function reduce the number of occurrences that can be a localization of
#' section titles. This function successively call subset_occurrences(),
#' select_first_token_sentence_if_section_title() and select_first_lemma_sentence().
#'
#'
#' @inheritParams extract_section_from_conllu
#' @inheritParams handle_typos
#' @param section_title_df  A dataframe with the sections title that has been
#' found inside the article, their position inside the output of poppler, the
#' font and the size of the font.
#' Word                         Font                 Size
#' 293     Introduction VMUQDX+ITCStoneSans-Semibold 10.0
#' 1321         Results VMUQDX+ITCStoneSans-Semibold 10.0
#'
#' @return
#'
reduce_occurrences <- function(conllu_df, occurrences, positions_sections_df, section_title_df) {
  if (length(occurrences) > 1) { # if several time the section name in the article
    occurrences <- subset_occurrences(occurrences, positions_sections_df)
  }
  if (length(occurrences) > 1) { # if there is still several time the section name in the article
    occurrences_NLP <- select_first_token_sentence_if_section_title(conllu_df, occurrences, positions_sections_df)
    if (length(occurrences_NLP) > 0) { # if not NULL, like for Methods in Materials and Methods
      occurrences <- occurrences_NLP
    }
  }
  if (length(occurrences) > 1) { # if there is still several time the section name in the article
    occurrences_NLP <- select_first_lemma_sentence(conllu_df, occurrences)
    if (length(occurrences_NLP) > 0) { # if not NULL, like for Methods in Materials and Methods
      occurrences <- occurrences_NLP
    }
  }
  # if (length(occurrences)>1){ #if there is still several time the section name in the article
  #   occurrences<-select_first_token_sentence_if_section_title(conllu_df, occurrences, section_title_df)}
  return(occurrences)
}


#' Return first token of the sentence if inside section title df
#'
#' This function call select_if_firsts_tokens_already_in_section_title() for
#' each of the index passed as input.
#' It returns the first occurrence for which the first token of the sentence is
#' already in the dataframe section_title_df.
#'
#' For example, if there is a Material and Methods section, and several time the
#' words methods in the article, the others functions cannot distinguished which
#' occurrence to select (since it is not the first lemma of a sentence).
#'
#' @inheritParams extract_section_from_conllu
#' @inheritParams handle_typos
#'
#' @return
select_first_token_sentence_if_section_title <- function(conllu_df, occurrences, positions_sections_df) {
  # Could be rewritted as a recursive call of select_if_firsts_tokens_already_in_section_title.
  for (index in occurrences) {
    if (select_if_firsts_tokens_already_in_section_title(conllu_df, index, positions_sections_df)) {
      return(index)
    }
  }
}

#' Send true if the first token of the sentence is already inside the section titles
#'
#' This function check if the first token is already in positions_sections_df.
#'
#' @inheritParams extract_section_from_conllu
#' @param index A int, position of a token inside the conllu_df.
#' @return
#' @export
#'
#'
select_if_firsts_tokens_already_in_section_title <- function(conllu_df, index, positions_sections_df) {
  # Could be rewritten as a recursive call to remove the use of select_first_token_sentence_if_section_title.
  # conllu_df[index,] return a token and all the associated data : lemma, but also sentence and doc_id
  occurrence <- conllu_df[index, ] # conllu_df[index,], where conllu_df is the dataframe of annotation generated by udpipe
  token_id <- occurrence$token_id
  token_id <- as.numeric(token_id)
  sentence_id <- occurrence$sentence_id
  first_tokens <- conllu_df[which(conllu_df$sentence_id == sentence_id)[1:(token_id - 1)], ]$token
  # update tabulizer : tolower can lead to unexpected results
  # doubling the if is a quick and safe fix
  for (token in first_tokens) {
    if (token %in% positions_sections_df$section) {
      return(TRUE)
    }
    if (capitalize_first_letter(token) %in% positions_sections_df$section) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Return an occurrence is first lemma of a sentence
#'
#' This function return the first occurrence passed as input which is the first
#' lemma of a sentence. Typically, it distinguish between sentence "northern blot
#' as describe in Materials and Methods" and sentence like "Materials and Methods",
#' the second one being the materials and methods section.
#'
#' @inheritParams extract_section_from_conllu
#' @inheritParams handle_typos
#'
#' @return A lesser list of occurrences
select_first_lemma_sentence <- function(conllu_df, occurrences) {
  for (index in occurrences) {
    # if (filter_first_lemma(conllu_df, index)){return(index) #Old
    if (recursive_filter_first_lemma(conllu_df, index, lemma_nb = 1)) {
      return(index)
    }
  }
}

#' Check if the first lemma of the sentence is the lemma at the position index
#'
#' Called by select_first_lemma_sentence(). Check if the first lemma of the
#' sentence is the lemma at the position index. The function is a recursive call,
#' the recursion lasting as long as the regex expressions are true.
#' The regex allow to handle section's title such as "1) Material and methods",
#' since "1" and ")" would be annotated as token/lemma by udpipe, the goal of the
#' function being to identify if the index corresponding to "Material" is the
#' first lemma of the sentence.
#' When the recursive call is made, the lemma at the index is compared to the
#' following lemma of the sentence, compared to the previous call.
#' The regex are : detect punctuation, detect decimal, detect character string
#' "Animals|Animal" (to handle a really particular article).
#' @inheritParams extract_section_from_conllu
#' @param index A int. Similar to occurrences, indicate the position of a token
#' and all the information related to it such as : lemma, sentence, head_token,
#' etc. Use here to not create confusion with the variable occurrence.
#' @param lemma_nb Variable used for the recursive call, allow to advance from
#' one lemma to the next in the sentence between each call.
#'
#' @return
#'
#'
recursive_filter_first_lemma <- function(conllu_df, index, lemma_nb) {
  # conllu_df[index,], where conllu_df is the dataframe of annotation generated by udpipe
  occurrence <- conllu_df[index, ]
  lemma <- conllu_df[index, ]$lemma
  sentence_id <- occurrence$sentence_id
  first_lemma <- conllu_df[which(conllu_df$sentence_id == sentence_id)[lemma_nb], ]$lemma
  if (stringr::str_detect(first_lemma, "\\d")) {
    lemma_nb <- lemma_nb + 1
    return(recursive_filter_first_lemma(conllu_df, index, lemma_nb))
  }
  if (stringr::str_detect(first_lemma, "[:punct:]")) {
    lemma_nb <- lemma_nb + 1
    return(recursive_filter_first_lemma(conllu_df, index, lemma_nb))
  }
  if (stringr::str_detect(capitalize_first_letter(first_lemma), "Animals|Animal")) { # "De Jong, WH et al 2008.pdf"
    lemma_nb <- lemma_nb + 1
    return(recursive_filter_first_lemma(conllu_df, index, lemma_nb))
  }
  if (first_lemma == lemma) {
    return(TRUE)
  }
  return(FALSE)
}

#' Detect and handle the summary box
#'
#' In "Berce, C et al 2016.pdf", but also in many other article, there is short
#' summary of the article in a box at the beginning of the article with sections
#' names. Depending of the content of this "summary box", this can lead the main
#' function of the package to fail to get meaningful result or to extract anything
#' at all. The function focus on the "introduction" section of the summary box,
#' since the function will ignore the box if the section introduction is absent
#' from the summary box : the function will localize the Introduction section,
#' usually after the summary box, and then ignore the summary box when looking
#' for the other section due to the subset_occurrence() function.
#'
#' @inheritParams extract_section_from_conllu
#' @inheritParams handle_typos
#' @inheritParams reduce_occurrences
#'
#' @return
#'
is_summary_box <- function(conllu_df, section, occurrences, section_title_df) {
  if (section == "Introduction" & length(occurrences) > 1) {
    putative_summary_box <- conllu_df[occurrences[1]:occurrences[2], ]

    occur_conclusion <- which(capitalize_first_letter(putative_summary_box$token) %in%
      c("Conclusions", "Conclusion", "CONCLUSION", "CONCLUSIONS"))

    if (length(occur_conclusion) == 1) {
      if (putative_summary_box[occur_conclusion + 1, ]$token == ":" |
        putative_summary_box[occur_conclusion + 1, ]$token == ":") {
        occurrences <- occurrences[2]
        message("Is_summary_box() has been called")
        return(occurrences)
      }
    }
  }
  return(occurrences)
}

#' Merge duplicated sections
#'
#' If a section appear in duplicate positions_sections_df, raise a message and
#' merge the duplicated sections titles.
#'
#' > positions_sections_df
#' section occurrences
#' 1 Introduction         238
#' 2      Results        1091
#' 3   Discussion        4839
#' 4   Discussion        9786
#' 5   Discussion        9805
#' to
#' 1 Introduction         238
#' 2      Results        1091
#' 3   Discussion        4839
#'
#' @inheritParams extract_section_from_conllu
#'
#' @return positions_sections_df
multiple_section_fix <- function(positions_sections_df) {
  df <- as.data.frame(table(positions_sections_df$section))
  if (max(df$Freq) > 1) {
    message("Some sections seems to have been detected in multiple occurence")
    new_size <- length(unique(positions_sections_df$section))
    positions_sections_df <- positions_sections_df[1:new_size, ]
  }
  return(positions_sections_df)
}
