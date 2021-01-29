#' Find what are the sections and in which order they appear
#'
#' The function call successively find_section_titles() on each vector of the list.
#' The function then call re_identify_font_section(), in case Acknowledgment and
#' References (if they exist) have a different font from the other sections titles.
#' This function will change the output only if the section_title_df contain 2
#' or less section.
#'
#' @inheritParams identify_font
#' @param list_of_sections A list of vector of string, that contain the sections
#' titles we want to localize in the article, if they exist.
#' @return A dataframe with the sections title that has been found inside the article,
#' their position inside the output of poppler, the font and the size of the font.
#' Word                         Font                 Size
#' 293     Introduction VMUQDX+ITCStoneSans-Semibold 10.0
#' 1321         Results VMUQDX+ITCStoneSans-Semibold 10.0
#' @export
#'
#'
create_section_title_df <- function(font_section, list_of_sections, poppler_output) {
  # create and return a dataframe with the section, their font and the size of the font like this :
  # Word                         Font                 Size
  # 293     Introduction VMUQDX+ITCStoneSans-Semibold 10.0
  # 1321         Results VMUQDX+ITCStoneSans-Semibold 10.0
  section_title_df <- data.frame()
  for (vector_title in list_of_sections) {
    section_title_df <- rbind(section_title_df, find_section_titles(vector_title, font_section, poppler_output))
  }

  section_title_df <- re_identify_font_section(poppler_output, section_title_df, list_of_sections)
  section_title_df <- section_title_df[order(as.numeric(row.names(section_title_df))), ]
  return(section_title_df)
}

#' Find if a section (vector of aliases) exist and where it is located
#'
#' The function check first if the word exist, and if the word with the maximum
#' size inside the text has the same font than the font of the titles. If this
#' fail, clean_font_txt() to remove the smallest font size from poppler_output.
#' Then recursive call.
#'
#' @inheritParams identify_font
#' @param vector_title A vector of string, that identify the different names of
#' a section title we aim to localise in the article, such as
#' c("Conclusions", "Conclusion", "CONCLUSION", "CONCLUSIONS").
#' @param font_section The font of the section as identified by identify_font().
#'
#' @return Position of the title section if exist inside poppler_output.
#'
#'
find_section_titles <- function(vector_title, font_section, poppler_output) {
  assumed_title_df <- poppler_output[which(poppler_output$Word %in% vector_title), ]

  if (dim(assumed_title_df)[1] > 0) { # if section exist #select the word with max size
    assumed_title_df <- assumed_title_df[which(assumed_title_df$Size == max(assumed_title_df$Size)), ]

    if (dim(assumed_title_df)[1] > 1) { ## if there is several words with same max size #which has font section
      assumed_title_df <- assumed_title_df[which(assumed_title_df$Font == font_section), ]
    }

    if (dim(assumed_title_df)[1] > 0) { # if there were indeed a section title
      if (assumed_title_df$Font == font_section) {
        return(assumed_title_df)
      }
    }

    if (dim(assumed_title_df)[1] == 0) { # if there is nothing, retry but with the font of text removed
      clean_df_poppler <- clean_font_txt(poppler_output)
      rm(poppler_output) # atavism due to a global variable in earlier versions
      poppler_output <- clean_df_poppler
      message("Clean_font_txt_() has been called in find_section_titles")
      return(find_section_titles(vector_title, font_section, poppler_output))
    }
  }
}

#' Re identify the font for the sections titles
#'
#' When Abstract and References has a different font than the other section
#' (it happens in some journals). This function check if the section_df is too
#' small (length =< 2) and looks for the font of "Introduction", "Abstract" or
#' "Background" in poppler output (if it exist in one occurrence only).
#' The function next call find_section_titles() with the new font, and return
#' the new section_title_df merge with the existing one to the calling function,
#' create_section_title_df().
#' Function initially made for "Campagnolo, L et al 2013.pdf".
#'
#' @inheritParams identify_font
#' @inheritParams reduce_occurrences
#' @inheritParams create_section_title_df
#'
#' @return
#'
#'
re_identify_font_section <- function(poppler_output, section_title_df, list_of_sections) {
  if (length(section_title_df$Word) <= 2) {
    introduction_df <- poppler_output[which(capitalize_first_letter(poppler_output$Word) %in%
      c("Introduction")), ]
    abstract_df <- poppler_output[which(capitalize_first_letter(poppler_output$Word) %in%
      c("Abstract")), ]
    background_df <- poppler_output[which(capitalize_first_letter(poppler_output$Word) %in%
      c("Background")), ]

    if (dim(introduction_df)[1] == 1) { # if Introduction exist ONE time
      new_font_section <- introduction_df$Font
      # section_title_df<-create_section_title_df(font_section, list_of_sections, poppler_output)
      for (vector_title in list_of_sections) {
        section_title_df <- rbind(section_title_df, find_section_titles(vector_title, new_font_section, poppler_output))
      }
    }
    if (dim(introduction_df)[1] != 1 & dim(abstract_df)[1] == 1) { # if Introduction exist ONE time
      new_font_section <- abstract_df$Font
      # section_title_df<-create_section_title_df(font_section, list_of_sections, poppler_output)
      for (vector_title in list_of_sections) {
        section_title_df <- rbind(section_title_df, find_section_titles(vector_title, new_font_section, poppler_output))
      }
    }
    if (dim(introduction_df)[1] != 1 & dim(abstract_df)[1] != 1 & dim(background_df)[1] == 1) { # if Introduction exist ONE time
      new_font_section <- background_df$Font
      # section_title_df<-create_section_title_df(font_section, list_of_sections, poppler_output)
      for (vector_title in list_of_sections) {
        section_title_df <- rbind(section_title_df, find_section_titles(vector_title, new_font_section, poppler_output))
      }
    }
  }
  return(section_title_df)
}
