% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locate_sections_position_in_ConllU.R
\name{multiple_section_fix}
\alias{multiple_section_fix}
\title{Merge duplicated sections}
\usage{
multiple_section_fix(positions_sections_df)
}
\arguments{
\item{positions_sections_df}{A dataframe with the positions of the sections
currently localized inside the conllu_df.}
}
\value{
positions_sections_df
}
\description{
If a section appear in duplicate positions_sections_df, raise a message and
merge the duplicated sections titles.
}
\details{
positions_sections_dfsection occurrences1 Introduction         2382      Results        10913   Discussion        48394   Discussion        97865   Discussion        9805to1 Introduction         2382      Results        10913   Discussion        4839
}
