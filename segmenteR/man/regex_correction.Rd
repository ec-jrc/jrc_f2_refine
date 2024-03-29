% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handle_typos.R
\name{regex_correction}
\alias{regex_correction}
\title{Search for token that comprise a section inside it}
\usage{
regex_correction(conllu_df, section)
}
\arguments{
\item{conllu_df}{A dataframe of the raw text of the article annotated using
the model, in the CoNLL-U format.}

\item{section}{A string, a section name.}
}
\description{
Called by handle_typos(). In "Attia, AB et al 2013.pdf", "Acknowledgements"
became "group.Acknowledgments". This function look for section inside tokens.
This only occurs in the Conll-U dataframe.
}
