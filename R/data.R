#' Frankenstein Data Frame
#'
#' This data frame provides the full text of Frankenstein by Mary Shelley.
#' The data frame provides information about each line of text including the following: The line that the text is on in the book. The type of section: 1) "misc" represents the text before the book begins including the table of contents, 2) "letter" represents that the section is one of the four written letters, and 3) "chapter" represents one of the 24 chapters in the novel. The section number corresponding to letter number or chapter number. A "0" depicts an unnumbered section.
#'
#' @format ## `Frankenstein`
#' A data frame with 7357 rows and 4 columns:
#' \describe{
#'   \item{line}{line number}
#'   \item{text}{the line of text}
#'   \item{section}{the section number}
#'   \item{section_type}{the type of section: misc, letter or chapter}
#'   ...
#' }
#' @source <https://www.gutenberg.org/files/84/84-h/84-h.htm>
"Frankenstein"
#'
#' Annotated Frankenstein Data Frame
#' This data frame provides the annotated text of Frankenstein. Using token shows each token and part of speech with other variables. Using enity shows the specific entities such as countries or names. Document shows each document that the text is split into.
#'
#' @format ## `anno_Frankenstein`
#' 3 elements
#' \describe{
#'   \item{token}{A tibble with 85810 rows and 10 columns}
#'   \item{entity}{A tibble with 1643 rows anf 6 columns}
#'   \item{document}{A tibble with 6387 rows and 1 column}
#'   ...
#' }
#' @source annotate using cleanNLP on Frankestein text
"anno_Frankenstein"



