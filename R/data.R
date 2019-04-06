#' Orthography to semantics weight matrix
#'
#' Orthography to semantics weight matrix generated with version 0.2.18 of the ndl package.
#'
#' @format A matrix with 664 rows and 6,768 columns:
#' \describe{
#'   \item{\code{rows}}{letter unigrams and bigrams}
#'   \item{\code{columns}}{words}
#' }
#' @source Hendrix, P, Ramscar, M., & Baayen, R. H. (2019). NDRa: a single route model of 
#' response times in the reading aloud task based on discriminative learning. Manuscript.
"weights_sem"

#' Phonology to semantics weight matrix
#'
#' Phonology to semantics weight matrix generated with version 0.2.18 of the ndl package.
#'
#' @format A matrix with 1,358 rows and 3,908 columns:
#' \describe{
#'   \item{\code{rows}}{demi-syllables (see Klatt, 1979)}
#'   \item{\code{columns}}{words}
#' }
#' @source Hendrix, P, Ramscar, M., & Baayen, R. H. (2019). NDRa: a single route model of 
#' response times in the reading aloud task based on discriminative learning. Manuscript.
#'
#' @references
#' Klatt, D. H. (1979). Speech perception: a model of acoustic-phonetic analysis and 
#' lexical access. Journal of Phonetics, 7, 279-312.
"weights_phon"

#' Input lexicon ELP simulations
#'
#' The input lexicon used for the ELP simulations in Hendrix (2018).
#'
#' @format A data frame with 2,524 rows and 3 columns:
#' \describe{
#'   \item{\code{Word}}{a mono-syllabic mono-morphemic word}
#'   \item{\code{Gestures}}{an approximation of the acoustic gestures required for the 
#'   pronunciation of the word through demi-syllables (see Klatt, 1979)}
#'   \item{\code{RTnaming}}{the naming latency for the word in the English Lexicon 
#'   Project (ELP, Balota et al. (2007)}
#' }
#' @source Balota, D. A., Yap, M. J., Hutchison, K. A., Cortese, M. J., Kessler, B., 
#' Loftis, B., Neely, J. H., Nelson, D. L., Simpson, G. B., & Treiman, R. (2007). The 
#' English lexicon project. Behavior research methods, 39(3), 445-459.
# 
#' @references
#' Hendrix, P, Ramscar, M., & Baayen, R. H. (2019). NDRa: a single route model of 
#' response times in the reading aloud task based on discriminative learning. Manuscript.
#' 
#' Klatt, D. H. (1979). Speech perception: a model of acoustic-phonetic analysis and 
#' lexical access. Journal of Phonetics, 7, 279-312.
"elp"
