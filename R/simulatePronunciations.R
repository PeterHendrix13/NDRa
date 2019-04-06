#' Generate simulated pronunciations
#'
#' Generate simulated pronunciation for a set of words or nonwords.
#' @param
#' lexicon A dataframe with the colums "Word" and "Gestures". "Gestures" are 
#' demi-syllables (see Klatt, 1979) and can be generated using gestures().
#' @param
#' weightsSem An orthography-to-semantics weight matrix with letter unigrams and 
#' bigrams as cues and words as outcomes. The default, "weights_sem" uses 
#' the weight matrix from Hendrix et al. (2018).
#' @param
#' weightsPhon A phonology-to-semantic weight matrix with demi-syllables as 
#' cues and words as outcomes. The default, "weigths_phon" uses the weight matrix 
#' from Hendrix et al. (2018).
#' @param
#' parallel Should computations be carried out in parallel? Defaults to TRUE.
#' @param
#' numCores The number of cores to use for parallel computation. By default all 
#' available cores are used.
#' 
#' @export
#' @examples
#' # Load data for the ELP simulations in Hendrix (2018)
#' data(elp)
#' 
#' # Generate simulated pronunciations for a lexicon
#' elp$SimPron = simulatePronunciations(elp)
#' 
#' @references
#' Hendrix, P, Ramscar, M., & Baayen, R. H. (2019). NDRa: a single route model of 
#' response times in the reading aloud task based on discriminative learning. Manuscript.
#'
#' Klatt, D. H. (1979). Speech perception: a model of acoustic-phonetic analysis and 
#' lexical access. Journal of Phonetics, 7, 279-312.

simulatePronunciations = function(lexicon = lex,
                                  weightsSem = weights_sem,
                                  weightsPhon = weights_phon,
                                  parallel = TRUE,
                                  numCores = detectCores(),
                                  verbose = TRUE) {
  
  # Set verbose options
  if (verbose) {
    pboptions(type = "timer", char = "=")
    cat("Simulating pronunciations\n")
  } else {
    pboptions(type = "none")
  }
  
  # Initialize weight matrices
  ws = weightsSem
  wp = weightsPhon
  
  # Simulate pronunciations
  if (parallel) {
    prons = unlist(pblapply(lexicon$Word, simulatePronunciation,
      ws, wp, cl = numCores))
  } else {
    prons = pbsapply(lexicon$Word, simulatePronunciation,
      ws, wp, USE.NAMES = FALSE)
  }
  
  # Return
  return(prons)
  
}
