#' Generate simulated naming latencies and pronunciations
#'
#' Generate simulated pronunciation for a set of words or nonwords.
#' lexicon A dataframe with the colums "Word" and "Gestures". "Gestures" are 
#' demi-syllables (see Klatt, 1979).
#' @param
#' weightsSem An orthography-to-semantics weight matrix. The default, "weights_sem" uses 
#' the weight matrix from Baayen et al. (2011).
#' @param
#' weightsPhon A phonology-to-semantic weight matrix. The default, "weigths_phon" uses 
#' the weight matrix from Hendrix (2018).
#' @param
#' parameters A list with the model parameters "wSem", "wPhon1", "wPhon2", "wH", "wCompl", 
#' "backoff", "wlex", and "N". The default values are the values used by Hendrix (2018). 
#' For more information, also see Hendrix (2018).
#' @param
#' parallel Should computations be carried out in parallel? Defaults to TRUE.
#' @param
#' numCores The number of cores to use for parallel computation. By default all 
#' available cores are used.
#' 
#' @import ndl pbapply
#' @export
#' @examples
#' # Load data for the ELP simulations in Hendrix (2018)
#' data(elp)
#' 
#' # Generate simulated naming latencies and pronunciations
#' elp$SimPron = simulate(elp$Word)
#' 
#' @references
#' Baayen, R. H., Milin, P., Filipovic-Durdevic, D., Hendrix, P., & Marelli, M. (2011). 
#' An amorphous model for morphological processing in visual comprehension based on naive 
#' discriminative learning. Psychological review, 118(3), 438.
#' 
#' Hendrix, P, Ramscar, M., & Baayen, R. H. (2019). NDRa: a single route model of 
#' response times in the reading aloud task based on discriminative learning. Manuscript.
#'
#' Klatt, D. H. (1979). Speech perception: a model of acoustic-phonetic analysis and 
#' lexical access. Journal of Phonetics, 7, 279-312.

simulateNDRa = function(lexicon = lex,
                        weightsSem = weights_sem,
                        weightsPhon = weights_phon,
                        parameters = list("wSem" = 0.20,
                                          "wPhon1" = 0.050,
                                          "wPhon2" = 0.098,
                                          "wH" = -0.152,
                                          "wCompl" = 1.27,
                                          "backoff" = 0.01,
                                          "wlex" = 4.7,
                                          "N" = 20),
                        parallel = TRUE,
                        numCores = detectCores(),
                        verbose = TRUE) {

  # Simulate naming latencies
  lexicon$SimRT = simulateRTs(lexicon, weightsSem, weightsPhon, parameters,
    verbose)

  # Simulate pronunciations
  lexicon$SimPron = simulatePronunciations(lexicon, weightsSem, weightsPhon,
    parallel, numCores, verbose)
  
  # Return
  return(lexicon)                      

}
