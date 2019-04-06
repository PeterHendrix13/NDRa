#' Generate simulated naming latencies
#' 
#' Generate simulated naming latencies for a set of words or nonwords.
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
#' parameters A list with the model parameters "wSem", "wPhon1", "wPhon2", "wH", "wCompl", 
#' "backoff", "wlex", and "N". The default values are the values used by Hendrix (2018). 
#' For more information, also see Hendrix et al. (2018).
#' 
#' @export
#' @examples
#' # Load data for the ELP simulations in Hendrix (2018)
#' data(elp)
#' 
#' # Generate simulated naming latencies
#' elp$SimRT = simulateSimRTs(elp)
#' 
#' # Evaluate simulated naming latencies
#' cor(elp$SimRT, -1000/elp$RTnaming)
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

simulateRTs = function(lexicon = lex,
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
                       verbose = TRUE) {
  
  # Feedback for start of simulation
  if (verbose) {
    cat("Simulating naming latencies\n")
  }
                                               
  # Define columns necessary for NDL data frame
  if (verbose) {
    cat("   Generating NDL data frame\n")
  }

  lexicon$Cues = orthoCoding(as.character(lexicon$Word), grams = c(1, 2))
  lexicon$Outcomes = lexicon$Word
  lexicon$Frequency = 1
  
  # Calculate activations of words from orthography
  if (verbose) {
    cat("   Calculating semantic activations\n")
  }
  neigh = intersect(colnames(weightsPhon), colnames(weightsSem))
  actsSem = suppressWarnings(estimateActivations(lexicon, weightsSem))
  actsSem = actsSem$activationMatrix[, neigh]
  rownames(actsSem) = lexicon$Word

  # Get activations from orthography for target word and competitors
  fromOrth = lapply(lexicon$Word, activationsOrthography, acts = actsSem,
                    n = parameters$N)
  lexicon$ActLemma = unlist(lapply(fromOrth, 
    FUN = function(x){ as.numeric(x$actLemma) }))
  for (i in 1:parameters$N) {
    lexicon[, paste("ActComp", i, sep = "")] = unlist(lapply(fromOrth,
      FUN = function(x){ as.numeric(x$actComps[i]) }))
    lexicon[, paste("Comp", i, sep = "")] = unlist(lapply(fromOrth,
      FUN = function(x){ names(x$actComps[i]) }))
  }

  # Shift by backoff parameter to avoid division by zero
  lexicon$ActLemmaShifted = lexicon$ActLemma + parameters$backoff
  lexicon$ActLemmaShifted[which(lexicon$ActLemmaShifted <= 
    parameters$backoff)] = parameters$backoff

  # Calculate activations of phonology from semantics
  if (verbose) {
    cat("   Calculating phonological activations\n")
  }
  actsPhon = t(weightsPhon)
  
  # Get activations from semantics for target word and competitors
  fromSem = mapply(activationsSemantics, lexicon$Word, lexicon$Gestures,
                   MoreArgs = list("lexicon" = lexicon, "acts" = actsPhon,
                                     "n" = parameters$N))
  lexicon$PhonAct = unlist(fromSem["actPhon1", ])
  lexicon$PhonAct2 = unlist(fromSem["actPhon2", ])
  lexicon$PhonActComp = unlist(fromSem["actPhon1Comp", ])
  lexicon$PhonAct2Comp = unlist(fromSem["actPhon2Comp", ])
  
  # Apply increased weight for target word
  lexicon$PhonActTotal = parameters$wlex * lexicon$PhonAct + lexicon$PhonActComp
  lexicon$PhonAct2Total = parameters$wlex * lexicon$PhonAct2 +
    lexicon$PhonAct2Comp
  
  # Shift by backoff parameter to avoid division by zero
  lexicon$PhonActTotalShifted = lexicon$PhonActTotal + parameters$backoff
  lexicon$PhonActTotalShifted[which(lexicon$PhonActTotalShifted <=
    parameters$backoff)] = parameters$backoff
  lexicon$PhonAct2TotalShifted = lexicon$PhonAct2Total + parameters$backoff
  lexicon$PhonAct2TotalShifted[which(lexicon$PhonAct2TotalShifted <=
    parameters$backoff)] = parameters$backoff

  # Ensure no activations are smaller than the backoff parameter
  lexicon$PhonActTotalShifted[which(lexicon$PhonActTotalShifted <=
    parameters$backoff)] = parameters$backoff
  lexicon$PhonAct2TotalShifted[which(lexicon$PhonAct2TotalShifted <=
    parameters$backoff)] = parameters$backoff

  # Calculate entropy over the activations of the target demi-syllables
  lexicon$Hnaming = apply(lexicon[, c("PhonActTotalShifted",
    "PhonAct2TotalShifted")], 1, entropy)
  
  # Define visual complexity
  for (i in 1:nrow(lexicon)) {
    lexicon$VisualComplexity[i] = sum(fullsum[unlist(strsplit(lexicon$Word[i],
      ""))])
  }

  # Calculate simulated RTs
  if (verbose) {
    cat("   Calculating simulated reaction times\n")
  }
  lexicon$SimRT =  
    1 / (lexicon$ActLemmaShifted ^ parameters$wSem *
    lexicon$PhonActTotalShifted ^ parameters$wPhon1 *
    lexicon$PhonAct2TotalShifted ^ parameters$wPhon2 * 
    lexicon$Hnaming ^ -parameters$wH *
    lexicon$VisualComplexity ^ -parameters$wCompl)
  
  # Return simulated RTs
  return(lexicon$SimRT)

}
