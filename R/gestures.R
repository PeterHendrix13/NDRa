#' Define gestures
#'
#' Generate gestures for a word or nonword.
#' @param
#' pron The pronunciation of a word or nonword in DISC notation
#' 
#' @export
#' @examples
#' # Load data for the ELP simulations in Hendrix (2018)
#' data(elp)
#' 
#' # Define gestures
#' elp$Gestures = sapply(elp$Phon, gestures)

# Define gestures
gestures = function(pron = "b8r") {
  
  # Chop pronunciation into parts
  parts = unlist(strsplit(pron, ""))
  
  # Get the position of the vowel
  numVowel = which(parts %in% vowelsDISC)
  
  # Define the first demi-syllable
  first = paste(parts[1:numVowel], collapse = "")
  
  # Define the second demi-syllable
  second = paste(parts[numVowel:length(parts)], collapse = "")
  
  # Define gestures
  gestures = paste("[", first, "_", second, "]", sep = "")
  
  # Return
  return(gestures)
  
}
