% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulateNDRa.R
\name{simulateNDRa}
\alias{simulateNDRa}
\title{Generate simulated naming latencies and pronunciations}
\usage{
simulateNDRa(lexicon = lex, weightsSem = weights_sem,
  weightsPhon = weights_phon, parameters = list(wSem = 0.2, wPhon1 =
  0.05, wPhon2 = 0.098, wH = 0.152, wCompl = 1.27, backoff = 0.01, wlex =
  4.7, N = 20, wAct = 0.055, rtConst = 450), parallel = TRUE,
  numCores = detectCores(), verbose = TRUE)
}
\arguments{
\item{weightsSem}{An orthography-to-semantics weight matrix with letter unigrams and 
bigrams as cues and words as outcomes. The default, "weights_sem" uses 
the weight matrix from Hendrix et al. (2018).}

\item{weightsPhon}{A phonology-to-semantic weight matrix. The default, "weigths_phon" uses 
the weight matrix from Hendrix (2018).}

\item{parameters}{A list with the model parameters "wSem", "wPhon1", "wPhon2", "wH", "wCompl", 
"backoff", "wlex", "N", "wAct", and "rtConst". The default values are the values used 
by Hendrix (2018). 
For more information, also see Hendrix (2018).}

\item{parallel}{Should computations be carried out in parallel? Defaults to TRUE.}

\item{numCores}{The number of cores to use for parallel computation. By default all 
available cores are used.}
}
\description{
Generate simulated pronunciation for a set of words or nonwords.
lexicon A dataframe with the colums "Word" and "Gestures". "Gestures" are 
demi-syllables (see Klatt, 1979).
}
\examples{
# Load data for the ELP simulations in Hendrix (2018)
data(elp)

# Generate simulated naming latencies and pronunciations
elp = simulateNDRa(elp)

}
\references{
Hendrix, P, Ramscar, M., & Baayen, R. H. (2019). NDRa: a single route model of 
response times in the reading aloud task based on discriminative learning. Manuscript.

Klatt, D. H. (1979). Speech perception: a model of acoustic-phonetic analysis and 
lexical access. Journal of Phonetics, 7, 279-312.
}
