# Get activations from ortography
activationsOrthography = function(word, acts, n = parameters$N) {
  # Get activation of word
  if (word %in% colnames(acts)) {
    actLemma = acts[word, word]
  # Get activation of nonword
  } else {
    actLemma = 0
  }
  
  # Get activations of competitors
  v = acts[word, ]
  v = v[names(v) != word]
  actComps = rev(sort(v))[1:n]
  
  # Return
  return(list("actLemma" = actLemma, "actComps" = actComps)) 

}


# Get activations from semantics
activationsSemantics = function(word, gestures, lexicon, acts, 
                                   n = parameters$N) {
  
  # Get diphones
  parts = unlist(strsplit(gestures, "_"))
  initphon = parts[1]
  secphon = parts[2]
  
  # Get relevant row of data frame
  tmp = lexicon[which(lexicon$Word == word & lexicon$Gestures == gestures)[1], ]
  
  # Get activation of first diphone
  if (initphon %in% colnames(acts)) {
    if (word %in% rownames(acts)) {
      actPhon1 = acts[word, initphon]
    } else {
      actPhon1 = 0
    } 
    actPhon1Comp = sum(tmp[, paste("ActComp", 1:n, sep = "")] * 
                     as.numeric(acts[as.character(tmp[, paste("Comp", 1:n, 
                       sep = "")]), initphon])
                   )
  } else {
    actPhon1 = 0
    actPhon1Comp = 0
  }
  
  # Get activation of second diphone
  if (secphon %in% colnames(acts)) {
    if (word %in% rownames(acts)) {
      actPhon2 = acts[word, secphon]
    } else {
      actPhon2 = 0
    }
    actPhon2Comp = sum(tmp[, paste("ActComp", 1:n, sep = "")] * 
                   as.numeric(acts[as.character(tmp[, paste("Comp", 1:n, 
                     sep = "")]), secphon]))
  } else {
    actPhon2 = 0
    actPhon2Comp = 0
  }
  
  # Return
  return(list("actPhon1" = actPhon1, "actPhon2" = actPhon2,
              "actPhon1Comp" = actPhon1Comp, "actPhon2Comp" = actPhon2Comp)) 

}


# Entropy
entropy = function(v) {
  p = v / sum(v)
  return(-sum(p * log2(p)))
}


# Check if all cues are in the training data
inTraining = function(cues, gestures, weightsSem, weightsPhon) {
  
  # Check if there are missing orthographic cues
  missingOrthographicCues = sum(!(unlist(strsplit(cues, "_")) %in% 
    rownames(weightsSem))) > 0
  
  # Check of there are missing phonological cues
  missingPhonologicalCues = sum(!(unlist(strsplit(gestures, "_")) %in% 
    rownames(weightsPhon))) > 0
  
  # Return
  if (missingOrthographicCues | missingPhonologicalCues) {
    return(FALSE)
  } else {
    return(TRUE)
  }
  
}


# Get vowel position
vowelPosition = function(word) {

  # Define variables
  lets = unlist(strsplit(word, ""))
  vowels = c("a", "i", "e", "u", "o")
  firstvowel = nchar(word) + 1
  lastvowel = nchar(word) + 1 
  
  # Find non word-initial y
  if ("y" %in% lets[2:nchar(word)]) {
    firstvowel = which(lets[2:nchar(word)] == "y")[1] + 1
    lastvowel = which(lets[2:nchar(word)] == "y")[1] + 1
  }
  
  # Find any other vowel if that precedes the non word-initial y
  if (grepl("[aeiou]", word)) {
    if (which(lets %in% vowels)[1] < firstvowel || is.na(firstvowel)) {
      firstvowel = which(lets %in% vowels)[1]
      lastvowel = which(lets %in% vowels)[length(which(lets %in% vowels))]
    }
  }
  
  # Make sure words with multiple orthographic vowels are processed correctly
  if (firstvowel != nchar(word) & lastvowel != (firstvowel + 1)) {
    if (lets[firstvowel + 1] %in% vowels) {
      if (nchar(word) != (firstvowel + 1)) {
        if (lets[firstvowel + 2] %in% vowels) {
          lastvowel = firstvowel + 2
        } else {
          lastvowel = firstvowel + 1
        }
      } else {
        lastvowel = firstvowel + 1
      }
    } else {
      lastvowel = firstvowel
    }
  }
  
  # Return
  return(list("firstvowel" = firstvowel, "lastvowel" = lastvowel))
  
}


# Get table of candidate diphones
candidatesTable = function(w, diphone="first") {

  # Make table
  tab = rev(sort(colSums(w)))
  
  # Restrict to word-initial or word-final diphones
  if (diphone == "first") {
    tab = tab[substr(names(tab), 1, 1) == "["]
  } else {
    tab = tab[substr(names(tab), nchar(names(tab)), nchar(names(tab))) == "]"]  
  }
  
  # Round
  tab = round(tab, 10)
  
  # Return
  return(tab)
  
}


# Get first demi-syllable
firstDemisyllable = function(word, weightsPhon, weightsSem, wordsSem, 
                             wordsPhon, firstvowel, compsearly, 
                             actcompsearly, compsoffset, compslate) {
  
  # Define variables
  cues = unlist(strsplit(orthoCoding(word, grams = c(2)), "_"))
  vowels = c("a", "e", "i", "o", "u", "y")

  # Get activations
  w = t(weightsPhon[, names(actcompsearly)]) * actcompsearly
  tab = candidatesTable(w, diphone = "first")

  # Select the demi-syllable(s) with the highest activation as the candidate(s)
  tab = tab[tab == max(tab)]
  candidates = names(tab)  
  
  # Resolve ties
  if (length(candidates) > 1) {

    # For real words we still have the option of exploring neighbors
    actcompsearly = sapply(compsearly, FUN = function(x) {
        sum(weightsSem[cues, x])
      })
    w = t(weightsPhon[names(tab), names(actcompsearly)]) * actcompsearly
    tab = candidatesTable(w, diphone = "first")
    tab = tab[tab == max(tab)]
    
    # Resolve additional ties
    if (length(tab) > 1) {
      
      # Update candidates
      candidates = candidates[which(candidates %in% names(tab))]
      candidatescons = substr(candidates, 1, nchar(candidates) - 1)
      
      # Determine the optimal size
      for (try in 1:(firstvowel - 1)){
        compsonset = wordsSem[which(substr(wordsSem, 1, (firstvowel - try)) == 
          substr(word, 1, firstvowel - try))]
        if (try == 1) {
          compsonset = compsonset[which(substr(compsonset, firstvowel, 
            firstvowel) %in% vowels)]
        }
        compsonset = compsonset[which(compsonset %in% wordsPhon)]
        if (length(compsonset) != length(compsearly)) {
          break
        }
      }
      
      # Recalculate activations
      actcompsonset = sapply(compsonset, FUN = function(x) {
          sum(weightsSem[cues, x])
        })
      w = t(weightsPhon[, names(actcompsonset)]) * actcompsonset
      
      # Select the demi-syllable(s) with the highest activation
      tab = candidatesTable(w, diphone = "first")
      sub = which(substr(names(tab), 1, nchar(names(tab)) - 1) %in%
        candidatescons)
      sel = which(tab[sub] == max(tab[sub]))
      if (length(sel) > 1) {
        nextcand = vector()
        for (i in 1:length(sel)) {
         substr = substr(names(sel)[i], 1, nchar(names(sel)[i]) - 1)
         sub
         nextcand[i] = grep(paste("\\", substr, sep = ""), names(tab[sub]))[2]
        }
        sel = which(nextcand == min(nextcand))
      } else {
        sel = 1
      }
      candidates = candidates[which(candidatescons ==
        substr(names(tab)[sub[sel]], 1, nchar(names(tab)[sub[sel]]) - 1))]
      tab = tab[candidates]
      tab = tab[tab == max(tab)]
      candidates = names(tab)
    } else {
      candidates = names(tab)
    }
  }
  
  # Define selected demi-syllable
  first = candidates
  
  # Return
  return(first)
  
}


# Get second demi-syllable
secondDemisyllable = function(word, weightsPhon, weightsSem, wordsSem,
                              wordsPhon, lastvowel, offset, compslate,
                              actcompslate, compsoffset) {
  
  # Define variables
  cues = unlist(strsplit(orthoCoding(word, grams = c(2)), "_"))
  vowels = c("a", "e", "i", "o", "u", "y")
  
  # Get activations
  w = t(weightsPhon[, compslate]) * actcompslate
  tab = candidatesTable(w, diphone = "second")

  # Select the demi-syllable(s) with the highest activation as the candidate(s)
  tab = tab[tab == max(tab)]
  candidates = names(tab)

  # Resolve ties
  if (length(candidates) > 1) {
  
    # For real words we still have the option of exploring neighbors
    actcompslate = sapply(compslate, FUN = function(x) {
        sum(weightsSem[cues, x])
      }) 
    w = t(weightsPhon[names(tab), names(actcompslate)]) * actcompslate
    tab = candidatesTable(w, diphone = "second")
    tab = tab[tab == max(tab)]

    # Resolve additional ties
    if (length(tab) > 1) {
     
      # Update candidates
      candidates = candidates[which(candidates %in% names(tab))]
      candidatescons = substr(candidates, 2, nchar(candidates))
      
      # Determine the optimal size
      for (try in 1:(nchar(word) - lastvowel)){
        sub = substr(word, lastvowel + try, nchar(word))
        compsoffset = wordsSem[grep(sub, wordsSem)]
        compsoffset = compsoffset[substr(compsoffset, nchar(compsoffset) - 
          nchar(offset) + try, nchar(compsoffset)) == sub]
        if (try == 1) {
          nums = which(!(substr(compsoffset, nchar(compsoffset) - nchar(sub),
            nchar(compsoffset) - nchar(sub)) %in% vowels))
          if (length(nums) > 0) {
            compsoffset = compsoffset[-nums]
          }
        }
        compsoffset = compsoffset[which(compsoffset %in% wordsPhon)]
        if (length(compsoffset) != length(compslate)) {
          break
        }
      }

      # Recalculate activations
      actcompsoffset = sapply(compsoffset, FUN = function(x) {
          sum(weightsSem[cues, x])
        })
      w = t(weightsPhon[, names(actcompsoffset)]) * actcompsoffset
      
      # Select the demi-syllable(s) with the highest activation
      tab = candidatesTable(w, diphone = "second")
      sub = which(substr(names(tab), 2, nchar(names(tab))) %in% 
        candidatescons)
      sel = which(tab[sub] == max(tab[sub]))
      # Resolve any further ties
      if (length(sel) > 1) {
        nextcand = vector()
        for (i in 1:length(sel)) {
         substr = substr(names(sel)[i], 2, nchar(names(sel)[i]))
         nextcand[i] = grep(substr, names(tab[sub]))[2]
        }
        sel = which(nextcand == min(nextcand))
      } else {
        sel = 1
      }
      candidates = candidates[which(candidatescons ==
        substr(names(tab)[sub[sel]], 2, nchar(names(tab)[sub[sel]])))]
      tab = tab[candidates]
      tab = tab[tab == max(tab)]
      candidates = names(tab)
    } else {
      candidates = names(tab)
    }
  }

  # Define selected demi-syllable
  second = candidates

  # Return
  return(second)
  
}


# Get simulated pronunciation
simulatePronunciation = function(word, lexicon, weightsSem, weightsPhon) {
  
  # Define variables
  vowels = c("a", "e", "i", "o", "u", "y")
  wordsSem = colnames(weightsSem)
  wordsPhon = colnames(weightsPhon)
  cues = unlist(strsplit(orthoCoding(word, grams = c(2)), "_"))
  cues = cues[which(cues %in% rownames(weightsSem))]
  
  # Get vowel positions
  vowelPosition = vowelPosition(word)
  firstvowel = vowelPosition$firstvowel
  lastvowel = vowelPosition$lastvowel
  
  # Get onset plus vowel
  onsetv = substr(word, 1, lastvowel)
  
  # Get onset
  onset = substr(word, 1, firstvowel - 1)
  
  # Get rhymes
  rhyme = substr(word, firstvowel, nchar(word))
  
  # Get offset
  offset = substr(word, lastvowel + 1, nchar(word))
  
  # Get competitor sets
  
  # Onset plus vowel competitors
  compsearly = wordsSem[which(substr(wordsSem, 1, lastvowel) == onsetv)]
  nums = which(substr(compsearly, lastvowel + 1, lastvowel + 1) %in%
    vowels[-which(vowels == "y")])
  if (length(nums) > 0) {
    compsearly = compsearly[-nums]
  }
  compsearly = compsearly[which(compsearly %in% wordsPhon)]
  
  # Onset competitors
  compsonset = wordsSem[which(substr(wordsSem, 1, (firstvowel - 1)) == onset)]
  compsonset = compsonset[which(substr(compsonset, firstvowel, firstvowel)
    %in% vowels)]
  compsonset = compsonset[which(compsonset %in% wordsPhon)]
  if (length(compsonset) < 1) {
    return(NA)
  }

  # Backup plan in case there are no matching onsets plus vowels
  if (length(compsearly) < 1) {
    compsearly = compsonset
    onsetvs = FALSE
  } else {
    onsetvs = TRUE
  }
  
  # Rhyme competitors
  compslate = wordsSem[grep(rhyme, wordsSem)]
  compslate = compslate[which(substr(compslate, nchar(compslate) -
    nchar(rhyme) + 1, nchar(compslate)) == rhyme)]
  pattern = paste("[", paste(vowels[-which(vowels == "y")], collapse = ""), 
    "]", sep = "")
  nums = grep(pattern, substr(compslate, 1, nchar(compslate) - nchar(rhyme)))
  if (length(nums) > 0) {
    compslate = compslate[-nums]
  }
  compslate = compslate[which(compslate %in% wordsPhon)]
  
  # Offset competitors
  compsoffset = wordsSem[grep(offset, wordsSem)]
  compsoffset = compsoffset[substr(compsoffset, nchar(compsoffset) -
    nchar(offset) + 1, nchar(compsoffset)) == offset]
  nums = which(!(substr(compsoffset, nchar(compsoffset) - nchar(offset),
    nchar(compsoffset) - nchar(offset)) %in% vowels))
  if (length(nums) > 0) {
    compsoffset = compsoffset[-nums]
  }
  compsoffset = compsoffset[which(compsoffset %in% wordsPhon)]
  if (length(compsoffset) < 1) {
    return(NA)
  }
 
  # Backup plan in case there are no matching rhymes
  if (length(compslate) < 1) {
    compslate = compsoffset
    rhymes = FALSE
  } else {
    rhymes = TRUE
  }

  # Get the activations of the competitor sets
  actcompsearly = sapply(compsearly, FUN = function(x) {
      sum(weightsSem[cues, x])
    })
  actcompslate = sapply(compslate, FUN = function(x) {
      sum(weightsSem[cues, x])
    })
  
  if (word %in% intersect(wordsSem, wordsPhon)) {
    actcompsearly[1:length(actcompsearly)] = 0
    actcompsearly[word] = 1
    actcompslate[1:length(actcompslate)] = 0
    actcompslate[word] = 1
  }
  
  # Calculate the most activated word-initial diphone
  first = firstDemisyllable(word, weightsPhon, weightsSem, wordsSem,
                            wordsPhon, firstvowel, compsearly,
                            actcompsearly, compsoffset, compslate)
 
  # Calculate the activated word-final diphone
  second = secondDemisyllable(word, weightsPhon, weightsSem, wordsSem,
                              wordsPhon, lastvowel, offset, compslate,
                              actcompslate, compsoffset)
                                     
  # Define pronunciation
  if (rhymes | !onsetvs) {
    pron = paste(substr(first, 1, nchar(first) - 1), second, sep = "")
  } else {
    pron = paste(first, substr(second, 2, nchar(second)), sep = "")
  }
  pron = substr(pron, 2, nchar(pron) - 1)
  
  # Return
  return(pron)

}
