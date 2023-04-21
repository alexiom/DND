################################################################################
# Title: Code For Disk Ciphers                                                 #
# Author: Alex Marsh                                                           #
# Date: 04/21/2023                                                             #
# Notes:                                                                       #
################################################################################

################################################################################
#                             Description:                                     #
# Code needed for encrypting or decrypting simple disk ciphers that are        #
# occasionally encountered in adventures.                                      #
################################################################################



char2listvec = function(phrase){
  
  if (length(phrase)==1) {
    ## --- If only one phrase, return as character vector
    N   = nchar(phrase)                               # num of chars in phrase
    
    # if N == 0, return "", otherwise set make phrase a character vector
    if (N==0) {
      out = ""
    } else {
      out = sapply(1:N,function(x){substr(phrase,x,x)})
    }
    
    out = list(out)
    names(out) = phrase
    
  } else if (length(phrase)>1) {
    ## --- If multiple phrases, return a list of character vectors
    K          = length(phrase)  # set number of phrases
    out        = as.list(phrase) # preallocate list
    names(out) = phrase          # set names of list elements as the phrases
    
    # loop over phrases
    for (i in 1:K) {
      # this code is identical to the code in the if statement above except 
      # that we are doing it for the ith value in phrase
      N = nchar(phrase[i])     
      if (N==0) {
        out[[i]] = ""
      } else {
        out[[i]] = sapply(1:N,function(x){substr(phrase[i],x,x)})
      }
      
    }
  } else {
    ## --- If phrase is length 0, return empty character value
    out = character()
  }
  
  ## --- return the value of out
  return(out)
}

tolowerSpace = function(x){
  tolower(gsub("_"," ",x))
}

cipher = function(phrase, in1let, decrypt = T, add_space=T, match_cap = F){
  # Description: A cipher that can decrypt or encrypt phrases using an two 
  #              rings where one (the inner ring) is on the inside of the other
  #              (the outer ring).
  
  # Arguments: 
  # phrase - The phrase that one would like to encrypt or decrypt
  # in1let - The letter in the inner ring that matches up with "A" in the 
  #          outer ring. in1let stands for "inner ring first letter".
  # decrypt - Whether to decrypt (TRUE) or encrypt (FALSE) the phrase.
  # add_space - Whether the cipher should add a space to the cipher as another
  #             letter to scramble (TRUE) or not (FALSE). Note that when FALSE,
  #             any spaces will stay spaces in the output.
  # match_cap - Whether the output should keep the capitalization of original
  #             string. Note that I am using "_" to indicated "capitalized 
  #             space."
  
  ## --- Set up
  # - Create capitalized version of phrase called PHRASE
  # - Ensure in1let is uppercase with no white space
  # - Set outer as capital letter vector LETTERS
  N      = length(phrase)
  PHRASE = trimws(toupper(phrase)) # create uppercase version of phrase
  in1let = trimws(toupper(in1let)) # guarantee in1let is uppercase with no ws
  outer  = LETTERS                 # set outer rings as the uppercase letters
  
  ## --- Add space to cipher if add_space is TRUE
  if(add_space){
    outer  = c(outer,"_")
    PHRASE = gsub(" ","_",PHRASE)
  }
  
  ## --- Create inner ring
  K                  = length(outer)              # set num of outer ring chars 
  delta              = (which(outer == in1let)-1) # calc shift for inner ring
  ids                = 1:K + delta                # shift 1:K by delta
  ids                = ids %% K                   # get remainder of ids/K
  ids[which(ids==0)] = K                          # if id was K, make it K now
  inner              = outer[ids]                 # set inner ring
  
  ## --- Add space at end of rings if add_space is FALSE
  if(!add_space){
    outer = c(outer," ")  # add space to end of outer ring
    inner = c(inner," ")  # add space to end of inner ring
    K     = K + 1         # recalc K
  }
  
  ## --- Make phrase and PHRASE into list of vectors of each character
  phrase_vec = char2listvec(phrase)
  PHRASE_vec = char2listvec(PHRASE)
  
  ## --- preallocate id vector to store letter ids
  ids_out = lapply(1:N,function(i){rep(0, length(PHRASE_vec[[i]]))})
  ids_in  = lapply(1:N,function(i){rep(0, length(PHRASE_vec[[i]]))})
  
  ## --- store ids for letters in PHRASE_vec for inner and outer ring
  for(j in 1:N){
    tempvec = PHRASE_vec[[j]] # store jth element of phrase list
    
    for(i in 1:length(tempvec)){
      ids_out[[j]][i] = which(tempvec[i] == outer) # store id for outer ring
      ids_in[[j]][i]  = which(tempvec[i] == inner) # store id for inner ring
    }
  }

  
  ## --- Encrypt or decrypt phrase
  # If decrypt is TRUE, then get letters from outer ring using ids from inner
  # If decrypt is FALSE, then get letters from inner ring using ids from outer
  if(decrypt) {
    out = lapply(ids_in,function(x){outer[x]})
  } else {
    out = lapply(ids_out,function(x){inner[x]})
  }
  
  ## --- Restore capitalization of original phrase 
  # Note that spaces will always be capitalized when using keep_space = F
  if(match_cap){
    
    ## --- If add space, make " " a "lower case" space by adding to letters
    if(add_space) letters2 = c(letters," ") else letters2 = letters

    ## --- Function for getting ids of characters that were first lower case
    getlowerids = function(x,pat=letpat){which(grepl(pat,x))}
    
    ## --- Function that restores original case for a given i of the out list
    restoreCase = function(i){
      xvec = out[[i]]     # get ith character vector of list
      ids  = lowids[[i]]  # get corresponding lowercase ids
      
      ## --- if id is in ids, make lowercase, otherwise return it
      ifelse(1:length(xvec) %in% ids,tolowerSpace(xvec),xvec)
    }
    
    letpat = paste(letters2,collapse = "|")  # create regex for all letters
    lowids = lapply(phrase_vec,getlowerids)  # ids for lowercase chars
    out    = lapply(1:N,restoreCase)         # update these chars as lower
  }
  
  ## --- Collapse character letter vector into character string
  out = lapply(out,paste0,collapse="") # collapse vector to char string
  out = unlist(out)                    # unlist out and make character vector
  
  ## --- If decrypting, make "capital spaces" normal spaces
  if(decrypt){
    out = gsub("_"," ",out)
  }
  
  ## --- Return decoded/encoded character string
  return(out)
}

#==============================================================================#
#                                  Examples                                    #
#==============================================================================#

testPhrase = "Hello World"

#------------------------------------------------------------------------------#
#                                   Encrypt                                    #
#------------------------------------------------------------------------------#

## --- Example 1: first letter being B, including spaces, and matching case
EnEx1 = cipher(testPhrase,"B",decrypt = F, add_space = T, match_cap = T)
EnEx1

## --- Example 2: first letter being B, ignoring spaces, and matching case
EnEx2 = cipher(testPhrase,"B",decrypt = F, add_space = F,match_cap = T)
EnEx2

## --- Example 3: first letter being B, including spaces, and all caps
EnEx3 = cipher(testPhrase,"B",decrypt = F, add_space = T, match_cap = F)
EnEx3

## --- Example 4: first letter being W, including spaces, and matching case
EnEx4 = cipher(testPhrase,"W",decrypt = F, add_space = T, match_cap = T)
EnEx4

#------------------------------------------------------------------------------#
#                                   Decrypt                                    #
#------------------------------------------------------------------------------#

## --- Decrypt Example 1
EnEx1
cipher(EnEx1,"B",decrypt = T, add_space = T, match_cap = T)


## --- Decrypt Example 2
EnEx2
cipher(EnEx2,"B",decrypt = T, add_space = F,match_cap = T)

## --- Decrypt Example 3
EnEx3
cipher(EnEx3,"B",decrypt = T, add_space = T, match_cap = F)

## --- Decrypt Example 4
EnEx4
cipher(EnEx4,"W",decrypt = T, add_space = T, match_cap = T)
