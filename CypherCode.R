char2vec = function(phrase){
  
  
  if (length(phrase)==1) {
    ## --- If only one phrase, return as character vector
    N   = nchar(phrase)                               # num of chars in phrase
    
    # if N == 0, return "", otherwise set make phrase a character vector
    if (N==0) {
      out = ""
    } else {
      out = sapply(1:N,function(x){substr(phrase,x,x)})
    }
    
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

cypher = function(phrase, in1let, decrypt = T, add_space=T, match_cap = F){
  # Description: A cypher that can decrypt or encrypt phrases using an two 
  #              rings where one (the inner ring) is on the inside of the other
  #              (the outer ring).
  
  # Arguments: 
  # phrase - The phrase that one would like to encrypt or decrypt
  # in1let - The letter in the inner ring that matches up with "A" in the 
  #          outer ring. in1let stands for "inner ring first letter".
  # decrypt - Whether to decrypt (TRUE) or encrypt (FALSE) the phrase.
  # add_space - Whether the cypher should add a space to the cypher as another
  #             letter to scramble (TRUE) or not (FALSE). Note that when FALSE,
  #             any spaces will stay spaces in the output.
  # match_cap - Whether the output should keep the capitalization of original
  #             string. Note that this causes some issues when add_space = T
  #             as spaces don't have "capitalization."  
  
  ## --- Set up
  # - Create capitalized version of phrase called PHRASE
  # - Ensure in1let is uppercase with no white space
  # - Set outer as capital letter vector LETTERS
  PHRASE = trimws(toupper(phrase)) # create uppercase version of phrase
  in1let = trimws(toupper(in1let)) # guarantee in1let is uppercase with no ws
  outer  = LETTERS                 # set outer rings as the uppercase letters
  
  ## --- Add space to cypher if add_space is TRUE
  if(add_space){
    outer = c(outer," ")
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
  
  ## --- Make phrase and PHRASE into vectors of each character
  phrase_vec = char2vec(phrase)
  PHRASE_vec = char2vec(PHRASE)
  
  ## --- preallocate id vector to store letter ids
  ids_out = rep(0, length(PHRASE_vec))
  ids_in  = rep(0, length(PHRASE_vec))
  
  ## --- store ids for letters in PHRASE_vec for inner and outer ring
  for(i in 1:length(PHRASE_vec)){
    ids_out[i] = which(PHRASE_vec[i] == outer) # store id for outer ring
    ids_in[i]  = which(PHRASE_vec[i] == inner) # store id for inner ring
  }
  
  ## --- Encrypt or decrypt phrase
  # If decrypt is TRUE, then get letters from outer ring using ids from inner
  # If decrypt is FALSE, then get letters from inner ring using ids from outer
  if(decrypt) {
    out = outer[ids_in]
  } else {
    out = inner[ids_out]
  }
  
  ## --- Restore capitalization of original phrase 
  # Note that spaces will always be capitalized when using keep_space = F
  if(match_case){
    letpat      = paste(letters,collapse = "|")   # create regex for all letters
    lowids      = which(grepl(letpat,phrase_vec)) # ids for lowercase chars
    out[lowids] = tolower(out[lowids])            # update these chars as lower
  }
  
  ## --- Collapse character letter vector into character string
  out = paste0(out,collapse = "") # collapse vector to char string
  
  ## --- Return decoded/encoded character string
  return(out)
}