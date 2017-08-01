cleanText <- function(x, strip.whitespace=TRUE, strip.numbers=TRUE,
                      strip.symbols=TRUE, lowercase=TRUE){

# clean text from whitespace, numbers, symbols, etc.

  if(length(x)>1) x <- paste(x, collapse=" ")
  
  # repair hyphenation
  x <- gsub("(\\w)- (\\w)","\\1\\2",x)
  
  # strip symbols
  if(strip.symbols){
    x <- gsub("[[:punct:]]*","",x)
    x <- gsub("[^\x01-\x7E]","",x) # non-ASCII
  }
  
  # strip numbers
  if(strip.numbers){
    x <- gsub("\\d*","",x)
  }
  
  # strip whitespace
  if(strip.whitespace){
    x <- gsub("\\s*","",x)
  }
  
  # force lower case
  if(lowercase){
    x <- tolower(x)
  }
  
  return(x)

}

