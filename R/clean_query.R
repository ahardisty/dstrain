clean_query <- function(x) {
  x = gsub("\t+", "", x, perl=TRUE); # remove all tabs
  x = gsub("\t+", "", x, perl=TRUE); # remove all tabs
  x = gsub("^\\s+", "", x, perl=TRUE); # remove leading whitespace
  x = gsub("\\s+$", "", x, perl=TRUE); # remove trailing whitespace
  x = gsub("[ ]+", " ", x, perl=TRUE); # collapse multiple spaces to a single space
  x = gsub("^[--]+.*$", "", x, perl=TRUE); # destroy any comments
  x = gsub("^[/]+.*$", "", x, perl=TRUE); # destroy any comments
  return(x)
  }
