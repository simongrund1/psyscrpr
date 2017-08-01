findPattern <- function(x, pattern, by.page=FALSE, ...){
# find pattern in data.frame with text extracted from PDF

  x <- if(by.page) split(x, x$page) else list(x)
  np <- length(x)

  txt <- lapply(x, function(p) cleanText(p,...))
  h <- sapply(txt, function(p) gregexpr(pattern,p))

  nh <- unname( sapply(h, function(p) sum(p!=-1)) )

  if(sum(nh)>0){

    ph <- if(by.page) which(nh>0) else 0
    nh <- nh[nh>0]

  }else{

    ph <- nh <- 0

  }

  return( list(page=ph, number=nh) )

}

