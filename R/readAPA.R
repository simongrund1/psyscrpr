readAPA <- function(file=".", clean=TRUE, reduce=TRUE, relax=FALSE,
                    remove.all.script=TRUE, negotiate=TRUE,
                    meta=c("author","year","journal","doi")){

# read text from PDF with publisher format (APA)

  if(file==".") file <- getwd()

  # read PDF via XML
  x <- .readXML(file)
  log <- attr(x,"log")

  # error handling
  if(!log$check) return(x) # error return

  # extract doi
  doi <- tryCatch( .getDOI(x[[1]]) , error=function(e) e )
  no.doi <- inherits(doi,"error")
  if(no.doi) log$message$doi <- doi$message

  # content negotiation
  if(negotiate & !no.doi){
    m <- tryCatch( .getMeta(doi)[meta] , error=function(e) e )
    no.meta <- inherits(m,"error")
    if(no.meta) log$message$meta <- m$message
  }else{
    no.meta <- TRUE
  }
  # unknown meta data
  if(no.doi | no.meta | !negotiate){
    m <- as.list(rep(NA_character_,length(meta)))
    names(m) <- meta
  }
  # if possible use original doi
  if(any(names(m)=="doi")) if(!no.doi & is.na(m$doi)) m$doi <- doi
  
  x <- do.call("rbind",x)
  
  # ***
  # clean text body
  #

  # regular lines (with tolerance)
  pos <- as.vector( sapply( c(73,87,460,473), function(l) (l-relax):(l+relax)) )
  
  if(clean){
    
    # remove title, abstract keywords (page 1)
    i1 <- rev(grep("^Keywords: |^Supplemental materials: ",x$text))[1] # by keywords
    if(!is.na(i1) & with(x, height[i1]==height[i1+1])) i1 <- i1+1 # catch two-line keywords
    if(is.na(i1)){ # alternative: find text onset (strict)
      i0 <- with( x, which( page==1 & height==11 ) )
      i1 <- with( x, which( page==1 & left%in%pos & height==12 ) )-1
      i1 <- intersect(i0,i1)[1]
    }
    if(is.na(i1)){ # alternative: find text onset (liberal)
      i1 <- with( x, which( page==1 & left%in%pos ) )[1]-1
    }
    i2 <- with( x, which( page==1 & top < top[i1] ) )
    x <- x[ -c(1:i1,i2) , ]
    
    # remove first-page script text (e.g., author note, ...)
    i1 <- with( x, which( page==1 & height==11 & left<page.width/2 ) )
    if(length(i1)>0) x <- x[ -i1 , ]
    
    # remove reference list
    i1 <- rev(grep("References",x$text))[1]
    x <- x[ -(i1:nrow(x)) , ]
    
    # remove page headers
    i1 <- lapply( 2:max(x$page), function(p) with( x, which( page==p & height!=12 &
      top<=min(top[page==p]) ) ) )
    i1 <- do.call("c",i1)
    if(length(i1)>0) x <- x[ -i1 , ]
    
    # recursively repair broken lines
    repair <- TRUE
    while(repair){
    
      n <- nrow(x)
      
      sidediff <- with( x, side[2:n] - side[1-(n-1)] )
      topdiff <- with( x, top[2:n] - top[1:(n-1)])
      postreg <- with( x, height[1:(n-1)]==12 & left[1:(n-1)]%in%pos )
      
      i2 <- which( topdiff>-16 & topdiff<5 & sidediff==0 & postreg )+1
      if(length(i2)>0){
        for(i in i2) x$text[i-1] <- paste(x$text[i-1], x$text[i])
        x <- x[ -i2 , ]
      }else{
        repair <- FALSE
      }
    
    }
    
    # reduce to regular text
    i1 <- with( x, which( left%in%pos ) )
    x <- x[i1,]
    
    # clean leftover captions
    i1 <- grep("^Table \\w+$|^Table \\w+ \\(contin|^Figure \\w+\\. |^Note.$", x$text)
    i2 <- with( x, which( width>(page.width/2) ) )
    if(length(c(i1,i2))>0) x <- x[ -c(i1,i2), ]
    
    # remove script-size text (smaller than 12; e.g., footnotes, table notes, ...)
    if(remove.all.script){
      i1 <- with( x, which( height<12 ) )
      if(length(i1)>0) x <- x[ -i1, ]
    }
  
  } # end if,clean

  # reduce columns in output
  if(reduce) x <- x[,c("page","line","side","text")]

  rownames(x) <- NULL
  attr(x,"meta") <- m
  attr(x,"log") <- log

  return(x)

}

