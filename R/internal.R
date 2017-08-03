# *** xml conversion ...........................................................
#

.readXML <- function(file){
# read PDF via XML to list / data.frames

  # convert to XML
  system2( "pdftohtml", paste0("-c -s -i -xml ",file),
           stdout=NULL, stderr=NULL )

  # error log
  log <- list(check=FALSE, message=list(), error=list())

  # read XML
  f <- sub("\\.pdf$",".xml",file)
  x <- tryCatch( .readXML.helper(f) , error=function(e) e )

  # attempt error recovery
  if(inherits(x,"error")){

    err <- as.character(x)
    inv.char <- grepl("PCDATA invalid Char|invalid character in attribute",err)
    if(inv.char){
    # ... xml-invalid chars
      xml <- readLines(f, encoding="UTF-8", skipNul=TRUE)
      xml <- gsub("[^\x20-\x7E]","",iconv(xml)) # printable only
      writeLines(xml, f, useBytes=TRUE)

      x <- tryCatch( .readXML.helper(f) , error=function(e) e )
      if(!inherits(x,"error")){
        log$check <- TRUE
        log$message$xml <- "removed non-ASCII"
      }
    }

  }else{
    log$check <- TRUE
  }

  invisible(file.remove(f))

  # get pages if check OK
  if(log$check){
    x <- lapply( x[names(x)=="page"], .get.xmlPage )
  }else{
    log$error$xml <- err
  }
  attr(x,"log") <- log

  return(x)

}

.readXML.helper <- function(file){
# helper function for error catching

  xml <- xml2::read_xml(file, options = c("RECOVER","NOBLANKS"))
  return(xml2::as_list(xml))

}

# *** xml parsing ..............................................................
#

.get.xmlPage <- function(x){
# parse xml page structure

  pn <- as.integer(attr(x,"number"))
  i <- names(x)=="text"
  n <- sum(i)

  # page info
  p.df <- data.frame( page=rep(pn,n), page.top=as.integer(attr(x,"top")),
                      page.left=as.integer(attr(x,"left")), page.width=as.integer(attr(x,"width")),
                      page.height=as.integer(attr(x,"height")), line=1:n )

  # line info
  xt <- lapply( x[i], .get.xmlText )
  l.df <- as.data.frame( do.call("rbind",xt), stringsAsFactors=FALSE, row.names=1:n )

  colnames(l.df) <- c("top","left","width","height","text")
  l.df <- within( l.df,{
    top <- as.integer(top)
    left <- as.integer(left)
    width <- as.integer(width)
    height <- as.integer(height)
  })

  # add side
  p.df$side <- as.integer( l.df$left > (p.df$page.width/2) ) + 1

  return( cbind(p.df,l.df) )

}

.get.xmlText <- function(x){
# parse xml text fields

  atr <- c(attr(x,"top"),attr(x,"left"),attr(x,"width"),attr(x,"height"))

  if(length(x)==0) x[[1]] <- ""
  txt <- if(length(x)>1 | is.list(x[[1]])){
    paste(unlist(x,recursive=T),collapse="")
  }else{
    x[[1]]
  }

  return( c(atr,txt) ) 

}


# *** doi lookup ...............................................................
#

.getDOI <- function(x){
# get doi from PDF header

  # regular case (same line)
  i <- grep("^DOI: ",x$text)
  doi <- sub("^DOI: ","",x$text[i])

  # ... (two lines)
  if(length(i)==0){
    i <- grep("^DOI:$",x$text)
    doi <-x$text[i+1]
  }
  # ... (as http)
  if(length(i)==0){
    i <- rev(grep("^http://dx.doi.org/",x$text))[1]
    if(!is.na(i)) doi <- sub("^http://dx.doi.org/","",x$text[i])
  }

  if(length(doi)==0) stop("DOI not found")
  return(doi)

}

.getMeta <- function(x){
# get BibTeX meta data by content negotiation

  bib <- suppressWarnings(
    system2("curl", args=paste0('-LH "Accept: application/x-bibtex" http://dx.doi.org/', x),
            stdout=TRUE, stderr=NULL)
  )

  if(!grepl("^\x40",bib[1])) stop("DOI lookup failed")
  bib <- sub("\\t","",bib[2:(length(bib)-1)])
  out <- .convertBibTeX(bib)

  return(out)

}

.convertBibTeX <- function(x){
# convert BibTeX data to plain list

  l <- as.list(seq_along(x))

  for(i in seq_along(x)){

    xi <- strsplit(x[i],split=" = ")
    nam <- xi[[1]][1]
    val <- gsub("^\\{|\\},$|,$|\\}$","",xi[[1]][2])

    if(nam=="author"){
      val <- gsub(" and",",",val)
      val <- paste(regmatches(val,gregexpr("\\S+,|\\S+$",val))[[1]], collapse=" ")
    }
    if(nam%in%c("author","title")){
      # val <- gsub("\\{.*\\}","",val)
      val <- .texReplace(val)
    }
    if(nam%in%c("year","volume","number")){
      val <- as.numeric(val)
    }
    names(l)[i] <- nam
    l[[i]] <- val

  }
  return(l)

}

.texReplace <- function(x){
# replace special TeX characters

  l <- list(
    c( "\\\\emph\\{(.*)\\}" , "\\1" ),
    c( "\\\\textbf\\{(.*)\\}" , "\\1" ),
    c( "\\s?\\\\textsuperscript\\{(.*)\\}" , "^\\1" ),
    c( "\\\\textquotesingle" , "'" ),
    c( "\\\\c\\{(\\w*)\\}" , "\\1" ),
    c( "\\\\c\\s(\\w*)" , "\\1" ),
    c( "\\\\v\\{(\\w*)\\}" , "\\1" ),
    c( "\\\\v\\s(\\w*)" , "\\1" ),
    c( "\\\\u\\{(\\w*)\\}" , "\\1" ),
    c( "\\\\u\\s(\\w*)" , "\\1" ),
    c( "\\\\r\\{(\\w*)\\}" , "\\1" ),
    c( "\\\\r\\s(\\w*)" , "\\1" ),
    c( "\\\\i" , "i" ),
    c( "\\\\j" , "j" ),
    c( "\\\\'\\{(\\w*)\\}" , "\\1" ),
    c( "\\\\'(\\w*)" , "\\1" ),
    c( '\\\\"\\{(\\w*)\\}' , "\\1" ),
    c( '\\\\"(\\w*)' , "\\1" ),
    c( "\\\\^\\{(\\w*)\\}" , "\\1" ),
    c( "\\\\^(\\w*)" , "\\1" ),
    c( "\\\\`\\{(\\w*)\\}" , "\\1" ),
    c( "\\\\`(\\w*)" , "\\1" ),
    c( "\\\\~\\{(\\w*)\\}" , "\\1" ),
    c( "\\\\~(\\w*)" , "\\1" ),
    c( "\\\\ss" , "ss" ),
    c( "\\\\aa" , "a" ),
    c( "\\\\o" , "o" )
  )

  # replace special characters
  for(r in 1:9){ # run with limited recursion (max. 9 passes)
    x0 <- x
    for(i in seq_along(l)){
      x <- gsub( l[[i]][1] , l[[i]][2], x )
    }
    if(identical(x,x0)) break
  }

  # remove braces
  x <- gsub( "\\{|\\}", "", x )

  return(x)

}

