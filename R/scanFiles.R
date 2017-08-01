scanFiles <- function(file=".", pattern, by.page=FALSE, clean=TRUE,
                      relax=FALSE, remove.all.script=TRUE,
                      method=c("APA"),
                      negotiate=TRUE, meta=c("author","year","journal","doi"),
                      file.names=FALSE, ...){

# scan one or several PDF for one or several patterns and generate report

  # match method
  method <- match.arg(method)

  # files (expand folders)
  if(length(file)==1) if(file==".") file <- getwd()
  dir <- which(!grepl("\\.pdf$",file))
  if(length(dir)>0){
    dirlist <- as.list(dir)
    for(dd in dir){
      dirlist[[dd]] <- list.files(file[dd], ".pdf$", recursive=TRUE, full.names=TRUE)
    }
    file <- c(do.call("c",dirlist), file[-dir])
  }
  nf <- length(file)

  # patterns
  np <- length(pattern)

  # output
  out <- matrix(0, nrow=np*nf, ncol=(length(meta)+3+by.page*2+file.names))
  out <- data.frame(out)
  colnames(out) <- c(meta,"pattern","hit","total", if(by.page) c("page","page.total"),
                     if(file.names) "file.name")

  # error logging
  err <- list()

  # loop over files
  for(ff in seq_along(file)){

    cat("[",ff,"/",nf,"] ", file[ff], " ... ", sep="")

    # read file
    f <- file[ff]
    args <- list(file=f, clean=clean, reduce=TRUE, relax=relax, 
                 remove.all.script=remove.all.script, meta=meta,
                 negotiate=negotiate)
    x <- do.call("readAPA",args)
    log <- attr(x,"log")

    # error messages
    if(log$check){ # regular processing

      # meta data
      m <- attr(x,"meta")

      # find patterns in text column
      res <- lapply(pattern, function(p) findPattern(x, p, by.page=by.page, ...))
      names(res) <- pattern

      # combine search results
      ht <- sapply(res, function(p) sum(p$number) )
      h <- as.numeric(ht>0)
      x1 <- data.frame(m, pattern=pattern, hit=h, total=ht, stringsAsFactors=FALSE)

      # results by page
      if(by.page){

        pg <- sapply(res, function(p) paste(p$page,collapse=","))
        pt <- sapply(res, function(p) paste(p$number,collapse=","))
        x1 <- cbind(x1, data.frame(page=pg, page.total=pt, stringsAsFactors=FALSE))

      }

      # add file name
      if(file.names) x1$file.name <- f

      # add results
      i1 <- ((ff-1)*np+1):(ff*np)
      out[i1,] <- x1

    }else{ # misc. error logging

      err <- c( err, list(file=file[ff], error=log$error) )

      # add NA
      i1 <- ((ff-1)*np+1):(ff*np)
      out[i1,] <- NA

    }

    # verbose
    mes <- log$message
    if(length(mes)>0) mes <- c("(", paste(log$message, collapse=", "), ")")
    cat(ifelse(log$check,"OK ","Error "), mes, "\n", sep="")

  } # end for

  # convert character to factor
  for(ii in 1:length(meta)) if(is.character(out[,ii])) out[,ii] <- factor(out[,ii])
  out$pattern <- factor(out$pattern)

  # append meta fields
  attr(out,"meta") <- meta

  # append error messages
  if(length(err)>0) attr(out,"error") <- err

  class(out) <- c("scrpr.frame", "data.frame")
  return(out)

}

# *** ..........................................................................

update.scrpr.frame <- function(object, meta=NULL, force=FALSE, ...){

  # check for doi
  doi <- colnames(object)=="doi"

  if(any(doi)){
    
    # check old vs. new meta
    meta0 <- attr(object,"meta")
    if(is.null(meta) | all(meta%in%meta0)){
      meta <- meta0
      new <- FALSE
    }else{
      new <- TRUE
    }

    # prepate output
    out <- data.frame( matrix(NA,nrow(object), length(meta)) )
    colnames(out) <- meta

    for(m in meta0) object[,m] <- as.character(object[,m])

    # update meta data by doi lookup
    doi0 <- unique(object[,doi])
    for(d in doi0){
      i <- which(object[,doi]==d)
      na <- any(is.na(object[i,meta0]))
      # if fields are new, missing, or forced
      if(na|new|force){
        m <- tryCatch(.getMeta(d), error=function(e) e)
        if(!inherits(m,"error")) out[i,meta] <- m[meta]
      }else{
        out[i,meta] <- object[i,meta]
      }
    }

    # remove old, append new
    ind0 <- which( colnames(object)%in%meta0 )
    object <- cbind( out, object[,-ind0,drop=F] )

    for(m in meta) object[,m] <- as.factor(object[,m])

  }else{
    stop("Cannot update without DOI.")
  }

  return(object)

}

print.scrpr.frame <- function(x, trim.char=TRUE, ...){

  # shorten author names
  aut <- colnames(x)=="author"
  if(any(aut)) levels(x[,aut]) <- .shortAuthor(levels(x[,aut]))

  # shorten titles
  ttl <- colnames(x)=="title"
  if(any(ttl)) levels(x[,ttl]) <- .shortChar(levels(x[,ttl]),trim=trim.char)

  # shorten file names
  fn <- colnames(x)=="file.name"
  if(any(fn) & trim.char){
    fn0 <- unique(x[,fn])
    fn1 <- gsub("^.*[\\/]","",fn0)
    x[,fn] <- fn1[match(x[,fn],fn0)]
  }

  # print data frame with shortened labels
  print.data.frame(x)
  invisible(x)
  
}

# *** ..........................................................................

.shortChar <- function(x,trim){

  # default: 32
  if(is.logical(trim)) trim <- 32

  x <- substr(x,1,trim)
  x <- paste0(gsub(" $","",x), "...")

  return(x)

}

.shortAuthor <- function(x){

  i1 <- gregexpr(", ", x)
  i2 <- sapply(i1,length)>1

  x[i2] <- sub("^([^,]+), .*","\\1 et al.", x[i2])
  x <- sub(", "," & ", x)

  return(x)

}

