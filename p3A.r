library(pixmap)
#islibrary(numbers)
secretencoder <- function(imgfilename,msg,startpix,stride,consec=NULL){
  if(!file.exists(imgfilename))
    stop('The file does not exsit.')
  if(!isprimeMy(stride))
    warning('Stride is not a prime number!')
  #if there is numbers package we use the folllowing one is okay
#  if(!isPrime(stride))
#    warning('Stride is not a prime number!')
  
  
  pict <- read.pnm(imgfilename)
  msglen <- nchar( msg )
  Nrow <- pict@size[1]
  Ncol <- pict@size[2]
  pos <- startpix
  usedpix <- NULL
  for(i in seq(msglen)){
    pos <- checkCol(pos, usedpix, Nrow,Ncol, consec)
    pos <- checkRow(pos, usedpix, Nrow,Ncol, consec)
    usedpix <- c(usedpix, pos)
    dcol <- ceiling(pos/Ncol)
    drow <- pos %% Nrow
    pict@grey[drow, dcol] = utf8ToInt( substring(msg, i, i+1) ) / 128
    pos <- pos + stride
    if(pos > Nrow*Ncol)
      pos <- pos %% (Nrow*Ncol)
  }
  
  dcol <- ceiling(pos/Ncol)
  drow <- pos %% Nrow
  pict@grey[drow, dcol] = 0
  return(pict)  
}

secretdecoder <- function(imgfilename,startpix,stride,consec=NULL){
  if(!file.exists(imgfilename))
    stop('The file does not exsit.')
  if(!isPrime(stride))
    warning('Stride is not a prime number!')
  
  pict <- read.pnm(imgfilename)
  Nrow <- pict@size[1]
  Ncol <- pict@size[2]
  pos <- startpix
  msg <- ''
  dcol <- ceiling(pos/Ncol)
  drow <- pos %% Nrow
  usedpix <- startpix
  while( pict@grey[drow, dcol] !=0 ){
    msg <- paste( msg, intToUtf8(pict@grey[drow,dcol]*128), sep='' )
    pos <- pos + stride
    if(pos > Nrow*Ncol)
      pos <- pos %% (Nrow*Ncol)
    pos <- checkCol(pos, usedpix, Nrow,Ncol, consec)
    pos <- checkRow(pos, usedpix, Nrow,Ncol, consec)
    dcol <- ceiling(pos/Ncol)
    drow <- pos %% Nrow
    usedpix <- c(usedpix, pos)
    }
  return(msg)
}



checkCol <- function(pos, usedpix, Nrow,Ncol, consec){
  if(length(usedpix) == 0)
    return(pos)
  
  count <- 1
  #check column direction up
  colCheck <- pos-1
  while(colcheck %in% usedpix && ceiling(colcheck/Nrow) == ceiling(pos/Nrow) ){
    count <- count + 1
    colcheck<- colcheck - 1
  }
  
  if (count > consec){
    while( pos %in% usedpix)
      pos <- pos + 1
    pos<- pos + 1
    count <- 1
    colcheck <- pos
  }
  else
    colcheck <- pos+1
  
  #check column direction down
  while(colcheck %in% usedpix){
    count <- count + 1
    colcheck <- colcheck + 1
  }

  if (count > consec){
    while( pos %in% usedpix)
      pos <- pos + 1
    pos<- pos + 1
  }
  
  if(pos > Nrow*Ncol){
    pos <- pos %% (Nrow*Ncol)
    pos <- checkCol(pos, usedpix, Nrow,Ncol, consec)
  }
    
  return(pos)
}

checkRow <- function(pos, usedpix,Nrow,Ncol, consec){
  if(length(usedpix) == 0)
    return(pos)
  
  count <- 1
  #check row direction up
  rowCheck <- pos - Nrow
  while(rowcheck %in% usedpix){
    count <- count + 1
    rowcheck<- rowcheck - Nrow
  }
  
  rowcheck <- pos+Nrow
  #check row direction down
  while(rowcheck %in% usedpix){
    count <- count + 1
    rowcheck <- rowcheck + Nrow
  }
  
  if (count > consec){
    while( pos %in% usedpix)
      pos <- pos + 1
    pos <- checkCol(pos, usedpix,Nrow,Ncol, consec)
    pos <- checkRow(pos, usedpix,Nrow,Ncol, consec)
  }
  
  if(pos > Nrow*Ncol){
    pos <- pos %% (Nrow*Ncol)
    pos <- checkRow(pos, usedpix, Nrow,Ncol, consec)
  }
  
  return(pos)
}

#Incase there is no numbers package on the machine
isprimeMy <- function(pn){
  if(sum(pn/1:pn==pn%/%1:pn)==2)
    return(TRUE)
  return(FALSE)
}

