#Remove empty directory in current dir
rmemptydirs <- function( drname, filelist, arg ){
  if ( length(filelist) == 0 )
    return(NULL)

  for( d in filelist )
    if(file.info(d)$isdir)
	if(length(dir(d)) == 0)
		unlink(d, recursive = TRUE)

  return(NULL)
}

#Find all number of bytes in all the files (nondir)
nbytes <- function( drname, filelist, arg) {
  if( length(filelist) == 0)
    return(arg)

  for(f in filelist)
    if(!file.info(f)$isdir)
      arg <- arg + file.info(f)$size

  return(arg)
}



walk <- function( currdir, f, arg, firstcall=TRUE ){
  currdir <- getDirRight(currdir)
  setwd(currdir)
  for( d in dir(currdir) )
    if(file.info(paste(currdir,'/',d,sep=''))$isdir){
      nextdir <- paste(currdir,'/',d,sep='')
      arg <- walk(nextdir, f, arg, firstcall=FALSE)
    }

  arg <- f( currdir, dir(currdir), arg )
  return(arg)      
}

getDirRight <- function(dir){
  rval <- dir
  tmp <- strsplit( getwd(), '/' )
  tmp <- tmp[[1]]
  upDir <- NULL
  
  for( ele in tmp[2:length(tmp)] )
    upDir <- paste( upDir, ele, sep='/')

  if(substr(rval,1,2) != './' && substr(rval,1,1) != '/')
    rval <- paste( './' , dir, sep='')
  
  if(substr(rval,1,2) == './')
    rval <- paste( upDir, substr(rval, 2, nchar(rval)), sep='')
  
  return(rval)
}

