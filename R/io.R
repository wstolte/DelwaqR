
#' convert Delwaq his file into an R array object,
#'
#' @param filename the .his file to be converted.
#' @return An R array object of the Delwaq .his file named  \code{filename}.
#' @examples
#' library(DelwaqR)
#' arr <- his2arr(filename = "DATA/NZBLOOM.his", timestamp = F, begintime = "2003-01-01 00:00:00")
#' dimnames(arr)
#' submod <- c("Chlfa", "OXY")
#' locmod <- c("NZR6NW020", "NZR9TS010")
#' df <- arr2df(arr, locmod=locmod, submod=submod)
#' df$value[df$variable == "fResptot"] <- -df$value[df$variable == "fResptot"]
#' library(ggplot2)
#' plot <- ggplot(df, aes(time, value))
#' plot +
#'   geom_line(aes(color = variable), size = 1) +
#'   geom_point(aes(color = variable), fill = "white",  shape = 21, size = 4) +
#'   facet_grid((. ~ location))
his2arr <- function(filename, timestamp = T, begintime = "1900-01-01 00:00:00"){
  require("stringr")
  ## Open file in binary mode
  zz <- file(filename, "rb")
  ## Read header lines
  readChar(zz,40)
  readChar(zz,40)
  readChar(zz,40)
  readChar(zz,4)
  ## reads time origin from Delwaq his file
  timeorigin <- readChar(zz,19)
  readChar(zz,17)
  ## Read 2 integers
  afm <- readBin(zz,integer(),n=2)
  ## reserve some memory
  syname <- vector("character",afm[1])
  idump <- vector("integer",afm[2])
  duname <- vector("integer",afm[2])
  ## Now a row of characters
  for(i in 1:afm[1]){
    syname[i] <- readChar(zz,20)
  }
  ## Now a few rows of integers and strings
  for(i in 1:afm[2]){
    idump[i] <- readBin(zz,integer(),n=1)
    duname[i] <- readChar(zz,20)
  }

  loc <- seek(zz)
  it <- -1
  itn <- vector("integer",0)
  tel<-0
  ## Keep reading until we no longer have data
  while(length(it)>0){
    tel<-tel+1
    it<-readBin(zz,integer(),n=1)
    if (length(it)>0){
      itn<-c(itn,it)
      conc<-readBin(zz,"double",n=afm[1]*afm[2],size=4)
    }
  }
  ## rewind
  seek(zz, where=loc)
  concar <- array(dim=c(length(itn),afm[2],afm[1]))
  for(i in 1:length(itn)){
    it <- readBin(zz,integer(),n=1)
    concar[i,,] <- matrix(readBin(zz,"double",n=afm[1]*afm[2],size=4),nrow=afm[2],ncol=afm[1],byrow=T)
  }
  ## close file connection
  close(zz)
  ## adapt date names using timeorigin in his file
  timeorigin <- str_replace_all(timeorigin,"[.]","-")
  ifelse(timestamp,
         itn2 <- as.character(as.POSIXct(x=as.numeric(itn), origin = timeorigin, tz = "GMT")),
         itn2 <- as.character(as.POSIXct(x=as.numeric(itn), origin = begintime, tz = "GMT")))
  dimnames(concar) <- list(itn2,str_trim(duname),str_trim(syname))
  return(concar)
}


# function arr2df is used to convert an array from the his2arr function
# into dataframe with selected substances and locations

#' extract data from array into a dataframe for selected locations and substances,
#'
#' @param arr the array to be extracted.
#' @param locmod the locations in the array to be extracted
#' @param submod the substances in the array to be extracted
#' @return A dataframe with model output values for \code{submod} and \code{locmod}.
#' @examples
#' library(DelwaqR)
#' arr <- his2arr(filename = "DATA/NZBLOOM.his", timestamp = F, begintime = "2003-01-01 00:00:00")
#' dimnames(arr)
#' submod <- c("Chlfa", "OXY")
#' locmod <- c("NZR6NW020", "NZR9TS010")
#' df <- arr2df(arr, locmod=locmod, submod=submod)
#' df$value[df$variable == "fResptot"] <- -df$value[df$variable == "fResptot"]
#' library(ggplot2)
#' plot <- ggplot(df, aes(time, value))
#' plot +
#'   geom_line(aes(color = variable), size = 1) +
#'   geom_point(aes(color = variable), fill = "white",  shape = 21, size = 4) +
#'   facet_grid((. ~ location))
arr2df <- function(arr, locmod, submod) {
  require(reshape2)

  #   df.map <- read.csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)
  #   df.map <- read.csv("p:/1205711-edspa/2012waq/metingen/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)

  if(length(submod) != 1 & length(locmod) != 1) {
    df.mod <- melt(arr[, locmod, submod], varnames=c("time", "location", "variable"))
  }
  if(length(submod) == 1 & length(locmod) != 1) {
    df.mod <- melt(arr[, locmod, submod], varnames=c("time", "location"))
    df.mod$variable <- submod
  }
  if(length(locmod) == 1 & length(submod) != 1) {
    df.mod <- melt(arr[, locmod, submod], varnames=c("time", "variable"))
    df.mod$location <- locmod
  }
  if(length(locmod) == 1 & length(submod) == 1) {
    df.mod <- melt(arr[, locmod, submod], varnames=c("time"))
    df.mod$location <- locmod
    df.mod$variable <- submod
    df.mod$time <- row.names(df.mod)
  }
  df.mod$time  <- as.POSIXct(x=df.mod$time)
  df.mod$location  <- factor(df.mod$location,levels = locmod)
  #df.mod$species   <- factor(df.mod$species, levels = submod)
  return(df.mod)
}

## function for easy saving of ggplot plots
saveseries <- function(plotdir, filename, locname, height, plottype) {
  dev.off()
  ggsave(file=paste(plotdir,"/", filename,"_",locname, plottype, "_area",".png",sep=""),
         width=10,height=height,dpi=300)
}

