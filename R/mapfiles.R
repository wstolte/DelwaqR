
# function OpenMapFile
# opens the file, reads the header and some constants (including the names of the substances)
# returns a list containing all specifications of the map file
# this list is later used to directly approach the data in the file
# This function does not read any data itself, but prepares for the function ReadMapFile

#' opens the map file, reads the header and some constants
#'
#' @param filename the filename of the map file to be read.
#' @return A dataframe with header and some constants of \code{filename}.
#' @examples
OpenMapFile<-function(filename){
  require("stringr")
  # prepare the list of file characteristics. Most are NA at this moment, only file type
  # and file name are known
  MFS<-list(FileType="map",FileName=filename,ByteOrder=NA,FormatType=NA,Header=NA,T0=NA,
            TUnit=NA,TStep=NA,NumSegm=NA,SegmName=NA,NumSubs=NA,SubsName=NA,NBytesBlock=NA,
            DataStart=NA,NTimes=NA)
  ## Open file in binary mode
  zz <- file(filename, "rb")
  ## Read header lines
  h1<-readChar(zz,40)
  h2<-readChar(zz,40)
  h3<-readChar(zz,40)
  h4<-readChar(zz,40)
  # store the header (120 bytes character string) in MFS
  MFS$Header<-paste(h1,h2,h3,h4,sep='')
  ## Read 2 integers (number of substances and number of segments) and store in MFS
  MFS$NumSubs<-readBin(zz,integer(),n=1)
  MFS$NumSegm<-readBin(zz,integer(),n=1)
  ## reserve some memory for storing the names of the substances
  MFS$SubsName <- vector("character",MFS$NumSubs)
  ## Now a row of characters (length 20 bytes) with the names of the substances
  for(i in 1:MFS$NumSubs){
    MFS$SubsName[i] <- readChar(zz,20)
  }
  # now read in time information. I could not find this in the files. I guess it may
  # be stored in the header, but it was not in the files I tried. I used the information
  # that data were stored weekly, and set T0 arbitrarily to 0. This part is to be improved
  # for generality, but should not be a weakness in the current application
  MFS$T0<-0
  MFS$TUnit<-"day"
  MFS$TStep<-7
  # Determine the position in the file where data start: after header, time information and
  # names of substances
  MFS$DataStart = (MFS$NumSubs*20)+(40*4)+(4*2)
  # data end at the end of the file, which is obtained with a generic function
  MFS$DataEnd   = file.info(filename)$size
  # one block of data (= one moment in time), has length given by space for time information
  # (4 bytes) + NumSegm*NumSubs data of 4 bytes each
  MFS$NBytesBlock = (MFS$NumSegm*MFS$NumSubs+1)*4
  # number of times stored in the file is equal to length of data divided by length of one
  # time block
  MFS$NTimes = (MFS$DataEnd-MFS$DataStart)/MFS$NBytesBlock
  # now the file is (technically) closed again, just to be sure. It will be reopened at each
  # read operation. This ensures that there do not remain random open files in the program
  close(zz)
  # the function returns the list with file characteristics, that can be used in subsequent
  # read operations
  return(MFS)
}

# function ReadMapFile is used to read a number of substances from a predetermined number
# of segments. It does so by first reading an entire block corresponding to one time slice,
# storing these data in a segment*substance matrix, and finally selecting only the segments
# and substances required
# Subs and Segms are vectors containing the sequential numbers of the substances and segments
# that one wants to read. Default is all segments and all substances. Default time is the
# first time block

#' opens the map file, reads map output data for a number of substances for one time slice
#'
#' @param MFS A dataframe with information on the map file, returned from OpenMapFile\(\).
#' @return A matrix with model map output values for \code{MFS}.
#' @examples
ReadMapFile<-function(MFS,Subs=seq(1,MFS$NumSubs),HSegms=seq(1,MFS$NumSegm/MFS$NumLay),VSegms=seq(1,MFS$NumLay),Time=1){
  require("stringr")
  BotLay=(VSegms==c(MFS$NumLay))
  # re-open the file, based on the file name
  zz<-file(MFS$FileName,"rb")
  # position at the start of the time block required
  if (BotLay)offLay<-(MFS$NBytesBlock-4)/MFS$NumLay*(MFS$NumLay-1)+4 else offLay<-4
  seek(zz,where=MFS$DataStart+(Time-1)*MFS$NBytesBlock)
  # read time information (not really used for now, but could be useful later, and anyway
  # this is required to start reading the data from the right position)
  tim<- readBin(zz,integer(),n=1,size=4)
  seek(zz,where=MFS$DataStart+(Time-1)*MFS$NBytesBlock+offLay)
  # read the data block corresponding to the time asked
  BlockLen<-MFS$NumSegm*MFS$NumSubs
  if(BotLay)BlockLen<-BlockLen/MFS$NumLay
  datblock<-readBin(zz,"double",n=BlockLen,size=4)
  # close the file
  close(zz)
  # rearrange the data read into a matrix of segments (rows) by substances (columns)
  # note that the data in the file are arranged sequentially by blocks of all substances
  # for a segment, then all substances for next segments etc. Therefore the matrix is
  # stored by row (byrow=T)
  dts<-matrix(data=datblock,nrow=BlockLen/MFS$NumSubs,ncol=MFS$NumSubs,byrow=T)
  # replace -999 by NA
  dts[dts< -900]<-NA
  # only keep the segments and substances required, rest is thrown away
  if (BotLay)Segms=HSegms else{
      Segms<-vector(length=0)
      for (i in 1:length(Vsegms))Sgems=c(Segms,HSegms*VSegms[i])
    }
  dts<-dts[Segms,Subs]
  # return the matrix with required substances and segments
  return(dts)
}


