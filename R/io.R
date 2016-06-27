
#' convert Delwaq his file into an R array object,
#'
#' @param filename the .his file to be converted.
#' @return An R array object of the Delwaq .his file named  \code{filename}.
#' @examples
#' library(DelwaqR)
#' arr <- his2arr(filename = "extdata/NZBLOOM.his", timestamp = F, begintime = "2003-01-01 00:00:00")
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
his2arr <- function (filename, timestamp = T, begintime = "1900-01-01 00:00:00"){
  library("stringr")
  if (substr(filename, nchar(filename) - 3, nchar(filename)) !=".his") {
    stop("filename does not seem to be a <.his> file")
  }

  zz <- file(filename, "rb")
  readChar(zz, 40)
  readChar(zz, 40)
  readChar(zz, 40)
  readChar(zz, 4)
  timeorigin <- readChar(zz, 19)
  readChar(zz, 7)
  scu.prep <- readChar(zz, 8)
  scu  <- as.numeric(scu.prep) # check for the internal timer
  sign.scu <- sign(scu)  # sign can be +1, 0 or -1

  ifelse(is.na(sign.scu), dec.sign.scu <- NA,   # NB. no error handling here yet, NA should only occur if timestamp = F and then you don't need dec.sign.scu
         ifelse(sign.scu == 1 , dec.sign.scu <- "*",
                ifelse(sign.scu == -1, dec.sign.scu <- "/",
                       ifelse(sign.scu == 0, stop("The sign of your internal timer is neither negative nor positive, but 0."), stop("Check dec.sign.scu.")))))

  scu.sym  <- readChar(zz, 1)
  readChar(zz, 1)
  afm <- readBin(zz, integer(), n = 2)

  syname <- vector("character", afm[1])
  idump <- vector("integer", afm[2])
  duname <- vector("integer", afm[2])
  for (i in 1:afm[1]) {
    syname[i] <- readChar(zz, 20)
  }
  for (i in 1:afm[2]) {
    idump[i] <- readBin(zz, integer(), n = 1)
    duname[i] <- readChar(zz, 20)
  }
  loc <- seek(zz)
  it <- -1
  itn <- vector("integer", 0)
  tel <- 0
  while (length(it) > 0) {
    tel <- tel + 1
    it <- readBin(zz, integer(), n = 1)
    if (length(it) > 0) {
      itn <- c(itn, it)
      conc <- readBin(zz, "double", n = afm[1] * afm[2],
                      size = 4)
      length(conc)
    }
  }
  seek(zz, where = loc)
  concar <- array(dim = c(length(itn), afm[2], afm[1]))
  for (i in 1:length(itn)) {
    it <- readBin(zz, integer(), n = 1)
    concar[i, , ] <- matrix(readBin(zz, "double", n = afm[1] * afm[2], size = 4), nrow = afm[2], ncol = afm[1], byrow = T)
  }
  close(zz)
  timeorigin <- str_replace_all(timeorigin, "[.]", "-")


  ifelse(timestamp, itn2 <- as.character(as.POSIXct(x = sapply(itn, function(x) as.numeric(eval(parse(text = paste(x, dec.sign.scu, scu))))),
                                                    origin = timeorigin, tz = "GMT")), itn2 <- as.character(as.POSIXct(x = as.character(as.POSIXct(x = as.numeric(itn),
                                                                                                                                                   origin = begintime, tz = "GMT")))))
  dimnames(concar) <- list(itn2, str_trim(duname), str_trim(syname))
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
#' arr <- his2arr(filename = "extdata/NZBLOOM.his", timestamp = F, begintime = "2003-01-01 00:00:00")
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
  library(reshape2)

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

  ifelse(nchar(as.character(df.mod$time[1])) < 19,
         df.mod$time  <- as.POSIXct(x=df.mod$time, format = "%Y-%m-%d"),
         df.mod$time  <- as.POSIXct(x=df.mod$time, format = "%Y-%m-%d %H:%M:%S")
  )
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

######Read Data#######################


############################################################
#' Read MWTL NetCDF and convert it to a Dataframe
#'
#' @param substance_mwtl substances with naming as in mwtl ncdf files
#' @param list_locations_mwtl locations with naming as in mwtl ncdf files
#' @param workdir directory where downloaded ncdf files are stored
#' @return dataframe with available mwtl measurements

DF_MWTL_NCDF <- function(substance_mwtl,list_locations_mwtl,workdir){
  library("ncdf")
  library("chron")

  file_name = substance_mwtl
  list_locations = list_locations_mwtl

  locations_present <- list.files(path = file.path(workdir,file_name))

  if(exists("save_dataframe")){rm("save_dataframe")}

  for(f in 1:length(list_locations)){

    nr_file <- grep(paste(as.character(list_locations)[f],".nc",sep = ""),locations_present)

    #Skip locations that are not present in Netcdf
    if(length(nr_file) != 1){next}else{}


    setwd(file.path(workdir,file_name))

    ################
    #Load NetCDF

    # Now open the file and read its data
    station <- open.ncdf(locations_present[nr_file], write=FALSE, readunlim=FALSE)

    # Get data
    cat(paste(station$filename,"has",station$nvars,"variables"), fill=TRUE)


    var_get = station[[10]][file_name]
    unit_for_plot = as.character(unlist(var_get[[file_name]]["units"]))

    time_ncdf     = get.var.ncdf(nc=station,varid="time")
    locations     = get.var.ncdf(nc=station,varid="locations")
    name_strlen1  = get.var.ncdf(nc=station,varid="name_strlen1")
    name_strlen2  = get.var.ncdf(nc=station,varid="name_strlen2")
    platform_id   = get.var.ncdf(nc=station,varid="platform_id")
    platform_name = get.var.ncdf(nc=station,varid="platform_name")
    lon           = get.var.ncdf(nc=station,varid="lon")
    lat           = get.var.ncdf(nc=station,varid="lat")
    wgs_84        = get.var.ncdf(nc=station,varid="wgs84")
    epsg          = get.var.ncdf(nc=station,varid="epsg")
    x             = get.var.ncdf(nc=station,varid="x")
    y             = get.var.ncdf(nc=station,varid="y")
    z             = get.var.ncdf(nc=station,varid="z")
    value         = get.var.ncdf(nc=station,varid=file_name)

    datetime = strptime(as.character(chron(time_ncdf, origin=c(month=1,day=1,year=1970))),format = "(%m/%d/%y %H:%M:%S)",tz = "GMT")

    if(length(platform_id) > 1){platform_id = platform_id}else{platform_id = rep(platform_id,length(value))}
    if(length(platform_name) > 1){platform_name = platform_name}else{platform_name = rep(platform_name,length(value))}
    if(length(lon) > 1){lon = lon}else{lon = rep(lon,length(value))}
    if(length(lat) > 1){lat = lat}else{lat = rep(lat,length(value))}
    if(length(lat) > 1){lat = lat}else{lat = rep(lat,length(value))}
    if(length(wgs_84) > 1){wgs_84 = wgs_84}else{wgs_84 = rep(wgs_84,length(value))}
    if(length(epsg) > 1){epsg = epsg}else{epsg = rep(epsg,length(value))}
    if(length(x) > 1){x = x}else{x = rep(x,length(value))}
    if(length(y) > 1){y = y}else{y = rep(y,length(value))}
    if(length(z) > 1){z = z}else{z = rep(z,length(value))}

    data_ncdf = data.frame(time = datetime,platform_id, platform_name ,
                           lon , lat , wgs_84 , epsg, x , y ,
                           z, value, unit = rep(unit_for_plot,length(value)), stringsAsFactors = FALSE)

    data_ncdf$location_name <- as.character(list_locations)[f]
    data_ncdf_corr = data_ncdf[!(duplicated(data_ncdf)),]

    if(TRUE %in% duplicated(data_ncdf[,c("time","location_name")])){
      stop(print(paste("NetCDF file: Duplicates on time and location_name in netcdf file ",
                       substance_mwtl," for file ",locations_present[nr_file]," . Please correct this file.",
                       sep = "" )))
    }

    #Close NetCDF connection
    close.ncdf(station)

    if(!(exists("save_dataframe"))){
      save_dataframe = data_ncdf
    }else{
      save_dataframe = rbind(save_dataframe,data_ncdf_corr)
    }
  }
  if(!(exists("save_dataframe"))){
    save_dataframe = data.frame(time = 0,platform_id = NA, platform_name = NA,
                                lon = 0 , lat = 0 , wgs_84 = NA , epsg = NA, x = 0 , y = 0 ,
                                z = 0, value = 0, unit = NA)
  }
  return(save_dataframe)
}


#####################################
#' Check and Read CSV data
#'
#' @param csv_file file with measurements
#' @param name_file file with names?
#' @return dataframe with available measurements available in the csv file
ReadCSVMWTLFormat <- function(csv_file, name_file){

  #Current mapping tables
  ##List locations
  csv_colnr = 13
  csv_colnam = c("time","platform_id","platform_name","lon","lat","wgs_84","epsg",
                 "x","y","z","value","unit","location_name")
  csv_class = c("character","character","character","numeric",
                "numeric","numeric","numeric","numeric","numeric","numeric",
                "numeric","character","character")
  min_list_csv = 1
  csv_time_column = "time"

  #Check datatype
  if(class(csv_file) != "data.frame"){
    stop(print(paste("CSV file ", name_file," should be of class data.frame",sep = "")))
  }

  #Check locations
  if(length(colnames(csv_file)) != csv_colnr){
    stop(print(paste("CSV file ",name_file," has to many/few columns : needed is ",csv_colnr,
                     ", current is", length(colnames(csv_file)),"!", sep = "")))
  }
  if(FALSE %in% (csv_colnam %in% colnames(csv_file))){
    stop(print(paste("CSV file ",name_file," has incorrect columnnames : needed is ",
                     paste(csv_colnam, collapse = ";"),", current is ",
                     paste(colnames(csv_file), collapse = ";"),sep ="")))
  }

  if(length(csv_file[,1]) < min_list_csv){
    stop(print(paste("CSV file ", name_file,"is to short : rows needed is ",
                     min_list_csv,", current is ",
                     length(csv_file[,1]),sep ="")))
  }

  ## Correct for the columns
  corr_csv_file = ColumnChanger(csv_file,csv_class)

  return(corr_csv_file)
}

