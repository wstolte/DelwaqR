
require(plyr)
require(reshape2)
require(ggplot2)
require(scales)


#' plot time series of stacked variables
#' @param unit the unit to display on the axis
#' @param locmod one or more locations
#' @param submod one or more substances
#' @param arr array with model results produced with \code{his2arr}
#' @return A standard plot of stacked variables
plotstacked <- function(unit, locmod, submod, arr) {

  df.map <- read.csv("d:/Tools&Scripts/Mapping tables/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)
  #   df.map <- read.csv("p:/1205711-edspa/2012waq/metingen/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)

  df.mod <- melt(arr[, locmod, submod], varnames=c("time", "location", "species"))
  df.mod$datetime  <- as.POSIXct(x=df.mod$time)
  df.mod$species   <- mapvalues(as.character(df.mod$species), from = df.map$Delwaq, to = df.map$Delwaq_long_name, warn_missing = F)
  df.mod$location  <- factor(df.mod$location,levels = locmod)
  #df.mod$species   <- factor(df.mod$species, levels = submod)

  p <- ggplot(df.mod,aes(x=datetime,y=value))
  p + geom_area(aes(fill=species)) +
    facet_grid(location~.) +
    labs(title=paste("Model version: ",modversion),x="Date",y=paste(filename," (",unit,")",sep="")) +
    scale_x_datetime(labels = date_format("%b"),breaks=date_breaks("months"))
}


saveseries <- function(plotdir, filename, locname, height) {
  dev.off()
  ggsave(file=paste(plotdir,"/", filename,"_",locname,"_area",".png",sep=""),
         width=10,height=height,dpi=300)
}

#====================================================================================
# Ecoplot functionality
# plots output variable (often fPPtot)
# and limiting factors in colored lines at the bottom
# author: Willem Stolte
#====================================================================================

#' plot time series variable with limiting factors
#' @param arr array with model results produced with \code{his2arr}
#' @param locmod one or more locations
#' @param submod one or more variables
#' @param limmod limiting factors to plot
#' @param plottype 1-solid lines of varying thickness; 2- thick lines of varying transparancy
#' @return A timeseries plot with variable and limiting factors
DelwaqEcoplot <- function (arr, locmod, submod, limmod, plottype) {

  #   if () {
  #     stop("Argument invalid.")
  #   }

  df.y <- arr2df(arr = arr, locmod = locmod, submod = submod)

  lablim = mapvalues(limmod,
                     c("Limit e", "Limit nit", "Limit pho", "Limit sil", "Limit gro", "Limit mor"),
                     c("L", "N","P","Si","gro", "mor")
  )

  df.lim <- arr2df(arr, locmod, limmod)
  df.lim$variable <- factor(df.lim$variable)

  #======= make dataframe for plotting ecoplot

  yy = range(ceiling(df.y$value*10)/10, na.rm = T)
  steps <- seq(-yy[2]/10, -length(limmod)*yy[2]/10, by= -yy[2]/10)
  df.lim$step <- steps[as.numeric(as.factor(df.lim$variable))]
  colnames(df.lim) <- mapvalues(colnames(df.lim), from = "variable", to = "limitation")
  df.lim$limitation <-  mapvalues(df.lim$limitation,
                                  c("Limit e", "Limit nit", "Limit pho", "Limit sil", "Limit gro", "Limit mor"),
                                  c("light", "nitrogen","phophorus","silica","growth", "mortality")
  )



  require(ggplot2)
  require(scales)

  ## define position of annotated text to indicate different limitations
  annotate.position.x <- as.POSIXct(as.numeric(min(df.lim$time)) - as.numeric(min(df.lim$time))/1200, origin = "1970-01-01 00:00:00")

  z = ggplot(aes(time, value), data = df.y)
  z = z + geom_line(aes(), color = "grey20", size = 0.5) + facet_wrap( ~ location)
  if(plottype == 1){
    z = z + geom_line(aes(x = time, y = step, color = limitation, size = value), data = df.lim)
  }
  if(plottype == 2) {
    z = z +  geom_line(aes(x = time, y = step, color = limitation, alpha = value), data = df.lim, size = 3)
  }
  #   labs(x = "date", y = paste(y_name, y_unit)) +
  z =  z +
    labs(x = "", y = "") +
    theme(text = element_text(size = 16)) +
    scale_x_datetime(breaks = date_breaks("2 months"),  minor_breaks = date_breaks("month"), labels = date_format("%b")) +
    scale_y_continuous(expand = c(0.15,0), breaks = pretty_breaks(n=2)(yy)) +#,
    #                        labels = comma_format(digits = 1)) +
    scale_size_continuous(range=c(0,4)) +
    annotate("text", x = annotate.position.x, y = steps, label = lablim, size = 3) +
    theme_bw(base_size = 12, base_family = "") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  z
}

#' plot time series variable with limiting factors
#' @param arr array with model results produced with \code{his2arr}
#' @param locmod one or more locations
#' @param submod one or more variables
#' @param limmod limiting factors to plot
#' @param plottype options: 1-solid lines of varying thickness; 2- thick lines of varying transparancy
#' @return A timeseries plot with variable and limiting factors
DelwaqEcoplot2 <- function (arr, locmod, submod, limmod, plottype) {

  #   if () {
  #     stop("Argument invalid.")
  #   }

  df.y <- arr2df(arr = arr, locmod = locmod, submod = submod)

  lablim = mapvalues(limmod,
                     c("Limit e", "Limit nit", "Limit pho", "Limit sil", "Limit gro", "Limit mor"),
                     c("L", "N","P","Si","gro", "mor")
  )

  df.lim <- arr2df(arr, locmod, limmod)
  df.lim$variable <- factor(df.lim$variable)

  #======= make dataframe for plotting ecoplot

  yy = range(ceiling(df.y$value*10)/10, na.rm = T)
  steps <- seq(-yy[2]/10, -length(limmod)*yy[2]/10, by= -yy[2]/10)
  df.lim$step <- steps[as.numeric(as.factor(df.lim$variable))]
  colnames(df.lim) <- mapvalues(colnames(df.lim), from = "variable", to = "limitation")
  df.lim$limitation <-  mapvalues(df.lim$limitation,
                                  c("Limit e", "Limit nit", "Limit pho", "Limit sil", "Limit gro", "Limit mor"),
                                  c("light", "nitrogen","phophorus","silica","growth", "mortality")
  )



  require(ggplot2)
  require(scales)

  ## define position of annotated text to indicate different limitations
  annotate.position.x <- as.POSIXct(as.numeric(min(df.lim$time)) - as.numeric(min(df.lim$time))/1200, origin = "1970-01-01 00:00:00")

  z = ggplot(aes(time, value), data = df.y) +
    #   geom_line(aes(), color = "grey20", size = 0.5) +
    facet_grid(location ~ .)
  if(plottype == 1){
    z = z + geom_line(aes(x = time, y = step, color = limitation, size = value), data = df.lim)
  }
  if(plottype == 2) {
    z = z +  geom_line(aes(x = time, y = step, color = limitation, alpha = value), data = df.lim, size = 3)
  }
  #   labs(x = "date", y = paste(y_name, y_unit)) +
  z =  z +
    labs(x = "", y = "") +
    theme(text = element_text(size = 16)) +
    scale_x_datetime(breaks = date_breaks("2 months"),  minor_breaks = date_breaks("month"), labels = date_format("%b")) +
    scale_y_continuous(expand = c(0.15,0), breaks = pretty_breaks(n=2)(yy)) +#,
    #                        labels = comma_format(digits = 1)) +
    scale_size_continuous(range=c(0,4)) +
    annotate("text", x = annotate.position.x, y = steps, label = lablim, size = 3) +
    theme_bw(base_size = 12, base_family = "") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  z
}
