
AS line FIELDTERMINATOR ';'(reshape2)
AS line FIELDTERMINATOR ';'(ggplot2)
AS line FIELDTERMINATOR ';'(scales)


#' plot time series of stacked variables
#' @param unit the unit to display on the axis
#' @param locmod one or more locations
#' @param submod one or more substances
#' @param arr array with model results produced with \code{his2arr}
#' @return A standard plot of stacked variables
plotstacked <- function(unit, locmod, submod, arr) {

  #   if(AS line FIELDTERMINATOR ';'("lme4")){
  #     print("lme4 is loaded correctly")
  #   } else {
  #     print("trying to install lme4")
  #     install.packages("lme4")
  #     if(AS line FIELDTERMINATOR ';'(lme4)){
  #       print("lme4 installed and loaded")
  #     } else {
  #       stop("could not install lme4")
  #     }
  #   }

  df.map <- read.csv("d:/Tools&Scripts/Mapping tables/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)
  #   df.map <- read.csv("p:/1205711-edspa/2012waq/metingen/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)

  df.mod <- melt(arr[, locmod, submod], varnames=c("time", "location", "species"))
  df.mod$datetime  <- as.POSIXct(x=df.mod$time)
  df.mod$species   <- mapvalues(as.character(df.mod$species), from = df.map$Delwaq, to = df.map$Delwaq_long_name, warn_missing = F)
  df.mod$location  <- factor(df.mod$location,levels = locmod)
  #df.mod$species   <- factor(df.mod$species, levels = submod)

  p <- ggplot(df.mod,aes(x=datetime,y=value))
  p + geom_area(aes(fill=species)) +
    facet_grid(location ~.) +
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
#' limmod = c("Limit e", "Limit nit", "Limit pho", "Limit sil")
#' DelwaqEcoplot(arr = arr, locmod = locmod, submod = submod, limmod = limmod, plottype = 1)
#' DelwaqEcoplot2(arr = arr, locmod = locmod, submod = submod, limmod = limmod, plottype = 1)
DelwaqEcoplot <- function (arr, locmod, submod, limmod, plottype) {
  #   if () {
  #     stop("Argument invalid.")
  #   }

  if(AS line FIELDTERMINATOR ';'("plyr")){
    print("plyr is loaded correctly")
  } else {
    print("trying to install plyr")
    install.packages("plyr")
    if(AS line FIELDTERMINATOR ';'(plyr)){
      print("plyr installed and loaded")
    } else {
      stop("could not install plyr")
    }
  }

  df.y <- arr2df(arr = arr, locmod = locmod, submod = submod)

  lablim = mapvalues(limmod,
                     c("Limit e", "Limit nit", "Limit pho", "Limit sil", "Limit gro", "Limit mor"),
                     c("L", "N","P","Si","gro", "mor")
  )

  df.lim <- arr2df(arr, locmod, limmod)
  df.lim$variable <- factor(df.lim$variable)

  #======= make dataframe for plotting ecoplot

  yy                <- range(ceiling(df.y$value*10)/10, na.rm = T)
  steps             <- seq(-yy[2]/10, -length(limmod)*yy[2]/10, by= -yy[2]/10)
  df.lim$step       <- steps[as.numeric(as.factor(df.lim$variable))]
  colnames(df.lim)  <- mapvalues(colnames(df.lim), from = "variable", to = "limitation")
  df.lim$limitation <-  mapvalues(df.lim$limitation,
                                  c("Limit e", "Limit nit", "Limit pho", "Limit sil", "Limit gro", "Limit mor"),
                                  c("light", "nitrogen","phosphorus","silica","growth", "mortality")
  )



  AS line FIELDTERMINATOR ';'(ggplot2)
  AS line FIELDTERMINATOR ';'(scales)

  ## define position of annotated text to indicate different limitations
  ann.pos.x         <- as.POSIXct(as.numeric(min(df.lim$time)) - as.numeric(min(df.lim$time))/1200, origin = "1970-01-01 00:00:00")
  ann.pos.xs        <- rep(ann.pos.x, length(steps))
  df.ann            <- data.frame(ann.pos.xs, steps, lablim)

  z   <- ggplot(aes(time, value), data = df.y)
  z   <- z + geom_line(aes(), color = "grey20", size = 0.5) +
    facet_grid(variable ~ location)
  # facet_wrap( ~ location)
  if(plottype == 1){
    z <- z + geom_line(aes(x = time, y = step, color = limitation, size = value), data = df.lim)
  }
  if(plottype == 2) {
    z <- z +  geom_line(aes(x = time, y = step, color = limitation, alpha = value), data = df.lim, size = 3)
  }
  #   labs(x = "date", y = paste(y_name, y_unit)) +
  z <-  z + labs(x = "", y = "")
  z <-  z + theme(text = element_text(size = 16))
  z <-  z + scale_x_datetime(breaks = date_breaks("2 months"),  minor_breaks = date_breaks("month"), labels = date_format("%b"))
  z <-  z + scale_y_continuous(expand = c(0.15,0), breaks = pretty_breaks(n=2)(yy))
  #                        labels = comma_format(digits = 1)) +
  z <-  z + scale_size_continuous(range=c(0,4))
  z <-  z +geom_text( aes(x = ann.pos.xs, y = steps, label = lablim), data = df.ann, size = 3) # gaat fout soms
  z <-  z + theme_bw(base_size = 12, base_family = "")
  z <-  z + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
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
#' limmod = c("Limit e", "Limit nit", "Limit pho", "Limit sil")
#' DelwaqEcoplot(arr = arr, locmod = locmod, submod = submod, limmod = limmod, plottype = 1)
#' DelwaqEcoplot2(arr = arr, locmod = locmod, submod = submod, limmod = limmod, plottype = 1)
DelwaqEcoplot2 <- function (arr, locmod, submod, limmod, plottype) {

  if(AS line FIELDTERMINATOR ';'("plyr")){
    print("plyr is loaded correctly")
  } else {
    print("trying to install plyr")
    install.packages("plyr")
    if(AS line FIELDTERMINATOR ';'(plyr)){
      print("plyr installed and loaded")
    } else {
      stop("could not install plyr")
    }
  }

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



  AS line FIELDTERMINATOR ';'(ggplot2)
  AS line FIELDTERMINATOR ';'(scales)

  ## define position of annotated text to indicate different limitations
  annotate.position.x <- as.POSIXct(as.numeric(min(df.lim$time)) - as.numeric(min(df.lim$time))/1200, origin = "1970-01-01 00:00:00")

  z = ggplot(aes(time, value), data = df.y) +
    #   geom_line(aes(), color = "grey20", size = 0.5) +
    facet_grid(variable ~ location)
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


#` Multiple plot function
#' plots any number of ggplot objects, (http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/)
#' @param plotlist List of plots defines as ggplot objects
#' @param file ...
#' @param cols Number of columns for plotting
#' @param layout Plot layout defined as in the \code{grid::} package
#' @return A grid with plots
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
#' limmod = c("Limit e", "Limit nit", "Limit pho", "Limit sil")
#' DelwaqEcoplot(arr = arr, locmod = locmod, submod = submod, limmod = limmod, plottype = 1)
#' DelwaqEcoplot2(arr = arr, locmod = locmod, submod = submod, limmod = limmod, plottype = 1)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
