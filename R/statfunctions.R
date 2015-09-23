##=========================================================================##
##                                                                         ##
##                 Start function "make.target.table" version 2            ##
##                                 -----------------                       ##
##  Function to make a table suitable for target diagram plots containing  ##
##             uRMSD and nBIAS for selected categories of data             ##
##                                                                         ##
##       Input:   formula: variables ~ .  (variables to be grouped by)     ##
##                 df (dataframe containing data                      ##
##                 val_obs (column with observed values)                   ##
##                 val_mod (column with modelled values)                   ##
##      Output:    df.target (dataframe with uRMSD and nbias)              ##
##   Reference:    Jolliff(2009) J Mar Sys, 76(1-2), 64-82                 ##
##      Author:    willem.stolte@deltares.nl                               ##
##  webaddress:    https://svn.oss.deltares.nl/repos/openearthtools/       ##
##                 trunk/r/applications/Delft3D/waq/target-function.R      ##
##  testscript:    https://svn.oss.deltares.nl/repos/openearthtools/       ##
##                 trunk/r/applications/Delft3D/waq/target-diagram.R       ##
##   copyright:    Deltares                                                ##
##                                                                         ##
##=========================================================================##

#' extract data from array into a dataframe for selected locations and substances,
#'
#' @param formulax formula to define the variables for statistics
#' @param df dataframe with observed and modelled values
#' @param val_obs the observed variable name
#' @param val_mod the modelled variable name
#' @param logtrans(logical) whether to logtransform the data before analysis
#' @return A dataframe with statistics to plot target diagram
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
make.target.table3 <- function (formulax, df, val_obs, val_mod, logtrans = F) {
  #   TESTDATA TO RUN THE FUNCTION AS SCRIPT
  #      df = stattable
  #     formulax = ~ variable
  #     val_obs = "value.x"
  #     val_mod = "value.y"
  #      logtrans = F
  #
  require(plyr)

  #   ## Do transformation
  #   if(logtrans) {
  #     min_obs <- (min(df$val_obs))
  #     min_mod <- (min(val_mod))
  #     min_all <- (min(min_obs, min_mod))
  #     val_obs <- (log(val_obs) + min_obs + 1)
  #     val_mod <- (log(val_mod) + min_mod + 1)
  #   }
  if(logtrans)  {
    print("log transformation used") }   else {
      print("no transformation")  }

  ## calculate squared differences (SD)
  if(logtrans)  {
    df.summary <- ddply(df, formulax, here(summarise),
                        observed = log(get(val_obs) + 1),
                        modelled = log(get(val_mod) + 1),
                        SD = ((log(get(val_obs) + 1) - mean(log(get(val_obs) + 1))) - (log(get(val_mod) + 1) - mean(log(get(val_mod) + 1))))^2
    )
  } else  {
    df.summary <- ddply(df, formulax, here(summarise),
                        observed = get(val_obs),
                        modelled = get(val_mod),
                        SD = ((get(val_obs) - mean(get(val_obs))) - (get(val_mod) - mean(get(val_mod))))^2
    )
  }


  ## calculate normalized root mean square difference (uRMSD)
  ## and normalized bias (nBIAS)

  df.target <- ddply(df.summary, formulax, summarise,
                     uRMSD = sqrt(mean(SD))*sign(sd(modelled)-sd(observed)),
                     nuRMSD = (sqrt(mean(SD))*sign(sd(modelled)-sd(observed)))/sd(observed),
                     nBIAS = (mean(modelled) - mean(observed))/sd(observed),
                     BIAS = mean(modelled) - mean(observed)
  )
  print("for target diagram:")
  print("use nuRMSD as normalized unbiased RMSD")
  print("use nBIAS as normalized bias")

  return(df.target)
}

####################### end function #########################################

# df.stat = read.csv("d:/weeber/Documents/Laptop/OpenEarthTools/Delft3D/waq/stattable.csv")
# str(df)
#
# make.target.table2(formulax = ~substance + location, df = df.stat, val_obs = "value.x",val_mod = "value.y")

#' produce dataframe with circle points for plotting
#' @param center
#' @param diameter
#' @param npoints
#' @return A dataframe with npoints points located on a circle

#' circleFun()
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
