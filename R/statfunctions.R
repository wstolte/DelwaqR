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

#' produce summary statistics from dataframe with matching measured and modelled values
#'
#' @param formulax formula to define the variables for statistics
#' @param df dataframe with observed and modelled values
#' @param val_obs the observed variable name
#' @param val_mod the modelled variable name
#' @param logtrans(logical) whether to logtransform the data before analysis
#' @return A dataframe with statistics (normalized unbiased RMSD and normalized BIAS) to plot target diagram
#' @examples
#' library(DelwaqR)
#' make.target.table3(~ substance + location + season, df.statii, "value.x", "value.y", logtrans = F)
make.target.table3 <- function (formulax, df, val_obs, val_mod, logtrans = F) {
  #   TESTDATA TO RUN THE FUNCTION AS SCRIPT
  #      df = stattable
  #     formulax = ~ variable
  #     val_obs = "value.x"
  #     val_mod = "value.y"
  #      logtrans = F
  #
  library(plyr)

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


## from plotrix package
## idea: convert to ggplot plotting function


taylor.diagram <- function (ref, model, add = FALSE, col = "red", pch = 19, pos.cor = TRUE, 
    xlab = "", ylab = "", main = "Taylor Diagram", show.gamma = TRUE, 
    ngamma = 3, gamma.col = 8, sd.arcs = 0, ref.sd = FALSE, sd.method = "sample", 
    grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9), pcex = 1, cex.axis = 1, 
    normalize = FALSE, mar = c(5, 4, 6, 6), ...) 
{
    grad.corr.full <- c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 
        1)
    R <- cor(ref, model, use = "pairwise")
    if (is.list(ref)) 
        ref <- unlist(ref)
    if (is.list(model)) 
        ref <- unlist(model)
    SD <- function(x, subn) {
        meanx <- mean(x, na.rm = TRUE)
        devx <- x - meanx
        ssd <- sqrt(sum(devx * devx, na.rm = TRUE)/(length(x[!is.na(x)]) - 
            subn))
        return(ssd)
    }
    subn <- sd.method != "sample"
    sd.r <- SD(ref, subn)
    sd.f <- SD(model, subn)
    if (normalize) {
        sd.f <- sd.f/sd.r
        sd.r <- 1
    }
    maxsd <- 1.5 * max(sd.f, sd.r)
    oldpar <- par("mar", "xpd", "xaxs", "yaxs")
    if (!add) {
        if (pos.cor) {
            if (nchar(ylab) == 0) 
                ylab = "Standard deviation"
            par(mar = mar)
            plot(0, xlim = c(0, maxsd), ylim = c(0, maxsd), xaxs = "i", 
                yaxs = "i", axes = FALSE, main = main, xlab = xlab, 
                ylab = ylab, type = "n", cex = cex.axis, ...)
            if (grad.corr.lines[1]) {
                for (gcl in grad.corr.lines) lines(c(0, maxsd * 
                  gcl), c(0, maxsd * sqrt(1 - gcl^2)), lty = 3)
            }
            segments(c(0, 0), c(0, 0), c(0, maxsd), c(maxsd, 
                0))
            axis.ticks <- pretty(c(0, maxsd))
            axis.ticks <- axis.ticks[axis.ticks <= maxsd]
            axis(1, at = axis.ticks, cex.axis = cex.axis)
            axis(2, at = axis.ticks, cex.axis = cex.axis)
            if (sd.arcs[1]) {
                if (length(sd.arcs) == 1) 
                  sd.arcs <- axis.ticks
                for (sdarc in sd.arcs) {
                  xcurve <- cos(seq(0, pi/2, by = 0.03)) * sdarc
                  ycurve <- sin(seq(0, pi/2, by = 0.03)) * sdarc
                  lines(xcurve, ycurve, col = "blue", lty = 3)
                }
            }
            if (show.gamma[1]) {
                if (length(show.gamma) > 1) 
                  gamma <- show.gamma
                else gamma <- pretty(c(0, maxsd), n = ngamma)[-1]
                if (gamma[length(gamma)] > maxsd) 
                  gamma <- gamma[-length(gamma)]
                labelpos <- seq(45, 70, length.out = length(gamma))
                for (gindex in 1:length(gamma)) {
                  xcurve <- cos(seq(0, pi, by = 0.03)) * gamma[gindex] + 
                    sd.r
                  endcurve <- which(xcurve < 0)
                  endcurve <- ifelse(length(endcurve), min(endcurve) - 
                    1, 105)
                  ycurve <- sin(seq(0, pi, by = 0.03)) * gamma[gindex]
                  maxcurve <- xcurve * xcurve + ycurve * ycurve
                  startcurve <- which(maxcurve > maxsd * maxsd)
                  startcurve <- ifelse(length(startcurve), max(startcurve) + 
                    1, 0)
                  lines(xcurve[startcurve:endcurve], ycurve[startcurve:endcurve], 
                    col = gamma.col)
                  if (xcurve[labelpos[gindex]] > 0) 
                    boxed.labels(xcurve[labelpos[gindex]], ycurve[labelpos[gindex]], 
                      gamma[gindex], border = FALSE)
                }
            }
            xcurve <- cos(seq(0, pi/2, by = 0.01)) * maxsd
            ycurve <- sin(seq(0, pi/2, by = 0.01)) * maxsd
            lines(xcurve, ycurve)
            bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
            medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
            smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
            segments(cos(bigtickangles) * maxsd, sin(bigtickangles) * 
                maxsd, cos(bigtickangles) * 0.97 * maxsd, sin(bigtickangles) * 
                0.97 * maxsd)
            par(xpd = TRUE)
            if (ref.sd) {
                xcurve <- cos(seq(0, pi/2, by = 0.01)) * sd.r
                ycurve <- sin(seq(0, pi/2, by = 0.01)) * sd.r
                lines(xcurve, ycurve)
            }
            points(sd.r, 0, cex = pcex)
            text(cos(c(bigtickangles, acos(c(0.95, 0.99)))) * 
                1.05 * maxsd, sin(c(bigtickangles, acos(c(0.95, 
                0.99)))) * 1.05 * maxsd, c(seq(0.1, 0.9, by = 0.1), 
                0.95, 0.99), cex = cex.axis)
            text(maxsd * 0.8, maxsd * 0.8, "Correlation", srt = 315, 
                cex = cex.axis)
            segments(cos(medtickangles) * maxsd, sin(medtickangles) * 
                maxsd, cos(medtickangles) * 0.98 * maxsd, sin(medtickangles) * 
                0.98 * maxsd)
            segments(cos(smltickangles) * maxsd, sin(smltickangles) * 
                maxsd, cos(smltickangles) * 0.99 * maxsd, sin(smltickangles) * 
                0.99 * maxsd)
        }
        else {
            x <- ref
            y <- model
            R <- cor(x, y, use = "pairwise.complete.obs")
            E <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
            xprime <- x - mean(x, na.rm = TRUE)
            yprime <- y - mean(y, na.rm = TRUE)
            sumofsquares <- (xprime - yprime)^2
            Eprime <- sqrt(sum(sumofsquares)/length(complete.cases(x)))
            E2 <- E^2 + Eprime^2
            if (add == FALSE) {
                maxray <- 1.5 * max(sd.f, sd.r)
                plot(c(-maxray, maxray), c(0, maxray), type = "n", 
                  asp = 1, bty = "n", xaxt = "n", yaxt = "n", 
                  xlab = xlab, ylab = ylab, main = main, cex = cex.axis)
                discrete <- seq(180, 0, by = -1)
                listepoints <- NULL
                for (i in discrete) {
                  listepoints <- cbind(listepoints, maxray * 
                    cos(i * pi/180), maxray * sin(i * pi/180))
                }
                listepoints <- matrix(listepoints, 2, length(listepoints)/2)
                listepoints <- t(listepoints)
                lines(listepoints[, 1], listepoints[, 2])
                lines(c(-maxray, maxray), c(0, 0))
                lines(c(0, 0), c(0, maxray))
                for (i in grad.corr.lines) {
                  lines(c(0, maxray * i), c(0, maxray * sqrt(1 - 
                    i^2)), lty = 3)
                  lines(c(0, -maxray * i), c(0, maxray * sqrt(1 - 
                    i^2)), lty = 3)
                }
                for (i in grad.corr.full) {
                  text(1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                    i^2), i, cex = 0.6)
                  text(-1.05 * maxray * i, 1.05 * maxray * sqrt(1 - 
                    i^2), -i, cex = 0.6)
                }
                seq.sd <- seq.int(0, 2 * maxray, by = (maxray/10))[-1]
                for (i in seq.sd) {
                  xcircle <- sd.r + (cos(discrete * pi/180) * 
                    i)
                  ycircle <- sin(discrete * pi/180) * i
                  for (j in 1:length(xcircle)) {
                    if ((xcircle[j]^2 + ycircle[j]^2) < (maxray^2)) {
                      points(xcircle[j], ycircle[j], col = "darkgreen", 
                        pch = ".")
                      if (j == 10) 
                        text(xcircle[j], ycircle[j], signif(i, 
                          2), cex = 0.5, col = "darkgreen")
                    }
                  }
                }
                seq.sd <- seq.int(0, maxray, length.out = 5)
                for (i in seq.sd) {
                  xcircle <- (cos(discrete * pi/180) * i)
                  ycircle <- sin(discrete * pi/180) * i
                  if (i) 
                    lines(xcircle, ycircle, lty = 3, col = "blue")
                  text(min(xcircle), -0.03 * maxray, signif(i, 
                    2), cex = 0.5, col = "blue")
                  text(max(xcircle), -0.03 * maxray, signif(i, 
                    2), cex = 0.5, col = "blue")
                }
                text(0, -0.08 * maxray, "Standard Deviation", 
                  cex = 0.7, col = "blue")
                text(0, -0.12 * maxray, "Centered RMS Difference", 
                  cex = 0.7, col = "darkgreen")
                points(sd.r, 0, pch = 22, bg = "darkgreen", cex = 1.1)
                text(0, 1.1 * maxray, "Correlation Coefficient", 
                  cex = 0.7)
            }
            S <- (2 * (1 + R))/(sd.f + (1/sd.f))^2
        }
    }
    points(sd.f * R, sd.f * sin(acos(R)), pch = pch, col = col, 
        cex = pcex)
    invisible(oldpar)
}