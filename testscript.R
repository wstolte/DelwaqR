
require(plyr)
require(ggplot2)
require(scales)
submap <- read.csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)
locmap <- read.csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2locations.csv", header = T, stringsAsFactors=FALSE)
locmod = c("Huibertgat_oost", "IMARES_st_2", "IMARES_st_3b", "Bocht_van_Watum", "IMARES_st_4b", "IMARES_st_5", "Groote_Gat_noord")

# submod = c("PO4", "NO3", "NH4", "Si"); plotname = "nutrients"
submod = c("Salinity", "ExtVl", "Chlfa", "OXY"); plotname = "light"

# modversion = "eco-scen4-run1-random-corr-spm-34_2_bal_all-day"
workdir <- "d:\\MODELS\\ED\\"
modversion = "eco-scen4-run1-random-corr-spm-34_2_bal_all-day"
savedir <- file.path(workdir, modversion)
dir.create(savedir)
modarr <- his2arr(paste(workdir, modversion, ".his", sep = ""))
moddat <- arr2df( arr = modarr, submod = submod, locmod = locmod)
moddat$variable   <- mapvalues(as.character(moddat$variable), from = submap$Delwaq, to = submap$Delwaq_long_name, warn_missing = F)
# moddat$variable <- factor(moddat$variable, levels = submod)

# # im_dat <-read.csv2("d:\\GIS-DATA\\Nederland\\EemsDoll\\naarPostGis\\IMARES\\2013_nutrients_long.csv",dec = ".")
im_dat <-read.csv2("d:\\GIS-DATA\\Nederland\\EemsDoll\\naarPostGis\\IMARES\\all_nutrients_long.csv",dec = ".")

im_dat$time <- as.POSIXct(im_dat$datetime, format = "%d-%m-%y %H:%M")
im_dat$variable   <- mapvalues(as.character(im_dat$variable), from = submap$short_name, to = submap$Delwaq, warn_missing = F)
im_dat$location <- mapvalues(as.character(im_dat$imst_loccode), from = locmap$IMARES_st, to = locmap$Delwaq_ED, warn_missing = F)
im_dat2 <- subset(im_dat, im_dat$variable %in% submod &
                    im_dat$location %in% locmod &
                    im_dat$time > min(moddat$time) &
                    im_dat$time < max(moddat$time))
im_dat2$variable   <- mapvalues(as.character(im_dat2$variable), from = submap$Delwaq, to = submap$Delwaq_long_name, warn_missing = F)

rws_dat <- read.csv2("d:\\GIS-DATA\\Nederland\\EemsDoll\\naarPostGis\\RWS\\MWTL_all.csv",dec = ".")
rws_dat$time <- as.POSIXct(rws_dat$datetime, format = "%Y-%m-%d %H:%M:%S")
rws_dat$variable   <- mapvalues(as.character(rws_dat$parhdhcod), from = submap$RWS_DONAR_parcod_hdh2, to = submap$Delwaq, warn_missing = F)
rws_dat$location <- mapvalues(as.character(rws_dat$locoms), from = locmap$locoms, to = locmap$Delwaq_ED, warn_missing = F)
rws_dat2 <- subset(rws_dat, rws_dat$variable %in% submod &
                     rws_dat$location %in% locmod &
                     rws_dat$time > min(moddat$time) &
                     rws_dat$time < max(moddat$time)
                   )
rws_dat2$variable   <- mapvalues(as.character(rws_dat2$variable), from = submap$Delwaq, to = submap$Delwaq_long_name, warn_missing = F)

#filter for high PO4 and NH4 measurements
rws_dat2 <- subset(rws_dat2, rws_dat2$wrd < 0.6 | rws_dat2$variable != "phosphate")
rws_dat2 <- subset(rws_dat2, rws_dat2$wrd < 0.6 | rws_dat2$variable != "ammonium")

# castim_dat <- dcast(im_dat, formula = time +  location ~ variable)

p <- ggplot(moddat,(aes(time, value)))
p + geom_line(color = "darkolivegreen", size = 1) +
  geom_point(data = im_dat2, aes(time, value), color = "blue", size = 3, alpha = 0.6) +
  geom_point(data = rws_dat2, aes(time, wrd), color = "red", size = 3, alpha = 0.6) +
  facet_grid(variable ~ location, scales = "free") +
  scale_x_datetime(breaks = date_breaks("2 months"),
                   labels = date_format("%b"))

ggsave(file.path(savedir, paste(plotname, modversion, ".png", sep = "_")),
       width = 12, height = 10)

##===============================================
## MAKE ECOPLOT DIAGRAMS
##==============================================

limmod = c("Limit e", "Limit nit", "Limit pho", "Limit sil", "Limit gro", "Limit mor")

locations = c("IM02", "IM03", "IM04")
DelwaqEcoplot(arr = modarr, locmod = locations, submod = "Chlfa", limmod = limmod, plottype = 1)


##===============================================
## MAKE TARGET DIAGRAMS
##==============================================
beginsummer <- as.POSIXct("2012-04-01")
endsummer <- as.POSIXct("2012-09-30")

im_dat2$time <- as.POSIXct(format(im_dat2$time, format = "%Y-%m-%d"))
stattable <- merge(im_dat2, moddat, by = c("time", "location", "variable"))

stattable2 <- subset(stattable, !is.na(stattable$value.x) & !is.na(stattable$value.y))
stattable2$season <- ifelse(stattable2$time < beginsummer |
                                stattable2$time > endsummer,
                              "winter","summer")

ttable <- make.target.table3(formulax = ~ variable + season, stattable2,
                             val_obs = "value.x", val_mod = "value.y", logtrans = F)

df.circle <- circleFun(c(0,0),2,npoints = 100)
df.sdcircle <- circleFun(c(0,0),1.4,npoints=100)

## Plot targetdiagram voor all groups

library(ggplot2)

ttable <- subset(ttable, ttable$nBIAS != Inf) # take out Inf's

z <- ifelse(max(abs(ttable$nuRMSD), na.rm = TRUE) >
              max(abs(ttable$nBIAS), na.rm = TRUE),
            max(abs(ttable$nuRMSD)),
            max(abs(ttable$nBIAS))) # maximum scale

zz <- ifelse(z > 1, 1.2 * z, 1.2)  # Litle bit of room for points at the outside
zz = 2 ## manual scale, overruling autoscale

p <- ggplot(ttable,aes(nuRMSD,nBIAS))
p +
  geom_point(aes(),size=4) +
  #   geom_point(aes(color = season),size=4) +
  geom_path(data=df.circle,aes(x,y)) +
  geom_path(data=df.sdcircle,aes(x,y), color="grey") +
  xlim(c(-zz,zz)) + ylim(c(-zz,zz)) +
  theme(aspect.ratio = 1) +
  facet_grid(season ~ variable) +
  theme(text = element_text(size = 16))

ggsave(file.path(savedir, paste(plotname, "IM_target_var_", modversion, ".png", sep = "_")),
       width = 10, height = 5)

##======================================================

im_dat2$time <- as.POSIXct(format(im_dat2$time, format = "%Y-%m-%d"))
stattable <- merge(im_dat2, moddat, by = c("time", "location", "variable"))

stattable2 <- subset(stattable, !is.na(stattable$value.x) & !is.na(stattable$value.y))
stattable2$season <- ifelse(stattable2$time < beginsummer |
                              stattable2$time > endsummer,
                            "winter","summer")
ttable <- make.target.table3(formulax = ~ variable + location + season, stattable2,
                             val_obs = "value.x", val_mod = "value.y", logtrans = F)

df.circle <- circleFun(c(0,0),2,npoints = 100)
df.sdcircle <- circleFun(c(0,0),1.4,npoints=100)

## Plot targetdiagram voor all groups

library(ggplot2)

ttable <- subset(ttable, ttable$nBIAS != Inf) # take out Inf's

z <- ifelse(max(abs(ttable$nuRMSD), na.rm = TRUE) >
              max(abs(ttable$nBIAS), na.rm = TRUE),
            max(abs(ttable$nuRMSD)),
            max(abs(ttable$nBIAS))) # maximum scale

zz <- ifelse(z > 1, 1.2 * z, 1.2)  # Litle bit of room for points at the outside
zz = 2 ## manual scale, overruling autoscale

p <- ggplot(ttable,aes(nuRMSD,nBIAS))
p +
  geom_point(aes(color = location),size=4) +
  #   geom_point(aes(color = season),size=4) +
  geom_path(data=df.circle,aes(x,y)) +
  geom_path(data=df.sdcircle,aes(x,y), color="grey") +
  xlim(c(-zz,zz)) + ylim(c(-zz,zz)) +
  theme(aspect.ratio = 1) +
  facet_grid(season ~ variable) +
  theme(text = element_text(size = 16))

ggsave(file.path(savedir, paste(plotname, "IM_target_var_loc_", modversion, ".png", sep = "_")),
       width = 10, height = 5)

#for rws data

rws_dat2$time <- as.POSIXct(format(rws_dat2$time, format = "%Y-%m-%d"))
stattable <- merge(rws_dat2, moddat, by = c("time", "location", "variable"))

stattable2 <- subset(stattable, !is.na(stattable$wrd) & !is.na(stattable$value))
stattable2$season <- ifelse(stattable2$time < beginsummer |
                              stattable2$time > endsummer,
                            "winter","summer")
ttable <- make.target.table3(formulax = ~ variable + season, stattable2,
                             val_obs = "wrd", val_mod = "value", logtrans = F)

df.circle <- circleFun(c(0,0),2,npoints = 100)
df.sdcircle <- circleFun(c(0,0),1.4,npoints=100)

## Plot targetdiagram voor all groups

library(ggplot2)

ttable <- subset(ttable, ttable$nBIAS != Inf) # take out Inf's

z <- ifelse(max(abs(ttable$nuRMSD), na.rm = TRUE) >
              max(abs(ttable$nBIAS), na.rm = TRUE),
            max(abs(ttable$nuRMSD)),
            max(abs(ttable$nBIAS))) # maximum scale

zz <- ifelse(z > 1, 1.2 * z, 1.2)  # Litle bit of room for points at the outside
zz = 2 ## manual scale, overruling autoscale

p <- ggplot(ttable,aes(nuRMSD,nBIAS))
p +
  geom_point(aes(),size=4) +
  #   geom_point(aes(color = season),size=4) +
  geom_path(data=df.circle,aes(x,y)) +
  geom_path(data=df.sdcircle,aes(x,y), color="grey") +
  xlim(c(-zz,zz)) + ylim(c(-zz,zz)) +
  theme(aspect.ratio = 1) +
  facet_grid(season ~ variable) +
  theme(text = element_text(size = 16))

ggsave(file.path(savedir, paste(plotname, "RWS_target_var_", modversion, ".png", sep = "_")),
       width = 10, height = 5)

##===============================================================

rws_dat2$time <- as.POSIXct(format(rws_dat2$time, format = "%Y-%m-%d"))
stattable <- merge(rws_dat2, moddat, by = c("time", "location", "variable"))

stattable2 <- subset(stattable, !is.na(stattable$wrd) & !is.na(stattable$value))
stattable2$season <- ifelse(stattable2$time < beginsummer |
                              stattable2$time > endsummer,
                            "winter","summer")
ttable <- make.target.table3(formulax = ~ variable + location + season, stattable2,
                             val_obs = "wrd", val_mod = "value", logtrans = F)

df.circle <- circleFun(c(0,0),2,npoints = 100)
df.sdcircle <- circleFun(c(0,0),1.4,npoints=100)

## Plot targetdiagram voor all groups

library(ggplot2)

ttable <- subset(ttable, ttable$nBIAS != Inf) # take out Inf's

z <- ifelse(max(abs(ttable$nuRMSD), na.rm = TRUE) >
              max(abs(ttable$nBIAS), na.rm = TRUE),
            max(abs(ttable$nuRMSD)),
            max(abs(ttable$nBIAS))) # maximum scale

zz <- ifelse(z > 1, 1.2 * z, 1.2)  # Litle bit of room for points at the outside
zz = 2 ## manual scale, overruling autoscale

p <- ggplot(ttable,aes(nuRMSD,nBIAS))
p +
  geom_point(aes(color = location),size=4) +
  #   geom_point(aes(color = season),size=4) +
  geom_path(data=df.circle,aes(x,y)) +
  geom_path(data=df.sdcircle,aes(x,y), color="grey") +
  xlim(c(-zz,zz)) + ylim(c(-zz,zz)) +
  theme(aspect.ratio = 1) +
  facet_grid(season ~ variable) +
  theme(text = element_text(size = 16))

ggsave(file.path(savedir, paste(plotname, "RWS_target_var_loc_", modversion, ".png", sep = "_")),
       width = 10, height = 5)

