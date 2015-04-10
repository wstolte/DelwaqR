
require(plyr)
require(ggplot2)
require(scales)
submap <- read.csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)
locmap <- read.csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2locations.csv", header = T, stringsAsFactors=FALSE)
# locmod = c("Huibertgat_oost", "IMARES_st_2", "IMARES_st_3b", "Bocht_van_Watum", "IMARES_st_4b", "IMARES_st_5", "Groote_Gat_noord")
locmod = c("Huibertgat_oost", "Bocht_van_Watum", "Groote_Gat_noord")

# submod = c("PO4", "NO3", "NH4", "Si"); plotname = "nutrients"
submod = c("ExtVl", "Chlfa", "SS"); plotname = "light+spm"

# modversion = "eco-scen4-run1-random-corr-spm-34_2_bal_all-day"
workdir <- "d:\\MODELS\\ED\\"
versions <- c("eco-scen4-run1-23-y2-day",
              "eco-scen4-run1-24-y2-day",
              "eco-scen4-run1-28-y2-day",
              "eco-scen4-run1-28-y2_2-day",
              "eco-scen4-run1-random-corr-spm-31-day",
              "eco-scen4-run1-random-corr-spm-32-day",
              "eco-scen4-run1-random-corr-spm-34_2-day")

for(ii in seq(1,length(versions))) {
modversion <- versions[1]
  savedir <- file.path(workdir, modversion)
dir.create(savedir)
modarr <- his2arr(paste(workdir, modversion, ".his", sep = ""))
moddat <- arr2df( arr = modarr, submod = submod, locmod = locmod)
moddat$variable <- factor(moddat$variable, levels = submod)
moddat$variable   <- mapvalues(as.character(moddat$variable), from = submap$Delwaq, to = submap$Delwaq_long_name, warn_missing = F)

rws_dat <- read.csv2("d:\\GIS-DATA\\Nederland\\EemsDoll\\naarPostGis\\RWS\\MWTL_all.csv",dec = ".")
rws_dat$time <- as.POSIXct(rws_dat$datetime, format = "%Y-%m-%d %H:%M:%S")
rws_dat$variable   <- mapvalues(as.character(rws_dat$parhdhcod), from = submap$RWS_DONAR_parcod_hdh2, to = submap$Delwaq, warn_missing = F)
rws_dat$location <- mapvalues(as.character(rws_dat$locoms), from = locmap$locoms, to = locmap$Delwaq_ED, warn_missing = F)
rws_dat2 <- subset(rws_dat, rws_dat$variable %in% submod &
                     rws_dat$location %in% locmod &
                     rws_dat$time > (min(moddat$time) - (0*365*24*60*60)) &
                     rws_dat$time < max(moddat$time)
                   )
rws_dat2$variable   <- mapvalues(as.character(rws_dat2$variable), from = submap$Delwaq, to = submap$Delwaq_long_name, warn_missing = F)

#filter for high PO4 and NH4 measurements
rws_dat2 <- subset(rws_dat2, rws_dat2$wrd < 0.6 | rws_dat2$variable != "phosphate")
rws_dat2 <- subset(rws_dat2, rws_dat2$wrd < 0.6 | rws_dat2$variable != "ammonium")
rws_dat2 <- subset(rws_dat2, rws_dat2$wrd < 1e11)

p <- ggplot(moddat,(aes(time, value)))
p + geom_line(aes(color = location), size = 1) +
#   geom_point(data = im_dat2, aes(time, value), color = "blue", size = 3, alpha = 0.6) +
  geom_point(data = rws_dat2, aes(time, wrd), color = "black", size = 3, alpha = 0.6) +
  facet_grid(variable ~ location, scales = "free") +
  scale_x_datetime(breaks = date_breaks("2 months"),
                   labels = date_format("%b"))

ggsave(file.path(savedir, paste(plotname, modversion, ".png", sep = "_")),
       width = 8, height = 6)


##===============================================
## MAKE TARGET DIAGRAMS
##==============================================
beginsummer <- as.POSIXct("2012-04-01")
endsummer <- as.POSIXct("2012-09-30")

#for rws data

rws_dat2$time <- as.POSIXct(format(rws_dat2$time, format = "%Y-%m-%d"))
moddat$time <- as.POSIXct(format(moddat$time, format = "%Y-%m-%d"))

stattable <- merge(rws_dat2, moddat, by = c("time", "location", "variable"))

stattable2 <- subset(stattable, !is.na(stattable$wrd) & !is.na(stattable$value))

q <- ggplot(stattable2, aes(time, wrd))
q + geom_point(aes()) +
  geom_line(aes(x=time, y=value), data = stattable2) +
  facet_grid(location~variable)

ttable <- make.target.table3(formulax = ~ variable + location, stattable2,
                             val_obs = "wrd", val_mod = "value", logtrans = F)
ttable$location <- as.factor(ttable$location)
ttable$location <- factor(ttable$location, levels = locmod)
levels(ttable$location)
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
  facet_grid(variable ~ .) +
  theme(text = element_text(size = 12))

ggsave(file.path(savedir, paste(plotname, "RWS_target_var_loc", modversion, ".png", sep = "_")),
       width = 6, height = 6)

##===============================================================

ttable <- make.target.table3(formulax = ~ variable , stattable2,
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
  facet_grid(variable ~ .) +
  theme(text = element_text(size = 16))

ggsave(file.path(savedir, paste(plotname, "RWS_target_var_", modversion, ".png", sep = "_")),
       width = 6, height = 6)

}

