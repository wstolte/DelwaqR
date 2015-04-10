## nu even zo, moet uit package komen
source("R/io.R")
source("R/plotfunctions.r")

workdir <- "d:\\MODELS\\ED\\"
modversion = "eco-scen4-run1-28-y2_2-day"

savedir <- file.path(workdir, modversion)
dir.create(savedir)
modarr <- his2arr(paste(workdir, modversion, ".his", sep = ""))

##===============================================
## MAKE ECOPLOT DIAGRAMS
##==============================================

limmod = c("Limit e", "Limit nit", "Limit pho", "Limit sil")

# locations = c("IM00", "IM01", "IM02", "IM03",
#               "IM04", "IM05", "IM06", "IM07",
#               "IM08", "IM09", "IM10")
# locations = "WZ460_Wierumergronde"
# locations = c("A1", "A2", "A3", "A4", "A5", "A6")
# locations = c("EDB01", "EDB03", "EDB04", "EDB05", "EDB06")
locations = c("WZ590_ZO_Lauwers_oos", "Ra", "Paap", "Huibertgat_oost",
              "Bocht_van_Watum", "Groote_Gat_noord", "Nieuwe_Statenzijl_bu")

DelwaqEcoplot(arr = modarr, locmod = locations, submod = "fPPtot", limmod = limmod, plottype = 1)
ggsave(file.path(savedir, paste(format(Sys.Date(), "%Y%m%d"), plotname, "ecoplot", modversion, ".png", sep = "_")),
       width = 10, height = 8)


