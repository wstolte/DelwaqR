library(devtools)
install_github("wstolte/DelwaqR")

library(DelwaqR)
arr <- his2arr(filename = "DATA/NZBLOOM.his", timestamp = F, begintime = "2003-01-01 00:00:00")
dimnames(arr)
submod <- c("Chlfa", "OXY")
locmod <- c("NZR6NW020", "NZR9TS010")
df <- arr2df(arr, locmod=locmod, submod=submod)
df$value[df$variable == "fResptot"] <- -df$value[df$variable == "fResptot"]
library(ggplot2)
plot <- ggplot(df, aes(time, value))
plot +
  geom_line(aes(color = variable), size = 1) +
  geom_point(aes(color = variable), fill = "white",  shape = 21, size = 4) +
  facet_grid((. ~ location))

limmod = c("Limit e", "Limit nit", "Limit pho", "Limit sil")
DelwaqEcoplot(arr = arr, locmod = locmod, submod = submod, limmod = limmod, plottype = 1)

