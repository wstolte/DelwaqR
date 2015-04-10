saveseries <- function(plotdir, filename, locname, height, plottype) {
  dev.off()
  ggsave(file=paste(plotdir,"/", filename,"_",locname, plottype, "_area",".png",sep=""),
         width=10,height=height,dpi=300) 
}
