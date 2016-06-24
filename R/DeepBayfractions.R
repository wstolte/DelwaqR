AS line FIELDTERMINATOR ';'(devtools)
AS line FIELDTERMINATOR ';'(DelwaqR)
AS line FIELDTERMINATOR ';'(ggplot2)
AS line FIELDTERMINATOR ';' (caTools)
AS line FIELDTERMINATOR ';'(reshape2)
AS line FIELDTERMINATOR ';'(plyr)
AS line FIELDTERMINATOR ';'(stringr)
AS line FIELDTERMINATOR ';'(scales)

moddir<-"p:/1208342-deepbay/Phase_2/DWAQ/runs/"
plotdir<-"p:/1208342-deepbay/Phase_2/DWAQ/Rplots/Tango/"

Mod2013<-his2arr(paste(moddir,"tango/waq_deepbay_2013_frac_y2.his",sep=""))
Mod2020<-his2arr(paste(moddir,"tango/waq_deepbay_2020_frac_y2.his",sep=""))
Mod2030<-his2arr(paste(moddir,"tango/waq_deepbay_2030_frac_y2.his",sep=""))

locex=c("DB01","DB03","DB04","DB05","DB06","DB07","DB08","DB09","DB10","DB11","DB12","DB13","DB14","DB15","DB16",
  "DB17","DB18","DB19","DB20","DB21","DB22","DB23","DB24","DB25","DB26","DB27","DB28","DB29","DB30","DB31","DB32","DB33",
  "DB34","DB35","DB36","DB37","DB38","DB39","DB40","DB41","DM1","DM2","DM3","DM4","DM5","S01","S02","S03","S04",
  "S06","S07","S08","S14","S15","S16")
loc=c("DM1","DM2","DM3","DM4","DM5")
subN=c("NH401","NH402","NH403","NH404","NH405","NO301","NO302","NO303","NO304","NO305","PON101","PON102","PON103","PON104",
  "PON105","PON201","PON202","PON203","PON204","PON205","PON301","PON302","PON303","PON304","PON305","PON401","PON402",
  "PON403","PON404","PO405","DON01","DON02","DON03","DON04","DON05")
subP=c("PO401","PO402","PO403","PO404","PO405","AAP01","AAP02","AAP03","AAP04","AAP05","VIVP01","VIVP02","VIVP03","VIVP04",
  "VIVP05","APATP01","APATP02","APATP03","APATP04","APATP05","POP101","POP102","POP103","POP104","POP105","POP201","POP202",
  "POP203","POP204","POP205","POP301","POP302","POP303","POP304","POP305","POP401","POP402","POP403","POP404","POP405",
  "DOP01","DOP02","DOP03","DOP04","DOP05")

Mod2013N<-arr2df(Mod2013,locmod=loc,submod=subN)
Mod13Nex<-arr2df(Mod2013,locmod=locex,submod=subN)
Mod2013N$season <- ifelse(Mod2013N$time >= as.POSIXct("2013-05-01 00:00:00") & Mod2013N$time < as.POSIXct("2013-09-01 00:00:00") , "wet", "dry")

Mod2013N$var <- str_split_fixed(Mod2013N$variable, pattern = "0", n = 2)[,1]
Mod2013N$frac <- str_split_fixed(Mod2013N$variable, pattern = "0", n = 2)[,2]
Mod2013N[Mod2013N$value>=0,]

a2013N<-ddply(Mod2013N, .(season,location,frac,var), summarize, mean=mean(value))
s2013N<-ddply(a2013N,.(season,location,frac),summarize,sum=sum(mean))
s2013N$frac<-mapvalues(as.character(s2013N$frac),c("1","2","3","4","5"), c("Initial","HongKong Load","Shenzhen Load","Boundary","Sediment"))
write.csv(s2013N,"p:/1208342-deepbay/Phase_2/DWAQ/scenarios/tracers/2013N.csv")

Mod13Nex$var <- str_split_fixed(Mod13Nex$variable, pattern = "0", n = 2)[,1]
Mod13Nex$frac <- str_split_fixed(Mod13Nex$variable, pattern = "0", n = 2)[,2]
Mod13Nex[Mod13Nex$value>=0,]

a13Nex<-ddply(Mod13Nex, .(location,frac,var), summarize, mean=mean(value))
s13Nex<-ddply(a13Nex,.(location,frac),summarize,sum=sum(mean))
s13Nex$frac<-mapvalues(as.character(s3Nex$frac),c("1","2","3","4","5"), c("Initial","HongKong Load","Shenzhen Load","Boundary","Sediment"))
s13Nex$location<-mapvalues(as.character(s13Nex$location),c("S14"),c("DB02/S14"))
write.csv(s13Nex,"p:/1208342-deepbay/Phase_2/DWAQ/scenarios/tracers/2013N_extended.csv")


theme_set(theme_bw(15))
ggplot(s13Nex,aes(location,sum,fill=frac))+ 
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="Accent")+
  #facet_wrap(~season)+
  labs(y="mg N/l",title="2013 nitrogen")+
theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggsave(file=paste(plotdir,"2013N.png",sep=""),  width=8,height=6,dpi=300)   
ggsave(file=paste(plotdir,"2013Next.png",sep=""),  width=15,height=5,dpi=300) 

Mod2013P<-arr2df(Mod2013,locmod=loc,submod=subP)
Mod2013P$season <- ifelse(Mod2013P$time >= as.POSIXct("2013-05-01 00:00:00") & Mod2013P$time < as.POSIXct("2013-09-01 00:00:00") , "wet", "dry")

Mod2013P$var <- str_split_fixed(Mod2013P$variable, pattern = "0", n = 2)[,1]
Mod2013P$frac <- str_split_fixed(Mod2013P$variable, pattern = "0", n = 2)[,2]
Mod2013P[Mod2013P$value>=0,]

a2013P<-ddply(Mod2013P, .(season,location,frac,var), summarize, mean=mean(value))
s2013P<-ddply(a2013P,.(season,location,frac),summarize,sum=sum(mean))
s2013P$frac<-mapvalues(as.character(s2013P$frac),c("1","2","3","4","5"), c("Initial","HongKong Load","Shenzhen Load","Boundary","Sediment"))
write.csv(s2013P,"p:/1208342-deepbay/Phase_2/DWAQ/scenarios/tracers/2013P.csv")

theme_set(theme_bw(15))
ggplot(s2013P,aes(location,sum,fill=frac))+ 
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="Accent")+
  facet_wrap(~season)+
  labs(y="mg P/l",title="2013 phosphorus")
ggsave(file=paste(plotdir,"2013P.png",sep=""),  width=8,height=6,dpi=300)   
