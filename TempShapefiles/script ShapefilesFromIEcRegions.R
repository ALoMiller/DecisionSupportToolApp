library(rgdal)

setwd("/net/work4/LobsterGroup/Management/RightWhales/VerticalLineModeling/IEC/IEc Data_V2")

Regions=readOGR(dsn=getwd(), layer="Regions_20190401"); summary(Regions)
plot(Regions, xlim=Lim$x, ylim=Lim$y)
Lim=data.frame(locator(2))

Regions$RegionName

Regions$State=NA;
Regions$State[grep("Maine", Regions$RegionName)]="ME"
Regions$State[grep("Mass", Regions$RegionName)]="MA"
Regions$State[grep("NH", Regions$RegionName)]="NH"

plot(Regions, xlim=Lim$x, ylim=Lim$y, col=factor(Regions$State))
plot(Regions, lwd=3, border="black", add=TRUE)
plot(Regions, lwd=1, border="white", add=TRUE)

MaineState=Regions[grep("Maine State", Regions$RegionName), ]; plot(MaineState)
NHState=Regions[grep("NH", Regions$RegionName), ]; plot(NHState)
MassState=Regions[grep("Mass State", Regions$RegionName), ]; plot(MassState, col="gray80")

StateWaters=rbind(MaineState, NHState, MassState); plot(StateWaters)

MaineFederal=Regions[grep("Maine Nearshore", Regions$RegionName), ]; plot(MaineFederal)
MassFederal=Regions[grep("Mass Nearshore", Regions$RegionName), ]; plot(MassFederal)

FederalNearshore=rbind(MaineFederal, MassFederal); plot(FederalNearshore)

setwd("//net/work4/LobsterGroup/Management/RightWhales/DecisionSupportTool/TempShapefiles/")

writeOGR(obj=StateWaters, dsn=getwd(), layer="StateWaters", driver="ESRI Shapefile")
writeOGR(obj=FederalNearshore, dsn=getwd(), layer="FederalNearshore", driver="ESRI Shapefile")

SW=readOGR(dsn=getwd(), layer="StateWaters"); summary(SW)
plot(SW)

NF=readOGR(dsn=getwd(), layer="FederalNearshore"); summary(NF)
plot(NF)
