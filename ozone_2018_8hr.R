###read 2015 ozone data, inspect vars
sample_oz15<- tidync("2015_UK_MDA8.nc")


#import ozone 2015 data
oz_2015 <- read_emep("2015_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_2015<- as.data.frame(oz_2015, xy=TRUE, na.rm=TRUE)
names(oz_2015)<- c('longitude', 'latitude', 'time', 'oz')
oz_2015$oz <- as.numeric(oz_2015$oz)


#import 2015 ozone observations
oz15_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                    year=2015, pollutant = c('O3'), hc=FALSE)
