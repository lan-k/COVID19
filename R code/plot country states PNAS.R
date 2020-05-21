

# library(readxl)
# library(RCurl)
# library(httr)
# library(tidyverse)
# library(runner)
# library(stringi)
# library(urltools)
library(RColorBrewer)


setwd("D:/work/SAHMRI/COVID-19/Data")

###read the data###
source("D:/work/SAHMRI/COVID-19/R code/data sources adj to Aus.R")

today <- as.character(format(Sys.Date(), "%Y%b%d"))
#save(region_data, file="COVID_Region_paper_28Apr2020.Rdata")

pop_density <- c(3.3,10060574/23844 ,5400,10715,488,12000,1011) #per sqkm
###order is "Australia","Lombardy", "Madrid", "New York", "Netherlands","Geneva","Ile-de-France","Iceland"
##order is madrid, geneva, iceland




####PLOTS########
cols <- brewer.pal(8,"Dark2")
cols[9] <-"black"

########x limits########

xmax1 = max(aus_rate$day, nyc_rate$day,madrid_rate$day,lombardy_rate$day,
            netherlands_rate$day,geneva_rate$day)  #italy is max
xmax2 = max(aus_rate$day, nsw_rate$day,vic_rate$day,qld_rate$day,
            wa_rate$day,sa_rate$day,tas_rate$day) #nsw is max
x1 = 1:xmax1
x2 = 1:xmax2


#####confirmed cases############
fn <- paste0("D:/work/SAHMRI/COVID-19/age adjusted paper/Cases ICU death ",paste0(today,".tif"))
tiff(file=fn, width = 2400, height = 2400, units = "px", res=300)
par(oma=c(1,1,1,6.5), xpd=F)
#par( mar=c(3, 4.7, 2, 0), mfrow=c(2,2)) #bottom, left, top,right
par( mar=c(2, 4.7, 2, 0), mfrow=c(2,2))

# fn <- paste0("D:/work/SAHMRI/COVID-19/age adjusted paper/Case comparisons ",paste0(today,".pdf"))
# pdf(file=fn)
# 
# par(oma=c(1,1,1,7), xpd=F)
# par( mar=c(2, 4.7, 2, 0), mfrow=c(2,1)) #bottom, left, top,right

ymax = max(c(max(aus_rate$cases,na.rm=T), max(nyc_rate$cases,na.rm=T),
             max(madrid_rate$cases,na.rm=T),max(lombardy_rate$cases,na.rm=T),
             max(netherlands_rate$cases,na.rm=T),max(geneva_rate$cases,na.rm=T)))


plot(x = x1, lombardy_rate$cases,log="y", type='o',  pch=20, 
     xlab = "", ylab="",  cex=0.9,
     main = "", xlim=c(1,xmax1),
     ylim=c(100,1000000), col=cols[1], yaxt = "n", font=2) #


title("A",adj=0, line=0.5)
axis(1, at=c(10,20,30,40,50,60,70,80),font=2, cex=0.9)
axis(2, c(100,1000,10000,100000,1000000), labels = c("100","1k","10k",'100k',""), 
     las = 1, font=2, cex=0.9, cex.axis=0.9)
mtext(side=2, text="Confirmed cases", line=3, font=2, cex=0.9, cex.axis=0.9)
#mtext(side=1, text="Days since 100 confirmed cases", line=3, font=2, cex=1)

#log - scale
lines(which(!is.na(madrid_rate$cases)), madrid_rate$cases[!is.na(madrid_rate$cases)], type='o',  pch=20,col=cols[2])
lines(which(!is.na(nyc_rate$cases)), nyc_rate$cases[!is.na(nyc_rate$cases)], type='o',  pch=20,col=cols[3] )
lines(which(!is.na(netherlands_rate$cases)), netherlands_rate$cases[!is.na(netherlands_rate$cases)], type='o',  pch=20,col=cols[4])
lines(which(!is.na(geneva_rate$cases)), geneva_rate$cases[!is.na(geneva_rate$cases)], type='o',  pch=20,col=cols[5])
lines(which(!is.na(iledefrance_rate$cases)), iledefrance_rate$cases[!is.na(iledefrance_rate$cases)], type='o',  pch=20,col=cols[6])
lines(which(!is.na(iceland_rate$cases)), iceland_rate$cases[!is.na(iceland_rate$cases)], type='o',  pch=20,col=cols[7])
lines(which(!is.na(sweden_rate$cases)), sweden_rate$cases[!is.na(sweden_rate$cases)], type='o',  pch=20,col=cols[8])

lines(which(!is.na(aus_rate$cases)), aus_rate$cases[!is.na(aus_rate$cases)], type='o',  pch=20,col=cols[9])


###case rate###
par( mar=c(2, 4.7, 2, 0))
ymax = max(c(max(aus_rate$case_rate,na.rm=T), max(nyc_rate$case_rate,na.rm=T),
             max(madrid_rate$case_rate,na.rm=T),max(lombardy_rate$case_rate,na.rm=T),
             max(netherlands_rate$case_rate,na.rm=T),max(geneva_rate$case_rate,na.rm=T)))



plot(x = x1, lombardy_rate$case_rate,log="y", type='o',  pch=20, 
     xlab = "", ylab="", 
     main = "", xlim=c(1,xmax1), 
     ylim=c(0.1,2*ymax), col=cols[1], yaxt = "n", font=2,cex=0.9)

title("B",adj=0, line=0.5)
axis(1, at=c(10,20,30,40,50,60,70,80),font=2, cex=0.9)
axis(2, c(0.1,1,10,100,1000), labels = c(0.1,1,10,100,"1k"),las = 1, font=2, cex=0.9)
mtext(side=2, text="Case rate per 100k", line=3, font=2, cex=0.9, cex.axis=0.9)
#mtext(side=1, text="Days since 100 confirmed cases", line=2.5, font=2, cex=0.9, cex.axis=0.9)


#log - scale
lines(which(!is.na(madrid_rate$case_rate)), madrid_rate$case_rate[!is.na(madrid_rate$case_rate)], type='o',  pch=20,col=cols[2])
lines(which(!is.na(nyc_rate$case_rate)), nyc_rate$case_rate[!is.na(nyc_rate$case_rate)], type='o',  pch=20,col=cols[3] )
lines(which(!is.na(netherlands_rate$case_rate)), netherlands_rate$case_rate[!is.na(netherlands_rate$case_rate)], type='o',  pch=20,col=cols[4])
lines(which(!is.na(geneva_rate$case_rate)), geneva_rate$case_rate[!is.na(geneva_rate$case_rate)], type='o',  pch=20,col=cols[5])
lines(which(!is.na(iledefrance_rate$case_rate)), iledefrance_rate$case_rate[!is.na(iledefrance_rate$case_rate)], type='o',  pch=20,col=cols[6])
lines(which(!is.na(iceland_rate$case_rate)), iceland_rate$case_rate[!is.na(iceland_rate$case_rate)], type='o',  pch=20,col=cols[7])
lines(which(!is.na(sweden_rate$case_rate)), sweden_rate$case_rate[!is.na(sweden_rate$case_rate)], type='o',  pch=20,col=cols[8])

lines(which(!is.na(aus_rate$case_rate)), aus_rate$case_rate[!is.na(aus_rate$case_rate)], type='o',  pch=20,col=cols[9])

#points(x2[1:length(aus_rate$hosp_rate)], aus_rate$hosp_rate, pch=20,col=cols[6])



########ICU & deaths###############

###add min x for ICeland 0 ICU occupancy

##ICU admissions

par( mar=c(3, 4.7, 2, 0))
iceland_rate$ICU_rate2 <- iceland_rate$ICU_rate
iceland_rate$ICU_rate2[iceland_rate$ICU_rate2 ==0] <- 0.001
iceland_rate$ICU_zero[iceland_rate$ICU_rate ==0] <- 0.001
plot(x = x1, lombardy_rate$ICU_rate,log="y", type='o',  pch=20,
     xlab = "", ylab="", 
     main = "", font=2,
     ylim=c(0.001,ymax), col=cols[1], yaxt = "n") #, type='o',  pch=20,

title("C",adj=0, line=0.5)
axis(1, at=c(10,20,30,40,50,60,70,80),font=2, cex=0.9)
axis(2, c(0.001, 0.01,0.1,1,10,100,1000), labels = c("0",0.01,0.1,1,10,100,"1k"), 
     las = 1, font=2, cex.axis=0.9)
mtext(side=2, text="ICU occupancy rate per 100k", line=2.9, font=2,cex=0.9, cex.axis=0.9)
mtext(side=1, text="Days since 100 confirmed cases", line=2.5, font=2,cex=0.9, cex.axis=0.9)



#log - scale
lines(which(!is.na(madrid_rate$ICU_rate)), madrid_rate$ICU_rate[!is.na(madrid_rate$ICU_rate)], type='o',  pch=20,col=cols[2])
lines(which(!is.na(nyc_rate$ICU_rate)), nyc_rate$ICU_rate[!is.na(nyc_rate$ICU_rate)], type='o',  pch=20,col=cols[3] )
lines(which(!is.na(netherlands_rate$ICU_rate)), netherlands_rate$ICU_rate[!is.na(netherlands_rate$ICU_rate)], type='o',  pch=20,col=cols[4])
lines(which(!is.na(geneva_rate$ICU_rate)), geneva_rate$ICU_rate[!is.na(geneva_rate$ICU_rate)], type='o',  pch=20,col=cols[5])
lines(which(!is.na(iledefrance_rate$ICU_rate)), iledefrance_rate$ICU_rate[!is.na(iledefrance_rate$ICU_rate)], type='o',  pch=20,col=cols[6])
lines(which(!is.na(iceland_rate$ICU_rate2)), iceland_rate$ICU_rate2[!is.na(iceland_rate$ICU_rate2)],  lty=2, col=cols[7])
lines(which(!is.na(iceland_rate$ICU_rate)), iceland_rate$ICU_rate[!is.na(iceland_rate$ICU_rate)], type='o',  pch=20,col=cols[7])
lines(which(!is.na(sweden_rate$ICU_rate)), sweden_rate$ICU_rate[!is.na(sweden_rate$ICU_rate)], type='o',  pch=20,col=cols[8])
lines(which(!is.na(aus_rate$ICU_rate)), aus_rate$ICU_rate[!is.na(aus_rate$ICU_rate)], type='o',  pch=20,col=cols[9])


###fix zeros

points(points(x2[1:length(iceland_rate$ICU_rate)], iceland_rate$ICU_zero, pch=20,col=cols[7]))


###Deaths

par( mar=c(3, 4.7, 2, 0))
plot(x = x1, lombardy_rate$death_rate,log="y", type='o',  pch=20,
     xlab = "", ylab="", 
     main = "",  font=2,
     ylim=c(0.001, ymax), col=cols[1], yaxt="n") #type='o',  pch=20,
#log - scale


title("D",adj=0, line=0.5)
axis(1, at=c(10,20,30,40,50,60,70,80),font=2, cex=0.9)
axis(2, c(0.001, 0.01,0.1,1,10,100,1000), labels = c(0.001,0.01,0.1,1,10,100,"1k"), las = 1, font=2, cex.axis=0.9)
mtext(side=2, text="Cumulative death rate per 100k", line=2.9, font=2,cex=0.9, cex.axis=0.9)
mtext(side=1, text="Days since 100 confirmed cases", line=2.5, font=2,cex=0.9, cex.axis=0.9)


lines(which(!is.na(madrid_rate$death_rate)), madrid_rate$death_rate[!is.na(madrid_rate$death_rate)], type='o',  pch=20,col=cols[2])
lines(which(!is.na(nyc_rate$death_rate)), nyc_rate$death_rate[!is.na(nyc_rate$death_rate)], type='o',  pch=20,col=cols[3] )
lines(which(!is.na(netherlands_rate$death_rate)), netherlands_rate$death_rate[!is.na(netherlands_rate$death_rate)], type='o',  pch=20,col=cols[4])
lines(which(!is.na(geneva_rate$death_rate)), geneva_rate$death_rate[!is.na(geneva_rate$death_rate)], type='o',  pch=20,col=cols[5])
lines(which(!is.na(iledefrance_rate$death_rate)), iledefrance_rate$death_rate[!is.na(iledefrance_rate$death_rate)], type='o',  pch=20,col=cols[6])
lines(which(!is.na(iceland_rate$death_rate)), iceland_rate$death_rate[!is.na(iceland_rate$death_rate)], type='o',  pch=20,col=cols[7])
lines(which(!is.na(sweden_rate$death_rate)), sweden_rate$death_rate[!is.na(sweden_rate$death_rate)], type='o',  pch=20,col=cols[8])
lines(which(!is.na(aus_rate$death_rate)), aus_rate$death_rate[!is.na(aus_rate$death_rate)], type='o',  pch=20,col=cols[9])


###overlay legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 8, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

leg = c("Australia",
        "Lombardy", "Madrid", "New York", "Netherlands", 
        "Geneva","Ile-de-France","Iceland","Sweden")     #, 
col_order = c(9,1:8)
legend("topright", legend=leg,  xpd = TRUE, inset=c(0,0),
       pch=20, col=cols[col_order],  cex=0.9,  text.font=2, bty="n") 

dev.off()







