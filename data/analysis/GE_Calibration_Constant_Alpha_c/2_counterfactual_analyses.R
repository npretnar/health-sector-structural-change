rm(list=ls())

#setwd("~/Dropbox/US_HEALTH_MACRO/Data_Work/GE_Calibration_Constant_Alpha_c")

# figure/table results output directory
results_dir = "../../../results"

# analysis directory
#analysis_dir = "data/analysis"

# prints in-line numbers and data references
sink(paste0(results_dir,"/2_counterfactual_analyses_SCRIPT_GE_Calibration_Constant_Alpha_c.txt")) # prints in-line data into a txt file

print("#########################################################################")
print("#########################################################################")
print("#########################################################################")
print("Now running Calibration R script for GE_Calibration_Constant_Alpha_c ... ")
print("#########################################################################")
print("#########################################################################")
print("#########################################################################")
print("#########################################################################")

plot_counters = TRUE

# parameters
parms_min <- read.table("parms_min.txt", quote="\"", comment.char="")

### MU ### 
mu_DATA <- read.table("mu.txt", quote="\"", comment.char="")
# no more growth in mark-ups
Tend = (2050-1950+5)/5
mu = c(mu_DATA$V1[1],mu_DATA$V1,rep(mu_DATA[nrow(mu_DATA),1],Tend-nrow(mu_DATA)-1))
print("Health Sector Relative Markups")
print("1955")
print(mu[2])
print("2010")
print(mu[13])

######################## 
print("Now plotting the counterfactuals. In order these are ...")
print("NOTE: in the code, we use the terms 'cost disease' and 'unbalanced technical change' interchangeably.")
print("1) Fixed mu; only cost disease and GE effects")
print("2) Fixed g_h and g_c; only mu and GE effects")
print("3) Fixed GE effects; only mu and cost disease")
print("4) Fixed g_h and g_c, mu; Only Demand Effects")
print("5) Fixed g_h and g_c, and GE effects; only mu")
print("6) Fixed mu, and GE effects; only mu; only cost disease")
# number of counterfactuals
Ncounters = 6

# read in q-data (1960 = 1)
p_data <- t(read.table("data_moments1.txt", quote="\"", comment.char=""))

# p-predicted (1960 = 1)
p_predicted <- t(read.table("siml_moments1.txt", quote="\"", comment.char=""))

# counterfactuals, in order fixing
# ... mu, gc, gh, gz, gn, zeta, zeta + gz, zeta + gz + gn, gc + gh, gc + gh + gn + gz
p_counter_files = list.files(pattern="p_counter")
p_counters = list()
for(i in 1:length(p_counter_files)){
  p_counters[[i]] <- t(read.table(paste0(p_counter_files[[i]]), quote="\"", comment.char=""))
}

# period 3 in the counters is 1960
p_counters_array = as.data.frame(do.call(cbind,p_counters))
row.names(p_counters_array) <- NULL
# normalize everything to period t=3
p_counters_normalized = p_counters_array
for(t in 1:nrow(p_counters_array)){
  p_counters_normalized[t,] = p_counters_array[t,] / p_counters_array[3,]
}

# plot out to 2050, with data and predicted values included
p_counters_normalized = p_counters_normalized[1:Tend,]
p_counters_normalized$data = NA
p_counters_normalized$predicted = NA
for(i in 1:12){
  p_counters_normalized$data[i+2] = p_data[i]
  p_counters_normalized$predicted[i+2] = p_predicted[i]
}

# plot
p_counters_normalized$year = seq(1950,2050,5)
ymin = min(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE) 
ymax = max(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE) 
png(filename=paste(results_dir,"/figureD2a.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2020,5),],plot(year,V1,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
#for(i in 1:11){
#  lines(p_counters_normalized$year,p_counters_normalized[,i],col=cols[i],lty=1+i,lwd=2)
#}
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2020,5),],points(year,data,pch=17,col="black",lwd=4))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2020,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2020,5),],lines(year,data,col="black",lwd=1,lty=1))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2020,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2020,10),las=1)
title(ylab="Relative Price of Health Care",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.75,
       legend = c("Data","Predicted"),
       #,expression(paste(mu,"=1",sep="")),
                  # expression(paste(g[c],",",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],",",g[n],"=0"))
                  # ),
       pch = c(17,1), # ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
       lty=c(1,3),lwd=1.4, #2,4,12,3,5,6,7,8,9,10,11,12),
       col = c("black","black")) #,"blue","darkorange","gold2","red","darkgreen","purple","goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon"),lwd=1.2,cex=0.6)
dev.off()

#### plot with counters ####
if(plot_counters){
# no's (1:3)
ymin = min(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#p_counters_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD7a.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],plot(year,V1,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),"year"],p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),i],col=cols[i],lty=1,lwd=1)
  points(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),"year"],p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),i],col=cols[i],pch=4+i,lwd=1.2)
}
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Relative Price of Health Care",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
        expression(paste("No Demand Effects")),
        expression(paste("No Markups")),
        expression(paste("No Technical Change")) #,
        #expression(paste(mu,"=1",sep="")),
        #expression(paste(g[c],"=",g[h],"=0")),
        #expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
        # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
        # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
        # expression(paste(g[c],",",g[h],"=0")),
        # expression(paste(g[c],",",g[h],",",g[z],"=0")),
        # expression(paste(g[c],"=0")),
        # expression(paste(g[h],"=0")),
        # expression(paste(g[z],"=0")),
        # expression(paste(g[n],"=0")),
        # expression(paste(g[zeta],"=0")),
        # expression(paste(g[z],",",g[zeta],"=0")),
        ),
       pch = c(17,1,5:7),
     #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
# yay's (4:6)
ymin = min(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#p_counters_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD8a.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],plot(year,V4,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),"year"],p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],lty=1,lwd=1)
  points(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),"year"],p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],pch=4+i,lwd=1.2)
}
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Relative Price of Health Care",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("Only Demand Effects")),
                  expression(paste("Only Markups")),
                  expression(paste("Only Technical Change"))
               #   expression(paste(mu,"=1",sep="")),
               #   expression(paste(g[c],"=",g[h],"=0")),
               #   expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[z],"=",g[zeta],"=",g[n],"=0",",",mu,"=1"))
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
}


#### what does population aging look like under these different counterfactuals? how many years of life do different agents gain or lose, relative to the prediction? ###
# read in q-data (1960 = 1)
le_data <- t(read.table("data_moments2.txt", quote="\"", comment.char=""))

# q-predicted (1960 = 1)
le_predicted <- t(read.table("siml_moments2.txt", quote="\"", comment.char=""))

# counterfactuals, in order fixing
# ... mu, gc, gh, gz, gn, zeta, zeta + gz, zeta + gz + gn, gc + gh, gc + gh + gn + gz
le_counter_files = list.files(pattern="le_counter")
le_counters = list()
for(i in 1:length(le_counter_files)){
  le_counters[[i]] <- t(read.table(paste0(le_counter_files[[i]]), quote="\"", comment.char=""))
}

# period 3 in the counters is 1960
le_counters_array = as.data.frame(do.call(cbind,le_counters))
row.names(le_counters_array) <- NULL
# normalize everything to period t=3
le_counters_normalized = le_counters_array
for(t in 1:nrow(p_counters_array)){
  le_counters_normalized[t,] = le_counters_array[t,] 
}

# plot out to 2050, with data and predicted values included
Tend = (2050-1950+5)/5
le_counters_normalized = le_counters_normalized[1:Tend,]
le_counters_normalized$data = NA
le_counters_normalized$predicted = NA
for(i in 1:12){
  le_counters_normalized$data[i+2] = le_data[i]
  le_counters_normalized$predicted[i+2] = le_predicted[i]
}

# plot
le_counters_normalized$year = seq(1950,2050,5)
ymin = min(le_counters_normalized[le_counters_normalized$year  %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
ymax = max(le_counters_normalized[le_counters_normalized$year  %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
png(filename=paste(results_dir,"/figureD2b.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2020,5),],plot(year,V1,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
#for(i in 1:10){
#  lines(le_counters_normalized$year,le_counters_normalized[,i],col=cols[i],lty=1+i,lwd=2)
#}
with(le_counters_normalized[le_counters_normalized$year  %in% seq(1960,2020,5),],points(year,data,pch=17,col="black",lwd=4))
with(le_counters_normalized[le_counters_normalized$year  %in% seq(1960,2020,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(le_counters_normalized[le_counters_normalized$year  %in% seq(1960,2020,5),],lines(year,data,col="black",lwd=1,lty=1))
with(le_counters_normalized[le_counters_normalized$year  %in% seq(1960,2020,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2020,10),las=1)
title(ylab="Life Expectancy at Age 20",cex.lab=1)
legend(x= "bottomright",inset=0,cex=0.75,
       legend = c("Data","Predicted"),
       #,expression(paste(mu,"=1",sep="")),
       # expression(paste(g[c],",",g[h],"=0",",",mu,"=1")),
       # expression(paste(g[c],",",g[h],"=0")),
       # expression(paste(g[c],",",g[h],",",g[z],"=0")),
       # expression(paste(g[c],"=0")),
       # expression(paste(g[h],"=0")),
       # expression(paste(g[z],"=0")),
       # expression(paste(g[n],"=0")),
       # expression(paste(g[zeta],"=0")),
       # expression(paste(g[z],",",g[zeta],"=0")),
       # expression(paste(g[z],",",g[zeta],",",g[n],"=0"))
       # ),
       pch = c(17,1), # ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
       lty=c(1,3),lwd=1.4, #2,4,12,3,5,6,7,8,9,10,11,12),
       col = c("black","black")) #,"blue","darkorange","gold2","red","darkgreen","purple","goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon"),lwd=1.2,cex=0.6)
dev.off()

#### plot with counters ####
if(plot_counters){
# no's (1:3)
ymin = min(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#le_counters_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD7b.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],plot(year,V1,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),"year"],le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),i],col=cols[i],lty=1,lwd=1)
  points(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),"year"],le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),i],col=cols[i],pch=4+i,lwd=1.2)
}
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Life Expectancy at Age 20",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("No Demand Effects")),
                  expression(paste("No Markups")),
                  expression(paste("No Technical Change")) #,
                  #expression(paste(mu,"=1",sep="")),
                  #expression(paste(g[c],"=",g[h],"=0")),
                  #expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
# yay's (4:6)
ymin = min(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#le_counters_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD8b.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],plot(year,V4,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),"year"],le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],lty=1,lwd=1)
  points(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),"year"],le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],pch=4+i,lwd=1.2)
}
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Life Expectancy at Age 20",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("Only Demand Effects")),
                  expression(paste("Only Markups")),
                  expression(paste("Only Technical Change"))
                  #   expression(paste(mu,"=1",sep="")),
                  #   expression(paste(g[c],"=",g[h],"=0")),
                  #   expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[z],"=",g[zeta],"=",g[n],"=0",",",mu,"=1"))
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
}

# ## life expectancy -- years lost in 2015 relative to prediction ##
# print("Years of Life Expectancy Lost Relative to Prediction in 2015 (negative numbers mean gained relative to prediction):")
# print("Data")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$data[14])
# print("Fixed mu -- no market concentration")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V1[14])
# print("Fixed A_c, A_h, pop0 -- no change to TFP, relative TFP, or aggregate health productivity")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V2[14])
# print("Fixed A_c, A_h, mu -- no supply-side changes, only aging")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V3[14])
# print("Fixed A_c -- no TFP growth for consumption")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V4[14])
# print("Fixed A_h -- no TFP growth for health services")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V5[14])
# print("Fixed Z -- no TFP growth for productivity of health care at generating healthy outcomes")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V6[14])
# print("Fixed pop0 -- no initial population growth")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V7[14])
# print("Fixed zeta -- no change to age-specific health productivity")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V8[14])
# print("Fixed zeta, Z -- no change to any health productivity")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V9[14])
# print("Fixed zeta, Z, pop0 -- no change to any health productivity or population")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V10[14])
# print("Fixed A_c and A_h -- no change to TFP or sectoral relative TFP")
# print(le_counters_normalized$predicted[14] - le_counters_normalized$V11[14])

#### what does the health share look like under these different counterfactuals? ###
he_share_data <- t(read.table("data_moments3.txt", quote="\"", comment.char=""))
he_share_predicted <- t(read.table("siml_moments3.txt", quote="\"", comment.char=""))

# retransform
he_share_data = 1/(1+exp(he_share_data))
he_share_predicted = 1/(1+exp(he_share_predicted))

# counterfactuals, in order fixing
# ... mu, gc, gh, gz, gn, zeta, zeta + gz, zeta + gz + gn, gc + gh, gc + gh + gn + gz
he_share_counter_files = list.files(pattern="he_counter")
he_share_counters = list()
for(i in 1:length(le_counter_files)){
  he_share_counters[[i]] <- t(read.table(paste0(he_share_counter_files[[i]]), quote="\"", comment.char=""))
}

# period 3 in the counters is 1960
he_counters_array = as.data.frame(do.call(cbind,he_share_counters))
row.names(he_counters_array) <- NULL
# normalize everything to period t=3
he_counters_normalized = he_counters_array
for(t in 1:nrow(he_counters_array)){
  he_counters_normalized[t,] = he_counters_array[t,] 
}

# plot out to 2050, with data and predicted values included
Tend = (2050-1950+5)/5
he_counters_normalized = he_counters_normalized[1:Tend,]
he_counters_normalized$data = NA
he_counters_normalized$predicted = NA
for(i in 1:12){
  he_counters_normalized$data[i+2] = he_share_data[i]
  he_counters_normalized$predicted[i+2] = he_share_predicted[i]
}

# plot
he_counters_normalized$year = seq(1950,2050,5)
ymin = min(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
ymax = max(he_counters_normalized[,c("data","predicted")],na.rm=TRUE)
png(filename=paste(results_dir,"/figureD2c.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2020,5),],plot(year,V1,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
#for(i in 1:10){
#  lines(he_counters_normalized$year,he_counters_normalized[,i],col=cols[i],lty=1+i,lwd=2)
#}
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2020,5),],points(year,data,pch=17,col="black",lwd=4))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2020,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2020,5),],lines(year,data,col="black",lwd=1,lty=1))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2020,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2020,10),las=1)
title(ylab="Health Share of Aggregate Expenditure",cex.lab=1)
legend(x= "topleft",inset=0,cex=0.75,
       legend = c("Data","Predicted"),
       #,expression(paste(mu,"=1",sep="")),
       # expression(paste(g[c],",",g[h],"=0",",",mu,"=1")),
       # expression(paste(g[c],",",g[h],"=0")),
       # expression(paste(g[c],",",g[h],",",g[z],"=0")),
       # expression(paste(g[c],"=0")),
       # expression(paste(g[h],"=0")),
       # expression(paste(g[z],"=0")),
       # expression(paste(g[n],"=0")),
       # expression(paste(g[zeta],"=0")),
       # expression(paste(g[z],",",g[zeta],"=0")),
       # expression(paste(g[z],",",g[zeta],",",g[n],"=0"))
       # ),
       pch = c(17,1), # ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
       lty=c(1,3),lwd=1.4, #2,4,12,3,5,6,7,8,9,10,11,12),
       col = c("black","black")) #,"blue","darkorange","gold2","red","darkgreen","purple","goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon"),lwd=1.2,cex=0.6)
dev.off()

#### plot with counters ####
if(plot_counters){
# no's (1:3)
ymin = min(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#he_counters_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD7c.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],plot(year,V1,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),"year"],he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),i],col=cols[i],lty=1,lwd=1)
  points(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),"year"],he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),i],col=cols[i],pch=4+i,lwd=1.2)
}
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Health Share of Aggregate Expenditure",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("No Demand Effects")),
                  expression(paste("No Markups")),
                  expression(paste("No Technical Change")) #,
                  #expression(paste(mu,"=1",sep="")),
                  #expression(paste(g[c],"=",g[h],"=0")),
                  #expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
# yay's (4:6)
ymin = min(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#he_counters_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD8c.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],plot(year,V4,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),"year"],he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],lty=1,lwd=1)
  points(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),"year"],he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],pch=4+i,lwd=1.2)
}
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Health Share of Aggregate Expenditure",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("Only Demand Effects")),
                  expression(paste("Only Markups")),
                  expression(paste("Only Technical Change"))
                  #   expression(paste(mu,"=1",sep="")),
                  #   expression(paste(g[c],"=",g[h],"=0")),
                  #   expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[z],"=",g[zeta],"=",g[n],"=0",",",mu,"=1"))
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
}

# ## health share -- percentage point difference from 1960-2015 ##
# print("Growth in health share from 1960 to 2015 (percentage points):")
# print("Data")
# print(he_counters_normalized$data[14] - he_counters_normalized$data[3])
# print("Predicted")
# print(he_counters_normalized$predicted[14] - he_counters_normalized$predicted[3])
# print("Fixed mu -- no market concentration")
# print(he_counters_normalized$V1[14]- he_counters_normalized$V1[3])
# print("Fixed A_c, A_h, Z -- no change to TFP, relative TFP, or aggregate health productivity")
# print(he_counters_normalized$V2[14]- he_counters_normalized$V2[3])
# print("Fixed A_c, A_h, mu -- no supply-side changes, only aging")
# print(he_counters_normalized$V3[14]- he_counters_normalized$V3[3])
# print("Fixed A_c -- no TFP growth for consumption")
# print(he_counters_normalized$V4[14]- he_counters_normalized$V4[3])
# print("Fixed A_h -- no TFP growth for health services")
# print(he_counters_normalized$V5[14]- he_counters_normalized$V5[3])
# print("Fixed Z -- no TFP growth for productivity of health care at generating healthy outcomes")
# print(he_counters_normalized$V6[14]- he_counters_normalized$V6[3])
# print("Fixed pop0 -- no initial population growth")
# print(he_counters_normalized$V7[14]- he_counters_normalized$V7[3])
# print("Fixed zeta -- no change to age-specific health productivity")
# print(he_counters_normalized$V8[14]- he_counters_normalized$V8[3])
# print("Fixed zeta, Z -- no change to any health productivity")
# print(he_counters_normalized$V9[14]- he_counters_normalized$V9[3])
# print("Fixed zeta, Z, pop0 -- no change to any health productivity or population")
# print(he_counters_normalized$V10[14]- he_counters_normalized$V10[3])
# print("Fixed A_c and A_h -- no change to TFP or sectoral relative TFP")
# print(he_counters_normalized$V11[14]- he_counters_normalized$V11[3])

#################### plot capital and labor shares in health ####################
# capital (K_h)
k_share_data <- t(read.table("data_moments4.txt", quote="\"", comment.char=""))
k_share_predicted <- t(read.table("siml_moments4.txt", quote="\"", comment.char=""))

# retransform
k_share_data = 1/(1+exp(k_share_data))
k_share_predicted = 1/(1+exp(k_share_predicted))

# counterfactuals, in order fixing
# ... mu, gc, gh, gz, gn, zeta, zeta + gz, zeta + gz + gn, gc + gh, gc + gh + gn + gz
k_counter_files = list.files(pattern="hk_counter")
k_counters = list()
for(i in 1:length(k_counter_files)){
  k_counters[[i]] <- t(read.table(paste0(k_counter_files[[i]]), quote="\"", comment.char=""))
}

# period 3 in the counters is 1960
k_counters_array = as.data.frame(do.call(cbind,k_counters))
row.names(k_counters_array) <- NULL
# normalize everything to period t=3
k_share_normalized = k_counters_array
for(t in 1:nrow(k_counters_array)){
  k_share_normalized[t,] = k_counters_array[t,] #/ k_counters_array[3,]
}

# plot out to 2050, with data and predicted values included
k_share_normalized = k_share_normalized[1:Tend,]
k_share_normalized$data = NA
k_share_normalized$predicted = NA
for(i in 1:12){
  k_share_normalized$data[i+2] = k_share_data[i]
  k_share_normalized$predicted[i+2] = k_share_predicted[i]
}

# plot
k_share_normalized$year = seq(1950,2050,5)
ymin = min(k_share_normalized[k_share_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
ymax = max(k_share_normalized[k_share_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
png(filename=paste(results_dir,"/figureD2d.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2020,5),],plot(year,data,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
#for(i in 1:10){
#  lines(he_counters_normalized$year,he_counters_normalized[,i],col=cols[i],lty=1+i,lwd=2)
#}
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2020,5),],points(year,data,pch=17,col="black",lwd=4))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2020,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2020,5),],lines(year,data,col="black",lwd=1,lty=1))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2020,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2020,10),las=1)
title(ylab="Health Production Share of Capital",cex.lab=1)
legend(x= "bottomright",inset=0,cex=0.75,
       legend = c("Data","Predicted"),
       #,expression(paste(mu,"=1",sep="")),
       # expression(paste(g[c],",",g[h],"=0",",",mu,"=1")),
       # expression(paste(g[c],",",g[h],"=0")),
       # expression(paste(g[c],",",g[h],",",g[z],"=0")),
       # expression(paste(g[c],"=0")),
       # expression(paste(g[h],"=0")),
       # expression(paste(g[z],"=0")),
       # expression(paste(g[n],"=0")),
       # expression(paste(g[zeta],"=0")),
       # expression(paste(g[z],",",g[zeta],"=0")),
       # expression(paste(g[z],",",g[zeta],",",g[n],"=0"))
       # ),
       pch = c(17,1), # ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
       lty=c(1,3),lwd=1.4, #2,4,12,3,5,6,7,8,9,10,11,12),
       col = c("black","black")) #,"blue","darkorange","gold2","red","darkgreen","purple","goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon"),lwd=1.2,cex=0.6)
dev.off()

#### plot with counters ####
if(plot_counters){
# no's (1:3)
ymin = min(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#k_share_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD7d.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],plot(year,V1,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),"year"],k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),i],col=cols[i],lty=1,lwd=1)
  points(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),"year"],k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),i],col=cols[i],pch=4+i,lwd=1.2)
}
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Health Production Share of Capital",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("No Demand Effects")),
                  expression(paste("No Markups")),
                  expression(paste("No Technical Change")) #,
                  #expression(paste(mu,"=1",sep="")),
                  #expression(paste(g[c],"=",g[h],"=0")),
                  #expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
# yay's (4:6)
ymin = min(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#k_share_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD8d.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],plot(year,V4,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),"year"],k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],lty=1,lwd=1)
  points(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),"year"],k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],pch=4+i,lwd=1.2)
}
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Health Production Share of Capital",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("Only Demand Effects")),
                  expression(paste("Only Markups")),
                  expression(paste("Only Technical Change"))
                  #   expression(paste(mu,"=1",sep="")),
                  #   expression(paste(g[c],"=",g[h],"=0")),
                  #   expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[z],"=",g[zeta],"=",g[n],"=0",",",mu,"=1"))
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
}

# 
# ## health capital share -- percentage point difference from 1960-2015 ##
# print("Growth in health capital share from 1960 to 2015 (percentage points):")
# print("Data")
# print((k_share_normalized$data[14] - k_share_normalized$data[3]) )
# print("Predicted")
# print((k_share_normalized$predicted[14] - k_share_normalized$predicted[3]))
# print("Fixed mu -- no market concentration")
# print((k_share_normalized$V1[14]- k_share_normalized$V1[3]) )
# print("Fixed A_c, A_h, Z -- no change to TFP, relative TFP, or aggregate health productivity")
# print((k_share_normalized$V2[14]- k_share_normalized$V2[3]) )
# print("Fixed A_c, A_h, mu -- no supply-side changes, only aging")
# print((k_share_normalized$V3[14]- k_share_normalized$V3[3]) )
# print("Fixed A_c -- no TFP growth for consumption")
# print((k_share_normalized$V4[14]- k_share_normalized$V4[3]) )
# print("Fixed A_h -- no TFP growth for health services")
# print((k_share_normalized$V5[14]- k_share_normalized$V5[3]) )
# print("Fixed Z -- no TFP growth for productivity of health care at generating healthy outcomes")
# print((k_share_normalized$V6[14]- k_share_normalized$V6[3]) )
# print("Fixed pop0 -- no initial population growth")
# print((k_share_normalized$V7[14]- k_share_normalized$V7[3]) )
# print("Fixed zeta -- no change to age-specific health productivity")
# print((k_share_normalized$V8[14]- k_share_normalized$V8[3]) )
# print("Fixed zeta, Z -- no change to any health productivity")
# print((k_share_normalized$V9[14]- k_share_normalized$V9[3]) )
# print("Fixed zeta, Z, pop0 -- no change to any health productivity or population")
# print((k_share_normalized$V10[14]- k_share_normalized$V10[3]) )
# print("Fixed A_c and A_h -- no change to TFP or sectoral relative TFP")
# print((k_share_normalized$V11[14]- k_share_normalized$V11[3]) )


##################### labor share in health production ########################
# labor (L_h)
l_share_data <- t(read.table("data_moments5.txt", quote="\"", comment.char=""))
l_share_predicted <- t(read.table("siml_moments5.txt", quote="\"", comment.char=""))

# retransform
l_share_data = 1/(1+exp(l_share_data))
l_share_predicted = 1/(1+exp(l_share_predicted))

# counterfactuals, in order fixing
# ... mu, gc, gh, gz, gn, zeta, zeta + gz, zeta + gz + gn, gc + gh, gc + gh + gn + gz
l_counter_files = list.files(pattern="hl_counter")
l_counters = list()
for(i in 1:length(l_counter_files)){
  l_counters[[i]] <- t(read.table(paste0(l_counter_files[[i]]), quote="\"", comment.char=""))
}

# period 3 in the counters is 1960
l_counters_array = as.data.frame(do.call(cbind,l_counters))
row.names(l_counters_array) <- NULL
# normalize everything to period t=3
l_share_normalized = l_counters_array
for(t in 1:nrow(l_counters_array)){
  l_share_normalized[t,] = l_counters_array[t,] #/ l_counters_array[3,]
}

# plot out to 2050, with data and predicted values included
l_share_normalized = l_share_normalized[1:Tend,]
l_share_normalized$data = NA
l_share_normalized$predicted = NA
for(i in 1:12){
  l_share_normalized$data[i+2] = l_share_data[i]
  l_share_normalized$predicted[i+2] = l_share_predicted[i]
}


# plot
l_share_normalized$year = seq(1950,2050,5)
ymin = min(l_share_normalized[l_share_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
ymax = max(l_share_normalized[l_share_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
png(filename=paste(results_dir,"/figureD2e.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2020,5),],plot(year,data,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
#for(i in 1:10){
#  lines(he_counters_normalized$year,he_counters_normalized[,i],col=cols[i],lty=1+i,lwd=2)
#}
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2020,5),],points(year,data,pch=17,col="black",lwd=4))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2020,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2020,5),],lines(year,data,col="black",lwd=1,lty=1))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2020,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2020,10),las=1)
title(ylab="Health Production Share of Labor",cex.lab=1)
legend(x= "bottomright",inset=0,cex=0.75,
       legend = c("Data","Predicted"),
       #,expression(paste(mu,"=1",sep="")),
       # expression(paste(g[c],",",g[h],"=0",",",mu,"=1")),
       # expression(paste(g[c],",",g[h],"=0")),
       # expression(paste(g[c],",",g[h],",",g[z],"=0")),
       # expression(paste(g[c],"=0")),
       # expression(paste(g[h],"=0")),
       # expression(paste(g[z],"=0")),
       # expression(paste(g[n],"=0")),
       # expression(paste(g[zeta],"=0")),
       # expression(paste(g[z],",",g[zeta],"=0")),
       # expression(paste(g[z],",",g[zeta],",",g[n],"=0"))
       # ),
       pch = c(17,1), # ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
       lty=c(1,3),lwd=1.4, #2,4,12,3,5,6,7,8,9,10,11,12),
       col = c("black","black")) #,"blue","darkorange","gold2","red","darkgreen","purple","goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon"),lwd=1.2,cex=0.6)
dev.off()

#### plot with counters ####
if(plot_counters){
# no's (1:3)
ymin = min(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#l_share_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD7e.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],plot(year,V1,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),"year"],l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),i],col=cols[i],lty=1,lwd=1)
  points(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),"year"],l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),i],col=cols[i],pch=4+i,lwd=1.2)
}
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Health Production Share of Labor",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("No Demand Effects")),
                  expression(paste("No Markups")),
                  expression(paste("No Technical Change")) #,
                  #expression(paste(mu,"=1",sep="")),
                  #expression(paste(g[c],"=",g[h],"=0")),
                  #expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
# yay's (4:6)
ymin = min(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#l_share_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD8e.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],plot(year,V4,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),"year"],l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],lty=1,lwd=1)
  points(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),"year"],l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],pch=4+i,lwd=1.2)
}
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Health Production Share of Labor",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("Only Demand Effects")),
                  expression(paste("Only Markups")),
                  expression(paste("Only Technical Change"))
                  #   expression(paste(mu,"=1",sep="")),
                  #   expression(paste(g[c],"=",g[h],"=0")),
                  #   expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[z],"=",g[zeta],"=",g[n],"=0",",",mu,"=1"))
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
}

## health labor share -- percentage point difference from 1960-2015 ##
# print("Growth in health labor share from 1960 to 2015 (percentage points):")
# print("Data")
# print((l_share_normalized$data[14] - l_share_normalized$data[3]) )
# print("Predicted")
# print((l_share_normalized$predicted[14] - l_share_normalized$predicted[3]))
# print("Fixed mu -- no market concentration")
# print((l_share_normalized$V1[14]- l_share_normalized$V1[3]) )
# print("Fixed A_c, A_h, Z -- no change to TFP, relative TFP, or aggregate health productivity")
# print((l_share_normalized$V2[14]- l_share_normalized$V2[3]) )
# print("Fixed A_c, A_h, mu -- no supply-side changes, only aging")
# print((l_share_normalized$V3[14]- l_share_normalized$V3[3]) )
# print("Fixed A_c -- no TFP growth for consumption")
# print((l_share_normalized$V4[14]- l_share_normalized$V4[3]) )
# print("Fixed A_h -- no TFP growth for health services")
# print((l_share_normalized$V5[14]- l_share_normalized$V5[3]) )
# print("Fixed Z -- no TFP growth for productivity of health care at generating healthy outcomes")
# print((l_share_normalized$V6[14]- l_share_normalized$V6[3]) )
# print("Fixed pop0 -- no initial population growth")
# print((l_share_normalized$V7[14]- l_share_normalized$V7[3]) )
# print("Fixed zeta -- no change to age-specific health productivity")
# print((l_share_normalized$V8[14]- l_share_normalized$V8[3]) )
# print("Fixed zeta, Z -- no change to any health productivity")
# print((l_share_normalized$V9[14]- l_share_normalized$V9[3]) )
# print("Fixed zeta, Z, pop0 -- no change to any health productivity or population")
# print((l_share_normalized$V10[14]- l_share_normalized$V10[3]) )
# print("Fixed A_c and A_h -- no change to TFP or sectoral relative TFP")
# print((l_share_normalized$V11[14]- l_share_normalized$V11[3]) )

#################### growth analyses ####################
# compute average annual GDP growth over 5-year intervals (not GDP per-cap)
#library(fredr)
#fredr_set_key('0120bb1cd22575483ecf9608ee6ca0cb')
#gdp = fredr_series_observations(series_id="GDPC1",frequency="a")
#save(gdp,file="gdp.Rdata")
load("../../raw/gdp.Rdata")
gdp$year = as.numeric(substr(as.character(gdp$date),1,4))
data_years = seq(1960,2015,5)
gdp = gdp[gdp$year %in% c(1955,data_years,2020),]
# compute five year average annual growth rate
gdp$growth = NA
for(t in 2:length(gdp$year)){
  gdp$growth[t] = (gdp$value[t] / gdp$value[t-1])^0.2 - 1
}
gdp <- na.omit(gdp) # 1960 growth is 1960/1955 average, etc.
# simulated
gy_sim <- read.table("gy_sim.txt", quote="\"", comment.char="")
gy_sim = as.vector(unlist(gy_sim))
# subset to data years to avoid transition from steady state to transition path
gy_sim = as.data.frame(list(year = seq(1955,2050,5),gy_sim=gy_sim)) # 1955 is 1955/1950 growth
gy_sim_all = gy_sim
gy_sim = merge(gy_sim,gdp[,c("year","growth")],by="year")

# plot predicted growth with data
ymin = min(gy_sim[gy_sim$year %in% seq(1960,2020,5),c("gy_sim","growth")],na.rm=TRUE)
ymax = max(gy_sim[gy_sim$year %in% seq(1960,2020,5),c("gy_sim","growth")],na.rm=TRUE)
png(filename=paste(results_dir,"/figureD2f.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],plot(year,gy_sim,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],points(year,growth,pch=17,col="black",lwd=4))
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],points(year,gy_sim,pch=1,col="black",lwd=2))
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],lines(year,growth,col="black",lwd=1,lty=1))
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],lines(year,gy_sim,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2020,10),las=1)
title(ylab="Average Annual GDP Growth Rate",cex.lab=1)
legend(x= "topright",inset=0,cex=0.75,
       legend = c("Data","Predicted"),
       pch = c(17,1), # ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
       lty=c(1,3),lwd=1.4, #2,4,12,3,5,6,7,8,9,10,11,12),
       col = c("black","black")) #,"blue","darkorange","gold2","red","darkgreen","purple","goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon"),lwd=1.2,cex=0.6)
dev.off()

###### gy counterfactuals #######
# import counterfactuals
gy_counter_files = list.files(pattern="gy_counter")
gy_counter = list()
for(i in 1:length(gy_counter_files)){
  gy_counter[[i]] <- t(read.table(paste0(gy_counter_files[[i]]), quote="\"", comment.char=""))
}

# period 2 in the counters is 1960 on 1955 growth
gy_counters_array = as.data.frame(do.call(cbind,gy_counter))
row.names(gy_counters_array) <- NULL
gy_counters_normalized = gy_counters_array
for(t in 1:nrow(gy_counters_array)){
  gy_counters_normalized[t,] = gy_counters_array[t,] 
}

# plot out to 2050, with data and predicted values included
Tend = (2050-1950+5)/5
gy_counters_normalized = gy_counters_normalized[1:Tend,]
gy_counters_normalized$data = NA
gy_counters_normalized$predicted = NA
for(i in 1:13){ # we have one more growth year
  gy_counters_normalized$data[i+1] = gy_sim$growth[i]
  gy_counters_normalized$predicted[i+1] = gy_sim$gy_sim[i]
}

# plot
gy_counters_normalized$year = seq(1955,2055,5)
# cut off at 2040 
gy_counters_normalized_all = gy_counters_normalized
#gy_counters_normalized = gy_counters_normalized[gy_counters_normalized$year <=2020 & gy_counters_normalized$year >= 1960,]

#### plot with counters ####
if(plot_counters){
# no's (1:3)
ymin = min(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#gy_counters_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD7f.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],plot(year,V1,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),"year"],gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),i],col=cols[i],lty=1,lwd=1)
  points(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),"year"],gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),i],col=cols[i],pch=4+i,lwd=1.2)
}
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Average Annual GDP Growth Rate",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "bottomleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("No Demand Effects")),
                  expression(paste("No Markups")),
                  expression(paste("No Technical Change")) #,
                  #expression(paste(mu,"=1",sep="")),
                  #expression(paste(g[c],"=",g[h],"=0")),
                  #expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
# yay's (4:6)
ymin = min(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#gy_counters_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
png(filename=paste(results_dir,"/figureD8f.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],plot(year,V4,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
for(i in 1:3){
  lines(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),"year"],gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],lty=1,lwd=1)
  points(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),"year"],gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),i+3],col=cols[i],pch=4+i,lwd=1.2)
}
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],points(year,data,pch=17,col="black",lwd=4))
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],points(year,predicted,pch=1,col="black",lwd=2))
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],lines(year,data,col="black",lwd=1,lty=1))
with(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),],lines(year,predicted,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Average Annual GDP Growth Rate",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "bottomleft",inset=0,cex=0.5,
       legend = c("Data","Predicted",
                  expression(paste("Only Demand Effects")),
                  expression(paste("Only Markups")),
                  expression(paste("Only Technical Change"))
                  #   expression(paste(mu,"=1",sep="")),
                  #   expression(paste(g[c],"=",g[h],"=0")),
                  #   expression(paste(g[z],"=",g[zeta],"=",g[n],"=0")) #,
                  # expression(paste(g[c],"=",g[h],"=0",",",mu,"=1")),
                  # expression(paste(g[c],"=",g[h],"=",g[z],"=",g[zeta],"=",g[n],"=0")),
                  # expression(paste(g[z],"=",g[zeta],"=",g[n],"=0",",",mu,"=1"))
                  # expression(paste(g[c],",",g[h],"=0")),
                  # expression(paste(g[c],",",g[h],",",g[z],"=0")),
                  # expression(paste(g[c],"=0")),
                  # expression(paste(g[h],"=0")),
                  # expression(paste(g[z],"=0")),
                  # expression(paste(g[n],"=0")),
                  # expression(paste(g[zeta],"=0")),
                  # expression(paste(g[z],",",g[zeta],"=0")),
       ),
       pch = c(17,1,5:7),
       #  pch = c(17,1,rep(NA,3)),
             lty=c(1,3,rep(1,3)),
       col = c("black","black",cols),
       lwd=1)
dev.off()
}

######## make the growth table, average annual 5-year growth rates deviation from the predictive baseline #########
growth_years = seq(1960,2040,20)
output_matrix = matrix(NA,ncol=(2+length(growth_years)),nrow=9)
# data relative to baseline (everything is X - baseline)
output_matrix[1,3:7] = growth_years
output_matrix[2,3:7] = gy_counters_normalized[gy_counters_normalized$year %in% growth_years,"data"]
# predictions 
output_matrix[3,3:7] = gy_counters_normalized[gy_counters_normalized$year %in% growth_years,"predicted"]
# counterfactuals
for(i in 1:6){
  output_matrix[i+3,3:7] = gy_counters_normalized[gy_counters_normalized$year %in% growth_years,i] #gy_counters_normalized_all[gy_counters_normalized_all$year %in% growth_years,i] - gy_sim_all[gy_sim_all$year %in% growth_years,"gy_sim"]
}
#output_matrix[2:nrow(output_matrix),] = output_matrix[2:nrow(output_matrix),] * 100

# write the matrix to disk as latex file
print("Baseline predicted avg. annual GDP growth by 2040 is:")
print(gy_sim_all[gy_sim_all$year == 2040,"gy_sim"])

# stargazer
#library(stargazer)
#print("Growth Counterfactual Table")
#stargazer(output_matrix,type="latex",summary=FALSE)
#capture.output(bla,file="~/Dropbox/US_HEALTH_MACRO/Data_Work/GE_Calibration_Constant_Alpha_c/growth_table.tex",row.names=FALSE)

######## make the counterfactual table; change in variables over time #########
output_matrix = matrix(NA,ncol=7,nrow=8)
# data relative to baseline (everything is X - baseline)
### p_t ### col = 3
output_matrix[1,3] = (p_counters_normalized$data[14]/p_counters_normalized$data[3] - 1)*100
output_matrix[2,3] = (p_counters_normalized$predicted[14]/p_counters_normalized$predicted[3] - 1)*100
### negative first (no demand, no markups, no tech change) ###
output_matrix[3:5,3] = unname(t((p_counters_normalized[14,1:3]/p_counters_normalized[3,1:3] - 1)*100))
### positive (only demand, only markups, only tech change) ###
output_matrix[6:8,3] = unname(t((p_counters_normalized[14,4:6]/p_counters_normalized[3,4:6] - 1)*100))
### LE_t ### col = 4
output_matrix[1,4] = -(le_counters_normalized$data[14]-le_counters_normalized$predicted[14]) # - 1)*100
output_matrix[2,4] = -(le_counters_normalized$predicted[14]-le_counters_normalized$predicted[14]) # - 1)*100
### negative first (no demand, no markups, no tech change) ###
output_matrix[3:5,4] = -unname(t((le_counters_normalized[14,1:3]-le_counters_normalized$predicted[14]))) # - 1)*100))
### positive (only demand, only markups, only tech change) ###
output_matrix[6:8,4] = -unname(t((le_counters_normalized[14,4:6]-le_counters_normalized$predicted[14]))) # - 1)*100))
### HE_t ### col = 5
output_matrix[1,5] = (he_counters_normalized$data[14]-he_counters_normalized$data[3]) # - 1)*100
output_matrix[2,5] = (he_counters_normalized$predicted[14]-he_counters_normalized$predicted[3]) # - 1)*100
### negative first (no demand, no markups, no tech change) ###
output_matrix[3:5,5] = unname(t((he_counters_normalized[14,1:3]-he_counters_normalized[3,1:3]))) # - 1)*100))
### positive (only demand, only markups, only tech change) ###
output_matrix[6:8,5] = unname(t((he_counters_normalized[14,4:6]-he_counters_normalized[3,4:6]))) # - 1)*100))
### KH_t ### col = 6
output_matrix[1,6] = (k_share_normalized$data[14]-k_share_normalized$data[3]) #- 1)*100
output_matrix[2,6] = (k_share_normalized$predicted[14]-k_share_normalized$predicted[3]) #- 1)*100
### negative first (no demand, no markups, no tech change) ###
output_matrix[3:5,6] = unname(t((k_share_normalized[14,1:3]-k_share_normalized[3,1:3]))) #- 1)*100))
### positive (only demand, only markups, only tech change) ###
output_matrix[6:8,6] = unname(t((k_share_normalized[14,4:6]-k_share_normalized[3,4:6]))) #- 1)*100))
### LH_t ### col = 7
output_matrix[1,7] = (l_share_normalized$data[14]-l_share_normalized$data[3]) #- 1)*100
output_matrix[2,7] = (l_share_normalized$predicted[14]-l_share_normalized$predicted[3]) #- 1)*100
### negative first (no demand, no markups, no tech change) ###
output_matrix[3:5,7] = unname(t((l_share_normalized[14,1:3]-l_share_normalized[3,1:3]))) #- 1)*100))
### positive (only demand, only markups, only tech change) ###
output_matrix[6:8,7] = unname(t((l_share_normalized[14,4:6]-l_share_normalized[3,4:6]))) #- 1)*100))

#print("Moment Counterfactual Table")
#stargazer(output_matrix,type="latex",summary=FALSE)


# # print growth in health services prices from 1960 to 2015 under data and counterfactuals
# print("Growth in health services prices from 1960 to 2015 (total growth, not avg. annual):")
# print("Data")
# print(p_counters_normalized$data[14]/p_counters_normalized$data[3] - 1)
# print("Predicted")
# print(p_counters_normalized$predicted[14]/p_counters_normalized$predicted[3]- 1)
# print("Fixed mu -- no market concentration")
# print(p_counters_normalized$V1[14]/p_counters_normalized$V1[3]- 1)
# print("Fixed A_c, A_h, Z -- no change to TFP, relative TFP, or aggregate health productivity")
# print(p_counters_normalized$V2[14]/p_counters_normalized$V2[3]- 1)
# print("Fixed A_c, A_h, mu -- no supply-side changes, only aging")
# print(p_counters_normalized$V3[14]/p_counters_normalized$V3[3]- 1)
# print("Fixed A_c -- no TFP growth for consumption")
# print(p_counters_normalized$V4[14]/p_counters_normalized$V4[3]- 1)
# print("Fixed A_h -- no TFP growth for health services")
# print(p_counters_normalized$V5[14]/p_counters_normalized$V5[3]- 1)
# print("Fixed Z -- no TFP growth for productivity of health care at generating healthy outcomes")
# print(p_counters_normalized$V6[14]/p_counters_normalized$V6[3]- 1)
# print("Fixed pop0 -- no initial population growth")
# print(p_counters_normalized$V7[14]/p_counters_normalized$V7[3]- 1)
# print("Fixed zeta -- no change to age-specific health productivity")
# print(p_counters_normalized$V8[14]/p_counters_normalized$V8[3]- 1)
# print("Fixed zeta, Z -- no change to any health productivity")
# print(p_counters_normalized$V9[14]/p_counters_normalized$V9[3]- 1)
# print("Fixed zeta, Z, pop0 -- no change to any health productivity or population")
# print(p_counters_normalized$V10[14]/p_counters_normalized$V10[3]- 1)
# print("Fixed A_c and A_h -- no change to TFP or sectoral relative TFP")
# print(p_counters_normalized$V11[14]/p_counters_normalized$V11[3]- 1)

# # stargazer
# library(stargazer)
# print("Growth Counterfactual Table")
# stargazer(output_matrix,type="latex",summary=FALSE)
#capture.output(bla,file="~/Dropbox/US_HEALTH_MACRO/Data_Work/GE_Calibration_Constant_Alpha_c/growth_table.tex",row.names=FALSE)


############################# export counterfactual parameters to table #######################
# Load the xtable package
#library(xtable)
# 
# # Create a data frame with the table content
# data <- data.frame(
#   Counterfactual = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)", "(13)"),
#   `Counterfactual` = c("Data", "Predicted", "$\\mu_t = 1$, $\\forall t$", "$g_{A_c} = g_{A_h} = 0$, $\\mu_t = 1$, $\\forall t$", "$g_{A_c} = g_{A_h} = 0$", "$g_{A_c} = g_{A_h} = g_z = 0$", "$g_{A_c} = 0$", "$g_{A_h} = 0$", "$g_z = 0$", "$g_N = 0$", "$g_{\\zeta_{j}} = 0$", "$g_{\\zeta_{j}} = g_z = 0$", "$g_{\\zeta_{j}} = g_z = g_N = 0$"),
#   `$p_t$ Growth$^*$` = rep("", 13),
#   `LE Lost$^\\%$` = rep("", 13),
#   `Health Share$^\\$$` = rep("", 13),
#   `$K_{ht} / K_t$$^\\$$` = rep("", 13),
#   `$L_{ht} / L_t$$^\\$$` = rep("", 13)
# )
# 
# # Print the table using xtable
# table_latex <- xtable(data, caption = "Counterfactual Simulations Relative to Model Predictions", label = "counterfactual_table")
# print(table_latex, caption.placement = "top", size = "\\small", floating = FALSE)
# 
# # Save the LaTeX table to a file
# saveToFile <- function(texTable, filename) {
#   cat("\\begin{table}[!htbp] \\centering\n", file = filename)
#   cat(texTable, file = filename, append = TRUE)
#   cat("\\end{table}", file = filename, append = TRUE)
# }
# 
# # Specify the file name
# filename <- "counterfactual_table.tex"
# 
# # Save the table to a LaTeX file
# saveToFile(table_latex, filename)

print("#########################################################################")
print("#########################################################################")
print("#########################################################################")
print("###### END Calibration R script for GE_Calibration_Constant_Alpha_c ...##")
print("#########################################################################")
print("#########################################################################")
print("#########################################################################")
print("#########################################################################")


closeAllConnections()
