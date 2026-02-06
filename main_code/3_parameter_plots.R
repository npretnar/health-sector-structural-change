rm(list=ls())

# LIBRARIES #
require("xtable")

#### plot the parameters (ex-ante calibrated and estimated) for all models ####

# figure/table results output directory
results_dir = "results"

# analysis directory
analysis_dir = "data/analysis"

# prints in-line numbers and data references
sink(paste0(results_dir,"/3_parameters_SCRIPT.txt")) # prints in-line data into a txt file

##### SUPPLEMENTAL APPENDIX PLOTS #####
# phi and theta plots
phi = c(6.7987,6.1929,6.1929,5.3966,5.3966,
        4.3935,4.3935,3.236,3.236,
        2.2828,2.2828,1.416,1.416,
        0.6157,0.6157,0.6157)
theta = c(0.15703,0.19885,0.19885,0.23727,
          0.23727,0.24746,0.24746,0.17051,0.17051,
          0.12512,0.12512,0.10218,
          0.10218,0.041686,0.041686,0.041686)
# phi plot
ymin = min(phi,na.rm=TRUE)
ymax = max(phi,na.rm=TRUE)
ages = seq(22.5,97.5,5)
png(filename=paste(results_dir,"/figureD1a.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
plot(ages,phi,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n")
grid()
lines(ages,phi,col="black",lty=1,lwd=2)
axis(1, at=seq(20,100,10),las=1)
title(ylab=expression(phi[j]),cex.lab=1)
dev.off()
# theta plot
ymin = min(theta,na.rm=TRUE)
ymax = max(theta,na.rm=TRUE)
ages = seq(22.5,97.5,5)
png(filename=paste(results_dir,"/figureD1b.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
plot(ages,theta,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n")
grid()
lines(ages,theta,col="black",lty=1,lwd=2)
axis(1, at=seq(20,100,10),las=1)
title(ylab=expression(theta[j]),cex.lab=1)
dev.off()

######################## zetas to get growth rates #######################
# growth rates from Hall and Jones 2007
g_zeta = c(0.45431/0.41618-1,0.49103/0.45372-1,0.49103/0.45372-1,
           0.44683/0.40857-1,0.44683/0.40857-1,
           0.40969/0.37102-1,0.40969/0.37102-1,
           0.33355/0.29524-1,0.33355/0.29524-1,
           0.28362/0.24656-1,0.28362/0.24656-1,
           0.25895/0.22285-1,0.25895/0.22285-1,
           0.25895/0.22285-1,0.25895/0.22285-1,0.25895/0.22285-1)
# g-zeta plot
ymin = min(g_zeta,na.rm=TRUE)
ymax = max(g_zeta,na.rm=TRUE)
ages = seq(22.5,97.5,5)
png(filename=paste(results_dir,"/figureD1c.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
plot(ages,g_zeta,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n")
grid()
lines(ages,g_zeta,col="black",lty=1,lwd=2)
axis(1, at=seq(20,100,10),las=1)
title(ylab=expression(g[zeta]),cex.lab=1)
dev.off()

######################## etas ########################
# Hansen 1993
JJ = 16
JR = 10
eff = vector(length=16)
eff[1] = (0.78 + 0.69) / 2.00  
eff[2] = (1.14 + 0.89) / 2.00
eff[3] = (1.14 + 0.89) / 2.00
eff[4] = (1.37 + 0.90) / 2.00
eff[5] = (1.37 + 0.90) / 2.00
eff[6] = (1.39 + 0.87) / 2.00
eff[7] = (1.39 + 0.87) / 2.00
eff[8] = (1.33 + 0.84) / 2.00
eff[9] = (1.33 + 0.84) / 2.00
eff[JR:JJ] = 0.0

# eta plot
ymin = min(eff,na.rm=TRUE)
ymax = max(eff,na.rm=TRUE)
ages = seq(22.5,97.5,5)
png(filename=paste(results_dir,"/figureD1d.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
plot(ages,eff,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n")
grid()
lines(ages,eff,col="black",lty=1,lwd=2)
axis(1, at=seq(20,100,10),las=1)
title(ylab=expression(eta[j]),cex.lab=1)
dev.off()

######################## combined exp(log(1+g_z) + theta_j log(1+g_zeta)) ########################
list_dirs = c("GE_Calibration_Mu_Growth_Past_2015","GE_Calibration_Constant_Alpha_c","GE_Calibration_Variable_Alpha_c",
              "GE_Calibration_HS_Markups_Full_Calibration","GE_Calibration_No_Mu_Growth")
parms_min <- list()
loss_D <- list()
gzZ0_D <- list()
g_z <- list()
Z_0 <- list()
full_household_growth <- list()
for(i in list_dirs){
  parms_min[[i]] <- read.table(paste0(analysis_dir,"/",i,"/parms_min.txt"), quote="\"", comment.char="")
  gzZ0_D[[i]] <- read.table(paste0(analysis_dir,"/",i,"/gzZ0_D.txt"), quote="\"", comment.char="")
  loss_D[[i]] <- read.table(paste0(analysis_dir,"/",i,"/loss_D.txt"), quote="\"", comment.char="")
  g_z[[i]] <- gzZ0_D[[i]]$V1
  Z_0[[i]] <- gzZ0_D[[i]]$V2
  full_household_growth[[i]] <- (exp(log(1+g_z[[i]]) + theta * log(1+g_zeta)))^(1/5) - 1
}

####### NOW READ IN ALL OF THE MU'S ##########
mu_DATA <- list()
for(i in list_dirs){
  mu_DATA[[i]] <- read.table(paste0(analysis_dir,"/",i,"/mu.txt"), quote="\"", comment.char="")
}
mu <- list()
Tend = (2050-1950+5)/5
for(i in list_dirs){
  if(i != "GE_Calibration_Mu_Growth_Past_2015"){
    mu[[i]] <- c(mu_DATA[[i]]$V1[1],mu_DATA[[i]]$V1,rep(mu_DATA[[i]][nrow(mu_DATA[[i]]),1],Tend-nrow(mu_DATA[[i]])-1))
  }
}
mu[["GE_Calibration_Mu_Growth_Past_2015"]] <- 
  c(mu_DATA[["GE_Calibration_Mu_Growth_Past_2015"]]$V1[1],mu_DATA[["GE_Calibration_Mu_Growth_Past_2015"]]$V1[1:20])

########### FIGURE 6 ###########
cols = c("darkgreen","red","blue","black","purple")
pchs = c(8,1,22,6:(length(list_dirs)+2))#c(16,22,6:(length(list_dirs)+3))

# Open a PNG device (optional)
#png(paste0(results_dir,"/figure6.png"), width = 2000, height = 2000, res = 300)
pdf(paste0(results_dir,"/figure6.pdf"),width=7,height=7)
# Define a 2x2 layout with custom widths and heights
layout_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
# Set up the layout with different relative sizes
layout(layout_matrix, widths = c(1, 1), heights = c(1, 1))
# Add borders to visualize plot boundaries
par(mar = c(4, 4.3, 2, 1))  # margin setup

#### FIGURE 6a ####
######################### MU PLOT ###################
# plot
year = seq(1950,2050,5)
ymin = min(unlist(lapply(mu,min)),na.rm=TRUE)
ymax = max(unlist(lapply(mu,max)),na.rm=TRUE)
#png(filename=paste("~/Dropbox/US_HEALTH_MACRO/Draft/mu_plot.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
plot(year,mu[[i]],type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n")
grid()
j = 1
for(i in list_dirs){
  if(i == "GE_Calibration_Constant_Alpha_c"){
    lines(year,mu[[i]],col=cols[j],lty=3,lwd=1)
    points(year,mu[[i]],col=cols[j],pch=pchs[j],lwd=1.2)
  }else{
    lines(year,mu[[i]],col=cols[j],lty=1,lwd=1)
    points(year,mu[[i]],col=cols[j],pch=pchs[j],lwd=1.2)
  }
  j = j+1
}
axis(1, at=seq(1950,2050,20),las=1)
title(ylab=expression(mu[t]),
      xlab=expression(paste("(a) Calibrated ",mu[t],sep="")),cex.lab=0.9)
legend(x = "topleft",inset = 0,
       legend = c(expression(paste("1) Constant ",alpha[c],", ",tilde(g)[mu],"> 0, t > 2015")),
                  expression(paste("2) Constant ",alpha[c],", ",tilde(g)[mu],"= 0, t > 2015")),
                  expression(paste("3) Non-constant ",alpha[ct])),
                  expression(paste("4) H-S Markups")),
                  expression(paste("5) ",tilde(g)[mu],"= 0"))),
       lty=c(1,3,1,1,1), pch=c(pchs),
       col=c(cols), lwd=rep(0.8,length(list_dirs)), cex=0.6)
#dev.off()

######################### A_h PLOT ###################
Tgrowth = 30
TT = Tgrowth + 15

# read in parms by directory
# A_c (same for all models)
g_c = read.table(paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/g_c.txt"), quote="\"", comment.char="")$V1[1]
A_c0 = 1
A_c = vector(mode="numeric",length=(TT+1))
A_c[1] = A_c0
# After Tgrowth it is constant until TT
for(it in 2:Tgrowth){
  A_c[it] = A_c[it-1] * (1+g_c)
}
A_c[Tgrowth:(TT+1)] = A_c[Tgrowth]

# A_h
g_h <- list()
A_h0 <- list()
A_h <- list()
for(i in list_dirs){
  # parms_min[[i]] <- read.table(paste0("~/Dropbox/US_HEALTH_MACRO/Data_Work/",i,"/parms_min.txt"), quote="\"", comment.char="")
  g_h[[i]] = parms_min[[i]]$V1[2:5]
  A_h0[[i]] = parms_min[[i]]$V1[1]
  # A_h
  A_h[[i]] = vector(mode="numeric",length=(TT+1))
  A_h[[i]][1] = A_h0[[i]]
  for(it in 2:5){
    A_h[[i]][it] = A_h[[i]][it-1] * (1+g_h[[i]][1])
  }
  for(it in 6:7){
    A_h[[i]][it] = A_h[[i]][it-1] * (1+g_h[[i]][2]) 
  }
  for(it in 8:10){
    A_h[[i]][it] = A_h[[i]][it-1] * (1+g_h[[i]][3]) 
  }
  for(it in 11:Tgrowth){
    A_h[[i]][it] = A_h[[i]][it-1] * (1+g_h[[i]][4]) 
  }
  # After Tgrowth it is constant until TT
  A_h[[i]][Tgrowth:(TT+1)] = A_h[[i]][Tgrowth]
}

### plot ###
# levels #
holder = as.data.frame(list(
  year = seq(1950,2050,5),
  do.call(cbind,lapply(A_h,function(x){A_c[1:length(seq(1950,2050,5))]/x[1:length(seq(1950,2050,5))]})),
  #A_h_A_c = A_h[1:length(seq(1950,2050,5))] / A_c[1:length(seq(1950,2050,5))],
  A_c = A_c[1:length(seq(1950,2050,5))]
))
#holder = holder[holder$year <=2020,]
ymin = min(holder[names(holder)%in%list_dirs],na.rm=TRUE)
ymax = max(holder[names(holder)%in%list_dirs],na.rm=TRUE)
#png(filename=paste("~/Dropbox/US_HEALTH_MACRO/Draft/A_c_A_h.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
plot(holder$year,holder[,i],type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n")
grid()
j = 1
for(i in list_dirs){
  #   lines(holder$year,holder[,i],col=cols[j],lty=1,lwd=1)
  #   points(holder$year,holder[,i],col=cols[j],pch=pchs[j],lwd=1.2)
  #   j = j+1
  # }
  if(i == "GE_Calibration_Constant_Alpha_c"){
    lines(holder$year,holder[,i],col=cols[j],lty=3,lwd=1)
    points(holder$year,holder[,i],col=cols[j],pch=pchs[j],lwd=1.2)
  }else{
    lines(holder$year,holder[,i],col=cols[j],lty=1,lwd=1)
    points(holder$year,holder[,i],col=cols[j],pch=pchs[j],lwd=1.2)
  }
  j = j+1
}
axis(1, at=seq(1950,2050,20),las=1)
title(ylab=expression(paste(A[ct]," / ",A[ht],sep="")),
      xlab=expression(paste("(b) Calibrated ",A[t]," = ",A[ct],"/",A[ht],sep="")),cex.lab=0.9)
legend(x = "topleft",inset = 0,
       legend = c(expression(paste("1) Constant ",alpha[c],", ",tilde(g)[mu],"> 0, t > 2015")),
                  expression(paste("2) Constant ",alpha[c],", ",tilde(g)[mu],"= 0, t > 2015")),
                  expression(paste("3) Non-constant ",alpha[ct])),
                  expression(paste("4) H-S Markups")),
                  expression(paste("5) ",tilde(g)[mu],"= 0"))),
       lty=c(1,3,1,1,1), pch=c(pchs),
       col=c(cols), lwd=rep(0.8,length(list_dirs)), cex=0.6)
#dev.off()

### FIGURE 6C ###
### (plot all of these together) full growth = (exp(log(1+g_z[[i]]) + theta * log(1+g_zeta)))^(1/5) - 1 ###
# combined growth rate
ymin = min(unlist(lapply(full_household_growth,min)),na.rm=TRUE)
ymax = max(unlist(lapply(full_household_growth,max)),na.rm=TRUE)
ages = seq(22.5,97.5,5)
#png(filename=paste("~/Dropbox/US_HEALTH_MACRO/Draft/g_combined_age.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
plot(ages,full_household_growth[[i]],type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n")
grid()
j = 1
for(i in list_dirs){
#   lines(ages,full_household_growth[[i]],col=cols[j],lty=1,lwd=1)
#   points(ages,full_household_growth[[i]],col=cols[j],pch=pchs[j],lwd=1.2)
#   j = j+1
# }
  if(i == "GE_Calibration_Constant_Alpha_c"){
    lines(ages,full_household_growth[[i]],col=cols[j],lty=3,lwd=1)
    points(ages,full_household_growth[[i]],col=cols[j],pch=pchs[j],lwd=1.2)
  }else{
    lines(ages,full_household_growth[[i]],col=cols[j],lty=1,lwd=1)
    points(ages,full_household_growth[[i]],col=cols[j],pch=pchs[j],lwd=1.2)
  }
  j = j+1
}
axis(1, at=seq(20,100,10),las=1)
title(ylab=expression(paste("(1+",g[z],")(1+",g[zeta[j]],")",)^theta[j],sep=""),
      xlab=expression(paste("(c) Calibrated Age-specific ",z[t],zeta[jt]^theta[j],sep="")),cex.lab=0.9)
legend(x = "bottomleft",inset = 0,
       legend = c(expression(paste("1) Constant ",alpha[c],", ",tilde(g)[mu],"> 0, t > 2015")),
                  expression(paste("2) Constant ",alpha[c],", ",tilde(g)[mu],"= 0, t > 2015")),
                  expression(paste("3) Non-constant ",alpha[ct])),
                  expression(paste("4) H-S Markups")),
                  expression(paste("5) ",tilde(g)[mu],"= 0"))),
       lty=c(1,3,1,1,1), pch=c(pchs),
       col=c(cols), lwd=rep(0.8,length(list_dirs)), cex=0.6)
dev.off()

################################## TABLE 2: INTERNAL PARAMETERS #############################
# ############################### internal parameters to table ##############################
# rounding function for tabular outputs
# round to 3 and make characters
format_round_3 <- function(x) {
  format(round(x, 3), nsmall = 3)
}

# loss function
parm_table <- matrix(NA,nrow=9,ncol=5)
j=1
for(i in list_dirs){
  parm_table[1:6,j] = parms_min[[i]]$V1
  parm_table[7,j] = gzZ0_D[[i]]$V1
  parm_table[8,j] = gzZ0_D[[i]]$V2
  parm_table[9,j] = loss_D[[i]]$V1
  j = j+1
}
parm_table <- format_round_3(parm_table)

################### output Table 2 using xtable ####################
# Row names with LaTeX math
parm_names <- c(
  "$A_{h,1}$",
  "$g_{A_h,1950-1970}$",
  "$g_{A_h,1975-1980}$",
  "$g_{A_h,1985-1995}$",
  "$g_{A_h,>1995}$",
  "$\\xi$",
  "$g_z$",
  "$z_1$",
  "RMSE"
)
parm_table <- cbind(parm_names,parm_table)
# Description labels
descriptions <- c(
  "Health-sector TFP",
  "5-yr Growth 1950-1970",
  "5-yr Growth 1975-1980",
  "5-yr Growth 1985-1995",
  "5-yr Growth After 1995",
  "Utility Scalar",
  "5-yr Growth $z_t$",
  "Health-demand Efficiency",
  "Loss Function"
)
parm_table <- cbind(parm_table,descriptions)

colnames(parm_table) <- c("Model",paste0("(",seq(1,5,1),")")," ")
# Create xtable
xtab <- xtable(parm_table,
               caption = "Internally Calibrated Parameters (All Models)",
               label = "internal_calibration")
# Build add.to.row list: replace header with custom lines, add hline after row 1
add_lines <- list()
add_lines$pos <- list(8)
add_lines$command <- c(
  "\\hline \n"
)

# Capture the LaTeX output as text
latex_lines <- capture.output(
  print(xtab,
        include.rownames = FALSE,
        include.colnames = TRUE,
        sanitize.text.function = identity,
        add.to.row = add_lines,
        caption.placement = "top",
        floating = TRUE)
)


# Replace the default tabular format with your custom format
# replace line 8 for double line spacing
latex_lines[6] <- "\\label{internal_calibration} \\small"    
latex_lines[8] <- " \\hline \\\\[-1.8ex]"
latex_lines[10] <- " \\hline \\\\[-1.8ex]"
latex_lines[19] <- " \\hline \\\\[-1.8ex]"
latex_lines[21] <- "  \\\\[-1.8ex] \\hline"

latex_lines <- sub(
  pattern = "^\\\\begin\\{tabular\\}\\{.*\\}$",
  replacement = " \\\\hspace*{3pt}\\\\makebox[\\\\linewidth][c]{ \\\\begin{tabular}{@{\\\\extracolsep{8pt}} lcccccc} \\\\\\\\[-5ex] \\\\hline",
  latex_lines
)
latex_lines <- sub(
  pattern = "^\\\\end\\{tabular\\}",
  replacement = " \\\\hline \\\\\\\\[-1.8ex] \\\\end{tabular}}",
  latex_lines
)

# Write to .tex file
writeLines(latex_lines, paste0(results_dir,"/table2.tex"))

#print("Internal Parameter Table")
#stargazer(parm_table,type="latex",summary=FALSE)

################################ health share and price decomposition #######################
######################################## from predicted models ##############################

list_preds = c("GE_Calibration_Constant_Alpha_c",
              "GE_Calibration_Mu_Growth_Past_2015")
n_pred = 5
predicts_2050 <- list()
for(i in list_preds){
  predicts_2050[[i]] <- list()
  for(j in 1:5){
    predicts_2050[[i]][[j]] <- read.table(paste0(analysis_dir,"/","/",i,"/pred_moments",j,".txt"), quote="\"", comment.char="")
  }
}


## 2 plots with both models and data but push them out to 2050 ##
# 1) prices; 2) health shares of agg. expenditure

########################### PRICE PREDICTIONS ##################################
# read in q-data (1960 = 1)
p_data <- t(read.table(paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/data_moments1.txt"), quote="\"", comment.char=""))

# put the predicts for prices and he_share in intermediate dataframes, note that predicts start at 1950 and data at 1960
# j = c(1,2)
holder_p = as.data.frame(list(
  p_baseline = t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[2]]),
  p_markups = t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[2]])
))
holder_p$data = NA
for(i in 1:12){
  holder_p$data[i] = p_data[i]
}

########### PREDICTION PLOTS FOR SUPPLEMENTAL APPENDICES ##########
# mins and maxes
ymin = min(holder_p,na.rm=TRUE) 
ymax = max(holder_p,na.rm=TRUE) 
holder_p$year = seq(1960,2055,5)
holder_p = holder_p[holder_p$year <=2050,]
png(filename=paste(results_dir,"/figureD15a.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(holder_p,plot(year,p_baseline,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
#for(i in 1:11){
#  lines(p_counters_normalized$year,p_counters_normalized[,i],col=cols[i],lty=1+i,lwd=2)
#}
with(holder_p,points(year,data,pch=17,col="black",lwd=4))
with(holder_p,points(year,p_markups,pch=pchs[1],col=cols[1],lwd=1.2))
with(holder_p,points(year,p_baseline,pch=pchs[2],col=cols[2],lwd=1.2))
with(holder_p,lines(year,data,col="black",lwd=1,lty=1))
with(holder_p,lines(year,p_markups,col=cols[1],lwd=1,lty=1))
with(holder_p,lines(year,p_baseline,col=cols[2],lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Relative Price of Health Care",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,
       legend = c("Data",
                  expression(paste("1) Constant ",alpha[c],", ",tilde(g)[mu],"> 0, t > 2015")),
                  expression(paste("2) Constant ",alpha[c],", ",tilde(g)[mu],"= 0, t > 2015"))),
       pch = c(17,pchs[1:2]), # ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
       lty=c(1,1,3),lwd=rep(0.8,3), #2,4,12,3,5,6,7,8,9,10,11,12),
       col = c("black",cols[1:2]),cex=0.6) #,"blue","darkorange","gold2","red","darkgreen","purple","goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon"),lwd=1.2,cex=0.6)
dev.off()

########################### HE SHARE PREDICTIONS ###############################
he_share_data <- t(read.table(paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/data_moments3.txt"), quote="\"", comment.char=""))
# retransform
he_share_data = 1/(1+exp(he_share_data))

# put the predicts for prices and he_share in intermediate dataframes, note that predicts start at 1950 and data at 1960
# j = c(1,2)
holder_h_sh = as.data.frame(list(
  h_sh_baseline = t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[1]]),
  h_sh_markups = t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[1]])
))
holder_h_sh$data = NA
for(i in 1:12){
  holder_h_sh$data[i] = he_share_data[i]
}

# mins and maxes
ymin = min(holder_h_sh,na.rm=TRUE) 
ymax = max(holder_h_sh,na.rm=TRUE) 
holder_h_sh$year = seq(1960,2055,5)
holder_h_sh = holder_h_sh[holder_h_sh$year <=2050,]
png(filename=paste(results_dir,"/figureD15b.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
with(holder_h_sh,plot(year,h_sh_baseline,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
#for(i in 1:11){
#  lines(p_counters_normalized$year,p_counters_normalized[,i],col=cols[i],lty=1+i,lwd=2)
#}
with(holder_h_sh,points(year,data,pch=17,col="black",lwd=4))
with(holder_h_sh,points(year,h_sh_markups,pch=pchs[1],col=cols[1],lwd=1.2))
with(holder_h_sh,points(year,h_sh_baseline,pch=pchs[2],col=cols[2],lwd=1.2))
with(holder_h_sh,lines(year,data,col="black",lwd=1,lty=1))
with(holder_h_sh,lines(year,h_sh_markups,col=cols[1],lwd=1,lty=1))
with(holder_h_sh,lines(year,h_sh_baseline,col=cols[2],lwd=1,lty=3))
axis(1, at=seq(1960,2050,10),las=1)
title(ylab="Health Share of Aggregate Expenditure",cex.lab=1)
# place legend in order of constellations of counterfactuals
legend(x= "topleft",inset=0,
       legend = c("Data",
                  expression(paste("1) Constant ",alpha[c],", ",tilde(g)[mu],"> 0, t > 2015")),
                  expression(paste("2) Constant ",alpha[c],", ",tilde(g)[mu],"= 0, t > 2015"))),
       pch = c(17,pchs[1:2]), # ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
       lty=c(1,1,3),lwd=rep(0.8,3), #2,4,12,3,5,6,7,8,9,10,11,12),
       col = c("black",cols[1:2]),cex=0.6) #,"blue","darkorange","gold2","red","darkgreen","purple","goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon"),lwd=1.2,cex=0.6)
dev.off()



##################### DECOMPOSITION: DRIVERS OF FUTURE SHARE GROWTH #################
# g_sigma = g_p + g_H - g_X
# X = p * H / sigma
# compute X
holder_x = as.data.frame(list(
  x_baseline = t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[3]]) * t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[4]]) / t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[1]]),
  x_markups = t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[3]]) * t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[4]]) / t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[1]])
))
# compute 5-yr growth rate decompisition by model
holder_growth_decomp_baseline = as.data.frame(list(
  g_x = -diff(log(holder_x$x_baseline)),
  g_p = diff(log(t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[3]]))),
  g_H = diff(log(t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[4]]))),
  g_sigma = diff(log(t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[1]])))
))
holder_growth_decomp_markups = as.data.frame(list(
  g_x = -diff(log(holder_x$x_markups)),
  g_p = diff(log(t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[3]]))),
  g_H = diff(log(t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[4]]))),
  g_sigma = diff(log(t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[1]])))
))

# plot the raw decompositions in separate plots
ymin = min(min(holder_growth_decomp_baseline),min(holder_growth_decomp_markups))
ymax = max(max(holder_growth_decomp_baseline),max(holder_growth_decomp_markups))

# add years
holder_growth_decomp_baseline$year = seq(1965,2055,5)
holder_growth_decomp_markups$year = seq(1965,2055,5)

# plotting vars
cols = c("black","red","blue","darkgreen")
pchs = c(1,8,22,6)
ltys = c(1,2,3,4)
var_names_ordered = c("g_sigma","g_p","g_H","g_x")

######################## TOTAL HEALTH EXPENDITURE DECOMPOSITION ##################
# normalize p*H, p, and H to 1 in 1960 and then plot ...
holder_decomp_baseline = as.data.frame(list(
  x =  t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[3]]) *  t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[4]]) / (t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[3]]) *  t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[4]]))[1],
  p = t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[3]]) / t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[3]])[1],
  H = t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[4]]) /  t(predicts_2050[["GE_Calibration_Constant_Alpha_c"]][[4]])[1]
))
holder_decomp_markups = as.data.frame(list(
  x =  t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[3]]) *  t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[4]]) / (t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[3]]) *  t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[4]]))[1],
  p = t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[3]]) / t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[3]])[1],
  H = t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[4]]) /  t(predicts_2050[["GE_Calibration_Mu_Growth_Past_2015"]][[4]])[1]
))


# add years
holder_decomp_baseline$year = seq(1960,2055,5)
holder_decomp_markups$year = seq(1960,2055,5)
holder_decomp_baseline <- holder_decomp_baseline[holder_decomp_baseline$year <=2050,]
holder_decomp_markups <- holder_decomp_markups[holder_decomp_markups$year <=2050,]

# plotting vars
cols = c("black","red","blue")
pchs = c(1,8,22)
ltys = c(1,2,3)
var_names_ordered = c("x","p","H")

# plot the raw decompositions in separate plots
ymin = min(min(holder_decomp_baseline[,var_names_ordered]),min(holder_decomp_markups[,var_names_ordered]))
ymax = max(max(holder_decomp_baseline[,var_names_ordered]),max(holder_decomp_markups[,var_names_ordered]))

# constant alpha c
png(filename=paste(results_dir,"/figureD15d.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
plot(holder_decomp_baseline$year,holder_decomp_baseline$x,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n")
grid()
for(j in 1:3){
  lines(holder_decomp_baseline$year,holder_decomp_baseline[,var_names_ordered[j]],col=cols[j],lty=ltys[j],lwd=1.2)
  points(holder_decomp_baseline$year,holder_decomp_baseline[,var_names_ordered[j]],col=cols[j],pch=pchs[j],lwd=1.2)
}
abline(h=1,lwd=1.5)
axis(1, at=seq(1960,2050,20),las=1)
title(ylab=expression(paste("Predicted Series Normalized",sep="")),cex.lab=1)
legend(x = "topleft",inset = 0,
       legend = c(expression(paste(p,"*",H,sep="")),
                  expression(paste(p,sep="")),
                  expression(paste(H,sep=""))),
       lty=ltys, pch=pchs,
       col=cols, lwd=rep(0.8,length(var_names_ordered)), cex=0.6)
dev.off()

# markups
png(filename=paste(results_dir,"/figureD15c.png",sep=""),width=5,height=4,units="in",res=600)
par(mar = c(2,5,2,2))
plot(holder_decomp_markups$year,holder_decomp_markups$x,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n")
grid()
for(j in 1:3){
  lines(holder_decomp_markups$year,holder_decomp_markups[,var_names_ordered[j]],col=cols[j],lty=ltys[j],lwd=1.2)
  points(holder_decomp_markups$year,holder_decomp_markups[,var_names_ordered[j]],col=cols[j],pch=pchs[j],lwd=1.2)
}
abline(h=1,lwd=1.5)
axis(1, at=seq(1960,2050,20),las=1)
title(ylab=expression(paste("Predicted Series Normalized",sep="")),cex.lab=1)
legend(x = "topleft",inset = 0,
       legend = c(expression(paste(p,"*",H,sep="")),
                  expression(paste(p,sep="")),
                  expression(paste(H,sep=""))),
       lty=ltys, pch=pchs,
       col=cols, lwd=rep(0.8,length(var_names_ordered)), cex=0.6)
dev.off()

####################### PRINT OUT DRIVERS RESULTS ##############################
print("Predicted future health expenditure increase (percent) from 2020-2050:")
print("Markups growing at avg. rate from 1955-2015:")
print(paste0((holder_decomp_markups[holder_decomp_markups$year == 2050,"x"] / holder_decomp_markups[holder_decomp_markups$year == 2020,"x"]-1)*100,"%"))
print("No markup growth after 2015:")
print(paste0((holder_decomp_baseline[holder_decomp_baseline$year == 2050,"x"] / holder_decomp_baseline[holder_decomp_baseline$year == 2020,"x"]-1)*100,"%"))

print("Predicted future health expenditure share increase (percentage points) from 2020-2050:")
print("Markups growing at avg. rate from 1955-2015:")
print(paste0((holder_h_sh[holder_h_sh$year == 2050,"h_sh_markups"] - holder_h_sh[holder_h_sh$year == 2020,"h_sh_markups"])*100))
print("No markup growth after 2015:")
print(paste0((holder_h_sh[holder_h_sh$year == 2050,"h_sh_baseline"] - holder_h_sh[holder_h_sh$year == 2020,"h_sh_baseline"])*100))

print("Decomposing increase to total health expenditure by price and quantity 2020-2050:")
print("PRICES % INCREASE:")
print("Markups growing at avg. rate from 1955-2015:")
print(paste0((holder_decomp_markups[holder_decomp_markups$year == 2050,"p"] / holder_decomp_markups[holder_decomp_markups$year == 2020,"p"]-1)*100,"%"))
print("No markup growth after 2015:")
print(paste0((holder_decomp_baseline[holder_decomp_baseline$year == 2050,"p"] / holder_decomp_baseline[holder_decomp_baseline$year == 2020,"p"]-1)*100,"%"))
print("QUANTITIES % INCREASE:")
print("Markups growing at avg. rate from 1955-2015:")
print(paste0((holder_decomp_markups[holder_decomp_markups$year == 2050,"H"] / holder_decomp_markups[holder_decomp_markups$year == 2020,"H"]-1)*100,"%"))
print("No markup growth after 2015:")
print(paste0((holder_decomp_baseline[holder_decomp_baseline$year == 2050,"H"] / holder_decomp_baseline[holder_decomp_baseline$year == 2020,"H"]-1)*100,"%"))


# end #
closeAllConnections()
