rm(list=ls())

library(xtable)

# figure/table results output directory
results_dir = "../../../results"

# analysis directory
#analysis_dir = "data/analysis"

# prints in-line numbers and data references
sink(paste0(results_dir,"/2_counterfactual_analyses_SCRIPT_GE_Calibration_Mu_Growth_Past_2015.txt")) # prints in-line data into a txt file

print("#########################################################################")
print("#########################################################################")
print("#########################################################################")
print("Now running Calibration R script for GE_Calibration_Mu_Growth_Past_2015 ... ")
print("#########################################################################")
print("#########################################################################")
print("#########################################################################")
print("#########################################################################")

plot_counters = TRUE

# parameters
parms_min <- read.table("parms_min.txt", quote="\"", comment.char="")

### MU ### 
# mu_DATA <- read.table("mu.txt", quote="\"", comment.char="")
# # no more growth in mark-ups
# Tend = (2050-1950+5)/5
# mu = c(mu_DATA$V1[1],mu_DATA$V1,rep(mu_DATA[nrow(mu_DATA),1],Tend-nrow(mu_DATA)-1))
# print("Health Sector Relative Markups")
# print("1955")
# print(mu[2])
# print("2010")
# print(mu[13])

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

Tend = (2050-1950+5)/5

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
p_counters_normalized$year = seq(1950,2050,5)


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
le_counters_normalized = le_counters_normalized[1:Tend,]
le_counters_normalized$data = NA
le_counters_normalized$predicted = NA
for(i in 1:12){
  le_counters_normalized$data[i+2] = le_data[i]
  le_counters_normalized$predicted[i+2] = le_predicted[i]
}
le_counters_normalized$year = seq(1950,2050,5)


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
he_counters_normalized$year = seq(1950,2050,5)




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
k_share_normalized$year = seq(1950,2050,5)

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
l_share_normalized$year = seq(1950,2050,5)

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

######## make the growth table, average annual 5-year growth rates deviation from the predictive baseline #########
growth_years = seq(1960,2040,20)
# output_matrix = matrix(NA,ncol=(2+length(growth_years)),nrow=9)
# # data relative to baseline (everything is X - baseline)
# output_matrix[1,3:7] = growth_years
# output_matrix[2,3:7] = gy_counters_normalized[gy_counters_normalized$year %in% growth_years,"data"]
# # predictions 
# output_matrix[3,3:7] = gy_counters_normalized[gy_counters_normalized$year %in% growth_years,"predicted"]
# # counterfactuals
# for(i in 1:6){
#   output_matrix[i+3,3:7] = gy_counters_normalized[gy_counters_normalized$year %in% growth_years,i] #gy_counters_normalized_all[gy_counters_normalized_all$year %in% growth_years,i] - gy_sim_all[gy_sim_all$year %in% growth_years,"gy_sim"]
# }
#output_matrix[2:nrow(output_matrix),] = output_matrix[2:nrow(output_matrix),] * 100

# write the matrix to disk as latex file
print("Baseline predicted avg. annual GDP growth by 2040 is:")
print(gy_sim_all[gy_sim_all$year == 2040,"gy_sim"])


######################## TABLE 3: COUNTERFACTUALS ##############################
############################# export counterfactual parameters to table #######################
# rounding function for tabular outputs
# round to 3 and make characters
format_round_3 <- function(x) {
  format(round(x, 3), nsmall = 3)
}
#### table body contents ####
# col (1)
column1 = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)")
# counterfactual names (col 2)
counterfactual_names = c("Data", "Predicted Baseline", 
  "$g_{\\zeta_j}=g_z=g_N=0$","$\\mu_t = \\mu_1$, $\\forall t$", "$g_{A_c} = g_{A_h} = 0$",
  "$g_{A_c} = g_{A_h} = 0$, $\\mu_t = \\mu_1$, $\\forall t$", "$g_{A_c} = g_{A_h} = g_{\\zeta_j} = g_z = g_N = 0$", "$g_{\\zeta_j} = g_z = g_N = 0$,$\\mu_t=\\mu_1$, $\\forall t$")

### start at column 3 and add the characters later
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
output_matrix <- format_round_3(output_matrix)
output_matrix <- output_matrix[,3:7]

output_matrix <- cbind(counterfactual_names,output_matrix)
output_matrix <- cbind(column1,output_matrix)
output_matrix <- unname(output_matrix)

colnames(output_matrix) <- c(" ","Counterfactual","$p_t$ Growth$^*$","LE Lost$^\\%$",
                             "Health Share$^\\$$","$K_{ht} / K_t$$^\\$$","$L_{ht} / L_t$$^\\$$")
# create xtab
xtab <- xtable(output_matrix,
               caption = "Counterfactual Simulations Relative to Model Predictions",
               label = "counterfactual_table")

### HERE ###

# Build add.to.row list: replace header with custom lines, add hline after row 1
add_lines <- list()
add_lines$pos <- list(2,5)
add_lines$command <- c(
  "\\hline \n",
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
latex_lines[6] <- "\\label{counterfactual_table} \\small"    
latex_lines[8] <- " \\hline \\\\[-1.8ex] \\multicolumn{7}{c}{\\underline{\\textit{Targeted Moments, Change from 1960-2015}}}\\\\[1ex]"
latex_lines[10] <- " \\hline \\\\[-1.8ex]"
latex_lines[21] <- " \\\\[-1.8ex] \\hline "


latex_lines <- sub(
  pattern = "^\\\\begin\\{tabular\\}\\{.*\\}$",
  replacement = " \\\\hspace*{3pt}\\\\makebox[\\\\linewidth][c]{ \\\\begin{tabular}{@{\\\\extracolsep{5pt}} llccccc} \\\\\\\\[-5ex] \\\\hline",
  latex_lines
)
latex_lines <- sub(
  pattern = "^\\\\end\\{tabular\\}",
  replacement = " \\\\hline \\\\\\\\[-1.8ex] \\\\end{tabular}}",
  latex_lines
)

latex_lines[23] <- "\\begin{tablenotes} \\\\ $^*$ Percent change from 1960-2015.\\\\ $^\\%$ Life expectancy years lost relative to 2015 baseline prediction. Negative values imply life-years gained. \\\\ $^\\$$ Percentage point difference from 1960-2015. \\end{tablenotes}\\end{table}"

# Write to .tex file
writeLines(latex_lines, paste0(results_dir,"/table3.tex"))

################################### FIGURE 5 ###################################
# Open a PNG device (optional)
#png(paste0(results_dir,"/figure5.png"), width = 2500, height = 1700, res = 300)
pdf(paste0(results_dir,"/figure5.pdf"),width=7,height=4.75999999999)
# Define a 2x2 layout with custom widths and heights
layout_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
# Set up the layout with different relative sizes
layout(layout_matrix, widths = c(1, 1), heights = c(1, 1, 1))
# Add borders to visualize plot boundaries
par(mar = c(4, 4.3, 2, 1))  # margin setup

#### FIGURE 5A ####
# plot
ymin = min(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE) 
ymax = max(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE) 
#png(filename=paste(results_dir,"/figure5a.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Relative Price of Health Care",xlab=expression(paste("(a) ",p[t],sep="")),cex.lab=0.85)
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
#dev.off()

#### FIGURE 5b ####
# plot
ymin = min(le_counters_normalized[le_counters_normalized$year  %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
ymax = max(le_counters_normalized[le_counters_normalized$year  %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
#png(filename=paste(results_dir,"/figure5b.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Life Expectancy at Age 20",xlab=expression(paste("(b) ",LE[t],sep="")),cex.lab=0.85)
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
#dev.off()

#### FIGURE 5c ####
# plot
ymin = min(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
ymax = max(he_counters_normalized[,c("data","predicted")],na.rm=TRUE)
#png(filename=paste(results_dir,"/figure5c.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Health Share of Aggregate Expenditure",xlab=expression(paste("(c) ",p[t],H[t],"/(",p[t],H[t],"+",C[t],")",sep="")),cex.lab=0.85)
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
#dev.off()

#### Figure 5d ####
# plot
ymin = min(k_share_normalized[k_share_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
ymax = max(k_share_normalized[k_share_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
#png(filename=paste(results_dir,"/figure5d.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Health Production Share of Capital",xlab=expression(paste("(d) ",K[ht],"/",K[t],sep="")),cex.lab=0.85)
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
#dev.off()

#### Figure 5e ####
# plot
ymin = min(l_share_normalized[l_share_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
ymax = max(l_share_normalized[l_share_normalized$year %in% seq(1960,2020,5),c("data","predicted")],na.rm=TRUE)
#png(filename=paste(results_dir,"/figure5e.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Health Production Share of Labor",xlab=expression(paste("(e) ",L[ht],"/",L[t],sep="")),cex.lab=0.85)
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
#dev.off()

##### Figure 5f ####
# plot predicted growth with data
ymin = min(gy_sim[gy_sim$year %in% seq(1960,2020,5),c("gy_sim","growth")],na.rm=TRUE)
ymax = max(gy_sim[gy_sim$year %in% seq(1960,2020,5),c("gy_sim","growth")],na.rm=TRUE)
#png(filename=paste(results_dir,"/figure5f.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],plot(year,gy_sim,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA,xaxt="n"))
grid()
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],points(year,growth,pch=17,col="black",lwd=4))
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],points(year,gy_sim,pch=1,col="black",lwd=2))
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],lines(year,growth,col="black",lwd=1,lty=1))
with(gy_sim[gy_sim$year %in% seq(1960,2020,5),],lines(year,gy_sim,col="black",lwd=1,lty=3))
axis(1, at=seq(1960,2020,10),las=1)
title(ylab="Average Annual GDP Growth Rate",xlab=expression(paste("(f) ",g[Yt],sep="")),cex.lab=0.85)
legend(x= "topright",inset=0,cex=0.75,
       legend = c("Data","Predicted"),
       pch = c(17,1), # ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
       lty=c(1,3),lwd=1.4, #2,4,12,3,5,6,7,8,9,10,11,12),
       col = c("black","black")) #,"blue","darkorange","gold2","red","darkgreen","purple","goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon"),lwd=1.2,cex=0.6)
dev.off()


############################### FIGURE 7 #######################################
# Open a PNG device (optional)
#png(paste0(results_dir,"/figure7.png"), width = 2500, height = 1700, res = 300)
pdf(paste0(results_dir,"/figure7.pdf"),width=7,height=4.75999999999)
# Define a 2x2 layout with custom widths and heights
layout_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
# Set up the layout with different relative sizes
layout(layout_matrix, widths = c(1, 1), heights = c(1, 1, 1))
# Add borders to visualize plot boundaries
par(mar = c(4, 4.3, 2, 1))  # margin setup

### figure 7a ###
#### plot with counters ####
# no's (1:3)
ymin = min(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#p_counters_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#  png(filename=paste(results_dir,"/figure7a.png",sep=""),width=5,height=4,units="in",res=600)
#  par(mar = c(2,5,2,2))
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
title(ylab="Relative Price of Health Care",xlab=expression(paste("(a) ",p[t],sep="")),cex.lab=0.85)
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
#dev.off()

### figure 7b ###
ymin = min(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#le_counters_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#png(filename=paste(results_dir,"/figure7b.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Life Expectancy at Age 20",xlab=expression(paste("(b) ",LE[t],sep="")),cex.lab=0.85)
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
#  dev.off()

### figure 7c ###
# no's (1:3)
ymin = min(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#he_counters_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#  png(filename=paste(results_dir,"/figure7c.png",sep=""),width=5,height=4,units="in",res=600)
#  par(mar = c(2,5,2,2))
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
title(ylab="Health Share of Aggregate Expenditure",xlab=expression(paste("(c) ",p[t],H[t],"/(",p[t],H[t],"+",C[t],")",sep="")),cex.lab=0.85)
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
#dev.off()

#### figure 7d ####

# no's (1:3)
ymin = min(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#k_share_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
# png(filename=paste(results_dir,"/figure7d.png",sep=""),width=5,height=4,units="in",res=600)
# par(mar = c(2,5,2,2))
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
title(ylab="Health Production Share of Capital",xlab=expression(paste("(d) ",K[ht],"/",K[t],sep="")),cex.lab=0.85)
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
# dev.off()

### FIGURE 7E ###
# no's (1:3)
ymin = min(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#l_share_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
# png(filename=paste(results_dir,"/figure7e.png",sep=""),width=5,height=4,units="in",res=600)
# par(mar = c(2,5,2,2))
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
title(ylab="Health Production Share of Labor",xlab=expression(paste("(e) ",L[ht],"/",L[t],sep="")),cex.lab=0.85)
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
# dev.off()


#### figure 7f #### 
# no's (1:3)
ymin = min(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#gy_counters_normalized$year = seq(1950,2050,5)
cols = c("blue","red","purple") #,"darkgreen","darkorange","gold2") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#  png(filename=paste(results_dir,"/figure7f.png",sep=""),width=5,height=4,units="in",res=600)
#  par(mar = c(2,5,2,2))
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
title(ylab="Average Annual GDP Growth Rate",xlab=expression(paste("(f) ",g[Yt],sep="")),cex.lab=0.85)
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

############################### FIGURE 8 #######################################
# Open a PNG device (optional)
#png(paste0(results_dir,"/figure8.png"), width = 2500, height = 1700, res = 300)
pdf(paste0(results_dir,"/figure8.pdf"), width = 7, height=4.75999999999)
# Define a 2x2 layout with custom widths and heights
layout_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
# Set up the layout with different relative sizes
layout(layout_matrix, widths = c(1, 1), heights = c(1, 1, 1))
# Add borders to visualize plot boundaries
par(mar = c(4, 4.3, 2, 1))  # margin setup

#### figure 8a ####
# yay's (4:6)
ymin = min(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(p_counters_normalized[p_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#p_counters_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#png(filename=paste(results_dir,"/figure8a.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Relative Price of Health Care",xlab=expression(paste("(a) ",p[t],sep="")),cex.lab=0.85)
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
#dev.off()

#### figure 8b ####
# yay's (4:6)
ymin = min(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(le_counters_normalized[le_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#le_counters_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#png(filename=paste(results_dir,"/figure8b.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Life Expectancy at Age 20",xlab=expression(paste("(b) ",LE[t],sep="")),cex.lab=0.85)
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
#dev.off()

#### figure 8c ####
# yay's (4:6)
ymin = min(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(he_counters_normalized[he_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#he_counters_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#png(filename=paste(results_dir,"/figure8c.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Health Share of Aggregate Expenditure",xlab=expression(paste("(c) ",p[t],H[t],"/(",p[t],H[t],"+",C[t],")",sep="")),cex.lab=0.85)
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
#dev.off()

#### Figure 8d ####
# yay's (4:6)
ymin = min(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(k_share_normalized[k_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#k_share_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#png(filename=paste(results_dir,"/figure8d.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Health Production Share of Capital",xlab=expression(paste("(d) ",K[ht],"/",K[t],sep="")),cex.lab=0.85)
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
#dev.off()

#### Figure 8e ####
# yay's (4:6)
ymin = min(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(l_share_normalized[l_share_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#l_share_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#png(filename=paste(results_dir,"/figure8e.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Health Production Share of Labor",xlab=expression(paste("(e) ",L[ht],"/",L[t],sep="")),cex.lab=0.85)
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
#dev.off()


#### Figure 8f ####
# yay's (4:6)
ymin = min(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
ymax = max(gy_counters_normalized[gy_counters_normalized$year %in% seq(1960,2050,5),c(1:8)],na.rm=TRUE)
#gy_counters_normalized$year = seq(1950,2050,5)
cols = c("darkorange","darkcyan","goldenrod4") #"goldenrod4","darkseagreen","cornflowerblue","darkcyan","darksalmon","gold2")
#png(filename=paste(results_dir,"/figure8f.png",sep=""),width=5,height=4,units="in",res=600)
#par(mar = c(2,5,2,2))
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
title(ylab="Average Annual GDP Growth Rate",xlab=expression(paste("(f) ",g[Yt],sep="")),cex.lab=0.85)
# place legend in order of constellations of counterfactuals
legend(x= "topright",inset=0,cex=0.5,
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


print("#########################################################################")
print("#########################################################################")
print("#########################################################################")
print("###### END Calibration R script for GE_Calibration_Mu_Growth_Past_2015 ...##")
print("#########################################################################")
print("#########################################################################")
print("#########################################################################")
print("#########################################################################")


closeAllConnections()
