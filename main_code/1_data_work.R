rm(list=ls())

####              DATA WORK              #####
#   HEALTH SECTOR STRUCTURAL CHANGE          #
####              August 2025            #####

###########################################
#   
#     Libs
#
###########################################

require("readr")
require("reshape2")
require("tidyr")
require("scales")
require("Hmisc")
require("readxl")
require("EnvStats")
require("ivreg")
require("xtable")
require("stringr")

# require("stargazer")
# require("haven")
# require("dplyr")
# require("purrr")
# require("broom")
# require("haven")
# require("ggplot2")
# require("nleqslv")
# require("fredr")

# decomp plot bounds #
bound_hold = c(-0.5,1) 

# data dirs
raw_data_dir = "data/raw/"

# figure/table results output directory
results_dir = "results"

# analysis directory
analysis_dir = "data/analysis"

# prints in-line numbers and data references
sink(paste0(results_dir,"/1_data_SCRIPT.txt")) # prints in-line data into a txt file


############################################# SECTION 1 ################################################## 
############################################# APPENDIX A #################################################

print("INTRODUCTION AND APPENDIX A")

###########################################
#
#     NIPA Data
#
###########################################

# pce by function
hold_names = seq(1948,2022,1)
# 2_5_3 (quantity indices)
bea_nipa_2_5_3 <- read_csv(paste0(raw_data_dir,"bea_nipa_2_5_3.csv"),skip = 3)
bea_nipa_2_5_3 <- bea_nipa_2_5_3[2:nrow(bea_nipa_2_5_3),]
#hold_names = colnames(bea_nipa_2_5_4)
colnames(bea_nipa_2_5_3) = c("Line","Series",hold_names)
bea_nipa_2_5_3 <- bea_nipa_2_5_3[1:(which(grepl("Legend/Footnotes",bea_nipa_2_5_3$Line))-1),] # cuts off at footnote rows
bea_nipa_2_5_3[,3:ncol(bea_nipa_2_5_3)] <- sapply(bea_nipa_2_5_3[,3:ncol(bea_nipa_2_5_3)],function(x){as.numeric(x)})
# 2_5_4 (prices)
bea_nipa_2_5_4 <- read_csv(paste0(raw_data_dir,"bea_nipa_2_5_4.csv"),skip = 3)
bea_nipa_2_5_4 <- bea_nipa_2_5_4[2:nrow(bea_nipa_2_5_4),]
#hold_names = colnames(bea_nipa_2_5_4)
colnames(bea_nipa_2_5_4) = c("Line","Series",hold_names)
bea_nipa_2_5_4 <- bea_nipa_2_5_4[1:(which(grepl("Legend/Footnotes",bea_nipa_2_5_4$Line))-1),] # cuts off at footnote rows
bea_nipa_2_5_4[,3:ncol(bea_nipa_2_5_4)] <- sapply(bea_nipa_2_5_4[,3:ncol(bea_nipa_2_5_4)],function(x){as.numeric(x)})
# 2_5_5 (expenditure)
bea_nipa_2_5_5 <- read_csv(paste0(raw_data_dir,"bea_nipa_2_5_5.csv"),skip = 3)
bea_nipa_2_5_5 <- bea_nipa_2_5_5[2:nrow(bea_nipa_2_5_5),]
#hold_names = colnames(bea_nipa_2_5_5)
colnames(bea_nipa_2_5_5) = c("Line","Series",hold_names)
bea_nipa_2_5_5 <- bea_nipa_2_5_5[1:(which(grepl("Legend/Footnotes",bea_nipa_2_5_5$Line))-1),] # cuts off at footnote rows
bea_nipa_2_5_5[,3:ncol(bea_nipa_2_5_5)] <- sapply(bea_nipa_2_5_5[,3:ncol(bea_nipa_2_5_5)],function(x){as.numeric(x)})

# gov't spending by function
hold_names = seq(1959,2022,1) # only goes back to 1959
# 3_15_5
bea_nipa_3_15_5 <- read_csv(paste0(raw_data_dir,"bea_nipa_3_15_5.csv"),skip = 3)
bea_nipa_3_15_5 <- bea_nipa_3_15_5[2:nrow(bea_nipa_3_15_5),]
#hold_names = colnames(bea_nipa_3_15_5)
colnames(bea_nipa_3_15_5) = c("Line","Series",hold_names)
bea_nipa_3_15_5 <- bea_nipa_3_15_5[1:(which(grepl("Legend/Footnotes",bea_nipa_3_15_5$Line))-1),] # cuts off at footnote rows
bea_nipa_3_15_5[,3:ncol(bea_nipa_3_15_5)] <- sapply(bea_nipa_3_15_5[,3:ncol(bea_nipa_3_15_5)],function(x){as.numeric(x)})

# 1_5_5 (GDP in expanded detail)
hold_names = seq(1948,2022,1) # only goes back to 1959
bea_nipa_1_5_5 <- read_csv(paste0(raw_data_dir,"bea_nipa_1_5_5.csv"),skip = 3)
bea_nipa_1_5_5 <- bea_nipa_1_5_5[2:nrow(bea_nipa_1_5_5),]
#hold_names = colnames(bea_nipa_1_5_5)
colnames(bea_nipa_1_5_5) = c("Line","Series",hold_names)
bea_nipa_1_5_5 <- bea_nipa_1_5_5[1:(which(grepl("Legend/Footnotes",bea_nipa_1_5_5$Line))-1),] # cuts off at footnote rows
bea_nipa_1_5_5[,3:ncol(bea_nipa_1_5_5)] <- sapply(bea_nipa_1_5_5[,3:ncol(bea_nipa_1_5_5)],function(x){as.numeric(x)})

########## Health Spending Share of Total PCE ##########
major_type_product = c("Personal consumption expenditures","Food and beverages purchased for off-premises consumption","Clothing, footwear, and related services",
                       "Housing, utilities, and fuels","Furnishings, household equipment, and routine household maintenance",
                       "Health","Transportation","Communication","Recreation","Education",
                       "Food services and accommodations","Financial services and insurance",
                       "Other goods and services","Net foreign travel and expenditures abroad by U.S. residents",
                       "Final consumption expenditures of nonprofit institutions serving households (NPISHs)28")
holder = bea_nipa_2_5_5[bea_nipa_2_5_5$Series %in% c("Personal consumption expenditures","Health"),]
holder2 = melt(holder[,2:ncol(holder)],id="Series")
holder2$variable = as.numeric(as.character(holder2$variable))
holder2 = spread(holder2,key="Series",value="value")
holder2$`Health Share` <- holder2$Health / holder2$`Personal consumption expenditures`
health_share_holder = holder2
rm(holder,holder2)


########### FIGURE 1 ###########
# Open a PNG device (optional)
#png(paste0(results_dir,"/figure1.png"), width = 2000, height = 2000, res = 300)
pdf(paste0(results_dir,"/figure1.pdf"), width = 7, height = 7) 
# Define a 2x2 layout with custom widths and heights
layout_matrix <- matrix(c(1, 2,
                          3, 4), nrow = 2, byrow = TRUE)
# Set up the layout with different relative sizes
layout(layout_matrix, widths = c(1, 1), heights = c(1, 1))
# Add borders to visualize plot boundaries
par(mar = c(4, 4, 2, 1))  # margin setup

#### Figure 1a ####
# plot agg health share of spending (PCE)
ymin = min(health_share_holder$`Health Share`)
ymax = max(health_share_holder$`Health Share`)
#png(filename=paste("../Draft/aggregate_health_share_pce.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(health_share_holder,plot(variable,`Health Share`,type="n",ylim=c(ymin,ymax),ann=FALSE,ylab=NA,xlab=NA,xaxt="n"))
grid()
with(health_share_holder,lines(variable,`Health Share`,col="black",lty=1,lwd=2))
axis(1, at=seq(1950, 2020, 10), las=1)
title(ylab=expression(paste("Personal Health Spending Share of PCE")),xlab=paste0("(a) Aggregate Health Share PCE"),cex.lab=0.9)
#dev.off()

# print some data values
print('The personal health spending share of total personal consumption expenditure was ...')
print('In 1950:')
print(health_share_holder$`Health Share`[3])
print('In 2019:')
print(health_share_holder$`Health Share`[nrow(health_share_holder)-3])
print('In 2020:')
print(health_share_holder$`Health Share`[nrow(health_share_holder)-2])
print('In 2021:')
print(health_share_holder$`Health Share`[nrow(health_share_holder)-1])
print('In 2022:')
print(health_share_holder$`Health Share`[nrow(health_share_holder)])
print('From 1948 to 2022, health spending grew x times faster than total spending:')
print((health_share_holder$Health[nrow(health_share_holder)] / health_share_holder$Health[1]) / (health_share_holder$`Personal consumption expenditures`[nrow(health_share_holder)] / health_share_holder$`Personal consumption expenditures`[1]) )
print('From 1948 to 2022, health spending grew x times faster than non-health spending:')
print((health_share_holder$Health[nrow(health_share_holder)] / health_share_holder$Health[1]) / ((health_share_holder$`Personal consumption expenditures`[nrow(health_share_holder)]-health_share_holder$Health[nrow(health_share_holder)]) / (health_share_holder$`Personal consumption expenditures`[1]-health_share_holder$Health[1])) )


########### Total Health Share of GDP (personal health plus government) ###########
holder = bea_nipa_3_15_5[bea_nipa_3_15_5$Line == "27",]
holder = melt(holder[,2:ncol(holder)],id="Series")
holder$variable = as.numeric(as.character(holder$variable))
holder = spread(holder,key="Series",value="value")
colnames(holder) = c("variable","Government Health Consumption")
holder = merge(holder,health_share_holder,by="variable",all.y=TRUE)
holder2 = holder
# gdp
holder = bea_nipa_1_5_5[bea_nipa_1_5_5$Line == "1",]
holder = melt(holder[,2:ncol(holder)],id="Series")
holder$variable = as.numeric(as.character(holder$variable))
holder = spread(holder,key="Series",value="value")
colnames(holder) = c("variable","GDP")
holder = merge(holder,holder2,by="variable",all.y=TRUE)
holder$`Total Health` = holder$`Government Health Consumption` + holder$Health
holder$`Total Health Share` = holder$`Total Health` / holder$GDP
health_share_holder = holder
rm(holder2,holder)

#### Figure 1b ####
# gdp share of health
ymin = min(health_share_holder$`Total Health Share`,na.rm = TRUE)
ymax = max(health_share_holder$`Total Health Share`,na.rm = TRUE)
#png(filename=paste("../Draft/aggregate_health_share_gdp.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(na.omit(health_share_holder),plot(variable,`Total Health Share`,type="n",ylim=c(ymin,ymax),ann=FALSE,ylab=NA,xlab=NA,xaxt="n"))
grid()
with(na.omit(health_share_holder),lines(variable,`Total Health Share`,col="black",lty=1,lwd=2))
axis(1, at=seq(1950, 2020, 10), las=1)
title(ylab=expression(paste("Total Health Share of GDP")),xlab=paste0("(b) Aggregate Health Share GDP"),cex.lab=0.9)
#legend(x = "topright",inset = 0,
#       legend = c("Durables Stock","Durables Expend","No Durables"),
#       lty=c(1,3,2),
#       col=c("black","red","blue"), lwd=2, cex=0.7)
#dev.off()

print('The total (personal + govt) health spending share of GDP was ...')
print('In 1959:')
print(health_share_holder[health_share_holder$variable == 1959,"Total Health Share"])
print('In 2019:')
print(health_share_holder[health_share_holder$variable == 2019,"Total Health Share"])
print('In 2020:')
print(health_share_holder[health_share_holder$variable == 2020,"Total Health Share"])
print('In 2021:')
print(health_share_holder[health_share_holder$variable == 2021,"Total Health Share"])
print('In 2022:')
print(health_share_holder[health_share_holder$variable == 2022,"Total Health Share"])
print('From 1959 to 2022, health spending grew x times faster than GDP:')
print((health_share_holder[health_share_holder$variable == 2022,"Total Health"] / health_share_holder[health_share_holder$variable == 1959,"Total Health"]) / (health_share_holder[health_share_holder$variable == 2021,"GDP"] / health_share_holder[health_share_holder$variable == 1959,"GDP"]) )
print('From 1959 to 2022, health spending grew x times faster than all other contributors to GDP:')
print((health_share_holder[health_share_holder$variable == 2022,"Total Health"]/ health_share_holder[health_share_holder$variable == 1959,"Total Health"]) / ((health_share_holder[health_share_holder$variable == 2021,"GDP"]  -health_share_holder[health_share_holder$variable == 2021,"Total Health"]) / (health_share_holder[health_share_holder$variable == 1959,"GDP"] - health_share_holder[health_share_holder$variable == 1959,"Total Health"])) )



#########################################################################
#
#       Make price index for non-health pce, domestic spending only
#
#########################################################################

major_subtype_product = major_type_product[2:length(major_type_product)]
non_health_names = major_subtype_product[!major_subtype_product %in% c("Health")]
# now compute 2012 real valuations (not quantity indices) in billions of 2012 dollars
agg_spending = melt(bea_nipa_2_5_5[,2:ncol(bea_nipa_2_5_5)],id="Series")
agg_spending$variable = as.numeric(as.character(agg_spending$variable))
agg_spending = spread(agg_spending,key="Series",value="value")
agg_prices = melt(bea_nipa_2_5_4[,2:ncol(bea_nipa_2_5_4)],id="Series")
agg_prices$variable = as.numeric(as.character(agg_prices$variable))
agg_prices = spread(agg_prices,key="Series",value="value")
for(i in nrow(agg_prices):1){
  for(j in 2:ncol(agg_prices)){
    agg_prices[i,j] <- agg_prices[i,j] / agg_prices[1,j]
  }
}
non_health_quantities = agg_spending[,non_health_names] / agg_prices[,non_health_names]
non_health_prices = agg_prices[,non_health_names] 
non_health_names = non_health_names[!non_health_names %in% c("Net foreign travel and expenditures abroad by U.S. residents")]
non_health_quantities = non_health_quantities[,non_health_names]
non_health_prices = non_health_prices[,non_health_names]
non_health_quantities$YEAR = agg_spending$variable 
non_health_prices$YEAR = agg_prices$variable 
non_health_spending = agg_spending[,non_health_names]
non_health_spending$YEAR = agg_spending$variable

# construct the quantity index
non_health_quantity_index <- matrix(NA,nrow=nrow(non_health_quantities),ncol=1)
non_health_quantity_index[1,1] <- 1
for(t in 2:nrow(non_health_quantity_index)){
  non_health_quantity_index[t,1] <- non_health_quantity_index[t-1,1] *
    sqrt( (sum(non_health_prices[t-1,non_health_names] * non_health_quantities[t,non_health_names]) * sum(non_health_spending[t,non_health_names])) /
            (sum(non_health_spending[t-1,non_health_names]) * sum(non_health_prices[t,non_health_names] * non_health_quantities[t-1,non_health_names])))
}

# aggregate quantity of non-health consumption in 1948 real dollars (billions)
non_health_real_consumption = non_health_quantity_index[,1] * sum(non_health_spending[1,non_health_names])

# health real consumption
health_quant_index = as.vector(unlist(unname(bea_nipa_2_5_3[bea_nipa_2_5_3$Series == "Health",3:ncol(bea_nipa_2_5_3)])))
health_quant_index = health_quant_index / health_quant_index[1]
health_real_consumption = agg_spending[agg_spending$variable == 1948,"Health"] * health_quant_index

# aggregate chain-weighted price index, 1948 = 1
non_health_price_index = rowSums(non_health_spending[,non_health_names]) / non_health_real_consumption

# plot relative price ratio over time (health svcs / imputed price of all other consumptions)
holder <- as.data.frame(list(year=non_health_prices$YEAR,health_price=agg_prices$Health,other_consumption_price=non_health_price_index))
holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO = holder$health_price / holder$other_consumption_price
holder2 = holder

#### Figure 1(c) ####
#png(filename=paste("../Draft/health_svcs_other_consumption_price_ratio.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(holder,plot(year,HEALTH_OTHER_CONSUMPTION_PRICE_RATIO,type="n",ann=FALSE,ylab=NA,xlab=NA,xaxt="n"))
grid()
with(holder,lines(year,HEALTH_OTHER_CONSUMPTION_PRICE_RATIO,col="black",lty=1,lwd=2))
axis(1, at=seq(1950, 2020, 10), las=1)
title(ylab=expression(paste("Relative Price of Health Care")),xlab=paste0("(c) Relative Health Price, 1948 = 1"),cex.lab=0.9)
#legend(x = "topright",inset = 0,
#       legend = c("Durables Stock","Durables Expend","No Durables"),
#       lty=c(1,3,2),
#       col=c("black","red","blue"), lwd=2, cex=0.7)
#dev.off()

print('The relative price of health care to all other domestic PCE has grown by x % since 1948:')
print((holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO[nrow(holder)] / holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO[1]-1)*100)
print('The relative price of health care to all other domestic PCE has grown by x times since 1948:')
print((holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO[nrow(holder)] / holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO[1]))
print('The absolute price of health care has grown by x % since 1948:')
print((holder$health_price[nrow(holder)] / holder$health_price[1]-1)*100)
print('The absolute price of health care has grown by x times since 1948:')
print((holder$health_price[nrow(holder)] / holder$health_price[1]))

# plot relative quantities of health consumption, re-indexed to 1 in 1959 (relative to all other consumption quantities, using a unit-less quantity index)
years = seq(1948,2022,1)
holder <- as.data.frame(list(year=years,health_real_consumption=health_real_consumption,
                             non_health_real_consumption = non_health_real_consumption))
holder$health_quant_ratio = health_real_consumption /non_health_real_consumption

#### Figure 1(d) ####
#png(filename=paste("../Draft/health_svcs_other_consumption_quantity_ratio.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(holder,plot(year,health_quant_ratio,type="n",ann=FALSE,ylab=NA,xlab=NA,xaxt="n"))
grid()
with(holder,lines(year,health_quant_ratio,col="black",lty=1,lwd=2))
axis(1, at=seq(1950, 2020, 10), las=1)
title(ylab=expression(paste("Relative Real Health Consumption, $1948")),xlab=paste0("(d) Relative Health Quantities, $1948"),cex.lab=0.9)
dev.off()

print('The relative quantity of health care to all other domestic Personal Consumption has grown by x % since 1948:')
print((holder$health_quant_ratio[nrow(holder)] / holder$health_quant_ratio[1]-1)*100)
print('The relative quantity of health care to all other domestic Personal Consumption has grown by x times since 1948:')
print((holder$health_quant_ratio[nrow(holder)] / holder$health_quant_ratio[1]))
print('The absolute quantity level of health care has grown by x % since 1948:')
print((holder$health_real_consumption[nrow(holder)] / holder$health_real_consumption[1]-1)*100)
print('The absolute quantity level of health care has grown by x times since 1948:')
print((holder$health_real_consumption[nrow(holder)] / holder$health_real_consumption[1]))

# include these appendix figures in code after Figure 1 #

#### Figure B.1 ####
# breakdown of personal vs government health spending share of gdp
health_share_holder$`Personal Health Share` <- health_share_holder$Health / health_share_holder$GDP
health_share_holder$`Government Health Share` <- health_share_holder$`Government Health Consumption` / health_share_holder$GDP
ymin = min(health_share_holder[,c("Personal Health Share","Government Health Share")],na.rm = TRUE)
ymax = max(health_share_holder[,c("Personal Health Share","Government Health Share")],na.rm = TRUE)
png(filename=paste(results_dir,"/figureB1.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,5,2,2))
with(na.omit(health_share_holder),plot(variable,`Personal Health Share`,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA)) #,xaxt="n"))
grid()
with(na.omit(health_share_holder),lines(variable,`Personal Health Share`,col="red",lty=2,lwd=2))
with(na.omit(health_share_holder),lines(variable,`Government Health Share`,col="blue",lty=3,lwd=2))
#axis(1, at=seq(1950, 2020, 10), las=1)
title(ylab=expression(paste("Share of GDP")),cex.lab=1)
legend(x = "topleft",inset = 0,
       legend = c("Direct Health Spending","Gov't Administration"),
       lty=c(2,3),
       col=c("red","blue"), lwd=2, cex=0.7)
dev.off()

print('The total personal health spending share of GDP was ...')
print('In 1959:')
print(health_share_holder[health_share_holder$variable == 1959,"Personal Health Share"])
print('In 2019:')
print(health_share_holder[health_share_holder$variable == 2019,"Personal Health Share"])
print('In 2020:')
print(health_share_holder[health_share_holder$variable == 2020,"Personal Health Share"])
print('In 2021:')
print(health_share_holder[health_share_holder$variable == 2021,"Personal Health Share"])
print('In 2022:')
print(health_share_holder[health_share_holder$variable == 2022,"Personal Health Share"])
print('The govt health spending (administrative) share of GDP was ...')
print('In 1959:')
print(health_share_holder[health_share_holder$variable == 1959,"Government Health Share"])
print('In 2019:')
print(health_share_holder[health_share_holder$variable == 2019,"Government Health Share"])
print('In 2020:')
print(health_share_holder[health_share_holder$variable == 2020,"Government Health Share"])
print('In 2021:')
print(health_share_holder[health_share_holder$variable == 2021,"Government Health Share"])
print('In 2022:')
print(health_share_holder[health_share_holder$variable == 2022,"Government Health Share"])

########### Breakdown of Personal Health Consumption, Spending Shares of PCE, by Health Spending Sub-type #############
# share of health spending by sub category #
health_consumption_types = c("Health","Pharmaceutical and other medical products9","Therapeutic appliances and equipment",
                             "Physician services10","Dental services","Paramedical services",
                             "Hospitals12","Nursing homes")
holder = bea_nipa_2_5_5[bea_nipa_2_5_5$Series %in% health_consumption_types,]
holder = melt(holder[,2:ncol(holder)],id="Series")
holder$value = as.numeric(holder$value)
holder$variable = as.numeric(as.character(holder$variable))
holder = spread(holder,key="Series",value="value")
health_consumption_subtypes = c("Pharmaceutical and other medical products9","Therapeutic appliances and equipment",
                                "Physician services10","Dental services","Paramedical services",
                                "Hospitals12","Nursing homes")
holder$`Pharmaceutical and other medical products share` = holder$`Pharmaceutical and other medical products9` / holder$Health
holder$`Therapeutic appliances and equipment share` = holder$`Therapeutic appliances and equipment` / holder$Health
holder$`Physician services share` = holder$`Physician services10` / holder$Health
holder$`Dental services share` = holder$`Dental services` / holder$Health
holder$`Paramedical services share` = holder$`Paramedical services` / holder$Health
holder$`Hospitals share` = holder$Hospitals12 / holder$Health 
holder$`Nursing homes share` = holder$`Nursing homes` / holder$Health
keep_names = c("variable",colnames(holder[,grepl("share",colnames(holder))]))
holder2 = holder[,keep_names]
cols = c("black","blue","red","darkorange","darkgreen","purple","brown")

#### Figure B.2(a) ####
png(filename=paste(results_dir,"/figureB2a.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,5,2,2))
with(holder2,plot(variable,`Pharmaceutical and other medical products share`,type="n",ann=FALSE,ylim=c(min(holder2[,2:ncol(holder2)]),max(holder2[,2:ncol(holder2)])),ylab=NA,xlab=NA)) #,xaxt="n"))
grid()
for(i in 2:ncol(holder2)){
  with(holder2,lines(variable,holder2[,keep_names[i]],col=cols[i-1],lty=i,lwd=2))
}
axis(1, at=seq(1950, 2020, 20), las=1)
title(ylab=expression(paste("Share of Personal Health Spending")),cex.lab=1)
legend(x = "topleft",inset = 0,
       legend = c("Drugs","Med. Appliances","Physician Svcs.","Dental Svcs.","Paramedical Svcs.","Hospital Svcs.","Nursing Homes"),
       lty=c(2,3,4,5,6,7,8),
       col=cols[1:(length(keep_names)-1)], lwd=2, cex=0.55)
dev.off()

print('Now printing some finer, product-level changes to their contribution to total health spending over time.')
print('Shares of health PCE in 1950:')
for(j in 2:ncol(holder2)){
  print(names(holder2)[j])
  print(holder2[holder2$variable == 1950,names(holder2)[j]])
}
print('Shares of health PCE in 2019:')
for(j in 2:ncol(holder2)){
  print(names(holder2)[j])
  print(holder2[holder2$variable == 2019,names(holder2)[j]])
}
print('Shares of health PCE in 2020:')
for(j in 2:ncol(holder2)){
  print(names(holder2)[j])
  print(holder2[holder2$variable == 2020,names(holder2)[j]])
}
print('Shares of health PCE in 2021:')
for(j in 2:ncol(holder2)){
  print(names(holder2)[j])
  print(holder2[holder2$variable == 2021,names(holder2)[j]])
}
print('Shares of health PCE in 2022:')
for(j in 2:ncol(holder2)){
  print(names(holder2)[j])
  print(holder2[holder2$variable == 2022,names(holder2)[j]])
}


# relative price in 1959 dollars of the components of health spending relative to the commodity-aggregated health price
holder = bea_nipa_2_5_4[bea_nipa_2_5_4$Series %in% health_consumption_types,]
holder = melt(holder[,2:ncol(holder)],id="Series")
holder$value = as.numeric(holder$value)
holder$variable = as.numeric(as.character(holder$variable))
holder = spread(holder,key="Series",value="value")
for(i in colnames(holder[2:ncol(holder)])){
  holder[,i] = holder[,i] / holder[1,i]
}
for(j in health_consumption_subtypes){
  holder[,j] = holder[,j] / holder[,"Health"]
}
holder2 = holder[,c("variable",health_consumption_subtypes)]

#### Figure B.2(b) ####
png(filename=paste(results_dir,"/figureB2b.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,5,2,2))
with(holder2,plot(variable,holder2[,health_consumption_subtypes[1]],type="n",ann=FALSE,ylim=c(min(holder2[,2:ncol(holder2)]),max(holder2[,2:ncol(holder2)])),ylab=NA,xlab=NA)) #,xaxt="n"))
grid()
for(i in 2:ncol(holder2)){
  with(holder2,lines(variable,holder2[,health_consumption_subtypes[i-1]],col=cols[i-1],lty=i,lwd=2))
}
abline(h=1,lwd=1.1,lty=1,col="black")
axis(1, at=seq(1950, 2020, 20), las=1)
title(ylab=expression(paste("Relative Price")),cex.lab=1)
legend(x = "topleft",inset = 0,
       legend = c("Drugs","Med. Appliances","Physician Svcs.","Dental Svcs.","Paramedical Svcs.","Hospital Svcs.","Nursing Homes"),
       lty=c(2,3,4,5,6,7,8),
       col=cols[1:(length(keep_names)-1)], lwd=2, cex=0.55)
dev.off()

################# EXPENDITURE AND PRICE RATIO DECOMPOSITION ##################
health_share_holder$`Non-health Expenditure` = health_share_holder$`Personal consumption expenditures` - health_share_holder$Health
names(health_share_holder)[names(health_share_holder) == "variable"] <- "year"
# note that there may be a slight error here due to the unwinding of the chain-weighted indices 
write(health_share_holder[health_share_holder$year %in% seq(1960,2015,5),"Health Share"],paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/health_share.txt"),ncolumns=1)
write(health_share_holder[health_share_holder$year %in% seq(1960,2015,5),"Health Share"],paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/health_share.txt"),ncolumns=1)
write(health_share_holder[health_share_holder$year %in% seq(1960,2015,5),"Health Share"],paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/health_share.txt"),ncolumns=1)
write(health_share_holder[health_share_holder$year %in% seq(1960,2015,5),"Health Share"],paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/health_share.txt"),ncolumns=1)
write(health_share_holder[health_share_holder$year %in% seq(1960,2015,5),"Health Share"],paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/health_share.txt"),ncolumns=1)
write(health_share_holder[health_share_holder$year %in% seq(1960,2015,5),"Health Share"],paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/health_share.txt"),ncolumns=1)


#####################################################
#
#      Import Population Distribution Data
#
#####################################################

us_pop_data <- read.csv(paste0(raw_data_dir,"/us_pop_data.csv"))
colnames(us_pop_data) = c("Index","Variant","Region","Notes","Country.code","Type","Parent.code","Year",as.character(seq(0,100,1)))

# 5-year age bins for calibrating (20-24, 25-29, 30-34, ...)
five_year_pop_data = as.data.frame(matrix(NA,nrow=nrow(us_pop_data),ncol=16))
t = 20
i = 1 # column number
while(t < 100){
  five_year_pop_data[,i] = us_pop_data[[paste0(t)]]+
    us_pop_data[[paste0(t+1)]]+
    us_pop_data[[paste0(t+2)]]+
    us_pop_data[[paste0(t+3)]]+
    us_pop_data[[paste0(t+4)]]
  i = i+1
  t = t+5
}
names(five_year_pop_data) <- seq(1,16,1)
five_year_pop_data$Year = us_pop_data$Year

# normalized 5-year pop data to 20-24 = 1
normalized_five_year = five_year_pop_data[,1:16] / five_year_pop_data[,1]
normalized_five_year$Year = five_year_pop_data$Year
# print the 1963 year for calibration purposes
print("1963 normalized distribution where 20-24 = 1:")
print(normalized_five_year[normalized_five_year$Year == 1963,1:16])

# plot the population cumulative distribution by age #
us_pop_data$Total_Pop = rowSums(us_pop_data[,9:ncol(us_pop_data)])
us_pop_age_dist = us_pop_data[,9:(ncol(us_pop_data)-1)] / us_pop_data$Total_Pop
# check that the rows sum to 1 #
#print(rowSums(us_pop_age_dist))
us_pop_age_dist$Year = us_pop_data$Year
years_to_subset = seq(1950,2020,10)
cols = c("black","blue","red","darkorange","darkgreen","purple","brown","magenta")
cols_sub = cols[c(3,5,6)]
years_to_subset2 = c(1950,1985,2020)

######################### FIGURE 2 ######################
# Open a PNG device (optional)
#png(paste0(results_dir, "/figure2.png"), width = 2500, height = 1250, res = 300)
pdf(paste0(results_dir,"/figure2.pdf"), width = 7, height = 3.5) 

# Define a 1x2 layout for side-by-side plots
layout_matrix <- matrix(c(1, 2), nrow = 1, byrow = TRUE)

# Set up the layout with equal widths
layout(layout_matrix, widths = c(1, 1), heights = c(1))

# Add borders to visualize plot boundaries
par(mar = c(4, 4, 2, 1))  # margin setup

#### Figure 2(a) ####
# plot
#png(filename=paste("~/Dropbox/US_HEALTH_MACRO/Draft/pop_dist_by_age.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,4,2,2))
plot(seq(0,100,1),us_pop_age_dist[us_pop_age_dist$Year == years_to_subset2[1],1:(ncol(us_pop_age_dist)-1)],type="n",ann=FALSE,ylim=c(min(us_pop_age_dist[us_pop_age_dist$Year %in% years_to_subset2,1:(ncol(us_pop_age_dist)-1)]),max(us_pop_age_dist[us_pop_age_dist$Year %in% years_to_subset2,1:(ncol(us_pop_age_dist)-1)])),ylab=NA,xlab=NA,xaxt="n")
grid()
for(i in 1:length(years_to_subset2)){
  x.poly = c(seq(0,100,1),100,0)
  y.poly = c(us_pop_age_dist[us_pop_age_dist$Year == years_to_subset2[i],1:(ncol(us_pop_age_dist)-1)],0,0)
  lines(seq(0,100,1),us_pop_age_dist[us_pop_age_dist$Year == years_to_subset2[i],1:(ncol(us_pop_age_dist)-1)],col=cols_sub[i],lty=1,lwd=2)
  polygon(x.poly, y.poly, col=alpha(cols_sub[i],0.15), border=NA)
}
axis(1, at=seq(0, 100, 20), las=1,cex=0.8)
title(ylab=expression(paste("Fraction of Population")),xlab="(a) Population Distribution by Age",cex.lab=0.85)
legend(x = "topright",inset = 0,
       legend = years_to_subset2,lty=1,lwd=2,col=cols_sub,cex=0.7)
#       lty=c(1,3,2),
#       col=c("black","red","blue"), lwd=2, cex=0.7)
#dev.off()


# plot the median age over time as well #
us_median_age <- read.csv(paste0(raw_data_dir,"/us_median_age.csv"))

print('Median age over time:')
print('1950:')
print(us_median_age[1,"X1950"])
print('1970:')
print(us_median_age[1,"X1970"])
print('2020:')
print(us_median_age[1,"X2020"])

# plot life expectancy by age over time, (these rates do not distinguish b/w accidental and non-accidental survival rates) #
us_life_tables <- read.csv(paste0(raw_data_dir,"/us_life_tables.csv"))
colnames(us_life_tables) <- c("Index","Variant","Type of aggregate","Notes","Country code","Type","Parent code","Period","Age (x)","Age interval (n)","Central death rate m(x,n)","Probability of dying q(x,n)","Probability of surviving p(x,n)","Number of survivors l(x)","Number of deaths d(x,n)","Number of person-years lived L(x,n)","Survival ratio S(x,n)","Person-years lived T(x,n)","Expectation of life e(x)","Average number of years lived a(x,n)")
us_life_tables = us_life_tables[,!names(us_life_tables) %in% c("Index","Parent code")]
us_life_tables = us_life_tables[!duplicated(us_life_tables),]

# life expectancy plot
# interpolate ... life expectancy to mid-point year, assuming first year is first and last year is the first year of the next cohort
holder = us_life_tables[us_life_tables$`Age (x)` == 0,c("Age (x)","Period","Expectation of life e(x)")]
holder$year = seq(1952,2017,5)
life_expectancies = approxExtrap(holder$year,holder$`Expectation of life e(x)`,xout=seq(1950,2020,1),method="linear")

#### Figure 2(b) ####
#png(filename=paste("~/Dropbox/US_HEALTH_MACRO/Draft/life_expectancy_at_birth.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,4,2,2))
with(life_expectancies,plot(x,y,type="n",ann=FALSE,ylab=NA,xlab=NA,xaxt="n"))
grid()
with(life_expectancies,lines(x,y,col='black',lty=1,lwd=2))
axis(1, at=seq(1950, 2020, 10), las=0.5)
title(ylab=expression(paste("Life Expectancy at Birth")),xlab="(b) Life Expectancy at Birth",cex.lab=0.85)
#legend(x = "topright",inset = 0,
#       legend = years_to_subset,lty=1,lwd=2,col=cols,cex=0.7)
#       lty=c(1,3,2),
#       col=c("black","red","blue"), lwd=2, cex=0.7)
dev.off()

# compute life expectancy at age 20 in the given year for estimation
holder = us_life_tables[us_life_tables$`Age (x)` == 20,c("Period","Expectation of life e(x)")]
holder$`Expectation of life e(x)` = holder$`Expectation of life e(x)` + 20 # total years lived
# take data to be midpoint of 5-year interval and interpolate
twenty_life_expectancies = as.data.frame(cbind(seq(1950,2020,1),approxExtrap(seq(1952.5,2017.5,5),holder$`Expectation of life e(x)`,xout=seq(1950,2020,1),method="linear")$y))
names(twenty_life_expectancies) = c("year","life_expectancy_20_year_old")
twenty_life_expectancies = twenty_life_expectancies[twenty_life_expectancies$year %in% seq(1960,2015,5),"life_expectancy_20_year_old"]

# write out for calibration
write(twenty_life_expectancies,paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/life_expectancies.txt"),ncolumns=1)
write(twenty_life_expectancies,paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/life_expectancies.txt"),ncolumns=1)
write(twenty_life_expectancies,paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/life_expectancies.txt"),ncolumns=1)
write(twenty_life_expectancies,paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/life_expectancies.txt"),ncolumns=1)
write(twenty_life_expectancies,paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/life_expectancies.txt"),ncolumns=1)
write(twenty_life_expectancies,paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/life_expectancies.txt"),ncolumns=1)

# working age to retiree ratio (18-64 / > 64)
holder = as.data.frame(list(year=us_pop_data$Year,
                            wapr=rowSums(us_pop_data[,colnames(us_pop_data) %in% as.character(seq(18,64,1))]) / rowSums(us_pop_data[,colnames(us_pop_data) %in% as.character(seq(65,100,1))])
))

print('Wapr over time:')
print('1950:')
print(holder[holder$year == 1950,"wapr"])
print('1970:')
print(holder[holder$year == 1970,"wapr"])
print('1990:')
print(holder[holder$year == 1990,"wapr"])
print('2020:')
print(holder[holder$year == 2020,"wapr"])

########## Death Rates From U.S. Data Sources ##########
# Health, United States 2017
# https://www.cdc.gov/nchs/hus/contents2017.htm
# Table 021
# Deaths per 100,000 all causes
table021 <- read_excel(paste0(raw_data_dir,"/HEALTH_2017_accidental_deaths/table021.xlsx"), 
                       skip = 3)
table021 = table021[c(4,5,6,7,8,9,10,11,12,13,14),1:41]
colnames(table021) = c("deaths (all causes)","1950","1960","1970",as.character(seq(1980,2016,1)))
table021 = as.data.frame(table021)
for(j in 2:ncol(table021)){
  table021[,j] <- as.numeric(table021[,j])
}
table021$Age = c(1,3,10,20,30,40,50,60,70,80,90)
table021 = table021[,2:ncol(table021)]
table021[,1:(ncol(table021)-1)] = table021[,1:(ncol(table021)-1)] / 100000

# interpolate between ages within each time period
ages = seq(0,100,1)
mort_all_cause_interp = as.data.frame(matrix(0,nrow=101,ncol=ncol(table021)))
colnames(mort_all_cause_interp) = c("Age","1950","1960","1970",as.character(seq(1980,2016,1)))
mort_all_cause_interp[,1] = ages
for(j in 2:ncol(mort_all_cause_interp)){
  mort_all_cause_interp[,j] = approxExtrap(table021[,ncol(table021)],table021[,j-1],ages,method="linear")$y
}

# interpolate between periods within each age group
mort_all_cause_interp2 = as.data.frame(matrix(0,nrow=101,ncol=(length(seq(1950,2019,1))+1)))
mort_all_cause_interp2[,1] = ages
colnames(mort_all_cause_interp2) = c("Age",as.character(seq(1950,2019,1)))
for(j in 1:nrow(mort_all_cause_interp2)){
  mort_all_cause_interp2[j,2:ncol(mort_all_cause_interp2)] = approxExtrap(as.numeric(colnames(mort_all_cause_interp[,2:ncol(mort_all_cause_interp)])),
                                                                          mort_all_cause_interp[j,2:ncol(mort_all_cause_interp)],as.numeric(colnames(mort_all_cause_interp2[,2:ncol(mort_all_cause_interp2)])),method="linear")$y
}
mort_all_cause_interp = mort_all_cause_interp2
rm(mort_all_cause_interp2)

# inverse total mortality (HJ "x")
inv_all_cause_mort = as.data.frame(matrix(0,nrow=101,ncol=(length(seq(1950,2019,1))+1)))
inv_all_cause_mort[,2:ncol(inv_all_cause_mort)] = 1/mort_all_cause_interp[,2:ncol(mort_all_cause_interp)]
inv_all_cause_mort[,1] = mort_all_cause_interp[,1]
colnames(inv_all_cause_mort) = c("Ages",as.character(seq(1950,2019,1)))

# Accidental Deaths
# Death rates from 1950-2016 from Health, United States, 2017
# sum of ...
# ... Table 27, drug overdoses (since 1999)
# ... Table 28, car accidents (since 1950)
# ... Table 29, homicide (since 1950)
# ... Table 30, suicide (since 1950)

# for a first pass, we count deaths by drug overdoses, car accidents, homicide, suicide, and occupational injuries, excluding fire-arm injuries since these may double count many homicides and suicides
# 027
table027 <- read_excel(paste0(raw_data_dir,"/HEALTH_2017_accidental_deaths/table027.xlsx"), 
                       skip = 3)
table027 = table027[4:12,]
colnames(table027) = c("overdoses",colnames(table027)[2:ncol(table027)])
table027 = as.data.frame(table027)
for(j in 2:ncol(table027)){
  table027[,j] <- as.numeric(table027[,j])
}
table027$Age = c(7.5,20,30,40,50,60,70,80,90)
# 028
table028 <- read_excel(paste0(raw_data_dir,"/HEALTH_2017_accidental_deaths/table028.xlsx"), 
                       skip = 3)
table028 = table028[c(4,6,7,8,11,12,14,15,17,18,19),]
colnames(table028) = c("car accidents","1950","1960","1970",as.character(seq(1980,2016,1)))
table028 = as.data.frame(table028)
for(j in 2:ncol(table028)){
  table028[,j] <- as.numeric(table028[,j])
}
table028$Age = table021$Age
# 029
table029 <- read_excel(paste0(raw_data_dir,"/HEALTH_2017_accidental_deaths/table029.xlsx"), 
                       skip = 3)
table029 = table029[c(4,6,7,8,12,13,15,16,18,19,20),]
colnames(table029) = c("homicides","1950","1960","1970",as.character(seq(1980,2016,1)))
table029 = as.data.frame(table029)
for(j in 2:ncol(table029)){
  table029[,j] <- as.numeric(table029[,j])
}
table029$Age = table021$Age
# 030
table030 <- read_excel(paste0(raw_data_dir,"/HEALTH_2017_accidental_deaths/table030.xlsx"), 
                       skip = 3)
table030 = table030[c(4,5,6,7,11,12,14,15,17,18,19),]
colnames(table030) = c("suicides","1950","1960","1970",as.character(seq(1980,2016,1)))
table030 = as.data.frame(cbind(table030))
for(j in 2:ncol(table030)){
  table030[,j] <- as.numeric(table030[,j])
}
table030[is.na(table030)] = 0
table030$Age = table021$Age

# accidental deaths not counting drug overdoses
mAcc = table028[,2:(ncol(table028)-1)] + table029[,2:(ncol(table029)-1)] + table030[,2:(ncol(table030)-1)]
mAcc$Age = table021$Age

# accidental deaths, not counting drug overdoses, interpolate within a time period across age groups
mAcc_interp = as.data.frame(matrix(0,nrow=101,ncol=ncol(mAcc[,1:ncol(mAcc)])))
colnames(mAcc_interp) = colnames(mAcc)
mAcc_interp[,ncol(mAcc)] = ages
for(j in 1:(ncol(mAcc_interp)-1)){
  mAcc_interp[,j] = approxExtrap(mAcc$Age,mAcc[,j],ages,method="linear")$y
}
mAcc_interp_no_drugs = mAcc_interp
mAcc_interp_no_drugs[,1:ncol(mAcc_interp_no_drugs)] = mAcc_interp_no_drugs[,1:ncol(mAcc_interp_no_drugs)] / 100000

# interpolate mAcc_interp_no_drugs over time within an age group
mAcc_interp_no_drugs2 = as.data.frame(matrix(0,nrow=101,ncol=(length(seq(1950,2019,1))+1)))
mAcc_interp_no_drugs2[,1] = ages
colnames(mAcc_interp_no_drugs2) = c("Age",as.character(seq(1950,2019,1)))
for(j in 1:nrow(mAcc_interp_no_drugs2)){
  mAcc_interp_no_drugs2[j,2:ncol(mAcc_interp_no_drugs2)] = approxExtrap(as.numeric(colnames(mAcc_interp_no_drugs[,1:(ncol(mAcc_interp_no_drugs)-1)])),
                                                                        mAcc_interp_no_drugs[j,1:(ncol(mAcc_interp_no_drugs)-1)],
                                                                        as.numeric(colnames(mAcc_interp_no_drugs2[,2:ncol(mAcc_interp_no_drugs2)])),
                                                                        method="linear")$y
}
mAcc_interp_no_drugs = mAcc_interp_no_drugs2
rm(mAcc_interp_no_drugs2)

# accidental deaths, counting reported drug overdoses after 1999
mAcc_drug = table027[,2:(ncol(table027)-1)]
mAcc_drug$Age = table027$Age

# drug overdoses, interpolate within a time period across age groups
mAcc_drug_interp = as.data.frame(matrix(0,nrow=101,ncol=ncol(mAcc_drug[,1:ncol(mAcc_drug)])))
colnames(mAcc_drug_interp) = colnames(mAcc_drug)
mAcc_drug_interp[,ncol(mAcc_drug)] = ages
for(j in 1:(ncol(mAcc_drug_interp)-1)){
  mAcc_drug_interp[,j] = approxExtrap(mAcc_drug$Age,mAcc_drug[,j],ages,method="linear")$y
}
mAcc_drug_interp[mAcc_drug_interp < 0] <- 0
holder = colnames(mAcc_interp)[colnames(mAcc_interp) %in% colnames(mAcc_drug_interp)]
holder = holder[1:(length(holder)-1)]
mAcc_interp[,holder] = mAcc_drug_interp[,holder] + mAcc_interp[,holder]
mAcc_interp[,1:(ncol(mAcc)-1)] = mAcc_interp[,1:(ncol(mAcc)-1)] / 100000

# interpolate within age groups over time
mAcc_interp2 = as.data.frame(matrix(0,nrow=101,ncol=(length(seq(1950,2019,1))+1)))
mAcc_interp2[,1] = ages
colnames(mAcc_interp2) = c("Age",as.character(seq(1950,2019,1)))
for(j in 1:nrow(mAcc_interp2)){
  mAcc_interp2[j,2:ncol(mAcc_interp2)] = approxExtrap(as.numeric(colnames(mAcc_interp[,1:(ncol(mAcc_interp)-1)])),
                                                      mAcc_interp[j,1:(ncol(mAcc_interp)-1)],
                                                      as.numeric(colnames(mAcc_interp2[,2:ncol(mAcc_interp2)])),
                                                      method="linear")$y
}
mAcc_interp = mAcc_interp2
rm(mAcc_interp2)

# adjusted mortality (a la HJ 2007)
mAdj = mort_all_cause_interp
mAdj[,2:ncol(mAdj)] = mAdj[,2:ncol(mAdj)] - mAcc_interp[,2:ncol(mAcc_interp)]
inv_adj_mort = mAdj
inv_adj_mort[,2:ncol(inv_adj_mort)] = 1/inv_adj_mort[,2:ncol(inv_adj_mort)]

# print interpolated 1963 mortality rates by age. Use the mid-point mortality rate .. (22, 27, 32, 37 ... etc.)
# construct accidental mortality rate csv file for exporting (goes to 2019)
mAcc_export_calibration = mAcc_interp[mAcc_interp$Age %in% seq(22,97,5),]
mAcc_export_calibration = mAcc_export_calibration[names(mAcc_export_calibration) %in% as.character(seq(1950,2015,5))]
mAcc_export_calibration = 1-(1-mAcc_export_calibration)^5

# export for calibration
#write.csv(mAcc_export_calibration,file="~/Dropbox/US_HEALTH_MACRO/Data_Work/mAcc_five_year.csv",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
for(j in 1:16){
  write(unlist(unname(as.vector(mAcc_export_calibration[j,]))),paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/mAcc",j,".txt"),ncolumns=1)
  write(unlist(unname(as.vector(mAcc_export_calibration[j,]))),paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/mAcc",j,".txt"),ncolumns=1)
  write(unlist(unname(as.vector(mAcc_export_calibration[j,]))),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/mAcc",j,".txt"),ncolumns=1)
  write(unlist(unname(as.vector(mAcc_export_calibration[j,]))),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/mAcc",j,".txt"),ncolumns=1)
  write(unlist(unname(as.vector(mAcc_export_calibration[j,]))),paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/mAcc",j,".txt"),ncolumns=1)
  write(unlist(unname(as.vector(mAcc_export_calibration[j,]))),paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/mAcc",j,".txt"),ncolumns=1)
}

### PLOT ALL CAUSE MORTALITY RATES DECLINE RELATIVE TO 1950 BY AGE ###
plotting_ages = c(10,20,30,40,50,60,70,80,90,100)
holder = mort_all_cause_interp[mort_all_cause_interp$Age %in% plotting_ages,]
holder[,2:ncol(holder)] = holder[,2:ncol(holder)] / holder[,2]
cols = c(cols,"gold","turquoise")

#### Figure B.3(a) ####
# plot
png(filename=paste(results_dir,"/figureB3a.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,4,2,2))
plot(seq(1950,2019,1),holder[holder$Age == plotting_ages[1],2:ncol(holder)],type="n",ann=FALSE,ylim=c(0.1,1),ylab=NA,xlab=NA,xaxt="n")
grid()
for(i in 1:length(plotting_ages)){
  lines(seq(1950,2019,1),holder[holder$Age == plotting_ages[i],2:ncol(holder)],col=cols[i],lty=1,lwd=2)
}
axis(1, at=seq(1950,2020,10), las=1)
abline(h=1,col="black",lty=2,lwd=1.3,cex=0.7)
title(ylab=expression(paste("Indexed All-cause Mortality Rates")),cex.lab=1)
legend(x = "bottomleft",inset = 0,
       legend = plotting_ages,lty=1,lwd=2,col=cols,cex=0.7)
dev.off()
print('All cause mortality over time, percent decline by age decile (1950-2019):')
for(i in 1:length(plotting_ages)){
  print(plotting_ages[i])
  print(holder[holder$Age == plotting_ages[i],"2019"] - 1)
}


### PLOT ACCIDENTAL MORTALITY RATES DECLINE RELATIVE TO 1950 BY AGE ###
holder = mAcc_interp[mAcc_interp$Age %in% plotting_ages,]
holder[,2:ncol(holder)] = holder[,2:ncol(holder)] / holder[,2]

#### Figure B.3(b) ####
# plot
png(filename=paste(results_dir,"/figureB3b.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,4,2,2))
plot(seq(1950,2019,1),holder[holder$Age == plotting_ages[1],2:ncol(holder)],type="n",ann=FALSE,ylim=c(0.3,2.1),ylab=NA,xlab=NA,xaxt="n")
grid()
for(i in 1:length(plotting_ages)){
  lines(seq(1950,2019,1),holder[holder$Age == plotting_ages[i],2:ncol(holder)],col=cols[i],lty=1,lwd=2)
}
axis(1, at=seq(1950,2020,10), las=1)
abline(h=1,col="black",lty=2,lwd=1.3,cex=0.7)
title(ylab=expression(paste("Indexed Accidental Mortality Rates")),cex.lab=1)
legend(x = "topleft",inset = 0,
       legend = plotting_ages,lty=1,lwd=2,col=cols,cex=0.7)
dev.off()
print('Accidental + homicide, suicide, overdose mortality over time, percent decline by age decile (1950-2019):')
for(i in 1:length(plotting_ages)){
  print(plotting_ages[i])
  print(holder[holder$Age == plotting_ages[i],"2019"] - 1)
}

### PLOT ACCIDENTAL MORTALITY RATES (NO DRUG OVERDOSES) DECLINE RELATIVE TO 1950 BY AGE ###
holder = mAcc_interp_no_drugs[mAcc_interp_no_drugs$Age %in% plotting_ages,]
holder[,2:ncol(holder)] = holder[,2:ncol(holder)] / holder[,2]

#### Figure B.3(c) ####
# plot
png(filename=paste(results_dir,"/figureB3c.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,4,2,2))
plot(seq(1950,2019,1),holder[holder$Age == plotting_ages[1],2:ncol(holder)],type="n",ann=FALSE,ylim=c(0.3,2.1),ylab=NA,xlab=NA,xaxt="n")
grid()
for(i in 1:length(plotting_ages)){
  lines(seq(1950,2019,1),holder[holder$Age == plotting_ages[i],2:ncol(holder)],col=cols[i],lty=1,lwd=2)
}
axis(1, at=seq(1950,2020,10), las=1)
abline(h=1,col="black",lty=2,lwd=1.3,cex=0.7)
title(ylab=expression(paste("Indexed Accidental Mortality Rates, No Overdoses")),cex.lab=0.75)
legend(x = "topleft",inset = 0,
       legend = plotting_ages,lty=1,lwd=2,col=cols,cex=0.7)
dev.off()
print('Accidental + homicide + suicide (no overdoses) mortality over time, percent decline by age decile (1950-2019):')
for(i in 1:length(plotting_ages)){
  print(plotting_ages[i])
  print(holder[holder$Age == plotting_ages[i],"2019"] - 1)
}

### NON-ACCIDENTAL (ADJUSTED) MORTALITY RATES RELATIVE TO 1950 BY AGE ###
holder = mAdj[mAdj$Age %in% plotting_ages,]
holder[,2:ncol(holder)] = holder[,2:ncol(holder)] / holder[,2]

print('Adjusted mortality over time, percent decline by age decile (1950-2019):')
for(i in 1:length(plotting_ages)){
  print(plotting_ages[i])
  print(holder[holder$Age == plotting_ages[i],"2019"] - 1)
}

######### GET INITIAL POPULATION DISTRIBUTION BY 5-YEAR INTERVAL. 
# LET INITIAL POPULATION BE 20-24 YEAR OLDS ...
start_years = seq(1950,2015,5)
end_years = seq(1954,2019,5)
pop0 = matrix(NA,nrow=1,ncol=14)
for(t in 1:ncol(pop0)){
  pop0[1,t] = sum(rowSums(us_pop_data[us_pop_data$Year %in% seq(start_years[t],end_years[t],1),
                                      as.character(seq(20,24,1))]))
}
pop0 = pop0 / pop0[1,1]

# WRITE OUT TO TXT FOR READ IN TO NUMERICAL MODEL #
#write.csv(pop0,file="~/Dropbox/US_HEALTH_MACRO/Data_Work/pop0.csv",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
write(pop0,paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/pop0.txt"),ncolumns=1)
write(pop0,paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/pop0.txt"),ncolumns=1)
write(pop0,paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/pop0.txt"),ncolumns=1)
write(pop0,paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/pop0.txt"),ncolumns=1)
write(pop0,paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/pop0.txt"),ncolumns=1)
write(pop0,paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/pop0.txt"),ncolumns=1)

print('Computing 5-year gross growth rates for new entrants:')
pop0_growth_5_years = vector(length=length(end_years)-1)
for(t in 2:length(end_years)){
  pop0_growth_5_years[t-1] = pop0[t] / pop0[t-1]
}
print("New entrant population growth 1990-2019:")
print((pop0[14]/pop0[9])^(1/(14-9+1)))
write((pop0[14]/pop0[9])^(1/(14-9+1)),paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/gn.txt"),ncolumns=1)
write((pop0[14]/pop0[9])^(1/(14-9+1)),paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/gn.txt"),ncolumns=1)
write((pop0[14]/pop0[9])^(1/(14-9+1)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/gn.txt"),ncolumns=1)
write((pop0[14]/pop0[9])^(1/(14-9+1)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/gn.txt"),ncolumns=1)
write((pop0[14]/pop0[9])^(1/(14-9+1)),paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/gn.txt"),ncolumns=1)
write((pop0[14]/pop0[9])^(1/(14-9+1)),paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/gn.txt"),ncolumns=1)


# ratio
holder <- as.data.frame(list(year=non_health_prices$YEAR,health_price=agg_prices$Health,other_consumption_price=non_health_price_index))
holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO = holder$health_price / holder$other_consumption_price

# write the relative price data to disk
holder = holder[holder$year %in% seq(1960,2015,5),]
holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO = holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO / holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO[1]
#write.csv(t(holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO),file="~/Dropbox/US_HEALTH_MACRO/Data_Work/q_price_data.csv",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
write(holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO,paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/p_price_data.txt"),ncolumns=1)
write(holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO,paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/p_price_data.txt"),ncolumns=1)
write(holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO,paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/p_price_data.txt"),ncolumns=1)
write(holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO,paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/p_price_data.txt"),ncolumns=1)
write(holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO,paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/p_price_data.txt"),ncolumns=1)
write(holder$HEALTH_OTHER_CONSUMPTION_PRICE_RATIO,paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/p_price_data.txt"),ncolumns=1)

####### TFP TO GET A VALUE FOR GROWTH OF A_C ########
tfp <- read_csv(paste0(raw_data_dir,"/tfp.csv")) # Feenstra et al. 2015
g_tfp_annual = (tfp[nrow(tfp),2] / tfp[1,2])^(1/(nrow(tfp)))-1
g_tfp = (1+g_tfp_annual)^5-1
print("g_c, growth in TFP by 5-year average:")
print(g_tfp)
print("g_c, growth in TFP by 1-year average:")
print(g_tfp_annual)
# write to disk for calibration
write(as.vector(unlist(g_tfp)),paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/g_c.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp)),paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/g_c.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/g_c.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/g_c.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp)),paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/g_c.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp)),paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/g_c.txt"),ncolumns=1)

write(as.vector(unlist(g_tfp_annual)),paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/g_c_annual.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp_annual)),paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/g_c_annual.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp_annual)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/g_c_annual.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp_annual)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/g_c_annual.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp_annual)),paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/g_c_annual.txt"),ncolumns=1)
write(as.vector(unlist(g_tfp_annual)),paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/g_c_annual.txt"),ncolumns=1)

######### CAPITAL SHARE OF CAPITAL IN HEALTH CARE ##########
capital <- read_csv(paste0(raw_data_dir,"/bea_fixed_assets_3_1.csv"),skip = 3)
capital <- capital[2:79,]
health_capital = capital[capital$`...2` == "Health and social assistance",]
total_capital = capital[capital$`...2` == "Private fixed assets",]
health_capital_share = health_capital[,3:ncol(capital)] / total_capital[,3:ncol(capital)]
health_capital_share = health_capital_share[2,]
colnames(health_capital_share) = seq(1948,2019,1)
health_capital_share2 = health_capital_share
health_capital_share = health_capital_share[,as.character(seq(1960,2015,5))]

#write.csv(health_capital_share[1,],file="~/Dropbox/US_HEALTH_MACRO/Data_Work/health_capital_share.csv",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
write(as.vector(unlist(health_capital_share)),paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/k_share.txt"),ncolumns=1)
write(as.vector(unlist(health_capital_share)),paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/k_share.txt"),ncolumns=1)
write(as.vector(unlist(health_capital_share)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/k_share.txt"),ncolumns=1)
write(as.vector(unlist(health_capital_share)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/k_share.txt"),ncolumns=1)
write(as.vector(unlist(health_capital_share)),paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/k_share.txt"),ncolumns=1)
write(as.vector(unlist(health_capital_share)),paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/k_share.txt"),ncolumns=1)

# write to text #
print("Share of all capital in health production has increased by x pct. points from 1948-2019")
print((health_capital_share2[,"2019"]-health_capital_share2[,"1948"] )* 100)


### FIGURE B.4(A) ###
# plot
png(filename=paste(results_dir,"/figureB4a.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,4,2,2))
plot(seq(1948,2019,1),health_capital_share2,type="n",ann=FALSE,ylab=NA,xlab=NA) #,xaxt="n")
grid()
lines(seq(1948,2019,1),health_capital_share2,col='black',lty=1,lwd=2)
#axis(1, at=seq(1950, 2020, 10), las=0.5)
title(ylab=expression(paste("Share of All Capital")),cex.lab=1)
#legend(x = "topright",inset = 0,
#       legend = years_to_subset,lty=1,lwd=2,col=cols,cex=0.7)
#       lty=c(1,3,2),
#       col=c("black","red","blue"), lwd=2, cex=0.7)
dev.off()

######### LABOR SHARE OF LABOR IN HEALTH CARE ##########
laborB <- read_csv(paste0(raw_data_dir,"/nipa_table_6_5B.csv"),skip = 4)
laborC <- read_csv(paste0(raw_data_dir,"/nipa_table_6_5C.csv"),skip = 4)
laborD <- read_csv(paste0(raw_data_dir,"/nipa_table_6_5D.csv"),skip = 4)
laborB <- laborB[2:88,]
laborC <- laborC[2:88,]
laborD <- laborD[2:98,]
# now get the labor shares for health care for each epoch of the industry definitions
# 1948-1987
labor_shareB = as.numeric(laborB[laborB$`...2` == "Health services",3:ncol(laborB)]) / as.numeric(laborB[1,3:ncol(laborB)])
# 1987-2000
labor_shareC = as.numeric(laborC[laborC$`...2` == "Health services",4:ncol(laborC)]) / as.numeric(laborC[1,4:ncol(laborC)])
# 2001-2019
labor_shareD = (as.numeric(laborD[laborD$`...2` == "Ambulatory health care services",6:ncol(laborD)]) +
                  as.numeric(laborD[laborD$`...2` == "Hospitals",6:ncol(laborD)])+
                  as.numeric(laborD[laborD$`...2` == "Nursing and residential care facilities",6:ncol(laborD)]) )/ as.numeric(laborD[1,6:ncol(laborD)])
labor_share_total = as.data.frame(list(year=seq(1948,2019,1),
                                       labor_share = c(labor_shareB,labor_shareC,labor_shareD)))
labor_share_total2 = labor_share_total
labor_share_total = labor_share_total[labor_share_total$year %in% seq(1960,2015,5),"labor_share"]
#write.csv(t(labor_share_total[,2]),file="~/Dropbox/US_HEALTH_MACRO/Data_Work/health_labor_share.csv",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=",")
write(as.vector(unlist(labor_share_total)),paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/l_share.txt"),ncolumns=1)
write(as.vector(unlist(labor_share_total)),paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/l_share.txt"),ncolumns=1)
write(as.vector(unlist(labor_share_total)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/l_share.txt"),ncolumns=1)
write(as.vector(unlist(labor_share_total)),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/l_share.txt"),ncolumns=1)
write(as.vector(unlist(labor_share_total)),paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/l_share.txt"),ncolumns=1)
write(as.vector(unlist(labor_share_total)),paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/l_share.txt"),ncolumns=1)

# write to text #
print("Share of all labor in health production has increased by x pct. points from 1948-2019")
print((labor_share_total2[labor_share_total2$year == 2019,"labor_share"]-labor_share_total2[labor_share_total2$year == 1948,"labor_share"] )* 100)

# LABOR TOTAL HEALTH
# 1948-1987
labor_healthB = as.numeric(laborB[laborB$`...2` == "Health services",3:ncol(laborB)]) #/ as.numeric(laborB[1,3:ncol(laborB)])
# 1987-2000
labor_healthC = as.numeric(laborC[laborC$`...2` == "Health services",4:ncol(laborC)]) #/ as.numeric(laborC[1,4:ncol(laborC)])
# 2001-2019
labor_healthD = (as.numeric(laborD[laborD$`...2` == "Ambulatory health care services",6:ncol(laborD)]) +
                   as.numeric(laborD[laborD$`...2` == "Hospitals",6:ncol(laborD)])+
                   as.numeric(laborD[laborD$`...2` == "Nursing and residential care facilities",6:ncol(laborD)]) ) #/ as.numeric(laborD[1,6:ncol(laborD)])
labor_health_total = as.data.frame(list(year=seq(1948,2019,1),
                                        labor_health = c(labor_healthB,labor_healthC,labor_healthD)))
labor_health_total2 = labor_health_total
#labor_health_total = labor_health_total[labor_health_total$year %in% seq(1950,2015,5),]

### FIGURE B.4(b) ###
# plot
png(filename=paste(results_dir,"/figureB4b.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,4,2,2))
plot(labor_share_total2$year,labor_share_total2$labor_share,type="n",ann=FALSE,ylab=NA,xlab=NA) #,xaxt="n")
grid()
lines(labor_share_total2$year,labor_share_total2$labor_share,col='black',lty=1,lwd=2)
#axis(1, at=seq(1950, 2020, 10), las=0.5)
title(ylab=expression(paste("Share of All Labor")),cex.lab=1)
#legend(x = "topright",inset = 0,
#       legend = years_to_subset,lty=1,lwd=2,col=cols,cex=0.7)
#       lty=c(1,3,2),
#       col=c("black","red","blue"), lwd=2, cex=0.7)
dev.off()


################################################################################
#
#     Check wage differentials from health sector to broader economy (avg.)
#             ROBUSTNESS FOR APPENDIX
#
################################################################################

# read in wage data
total_priv_wages <- read_csv(paste0(raw_data_dir,"/total_priv_wages.csv"))
educ_health_wages <- read_csv(paste0(raw_data_dir,"/educ_health_wages.csv"))

# merge to one dataset #
educ_health_wages = educ_health_wages[,c("date","value")]
names(educ_health_wages)[names(educ_health_wages) == "value"] <- "educ_health_w"
total_priv_wages = total_priv_wages[,c("date","value")]
names(total_priv_wages)[names(total_priv_wages) == "value"] <- "total_priv_w"
sector_wages = merge(total_priv_wages,educ_health_wages,by="date")

### FIGURE B.6 ###
# plot them together
ymin = min(sector_wages[,2:3],na.rm = TRUE)
ymax = max(sector_wages[,2:3],na.rm = TRUE)
png(filename=paste(results_dir,"/figureB6.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,5,2,2))
with(na.omit(sector_wages),plot(date,total_priv_w,type="n",ann=FALSE,ylim=c(ymin,ymax),ylab=NA,xlab=NA)) #,xaxt="n"))
grid()
with(na.omit(sector_wages),lines(date,total_priv_w,col="red",lty=2,lwd=2))
with(na.omit(sector_wages),lines(date,educ_health_w,col="blue",lty=3,lwd=2))
#axis(2, at=seq(2006, 2024, 3), las=1)
title(ylab=expression(paste("Wages")),cex.lab=1)
legend(x = "topleft",inset = 0,
       legend = c("Total Private","Educ & Health"),
       lty=c(2,3),
       col=c("red","blue"), lwd=2, cex=0.7)
dev.off()



############################################# SECTION I ###################################################### 
############################################# AND APPENDIX C #################################################

############################### THIS IS THE STUFF IN SECTION I ###############################

####################################################################
#
#       g_mu identification taking A_ct, alpha_c, and alpha_h
#       as given ...
#
#       Identifying g_mu and g_Ah given alpha_c and alpha_h + 
#       data.
#
####################################################################

# ASSUME alpha_c = 0.4
alpha_c = 0.4
# ASSUME A_C grows at the TFP rate from FRED (Penn World Tables 10.01, see Feenstra et al. 2015)
tfp$growth = NA
for(t in 2:nrow(tfp)){
  tfp$growth[t] = tfp$RTFPNAUSA632NRUG[t] / tfp$RTFPNAUSA632NRUG[t-1]
}
tfp$year = as.numeric(substr(tfp$DATE,1,4))
# HEALTH SHARE GROWTH
health_share_holder$share_growth = NA
for(t in 2:nrow(health_share_holder)){
  health_share_holder$share_growth[t] = health_share_holder$`Health Share`[t] / health_share_holder$`Health Share`[t-1]
}
# TOTAL SPEND GROWTH
health_share_holder$total_spend_growth = NA
for(t in 2:nrow(health_share_holder)){
  health_share_holder$total_spend_growth[t] = health_share_holder$`Personal consumption expenditures`[t] / health_share_holder$`Personal consumption expenditures`[t-1]
}
# HEALTH LABOR
labor_health_total2$labor_growth = NA
for(t in 2:nrow(labor_health_total2)){
  labor_health_total2$labor_growth[t] = labor_health_total2$labor_health[t] / labor_health_total2$labor_health[t-1]
}
# HEALTH CAPITAL
health_capital_holder = as.data.frame(list(health = unname(unlist(as.vector(health_capital[2,3:ncol(health_capital)]))),
                                           year = seq(1948,2019,1)))
health_capital_holder$capital_growth = NA 
for(t in 2:nrow(health_capital_holder)){
  health_capital_holder$capital_growth[t] = health_capital_holder$health[t] / health_capital_holder$health[t-1]
}

# merge all of the dataframes for mu growth decomposition
holder = merge(merge(
  merge(health_capital_holder[,c("year","capital_growth")],labor_health_total2[,c("year","labor_growth")],by="year"),
  health_share_holder[,c("year","total_spend_growth","share_growth")],by="year"),
  tfp[,c("year","growth")],by="year")
holder = na.omit(holder)
holder[,2:5] = log(holder[,2:5])
##### NOMINAL DECOMPOSITION (DO NOT USE, ONLY FOR ILLUSTRATION AND EXPERIMENTATION) #####
holder$mu_growth = holder$share_growth - holder$growth - 
  alpha_c * holder$capital_growth - (1-alpha_c) * holder$labor_growth + 
  holder$total_spend_growth

########## USING CAPITAL QUANTITY SERIES FOR DEFLATION RETURNS WITH SEPARATE CAPITAL/LABOR SERIES #########

# Steps
# Stage 1
# 1) Build a real capital index with 1954 = 1
# 2) Deflate Capital and Total Expenditure Using the PCE deflator
# 3) Set alpha_c = 0.4 and use TFP from PWT 10.01 along with expenditure share to get mu_growth
# Stage 2
# 4) Now using L/K ratio and assumption alpha_h = 0.26 from Donahoe (2000) get r/w growth rate (G.E. effect)
# Stage 3
# 5) Now with r/w in hand go back to original pricing kernel and get A_h using q (relative price)

###### WE CAN NOW DECOMPOSE THE GROWTH IN q (WE DO EVERYTHING IN TERMS OF 1975 DOLLARS) ########

# start year = 1954
start_year = 1954

##### PLACE EVERYTHING IN BASE YEAR TERMS #####
capital_quantity_indices <- read_csv(paste0(raw_data_dir,"/bea_fixed_assets_3_2_ESI.csv"),skip = 3)
capital_quantity_indices <- capital_quantity_indices[2:79,]
health_capital_quantities <- capital_quantity_indices[capital_quantity_indices$`...2` == "Health and social assistance",]
health_capital_quantities[,3:ncol(capital_quantity_indices)] <- health_capital_quantities[,3:ncol(health_capital_quantities)] / 
  health_capital_quantities$`1975`[1]
holder2 = as.data.frame(list(year = seq(1947,2021,1),health_capital_quantities=unname(unlist(as.vector(health_capital_quantities[1,3:ncol(health_capital_quantities)])))))
health_capital_holder <- merge(health_capital_holder,holder2,by="year")

##### GET HEALTH CAPITAL IN TERMS OF REAL 1975 DOLLARS (FOR COMPARISON WITH H-S) #####
health_capital_holder$real = health_capital_holder$health_capital_quantities * health_capital_holder[health_capital_holder$year==1975,"health"]

##### NOW MERGE BUT INCLUDE AN AGG_PRICE INDEX, THAT IS INDEXED TO 1975 ####
agg_prices$year = agg_prices$variable
holder = merge(merge(merge(
  merge(health_capital_holder[,c("year","real")],labor_health_total2[,c("year","labor_growth","labor_health")],by="year"),
  health_share_holder[,c("year","Health Share","Personal consumption expenditures")],by="year"),
  tfp[,c("year","growth")],by="year"),agg_prices[,c("year","Personal consumption expenditures")],by="year")
holder$`Personal consumption expenditures.y` = holder$`Personal consumption expenditures.y` / holder[holder$year == 1975,"Personal consumption expenditures.y"]
holder$total_spend_deflate = holder$`Personal consumption expenditures.x` / holder$`Personal consumption expenditures.y`
holder$health_capital_deflate = holder$real / holder$`Personal consumption expenditures.y`

##### NOW COMPUTE THE REAL VALUES #####
holder$health_capital_growth = NA
holder$total_spend_growth = NA
holder$health_share_growth = NA
for(t in 2:nrow(holder)){
  holder$health_capital_growth[t] = holder$health_capital_deflate[t] / holder$health_capital_deflate[t-1]
  holder$total_spend_growth[t] = holder$total_spend_deflate[t] / holder$total_spend_deflate[t-1]
  holder$health_share_growth[t] = holder$`Health Share`[t] / holder$`Health Share`[t-1]
}

##### BACKING OUT GROWTH RATE IN MARKUPS #####
# This is ...
# \begin{align}
# \widetilde{g}_{\sigma_H,t} = \widetilde{g}_{\mu,t} + \widetilde{g}_{A_c,t} + \alpha_c \widetilde{g}_{K_h,t} + (1-\alpha_c)\widetilde{g}_{L_h,t} -  \widetilde{g}_{X,t} \label{id3}
# \end{align}
# where we express the share of total spending which goes to health spending as a function of agg TFP, and other measurable variables

#holder2 = holder
holder = na.omit(holder)
#holder[,2:5] = log(holder[,2:5])
holder$mu_growth = exp(log(holder$health_share_growth) - log(holder$growth) - 
                         alpha_c * log(holder$health_capital_growth) - (1-alpha_c) * log(holder$labor_growth) + 
                         log(holder$total_spend_growth))
# store
#### SUPPOSE WE START AT mu = 1 IN 1954 ... WHERE WOULD MU GO? ####
holder$mu = NA
#first year in holder is 1955
holder$mu[1] = holder$mu_growth[1]
for(t in 2:nrow(holder)){
  holder$mu[t] = holder$mu_growth[t] * holder$mu[t-1]
}
original_markup_const_alpha_c = holder

###### STAGE 2 ... NOW, KNOWING alpha_h K_h AND L_h IDENTIFIES A_h growth FROM (8) 
### NOTE WE NEED JUST RELATIVE PRICES, i.e. q FROM THE MODEL (NOW p in the current draft) ####
alpha_h = 0.26 #donahoe
holder3 <- as.data.frame(list(year=non_health_prices$YEAR,health_price=agg_prices$Health,other_consumption_price=non_health_price_index))
# normalize prices to 1975
holder3$other_consumption_price = holder3$other_consumption_price / holder3[holder3$year == 1975,"other_consumption_price"]
holder3$health_price = holder3$health_price / holder3[holder3$year == 1975,"health_price"]
holder3$q <- holder3$health_price / holder3$other_consumption_price
holder3$q_growth = NA
for(t in 2:nrow(holder3)){
  holder3$q_growth[t] = holder3$q[t] / holder3$q[t-1]
}
holder = merge(holder3[,c("year","q_growth")],holder,by="year")

# \begin{align}
# \widetilde{g}_{q,t} = \widetilde{g}_{\mu,t} + (\alpha_h - \alpha_c) \big(\widetilde{g}_{r,t} - \widetilde{g}_{w,t}\big) + \widetilde{g}_{A_c,t} - \widetilde{g}_{A_h,t} \label{id1}
# \end{align}
holder$A_h_growth = exp(
  log(holder$mu_growth) + (alpha_h - alpha_c) * (log(holder$labor_growth) - log(holder$health_capital_growth)) - log(holder$q_growth) + log(holder$growth)
)

##### print geometric mean annual growth rate of A_h #####
print("Model Period TFP Growth Rates:")
print("First year:")
print(holder2$year[1])
print("Last year:")
print(holder2$year[nrow(holder2)])
print("Geometric mean A_h gross growth rate from model + data, but not solving the G.E.:")
print(geoMean(holder$A_h_growth))
print("Geometric mean A_c gross growth rate from Penn World Tables TFP data:")
print(geoMean(holder$growth))
print("5-YEAR GROWTH RATES FOR G.E. CALIBRATION:")
print("Geometric mean A_h gross growth rate from model + data, but not solving the G.E.:")
print(geoMean(holder$A_h_growth)^5)
print("Geometric mean A_c gross growth rate from Penn World Tables TFP data:")
print(geoMean(holder$growth)^5)

# save original A_h_growth for later plotting ...
A_h_growth_original = holder$A_h_growth
mu_growth_original = holder$mu_growth

# write the g_H for TFP for health sector to disk for calibration
write(geoMean(holder$A_h_growth)^5-1,paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/g_h.txt"),ncolumns=1)
write(geoMean(holder$A_h_growth)^5-1,paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/g_h.txt"),ncolumns=1)
write(geoMean(holder$A_h_growth)^5-1,paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/g_h.txt"),ncolumns=1)
write(geoMean(holder$A_h_growth)^5-1,paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/g_h.txt"),ncolumns=1)
write(geoMean(holder$A_h_growth)^5-1,paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/g_h.txt"),ncolumns=1)
write(geoMean(holder$A_h_growth)^5-1,paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/g_h.txt"),ncolumns=1)

### 
# get 5-year on 5-year growth rate for A_h #
A_h = vector(mode="numeric",length=(nrow(holder)+1))
A_h[1] = 1
for(t in 2:length(A_h)){
  A_h[t] = A_h_growth_original[t-1] * A_h[t-1]
}
A_h = as.data.frame(list(
  year=seq(min(holder$year)-1,max(holder$year),1),
  A_h=A_h
))
A_h = A_h[A_h$year %in% seq(1955,2015,5),]
A_h$growth = NA
for(t in 2:nrow(A_h)){
  A_h$growth[t] = A_h$A_h[t] / A_h$A_h[t-1]
}

write(A_h[2:nrow(A_h),"growth"],paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/g_h_rates.txt"),ncolumns=1)
write(A_h[2:nrow(A_h),"growth"],paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/g_h_rates.txt"),ncolumns=1)
write(A_h[2:nrow(A_h),"growth"],paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/g_h_rates.txt"),ncolumns=1)
write(A_h[2:nrow(A_h),"growth"],paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/g_h_rates.txt"),ncolumns=1)
write(A_h[2:nrow(A_h),"growth"],paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/g_h_rates.txt"),ncolumns=1)
write(A_h[2:nrow(A_h),"growth"],paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/g_h_rates.txt"),ncolumns=1)

###

####################################################################################
############################### VARIABLE LABOR SHARE ###############################

# labor share
### Penn World Tables 10.0.1 ###
# fredr(series_id = 'LABSHPUSA156NRUG') 
labor_share <- read_csv(paste0(raw_data_dir,"/labor_share.csv"))

### plot the labor share ###
labor_share$year = as.numeric(substr(as.character(labor_share$date),1,4))

### FIGURE B.5 ###
# plot
png(filename=paste(results_dir,"/figureB5.png",sep=""),width=5,height=4,units="in",res=300)
par(mar = c(2,4,2,2))
with(labor_share,plot(year,value,type="n",ann=FALSE,ylab=NA,xlab=NA) ) #,xaxt="n")
grid()
with(labor_share,lines(year,value,col='black',lty=1,lwd=2))
#axis(1, at=seq(1950, 2020, 10), las=0.5)
title(ylab=expression(paste("1 - ",alpha[ct],sep="")),cex.lab=1)
#legend(x = "topright",inset = 0,
#       legend = years_to_subset,lty=1,lwd=2,col=cols,cex=0.7)
#       lty=c(1,3,2),
#       col=c("black","red","blue"), lwd=2, cex=0.7)
dev.off()

##### deflated price growth #####
print("Price growth (relative):")
print(geoMean(holder$q_growth))

##### MARKUP growth #####
print("Markup growth:")
print(geoMean(holder$mu_growth))

########### NOW PLOT THE DECOMPOSITION #############
# make holder ge_effect_total which includes the alpha_h - alpha_c weight
holder$l_ge_effect_growth_total = (alpha_h - alpha_c) * (log(holder$labor_growth) - log(holder$health_capital_growth))
holder$l_neg_A_h_growth = - log(holder$A_h_growth)
holder$l_q_growth = log(holder[,"q_growth"])
holder$l_mu_growth = log(holder$mu_growth)
holder$l_growth = log(holder$growth)
holder3 = holder[, grep("^l_", names(holder))]
holder3$year = holder$year
print("Make sure all of the following are really small (decomp works!):")
print(holder$l_q_growth - holder$l_mu_growth - holder$l_ge_effect_growth_total - holder$l_growth - holder$l_neg_A_h_growth)


##### NOW NORMALIZE ALL OF THE VARIABLES TO 0 IN THE FIRST YEAR (1954) AND CONTINUE #####
holder4 = rbind(c(0,0,0,0,0,1954),holder3)
for(t in 2:nrow(holder4)){
  holder4[t,1:5] = holder4[t,1:5] + holder4[t-1,1:5]
}
### exponentiate, renormalize to 1975 then take logs again ###
holder4[,1:5] = exp(holder4[,1:5])
for(j in 1:5){
  holder4[,j] = holder4[,j]/holder4[holder4$year == 1975,j]
}
holder4[,1:5] = log(holder4[,1:5])
print("Make sure all of the following are really small (decomp works after new 1975 normalization!):")
print(holder$l_q_growth - holder$l_mu_growth - holder$l_ge_effect_growth_total - holder$l_growth - holder$l_neg_A_h_growth)

########### FIGURE 3 ###########
# Open a PNG device (optional)
#png(paste0(results_dir,"/figure3.png"), width = 2000, height = 2000, res = 300)
pdf(paste0(results_dir,"/figure3.pdf"), width = 7, height = 7) 

# Define a 2x2 layout with custom widths and heights
layout_matrix <- matrix(c(1, 2,
                          3, 4), nrow = 2, byrow = TRUE)
# Set up the layout with different relative sizes
layout(layout_matrix, widths = c(1, 1), heights = c(1, 1))
# Add borders to visualize plot boundaries
par(mar = c(4, 4, 2, 1))  # margin setup

#### FIGURE 3A ####
##### CUMULATIVE YEAR ON YEAR GROWTH RATES #####
#bound_hold = c(min(holder4[,1:5]),
#               max(holder4[,1:5])) # hold for later H-S plot (same bounds)
# in order p, GE, mu, A
cols=c("black","blue","red","purple")
#png(filename=paste("../Draft/price_growth_decomp_cumulative.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(holder4,plot(year,l_q_growth,type="n",ann=FALSE,ylim=bound_hold,ylab=NA,xlab=NA)) #,xaxt="n"))
grid()
with(holder4,lines(year,l_q_growth,col=cols[1],lty=1,lwd=2))
with(holder4,lines(year,l_ge_effect_growth_total,col=cols[2],lty=2,lwd=2))
#with(holder4,lines(year,l_mu_growth,col=cols[3],lty=3,lwd=2))
with(holder4,lines(year,l_mu_growth,col=cols[3],lty=3,lwd=4))
with(holder4,lines(year,(l_growth+l_neg_A_h_growth),col=cols[4],lty=4,lwd=2))
#with(holder4,lines(year,l_neg_A_h_growth,col=cols[5],lty=5,lwd=2))
abline(h=0,lwd=1.1,lty=1,col="black")
axis(1, at=seq(1950, 2020, 20), las=1)
title(ylab=expression(paste("Cumulative Growth")),xlab=expression(paste("(a) Baseline, Constant ",alpha[c],sep="")),cex.lab=0.9)
legend(x = "topleft",inset = 0,
       legend = c("p","GE",expression(paste(mu)),expression(paste(A))), #expression(paste(A[c])),expression(paste(A[h]))),
       lty=seq(1,4,1),
       col=cols[1:4], lwd=c(2,2,4,2), cex=0.55)
#dev.off()


##### PLOT mu_t ALONG WITH HORENSTEIN AND SANTOS 2019 #####
# See Horenstein and Santos 2019 Table 5
hs_years_mu = seq(1975,2005,5)
hs_mu = c(0.97,1,1.07,1.38,1.24,1.56,1.68) # this is an index to unity in 1980 but we change it to be 1975
# no more growth in mark-ups
#Tend = (2050-1950+5)/5
#mu = c(mu,rep(mu[length(mu)],Tend-length(mu)))

#year = seq(1950,2050,5)
mu_horenstein = as.data.frame(list(year=hs_years_mu,mu=hs_mu))
#mu_horenstein = mu_horenstein[mu_horenstein$year < 2020,]

# redo later for time varying alpha
mu_estimated = as.data.frame(list(year=holder4$year,mu=exp(holder4$l_mu_growth)))
mu_estimated = merge(mu_estimated,mu_horenstein,by="year",all.x=TRUE)

############### GET THE MU'S, USING H-S 2019 LEVELS FROM TABLE 5 AND THE GROWTH RATE AS ESTIMATED FROM THE PRODUCTION DATA #################
# normalize mu_estimated = 1 in 1975
mu_estimated$normalized = mu_estimated$mu.x / mu_estimated[mu_estimated$year == 1975,"mu.x"] # normalize to 1975 (first year)
# extrapolate back to 1950
mu_estimated[mu_estimated$year %in% seq(1955,1970,5),"mu.y"] = mu_estimated[mu_estimated$year == 1975,"mu.y"] * mu_estimated[mu_estimated$year %in% seq(1950,1970,5),"normalized"]
# interpolate forward to 2015
mu_estimated[mu_estimated$year %in% seq(1975,2015,5),"mu.y"] = mu_estimated[mu_estimated$year == 1975,"mu.y"] * mu_estimated[mu_estimated$year %in% seq(1975,2015,5),"normalized"]

print("mu's for the calibrator. These mu's have the 1975 level of relative markups from Table 5 of Horenstein and Santos, 2019, but grow at the rate estimated from the Exact Identifiation exercise.")
print("Years, 1955 - 2015 in 5 year intervals.")
print("Mu (1955-2015):")
print(mu_estimated[mu_estimated$year %in% seq(1955,2015,5),"mu.y"])
print("EVERYTHING WILL BE ASSUMED CONSTANT AFTER 2015 IN THE MAIN MODEL!")

### for the extended growth model, do avg. growth 1955 to 2015 ###
print("Average markup growth (gross), 1955-2015 annually:")
print((mu_estimated[mu_estimated$year ==2015,"mu.y"] / mu_estimated[mu_estimated$year ==1955,"mu.y"])^(1/(2015-1955+1)))
print("Now suppose we extrapolate out to 2100 at this growth rate ... what do we get?")
gross_mu_growth = (mu_estimated[mu_estimated$year ==2015,"mu.y"] / mu_estimated[mu_estimated$year ==1955,"mu.y"])^(1/(2015-1955+1))
mu_estimated_extrap = mu_estimated
all_years <- data.frame(year = 1954:2100)
mu_estimated_extrap <- merge(all_years, mu_estimated_extrap, by = "year", all.x = TRUE)
for(t in 2016:2100){
  mu_estimated_extrap[mu_estimated_extrap$year == t,"mu.y"] = mu_estimated_extrap[mu_estimated_extrap$year == t-1,"mu.y"] * gross_mu_growth
}

# write 1955 to 2015 to disk
print("Markup in 1975 from H-S is approx. 1.5 ...")
write(1.5 * as.vector(mu_estimated[mu_estimated$year %in% seq(1955,2015,5),"mu.y"]),paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/mu.txt"),ncolumns=1)
#write(1.5 * as.vector(mu_estimated[mu_estimated$year %in% seq(1955,2015,5),"mu.y"]),"~/Dropbox/US_HEALTH_MACRO/Data_Work/GE_Calibration_Variable_Alpha_c/mu.txt",ncolumns=1)

# output the extended markups
write(1.5 * as.vector(mu_estimated_extrap[mu_estimated_extrap$year %in% seq(1955,2100,5),"mu.y"]),paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/mu.txt"),ncolumns=1)

# output to no mu growth calibration
write(rep(1.5 * mu_estimated[mu_estimated$year ==1955,"mu.y"],length(as.vector(mu_estimated[mu_estimated$year %in% seq(1955,2015,5),"mu.y"]))),
      paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/mu.txt"),ncolumns=1)

# make H-S estimates go back to 1955 like ours
hs_mu_NORMALIZED = hs_mu / hs_mu[1]
holder_HS = as.data.frame(list(
                          year = seq(1955,2015,5),
                          mu = c(NA,NA,NA,NA,hs_mu_NORMALIZED,NA,NA)))
print("Horenstein-Santos 5-year markup growth rates, 1975-2005:")
print((holder_HS[holder_HS$year==2005,"mu"] / holder_HS[holder_HS$year==1975,"mu"])^(1/7)-1)
# extrap back and forward
growth_back = (holder_HS[holder_HS$year==2005,"mu"] / holder_HS[holder_HS$year==1975,"mu"])^(1/7)
for(t in seq(1970,1955,-5)){
  holder_HS[holder_HS$year == t,"mu"] = holder_HS[holder_HS$year == t+5,"mu"] / growth_back
}
# extrap forward
for(t in seq(2010,2015,5)){
  holder_HS[holder_HS$year == t,"mu"] = holder_HS[holder_HS$year == t-5,"mu"] * growth_back
}
# these output to the 2 H-S simulations
print("The H-S Markups we use take the change rates as posted in Table 5 and multiply by the 1975 1.5 level from Figure 7.b (their paper).")
write(1.5 * as.vector(holder_HS[holder_HS$year %in% seq(1955,2015,5),"mu"]),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Sim_Only/mu.txt"),ncolumns=1)
write(1.5 * as.vector(holder_HS[holder_HS$year %in% seq(1955,2015,5),"mu"]),paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/mu.txt"),ncolumns=1)

################################################## APPENDIX C.2 #########################################################
################################################################################
#
#   Now attempt to identify alpha_h given MFPs from the data 
#     (estimates of g_mu are the same in this exercise)
#
#   Also see MFP measures from https://www.bls.gov/mfp/.
#
#   In this exercise we are identifying alpha_h and g_mu given A_h from lit. 
#   Note that there are significant variations in A_h from the lit, so we consider
#     several different simulations on A_h to back out mu and alpha_h, holding
#     alpha_c fixed in each one.
#     
################################################################################

# vector of productivity growth estimates, and we will assume that g_Ah is constant
g_Ah_constant = c(0.0,0.001,0.004,-0.003,-0.006,0.01)
# first is 0 (a standard baseline from the literature, including Triplett and Bosworth (verify))
# second is 0.1% (svcs. MFP 1987-2018 from Shatto and Clemens 2022 equivalent to hospital MFP low estimates from Cylus and Dickensheets 2007-2008)
# third is 0.4% (Shatto and Clemens 2022 and Spitalnic et al. 2022 estimate of hospital MFP)
# fourth is -0.3% (Shatto and Clemens 2022, ambulatory health svcs. decline from 1987-2018)
# fifth is -0.6% (Shatto and Clemens 2022, nursing and residential care facilities decline from 1987-2018)
# sixth is 1% (Cylus and Dickensheets 2007-2008 method 2 (Table 1) estimate for hospital MFP using labor quantity data for total hospital employees)
# NOTE: the degree to which the different sub-sectors have contributed to health-care svcs.
# ... productivity growth obviously depends on their shares in the health-services basket, which
# ... are difficult to estimate. 

##### MAKE LAGGED VARIABLES FOR IVS IN HOLDER #####
holder$q_growth_LAG = c(NA,holder$q_growth[2:nrow(holder)])
holder$mu_growth_LAG = c(NA,holder$mu_growth[2:nrow(holder)])
holder$ge_effect_growth_LAG = c(NA,holder$labor_growth[2:nrow(holder)]/holder$health_capital_growth[2:nrow(holder)])
holder$growth_LAG = c(NA,holder$growth[2:nrow(holder)])
holder5 = na.omit(holder)
print("Years for regressions:")
print(holder$year)

################# OLS THEN IV WITH LAGGED GROWTH RATES ###############
#### NOTE THAT ESTIMATES OF g_mu FROM THE FIRST STAGE ARE THE SAME IN THIS EXERCISE ####
# THUS, WE ONLY REPEAT STAGE 3, ASSUMING alpha_h IS UNKNOWN, AND ALLOWING alpha_h TO BE A MINIMUM VARIANCE ESTIMATOR #
# ... (i.e., OLS with No Intercept) #
alpha_h_ident_regs_OLS = list()
alpha_h_ident_regs_2SLS = list()
for(n in 1:length(g_Ah_constant)){
  
  # OLS 
  temp = data.frame(
    y_temp = log(holder$q_growth) - log(holder$mu_growth)+ alpha_c * log(holder$ge_effect_growth) - log(holder$growth) + g_Ah_constant[n],
    x_temp = log(holder$ge_effect_growth))
  alpha_h_ident_regs_OLS[[n]] = lm(y_temp ~ 0 + x_temp,data=temp)
  print("Identifying alpha_h regression summary (OLS)!")
  print("g_AH assumed:")
  print(g_Ah_constant[n])
  print(summary(alpha_h_ident_regs_OLS[[n]]))
  print("alpha_h:")
  print(unname(alpha_h_ident_regs_OLS[[n]]$coefficients[1]))
  
  # IV REG (2SLS WITH LAGS, SO NOW STARTS IN 1956
  holder5$y_temp = log(holder5$q_growth) - log(holder5$mu_growth)+ alpha_c * log(holder5$ge_effect_growth) - log(holder5$growth) + g_Ah_constant[n]
  holder5$x_temp = log(holder5$ge_effect_growth)
  alpha_h_ident_regs_2SLS[[n]] = ivreg(y_temp ~ 0 + x_temp | q_growth_LAG + mu_growth_LAG + ge_effect_growth_LAG + growth_LAG,data=holder5,)
  print("Identifying alpha_h regression summary (2SLS)!")
  print("g_AH assumed:")
  print(g_Ah_constant[n])
  print(summary(alpha_h_ident_regs_2SLS[[n]]))
  print("alpha_h:")
  print(unname(alpha_h_ident_regs_2SLS[[n]]$coefficients[1]))
}

########## REPORT ESTIMATES OF alpha_h CONDITIONAL UPON g_AH and 1-SD ERROR BARS ##########
holder6 = as.data.frame(list(
  g_AH = g_Ah_constant,
  alpha_h_OLS = unname(unlist(lapply(alpha_h_ident_regs_OLS,function(x){x$coefficients[1]}))),
  se_OLS = unname(unlist(lapply(alpha_h_ident_regs_OLS,function(x){summary(x)$coefficients[1,2]}))),
  alpha_h_IV = unname(unlist(lapply(alpha_h_ident_regs_2SLS,function(x){x$coefficients[1]}))),
  se_IV = unname(unlist(lapply(alpha_h_ident_regs_2SLS,function(x){summary(x)$coefficients[1,2]}))),
  R_squared_IV = unname(unlist(lapply(alpha_h_ident_regs_2SLS,function(x){summary(x)$adj.r.squared})))
))
print("Robustness Check, 2SLS IV Regression for Values of alpha_h from Growth Accounting Exercise:")
print("As of November 2024, this is Appendix B.3.")
print(holder6)

holder_CHECK = holder


# rounding function for tabular outputs
# round to 3 and make characters
format_round_3 <- function(x) {
  format(round(x, 3), nsmall = 3)
}

################### output Table C.1 using xtable ####################
output_matrix = t(holder6)
# only IV regs
output_matrix = unname(output_matrix[c(1,4:6),])
output_matrix <- format_round_3(output_matrix)
output_matrix[3,] <- sapply(output_matrix[3,],function(x){str_c("(",str_trim(x),")")})


# Row names with LaTeX math
output_names <- c(
  "$\\widetilde{g}_{A_h}$",
  "$\\widehat{\\alpha}_h$",
  "",
  "$R^2$"
)
output_matrix <- cbind(output_names,output_matrix)
colnames(output_matrix) <- NULL
# Create xtable
xtab <- xtable(output_matrix,
               caption = "2SLS Regressions of (C.6) with One-period Lagged IV’s",
               label = "tab:2sls")
# Build add.to.row list: replace header with custom lines, add hline after row 1
add_lines <- list()
add_lines$pos <- list(1,3)
add_lines$command <- c(
  "\\hline \n",  # hline after first data row
  "\\hline \n"
)

# Capture the LaTeX output as text
latex_lines <- capture.output(
  print(xtab,
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = identity,
        add.to.row = add_lines,
        caption.placement = "top",
        floating = TRUE)
)

# Replace the default tabular format with your custom format
latex_lines <- sub(
  pattern = "^\\\\begin\\{tabular\\}\\{.*\\}$",
  replacement = "\\\\begin{tabular}{@{\\\\extracolsep{10pt}} lcccccc}",
  latex_lines
)

# Write to .tex file
writeLines(latex_lines, paste0(results_dir,"/tableC1.tex"))

#stargazer(output_matrix,type = "latex",summary=FALSE,title="2SLS Regressions of (C.6) with One-period Lagged IV’s",sanitize.text.function = identity)


# L_h = full-time equivalent employees (1000's)
# K_h = billions of dollars

rm(holder)
##### NOW MERGE BUT INCLUDE AN AGG_PRICE INDEX, THAT IS INDEXED TO 1975 ####
holder = 
  merge(
    merge(
      merge(
        merge(health_capital_holder[,c("year","real")],labor_health_total2[,c("year","labor_growth","labor_health")],by="year"),
  health_share_holder[,c("year","Health Share","Personal consumption expenditures")],by="year"),
  tfp[,c("year","growth")],by="year"),agg_prices[,c("year","Personal consumption expenditures")],by="year")
holder$`Personal consumption expenditures.y` = holder$`Personal consumption expenditures.y` / holder[holder$year == 1975,"Personal consumption expenditures.y"]
holder$total_spend_deflate = holder$`Personal consumption expenditures.x` / holder$`Personal consumption expenditures.y`
holder$health_capital_deflate = holder$real / holder$`Personal consumption expenditures.y`
holder$L_h_K_h_deflate = holder$labor_health / holder$health_capital_deflate

##### NOW COMPUTE THE REAL VALUES #####
holder$health_capital_growth = NA
holder$total_spend_growth = NA
holder$health_share_growth = NA
for(t in 2:nrow(holder)){
  holder$health_capital_growth[t] = holder$health_capital_deflate[t] / holder$health_capital_deflate[t-1]
  holder$total_spend_growth[t] = holder$total_spend_deflate[t] / holder$total_spend_deflate[t-1]
  holder$health_share_growth[t] = holder$`Health Share`[t] / holder$`Health Share`[t-1]
}

#### back to labor_share data frame ... ####
# add labor_share first difference
labor_share$year = as.numeric(substr(as.character(labor_share$date),1,4))
labor_share$labor_share_growth = NA 
for(t in 2:nrow(labor_share)){
  labor_share$labor_share_growth[t] = labor_share$value[t] - labor_share$value[t-1]
}


# log labor share weighted by share
labor_share$log_share_level = log(labor_share$value)
labor_share$alpha_log_share_level = labor_share$value * labor_share$log_share_level
labor_share$capital_share = 1- labor_share$value
labor_share$log_cap_share_level = log(labor_share$capital_share)
labor_share$one_alpha_log_cap_share_level = (1-labor_share$value) * labor_share$log_cap_share_level
# now get the weighted share first differences
labor_share$alpha_log_share_growth = NA
labor_share$one_alpha_log_cap_share_growth = NA
for(t in 2:nrow(labor_share)){
  labor_share$alpha_log_share_growth[t] = labor_share$alpha_log_share_level[t] - labor_share$alpha_log_share_level[t-1]
  labor_share$one_alpha_log_cap_share_growth[t] = labor_share$one_alpha_log_cap_share_level[t] - labor_share$one_alpha_log_cap_share_level[t-1]
}
# get delta alpha_c * log (alpha_h / (1-alpha_h))
labor_share$delta_alpha_log_alpha_h = labor_share$labor_share_growth * log(alpha_h / (1-alpha_h))
# get alpha_c * log(L / K) then take first differences
holder = merge(labor_share,holder,by="year")
holder$alpha_c_log_L_K = holder$value * log(holder$L_h_K_h_deflate)
holder$alpha_c_log_L_K_growth = NA
for(t in 2:nrow(holder)){
  holder$alpha_c_log_L_K_growth[t] = holder$alpha_c_log_L_K[t] - holder$alpha_c_log_L_K[t-1]
}
holder = na.omit(holder)


################################ STAGE 1 #######################################
################## conditional upon variable labor share #######################
print("Computing mu_growth for time varying alpha. This is mu_growth under the assumption that 
the labor share of the C+I (non-health sector) is time-varying according to the PWT. 
NOTE: we cannot identify g_mu without knowledge of alpha_h when alpha_c depends on t, non-trivially.")
holder$mu_growth = exp(log(holder$health_share_growth) - log(holder$growth) + log(holder$total_spend_growth) + 
                         holder$delta_alpha_log_alpha_h - holder$alpha_log_share_growth - holder$one_alpha_log_cap_share_growth - 
                         log(holder$labor_growth) + holder$alpha_c_log_L_K_growth
                         )
mu_growth_time_varying = holder$mu_growth


#### SUPPOSE WE START AT mu = 1 IN 1954 ... WHERE WOULD MU GO? ####
holder$mu = NA
#first year in holder is 1955
holder$mu[1] = holder$mu_growth[1]
for(t in 2:nrow(holder)){
 holder$mu[t] = holder$mu_growth[t] * holder$mu[t-1]
}
markup_non_constant_alpha_c = holder


################################ STAGE 2 #######################################
print("Now computing the Appendix C.1 G.E. effect, which is the same as long as we assume
      alpha_h = 0.26 and use the health-sector MRTS condition.")
###### STAGE 2 ... NOW, KNOWING alpha_h K_h AND L_h IDENTIFIES THE GENERAL EQUILIBRIUM EFFECT ######
# I consider 0.26 D= alpha_h from Donahoe (2000)
alpha_h = 0.26
# real capital per worker
holder2 = holder

holder3 <- as.data.frame(list(year=non_health_prices$YEAR,health_price=agg_prices$Health,other_consumption_price=non_health_price_index))
# normalize prices to 1975
holder3$other_consumption_price = holder3$other_consumption_price / holder3[holder3$year == 1975,"other_consumption_price"]
holder3$health_price = holder3$health_price / holder3[holder3$year == 1975,"health_price"]
holder3$q <- holder3$health_price / holder3$other_consumption_price
holder3$q_growth = NA
for(t in 2:nrow(holder3)){
  holder3$q_growth[t] = holder3$q[t] / holder3$q[t-1]
}
holder = merge(holder3[,c("year","q_growth")],holder,by="year")

# \begin{align}
# \widetilde{g}_{q,t} = \widetilde{g}_{\mu,t} + (\alpha_h - \alpha_ct) \big(\widetilde{g}_{r,t} - \widetilde{g}_{w,t}\big) + \widetilde{g}_{A_c,t} - \widetilde{g}_{A_h,t} \label{id1}
# \end{align}
holder$A_h_growth = exp(
  log(holder$mu_growth) + (alpha_h - exp(holder$log_share_level)) * (log(holder$labor_growth) - log(holder$health_capital_growth)) - log(holder$q_growth) + log(holder$growth)
)
# save time-varying alpha A_h_growth for plotting
A_h_growth_time_varying = holder$A_h_growth



# A_h_CHECK = matrix(NA,nrow=(length(A_h_growth_original)+1),ncol=2)
# A_h_CHECK[1,]=c(1,1)
# A_h_CHECK[2:nrow(A_h_CHECK),1] = A_h_growth_original
# A_h_CHECK[2:nrow(A_h_CHECK),2] = A_h_growth_time_varying
# for(t in 2:nrow(A_h_CHECK)){
#   A_h_CHECK[t,] = A_h_CHECK[t,] * A_h_CHECK[t-1,]
# }
# # normalize to 1975
# A_h_CHECK = as.data.frame(cbind(seq(1954,2019,1),A_h_CHECK))
# base = A_h_CHECK[A_h_CHECK$V1 == 1975,2:3]
# for(t in 1:nrow(A_h_CHECK)){
#   A_h_CHECK[t,2:3] = A_h_CHECK[t,2:3] / base
# }


##### print geometric mean annual growth rate of A_h #####
print("Model Period TFP Growth Rates:")
print("First year:")
print(holder$year[1])
print("Last year:")
print(holder$year[nrow(holder)])
print("Geometric mean A_h gross growth rate from model + data, but not solving the G.E.:")
print(geoMean(holder$A_h_growth))
print("Geometric mean A_c gross growth rate from Penn World Tables TFP data:")
print(geoMean(holder$growth))
print("5-YEAR GROWTH RATES FOR G.E. CALIBRATION:")
print("Geometric mean A_h gross growth rate from model + data, but not solving the G.E.:")
print(geoMean(holder$A_h_growth)^5)
print("Geometric mean A_c gross growth rate from Penn World Tables TFP data:")
print(geoMean(holder$growth)^5)


##### deflated price growth #####
print("Price growth (relative):")
print(geoMean(holder$q_growth))

##### MARKUP growth #####
print("Markup growth:")
print(geoMean(holder$mu_growth))


########### NOW PLOT THE DECOMPOSITION #############
# make holder ge_effect_total which includes the alpha_h - alpha_c weight
holder$l_ge_effect_growth_total = (alpha_h - exp(holder$log_share_level)) * (log(holder$labor_growth) - log(holder$health_capital_growth))
holder$l_neg_A_h_growth = - log(holder$A_h_growth)
holder$l_q_growth = log(holder[,"q_growth"])
holder$l_mu_growth = log(holder$mu_growth)
holder$l_growth = log(holder$growth)
holder3 = holder[, grep("^l_", names(holder))]
holder3$year = holder$year
print("Make sure all of the following are really small (decomp works!):")
print(holder$l_q_growth - holder$l_mu_growth - holder$l_ge_effect_growth_total - holder$l_growth - holder$l_neg_A_h_growth)

##### NOW NORMALIZE ALL OF THE VARIABLES TO 0 IN 1954 AND CONTINUE #####
holder4 = rbind(c(0,0,0,0,0,1954),holder3)
for(t in 2:nrow(holder4)){
  holder4[t,1:5] = holder4[t,1:5] + holder4[t-1,1:5]
}
### exponentiate, renormalize to 1975 then take logs again ###
holder4[,1:5] = exp(holder4[,1:5])
for(j in 1:5){
  holder4[,j] = holder4[,j]/holder4[holder4$year == 1975,j]
}
holder4[,1:5] = log(holder4[,1:5])

##### FIGURE 3b #####
##### CUMULATIVE YEAR ON YEAR GROWTH RATES #####
#png(filename=paste("../Draft/price_growth_decomp_cumulative_VARYING_LABOR_SHARE.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(holder4,plot(year,l_q_growth,type="n",ann=FALSE,ylim=bound_hold,ylab=NA,xlab=NA)) #,xaxt="n"))
grid()
with(holder4,lines(year,l_q_growth,col=cols[1],lty=1,lwd=2))
with(holder4,lines(year,l_ge_effect_growth_total,col=cols[2],lty=2,lwd=2))
with(holder4,lines(year,l_mu_growth,col=cols[3],lty=3,lwd=4))
with(holder4,lines(year,(l_growth+l_neg_A_h_growth),col=cols[4],lty=4,lwd=2))
#with(holder4,lines(year,l_neg_A_h_growth,col=cols[5],lty=5,lwd=2))
abline(h=0,lwd=1.1,lty=1,col="black")
axis(1, at=seq(1950, 2020, 20), las=1)
title(ylab=expression(paste("Cumulative Growth")),xlab=expression(paste("(b) Non-constant ",alpha[ct],sep="")),cex.lab=0.9)
legend(x = "topleft",inset = 0,
       legend = c("p","GE",expression(paste(mu)),expression(paste(A))), #expression(paste(A[c])),expression(paste(A[h]))),
       lty=seq(1,4,1),
       col=cols[1:4], lwd=c(2,2,4,2), cex=0.55)
#dev.off()

# redo now for time varying alpha
mu_estimated = as.data.frame(list(year=holder4$year,mu=exp(holder4$l_mu_growth)))
mu_estimated = merge(mu_estimated,mu_horenstein,by="year",all.x=TRUE)

############### GET THE MU'S, USING H-S 2019 LEVELS FROM TABLE 5 AND THE GROWTH RATE AS ESTIMATED FROM THE PRODUCTION DATA #################
# normalize mu_estimated = 1 in 1975
mu_estimated$normalized = mu_estimated$mu.x / mu_estimated[mu_estimated$year == 1975,"mu.x"] # normalize to 1975 (first year)
# extrapolate back to 1950
mu_estimated[mu_estimated$year %in% seq(1955,1970,5),"mu.y"] = mu_estimated[mu_estimated$year == 1975,"mu.y"] * mu_estimated[mu_estimated$year %in% seq(1950,1970,5),"normalized"]
# interpolate forward to 2015
mu_estimated[mu_estimated$year %in% seq(1975,2015,5),"mu.y"] = mu_estimated[mu_estimated$year == 1975,"mu.y"] * mu_estimated[mu_estimated$year %in% seq(1975,2015,5),"normalized"]

print("mu's for the calibrator for time-varying alphas. These mu's have the 1975 level of relative markups from Table 5 of Horenstein and Santos, 2019, but grow at the rate estimated from the Exact Identifiation exercise.")
print("Years, 1955 - 2015 in 5 year intervals.")
print("Mu (1955-2015):")
print(mu_estimated[mu_estimated$year %in% seq(1955,2015,5),"mu.y"])
print("EVERYTHING WILL BE ASSUMED CONSTANT AFTER 2015!")

# write 1955 to 2015 to disk
print("Markup in 1975 from H-S is approx. 1.5 ...")
#write(1.5 * as.vector(mu_estimated[mu_estimated$year %in% seq(1955,2015,5),"mu.y"]),"~/Dropbox/US_HEALTH_MACRO/Data_Work/GE_Calibration_Constant_Alpha_c/mu.txt",ncolumns=1)
write(1.5 * as.vector(mu_estimated[mu_estimated$year %in% seq(1955,2015,5),"mu.y"]),paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/mu.txt"),ncolumns=1)



#######################################################################################################
####################################### COMPARISON WITH H-S ###########################################
#######################################################################################################

##### Start with Horenstein and Santos (2019) IMPLIED MARKUPS AND TRY TO GET A_h's #####
# for internally estimated mu growth's
holder6 = merge(original_markup_const_alpha_c[,c("year","mu")],markup_non_constant_alpha_c[,c("year","mu")],by="year")
names(holder6) = c("year","baseline","time_varying_alpha")
holder6 = rbind(c(1954,1,1),holder6)
# for indexed mu's from H-S 2019
holder_HS = as.data.frame(list(
  year = hs_years_mu,
  hs_mus = hs_mu
))
holder6 = merge(holder6,holder_HS,by="year",all.x=TRUE)
# normalize everything to one in 1975
base = holder6[holder6$year == 1975,2:4]
for(t in 1:nrow(holder6)){
  holder6[t,2:4] <- holder6[t,2:4] / base
}


################################### REPEAT IDENTIFICATION OF g_{A_h} ###############################
######################################## but using H-S markups #####################################

print("Note that the H-S mu is only available for five-year increments...")

holder_HS$mu_growth_HS = NA # one year growth
for(t in 2:nrow(holder_HS)){
  holder_HS$mu_growth_HS[t] = (holder_HS$hs_mus[t] / holder_HS$hs_mus[t-1])^(1/5)
}
holder_HS$mu_growth_HS5 = NA # five year growth
for(t in 2:nrow(holder_HS)){
  holder_HS$mu_growth_HS5[t] = (holder_HS$hs_mus[t] / holder_HS$hs_mus[t-1])
}
holder = merge(holder_HS,holder,by="year",all.y=TRUE)
holder6 = holder[holder$year %in% seq(1955,2020,5),]
#holder6 = holder6[,grep("^l_", names(holder6))]
# compute five year growth to be able to identify A_h(5) (the five year A_h) #
holder6 = holder6[,c("year","mu_growth_HS","mu_growth_HS5","labor_health","health_capital_deflate","total_spend_deflate","Health Share")]

# ASSUME A_C grows at the TFP rate from FRED (Penn World Tables 10.01, see Feenstra et al. 2015)
tfp5 = tfp[tfp$year %in% seq(1955,2020,5),]
tfp5$growth = NA
for(t in 2:nrow(tfp5)){
  tfp5$growth[t] = tfp5$RTFPNAUSA632NRUG[t] / tfp5$RTFPNAUSA632NRUG[t-1]
}
# PRICES
holder3 <- as.data.frame(list(year=non_health_prices$YEAR,health_price=agg_prices$Health,other_consumption_price=non_health_price_index))
# normalize prices to 1975
holder3$other_consumption_price = holder3$other_consumption_price / holder3[holder3$year == 1975,"other_consumption_price"]
holder3$health_price = holder3$health_price / holder3[holder3$year == 1975,"health_price"]
holder3$q <- holder3$health_price / holder3$other_consumption_price
holder3 = holder3[holder3$year %in% seq(1955,2020,5),]
holder3$q_growth = NA
for(t in 2:nrow(holder3)){
  holder3$q_growth[t] = holder3$q[t] / holder3$q[t-1]
}

# THIS IS A MESS AND YOU DIDN'T NEED TO DO ALL OF THIS; THE TFP IS NOT IN THE HOLDER BUT EVERYTHING IS (REAL) IS #
# vars: c("labor_health","health_capital_deflate","total_spend_deflate","Health Share")
holder6 = merge(holder6,tfp5[,c("year","growth")],by="year")
holder6 = merge(holder6,holder3[,c("year","q_growth")],by="year")

# labor_growth & health_capital_growth
holder6$labor_growth = NA
holder6$health_capital_growth = NA
for(t in 2:nrow(holder6)){
  holder6$labor_growth[t] = holder6$labor_health[t] / holder6$labor_health[t-1]
  holder6$health_capital_growth[t] = holder6$health_capital_deflate[t] / holder6$health_capital_deflate[t-1]
}


###### STAGE 2 ... NOW, KNOWING alpha_h K_h AND L_h IDENTIFIES THE GENERAL EQUILIBRIUM EFFECT ######
# \begin{align}
# \widetilde{g}_{q,t} = \widetilde{g}_{\mu,t} + (\alpha_h - \alpha_c) \big(\widetilde{g}_{r,t} - \widetilde{g}_{w,t}\big) + \widetilde{g}_{A_c,t} - \widetilde{g}_{A_h,t} \label{id1}
# \end{align}
holder6 <- na.omit(holder6[,c("year","mu_growth_HS","mu_growth_HS5","labor_growth","health_capital_growth","q_growth","growth")])
holder6$A_h_growth5 = exp(
  log(holder6$mu_growth_HS5) + (alpha_h - alpha_c) * (log(holder6$labor_growth) - log(holder6$health_capital_growth)) - log(holder6$q_growth) + log(holder6$growth)
)
# annualized A_h
holder6$A_h_growth = holder6$A_h_growth5^0.2

##### print geometric mean annual growth rate of A_h #####
print("Model Period TFP Growth Rates:")
print("First year:")
print(holder6$year[1])
print("Last year:")
print(holder6$year[nrow(holder6)])
print("Geometric mean A_h gross growth rate from model + data, but not solving the G.E.:")
print(geoMean(holder6$A_h_growth))
print("Geometric mean A_c gross growth rate from Penn World Tables TFP data:")
print(geoMean(holder6$growth^0.2))
print("5-YEAR GROWTH RATES FOR G.E. CALIBRATION:")
print("Geometric mean A_h gross growth rate from model + data, but not solving the G.E.:")
print(geoMean(holder6$A_h_growth5))
print("Geometric mean A_c gross growth rate from Penn World Tables TFP data:")
print(geoMean(holder6$growth))

print("Using H-S Markups along with aggregate, sectoral input data 
      thus implies 0% annual growth in health sector productivity.")

# save HS A_h_growth for later plotting ...
A_h_growth_HS = holder6$A_h_growth
mu_growth_HS = holder6$mu_growth_HS


########### NOW PLOT THE DECOMPOSITION #############
# make holder ge_effect_total which includes the alpha_h - alpha_c weight
holder6$l_ge_effect_growth_total = (alpha_h - alpha_c) * (log(holder6$labor_growth) - log(holder6$health_capital_growth))
holder6$l_neg_A_h_growth = - log(holder6$A_h_growth5)
holder6$l_q_growth = log(holder6$q_growth)
holder6$l_mu_growth = log(holder6$mu_growth_HS5)
holder6$l_growth = log(holder6$growth)
years = holder6$year
holder7 = holder6[,grep("^l_", names(holder6))]
holder7$year = years

##### NOW NORMALIZE ALL OF THE VARIABLES TO 0 IN THE BASE YEAR AND CONTINUE #####
holder7 = rbind(c(0,0,0,0,0,1975),holder7)
for(t in 2:nrow(holder7)){
  holder7[t,1:5] = holder7[t,1:5] + holder7[t-1,1:5]
}
holder7 <- na.omit(holder7)
year_extra = as.data.frame(list(
  year=seq(1955,2020,5)
))
holder7 <- merge(holder7,year_extra,by="year",all.y=TRUE)

##### Figure 3c #########
##### CUMULATIVE YEAR ON YEAR GROWTH RATES #####
#png(filename=paste("../Draft/price_growth_decomp_cumulative_HS.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(holder7,plot(year,l_q_growth,type="n",ann=FALSE,ylim=bound_hold,ylab=NA,xlab=NA)) #,xaxt="n"))
grid()
with(holder7,lines(year,l_q_growth,col=cols[1],lty=1,lwd=2))
with(holder7,lines(year,l_ge_effect_growth_total,col=cols[2],lty=2,lwd=2))
with(holder7,lines(year,l_mu_growth,col=cols[3],lty=3,lwd=4))
with(holder7,lines(year,(l_growth+l_neg_A_h_growth),col=cols[4],lty=4,lwd=2))
#with(holder6,lines(year,l_neg_A_h_growth,col=cols[5],lty=5,lwd=2))
abline(h=0,lwd=1.1,lty=1,col="black")
axis(1, at=seq(1950, 2020, 20), las=1)
title(ylab=expression(paste("Cumulative Growth")),xlab="(c) Horenstein and Santos (2019) Markups",cex.lab=0.9)
legend(x = "topleft",inset = 0,
       legend = c("p","GE",expression(paste(mu)),expression(paste(A))), #expression(paste(A[c])),expression(paste(A[h]))),
       lty=seq(1,4,1),
       col=cols[1:4], lwd=c(2,2,4,2), cex=0.55)
#dev.off()


### variable labor share output ###
write(unlist(labor_share[labor_share$year %in% seq(1950,2015,5),"value"]),paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/labor_share_alpha_c.txt"),ncolumns = 1)

################################### REPEAT IDENTIFICATION OF g_{A_h} ###############################
################################# but now assuming 0 markup growth #################################

print("Now identify only A_h from the second stage, assuming g_mu = 0.")

#### STAGE 3 ... NOW GET A_h FROM 31: NOTE WE NEED JUST RELATIVE PRICES, i.e. q FROM THE MODEL ####
# \begin{align}
# \widetilde{g}_{q,t} = \widetilde{g}_{\mu,t} + (\alpha_h - \alpha_c) \big(\widetilde{g}_{r,t} - \widetilde{g}_{w,t}\big) + \widetilde{g}_{A_c,t} - \widetilde{g}_{A_h,t} \label{id1}
# \end{align}
# holder6 has A_h alone in it ... (keep it)
holder8 <- na.omit(holder[,c("year","labor_growth","health_capital_growth","q_growth","growth")])
holder8$A_h_growth_no_mu = exp((alpha_h - alpha_c) * (log(holder8$labor_growth)-log(holder8$health_capital_growth)) - log(holder8$q_growth) + log(holder8$growth)
)

##### print geometric mean annual growth rate of A_h #####
print("Model Period TFP Growth Rates:")
print("First year:")
print(holder8$year[1])
print("Last year:")
print(holder8$year[nrow(holder8)])
print("Geometric mean A_h gross growth rate from model + data, but not solving the G.E.:")
print(geoMean(holder8$A_h_growth_no_mu))
print("Geometric mean A_c gross growth rate from Penn World Tables TFP data:")
print(geoMean(holder8$growth))
print("5-YEAR GROWTH RATES FOR G.E. CALIBRATION:")
print("Geometric mean A_h gross growth rate from model + data, but not solving the G.E.:")
print(geoMean(holder8$A_h_growth_no_mu)^5)
print("Geometric mean A_c gross growth rate from Penn World Tables TFP data:")
print(geoMean(holder8$growth)^5)

print("Using zero markups along with aggregate, sectoral input data 
      thus implies negative annual growth in health sector productivity.")

# save A_h_growth_no_mu for later plotting ...
A_h_growth_no_mu = holder8$A_h_growth_no_mu

########### NOW PLOT THE DECOMPOSITION #############
# make holder ge_effect_total which includes the alpha_h - alpha_c weight
holder8$l_ge_effect_growth_total = (alpha_h - alpha_c) * (log(holder8$labor_growth)-log(holder8$health_capital_growth))
holder8$l_neg_A_h_growth = - log(holder8$A_h_growth_no_mu)
holder8$l_q_growth = log(holder8[,"q_growth"])
holder8$l_growth = log(holder8$growth)
years = holder8$year
holder8 = holder8[,grep("^l_", names(holder8))]
holder8$year = years


##### NOW NORMALIZE ALL OF THE VARIABLES TO 0 IN THE BASE YEAR AND CONTINUE #####
holder8 = rbind(c(0,0,0,0,1954),holder8)
for(t in 2:nrow(holder4)){
  holder8[t,1:4] = holder8[t,1:4] + holder8[t-1,1:4]
}
### exponentiate, renormalize to 1975 then take logs again ###
holder8[,1:4] = exp(holder8[,1:4])
for(j in 1:4){
  holder8[,j] = holder8[,j]/holder8[holder8$year == 1975,j]
}
holder8[,1:4] = log(holder8[,1:4])
holder8 <- na.omit(holder8)

#cols = c("black","blue","red","darkorange","darkgreen","purple","brown")
#export bounds


###### FIGURE 3d ######
##### CUMULATIVE YEAR ON YEAR GROWTH RATES #####
#png(filename=paste("../Draft/price_growth_decomp_cumulative_no_mu.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(holder8,plot(year,l_q_growth,type="n",ann=FALSE,ylim=bound_hold,ylab=NA,xlab=NA)) #,xaxt="n"))
grid()
with(holder8,lines(year,l_q_growth,col=cols[1],lty=1,lwd=2))
with(holder8,lines(year,l_ge_effect_growth_total,col=cols[2],lty=2,lwd=2))
#with(holder7,lines(year,l_mu_growth,col=cols[3],lty=3,lwd=2))
with(holder8,lines(year,(l_growth+l_neg_A_h_growth),col=cols[4],lty=4,lwd=2))
#with(holder6,lines(year,l_neg_A_h_growth,col=cols[5],lty=5,lwd=2))
abline(h=0,lwd=1.1,lty=1,col="black")
axis(1, at=seq(1950, 2020, 20), las=1)
title(ylab=expression(paste("Cumulative Growth")),xlab=expression(paste("(d) No Markup Growth, ",widetilde(g)[mu],"= 0",sep="")),cex.lab=0.9)
legend(x = "topleft",inset = 0,
       legend = c("p","GE",expression(paste(A))), #expression(paste(A[c])),expression(paste(A[h]))),
       lty=c(1,2,4),
       col=cols[c(1,2,4)], lwd=2, cex=0.55)
dev.off()


################################################################################
################################################################################
##################### PLOT A_H AND MARKUPS TOGETHER ############################
################################################################################
################################################################################

holder_AH = as.data.frame(
  list(
    year=seq(1955,2019,1),
    A_h_growth_original = A_h_growth_original,
    A_h_growth_time_varying = A_h_growth_time_varying,
    A_h_growth_no_mu = A_h_growth_no_mu
  )
)
# normalize to 1 in 1954
holder_AH = rbind(c(1954,1,1,1),holder_AH)
for(t in 2:nrow(holder_AH)){
  holder_AH[t,2:4] = holder_AH[t,2:4] * holder_AH[t-1,2:4]
}
# normalize to 1 in 1975
base = holder_AH[holder_AH$year == 1975,2:4]
for(t in 1:nrow(holder_AH)){
  holder_AH[t,2:4] = holder_AH[t,2:4] / base
}
# H-S five year A_h
holder_AH_HS = as.data.frame(
  list(
    year=seq(1980,2005,5),
    A_h_growth_HS = A_h_growth_HS^5
  )
)
holder_AH_HS = rbind(c(1975,1),holder_AH_HS)
for(t in 2:nrow(holder_AH_HS)){
  holder_AH_HS[t,2] = holder_AH_HS[t,2] * holder_AH_HS[t-1,2]
}
holder_AH = merge(holder_AH,holder_AH_HS,by="year",all.x=TRUE)


save(holder_AH, file=paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/A_h_decomp_estimates.Rdata"))
save(holder_AH, file=paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/A_h_decomp_estimates.Rdata"))
save(holder_AH, file=paste0(analysis_dir,"/GE_Calibration_No_Mu_Growth/A_h_decomp_estimates.Rdata"))
save(holder_AH, file=paste0(analysis_dir,"/GE_Calibration_Mu_Growth_Past_2015/A_h_decomp_estimates.Rdata"))
save(holder_AH, file=paste0(analysis_dir,"/GE_Calibration_HS_Markups_Full_Calibration/A_h_decomp_estimates.Rdata"))


########### FIGURE 4 ###########
# Open a PNG device (optional)
#png(paste0(results_dir,"/figure4.png"), width = 2500, height = 1250, res = 300)
pdf(paste0(results_dir,"/figure4.pdf"), width = 7, height = 3.5) 

# Define a 2x2 layout with custom widths and heights
layout_matrix <- matrix(c(1, 2), nrow = 1, byrow = TRUE)
# Set up the layout with different relative sizes
layout(layout_matrix, widths = c(1, 1), heights = c(1))
# Add borders to visualize plot boundaries
par(mar = c(4, 4, 2, 1))  # margin setup

###### figure 4a ######
### PLOT A_H ###
cols = c("red","blue","purple")
#png(filename=paste("../Draft/A_h_TOGETHER.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(holder_AH,plot(year,A_h_growth_original,type="n",ann=FALSE,ylim=c(min(holder_AH[,2:5],na.rm=TRUE),
                                                                     max(holder_AH[,2:5],na.rm=TRUE)),ylab=NA,xlab=NA,xaxt="n"))
grid()
with(holder_AH,lines(year,A_h_growth_original,col=cols[1],lty=2,lwd=2))
with(holder_AH,lines(year,A_h_growth_time_varying,col=cols[2],lty=3,lwd=4))
with(holder_AH,lines(year,A_h_growth_no_mu,col=cols[3],lty=4,lwd=2))
with(holder_AH,points(year,A_h_growth_HS,col="black",pch=2,lwd=1.5))
abline(h=1,lwd=1.1,lty=1,col="black")
axis(1, at=seq(1950, 2020, 10), las=1)
title(ylab=expression(paste(A[ht]," 1975 = 1")),xlab="(a) Health-sector TFP's",cex.lab=0.85)
legend(x = "bottomleft",inset = 0,
       legend = c(expression(paste("Constant ",alpha[c])),
                  expression(paste("Non-constant ",alpha[ct])),
                  expression(paste("Horenstein & Santos, 2019")),
                  expression(paste(tilde(g)[mu],"= 0"))),
       lty=c(2,3,NA,4),pch=c(NA,NA,2,NA),
       col=c(cols[1:2],"black",cols[3]), lwd=c(2,4,1.2,2), cex=0.55)
#dev.off()

print("What is the implied % DECLINE in A_h by not accounting for relative markup growth?")
print("1954-2019:")
print((holder_AH[holder_AH$year==2019,"A_h_growth_no_mu"]/holder_AH[holder_AH$year==1954,"A_h_growth_no_mu"]-1) * 100)
print("What is the implied % INCREASE in A_h in the baseline model?")
print("1954-2019:")
print((holder_AH[holder_AH$year==2019,"A_h_growth_original"]/holder_AH[holder_AH$year==1954,"A_h_growth_original"]-1) * 100)
print("What is the implied % DECREASE in A_h in the H-S model?")
print("1975-2005:")
print((holder_AH[holder_AH$year==2005,"A_h_growth_HS"]/holder_AH[holder_AH$year==1975,"A_h_growth_HS"]-1) * 100)
print("What is the implied % INCREASE in A_h in the time-varying alpha model?")
print("1954-2019:")
print((holder_AH[holder_AH$year==2019,"A_h_growth_time_varying"]/holder_AH[holder_AH$year==1954,"A_h_growth_time_varying"]-1) * 100)

print("Average annual A_h growth rates, 1954-2019:")
print("What is the implied A_h gross growth in the baseline model? Avg. annual (1954-2019):")
print(geoMean(A_h_growth_original))
print("What is the implied A_h gross growth in the time-varying alpha model? Avg. annual (1954-2019):")
print(geoMean(A_h_growth_time_varying))
print("What is the implied A_h gross growth in the no-mu growth model? Avg. annual (1954-2019):")
print(geoMean(A_h_growth_no_mu))


print("Average A_h growth rates, 1975-2005 (apple to apples comparison):")
print("What is the implied A_h gross growth in the baseline model? Avg. annual (1975-2005):")
print((holder_AH[holder_AH$year==2005,"A_h_growth_original"]/holder_AH[holder_AH$year==1975,"A_h_growth_original"])^(1/(2005-1975+1)))
print("What is the implied A_h gross growth in the time-varying alpha model? Avg. annual (1975-2005):")
print((holder_AH[holder_AH$year==2005,"A_h_growth_time_varying"]/holder_AH[holder_AH$year==1975,"A_h_growth_time_varying"])^(1/(2005-1975+1)))
print("What is the implied A_h gross growth in the H-S model? Avg. annual (1975-2005):")
print((holder_AH[holder_AH$year==2005,"A_h_growth_HS"]/holder_AH[holder_AH$year==1975,"A_h_growth_HS"])^(1/(2005-1975+1)))
print("What is the implied A_h gross growth in the no-mu growth model? Avg. annual (1975-2005):")
print((holder_AH[holder_AH$year==2005,"A_h_growth_no_mu"]/holder_AH[holder_AH$year==1975,"A_h_growth_no_mu"])^(1/(2005-1975+1)))


### 
holder_mu = as.data.frame(
  list(
    year=seq(1955,2019,1),
    mu_growth_original = mu_growth_original,
    mu_growth_time_varying = mu_growth_time_varying
  )
)
# normalize to 1 in 1954
holder_mu = rbind(c(1954,1,1,1),holder_mu)
for(t in 2:nrow(holder_mu)){
  holder_mu[t,2:3] = holder_mu[t,2:3] * holder_mu[t-1,2:3]
}
base = holder_mu[holder_mu$year == 1975,2:3]
for(t in 1:nrow(holder_mu)){
  holder_mu[t,2:3] = holder_mu[t,2:3] / base
}
# H-S annualized mu
holder_mu_HS = as.data.frame(
  list(
    year=seq(1980,2005,5),
    mu_growth_HS = mu_growth_HS^5
  )
)
holder_mu_HS = rbind(c(1975,1),holder_mu_HS)
for(t in 2:nrow(holder_mu_HS)){
  holder_mu_HS[t,2] = holder_mu_HS[t,2] * holder_mu_HS[t-1,2]
}
holder_mu = merge(holder_mu,holder_mu_HS,by="year",all.x=TRUE)


#### FIGURE 4B ######
#### PLOT MU ####
#png(filename=paste("../Draft/mu_TOGETHER.png",sep=""),width=5,height=4,units="in",res=300)
#par(mar = c(2,5,2,2))
with(holder_mu,plot(year,mu_growth_original,type="n",ann=FALSE,ylim=c(min(holder_mu[,2:4],na.rm=TRUE),
                                                                       max(holder_mu[,2:4],na.rm=TRUE)),ylab=NA,xlab=NA,xaxt="n"))
grid()
with(holder_mu,lines(year,mu_growth_original,col=cols[1],lty=2,lwd=2))
with(holder_mu,lines(year,mu_growth_time_varying,col=cols[2],lty=3,lwd=4))
#with(holder_mu,lines(year,A_h_growth_no_mu,col=cols[3],lty=4,lwd=2))
with(holder_mu,points(year,mu_growth_HS,col="black",pch=2,lwd=1.5))
abline(h=1,lwd=1.1,lty=1,col="black")
axis(1, at=seq(1950, 2020, 10), las=1)
title(ylab=expression(paste(mu[t]," 1975 = 1")),xlab="(b) Relative Markups",cex.lab=0.85)
legend(x = "topleft",inset = 0,
       legend = c(expression(paste("Constant ",alpha[c])),
                  expression(paste("Non-constant ",alpha[ct])),
               #   expression(paste(tilde(g)[mu],"= 0")),
                  expression(paste("Horenstein & Santos, 2019"))),
       lty=c(2,3,NA),pch=c(NA,NA,2),
       col=c(cols[1:2],"black"), lwd=c(2,4,1.2), cex=0.55)
dev.off()

save(holder_mu, file=paste0(analysis_dir,"/GE_Calibration_Constant_Alpha_c/mu_decomp_estimates.Rdata"))
save(holder_mu, file=paste0(analysis_dir,"/GE_Calibration_Variable_Alpha_c/mu_decomp_estimates.Rdata"))

print("What is the implied % relative markup growth in the baseline model?")
print("1954-2019:")
print((holder_mu[holder_mu$year==2019,"mu_growth_original"]/holder_mu[holder_mu$year==1954,"mu_growth_original"]-1) * 100)
print("What is the implied relative markup growth (%) in the time-varying alpha model?")
print("1954-2019:")
print((holder_mu[holder_mu$year==2019,"mu_growth_time_varying"]/holder_mu[holder_mu$year==1954,"mu_growth_time_varying"]-1) * 100)

print("1975-2005 (all scaled over the same period):")
print("What is the implied % relative markup growth in the baseline model?")
print("1975-2005:")
print((holder_mu[holder_mu$year==2005,"mu_growth_original"]/holder_mu[holder_mu$year==1975,"mu_growth_original"]-1) * 100)
print("What is the implied relative markup growth (%) in the time-varying alpha model?")
print("1975-2005:")
print((holder_mu[holder_mu$year==2005,"mu_growth_time_varying"]/holder_mu[holder_mu$year==1975,"mu_growth_time_varying"]-1) * 100)
print("What is the implied relative markup growth (%) in the H-S model?")
print("1975-2005:")
print((holder_mu[holder_mu$year==2005,"mu_growth_HS"]/holder_mu[holder_mu$year==1975,"mu_growth_HS"]-1) * 100)

print("Average annual markup growth rates, 1954-2019:")
print("What is the implied relative markup gross growth in the baseline model? Avg. annual (1954-2019):")
print(geoMean(mu_growth_original))
print("What is the implied relative markup gross growth in the time-varying alpha model? Avg. annual (1954-2019):")
print(geoMean(mu_growth_time_varying))

print("Average annual markup growth rates, 1975-2005 (apple to apples comparison):")
print("What is the implied relative markup gross growth in the baseline model? Avg. annual (1975-2005):")
print((holder_mu[holder_mu$year==2005,"mu_growth_original"]/holder_mu[holder_mu$year==1975,"mu_growth_original"])^(1/(2005-1975+1)))
print("What is the implied relative markup gross growth in the time-varying alpha model? Avg. annual (1975-2005):")
print((holder_mu[holder_mu$year==2005,"mu_growth_time_varying"]/holder_mu[holder_mu$year==1975,"mu_growth_time_varying"])^(1/(2005-1975+1)))
print("What is the implied relative markup gross growth in the H-S model? Avg. annual (1975-2005):")
print((holder_mu[holder_mu$year==2005,"mu_growth_HS"]/holder_mu[holder_mu$year==1975,"mu_growth_HS"])^(1/(2005-1975+1)))

##### print labor share max and year ####
print("What is the time-varying capital share min, and in what year is it (i.e., labor share max)?")
print("Capital share min over time:")
print(min(1-labor_share$value))
print("Year of capital share min:")
print(unname(unlist(labor_share[labor_share$value == min(labor_share$value),"year"])))
print("Capital share max over time:")
print(max(1-labor_share$value))
print("Year of capital share max:")
print(unname(unlist(labor_share[labor_share$value == max(labor_share$value),"year"])))

# end #
closeAllConnections()

