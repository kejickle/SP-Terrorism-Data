data = read.csv("terrordata.csv")
myvars = c("iyear", "country_txt", "crit1", "region", "latitude", "longitude", 
           "attacktype1", "attacktype2", "attacktype3",  
           "weaptype1", "targtype1", "gname", "nperps", "nperpcap", 
           "nkill", "nkillus", "nkillter", "nwound", "nwoundus", "nwoundte", 
           "propvalue", "nhostkid", "nhostkidus", "nhours", "ndays", "ransompaid")
data = data[ , myvars]
colnames(data)[which(colnames(data)=="iyear")] = "year"
colnames(data)[which(colnames(data)=="country_txt")] = "country"
colnames(data)[which(colnames(data)=="crit1")] = "attack.aimed.at"
colnames(data)[which(colnames(data)=="gname")] = "perpetrator.name"
colnames(data)[which(colnames(data)=="nperps")] = "number.participating.perpetrators"
colnames(data)[which(colnames(data)=="nperpcap")] = "number.perpetrators.captured"
colnames(data)[which(colnames(data)=="propvalue")] = "damage.property.value"
colnames(data)[which(colnames(data)=="nhostkid")] = "number.hostages.kidnappings"
colnames(data)[which(colnames(data)=="nhostkidus")] = "number.hostages.kidnappings.US"
colnames(data)[which(colnames(data)=="nhours")] = "hours.hostage.kidnap"
colnames(data)[which(colnames(data)=="ndays")] = "days.hostage.kidnap"
colnames(data)[which(colnames(data)=="ransompaid")] = "ransom.paid"
colnames(data)[which(colnames(data)=="nkill")] = "number.killed"
colnames(data)[which(colnames(data)=="nkillus")] = "number.US.killed"
colnames(data)[which(colnames(data)=="nkillter")] = "number.perpetrators.killed"
colnames(data)[which(colnames(data)=="nwound")] = "number.injured"
colnames(data)[which(colnames(data)=="nwoundus")] = "number.US.injured"
colnames(data)[which(colnames(data)=="nwoundte")] = "number.perpetrators.injured"
codebook = read.csv("codebook.csv")
data$latitdue[data$latitude== "" ] = NA
data$latitude = as.numeric(levels(data$latitude))[data$latitude]
saveRDS(data, file = "terrorismData.Rda")

data = readRDS(file = "terrorismData.Rda")



womenInGov = read.csv("womenParl.csv", skip = 2)
militarySpending= read.csv("military.csv", skip=2)
unemployment = read.csv("unemployment.csv", skip = 2)
colnames(womenInGov)[1] = "country"
colnames(militarySpending)[1] = "country"
colnames(unemployment)[1] = "country"
unemployment = unemployment[,-c(2,3,4)]
for (i in 2:length(colnames(unemployment))){
  colnames(unemployment)[i] <- paste("unemployment.", 1958+i, sep="")
}
saveRDS(womenInGov, file = "womenInGov.Rda")
saveRDS(militarySpending, file = "militarySpending.Rda")
saveRDS(unemployment, file = "unemployment.Rda")



w = women[, c(1,grep(year, colnames(women)))]
m = milt[, c(1,grep(year, colnames(milt)))]

saveRDS(data, file = "terrorismData.Rda")


l$coeff[grep(x, names(l$coeff))]
