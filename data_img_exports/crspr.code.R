#written 2/28/2018 for JGI MGM26 workshop by EBG
rm(list=ls())
library(vegan)

setwd('/Users/grah930/Documents/jgi_project/')

data = read.csv('Cog_crsp.csv')
#####NEED TO SUBSET DATA
wis_corn
wis_native
kan_corn
kan_native
iowa_corn
iowa_native

#######ADONIS across groups######
#####NEED TO CHECK CODE

wis_all = cbind(wis_corn,wis_native)
kan_all = cbind(kan_corn,kan_native)
iowa_all = cbind(iowa_corn,kan_native)

wis_dist = vegdist(wis_all, method = 'bray', binary = F)
kan_dist = vegdist(kan_all, method = 'bray', binary = F)
iowa_dist = vegdist(iowa_all, method = 'bray', binary = F)

group_wis = c(rep('corn',ncol(wis_corn)),rep('native',ncol(wis_native)))
group_kan = c(rep('corn',ncol(kan_corn)),rep('native',ncol(kan_native)))
group_iowa = c(rep('corn',ncol(iowa_corn)),rep('native',ncol(iowa_native)))

adonis(wis_dist~group_wis)
adonis(kan_dist~group_kan)
adonis(iowa_dist~group_iowa)

########looop through differential expression######
#####NEED TO CHECK CODE

wis.result = numeric()
kan.result = numeric()
iowa.result = numeric()

for (i in 1:nrow(data)){
  mod.wis = t.test(wis_corn[i,],wis_native[i,])
  mod.kan = t.test(wis_corn[i,],wis_native[i,])
  mod.iowa = t.test(iowa_corn[i,],iowa_native[i,])
  wis.result = rbind(wis.result,c(mod.wis$p.val,
       mean(wis_corn[i,],na.rm = T),mean(wis_native[i,],na.rm = T)))
  kan.result = rbind(kan.result,c(mod.kan$p.val,
                                  mean(kan_corn[i,],na.rm = T),mean(kan_native[i,],na.rm = T)))
  iowa.result = rbind(iowa.result,c(mod.iowa$p.val,
                                  mean(iowa_corn[i,],na.rm = T),mean(iowa_native[i,],na.rm = T)))
}
wis.result = as.data.frame(wis.result);names(wis.result) = c('p.val','mean.corn','mean.native')
row.names(wis.result) = row.names(data)

kan.result = as.data.frame(kan.result);names(kan.result) = c('p.val','mean.corn','mean.native')
row.names(kan.result) = row.names(data)

iowa.result = as.data.frame(iowa.result);names(iowa.result) = c('p.val','mean.corn','mean.native')
row.names(iowa.result) = row.names(data)

write.csv(wis.result,'wis.result.csv')
write.csv(kan.result,'kan.result.csv')
write.csv(iowa.result,'iowa.result.csv')