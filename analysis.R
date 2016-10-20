# initial analysis
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(radarchart)
library(animation)
cards <- fread("cards.csv")
flat <- fread("cards_flat.csv")
costs <- fread("dust_costs.csv")
entourages <- fread("entourages.csv")
mechanic <- fread("mechanics.csv")
reqi <- fread("play_requirements.csv")

# initial graph
cardset <- cards[,.N,by=set][order(-N)]
ggplot(cardset,aes(x=set,y=N,fill=set))+
    theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))+
    geom_bar(stat="identity")+
    ggtitle("Number of cards by set")

# Goals:
#  Look at cost, attack, health by player class
#                           ... by expansion over time
#  Look at dust cost of cards by player class
#                         ... by expansion over time

# extract all legal cards
# this excludes tavern brawl, which can be dealt with later
df <- data.frame(cards)
features <- c("BRM","CORE","EXPERT1","GVG","KARA","LOE","NAXX","OG","TGT")
legal <- df[df$set %in% features,]
# add dates
legal$release <- as.Date("2014-03-14")
legal$release[legal$set == "NAXX"] <- as.Date("2014-07-22")
legal$release[legal$set == "GVG"] <- as.Date("2014-12-08")
legal$release[legal$set == "BRM"] <- as.Date("2015-04-02")
legal$release[legal$set == "TGT"] <- as.Date("2015-08-24")
legal$release[legal$set == "LOE"] <- as.Date("2015-11-12")
legal$release[legal$set == "OG"] <- as.Date("2016-04-26")
legal$release[legal$set == "KARA"] <- as.Date("2016-08-11")

df <- data.frame(costs)
# decimate set due to repeating nature
dust <- df[df$action == "CRAFTING_NORMAL",]
dust$rarity <- 'common'
dust$rarity[dust$cost == 100] <- 'rare'
dust$rarity[dust$cost == 400] <- 'epic'
dust$rarity[dust$cost == 1600] <- 'legendary'

# get combined list, 675 obs
total <- merge(legal,dust,by="card_id")
cross <- table(total$rarity.x,total$set)
# clean up
tmpRow <- cross[4,]
cross[4,] <- cross[3,]
cross[3,] <- cross[2,]
cross[2,] <- tmpRow
tmpRow <- cross[,4]
cross[,4] <- cross[,3]
cross[,3] <- tmpRow
row.names(cross) <- c("COMMON","RARE","EPIC","LEGENDARY")
colnames(cross) <- c("Classic","Goblins vs Gnomes","Grand Tournament","Old Gods")
# colors selected from the pantone fall 2016 collection
# here: set yaxis better
par()
# plot craftable cards by expansion
barplot(cross,main="Craftable cards by expansion",xlab="Expansion",ylab="# of cards",col=c("#4C6A92","#006E51","#D8AE47","#B93A32"),legend=rownames(cross),beside=T)

cross[4,] / colSums(cross)

# how about by player class?
cross2 <- table(total$rarity.x,total$playerClass)
tmpRow <- cross2[4,]
cross2[4,] <- cross2[3,]
cross2[3,] <- cross2[2,]
cross2[2,] <- tmpRow
row.names(cross2) <- c("COMMON","RARE","EPIC","LEGENDARY")

# how much to construct every card possible?
cross3 <- cross2
cross3[1,] <- cross3[1,] * 40
cross3[2,] <- cross3[2,] * 100
cross3[3,] <- cross3[3,] * 400
cross3[4,] <- cross3[4,] * 1600
colSums(cross3)

table(legal$cost,legal$playerClass)
table(legal$attack,legal$playerClass)
table(legal$health,legal$playerClass)
legal$name[legal$health == 100]

# todo:
#  clean up power analysis
#  create markdown file
#  possible Tableau extension
#  add human comments
#  filter out unplayable cards from adventures
