# initial analysis
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(radarchart)
library(animation)
library(shiny)
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
legal <- legal[is.na(legal$collectible) == 0,]
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

# get combined list, 675 obs
total <- merge(legal,dust,by="card_id")
cross <- table(total$rarity,total$set)
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

# proportions of legendaries and epics
(cross[4,]+cross[3,]) / colSums(cross)
# not much of a power creep by percentage

# how about by player class?
cross2 <- table(total$rarity,total$playerClass)
tmpRow <- cross2[4,]
cross2[4,] <- cross2[3,]
cross2[3,] <- cross2[2,]
cross2[2,] <- tmpRow
row.names(cross2) <- c("COMMON","RARE","EPIC","LEGENDARY")
# fairly easy to break into any class; one card discrepancy among 9 classes

# how much to construct every card possible?
#  11960 dust for all classes but hunter
#  13560 dust for hunter

t <- table(legal$cost,legal$playerClass)
colMeans(t)
# costly cards (10): DOOM! for warlock, mind control for priest, anyfin can happen for paladin,
#  pyroblast for mage, varian wrynn for warrior
# also neutral giants
table(legal$attack,legal$playerClass)
# attacky cards(9): mal'ganis for warlock, anima golem for warlock, malorne for druid
table(legal$health,legal$playerClass)
# healthy cards(9): gahz'rilla for hunter, anima golem for warlock, grommash hellscream for warrior

# look at actual cost
features = c("card_id","playerClass","type","name","cost","attack","health","rarity","durability","set")
mech <- merge(legal[names(legal) %in% features],mechanic,by="card_id",all.x=T)
mech3 <- subset(mech,mech$type == "MINION")
features = c("name","cost","attack","health")
mech2 <- mech[names(mech) %in% features]
mech2 <- subset(mech2,mech$type == "MINION")
sort(table(mech3$mechanic))
# factor analysis: combine similarities
#  adjacent buff -> aura
#  topdeck -> battlecry
#  invis deathrattle -> deathrattle
r <- data.frame(unique(mech3$mechanic)[2:23]) # get rid of NA
t <- apply(r,1,function(x) grepl(x,mech3$mechanic))
colnames(t) <- unique(mech$mechanic)[2:23]
mech2 <- cbind(mech2,t)
fit <- lsfit(mech2[,c(-1,-2)], mech2[,2])
z <- fit$coefficients[2:25]
trueCost <- apply(mech2[3:26],1,function(x) x*z)
colSums(trueCost)
# fit a line here? maybe not necessary
plot(colSums(trueCost),mech2$cost)

mech3$trueCost <- colSums(trueCost)
mech3$delta <- mech3$trueCost - mech3$cost
ind <- sort(mech3$delta,index.return=T)
sortedMech <- mech3[ind$ix,]
# dropping Molten Giant as he is a very clear outlier
sortedMech <- sortedMech[2:694,]

# computed power by player class
ggplot(sortedMech,aes(x=playerClass,y=delta))+
    theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))+
    geom_point(alpha=0.5,aes(color=factor(as.factor(sortedMech$set))))+
    ggtitle("Calculated cost by player class")

# computed power by expansion
ggplot(sortedMech,aes(x=set,y=delta))+
    theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))+
    geom_point(alpha=0.5,aes(color=factor(as.factor(sortedMech$playerClass))))+
    ggtitle("Calculated cost by set")

# shiny app
if (interactive()) {
    
    ui <- fluidPage(
        sidebarLayout(
            sidebarPanel(
        radioButtons("type","View cards sorted:",c("by player class" = "pClass","by card set" = "cSet"),selected = "pClass"),
        # have all be radio or checkboxGroupInput
        radioButtons("classSort","Filter by sets:",
                           c("Basic"="CORE","Classic"="EXPERT1","Naxxramas"="NAXX",
                             "Goblins vs Gnomes"="GVG","Blackrock Mountain"="BRM","The Grand Tournament"="TGT",
                             "League of Explorers"="LOE","Whispers of the Old Gods"="OG","One Night in Karazhan"="KARA")),
        radioButtons("setSort","Filter by classes:",
                           c("Druid"="DRUID","Hunter"="HUNTER","Mage"="MAGE",
                             "Paladin"="PALADIN","Priest"="PRIEST","Rogue"="ROGUE",
                             "Shaman"="SHAMAN","Warlock"="WARLOCK","Warrior"="WARRIOR","Neutral"="NEUTRAL")),
        radioButtons("rareSort","Filter by rarity",c("None"="NONE","Common"="COMMON","Rare"="RARE","Epic"="EPIC",
                                                     "Legendary"="LEGENDARY"))),
        mainPanel(
        plotOutput("finalPlot"))
    ))
    
    server <- function(input,output) {
        output$finalPlot <- renderPlot({
            # if radiobutton is this
            # render this plot
            # filter on this checkbox
            if (input$type == "pClass") {
                sub <- subset(sortedMech,input$classSort == sortedMech$set)
                if (input$rareSort != "NONE") {
                    sub <- subset(sub,input$rareSort == sub$rarity)
                    p <- ggplot(sub,aes(x=playerClass,y=delta))+
                        theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))+
                        geom_point(alpha=0.6,col="steelblue",size=2)+
                        coord_cartesian(ylim = c(-4,4))+
                        ggtitle("Calculated cost of cards by player class")
                    print(p)
                }
                else {
                    p <- ggplot(sub,aes(x=playerClass,y=delta))+
                        theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))+
                        geom_point(alpha=0.6,col="steelblue",size=2)+
                        coord_cartesian(ylim = c(-4,4))+
                        ggtitle("Calculated cost of cards by player class")
                    print(p)
                }
            }
            if (input$type == "cSet") {
                sub <- subset(sortedMech,input$setSort == sortedMech$playerClass)
                if (input$rareSort != "NONE") {
                    sub <- subset(sub,input$rareSort == sub$rarity)
                    p <-ggplot(sub,aes(x=set,y=delta))+
                        theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))+
                        geom_point(alpha=0.6,col="steelblue",size=2)+
                        coord_cartesian(ylim = c(-4,4))+
                        ggtitle("Calculated cost of cards by set")
                    print(p)
                }
                else {
                    p <-ggplot(sub,aes(x=set,y=delta))+
                        theme(axis.text.x = element_text(angle = 45, size=8,hjust = 1))+
                        geom_point(alpha=0.6,col="steelblue",size=2)+
                        coord_cartesian(ylim = c(-4,4))+
                        ggtitle("Calculated cost of cards by set")
                    print(p)
                }
            }
        })
    }
    shinyApp(ui,server)
}

# todo:
#  create markdown file
#  possible Tableau extension
