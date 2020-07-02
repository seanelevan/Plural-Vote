library("ggplot2")
library("usmap")

#regression <- lm(formula = difference ~ msnbc + nyt + foxnews + huffingtonpost + washingtonpost)$coefficients
last <- read.csv("details4.csv")
last <- as.data.frame(lapply(last, as.numeric))
redo <- (42.623382-3.913186*last[, 3]-35.471488*last[, 4]-last[, 5]*43.467061-last[, 6]*2.373378-last[, 7]*53.326425)

redo <- redo

dataframe <- merge(dataframe, read.csv("Delegates.csv", stringsAsFactors = FALSE)[c(1, 7)], by = 1)

difference <- as.numeric(as.character(unlist(dataframe[(ncol(dataframe))])))

correction <- (difference-redo)

last <- read.csv("details3.csv")
last <- as.data.frame(lapply(last, as.numeric))
redo <- (42.623382-3.913186*last[, 3]-35.471488*last[, 4]-last[, 5]*43.467061-last[, 6]*2.373378-last[, 7]*53.326425)

redo <- redo

dataframe <- merge(dataframe, read.csv("Delegates.csv", stringsAsFactors = FALSE)[c(1, 6)], by = 1)

difference <- as.numeric(as.character(unlist(dataframe[(ncol(dataframe))])))

correction <- (difference-redo)



last <- read.csv("details2.csv")
last <- as.data.frame(lapply(last, as.numeric))
redo <- (47.474-33.477*last[, 3]-23.703*last[, 4]-last[, 5]*53.04-last[, 6]*18.96-last[, 7]*26.89)

#redo <- (redo+correction*(3/4))

#redo <- (redo-median(redo))

# mean((as.numeric(as.character(redo))-(as.numeric(as.character(unlist(dataframe[(ncol(dataframe)-3)])))-as.numeric(as.character(unlist(dataframe[(ncol(dataframe)-2)]))))))

# 2020 MODEL

last <- read.csv("details2.csv")
last <- as.data.frame(lapply(last, as.numeric))
redo <- (47.81-22.11*last[, 3]-21.11*last[, 4]-last[, 5]*53.04-last[, 6]*18.96-last[, 7]*26.89)

#redo <- (redo+correction*(3/4))

#redo <- (redo-median(redo))

difference <- as.numeric(as.character(unlist(dataframe[(ncol(dataframe)-1)])))-as.numeric(as.character(unlist(dataframe[(ncol(dataframe))])))

correction <- (difference-redo)




redo <- (47.81-22.11*msnbc-21.11*nyt-foxnews*53.04-huffingtonpost*18.96-washingtonpost*26.89)

redo <- (redo+correction*(3/4))

redo <- (redo-median(redo))



# cor((redo), as.numeric(as.character(unlist(dataframe[(ncol(dataframe)-3)])))-as.numeric(as.character(unlist(dataframe[(ncol(dataframe)-2)]))))

datas <- cbind(levels(dataframe[, 1]), redo)

datas <- data.frame(datas)

colnames(datas) <- c("state", "polls")

datas[, "state"] <- as.character(datas[, "state"])

datas[, "polls"] <- as.numeric(as.character(datas[, "polls"]))

datas <- datas[order(datas$polls),]

TrumpECs <- 0;
BidenECs <- 0;

simulationsList <- c()
for(i in 1:10000){
  use <- datas[, "polls"]+rlogis(1, 0, 10)
for(i in 1:nrow(datas)){
  margin = (use[i]+rlogis(1, 0, 10))
  if(margin > 100){
    margin <- 100
  }
  if(margin < -100){
    margin <- -100
  }
  if(margin < 0){
      BidenECs <- BidenECs+delegates[delegates["State"] == datas[i, "state"],][, 2];
  }
}
  if((10+rlogis(1, 0, 10)) < 0){
    BidenECs <- BidenECs+1;
  }
  if((-10+rlogis(1, 0, 10)) < 0){
    BidenECs <- BidenECs+1;
  }
  simulationsList <- c(simulationsList, BidenECs)
  BidenECs <- 0
}

bidenwin <- sum(simulationsList > 269)/10000*100
tie <- sum(simulationsList == 269)/10000*100
trumpwin <- sum(simulationsList < 269)/10000*100
ecs <- round(mean(simulationsList), 1)

colorcombo <- c("#1b73fe", "#1b73fe", "#1b73fe", "#1b73fe", "#1b73fe", "#1b73fe", "#4d92fe", "#4d92fe", "#80b1fe", "#80b1fe", "#b3d0ff", "#b3d0ff", "#e6efff", "#e6efff", "#e6efff", "#e6efff", "#e6efff", "#e6efff", "#e6efff", "#e6efff", "#ffe8e6", "#ffe8e6", "#ffe8e6", "#ffe8e6", "#ffe8e6", "#ffe8e6", "#ffe8e6", "#ffe8e6", "#ffbbb3", "#ffbbb3", "#fe8e80", "#fe8e80", "#fe604d", "#fe604d", "#fe4832", "#fe4832", "#fe4832", "#fe4832", "#fe4832")

colorcombo <- rep(colorcombo, each = 20)

colorcombo <- c(rep("#1b73fe", 14), unique(colorcombo), rep("#fe4832", 14))

plot_usmap(data = datas, values = "polls", color = "#FFFFFF", label_color = "#FFFFFF") + scale_fill_gradientn(guide="legend", colors=colorcombo, limits=c(-80, 80), breaks=c(-10, -6, -4, -2, 2, 4, 6, 10), labels=c("D+10", "D+6", "D+4", "D+2", "R+2", "R+4", "R+6", "R+10"), name = "Predicted margin") + 
  theme(legend.position = "right") +
  labs(title="Forecast Based on Search Trends",
       subtitle=paste("Partisan media consumption informs the predictions\nProbabilities: ", sep="", bidenwin, "% Biden, ", tie, "% tie, ", trumpwin, "% Trump\n", ecs, " average electoral votes won by Biden"),
       x = NULL,
       y = NULL,
       caption="\nWe regress Google Trends data for\npolarizing media outlets to predict outcomes.") + theme_classic() + theme(text = element_text(size=20),
                                                                                                               plot.background = element_rect(fill = "#F5F5F5"),
                                                                                                               panel.background = element_rect(fill = "#F5F5F5"),
                                                                                                               legend.background = element_rect(fill = "#F5F5F5"),
                                                                                                               panel.grid.major=element_blank(),
                                                                                                               panel.grid.minor=element_blank(),
                                                                                                               panel.border=element_blank(),
                                                                                                               axis.line=element_blank(),
                                                                                                               axis.ticks=element_blank(),
                                                                                                               axis.text.x=element_blank(),
                                                                                                               axis.text.y=element_blank(),
                                                                                                               rect = element_blank(),
                                                                                                               plot.title=element_text(size = 24, face="bold"),
                                                                                                               plot.title.position = "plot",
                                                                                                               plot.subtitle=element_text(face="italic", size=18, margin=margin(b=12)),
                                                                                                               plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e")
       )
