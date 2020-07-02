library(gtrendsR)

preDestTwo = c("2016-12-07 2017-01-07", "2016-12-07 2017-01-07", "2017-01-07 2017-02-07", "2017-02-07 2017-03-07", "2017-03-07 2017-04-07", "2017-04-07 2017-05-07", "2017-05-07 2017-06-07", "2017-06-07 2017-07-07", "2017-07-07 2017-08-07", "2017-08-07 2017-09-07", "2017-09-07 2017-10-07", "2017-10-07 2017-11-07", "2017-11-07 2017-12-07")
preDestTwo = c(preDestTwo, "2017-12-07 2018-01-07", "2018-01-07 2018-02-07", "2018-02-07 2018-03-07", "2018-03-07 2018-04-07", "2018-04-07 2018-05-07", "2018-05-07 2018-06-07", "2018-06-07 2018-07-07", "2018-07-07 2018-08-07", "2018-08-07 2018-09-07", "2018-09-07 2018-10-07", "2018-10-07 2018-11-07", "2018-11-07 2018-12-07")
preDestTwo = c(preDestTwo, "2018-12-07 2019-01-07", "2019-01-07 2019-02-07", "2019-02-07 2019-03-07", "2019-03-07 2019-04-07", "2019-04-07 2019-05-07", "2019-05-07 2019-06-07", "2019-06-07 2019-07-07", "2019-07-07 2019-08-07", "2019-08-07 2019-09-07", "2019-09-07 2019-10-07", "2019-10-07 2019-11-07", "2019-11-07 2019-12-07")
preDestTwo = c(preDestTwo, "2019-12-07 2020-01-07", "2020-01-07 2020-02-07", "2020-03-07 2020-04-07", "2020-04-07 2020-05-07", "2020-05-07 2020-06-07", "2020-06-07 2020-06-26")


dateSeries <- c()
msnbcSeries <- c()
nytSeries <- c()
foxnewsSeries <- c()
huffingtonpostSeries <- c()
washingtonpostSeries <- c()

for(i in 1:length(preDestTwo)){
  
#preDest = "today 12-m"

preDest = preDestTwo[i]

#preDest = "today 1-m"

#preDest = "now 7-d"

#preDest = "now 1-d"

msnbcGeo <- gtrends(c("msnbc"), time=preDest, geo = c("US"))[["interest_by_region"]]

msnbc <- (cbind(msnbcGeo[["location"]], msnbcGeo[["hits"]]))

nytGeo <- gtrends(c("new york times"), time=preDest, geo = c("US"))[["interest_by_region"]]

nyt <- (cbind(nytGeo[["location"]], nytGeo[["hits"]]))

foxnewsGeo <- gtrends(c("fox news"), time=preDest, geo = c("US"))[["interest_by_region"]]

foxnews <- (cbind(foxnewsGeo[["location"]], foxnewsGeo[["hits"]]))

huffingtonpostGeo <- gtrends(c("huffington post"), time=preDest, geo = c("US"))[["interest_by_region"]]

huffingtonpost <- (cbind(washingtonpostGeo[["location"]], huffingtonpostGeo[["hits"]]))

huffingtonpost[is.na(huffingtonpost)] <- 4

washingtonpostGeo <- gtrends(c("washington post"), time=preDest, geo = c("US"))[["interest_by_region"]]

washingtonpost <- (cbind(washingtonpostGeo[["location"]], washingtonpostGeo[["hits"]]))

dataframe <- merge(msnbc, nyt, by = 1)

dataframe <- merge(dataframe, foxnews, by = 1)

dataframe <- merge(dataframe, huffingtonpost, by = 1)

dataframe <- merge(dataframe, washingtonpost, by = 1)

dataframe <- merge(dataframe, read.csv("Delegates.csv", stringsAsFactors = FALSE)[c(1, 3, 4)], by = 1)

msnbc <- as.numeric(as.character(dataframe[, c(2)]))-min(as.numeric(as.character(dataframe[, c(2)])))
msnbc <- msnbc/max(msnbc)

nyt <- as.numeric(as.character(dataframe[, c(3)]))-min(as.numeric(as.character(dataframe[, c(3)])))
nyt <- nyt/max(nyt)

foxnews <- as.numeric(as.character(dataframe[, c(4)]))-min(as.numeric(as.character(dataframe[, c(4)])))
foxnews <- 1-(foxnews/max(foxnews))

huffingtonpost <- as.numeric(as.character(dataframe[, c(5)]))-min(as.numeric(as.character(dataframe[, c(5)])))
huffingtonpost <- huffingtonpost/max(huffingtonpost)

washingtonpost <- as.numeric(as.character(dataframe[, c(6)]))-min(as.numeric(as.character(dataframe[, c(6)])))
washingtonpost <- washingtonpost/max(washingtonpost)

redo <- (47.81-22.11*msnbc-21.11*nyt-foxnews*53.04-huffingtonpost*18.96-washingtonpost*26.89)

redo <- (redo+correction*(3/4))

redo <- (redo-median(redo))

dateSeries <- cbind(dateSeries, redo)

msnbcSeries <- cbind(msnbcSeries, msnbc)

nytSeries <- cbind(nytSeries, nyt)

foxnewsSeries <- cbind(foxnewsSeries, foxnews)

huffingtonpostSeries <- cbind(huffingtonpostSeries, huffingtonpost)

washingtonpostSeries <- cbind(washingtonpostSeries, washingtonpost)

}

dateSeries <- cbind(as.character(dataframe[, 1]), dateSeries)
