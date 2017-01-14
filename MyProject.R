library(sqldf)
library(quantmod)
library(tseries)
library(timeDate)
symbols <- c("MSFT", "AAPL", "TSLA", "GOOG")
nrStocks = length(symbols)
dateStart <- "2010-01-01"
dateend <-"2015-12-31"
DateToday <- Sys.Date()
Symbol<-0
z <- zoo()


for (s in symbols) {
  cat("Downloading ", s, " out of ", nrStocks , "n")
   x[[s]]<- get.hist.quote(instrument = s, start = dateStart, end=dateend, quote = "AdjClose", retclass = "zoo", quiet = T)
  #zzz <- merge(z, x)
}

OnInit(){
  Tradepool <<- data.frame(Symbol=character(), OrderType=character(),price=numeric(),Amount=numeric(), Stoploss=numeric(), Takeprofit=numeric(), stringsAsFactors = FALSE)
  HistoricTradepool <<- data.frame(Symbol=character(), OrderType=character(),price=numeric(),Amount=numeric(), Stoploss=numeric(), Takeprofit=numeric(), stringsAsFactors = FALSE)
}

OnBar<-function(){
account<-1
for(i in 3:length(Symbol)){
  Price<<-Symbol[[i]]
  OrderSend("MSFT", "OP_BUY", 10000, 2, 2)
  if(Symbol[[i]]>Symbol[[i-1]] ){
    
  } else{
  }
  account=account+1
  if(length(Symbol)-1==account){ #You put the OnDeinit functions here
    print("this is last")
  }
}

return(account)
}
OnBar()

OrderSend<-function(Security, OrderType1, Amount1, SL, TP){
  Amount2<-100000
  SL2<-Price-(Price*SL/100)
  TP2<-Price+(Price*TP/100)
  Tradepool[length(Tradepool[,1])+1,]<<-c(Security, OrderType1, Price,  Amount2, SL2, TP2)
 # data2[i,]<-c("MSFT", 3+i, 3+i ,4+i)
  #data2[,0]
  #data2[0,1]
  #data2 <- data2[-c(2, 4, 6), ]
  #data2 <- data2[-c(1), ]
}

Download <-function(){

  sqldf("attach 'Shares.sqlite' as new")
  db <- dbConnect(SQLite(), dbname="Shares.sqlite2")
  symbols <- c("MSFT", "AAPL", "TSLA", "GOOG")
  nrStocks = length(symbols)
  x<-list()
  dateStart <- "2000-01-01"
  #dateend <-"2015-12-31"
  dateend <- Sys.Date()
  for (s in symbols) {
    cat("Downloading ", s, " out of ", nrStocks , "n")
    x[[s]]<- get.hist.quote(instrument = s, start = dateStart, end=dateend, quote = "AdjClose", retclass = "zoo", quiet = T)
    Data<- x[[s]]
    Date <- index(Data)
    y <- coredata(Data)
    z <- as.data.frame(cbind(Date,y))
    dbWriteTable(conn=db, s, z, append=TRUE
  }
}

CheckCloseTrade<-function(){
  for(i in length(Tradepool):1){
    if(Tradepool[i,2]=="OP_BUY"){
      HistoricTradepool[length(HistoricTradepool[,1])+1,]<<-(Tradepool[i,1],Tradepool[i,2],Tradepool[i,3],Tradepool[i,4],Tradepool[i,5],Tradepool[i,6])
      Tradepool<<-Tradepool[-c(i),]
    }
  }
}

ReadData<-function(ShareName){
  library(sqldf)
  db <- dbConnect(SQLite(), dbname="Shares2.sqlite3")
  a <- dbReadTable(conn=db, ShareName)
  index.date <- as.Date(a[[1]]
  ShareName <- zoo(a[-1], index.date)
  Symbol<<-zoo(a[-1], index.date)
}



