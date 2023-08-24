#Download libraries
library(etherscanr)
library(ggplot2)
library(jsonlite)
library(sqldf)
library(plotly)
library(processx)
library(dplyr)
library(tidyr)
library(httr)
library(lmtest)
library(lpirfs)
# Define the scrapping function
getHistorical_tokentransfert <- function(startt_block, endd_block, address, topic0, topic1, api_key) {
  url1 <- paste("https://api.etherscan.io/api?module=logs&action=getLogs", 
                "&fromBlock=", startt_block,
                "&toBlock="  , endd_block,
                "&address="  , address,
                "&topic0="   , topic0,
                "&topic1="   , topic1,
                "&apikey="   , api_key,
                sep="")
  return(url1)
}
#########################################################################################
#                                       TOKEN PURCHASES
#########################################################################################
#                                       INPUT VARIABLES

weth_address  <- "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2"     #WETH address
#
wallet_buys_1   <- "0x000000000000000000000000d70141861e0a6929b7c7e4aabf4f54962a8da451"   
wallet_buys_2   <- "0x000000000000000000000000248b62fb89dbf4d6836eab93be433ed558834666" 
# smart contract that buys tokens
start_block     <- 17578536                                       #Earliest block for downloading data (start of contract address)
end_block       <- "latest"                                       #Latest block at the date of downloading
terminate_block <- 18078536                                       #Block until which we want to download data
api_key <- "..."                                                  #API key to access etherscan
#
topic_swap <- "0xd013ca23e77a65003c2c659c5442c00c805371b7fc1ebd4c206c41d1536bd90b"
topic_transfer  <- "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef" 
#Solidity event of operation, this hash is for transfers
#########################################################################################
#                                       DOWNLOAD DATA
#########################################################################################
start_block <- 17578536  
add_data <- data.frame(fromJSON(getHistorical_tokentransfert(start_block, end_block, weth_address, topic_transfer, wallet_buys_1, api_key)))
i <- 1
interim <- data.frame(result.blockNumber = tail(as.numeric(add_data$result.blockNumber),1))
while (tail(as.numeric(interim$result.blockNumber),1) < terminate_block) {
  start_block <- tail(as.numeric(interim$result.blockNumber),1) + 1 #shift the start of new download
  interim <- data.frame(fromJSON(getHistorical_tokentransfert(start_block, end_block, weth_address, topic_transfer, wallet_buys_1, api_key)))
  add_data <- rbind(interim,add_data)
  print(i)
  i=i+1
}
add_transfer_data <- add_data  
add_transfer_data <- add_transfer_data[,c(5,6,9,10,8,12)]
colnames(add_transfer_data) <- c("amount","block","gas_price","gas_used","timestamp","txhash")
add_transfer_data$weth <- as.numeric(add_transfer_data$amount)/10^18
add_transfer_data$block <- as.numeric(add_transfer_data$block)
add_transfer_data$timestamp <- as.numeric(add_transfer_data$timestamp)
add_transfer_data$gas <- as.numeric(add_transfer_data$gas_price)*as.numeric(add_transfer_data$gas_used)/10^18
add_transfer_data$date <- as.POSIXct(add_transfer_data$timestamp, origin = "1970-01-01", tz = "GMT")
add_transfer_data_1 <- add_transfer_data
#
start_block <- 17578536 
add_data <- data.frame(fromJSON(getHistorical_tokentransfert(start_block, end_block, weth_address, topic_transfer, wallet_buys_2, api_key)))
i <- 1
interim <- data.frame(result.blockNumber = tail(as.numeric(add_data$result.blockNumber),1))
while (tail(as.numeric(interim$result.blockNumber),1) < terminate_block) {
  start_block <- tail(as.numeric(interim$result.blockNumber),1) + 1 #shift the start of new download
  interim <- data.frame(fromJSON(getHistorical_tokentransfert(start_block, end_block, weth_address, topic_transfer, wallet_buys_1, api_key)))
  add_data <- rbind(interim,add_data)
  print(i)
  i=i+1
}
add_transfer_data <- add_data                         
add_transfer_data <- add_transfer_data[,c(5,6,9,10,8,12)]
colnames(add_transfer_data) <- c("amount","block","gas_price","gas_used","timestamp","txhash")
add_transfer_data$weth <- as.numeric(add_transfer_data$amount)/10^18
add_transfer_data$block <- as.numeric(add_transfer_data$block)
add_transfer_data$timestamp <- as.numeric(add_transfer_data$timestamp)
add_transfer_data$gas <- as.numeric(add_transfer_data$gas_price)*as.numeric(add_transfer_data$gas_used)/10^18
add_transfer_data$date <- as.POSIXct(add_transfer_data$timestamp, origin = "1970-01-01", tz = "GMT")
add_transfer_data_2 <- add_transfer_data
add_transfer_data <- rbind(add_transfer_data_1,add_transfer_data_2)
add_transfer_data <- sqldf("SELECT * FROM add_transfer_data ORDER BY date ASC")
#Check total costs
sum(add_transfer_data$weth)+sum(add_transfer_data$gas)
#Create a graph
m <- list(
  l = 50,
  r = -50,
  b = 100,
  t = 100,
  pad = 4
)
pfig_costs <- plot_ly(add_transfer_data) %>% 
  layout(title = 'AIMBOT costs')%>%
  add_trace(x = ~date, y = ~cumsum(weth)+cumsum(gas),line = list(color = 'rgb(5, 12, 24)', width=3), name = "Total costs",mode = "lines", type = "scatter")%>% 
  add_trace(x = ~date, y = ~cumsum(gas), line = list(color = 'rgb(8,48,107)', width=3, dash = 'dash'), name = "GAS costs",mode = "lines", type = "scatter") %>% 
  add_trace(x = ~date, y = ~cumsum(weth), line = list(color = 'rgb(208,48,107)', width=3, dash = 'dot'), name = "WETH invested",mode = "lines", type = "scatter") %>% 
  layout(yaxis = list(showgrid = F,title="ETH (cumulated)"), xaxis = list(nticks = 10, showgrid = F, title = "", type = 'date', tickformat = "%d-%m"))  %>% layout(font = list(size=18, family ="Gravitas One"),legend = list(orientation = "h", xanchor = "center", y=-0.07,x = 0.5))  %>% 
  layout(autosize = F, width = 900, height = 600, margin = m)
pfig_costs
#orca(pfig_costs, "costs_aimbot.pdf", width = 900) #tool to download figures
#########################################################################################
#                                       TOKEN SELLS
#########################################################################################
#                                       INPUT VARIABLES

weth_address  <- "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2"     #WETH address
#
wallet_address <- "0x99F403fc793C3A674ef7f4CaE3f2aF36D02C0C28"    
# smart contract that gets tokens
start_block     <- 17578536                                       #Earliest block for downloading data (start of contract address)
end_block       <- "latest"                                       #Latest block at the date of downloading
terminate_block <- 18078536                                       #Block until which we want to download data
api_key <- "..."                                                  #API key to access etherscan
#
topic_swap <- "0xd013ca23e77a65003c2c659c5442c00c805371b7fc1ebd4c206c41d1536bd90b"
topic_transfer  <- "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef" 
#Solidity event of operation, this hash is for transfers
#########################################################################################
#                                       DOWNLOAD DATA
#########################################################################################
getHistorical_IncomingETHtransfert <- function(startt_block, endd_block, address, api_key) {
  url <- paste("https://api.etherscan.io/api?module=account&action=txlistinternal", 
               "&startblock=", startt_block,
               "&endblock="  , endd_block,
               "&address="   , address,
               "&sort=asc",
               "&apikey="    , api_key,
               sep="")
  return(url)
}
eth_transfer <- data.frame(fromJSON(getHistorical_IncomingETHtransfert(start_block, end_block, wallet_address, api_key)))
eth_transfer_data <- eth_transfer[,c(8,3,4,5,6)]
colnames(eth_transfer_data) <- c("amount","block","timestamp","txhash","address_from")
eth_transfer_data$weth <- as.numeric(eth_transfer_data$amount)/10^18
eth_transfer_data$block <- as.numeric(eth_transfer_data$block)
eth_transfer_data$timestamp <- as.numeric(eth_transfer_data$timestamp)
eth_transfer_data$date <- as.POSIXct(eth_transfer_data$timestamp, origin = "1970-01-01", tz = "GMT")
eth_transfer_data$earned <- ifelse(eth_transfer_data$address_from=='0xc5be2381867b3a3f85f4eac9e9dcd5f9420339a6',0,eth_transfer_data$weth)
eth_transfer_data$tax <-ifelse(eth_transfer_data$address_from=='0xc5be2381867b3a3f85f4eac9e9dcd5f9420339a6',eth_transfer_data$weth,0)
sum(eth_transfer_data$weth)
#Create a graph
pfig_aimbot <- plot_ly(add_transfer_data) %>% 
  layout(title = 'AIMBOT')%>%
  add_trace(x = ~date, y = ~cumsum(weth)+cumsum(gas),line = list(color = 'rgb(5, 12, 24)', width=3), name = "Total costs",mode = "lines", type = "scatter")%>% 
  add_trace(x = ~date, y = ~cumsum(gas), line = list(color = 'rgb(8,48,107)', width=3, dash = 'dash'), name = "GAS costs",mode = "lines", type = "scatter") %>% 
  add_trace(x = ~date, y = ~cumsum(weth), line = list(color = 'rgb(208,48,107)', width=3, dash = 'dot'), name = "WETH invested",mode = "lines", type = "scatter") %>% 
  add_trace(x = ~eth_transfer_data$date, y = ~cumsum(eth_transfer_data$earned), line = list(color = 'rgb(108,248,107)', width=3), name = "ETH earned",mode = "lines", type = "scatter") %>% 
  add_trace(x = ~eth_transfer_data$date, y = ~cumsum(eth_transfer_data$tax), line = list(color = 'rgb(108,248,107)', width=3, dash = 'dot'), name = "ETH 2% tax",mode = "lines", type = "scatter") %>% 
  layout(yaxis = list(showgrid = F,title="ETH (cumulated)"), xaxis = list(nticks = 4, showgrid = F, title = "", type = 'date', tickformat = "%d-%m"))  %>% layout(font = list(size=18, family ="Gravitas One"),legend = list(orientation = "h", xanchor = "center", y=-0.07,x = 0.5))  %>% 
  layout(autosize = F, width = 900, height = 600, margin = m)
pfig_aimbot
#orca(pfig_aimbot, "aimbot_new.png", width = 900)

