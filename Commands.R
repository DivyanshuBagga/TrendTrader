library(tidyverse);
library(tidyquant);
library(gridExtra);

# Workflow
# First, load tidyverse, tidyquant and gridExtra
# Register on quandl.com, add the api key using quandl_api_key(quandlAPIKey)
# To add a symbol, NSE/ICICIBANK (ICICIBANK table in NSE database on Quandl), run addPrices(symbol), for this example symbol = 'NSE/ICICIBANK' or symbol <- "NSE/NIFTY_500"
# To update the latest prices, of all added symbols, simply run getPrices()
# To remove some symbol, run removePrices(symbol)
# To see the chart, run stockChart(symbol)
# To save Prices to Disk, use write_rds(Prices, "~/Documents/Trading/Tidy/Prices.rds")

# Initialize defaults for golbal variables.
# Run: init()
init <- function(env = globalenv()){
	# env$Index <- tibble(character(),list()) 
	type <-  c("Prices","Index","Commodities","Financials")

	map(. = type, .f = function(type, envir){
		get(type, envir = envir) <- read_rds(paste("~/Documents/Trading/Tidy/", type, ".rds", sep = ""));
		 }, envir = env);
}

# save defaults for golbal variables
# Run: saveState()
saveState <- function(env = globalenv()){
	type <-  c("Prices","Index","Commodities","Financials")

	map(. = type, .f = function(type, envir){
		get(type, envir = envir) %>%
		 write_rds(paste("~/Documents/Trading/Tidy/", type, ".rds", sep = ""));
		 }, envir = env);
}


# Updates the Prices in global environment.
# Only queries and adds fresh entries
# Example: getPrices()
# other supported types are "Index", "Commodities", and "Financials"
getPrices <- function(env = globalenv()){

	type <-  c("Prices","Index","Commodities")
	map(. = type, .f = possibly( function(type, envir){
	  	assign(x = type, envir = envir, 
	  			value = transmute( get(type, envir = envir), symbol = symbol, data = map(.x = symbol, updatePrices, type = type, env = envir) )
	  			 );
  		 }, paste(type, "giving error")), envir = env);
}

updatePrices <- function(sym, type = "Prices", env = globalenv()){
	get(type, envir = env) %>%
	filter(symbol == sym) %>%
	select(data) %>%	 
 	map_dfr(1) %>%
 	filter(!is.na(date)) -> tib # To view latest, use tail(tib) 	
  	timetk::tk_xts(tib,silent = TRUE) -> prices;
  	edate <- end(prices);
  	if (weekdays(edate) == "Friday") edate <- edate + days(2);
	if(today() > edate) {
		tq_get(sym, get = 	"quandl", from = edate + days(1), to = today()) %>%
		filter(!is.na(date) & !is.na(open)) -> latest;
		if(type == "Prices") latest <- rename(latest, volume = total.trade.quantity);
		if(type == "Index") latest <- rename(latest, volume = shares.traded);
		tib <- rbind(tib,latest);
		tib;
	} else { tib; }
}

# Add new symbol to Prices
# Example: type <- "index";
#			sym <- "NSE/NIFTY_IT";
#			addPrices(sym,type);	
addPrices <- function(sym, type = "Prices", env = globalenv()){
	get(type, envir = env) %>%
	filter(symbol == sym) %>%
	count() -> occurs;
	if(occurs == 0){
		tq_get(sym, get = 	"quandl") %>%
		filter(!is.na(date) & !is.na(open)) -> added;
		if(type == "Prices") added <- rename(added, volume = total.trade.quantity);
		if(type == "Index") added <- rename(added, volume = shares.traded);
		assign(x = type, envir = env
				value = rbind(get(type, env), tibble(symbol = sym, data = list(added)))
				);
	} else{
		getPrices();
	}
}

# Remove existing symbol from Prices
# Example: type <- "financial";
#			sym <- "NSE/NIFTY_500";
#			removePrices(sym,type);	
removePrices <- function(sym, type = "Prices", env = globalenv()){
	assign(x = type, envir = env
			value = filter(get(type, env), symbol != sym)
		  );
}

# Find the lowest price in given time period (default 6 months)
# Usage:type <- "index"
#		sym <- "NSE/NIFTY_500" 
#		findMin(sym,type = type)
findMin <- function(sym, time.period = months(6), type = "Prices", env = globalenv()){
	get(type, envir = globalenv()) %>%
	filter(symbol == sym) %>%
	select(data) %>%
	map_dfr(1) %>%
	filter(date <= today() & date >= (today()-time.period)) %>%
	summarize(min(open, na.rm = TRUE),min(close, na.rm = TRUE)) %>%
	min();
}

# Find the highest price in given time period (default 6 months)
# Usage: findMax(sym, type = type)
findMax <- function(sym, time.period = months(6), type = "Prices", env = globalenv()){
	get(type, envir = globalenv()) %>%
	filter(symbol == sym) %>%
	select(data) %>%
	map_dfr(1) %>%
	filter(date <= today() & date >= (today()-time.period)) %>%
	summarize(max(open, na.rm = TRUE),max(close, na.rm = TRUE)) %>%
	max();
}

# Find the greatest volume in given time period (default 6 months)
# Usage: findPeakVolume(sym, type = type)
findPeakVolume <- function(sym, time.period = months(6), type = "Prices", env = globalenv()){
	get(type, envir = globalenv()) %>%
	filter(symbol == sym) %>%
	select(data) %>%
	map_dfr(1) %>%
	filter(date <= today() & date >= (today()-time.period)) %>%
	mutate(volume = sqrt(volume)) %>%
	summarize(max(volume, na.rm = TRUE));	
}

# Reads the financials from the CSV format
# Example:path <- "~/Documents/HDFCBank/ProfitLoss.csv"
#		 symbol <- "NSE/HDFCBANK"
#		 getFinancial(sym,path)
getFinancial <- function(symbol, path){

	useful <- c("Sales","Expenses","Operating Profit","Other Income","Depreciation","Interest","Tax","Net profit","Price","Market Cap","EPS");

	read_csv(path) %>%
	filter(`Rs Cr` %in%useful) %>%
	select(`Rs Cr`, starts_with("Mar"), Trailing) %>%
	gather(year, finances, -`Rs Cr`, ) %>%
	mutate(finances = parse_number(finances)) %>%
	spread(`Rs Cr`, finances, convert = TRUE) -> fin
	
	env$Financials <- rbind(env$Financials, tibble(symbol = c(symbol), data = list(fin)));

}


# Shows chart for the given symbol and time period (default: 6 months).
# Usage: stockChart(sym)
stockChart <- function(sym, time.period = months(6), withVolume = FALSE, type = "Prices", env = globalenv()){
	get(type, envir = globalenv()) %>%
	filter(symbol == sym) %>%
	select(data) %>%
	map_dfr(1) -> priceData
	if(withVolume) {
		priceData <- mutate(priceData, volume = sqrt(total.trade.quantity)) 
	}
	
    priceGraph <- ggplot(priceData, aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(ma_fun = SMA, n = 50, linetype = 5, size = 0.25) +
    geom_ma(ma_fun = SMA, n = 200, color = "green", size = 0.25) + 
    labs(title = paste(sym," Candlestick Chart"), 
         subtitle = "50 and 200-Day SMA", 
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(today() - time.period, today()),
    			  ylim = c(0.96*findMin(sym, time.period), 1.04*findMax(sym, time.period))) +
    theme_tq();
    
    if(withVolume){
	    volumeGraph <- ggplot(priceData, aes(x = date, y = volume)) +
    	geom_segment(aes(xend = date, yend = 0, color = volume), na.rm = TRUE) + 
    	labs(y = "Sq. Root of Volume", x = "") +
    	coord_x_date(xlim = c(today() - time.period, today()),
    			  ylim = c(0, 10000)) + #issue: using findPeakVolume instead of 2500 gives error
    	theme_tq() +
    	theme(legend.position = "none") 

		grid.arrange(priceGraph,volumeGraph, nrow = 2, heights = c(2,1))
		} else{
		priceGraph;
		}	
}

# To get the updated Inflation estimates
Inflation <- Quandl::Quandl("ODA/IND_PCPIPCH", api_key="5x-HQqm_rkLXyjTCMtHu")
ConsumerInflation <- Quandl::Quandl("FRED/FPCPITOTLZGIND", api_key="5x-HQqm_rkLXyjTCMtHu")

 



