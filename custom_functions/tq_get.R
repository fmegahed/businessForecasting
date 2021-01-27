# This script is written to overcome the timetk clash for some mac users
# I have created a function called tq_get, which you can use in lieu of the tq_get function from tidyverse
# As of now, the function can only pull stock prices or stock data

if(require(pacman)==FALSE) install.packages('pacman') # install pacman if needed
pacman::p_load(tidyverse, magrittr, quantmod) # load required packages

tq_get = function(x, get = 'stock.prices', from = '2011-01-03', to = Sys.Date()){
  if(is.character(x) == FALSE){ 
    stop(call. = FALSE, "get_symbols(): Please use a character input to 'x'.") }
  
  if(length(get) > 1){
    stop(call. = FALSE, "get_symbols(): Please use only one value for `get` source.") }
  
  if(get == 'stock.prices'){
    src_value <- 'yahoo'} else if (get == 'economic.data'){
      src_value <-'FRED'
    } else {
      stop(call. = FALSE, "In our class, we are only using one of the following inputs to `get`: `stock.prices` or `economic.data` ")
    } 
      
  if (is.character(x) && length(x) == 1 && length(get) == 1){
    quantmod::getSymbols(Symbols = x, src = src_value, from = from, to = to)
    symbol_names <- x
    ret <- data.frame(date = get(x) %>% time(),
                      open = get(x) %>% .[,1] %>% as.vector(),
                      high = get(x) %>% .[,2] %>%  as.vector(),
                      low = get(x) %>% .[,3] %>% as.vector(),
                      close = get(x) %>% .[,4] %>%  as.vector(),
                      volume = get(x) %>% .[,5] %>% as.vector(),
                      adjusted = get(x) %>% .[,6] %>% as.vector())
    ret <- ret %>% tibble::add_column(symbol = symbol_names, .before = 1)
  } else if (is.character(x) && length(x) >= 2 && length(get) == 1){
    quantmod::getSymbols(Symbols = x, src = src_value, from = from, to = to)
    ret <- NULL
    for (i in 1:length(x)) {
      symbol_names <- x[i]
      temp <- data.frame(date = get(x[i]) %>% time(),
                        open = get(x[i]) %>% .[,1] %>% as.vector(),
                        high = get(x[i]) %>% .[,2] %>%  as.vector(),
                        low = get(x[i]) %>% .[,3] %>% as.vector(),
                        close = get(x[i]) %>% .[,4] %>%  as.vector(),
                        volume = get(x[i]) %>% .[,5] %>% as.vector(),
                        adjusted = get(x[i]) %>% .[,6] %>% as.vector())
      temp <- temp %>% tibble::add_column(symbol = symbol_names, .before = 1)
      ret <- rbind(ret, temp)
    }
  }
  return(ret)
}