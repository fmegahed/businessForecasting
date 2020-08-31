if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)
retailSales = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=RSCCASN&scale=left&cosd=1992-01-01&coed=2020-07-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2020-08-23&revision_date=2020-08-23&nd=1992-01-01")
retailSales$DATE = ymd(retailSales$DATE)

retailSales$RSCCASN %>% summary()

# Option 1: 
retailSales[seq(12,336, 12), 'RSCCASN'] %>% median() 

# Option 2:
retailSales$MONTH = month(retailSales$DATE)
retailSales %>% filter(MONTH == 12) -> retailSalesDec
retailSalesDec$RSCCASN %>% median()
