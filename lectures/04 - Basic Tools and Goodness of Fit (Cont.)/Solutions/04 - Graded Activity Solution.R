if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

gdp = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2020-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2020-08-26&revision_date=2020-08-26&nd=1947-01-01")
gdp$DATE = ymd(gdp$DATE)
gdp$GDP = log(gdp$GDP)
gdp$GDP = c(rep(NA, 4), diff(gdp$GDP, 4))
gdp %>% na.omit() %>% 
  ggplot(aes(x = DATE, y = GDP)) + 
  geom_line() +
  labs(title = "Differences, with lag = 4, of the Log GDP",
       x = "Date", y = "DL(GDP)",
       caption = "Data from FRED") +
  scale_x_date() + theme_bw()