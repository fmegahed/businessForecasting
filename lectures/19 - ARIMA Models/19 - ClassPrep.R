
pacman::p_load(tidyverse, fpp2, ggfortify, lubridate, zoo, xts)
df = read.csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GNP&scale=left&cosd=1947-01-01&coed=2020-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2020-10-26&revision_date=2020-10-26&nd=1947-01-01')
df = df[-294,]

GNP = xts(df$GNP, freq=4, Start = '1947-01-01', order.by = ymd(df$DATE)) 
ndiffs(GNP)

gnpDiff2 = diff(GNP, differences = 2)
acf(gnpDiff2, na.action = na.pass)
pacf(gnpDiff2, na.action = na.pass)

model1 = Arima(GNP, order = c(2,2,1))
checkresiduals(model1)
forecast(model1, h=10) %>% autoplot() + geom_line(aes(y=fitted(model1)), col = 'blue')
