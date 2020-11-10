setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # automatically set the working directory

pacman::p_load(tidyverse, # add tidyverse --> (a) dplyr functions and pipe, and (b) for the ggplot2 functions
               magrittr, # have the compound assignment pipe
               fpp2, # it loads the forecast package, accuracy, ses/holt/hw
               lubridate) # to convert DATE from character to a date format

gnp = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GNP&scale=left&cosd=1947-01-01&coed=2020-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2020-10-12&revision_date=2020-10-12&nd=1947-01-01")

gnp %<>% mutate(DATE = ymd(DATE), # converts the DATE column into date
                diff1 = c(NA, diff(gnp$GNP)),
                diff2A = c(NA, NA, diff(gnp$GNP, differences = 2)) # one way for calculating the 2nd diff
                )

# An alternative way for calculating the second difference
diff2 = diff(gnp$GNP) %>% diff() # applied the diff function twice (i.e. 2nd diff) + saved in an object titled diff2
diff2 = c(NA,NA, diff2) # added two NAs in the beginning given that the previous line of code resulted in 291 obsv
gnp$diff2B = diff2 # we needed 293 to be able to add it to the data frame


# makes plotting data in multiple lines a lot easier in ggplot2
df = pivot_longer(data = gnp, # first argument is the data you want to pivot longer
                  cols = c(2,3,4, 5) ) # second argument is the columns that we want to put under value

# Tall Data Plot
df %>% ggplot(aes(x = DATE, y = value, color = name, group = name)) +
  geom_line() + # we are saying we want to create a line plot
  facet_wrap(~ name, ncol = 1, scales = 'free_y') + # creates panels (by row due to ncol =1) for each value of name, 
#scales argument allows us to have different values for the y-axis (so we can actually see what is going on)
  theme_bw() + # black and white theme
  theme(legend.position = 'bottom') + # move legend position to the bottom
  labs(color = 'Time Series') + # renames legend from name (gnpData) to 'Time Series'
  geom_smooth(method = 'lm') # adds a trend line to each of the panels (optional)

mean(gnp$diff1, na.rm = TRUE) # calculate mean and ignore NAs in the calc
mean(gnp$diff2A, na.rm = TRUE) # obviously 2A and 2B are identical (so in actual analysis we only need one)
mean(gnp$GNP) # computing mean of the original data


# After Class Question -- Wide Data Plot
# Main Issue -- How to create multiple panels

# To fix issue it -- we can use the package ggpubr
pacman::p_load(ggpubr)

p1 = gnp %>% ggplot(aes(x = DATE, y = GNP)) + geom_line() + theme_bw() + geom_smooth(method = 'lm')
p2 = gnp %>% ggplot(aes(x = DATE, y = diff1)) + geom_line() + theme_bw() + geom_smooth(method = 'lm')
p3 = gnp %>% ggplot(aes(x = DATE, y = diff2B)) + geom_line() + theme_bw() + geom_smooth(method = 'lm')

ggarrange(p1, p2, p3, ncol = 1) # putting the three plots together
