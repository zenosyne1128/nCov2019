##### 1 draw dynamic geographic maps
library('maps')
library(nCov2019)
from = "2020-01-15"
to = "2020-03-20"
y <- load_nCov2019(lang = 'en')

# To generate a historical world map;
# with default figure size and save with default filename
# the gif file will be saved in current working directory 
plot(y, from = from, to = to, filename = "global1.gif")


#second method for dynamic drawing
library(nCov2019)
library(maps)
y <- load_nCov2019(lang = 'en')
library(magick)

d <- c(paste0("2020-02-", 10:29), paste0("2020-03-0", 1:9))  #from 02-20 to 03-09
img <- image_graph(600, 450, res = 96)
out <- lapply(d, function(date){
  p <- plot(y, date=date,
            label=FALSE, continuous_scale=FALSE)
  print(p)
})

dev.off()
animation <- image_animate(img, fps = 2)
image_write(animation, "global2.gif")


##### 2 Outbreak Trend Curves of Top ten Countries Around the World.
library('ggrepel')
library('ggplot2')
y <- load_nCov2019(lang = 'en')
df <- y['global']
d <- subset(df, time == time(y))
t10 <- d[order(d$cum_confirm,decreasing = T),]$country[1:10]
df <- df[which(df$country %in% t10),]                      #substract top 10 countries

p <- ggplot(df, aes(time, as.numeric(cum_confirm), 
               group = country, color = country)) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = country),
                   data = df[df$time == "2020-03-20", ], hjust = 1) + 
  theme_bw() + theme(legend.position = 'none') +
  xlab(NULL) + ylab(NULL) + 
  scale_x_date(date_labels = "%Y-%m-%d",
               limits = c(as.Date("2020-02-01"), as.Date("2020-03-27"))) +
  coord_cartesian(ylim = c(0,100000)) +                          
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(title = "Outbreak Trend Curves of Top 10 Countries Around the World \n ")
print(p)

#### 3 Change Rate of Top ten Countries Around the World
library('ggrepel')
library('ggplot2')
y <- load_nCov2019(lang = 'en')
df <- y['global']
d <- subset(df, time == time(y))
t10 <- d[order(d$cum_confirm,decreasing = T),]$country[1:10]
df <- df[which(df$country %in% t10),]                      #substract top 10 countries
rate <- df$cum_confirm
for (i in 1:length(df$time)){
  da <- subset(df, time == df$time[i]-1 & country == df$country[i])
  if(length(row.names(da)) == 0){
    rate[i] <- 0                          # define the rate of first confirm is 0
  }
  else
    rate[i] <- (df$cum_confirm[i]-da$cum_confirm)/df$cum_confirm[i]
}
df$rate <- rate
p <- ggplot(df, aes(time, as.numeric(rate), 
                    group = country, color = country)) +
  geom_point() + geom_line() +
  geom_label_repel(aes(label = country),
                   data = df[df$time == "2020-03-20", ], hjust = 1) + 
  theme_bw() + theme(legend.position = 'none') +
  xlab(NULL) + ylab(NULL) + 
  scale_x_date(date_labels = "%Y-%m-%d",
               limits = c(as.Date("2020-02-01"), as.Date("2020-03-27"))) +
  coord_cartesian(ylim = c(0,1)) +                          
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(title = "Outbreak Rate Curves of Top 10 Countries Around the World \n ")
print(p)
