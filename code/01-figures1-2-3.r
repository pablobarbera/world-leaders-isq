## Replicate Figures 1 and 2

setwd("~/Dropbox/research/worldleaders/final-submission")
options(stringsAsFactors=F)

results <- read.csv("data01-leader-adoption.csv")
results$month <- as.Date(results$month)

##########################################
## FIGURE 1
##########################################

# empty data frame for plot
df <- data.frame(dates = 
	seq(as.Date("2007-01-01"), 
		as.Date("2015-01-01"), by="month"))
df$twitter <- NA
df$facebook <- NA
df$either <- NA

# for each month, count how many Tw/FB accounts are active
for (i in 1:nrow(df)){
	tab <- results[results$month==df$dates[i],]
	df$twitter[i] <- table(tab$leader_active_tw_any)['1'] / nrow(tab)
	df$facebook[i] <- table(tab$leader_active_fb_any)['1'] / nrow(tab)
	df$either[i] <- table(tab$leader_active_any)['1'] / nrow(tab)
}

# replace NAs with 0s
df$twitter[is.na(df$twitter)] <- 0
df$facebook[is.na(df$facebook)] <- 0
df$either[is.na(df$either)] <- 0
df$dates <- as.character(df$dates)

# reshape so that we can visualize it with ggplot2
library(reshape)
df <- melt(df)
df$dates <- as.Date(df$dates)

library(ggplot2)
library(scales)
p <- ggplot(df, aes(x=dates, y=value, color=variable, group=variable))
pq <- p + geom_line(show_guide=FALSE) + theme_bw() +
	scale_x_date(limits=c(as.Date('2007-01-01'), as.Date('2015-10-30')),
		breaks=as.Date(c("2008-01-01", "2010-01-01", "2012-01-01", "2014-01-01")),
		labels=date_format("%Y")) +
	scale_color_manual(values=c("darkblue", "red", "grey60")) +
	scale_y_continuous("% of countries", label=percent, limits=c(0, 0.82)) +
	ggplot2::annotate("text", x=as.Date("2015-01-15"), y=0.68, size=3, hjust=0, color="red", label="Facebook") +
	ggplot2::annotate("text", x=as.Date("2015-01-15"), y=0.74, size=3, hjust=0, color="blue", label="Twitter") +
	ggplot2::annotate("text", x=as.Date("2015-01-15"), y=0.81, size=3, hjust=0, color="grey60", label="Either") +
	theme(axis.title.x=element_blank()) 
pq

ggsave(pq, file="figure1.pdf", height=2.5, width=5.5)


##########################################
### FIGURE 2
##########################################

library(rworldmap)
library(ggplot2)
library(scales)
library(grid)
library(gpclib)
library(maptools)
gpclibPermit()

# display data as of January 1st, 2014
tab <- results[results$month==as.Date("2014-01-01"),]
tab <- data.frame(country = tab$iso3c, presence = tab$leader_active_any,
    stringsAsFactors=F)
sPDF <- joinCountryData2Map( tab, joinCode = "NAME", 
    nameJoinColumn = "country")
world.data <- fortify(sPDF, region="ISO3")

new.data <- sPDF@data[,c("ISO3", "presence")]
new.data$id <- as.character(new.data$ISO3)

plot.data <- merge(world.data, new.data, all.x=TRUE, sort=FALSE)
plot.data <- plot.data[order(plot.data$order),]

plot <- ggplot(plot.data, aes(long,lat,group=group,fill=factor(presence))) + 
    geom_polygon(color="grey20", size=0.25) +
    expand_limits(x = plot.data$long, y = plot.data$lat) +
    #coord_map(xlim=c(-150, 180), ylim=c(-62,70)) +
    scale_y_continuous(limits=c(-62,85)) +
    #coord_map() +
    scale_fill_manual(values=c("grey80", "grey50"), guide=FALSE, na.value="grey80") +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin=unit(c(-.2,-.4,-1,-1.5), "cm"),
            panel.spacing=unit(c(-.2,-.2,0,-1), "cm"))
#plot
ggsave(plot, file="figure2.pdf", width=6, height=3.50)


#####################################################
####### summary statistics reported in paper  #######
#####################################################

tab <- results[results$month==as.Date("2014-01-01"),]

# how many on ANY social media?
prop.table(table(tab$leader_active_any))

# how many on ANY social media in early 2015?
tab <- results[results$month==as.Date("2015-08-01"),]
prop.table(table(tab$leader_active_any))

# personal vs institutional accounts on Twitter
tab <- results[results$month==as.Date("2014-01-01"),]
prop.table(table(tab$leader_active_tw_personal))
prop.table(table(tab$leader_active_tw_institution))

# personal vs institutional accounts on Facebook
prop.table(table(tab$leader_active_fb_personal))
prop.table(table(tab$leader_active_fb_institution))


##########################################
## FIGURE 3
##########################################

df <- read.csv("data02-user-profiles.csv")

p <- ggplot(df, aes(x=region, y=audience))
pq <- p + geom_boxplot() + facet_grid(~platform) +
	scale_y_log10(label=comma, breaks=c(1000, 100000, 10000000)) + 
	coord_flip() + theme_bw() + theme(axis.title.y = element_blank(),
		axis.title.x = element_blank())
pq

ggsave(pq, file="figure3.pdf", height=2.5, width=8)



