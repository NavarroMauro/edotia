RESULTS
========================================================

```r
# Hosts analysis (Heliaster helianthus) calling libraries and and database
library("ggplot2")  # Plotting library
library(plyr)  # summarise library
```




```r
# Calling data base Edotia-DB
edotiadb <- read.csv("/home/mauro/Documents/edotia/lifehistory/dbedotia/edotiadb.csv", 
    header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
```



```r
# Changing to more friendly headers names. NULL indicate that this column
# have to be deleted
edotiadb <- transform(edotiadb, H.density = heliaster.density.m2, heliaster.density.m2 = NULL)
edotiadb <- transform(edotiadb, A.sampled = sampled.area.m2, sampled.area.m2 = NULL)
edotiadb <- transform(edotiadb, H.size = size.heliaster, size.heliaster = NULL)
edotiadb <- transform(edotiadb, E.sex = edotia.sex, edotia.sex = NULL)
edotiadb <- transform(edotiadb, E.large = large, large = NULL)
edotiadb <- transform(edotiadb, N.eggs = no.eggs, no.eggs = NULL)
edotiadb <- transform(edotiadb, E.width = widht, widht = NULL)
edotiadb <- transform(edotiadb, H.id = id, id = NULL)
```



```r
# Filtering data base to use just H.helianthus with isopods
# (JFM=Juveniles, Females, Males and Ovigerous females)
edotiaJFM <- edotiadb[edotiadb$isopods == 1, ]
```



```r
# Doing some data mining. For example request for headers names use the
# function names() or get the header and the first rows of your data base
# with the command head(), in that way you will have and idea how they
# look.  Get summary mean, sd and length(count or nrow)

edotiadb.filtered <- ddply(edotiadb, .(H.id, H.size, E.sex), summarise, mean.E.width = mean(E.width, 
    na.rm = TRUE), sd.E.width = sd(E.width, na.rm = TRUE), mean.E.large = mean(E.large, 
    na.rm = TRUE), sd.E.large = sd(E.large, na.rm = TRUE), count = length(E.large))
```



```r
edotiaJFM.summarised <- ddply(edotiaJFM, .(H.id, H.size, E.sex), summarise, 
    mean.E.width = mean(E.width, na.rm = TRUE), sd.E.width = sd(E.width, na.rm = TRUE), 
    mean.E.large = mean(E.large, na.rm = TRUE), sd.E.large = sd(E.large, na.rm = TRUE), 
    count = length(E.large))
```



```r
# PLOTS Faceted bar distribution plot: size distribution of Edotia dahli
# over Heliaster helianthus of different sizes
fig.1 <- ggplot(edotiaJFM, aes(E.large, ..count..))
fig.1 <- fig.1 + geom_histogram(binwidth = 0.5)
fig.1 <- fig.1 + facet_grid(E.sex ~ H.size)
fig.1 <- fig.1 + opts(title = "Non title")
fig.1 <- fig.1 + labs(list(x = "Heliaster size (cm)", y = "Edotia dalhi number"))
fig.1 <- fig.1 + theme_bw()
# fig.1 <- fig.1 + opts(panel.background = theme_rect(fill='white',
# colour='grey')) # To change the panel's background color
print(fig.1)
```

![plot of chunk ggplot2](figure/ggplot2.png) 



# ggplot2 Faceted barplot
fig.2 <- ggplot(edotiaJFM, aes(H.size, fill=E.sex))
fig.2 <- fig.2 + geom_bar(binwidth = 0.5, position="dodge")
fig.2 <- fig.2 + scale_fill_grey()
fig.2 <- fig.2 + opts(panel.background = theme_rect(fill='white', colour='grey')) # To change the panel's background color
print(fig.2)

# ggplot2 faceted dotplot
fig.5 <- ggplot(edotiaJFM, aes(E.width, E.large))
fig.5 <- fig.5 + geom_point(na.rm=TRUE)
fig.5 <- fig.5 + facet_wrap(~E.sex)
fig.5 <- fig.5 + stat_smooth(method="lm", se=TRUE)
fig.5 <- fig.5 + opts(panel.background = theme_rect(fill='white', colour='grey'))
print(fig.5)

# ggplot2 dotplot x sex ALL TOGETHER!!!
fig.6 <- ggplot(edotiadb, aes(E.width, E.large, shape = E.sex))
fig.6 <- fig.6 + scale_shape(solid = FALSE)
fig.6 <- fig.6 + geom_point(na.rm=TRUE)
fig.6 <- fig.6 + opts(panel.background = theme_rect(fill='white', colour='grey'))
print(fig.6)
