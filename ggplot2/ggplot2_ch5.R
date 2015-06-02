library(ggplot2)

df <- data.frame(x = rnorm(2000), y = rnorm(2000))

norm <- ggplot(df, aes(x, y))

norm + geom_point()
norm + geom_point(shape = 1)
norm + geom_point(shape = ".")

norm + geom_point(colour = "black", alpha = 1/3)
norm + geom_point(colour = "black", alpha = 1/5)
norm + geom_point(colour = "black", alpha = 1/10)


td <- ggplot(diamonds, aes(table, depth)) + xlim(50, 70) + ylim(50, 70)
td + geom_point()
td + geom_jitter()
jit <- position_jitter(width = 0.5)
td + geom_jitter(position = jit)
td + geom_jitter(position = jit, colour = "black", alpha = 1/10)
td + geom_jitter(position = jit, colour = "black", alpha = 1/50)
td + geom_jitter(position = jit, colour = "black", alpha = 1/200)


d <- ggplot(diamonds, aes(carat, price)) + xlim(1, 3)
d + geom_point()

d + stat_bin2d()
d + stat_bin2d(bins = 10)
d + stat_bin2d(binwidth = c(0.02, 200)) # binwidth for carat is 0.02, binwidth for price is 200

install.packages('hexbin')
library(hexbin)
d + stat_binhex()
d + stat_binhex(bins = 10)
d + stat_binhex(binwidth = c(0.02, 200)) # binwidth for carat is 0.02, binwidth for price is 200


d + geom_point() + geom_density2d()
d + stat_density2d(geom = "point", aes(size = ..density..), contour = F) + scale_size_area()
d + stat_density2d(geom = "tile", aes(fill = ..density..), contour = F)
last_plot() + scale_fill_gradient(limits = c(1e-5, 8e-4))


install.packages('maps')
library(maps)

head(us.cities)
data(us.cities)
big_cities <- subset(us.cities, pop > 500000)
qplot(long, lat, data = big_cities) + borders("state", size = 0.5)
tx_cities <- subset(us.cities, country.etc == "TX")
ggplot(tx_cities, aes(long, lat)) + borders("county", "texas", colour = "grey70") + geom_point(colour = "black", alpha = 0.5)


states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, by = "region")
choro <- choro[order(choro$order), ]
head(choro)

qplot(long, lat, data = choro, group = group, fill = -assault, geom = "polygon")
qplot(long, lat, data = choro, group = group, fill = -assault / murder, geom = "polygon")
qplot(long, lat, data = choro, group = group, fill = -rape, geom = "polygon")


library(plyr)
ia <- map_data("county", "iowa")
head(ia)

mid_range <- function(x) mean(range(x, na.rm = TRUE))
# Calculate the mid lat and mid long for each subregion
centres <- ddply(ia, .(subregion), colwise(mid_range, .(lat, long)))
head(centres)

ggplot(ia, aes(long, lat)) + geom_polygon(aes(group = group), fill = NA, colour = "grey60") + geom_text(aes(label = subregion), data = centres, size = 4, angle = 45)

# Sample with binomial distribution (p == 0.2)
d <- subset(diamonds, carat < 2.5 & rbinom(nrow(diamonds), 1, 0.2) == 1)
d$lcarat <- log10(d$carat)
d$lprice <- log10(d$price)

detrend <- lm(lprice ~ lcarat, data = d)
d$lprice2 <- resid(detrend)

mod <- lm(lprice2 ~ lcarat * color, data = d)

install.packages('effects')
library(effects)
effectdf <- function(...) {
  suppressWarnings(as.data.frame(effect(...)))
}

color <- effectdf("color", mod)
both1 <- effectdf("lcarat:color", mod)

carat <- effectdf("lcarat", mod, default.levels = 50)
both2 <- effectdf("lcarat:color", mod, default.levels = 3)

qplot(lcarat, lprice, data = d, colour = color)
qplot(lcarat, lprice2, data = d, colour = color)

fplot <- ggplot(mapping = aes(y = fit, ymin = lower, ymax = upper)) + ylim(range(both2$lower, both2$upper))
fplot %+% color + aes(x = color) + geom_point() + geom_errorbar()
fplot %+% both2 + aes(x = color, colour = lcarat, group = interaction(color, lcarat)) + geom_errorbar() + geom_line(aes(group = lcarat)) + scale_colour_gradient()

fplot %+% carat + aes(x = lcarat) + geom_smooth(stat = "identity")

ends <- subset(both1, lcarat == max(lcarat))
fplot %+% both1 + aes(x = lcarat, colour = color) + geom_smooth(stat = "identity") + scale_colour_hue() + theme(legend.position = "none") + geom_text(aes(label = color, x = lcarat + 0.02), ends)


midm <- function(x) mean(x, trim = 0.5)
m2 + stat_summary(aes(colour = "trimmed"), fun.y = midm, peom = "point") + stat_summary(aes(colour = "raw"), fun.y = mean, geom = "point") + scale_colour_hue("Mean")


(unemp <- qplot(date, unemploy, data = economics, geom = "line", xlab = "", ylab = "No. unemployed (1000s)"))
presidential <- presidential[-(1:3), ]

yrng <- range(economics$unemploy)
xrng <- range(economics$date)
# Mark vline at the start of each president
unemp + geom_vline(aes(xintercept = as.numeric(start)), data = presidential)


library(scales)
unemp + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = party), ymin = yrng[1], ymax = yrng[2], data = presidential, alpha = 0.2) + scale_fill_manual(values = c("blue", "red"))
last_plot() + geom_text(aes(x = start, y = yrng[1], label = name), data = presidential, size = 3, hjust = 0, vjust = 0)

caption <- paste(strwrap("Unemployment rates in the US have varies a lot over the years", 40), collapse = "\n")
unemp + geom_text(aes(x, y, label = caption), data = data.frame(x = xrng[2], y = yrng[2]), hjust = 1, vjust = 1, size = 4)

highest <- subset(economics, unemploy == max(unemploy))
unemp + geom_point(data = highest, size = 3, colour = "red", alpha = 0.5)


qplot(percwhite, percbelowpoverty, data = midwest)
qplot(percwhite, percbelowpoverty, data = midwest, size = poptotal / 1e6) + scale_size_area("Population\n(millions)", breaks = c(0.5, 1, 2, 4))
qplot(percwhite, percbelowpoverty, data = midwest, size = area) + scale_size_area()

lm_smooth <- geom_smooth(method = lm, size = 1)
qplot(percwhite, percbelowpoverty, data = midwest) + lm_smooth
qplot(percwhite, percbelowpoverty, data = midwest, weight = popdensity, size = popdensity) + lm_smooth


qplot(percbelowpoverty, data = midwest, binwidth = 1)
qplot(percbelowpoverty, data = midwest, weight = poptotal, binwidth = 1) + ylab("population")