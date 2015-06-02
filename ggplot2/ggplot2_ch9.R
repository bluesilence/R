ddply(diamonds, .(color), subset, carat == min(carat))
ddply(diamonds, .(color), subset, order(carat) <= 2)
ddply(diamonds, .(color), subset, carat > quantile(carat, 0.99))
ddply(diamonds, .(color), subset, price > mean(price))

ddply(diamonds, .(color), transform, price = price - mean(price))

nmissing <- function(x) sum(is.na(x))
nmissing(msleep$name)
nmissing(msleep$brainwt)
nmissing_df <- colwise(nmissing)
nmissing_df(msleep)
colwise(nmissing)(msleep)

msleep2 <- msleep[, -6]
numcolwise(median)(msleep2, na.rm = T)
numcolwise(quantile)(msleep2, na.rm = T)
numcolwise(quantile)(msleep2, probs = c(0.25, 0.75), na.rm = T)
ddply(msleep2, .(vore), numcolwise(median), na.rm = T)
ddply(msleep2, .(vore), numcolwise(mean), na.rm = T)

my_summary <- function(df) {
  with(df, data.frame(
    pc_cor = cor(price, carat, method = "spearman"),
    lpc_cor = cor(log(price), log(carat))
  ))
}

ddply(diamonds, .(cut), my_summary)
ddply(diamonds, .(color), my_summary)
qplot(carat, price, data = diamonds, geom = "smooth", colour = color)
dense <- subset(diamonds, carat < 2)
qplot(carat, price, data = dense, geom = "smooth", colour = color, fullrange = TRUE)


library(mgcv)
smooth <- function(df) {
  mod <- gam(price ~ s(carat, bs = "cs"), data = df)
  grid <- data.frame(carat = seq(0.2, 2, length = 50))
  pred = predict(mod, grid, se = T)
  grid$price <- pred$fit
  grid$se <- pred$se.fit
  grid
}
smoothes <- ddply(dense, .(color), smooth)
qplot(carat, price, data = smoothes, colour = color, geom = "line")
qplot(carat, price, data = smoothes, colour = color, geom = "smooth", ymax = price + 2 * se, ymin = price - 2 * se)


mod <- gam(price ~ s(carat, bs = "cs") + color, data = dense)
grid <- with(diamonds, expand.grid(
  carat = seq(0.2, 2, length = 50),
  color = levels(color)
))
grid$pred <- predict(mod, grid)
qplot(carat, pred, data = grid, colour = color, geom = "line")

ggplot(economics, aes(date)) + geom_line(aes(y = unemploy, colour = "unemploy")) + geom_line(aes(y = uempmed, colour = "uempmed")) + scale_colour_hue("variable")
require(reshape2)
emp <- melt(economics, id = "date", measure = c("unemploy", "uempmed"))
head(emp)
qplot(date, value, data = emp, geom = "line", colour = variable)

range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / diff(rng)
}

emp2 <- ddply(emp, .(variable), transform, value = range01(value))
head(emp2)
qplot(date, value, data = emp2, geom = "line", colour = variable, linetype = variable)
qplot(date, value, data = emp2, geom = "line") + facet_grid(variable ~ ., scales = "free_y")


popular <- subset(movies, votes > 1e4)
ratings <- popular[, 7:16]
ratings$.row <- rownames(ratings)
molten <- melt(ratings, id = ".row")

pcp <- ggplot(molten, aes(variable, value, group = .row))
pcp + geom_line()
pcp + geom_line(colour = "black", alpha = 1/20)
jit <- position_jitter(width = 0.25, height = 2.5)
pcp + geom_line(position = jit)
pcp + geom_line(colour = "black", alpha = 1/20, position = "jitter")


cl <- kmeans(ratings[1:10], 6)
head(cl)
ratings$cluster <- reorder(factor(cl$cluster), popular$rating)
levels(ratings$cluster) <- seq_along(levels(ratings$cluster))
molten <- melt(ratings, id = c(".row", "cluster"))

pcp_cl <- ggplot(molten, aes(variable, value, group = .row, colour = cluster))
install.packages('devtools')
library(devtools)
# Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
# there is no package called 慩ML?Error: package or namespace load failed for 慸evtools?

pcp_cl + geom_line(position = "gitter", alpha = 1/5)


qplot(displ, cty, data = mpg) + geom_smooth(method = "lm")
mpgmod <- lm(cty ~ displ, data = mpg)
mpgmod
#fortify returns the original data and the data of the model
fortify(mpgmod)

mod <- lm(cty ~ displ, data = mpg)
basic <- ggplot(mod, aes(.fitted, .resid)) +
  geom_hline(yintercept = 0, colour = "grey50", size = 0.5) +
  geom_point() +
  geom_smooth(size = 0.5, se = F)
basic
basic + aes(y = .stdresid)
basic + aes(size = .cooksd) + scale_size_area("Cook's distance")
full <- basic %+% fortify(mod, mpg)
full + aes(colour = factor(cyl))
full + aes(displ, colour = factor(cyl))

fortify.Image <- function(model, data, ...) {
  colours <- channel(model, "x11")
  colours <- colours[, rev(seq_len(ncol(colours)))]
  melt(colours, c("x", "y"))
}

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")

library(EBImage)
library(reshape2)
library(ggplot2)
img <- readImage("http://had.co.nz/me.jpg")

qplot(x, y, data = img, fill = value, geom = "tile") + scale_fill_identity() + coord_equal()
