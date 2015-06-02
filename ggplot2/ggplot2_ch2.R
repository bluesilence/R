library(ggplot2)

head(diamonds)

set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]

qplot(carat, price, data = diamonds)

qplot(log(carat), log(price), data = diamonds)

qplot(carat, x * y * z, data = diamonds)

qplot(carat, price, data = dsmall, colour = color)

qplot(carat, price, data = dsmall, shape = cut)


qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))


qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 1)

qplot(carat, price, data = diamonds, geom = c("point", "smooth"))

library(mgcv)

qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "gam", formula = y ~ s(x))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "gam", formula = y ~ s(x, bs = "cs"))

library(splines)

qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm")
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm", formula = y ~ ns(x, 5))

qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 5))
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 50))
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 200))

qplot(color, price / carat, data = diamonds, geom = "boxplot")
qplot(color, price / carat, data = diamonds, geom = "boxplot", colour = cut)
qplot(color, price / carat, data = diamonds, geom = "boxplot", size = cut)
qplot(color, price / carat, data = diamonds, geom = "boxplot", shape = clarity)


qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1, xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01, xlim = c(0,3))

qplot(carat, data = diamonds, geom = "density")
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "density", fill = color)

qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) + scale_y_continuous("carat")



qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")


year <- function(x) as.POSIXlt(x)$year + 1900

qplot(unemploy / pop, uempmed, data = economics, geom = c("point", "path"))
qplot(unemploy / pop, uempmed, data = economics, geom = "path", colour = year(date))

qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
qplot(carat, ..density.., data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))


qplot(
    carat, price, data = dsmall,
    xlab = "Price ($)", ylab = "Weight (carats)",
    main = "Price-weight relationship"
)

qplot(
  carat, price/carat, data = dsmall,
  ylab = expression(frac(price, carat)),
  xlab = "Weight (carats)"，
  main = "Small diamonds",
  xlim = c(.2, 1)
)

qplot(carat, price, data = dsmall, log = "xy")
