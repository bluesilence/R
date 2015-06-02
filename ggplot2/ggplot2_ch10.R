qplot(x, y, data = diamonds, na.rm = TRUE)
last_plot() + xlim(3, 11) + ylim(3, 11)
last_plot() + xlim(4, 10) + ylim(4, 10)
last_plot() + xlim(4, 5) + ylim(4, 5)
last_plot() + xlim(4, 4.5) + ylim(4, 4.5)
last_plot() + geom_abline(colour = "red")

qplot(x, y, data = diamonds, na.rm = T) + geom_abline(colour = "red") + xlim(4, 4.5) + ylim(4, 4.5)

gradient_rb <- scale_colour_gradient(low = "red", high = "blue")
qplot(cty, hwy, data = mpg, colour = displ) + gradient_rb
qplot(bodywt, brainwt, data = msleep, colour = awake, log = "xy") + gradient_rb

xquiet <- scale_x_continuous("", breaks = NULL)
yquiet <- scale_y_continuous("", breaks = NULL)
quiet <- list(xquiet, yquiet)

qplot(mpg, wt, data = mtcars) + quiet
qplot(displ, cty, data = mpg) + quiet
geom_lm <- function(formula = y ~ x) {
  geom_smooth(formula = formula, se = FALSE, method = "lm")
}
qplot(mpg, wt, data = mtcars) + geom_lm()
library(splines)
qplot(mpg, wt, data = mtcars) + geom_lm(y ~ ns(x, 3))

library(reshape2)
library(plyr)
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / diff(rng)
}
pcp_data <- function(df) {
  numeric <- laply(df, is.numeric)
  df[numeric] <- colwise(range01)(df[numeric])
  df$.row <- rownames(df)
  dfm <- melt(df, id = c(".row", names(df)[!numeric]))
  class(dfm) <- c("pcp", class(dfm))
  dfm
}
pcp <- function(df, ...) {
  df <- pcp_data(df)
  ggplot(df, aes(variable, value)) + geom_line(aes(group = .row))
}
pcp(mpg)
pcp(mpg) + aes(colour = drv)
}