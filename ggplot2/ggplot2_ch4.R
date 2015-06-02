library(ggplot2)

p <- ggplot(diamonds, aes(carat, price, colour = cut))

p <- p + layer(geom = "point")
p

p <- ggplot(diamonds, aes(x = carat))
p <- p + layer(
  geom = "bar",
  geom_params = list(fill = "steelblue"),
  stat = "bin",
  stat_params = list(binwidth = 2)
)
p

geom_histogram(binwidth = 2, fill = "steelblue")

ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point()
qplot(sleep_rem / sleep_total, awake, data = msleep)

qplot(sleep_rem / sleep_total, awake, data = msleep) + geom_smooth()
qplot(sleep_rem / sleep_total, awake, data = msleep, geom = c("point", "smooth"))
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point() + geom_smooth()

p <- ggplot(msleep, aes(sleep_rem / sleep_total, awake))
summary(p)

p <- p + geom_point()
summary(p)

library(scales)
bestfit <- geom_smooth(method = "lm", se = F, colour = alpha("steelblue", 1 / 2), size = 2)

qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(awake, brainwt, data = msleep, log = "y") + bestfit
qplot(bodywt, brainwt, data = msleep) + bestfit
qplot(bodywt, brainwt, data = msleep, log = "xy") + bestfit


p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p
mtcars <- transform(mtcars, mpg = mpg ^ 2)
class(mtcars)
class(p)

p %+% mtcars
p

p <- ggplot(mtchars)
summary(p)

p <- ggplot(mtcars, aes(x = mpg, y = wt))
p + geom_point()
p + geom_point(aes(colour = factor(cyl)))
p + geom_point(aes(y = disp))


p <- ggplot(mtcars, aes(mpg, wt))
p + geom_point(colour = "blue")
p + geom_point(aes(colour = "blue"))


library(nlme)
head(Oxboys)
p <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()
p
p <- ggplot(Oxboys, aes(age, height)) + geom_line()
p

boysbox <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
boysbox + geom_line(aes(group = Subject), colour = "#3366FF")

?approx

xgrid <- with(df, seq(min(x), max(x), length = 50))
interp <- data.frame(
  x = xgrid,
  y = approx(df$x, df$y, xout = xgrid)$y,
  colour = approx(df$x, df$colour, xout = xgrid)$y
)
qplot(x, y, data = df, colour = colour, size = I(5)) + geom_line(data = interp, size = 2)

ggplot(diamonds, aes(carat)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1)

qplot(carat, ..density.., data = diamonds, geom = "histogram", binwidth = 0.1)

d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
d + stat_bin(aes(y = ..count..), binwidth = 0.1, geom = "area")
d + stat_bin(
  aes(size = ..density..), binwidth = 0.1,
  geom = "point", position = "identity"
)

# y = 1 will throw error
d + stat_bin(
  aes(y = 1, fill = ..count..), binwidth = 0.1,
  geom = "tile", position = "identity"
)


require(nlme, quiet = TRUE, warn.conflicts = FALSE)
model <- lme(height ~ age, data = Oxboys, random = ~ 1 + age | Subject)
oplot <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()
age_grid <- seq(-1, 1, length = 10)
subjects <- unique(Oxboys$Subject)

preds <- expand.grid(age = age_grid, Subject = subjects)
preds$height <- predict(model, preds)
oplot + geom_line(data = preds, colour = "blue", size = 0.4)

Oxboys$fitted <- predict(model)
Oxboys$resid <- with(Oxboys, fitted - height)

oplot %+% Oxboys + aes(y = resid) + geom_smooth(aes(group = 1))

model2 <- update(model, height ~ age + I(age ^ 2))
Oxboys$fitted2 <- predict(model2)
Oxboys$resid2 <- with(Oxboys, fitted2 - height)

oplot %+% Oxboys + aes(y = resid2) + geom_smooth(aes(group = 1))

model3 <- update(model, height ~ age + I(age ^ 2) + I(age ^ 3))
Oxboys$fitted3 <- predict(model3)
Oxboys$resid3 <- with(Oxboys, fitted3 - height)

oplot %+% Oxboys + aes(y = resid3) + geom_smooth(aes(group = 1))
