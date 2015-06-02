library(ggplot2)

head(mpg)

plot <- qplot(cty, hwy, data = mpg)
plot

plot + aes(x = drv)
plot + aes(x = drv) + scale_x_discrete()

head(msleep)

p <- qplot(sleep_total, sleep_cycle, data = msleep, colour = vore)
p

p + scale_colour_hue()

p + scale_colour_hue("What does\nit eat?",
                     breaks = c("herbi", "carni", "omni", NA),
                     labels = c("plants", "meat", "both", "don't know"))

p + scale_colour_brewer(palette = "Set1")


p <- qplot(cty, hwy, data = mpg, colour = displ)
p
p + scale_x_continuous("City mpg")
p + xlab("City mpg")
p + ylab("Highway mpg")
p + labs(x = "City mpg", y = "Highway", colour = "Displacement")
p + xlab(expression(frac(miles, gallon)))


p <- qplot(cyl, wt, data = mtcars)
p
p + scale_x_continuous(breaks = c(5.5, 6.5))
p + scale_x_continuous(limits = c(5.5, 6.5))

p <- qplot(wt, cyl, data = mtcars, colour = cyl)
p
p + scale_colour_gradient(breaks = c(5.5, 6.5))
p + scale_colour_gradient(limits = c(5.5, 6.5))


# Data changes, scale doesn't change
qplot(log10(carat), log10(price), data = diamonds)

# Scale changes, data doesn't change
qplot(carat, price, data = diamonds) + scale_x_log10() + scale_y_log10()

library(scales)
plot <- qplot(date, psavert, data = economics, geom = "line") + ylab("Personal savings rate") + geom_hline(xintercept = 0, colour = "grey50")
plot
plot + scale_x_date(breaks = date_breaks("10 years"))
plot + scale_x_date(limits = as.Date(c("2004-01-01", "2005-01-01")), labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 month"))


install.packages('MASS')
f2d <- with(faithful, MASS::kde2d(eruptions, waiting, h = c(1, 10), n = 50))
df <- with(f2d, cbind(expand.grid(x, y), as.vector(z)))
names(df) <- c("eruptions", "waiting", "density")
head(df)

library(ggplot2)
erupt <- ggplot(df, aes(waiting, eruptions, fill = density)) + geom_tile() + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
erupt + scale_fill_gradient(limits = c(0, 0.04))
erupt + scale_fill_gradient(limits = c(0, 0.04), low = "white", high = "black")


install.packages('vcd')
library(vcd)
fill_gradn <- function(pal) {
  scale_fill_gradientn(colours = pal(7), limits = c(0, 0.04))
}
rainbow_hcl

# To be continued...