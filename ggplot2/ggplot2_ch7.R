mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4", "f"))

qplot(cty, hwy, data = mpg2) + facet_null()

qplot(cty, hwy, data = mpg2) + facet_grid(. ~ cyl)
qplot(cty, data = mpg2, geom = "histogram", binwidth = 2) + facet_grid(cyl ~ .)
qplot(cty, hwy, data = mpg2) + facet_grid(drv ~ cyl)


p <- qplot(displ, hwy, data = mpg2) + geom_smooth(method = "lm", se = F)
p + facet_grid(cyl ~ drv)
p + facet_grid(cyl ~ drv, margins = T)

qplot(displ, hwy, data = mpg2) + geom_smooth(aes(colour = drv), method = "lm", se = F) + facet_grid(cyl ~ drv, margins = T)

library(plyr)
movies$decade <- round_any(movies$year, 10, floor)
qplot(rating, ..density.., data = subset(movies, decade > 1890), geom = "histogram", binwidth = 0.5) + facet_wrap(~ decade, ncol = 6)

p <- qplot(cty, hwy, data = mpg)
p + facet_wrap(~ cyl)
p + facet_wrap(~ cyl, scales = "free")


library(reshape2)
head(economics)
em <- melt(economics, id = "date")
head(em)
qplot(date, value, data = em, geom = "line", group = variable) + facet_grid(variable ~ ., scale = "free_y")

mpg3 <- within(mpg2, {
  model <- reorder(model, cty)
  manufacturer <- reorder(manufacturer, -cty)
})
models <- qplot(cty, model, data = mpg3)
models
models + facet_grid(manufacturer ~ ., scales = "free", space = "free") + theme(strip.text.y = element_text())
models + facet_grid(. ~ manufacturer, scales = "free", space = "free") + theme(strip.text.y = element_text())


xmaj <- c(0.3, 0.5, 1, 3, 5)
xmin <- as.vector(outer(1:10, 10^c(-1, 0)))

ymaj <- c(500, 1000, 5000, 10000)
ymin <- as.vector(outer(1:10, 10^c(2, 3, 4)))
dplot <- ggplot(subset(diamonds, color %in% c("D", "E", "G", "J")),
                aes(carat, price, colour = color)) +
         scale_x_log10(breaks = xmaj, labels = xmaj, minor = xmin) +
         scale_y_log10(breaks = ymaj, labels = ymaj, minor = ymin) +
         scale_colour_hue(limits = levels(diamonds$color)) +
         theme(legend.position = "none")

dplot + geom_point()
dplot + geom_point() + facet_grid(. ~ color)
dplot + geom_smooth(method = lm, se = F, fullrange = T)
dplot + geom_smooth(method = lm, se = F, fullrange = T) + facet_grid(. ~ color)
dplot + geom_smooth(method = lm, fullrange = T) + facet_grid(. ~ color)

qplot(color, data = diamonds, geom = "bar", fill = cut, position = "dodge")
qplot(cut, data = diamonds, geom = "bar", fill = cut) + facet_grid(. ~ color) + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8, colour = "grey50"))


mpg4 <- subset(mpg, manufacturer %in% c("audi", "volkswagen", "jeep"))
mpg4$manufacturer <- as.character(mpg4$manufacturer)
mpg4$model <- as.character(mpg4$model)

base <- ggplot(mpg4, aes(fill = model)) + geom_bar(position = "dodge") + theme(legend.position = "none")
base + aes(x = model) + facet_grid(. ~ manufacturer)
last_plot() + facet_grid(. ~ manufacturer, scales = "free_x", space = "free")
base + aes(x = manufacturer)

mpg2$disp_ww <- cut_interval(mpg2$displ, length = 1)
mpg2$disp_wn <- cut_interval(mpg2$displ, n = 6)
mpg2$disp_nn <- cut_number(mpg2$displ, n = 3)

plot <- qplot(cty, hwy, data = mpg2) + labs(x = NULL, y = NULL)
plot + facet_wrap(~ disp_ww, nrow = 1)
plot + facet_wrap(~ disp_wn, nrow = 1)
plot + facet_wrap(~ disp_nn, nrow = 1)


(p <- qplot(disp, wt, data = mtcars) + geom_smooth())
p + scale_x_continuous(limits = c(325, 500))
p + coord_cartesian(xlim = c(325, 500))
p + scale_x_continuous(limits = c(325, 400)) + coord_cartesian(xlim = c(325, 500))

(d <- ggplot(diamonds, aes(carat, price)) +
   stat_bin2d(bins = 25, colour = "grey70") +
   theme(legend.position = "none"))
d + scale_x_continuous(limits = c(0, 2))
d + coord_cartesian(xlim = c(0, 2))
d + scale_x_continuous(limits = c(0, 1)) + coord_cartesian(xlim = c(0, 2))

qplot(displ, cty, data = mpg) + geom_smooth()
qplot(cty, displ, data = mpg) + geom_smooth()
qplot(cty, displ, data = mpg) + geom_smooth() + coord_flip()


qplot(carat, price, data = diamonds, log = "xy") + geom_smooth(method = "lm")
library(scales)
last_plot() + coord_trans(x = exp_trans(10), y = exp_trans(10))

(pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) + geom_bar(width = 1))
pie + coord_polar(theta = "y")
pie + coord_polar() #theta = "x"
