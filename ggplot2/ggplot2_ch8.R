hgram <- qplot(rating, data = movies, binwidth = 1)
hgram
previous_theme <- theme_set(theme_bw())
hgram
hgram + previous_theme
theme_set(previous_theme)

hgramt <- hgram + labs(title = "This is a histogram")
hgramt
hgramt + theme(plot.title = element_text(size = 20))
hgramt + theme(plot.title = element_text(size = 20, colour = "red"))
hgramt + theme(plot.title = element_text(size = 20, hjust = 0))
hgramt + theme(plot.title = element_text(size = 20, face = "bold"))
hgramt + theme(plot.title = element_text(size = 20, angle = 180))

hgram + theme(panel.grid.major = element_line(colour = "red"))
hgram + theme(panel.grid.major = element_line(size = 2))
hgram + theme(panel.grid.major = element_line(linetype = "dotted"))
hgram + theme(axis.line = element_line())
hgram + theme(axis.line = element_line(colour = "red"))
hgram + theme(axis.line = element_line(size = 0.5, linetype = "dashed"))

hgram + theme(plot.background = element_rect(fill = "grey80", colour = NA))
hgram + theme(plot.background = element_rect(size = 2))
hgram + theme(plot.background = element_rect(colour = "red"))
hgram + theme(plot.background = element_rect())
hgram + theme(plot.background = element_rect(colour = NA))
hgram + theme(plot.background = element_rect(linetype = "dotted"))

hgramt
last_plot() + theme(panel.grid.minor = element_blank())
last_plot() + theme(panel.grid.major = element_blank())
last_plot() + theme(panel.background = element_blank())
last_plot() + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
last_plot() + theme(axis.line = element_line())


old_theme <- theme_update(
  plot.background = element_rect(fill = "#3366FF"),
  panel.background = element_rect(fill = "#003DF5"),
  axis.text.x = element_text(colour = "#CCFF33"),
  axis.text.y = element_text(colour = "#CCFF33", hjust = 1),
  axis.title.x = element_text(colour = "#CCFF33", face = "bold"),
  axis.title.y = element_text(colour = "#CCFF33", face = "bold", angle = 90)
)
qplot(cut, data = diamonds, geom = "bar")
qplot(cty, hwy, data = mpg)
theme_set(old_theme)

p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))
p
scale_colour_discrete <- scale_colour_brewer
p

update_geom_defaults("point", aes(colour = "darkblue"))
qplot(mpg, wt, data = mtcars)
update_stat_defaults("bin", aes(y = ..density..))
qplot(rating, data = movies, geom = "histogram", binwidth = 1)


qplot(mpg, wt, data = mtcars)
ggsave(file = "output.pdf")
pdf(file = "output.pdf", width = 6, height = 6)
qplot(mpg, wt, data = mtcars)
qplot(wt, mpg, data = mtcars)
dev.off()


(a <- qplot(date, unemploy, data = economics, geom = "line"))
(b <- qplot(uempmed, unemploy, data = economics) + geom_smooth(se = F))
(c <- qplot(uempmed, unemploy, data = economics, geom = "path"))

library(grid)
vp1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
vp1 <- viewport()
vp2 <- viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.5)
vp2 <- viewport(width = 0.5, height = 0.5)
vp3 <- viewport(width = unit(2, "cm"), height = unit(3, "cm"))
vp4 <- viewport(x = 1, y = 1, just = c("top", "right"))
vp5 <- viewport(x = 0, y = 0, just = c("bottom", "right"))

subvp <- viewport(width = 0.4, height = 0.4, x = 0.75, y = 0.35)
b
print(c, vp = subvp)
csmall <- c + theme_gray(9) + labs(x = NULL, y = NULL) + theme(plot.margin = unit(rep(0, 4), "lines"))
b
print(csmall, vp = subvp)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1:2))
print(b, vp = vplayout(2, 1))
print(c, vp = vplayout(2, 2))
