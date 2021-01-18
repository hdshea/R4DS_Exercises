library(tidyverse)

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy))

nrow(mpg)
ncol(mpg)

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = cyl, y = hwy))

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = class, y = drv))

count(mpg, drv, class)

ggplot(mpg, aes(x = class, y = drv)) +
    geom_count()

mpg %>%
    count(class, drv) %>%
    ggplot(aes(x = class, y = drv)) +
    geom_tile(mapping = aes(fill = n))

mpg %>%
    count(class, drv) %>%
    complete(class, drv, fill = list(n = 0)) %>%
    ggplot(aes(x = class, y = drv)) +
    geom_tile(mapping = aes(fill = n))

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")

ggplot(mtcars, aes(wt, mpg)) +
    geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)

ggplot(mpg, aes(x = displ, y = hwy, colour = displ < 5)) +
    geom_point()

#Facets

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_grid(drv ~ cyl)

ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point() +
    facet_grid(. ~ cty)

ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point() +
    geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_smooth(mapping = aes(group = drv), se = FALSE) +
    geom_point()

## Statistical Transformations

ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut))

count(diamonds, cut)

ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, y = stat(prop)))

ggplot(data = diamonds) + 
    stat_summary(
        mapping = aes(x = cut, y = depth),
        fun.min = min,
        fun.max = max,
        fun = median
    )

ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
    geom_point(position = "jitter")

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
    geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
    geom_boxplot() +
    coord_flip()

nz <- map_data("italy")

ggplot(nz, aes(long, lat, group = group)) +
    geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
    geom_polygon(fill = "white", colour = "black") +
    coord_quickmap()

# Coordinate Systems

ggplot(mpg, aes(x = factor(1), fill = drv)) +
    geom_bar() +
    coord_polar(theta="y") +
    ggtitle("Pie Chart", "A Stacked Bar Chart in Polar Coordinates") +
    ylab("Number of Cars") +
    xlab("Drive Type")

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Highway MPG",
         x = "Class",
         title = "Highway MPG by car class",
         subtitle = "1999-2008",
         caption = "Source: http://fueleconomy.gov")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
    geom_point() + 
    geom_abline() +
    coord_fixed()

