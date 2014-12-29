## ggplot tutorial
library(ggplot2)
library(dplyr)
library(nlme)

p = ggplot(diamonds,aes(carat,price,colour=cut))
p = p + layer(geom="point")
p

p = ggplot(msleep,aes(sleep_rem/sleep_total,awake)) + geom_point() + geom_smooth()
# or
p = ggplot(msleep,aes(sleep_rem/sleep_total,awake))
p + layer(geom="point")

# again, using geom_point shortcut
p = ggplot(diamonds,aes(x=carat)) + geom_histogram(bindwidth=2,fill="steelblue")
p

# Layers are regular R-objects and so can be stored as variables, making it easy to write clean code reducing duplication. Following example we create a layer that displays a translucent thick blue line of best fit:
bestfit = geom_smooth(method = "lm", se = F, colour = "steelblue", alpha=0.5, size=1)
# now use the above bestfit layer in a ggplot():
ggplot(msleep,aes(sleep_rem,sleep_total)) + geom_point() + bestfit
# as opposed to this smoother:
ggplot(msleep,aes(sleep_rem,sleep_total)) + geom_point() + geom_smooth()

# You can replace a dataset with %+% as shown in the following example:
p = ggplot(mtcars,aes(mpg,wt,colour=cyl)) + geom_point()
p
mtcars = transform(mtcars,mpg=mpg^2)
p %+% mtcars
p
# Default mappings in the plot p can be extended or overridden in thelayers:
p = ggplot(mtcars,aes(x=mpg, y=wt))
p + geom_point()
p + geom_point(aes(colour=factor(cyl)))
p + geom_point(aes(y=disp))
# Setting is different than mapping
p + geom_point(colour="red")

# Multiple groups
p = ggplot(Oxboys,aes(age,height,group=Subject)) + geom_line()
p
