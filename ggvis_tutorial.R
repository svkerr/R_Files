# ggvis tutorial
# ggvis allows you to seamlessly intermingle ggvis and dplyr code!

library(dplyr)
library(ggvis)

str(mtcars)
str(cocaine)

mtcars %>%
  ggvis(x = ~wt, y = ~mpg) %>%
  layer_points()

mtcars %>%
  ggvis(~wt, ~mpg) %>%    # x and y are defaults
  layer_points()

mtcars %>%
  ggvis(~mpg,~disp) %>%
  mutate(disp = disp/61.0237) %>% # Convert engine displacement to litres
  layer_points()

# You can add more variables to the plot by mapping them to other visual properties like fill, stroke, size and shape

mtcars %>%
  ggvis(~mpg, ~disp, fill = ~vs) %>%
  layer_points

mtcars %>%
  ggvis(~mpg, ~disp, stroke = ~vs) %>%
  layer_points

mtcars %>%
  ggvis(~mpg,~disp, shape = ~factor(cyl)) %>%
  layer_points()

# If you want to make the points a fixed colour or size, you need to use := instead of =. The := operator means to use a raw, unscaled value. 
mtcars %>%
  ggvis(~mpg, ~disp, fill:="red") %>% layer_points()

mtcars %>%
  ggvis(~wt, ~mpg, size:=300, opacity:= 0.4) %>% layer_points()

mtcars %>%
  ggvis(~wt, ~mpg, shape:="cross") %>% layer_points()

# As well as mapping visual properties to variables or setting them to specific values, you can also conenct them to interactive controls. Here we control size and opacity of points with two sliders:
mtcars %>%
  ggvis(~wt, ~mpg, size:= input_slider(10,100),
        opacity:= input_slider(0,1)) %>%
  layer_points()

# You can also connect interactive components to other plot parameters like the width and centers of histogram bins:
mtcars %>%
  ggvis(~wt) %>%
  layer_histograms(width = input_slider(0,2, step=0.10, label = "width"),
                   center = input_slider(0,2, step = 0.5, label = "center"))

## ggvis Cookbook stuff
mtcars %>%
  ggvis(~wt, ~mpg) %>%
  layer_points(size:=25, shape:="diamond", stroke:="red", fill:=NA)
## regression lines

mtcars %>%
  ggvis(~wt, ~mpg, fill:="red") %>%
  layer_points() %>%
  layer_smooths()

# with a linear model and a 95% confint
mtcars %>%
  ggvis(~wt,~mpg, fill:="red") %>%
  layer_points() %>%
  layer_model_predictions(model="lm", se=TRUE)

# Scatter plots with grouping
# Coloring points by a variable:
mtcars %>%
  ggvis(~wt,~mpg) %>%
  layer_points(fill = ~factor(cyl))

# Coloring points, and adding a smoother for each group. The grouping must be applied before the smoothing transform
mtcars %>%
  ggvis(~wt, ~mpg, fill=~factor(cyl)) %>%
  layer_points() %>%
  group_by(cyl) %>%    # note bringing in dplyr here and don't need ~ before variable
  layer_model_predictions(model = "lm")

# Bar graphs
head(pressure)
# When the variable on the x axis is continuous (e.g., numeric or date-time):
pressure %>%
  ggvis(~temperature,~pressure) %>%
  layer_bars()

# When the variable on the x-axis is categorial(e.g., factor or character)
# first modify data set so that temp (x variable) is a factor
pressure2 = pressure %>% mutate(temperature = factor(temperature))
pressure2 %>%
  ggvis(~temperature, ~pressure) %>%
  layer_bars()

# Histograms
head(faithful)

# Add a layer of smoothing lines
mtcars %>%
  ggvis(x = ~wt, y = ~mpg) %>%
  layer_points() %>%
  layer_smooths()

# Another property mapping: fill color
mtcars %>%
  ggvis(x = ~wt, y = ~mpg, fill = ~cyl) %>%
  layer_points()

# Use factor() to treat cyl as a category
# Note: factor() is a nice way to spot clusters graphically
mtcars %>%
  ggvis(x = ~wt, y = ~mpg, fill = ~factor(cyl)) %>%
  layer_points()

# Two layers
pressure %>%
  ggvis(~temperature, ~pressure) %>%
  layer_lines() %>%
  layer_points()

# When no layer is specified, ggvis will guess
pressure %>%
  ggvis(~temperature, ~pressure) 

# Histograms
cocaine %>%
  ggvis(~potency) %>%
  layer_histograms()

# Can also change width of bins
cocaine %>%
  ggvis(~potency) %>%
  layer_histograms(width = 2)



  



