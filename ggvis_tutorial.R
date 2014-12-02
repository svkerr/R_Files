# ggvis tutorial

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



  



