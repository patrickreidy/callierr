# callierr

An implementation of the color schemes set out by the Graphic Standards for 
the Callier Center for Communication Disorders.
Provides functions for defining color palettes that follow qualitative,
sequential, and diverging color schemes.
Interfaces with R's base graphics and ggplot2 plotting systems.


## Installation

```r
# First, install `devtools` from CRAN if not already installed.
install.packages("devtools") 
# Install `callierr` from Github.
devtools::install_github("callierr", username = "patrickreidy")
```


## Examples

```r
# A basic plot using ggplot's default qualitative palette.
base_plot <- ggplot(data = diamonds, aes(x = carat, y = price/1000, color = color)) +
  theme_bw() +
  coord_cartesian(xlim = c(1.0, 2.25), ylim = c(2.5, 17.5)) +
  xlab("Carat weight") +
  ylab("Price (x $1000)") +
  guides(color = guide_legend(title = "Grade")) +
  geom_smooth(se = FALSE, size = 0.75)

# Add a qualitative color scheme based on the Callier Center's colors.
base_plot + scale_color_callier(scheme = "qualitative", steps = 7)

# Add a sequential color scheme based on the Callier Center's colors.
base_plot + scale_color_callier(scheme = "sequential", steps = 7, hue = "orange", direction = "decreasing")

# Add a diverging color scheme based on the Callier Center's colors.
base_plot + scale_color_callier(scheme = "diverging", steps = list(orange = c("D", "E", "F"), blue = c("G", "H", "I", "J")))
```

