
library(callier)
library(ggplot2)


# Base plot: Smooths of diamond price plotted against carat. Smooths separated
#   by diamond color rating: alphabetic from D (best) to J (worst).
base_plot <- ggplot(data = diamonds, aes(x = carat, y = price, color = color)) +
  theme_bw() +
  coord_cartesian(xlim = c(1, 2.5), ylim = c(2500, 17500)) +
  geom_smooth(se = FALSE, size = 2)


# Sequential Callier color scale, where the lightness of the tints decreases
# (i.e., the visual impact increases).
base_plot +
  scale_color_callier(scheme = 'seq', steps = 7, hue = 'orange', direction = 'increase')
base_plot +
  scale_color_callier(scheme = 'seq', steps = 7, hue = 'blue', direction = 'increase')

# Sequential Callier color scale, where the lightness of the tints increases
# (i.e., the visual impact decreases).
base_plot +
  scale_color_callier(scheme = 'seq', steps = 7, hue = 'orange', direction = 'decrease')
base_plot +
  scale_color_callier(scheme = 'seq', steps = 7, hue = 'blue', direction = 'decrease')





# Diverging Callier color scale, where the criterion point is the midpoint of
# the rating scale (G).
base_plot +
  scale_color_callier(scheme = 'div', steps = 7, lower = 'blue')
base_plot +
  scale_color_callier(scheme = 'div', steps = 7, lower = 'orange')

# Diverging Callier color scale, where the colorless grades (D--F) share the
# same hue and the near colorless grades (G--J) share a different hue.
# That is, the criterion point falls between grades F and G.
base_plot +
  scale_color_manual(
    values = c(CallierSequential(steps = 3, hue = 'orange', direction = 'decrease'),
               CallierSequential(steps = 4, hue = 'blue', direction = 'increase'))
  )
base_plot +
  scale_color_manual(
    values = c(CallierSequential(steps = 3, hue = 'blue', direction = 'decrease'),
               CallierSequential(steps = 4, hue = 'orange', direction = 'increase'))
  )





ColorChart(c(CallierSequential(steps = 3, hue = 'blue', direction = 'decrease'),
             CallierSequential(steps = 4, hue = 'orange', direction = 'increase')))
