
library(callier)
library(ggplot2)





base <- ggplot(data = diamonds, aes(x = carat, y = price/1000, color = color)) +
  theme_bw() +
  coord_cartesian(xlim = c(1.0, 2.25), ylim = c(2.5, 17.5)) +
  xlab('Carat weight') +
  ylab('Price (in thousands of dollars)') +
  guides(color = guide_legend(title = 'Color grade')) +
  geom_smooth(se = FALSE, size = 2)
base



qualitative <- base +
  scale_color_callier(scheme = 'qual', steps = 7)
qualitative



sequential <- base +
  scale_color_callier(scheme = 'seq', steps = 7, direction = 'decr')
sequential



div_steps <- list(orange = c('D', 'E', 'F'), blue = c('G', 'H', 'I', 'J'))
diverging <- base +
  scale_color_callier(scheme = 'div', steps = div_steps)
diverging
