
library(callierr)
library(extrafont)
library(fontcm)
library(ggplot2)





base <- ggplot(data = diamonds, aes(x = carat, y = price/1000, color = color)) +
  theme_bw() +
  coord_cartesian(xlim = c(1.0, 2.25), ylim = c(2.5, 17.5)) +
  xlab('Carat weight') +
  ylab('Price (x $1000)') +
  guides(color = guide_legend(title = 'Grade')) +
  theme(text=element_text(family='CM Sans', size = 10),
        legend.text = element_text(size=6),
        legend.key.size = unit(3, 'mm')) +
  geom_smooth(se = FALSE, size = 0.75)
base
ggsave(filename = '/Users/patrick/R/callierr/flash/flash-diamonds-base.pdf', plot = base, width = 11, height = 5, units = 'cm')
embed_fonts(file = '/Users/patrick/R/callierr/flash/flash-diamonds-base.pdf')

qualitative <- base +
  scale_color_callier(scheme = 'qual', steps = 7)
qualitative
ggsave(filename = '/Users/patrick/R/callierr/flash/flash-diamonds-qual.pdf', plot = qualitative, width = 11, height = 5, units = 'cm')
embed_fonts(file = '/Users/patrick/R/callierr/flash/flash-diamonds-qual.pdf')



sequential <- base +
  scale_color_callier(scheme = 'seq', steps = 7, direction = 'decr')
sequential
ggsave(filename = '/Users/patrick/R/callierr/flash/flash-diamonds-seq.pdf', plot = sequential, width = 11, height = 5, units = 'cm')
embed_fonts(file = '/Users/patrick/R/callierr/flash/flash-diamonds-seq.pdf')


div_steps <- list(orange = c('D', 'E', 'F'), blue = c('G', 'H', 'I', 'J'))
diverging <- base +
  scale_color_callier(scheme = 'div', steps = div_steps)
diverging
ggsave(filename = '/Users/patrick/R/callierr/flash/flash-diamonds-div.pdf', plot = diverging, width = 11, height = 5, units = 'cm')
embed_fonts(file = '/Users/patrick/R/callierr/flash/flash-diamonds-div.pdf')



color_chart <- ColorChart(palette = CallierDiverging(steps = div_steps), chipsize = 10)
color_chart <- color_chart +
  theme(text=element_text(family='CM Sans', size = 10),
        plot.margin=unit(c(1,1,1,1), units = 'mm')) +
  ylab('')
ggsave(filename = '/Users/patrick/R/callierr/flash/flash-color-chart.pdf', plot = color_chart, width = 11, height = 5, units = 'cm')
embed_fonts(file = '/Users/patrick/R/callierr/flash/flash-color-chart.pdf')
