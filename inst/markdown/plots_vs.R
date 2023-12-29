


validationHeatmap <- function(d, var, cat, interactive = T) {

  # possible extension: when giving multiple vars, plot as facets in same row

  d <- filter(d, variable == var)

  d$period <- as.character(d$period)

  # text for tooltip, category 1
  if (cat == 1) {
    d <- d %>%
      mutate(text = paste0(region, "\n",
                           period, "\n",
                           "Value: ", round(value,2), "\n",
                           "Ref_Value: ", round(ref_value,2), "\n",
                           "Ref_Source: ", ref_model, "\n",
                           paste0("Max: ", d$max_yel*100, "% / ", d$max_red*100, "%")
                          )
            )
  }

  # text for tooltip, category 3
  if (cat == 3) {
    d <- d %>%
      mutate(text = paste0(region, "\n",
                           period, "\n",
                           "Value: ", round(value,2), "\n",
                           paste0("Min: ", d$min_yel, " / ", d$min_red,"\n",
                                  "Max: ", d$max_yel, " / ", d$max_red)
                          )
      )
    }

  # classic ggplot, with text in aes
  p <- ggplot(d, aes(x = region, y = period, fill=check, text=text)) +
    geom_tile(color="white", linewidth=0.2) +
    scale_fill_manual(values = colors, breaks = colors) +
    facet_grid(scenario~.)
  # make it beautiful
  # from https://www.r-bloggers.com/2016/02/making-faceted-heatmaps-with-ggplot2/
  p <- p + labs(x=NULL, y=NULL, title=var)
  p <- p + theme_tufte(base_family="Helvetica")
  p <- p + theme(axis.ticks=element_blank())
  p <- p + theme(axis.text=element_text(size=7))
  p <- p + coord_equal()
  p <- p + theme(legend.position = "none")

  # p + theme(panel.spacing = unit(2, "lines"))
  p
  fig <- ggplotly(p, tooltip="text")

  # improve plotly layout, works but very manual
  #fig <- fig %>% subplot(heights = 0.3) %>%
  #  layout(title = list(y=0.64))
  if (interactive) {
    return(fig)
  } else {
    return(p)
  }

}
