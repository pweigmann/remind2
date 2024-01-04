

validationHeatmap <- function(d, var, cat, metric, interactive = T) {

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
                           "Thresholds: \n",
                           ifelse(metric == "relative",
                             paste0("Max: ", max_yel*100, "% / ", max_red*100, "%"),
                             paste0("Max: ", max_yel, " / ", max_red))
                          )
            )
  }

  # text for tooltip, category 2
  if (cat == 2) {
    d <- d %>%
      mutate(text = paste0(region, "\n",
                           period, "\n",
                           "Value: ", round(value,2), "\n",
                           ifelse(metric == "relative",
                                  paste0("Ref Value: ", round(ref_value,2), "\n",
                                         "Rel Deviation: ", round(check_value,2)*100, "% \n",
                                         "Thresholds: \n",
                                         "Min: ", min_yel*100, "% / ", min_red*100,"% \n",
                                         "Max: ", max_yel*100, "% / ", max_red*100, "%"),
                                  # for metric == "absolute"
                                  paste0("Thresholds: \n",
                                         "Min: ", min_yel, " / ", min_red,"\n",
                                         "Max: ", max_yel, " / ", max_red)
                                )
                           )
            )
  }

  if (cat == 3) {
    d <- d %>%
      mutate(text = paste0(region, "\n",
                           period, "\n",
                           "Avg. growth/yr: ", round(check_value,2)*100, "% \n",
                           "Absolute value: ", round(value,2), " \n",
                           "Thresholds: \n",
                           paste0("Min: ", min_yel*100, "% / ", min_red*100, "% \n",
                                  "Max: ", max_yel*100, "% / ", max_red*100, "%")
      )
      )
  }

  # classic ggplot, with text in aes
  p <- ggplot(d, aes(x = region, y = period, fill=check, text=text)) +
    geom_tile(color="white", linewidth=0.0) +
    scale_fill_manual(values = colors, breaks = colors) +
    facet_grid(scenario~.)
  # make it beautiful
  # from https://www.r-bloggers.com/2016/02/making-faceted-heatmaps-with-ggplot2/
  p <- p + labs(x=NULL, y=NULL, title=paste0(var, " [", d$unit[1], "]"))
  p <- p + theme_tufte(base_family="Helvetica")
  p <- p + theme(axis.ticks=element_blank())
  p <- p + theme(axis.text=element_text(size=7))
  p <- p + coord_equal()
  p <- p + theme(legend.position = "none")


  # p + theme(panel.spacing = unit(2, "lines"))

  # not great, only works with "World" being the first region
  if("World" %in% d$region) {
    p <- p + geom_vline(xintercept = 1.5, linewidth = 0.8, color = "white")
  }
  fig <- ggplotly(p, tooltip="text")

  # improve plotly layout, works but very manual
  #fig <- fig %>% subplot(heights = 0.3) %>%
  #   layout(title = list(y=0.64))
  if (interactive) {
    return(fig)
  } else {
    return(p)
  }

}
