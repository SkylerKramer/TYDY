# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## THIS FUNCTION PLOTS SIZE AND COLOR VS THEIR RESPECTIVE RANKS (TWO PLOTS)
singleScreen_rankPlot <- function(ssDF = NULL){
  # create plot of sorted size and make interactive
  fig1 <- boldPlots(
    ggplot2::ggplot(ssDF, ggplot2::aes(Size_Rank, Size, colour = Color)) +
      ggplot2::geom_point() +
      ggplot2::scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(n = 9, name = "Reds"))) +
      ggplot2::labs(x = "Size Rank", y = "Size") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  ) %>% plotly::ggplotly()
  
  # create plot of sorted color and make interactive
  fig2 <- boldPlots(
    ggplot2::ggplot(ssDF, ggplot2::aes(Color_Rank, Color, colour = Color)) +
      ggplot2::geom_point() +
      ggplot2::scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(n = 9, name = "Reds"))) +
      ggplot2::labs(x = "Color Rank", y = "Color") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  ) %>% plotly::ggplotly()
  
  # merge subfigures
  fig <- plotly::subplot(fig1, fig2, titleY = TRUE, titleX = TRUE, margin = 0.1) %>%
    plotly::layout(title = "<b>Ranked Plots</b>", margin = list(t = 50))
  
  # return interactive figure
  return(fig)
}

## THIS FUNCTION PLOTS SIZE VS COLOR
singleScreen_relationshipPlot <- function(ssDF = NULL){
  # create plot of size vs color and make interactive
  fig <- boldPlots(
    ggplot2::ggplot(ssDF, ggplot2::aes(Size, Color, colour = Color, size = Size)) +
      ggplot2::geom_point() +
      ggplot2::scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(n = 9, name = "Reds"))) +
      ggplot2::labs(x = "Size", y = "Color") +
      ggplot2::theme_bw()
  ) %>% plotly::ggplotly()
  
  # return interactive figure
  return(fig)
}

## This function creates the lollipop plot for family enrichment
singleScreen_familyEnrichmentPlot <- function(familyData = NULL){
  # specify break values for y-axis of plot
  breakVals <- pretty(familyData %>% dplyr::pull(`Sig.`))
  
  # initialize plot with segments
  p <- ggplot2::ggplot(familyData) +
    ggplot2::geom_segment(ggplot2::aes(
      x = Family, xend = Family,
      y = `Sig.`, yend = 0,
      colour = Sig
    ), size = 1)
  
  # add points
  p <- p +
    ggplot2::geom_point(ggplot2::aes(
      x = Family, y = `Sig.`,
      text = glue::glue(
        "
      Type: Sig. Up
      Family: {Family}
      Family Size: {n}
      Obs. Up: {`Sig.`}
      Exp. Up: {Expected_Sig}
      FET p-value: {Sig_pvalue}
      "
      )
    ), size = 3, shape = 21, fill = "gold")
  
  # add expected lines and horizontal zero line
  p <- p +
    ggplot2::geom_path(ggplot2::aes(x = Family, y = Expected_Sig), group = 1, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, size = 1)
  
  # reformat plot, add labels, and adjust theme
  p <- p +
    ggplot2::scale_x_discrete(limits = rev) +
    ggplot2::scale_y_continuous(breaks = breakVals, labels = breakVals) +
    ggplot2::scale_colour_manual(values = c("black", "red")) +
    ggplot2::labs(
      x = "TF Family", y = "No. TFs",
      title = "Family Distribution of Sig. TFs",
      colour = "Sig."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  
  # make bold
  p <- boldPlots(p)
  
  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      yaxis = list(title = list(standoff = 20L)),
      xaxis = list(title = list(standoff = 20L))
    )
  
  # return interactive figure
  return(fig)
}
