# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

## This function creates the sorted feature plot for data without replicates and colors by absolute difference
dualScreenSing_sortedFeaturePlot_diff <- function(processDF = NULL){
  
  # format data
  formatDF <- processDF %>%
    dplyr::arrange(dplyr::desc(NormFeature.x)) %>%
    tibble::rowid_to_column("Rank")
  
  # make plot
  sfp <- boldPlots(
    ggplot2::ggplot(formatDF) +
      ggplot2::geom_line(ggplot2::aes(Rank, NormFeature.x), colour = "black", size = 1) +
      ggplot2::geom_point(
        ggplot2::aes(
          Rank, NormFeature.y, 
          colour = abs(NormFeature.Diff),
          text = glue::glue(
            "
            Identifier: {Identifier}
            Family: {Family}
            NormFeature.x: {NormFeature.x}
            NormFeature.y: {NormFeature.y}
            NormFeature.Diff: {NormFeature.Diff}
            "
          ),
          shape = TF.type
        ), alpha = 0.75
      ) +
      ggplot2::scale_colour_viridis_c() +
      ggplot2::scale_shape_manual(values = c(4,20,18)) +
      ggplot2::labs(
        x = "Feature Rank in Control Data", y = "Normalized Values", title = "Sorted Feature Plot",
        colour = "Feature Difference", shape = "Significance"
      ) +
      ggplot2::theme_bw()
  )
  
  # make interactive figure
  fig <- plotly::ggplotly(sfp, tooltip = "text")
  
  # return interactive figure
  return(fig)
}

## This function creates the sorted feature plot for data without replicates and colors by family
dualScreenSing_sortedFeaturePlot_family <- function(processDF = NULL){
  
  # format data
  formatDF <- processDF %>%
    dplyr::filter(TF.type != "Not Sig.") %>%
    dplyr::arrange(dplyr::desc(NormFeature.x)) %>%
    tibble::rowid_to_column("Rank")
  
  # make plot
  sfp <- boldPlots(
    ggplot2::ggplot(formatDF) +
      ggplot2::geom_line(ggplot2::aes(Rank, NormFeature.x), colour = "black", size = 1) +
      ggplot2::geom_point(
        ggplot2::aes(
          Rank, NormFeature.y, 
          colour = Family,
          text = glue::glue(
            "
            Identifier: {Identifier}
            Family: {Family}
            NormFeature.x: {NormFeature.x}
            NormFeature.y: {NormFeature.y}
            NormFeature.Diff: {NormFeature.Diff}
            "
          )
        )
      ) +
      ggplot2::labs(x = "Feature Rank in Filtered Control Data", y = "Normalized Values", title = "Sorted Feature Plot",colour = "Family") +
      ggplot2::theme_bw()
  )
  
  # make interactive figure
  fig <- plotly::ggplotly(sfp, tooltip = "text")
  
  # return interactive figure
  return(fig)
}

## This function creates the distribution of the feature and CV before and after applying the CV filter
dualScreen_cvPlots <- function(ctrlDF = NULL, testDF = NULL, voi = NULL, ctrlFilter = NULL, testFilter = NULL){
  
  # bind the two data sets
  stackDF <- rbind(
    ctrlDF %>% dplyr::select(Identifier, Avg, CV) %>% dplyr::mutate(Experiment = "Control"),
    testDF %>% dplyr::select(Identifier, Avg, CV) %>% dplyr::mutate(Experiment = "Test")
  )
  
  # get filter message
  # filter()
  
  # pre-filter histogram of average feature
  p_preFeature <- boldPlots(
    ggplot2::ggplot(stackDF, ggplot2::aes(Avg, fill = Experiment)) +
      ggplot2::geom_histogram(color = "black", bins = 30, alpha = 0.4, position = "identity") +
      ggplot2::scale_fill_manual(values = c("magenta", "cyan")) +
      ggplot2::labs(x = paste0("Average ", voi), y = "Frequency", title = paste0("Average-", voi, " Histogram")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  )
  
  # pre-filter CV density plot
  p_preCV <- boldPlots(
    ggplot2::ggplot(stackDF, ggplot2::aes(CV, fill = Experiment)) +
      ggplot2::geom_density(color = "black", alpha = 0.4) +
      ggplot2::scale_fill_manual(values = c("magenta", "cyan")) +
      ggplot2::labs(x = "Coefficient of Variation", y = "Density", title = "CV Density Plot") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  )
  
  # post-filter histogram of average feature
  p_postFeature <- boldPlots(
    stackDF %>% dplyr::filter((Experiment == "Control" & CV < ctrlFilter) | (Experiment == "Test" & CV < testFilter)) %>%
      ggplot2::ggplot(ggplot2::aes(Avg, fill = Experiment)) +
      ggplot2::geom_histogram(color = "black", bins = 30, alpha = 0.4, position = "identity") +
      ggplot2::scale_fill_manual(values = c("magenta", "cyan")) +
      ggplot2::labs(x = paste0("Average ", voi), y = "Frequency", title = paste0("Average-", voi, " Histogram")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  )
  
  # post-filter CV density plot
  p_postCV <- boldPlots(
    stackDF %>% dplyr::filter((Experiment == "Control" & CV < ctrlFilter) | (Experiment == "Test" & CV < testFilter)) %>%
      ggplot2::ggplot(ggplot2::aes(CV, fill = Experiment)) +
      ggplot2::geom_density(color = "black", alpha = 0.4) +
      ggplot2::scale_fill_manual(values = c("magenta", "cyan")) +
      ggplot2::labs(x = "Coefficient of Variation", y = "Density", title = "CV Density Plot") +
      ggplot2::theme_bw()
  )
  
  # convert all plots to interactive figures and add sub-titles
  fig_preFeature <- plotly::ggplotly(p_preFeature) %>%
    plotly::add_annotations(
      text = paste0("Pre-Filtered Average-", voi, " Histogram"),
      x = 0, y = 1,
      xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "top",
      yshift = 20, showarrow = FALSE
    )
  
  fig_preCV <- plotly::ggplotly(p_preCV) %>%
    plotly::add_annotations(
      text = "Pre-Filtered CV Density Plot",
      x = 0, y = 1,
      xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "top",
      yshift = 20, showarrow = FALSE
    )
  
  fig_postFeature <- plotly::ggplotly(p_postFeature) %>%
    plotly::add_annotations(
      text = paste0("Post-Filtered Average-", voi, " Histogram"),
      x = 0, y = 1,
      xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "top",
      yshift = 20, showarrow = FALSE
    )
  
  fig_postCV <-plotly::ggplotly(p_postCV) %>%
    plotly::add_annotations(
      text = "Post-Filtered CV Density Plot",
      x = 0, y = 1,
      xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "top",
      yshift = 20, showarrow = FALSE
    )
  
  # make final interactive figure
  fig <- plotly::subplot(
    plotly::style(fig_preFeature, showlegend = FALSE),
    plotly::style(fig_preCV, showlegend = FALSE),
    plotly::style(fig_postFeature, showlegend = FALSE),
    fig_postCV,
    nrows = 2, margin = 0.1
  ) %>%
    plotly::layout(
      title = paste0("<b>Average-", voi, " Histogram and CV Density Plot</b>"),
      legend = list(
        title = list(text = "<b> Experiment </b>", side = "left"),
        orientation = "h",
        x = 0, y = -0.1
      )
    )
  
  # return interactive figure
  return(fig)
}

## This function creates a volcano plot after statistically comparing the data
dualScreen_volcanoPlot <- function(pvalData = NULL){
  
  # compute log2(fold change) of feature and -log2(p-value) for volcano plot
  volcanoDF <- pvalData %>%
    dplyr::mutate(log2FC = log2(Avg.y / Avg.x), log2PV = -log2(ttest_pv)) %>%
    dplyr::select(c(Common_Name, Family, TF.type, log2FC, log2PV))
  
  # make plot
  p <- boldPlots(
    ggplot2::ggplot(
      volcanoDF, ggplot2::aes(
        log2FC, log2PV, size = abs(log2FC), colour = log2PV, text = glue::glue(
          "
      Common_Name: {Common_Name}
      Family: {Family}
      TF.type: {TF.type}
      "
        )
      )
    ) +
      ggplot2::geom_point() +
      ggplot2::scale_color_viridis_c() +
      ggplot2::labs(x = "log2(FC)", y = "-log2(pv)", title = "Volcano Plot", colour = "-log2(pv)") +
      ggplot2::theme_bw()
  )
  
  # make interactive figure
  fig <- plotly::ggplotly(p, tooltip = "text")
  
  # return interactive figure
  return(fig)
}

## This function creates the sorted feature plot, colored by p-value
dualScreen_sortedFeaturePlot_pval <- function(pvalData = NULL){
  # rank by averge feature in control set
  pvalData <- pvalData %>%
    dplyr::arrange(dplyr::desc(Avg.x)) %>%
    tibble::rowid_to_column("Rank") %>%
    dplyr::mutate(significant = ifelse(ttest_pv > 0.05, "Not Sig.", "Sig."))
  
  # specify range for shading
  mods <- pvalData %>%
    dplyr::select(Rank, Avg.x) %>%
    dplyr::mutate(
      up10 = Avg.x * 1.1, down10 = Avg.x * 0.9,
      up20 = Avg.x * 1.2, down20 = Avg.x * 0.8
    )
  
  # if(nothing is significant){...}
  
  # create sorted feature plot (sfp)
  sfp <- boldPlots(
    ggplot2::ggplot(pvalData) +
    ggplot2::geom_line(ggplot2::aes(Rank, Avg.x), colour = "black", size = 1) +
    ggplot2::geom_point(
      ggplot2::aes(
        Rank, Avg.y, 
        colour = Abs_Diff,
        text = glue::glue(
          "
            Identifier: {Identifier}
            Family: {Family}
            Avg.x: {Avg.x}
            Avg.y: {Avg.y}
            p-value: {ttest_pv}
            "
        ),
        shape = TF.type
      )
    ) +
    ggplot2::scale_shape_manual(values = c(4, 20, 18)) +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::labs(
      x = "Average Feature Rank in Control Data", y = "Average Value", title = "Sorted Feature Plot",
      colour = "Absolute Difference", shape = "Significance"
    ) +
    ggplot2::theme_bw()
  )
  
  # make interactive figure and add shading at +/- 10 and 20% around the reference line
  fig <- plotly::ggplotly(sfp, tooltip = "text") %>%
    
    # shading at +/- 10%
    plotly::add_trace(
      data = mods, x = ~Rank, y = ~up10, type = "scatter", mode = "lines",
      line = list(color = "transparent"),
      showlegend = FALSE, name = "10% above control"
    ) %>%
    plotly::add_trace(
      x = ~Rank, y = ~down10, type = "scatter", mode = "lines",
      fill = "tonexty", fillcolor = "rgba(0,100,80,0.2)", line = list(color = "transparent"),
      showlegend = F, name = "10% below control"
    ) %>%
    
    # shading at +/- 20%
    plotly::add_trace(
      x = ~Rank, y = ~up20, type = "scatter", mode = "lines",
      line = list(color = "transparent"),
      showlegend = FALSE, name = "20% below control"
    ) %>%
    plotly::add_trace(
      x = ~Rank, y = ~down20, type = "scatter", mode = "lines",
      fill = "tonexty", fillcolor = "rgba(180,157,225,0.2)", line = list(color = "transparent"),
      showlegend = FALSE, name = "20% below control"
    )
  
  # return interactive figure
  return(fig)
}

## This function creates the sorted feature plot, colored by family
dualScreen_sortedFeaturePlot_family <- function(pvalData = NULL){
  
  # rank by averge feature in control set
  pvalData <- pvalData %>%
    dplyr::filter(ttest_pv <= 0.05) %>%
    dplyr::arrange(dplyr::desc(Avg.x)) %>%
    tibble::rowid_to_column("Rank")
  
  # specify range for shading
  mods <- pvalData %>%
    dplyr::select(Rank, Avg.x) %>%
    dplyr::mutate(
      up10 = Avg.x * 1.1, down10 = Avg.x * 0.9,
      up20 = Avg.x * 1.2, down20 = Avg.x * 0.8
    )
  
  # if(nothing is significant){...}
  
  # create sorted feature plot (sfp)
  sfp <- boldPlots(
    ggplot2::ggplot(pvalData) +
      ggplot2::geom_line(ggplot2::aes(Rank, Avg.x), colour = "black", size = 1) +
      ggplot2::geom_point(
        ggplot2::aes(
          Rank, Avg.y, colour = Family,
          text = glue::glue(
            "
          Identifier: {Identifier}
          Family: {Family}
          Avg.x: {Avg.x}
          Avg.y: {Avg.y}
          p-value: {ttest_pv}
          "
          )
        ), shape = 18
      ) +
      ggplot2::labs(x = "Average Feature Rank in Control Data", y = "Average Value", title = "Sorted Feature Plot") +
      ggplot2::theme_bw()
  )
  
  # make interactive figure and add shading at +/- 10 and 20% around the reference line
  fig <- plotly::ggplotly(sfp, tooltip = "text") %>%
    
    # shading at +/- 10%
    plotly::add_trace(
      data = mods, x = ~Rank, y = ~up10, type = "scatter", mode = "lines",
      line = list(color = "transparent"),
      showlegend = FALSE, name = "10% above control"
    ) %>%
    plotly::add_trace(
      x = ~Rank, y = ~down10, type = "scatter", mode = "lines",
      fill = "tonexty", fillcolor = "rgba(0,100,80,0.2)", line = list(color = "transparent"),
      showlegend = F, name = "10% below control"
    ) %>%
    
    # shading at +/- 20%
    plotly::add_trace(
      x = ~Rank, y = ~up20, type = "scatter", mode = "lines",
      line = list(color = "transparent"),
      showlegend = FALSE, name = "20% below control"
    ) %>%
    plotly::add_trace(
      x = ~Rank, y = ~down20, type = "scatter", mode = "lines",
      fill = "tonexty", fillcolor = "rgba(180,157,225,0.2)", line = list(color = "transparent"),
      showlegend = FALSE, name = "20% below control"
    )
  
  # return interactive plot
  return(fig)
}

## This function creates the lollipop plot for family enrichment
dualScreen_familyEnrichmentPlot <- function(familyData = NULL){
  
  # specify break values for y-axis of plot
  breakVals <- pretty(c(
    familyData %>% dplyr::pull(`Sig. Up`),
    familyData %>% dplyr::pull(`Sig. Down`)
  ))
  
  # initialize plot with segments for both types of TF (Sig. Up and Sig. Down)
  p <- ggplot2::ggplot(familyData) +
    ggplot2::geom_segment(ggplot2::aes(
      x = Family, xend = Family,
      y = `Sig. Up`, yend = 0,
      colour = Up_Sig
    ), size = 1) +
    ggplot2::geom_segment(ggplot2::aes(
      x = Family, xend = Family,
      y = `Sig. Down`, yend = 0,
      colour = Down_Sig
    ), size = 1)
  
  # add points for activators and repressors
  p <- p +
    ggplot2::geom_point(ggplot2::aes(
      x = Family, y = `Sig. Up`,
      text = glue::glue(
        "
      Type: Sig. Up
      Family: {Family}
      Family Size: {n}
      Obs. Up: {`Sig. Up`}
      Exp. Up: {Expected_Up}
      FET p-value: {Up_pvalue}
      "
      )
    ), size = 3, shape = 21, fill = "gold") +
    ggplot2::geom_point(ggplot2::aes(
      x = Family, y = `Sig. Down`,
      text = glue::glue(
        "
      Type: Sig. Down
      Family: {Family}
      Family Size: {n}
      Obs. Down: {abs(`Sig. Down`)}
      Exp. Down: {Expected_Down}
      FET p-value: {Down_pvalue}
      "
      )
    ), size = 3, shape = 21, fill = "skyblue")
  
  # add expected lines and horizontal zero line
  p <- p +
    ggplot2::geom_path(ggplot2::aes(x = Family, y = Expected_Up), group = 1, linetype = "dashed") +
    ggplot2::geom_path(ggplot2::aes(x = Family, y = Expected_Down * -1), group = 1, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, size = 1)
  
  # reformat plot, add labels, and adjust theme
  p <- p +
    ggplot2::scale_x_discrete(limits = rev) +
    ggplot2::scale_y_continuous(breaks = breakVals, labels = abs(breakVals)) +
    ggplot2::scale_colour_manual(values = c("black", "red")) +
    ggplot2::labs(
      x = "TF Family", y = "No. TF Types",
      title = "Family Distribution of Sig. Up (Gold) and Sig. Down (Blue)",
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
