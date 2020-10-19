

hc_plotter <- function(plot_dat, filters, plot_type = "Number of Ads", total_vs_overtime = "Total", targeting = "Geotargeting", mapdata = mapdata, platform) {
  
  if (plot_type %in% c("Number of Ads", "Spent (in EUR)", "Impressions")) {
    if(total_vs_overtime == "Total"){
      plot_dat_fin <- plot_dat$total
    } else if (total_vs_overtime == "Over Time"){
      plot_dat_fin <- plot_dat$times
    } else {
      return(invisible())
    }
  } else if (plot_type == "Targeted Ads"){
    if(targeting == "Geotargeting"){
      plot_dat_fin <- plot_dat$map_data
    } else if (targeting == "Over Time"){
      plot_dat_fin <- plot_dat$times
    } else {
      return(invisible())
    }   
  } else {
    return(invisible())
  }  
  
  if(platform == "Facebook"){
    credits_text <- "Source: Facebook Ad Library. Ads since September 1st 2020."
    href_text <- "https://www.facebook.com/ads/library/"
  } else if (platform == "Google"){
    credits_text <- "Source: Google Transparency Report. Ads since September 1st 2020."
    href_text <- "https://www.facebook.com/ads/library/"   
  }
  
  hc_data <- plot_dat_fin %>% 
    filter(advertiser_name %in% filters) 
  
  if(plot_type == "Number of Ads"){
    
    title_text <- glue::glue("Number of ads on {platform}")
    subtitle_text <- "Since September 1st 2020"
    
    if(total_vs_overtime == "Total"){
      
      
      lvls <- hc_data %>% 
        mutate(advertiser_name = fct_reorder(advertiser_name, n)) %>% 
        pull(advertiser_name) %>% 
        levels() %>% 
        rev()
      
      hc_plot <- hc_data %>% 
        hchart(
          type = "bar",
          hcaes(
            x = advertiser_name,
            y = n, 
            color = color),
          tooltip = list(pointFormat = "<b>Number of Ads:</b> {point.n}")) %>%
        hc_yAxis(title = list(text = "Number of Ads"))  %>% 
        hc_xAxis(categories = lvls, title = list(text = "")) 
    }
    
    if(total_vs_overtime == "Over Time"){
      hc_plot <- hc_data %>%
        hchart("line", hcaes(x = date_range_start,
                             y = n,
                             group = advertiser_name#,
                             #color = color
                             )) %>%
        hc_title(
          text = title_text
        ) %>%
        hc_yAxis(
          align = "left",
          title = list(text = "Number of Ads")
        ) %>%
        hc_xAxis(
          align = "left",
          title = list(text = "Start Date of Ad")
        ) %>%
        hc_colors(unique(hc_data$color))
      
      # fin <- hc_data %>% 
      #   apex(type = "line", mapping = aes(x = date_range_start, 
      #                                     y = n, 
      #                                     group = advertiser_name,
      #                                     color = color))  %>% 
      #   ax_colors(unique(hc_data$color)) %>% 
      #   ax_labs(
      #     title = title_text,
      #     subtitle = subtitle_text
      #   ) 
      # 
      # # print(hc_data)
      # 
      # return(fin)
    }
  } else if (plot_type == "Spent (in EUR)") {
    
    title_text <- glue::glue("Total spending on {platform} ads")
    subtitle_text <- glue::glue("{platform} only provides lower & upper bound of Euros spend")
    
    if(total_vs_overtime == "Total"){
    
    lvls <- hc_data %>% 
      mutate(advertiser_name = fct_reorder(advertiser_name, spend_range_mid)) %>% 
      pull(advertiser_name) %>% 
      levels() %>% 
      rev()
    
    hc_plot <- hc_data %>% 
      hchart(
        type = "errorbar",
        hcaes(
          x = advertiser_name,
          low = spend_range_min, 
          high = spend_range_max,
          color = color),
        tooltip = list(pointFormat = "<b>Lower bound:</b> {point.spend_range_min}€<br><b>Mid point:</b> {point.spend_range_mid}€<br><b>Upper bound:</b> {point.spend_range_max}€")) %>%
      hc_add_series(
        data = hc_data, 
        type = "point",
        hcaes(
          x = advertiser_name, 
          y = spend_range_mid,
          color = color
        ),
        name = "Euros spent",
        tooltip = list(pointFormat = "<b>Lower bound:</b> {point.spend_range_min}€<br><b>Mid point:</b> {point.spend_range_mid}€<br><b>Upper bound:</b> {point.spend_range_max}€<br><br><b>Number of Ads:</b> {point.n}"))%>%
      hc_yAxis(reversed = F, min = 0, title = list(text = "€ spent on Ads"))  %>% 
      hc_xAxis(categories = lvls, title = list(text = "")) %>% 
      hc_chart(inverted = TRUE)      
      }
    
    if(total_vs_overtime == "Over Time"){
      hc_plot <- hc_data %>%
        hchart("line", hcaes(x = date_range_start, 
                             y = spend_range_mid, 
                             group = advertiser_name),
               tooltip = list(pointFormat = "<b>Mid point:</b> {point.spend_range_mid}€")) %>%
        hc_title(
          text = title_text
        ) %>%
        hc_yAxis(
          align = "left",
          title = list(text = "Spent on Ads (in EUR)")
        ) %>%
        hc_xAxis(
          align = "left",
          title = list(text = "Start Date of Ad")
        ) %>% 
        hc_colors(unique(hc_data$color))
    }
    
  } else if (plot_type == "Impressions"){
    
    title_text <- glue::glue("How many impressions did {platform} ads get")
    subtitle_text <- glue::glue("{platform} only provides lower & upper bound of impressions")
    
    if(total_vs_overtime == "Total"){
      lvls <- hc_data %>% 
        mutate(advertiser_name = fct_reorder(advertiser_name, impressions_range_mid)) %>% 
        pull(advertiser_name) %>% 
        levels() %>% 
        rev()
      
      hc_plot <- hc_data %>% 
        drop_na(impressions_range_min, impressions_range_mid, impressions_range_max) %>% 
        hchart(
          type = "errorbar",
          hcaes(
            x = advertiser_name,
            low = impressions_range_min, 
            high = impressions_range_max,
            color = color),
          tooltip = list(pointFormat = "<b>Lower bound:</b> {point.impressions_range_min}€<br><b>Mid point:</b> {point.impressions_range_mid}€<br><b>Upper bound:</b> {point.impressions_range_max}€")) %>%
        hc_add_series(
          data = hc_data, 
          type = "point",
          hcaes(
            x = advertiser_name, 
            y = impressions_range_mid,
            color = color
          ),
          name = "Impressions",
          tooltip = list(pointFormat = "<b>Lower bound:</b> {point.impressions_range_min}<br><b>Mid point:</b> {point.impressions_range_mid}<br><b>Upper bound:</b> {point.impressions_range_max}<br><br><b>Number of Ads:</b> {point.n}"))%>%
        hc_yAxis(reversed = F, min = 0, title = list(text = "Impressions"))  %>% 
        hc_xAxis(categories = lvls, title = list(text = "")) %>% 
        hc_chart(inverted = TRUE)            
    }
    if(total_vs_overtime == "Over Time"){
      hc_plot <- hc_data %>%
        hchart("line", hcaes(x = date_range_start, 
                             y = impressions_range_mid, 
                             group = advertiser_name),
               tooltip = list(pointFormat = "<b>Mid point:</b> {point.impressions_range_mid}€")) %>%
        hc_title(
          text = title_text
        ) %>%
        hc_yAxis(
          align = "left",
          title = list(text = "Impressions")
        ) %>%
        hc_xAxis(
          align = "left",
          title = list(text = "Start Date of Ad")
        ) %>% 
        hc_colors(unique(hc_data$color))     
    }
  } else if (plot_type == "Targeted Ads"){
    
    # if(download_data){
    #   fb_aggr %>%
    #     hc_plotter(filters = filters,
    #                plot_type = "Targeted Ads",
    #                total_vs_overtime = "Total",
    #                targeting = "Geotargeting",
    #                platform = "Facebook")      
    # }
    
    n_cols <- length(unique(hc_data$advertiser_name))
    
    hc_fin <- hc_data %>% 
      group_split(advertiser_name) %>% 
      map(~{chart_maps(.x, F)}) %>% hw_grid(ncol = n_cols) %>% 
      htmltools::browsable()
    
    return(hc_fin)
  }
  
  hc_plot %>% 
    hc_title(
      text = title_text
    ) %>%
    hc_subtitle(
      text = subtitle_text
    ) %>% 
    hc_credits(
      enabled = T,
      text = credits_text,
      href = href_text
    )  %>%
    hc_exporting(
      enabled = TRUE
    ) %>% hw_grid(ncol = 1) 
}


chart_maps <- function(x, download_data = T) {
  hc <- hcmap2(
    "https://code.highcharts.com/mapdata/countries/nl/nl-all.js",
    custom_map = mapdata,
    data = x,
    download_map_data = F,
    value = "percentage",
    joinBy = c("name", "name"),
    name = "Median Ad Audience",
    dataLabels = list(enabled = TRUE, format = "{point.name}"),
    borderColor = "#FAFAFA",
    borderWidth = 0.1,
    tooltip = list(
      valueDecimals = 2,
      valueSuffix = "%"
    )
  ) %>% 
    hc_colorAxis(
      minColor = "white",
      maxColor = unique(x$colorful)
    )%>% 
    hc_title(
      text = unique(x$advertiser_name)
    ) %>%
    hc_exporting(
      enabled = TRUE
    )
  
  download_data <<- F
  
  return(hc)
}

hcmap2 <- function(map = "custom/world",
                  data = NULL, joinBy = "hc-key", value = NULL,
                  download_map_data = FALSE, custom_map = NULL, ...) {
  
  url <- "https://code.highcharts.com/mapdata"
  map <- str_replace(map, "\\.js", "")
  map <- str_replace(map, "https://code\\.highcharts\\.com/mapdata/", "")
  mapfile <- sprintf("%s.js", map)
  
  hc <- highchart(type = "map")
  
  if(download_map_data) {
    
    mapdata <- download_map_data(file.path(url, mapfile))
    
  } else {
    
    mapdata <- custom_map
    
  }
  
  if(is.null(data)) {
    
    hc <- hc %>% 
      highcharter:::hc_add_series.default(
        mapData = mapdata, ...)
    
  } else {
    
    stopifnot(joinBy %in% names(data))
    data <- mutate_(data, "value" = value)
    
    hc <- hc %>% 
      highcharter:::hc_add_series.default(
        mapData = mapdata,
        data = list_parse(data), joinBy = joinBy, ...) %>% 
      hc_colorAxis(auxpar = NULL)
    
  }
  
  hc
  
}

