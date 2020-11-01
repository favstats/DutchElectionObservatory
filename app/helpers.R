

hc_plotter <- function(plot_dat, filters, plot_type = "Number of Ads", total_vs_overtime = "Total", targeting = "Geo targeting", mapdata = mapdata, platform, js_scrip) {
  
  if (plot_type %in% c("Number of Ads", "Spent (in EUR)", "Impressions")) {
    if(total_vs_overtime == "Total"){
      plot_dat_fin <- plot_dat$total
    } else if (total_vs_overtime == "Over Time"){
      plot_dat_fin <- plot_dat$times
    } else {
      return(invisible())
    }
  } else if (plot_type == "Targeted Ads"){
    if(targeting == "Geo targeting"){
      plot_dat_fin <- plot_dat$geo
    } else if (targeting == "Gender targeting"){
      plot_dat_fin <- plot_dat$gender
    } else if (targeting == "Age targeting"){
      plot_dat_fin <- plot_dat$age
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
  
  
  if(!is.null(plot_dat_fin$page_name)){
    plot_dat_fin <- plot_dat_fin %>% rename(advertiser_name = page_name)
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
    subtitle_text <- glue::glue("{platform} only provides lower & upper bound of Euros spent")
    
    if(total_vs_overtime == "Total"){
    
    lvls <- hc_data %>% 
      mutate(advertiser_name = fct_reorder(advertiser_name, spend_range_mid)) %>% 
      pull(advertiser_name) %>% 
      levels() %>% 
      rev()
    
    
    # js_scrip <- "function() { return '<a target=\"_top\" href=\"https://www.france-politique.fr/election-presidentielle-1965.htm\">' + this.value + '</a>';}"
    

    # var obj = {
    #   key1: "xd",
    #   key2: "xw",
    #   GroenLinks: "AR148211418645135360"
    # };
    # 
    # var getProperty = function (propertyName) {
    #   return obj[propertyName];
    # };
    # 
    # document.write(getProperty("GroenLinks"));
    # 
    # getProperty("key1");
    # getProperty("key2");
    
    
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
      hc_xAxis(categories = lvls, title = list(text = "")#,  
               # labels = list(
               #   formatter = JS(js_scrip)
               # )
               ) %>% 
      hc_chart(inverted = TRUE)  
    }
    

    # return '<a target="_top" href=" '+ categoryLinks[this.value] + '">' + this.value + '</a>';
    # 
    # JS("function(){
    #   
    #                   return '<a  href=" '+ categoryLinks[this.value] + '">' + this.value + '</a>';
    #                   }")
    # 
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
          tooltip = list(pointFormat = "<b>Lower bound:</b> {point.impressions_range_min}<br><b>Mid point:</b> {point.impressions_range_mid}<br><b>Upper bound:</b> {point.impressions_range_max}")) %>%
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
               tooltip = list(pointFormat = "<b>Mid point:</b> {point.impressions_range_mid}")) %>%
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
    
    if(targeting == "Geo targeting"){
      
      if(platform == "Facebook"){
        n_cols <- length(unique(hc_data$advertiser_name))
        
        if(n_cols > 3){
          # print("hello")
          hc_fin <- tibble(text = "Please select up to 3 advertisers", value = 12) %>% 
            hchart("wordcloud", hcaes(name = text, weight = value)) %>% hw_grid(ncol = 1) 
          
          return(hc_fin)
        }
        
        hc_fin <- hc_data %>% 
          group_split(advertiser_name) %>% 
          map(~{chart_maps(.x, F)}) %>% hw_grid(ncol = n_cols) %>% 
          htmltools::browsable()
        
        return(hc_fin)             
      } else if (platform == "Google"){
        
        title_text <- glue::glue("Geo targeting with {platform} ads")
        subtitle_text <- glue::glue("")
        
        
        hc_plot <- hc_data %>% 
          filter(n != 0) %>% 
          hchart("bar", hcaes(x = advertiser_name, y = perc, group = geo_targeting_included)) %>%
          hc_xAxis(
            align = "left",
            title = list(text = "Advertiser Name")
          ) %>%
          hc_yAxis(
            align = "left",
            title = list(text = "Percentage of Ads")
          ) 
        
      }
    } else if (targeting == "Gender targeting"){
      
      
      title_text <- glue::glue("Gender targeting with {platform} ads")
      subtitle_text <- glue::glue("")
      
      
      if(platform == "Google"){
        
        hc_plot <- hc_data %>% 
          # filter(n != 0) %>% 
          hchart("bar", hcaes(x = advertiser_name, y = perc, group = gender_targeting))%>%
          hc_xAxis(
            align = "left",
            title = list(text = "Advertiser Name")
          ) %>%
          hc_yAxis(
            align = "left",
            title = list(text = "Percentage of Ads")
          ) 
      } else if (platform == "Facebook"){
        
        
        hc_data <- hc_data %>% 
          filter(gender %in% c("male", "female")) %>% 
          data_to_boxplot(percentage, advertiser_name, gender)
        
        hc_plot <-  highchart() %>%
          hc_xAxis(type = "category") %>%
          hc_add_series_list(hc_data)%>%
          hc_yAxis(reversed = F, min = 0, title = list(text = "% Audience of Ad")) # %>% 
          # hc_chart(inverted = TRUE)
        
      }
    } else if (targeting == "Age targeting"){
      
      title_text <- glue::glue("Age targeting with {platform} ads")
      subtitle_text <- glue::glue("")
      
      if(platform == "Google"){
        
        hc_plot <- hc_data %>% 
          mutate(age_targeting2 = case_when(
            age_targeting2 == "18-24, 25-34, 35-44, 45-54, 55-64" ~ "18-65",
            age_targeting2 == "18-24, 35-44, 45-54, 55-64, ≥65" ~ "18-24, 35-65+",
            age_targeting2 == "18-24, 45-54, 55-64, ≥65" ~ "18-24, 45-65+",
            age_targeting2 == "25-34, 35-44, 45-54, 55-64, ≥65" ~ "25-65+",
            T ~ age_targeting2
          )) %>% 
          # filter(n != 0) %>% 
          hchart("bar", hcaes(x = advertiser_name, y = perc, group = age_targeting2))%>%
          hc_xAxis(
            align = "left",
            title = list(text = "Advertiser Name")
          ) %>%
          hc_yAxis(
            align = "left",
            title = list(text = "Percentage of Ads")
          ) 
      } else if (platform == "Facebook"){
        
        hc_data <- hc_data %>% 
          data_to_boxplot(percentage, advertiser_name, age)
          
        
        hc_plot <-  highchart() %>%
          hc_xAxis(type = "category") %>%
          hc_add_series_list(hc_data)%>%
          hc_yAxis(reversed = F, min = 0, title = list(text = "% Audience of Ad")) # %>% 
          # hc_chart(inverted = TRUE)
        
      }
    }
    

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
      maxColor = unique(x$colorful),
      min = 0,
      max = 35
    )%>% 
    hc_title(
      text = unique(x$advertiser_name)
    ) %>%
    hc_exporting(
      enabled = TRUE
    )
  
  # download_data <<- F
  
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


radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}
