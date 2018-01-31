shinyServer(function(input, output){
  ##############Yearly Sunlight by States################
  output$map <- renderGvis({
    data_filtered <- data %>% select(.,23,30) %>% group_by(state_name) %>%
        summarize(total=sum(yearly_sunlight_kwh_total)) %>% filter(.,total>input$kwh[1] & total<=input$kwh[2])
    gvisGeoChart(data_filtered, "state_name", "total",
               options=list(region="US", displayMode="Regions", 
                            resolution="provinces",
                            width=600, height=400, colorAxis="{colors:[\'white',\'yellow',\'orange',\'red',\'brown']}"))
  })
  ##############Coal Consumption by State################ 
  output$map2 <- renderGvis({
    data_filtered3 <- df2 %>% group_by(.,States,date1) %>% summarise(Total=sum(as.numeric(CoalTons))) %>% 
      select(date1,States,'Coal in Thousands Tons'=Total) %>% arrange(.,date1) %>% 
      filter(.,date1==input$year)
    gvisGeoChart(data_filtered3, "States", "Coal in Thousands Tons",
                 options=list(region="US", displayMode="Regions", 
                              resolution="provinces",
                              width='auto', height='auto',
                              gvis.editor= "COAL CONSUMPTION OVER YEARS (in thousand tons)"))
    })
 
  ##############Natural Gas Consumption by State################ 
  output$map3 <- renderGvis({
    data_filtered4 <- df3 %>% group_by(.,States,date1) %>% summarise(Total=sum(as.numeric(NGTons))) %>% 
      select(date1,States,'Natural Gas in Thousands Tons'=Total) %>% arrange(.,date1) %>% 
      filter(.,date1==input$year)
    gvisGeoChart(data_filtered4, "States", "Natural Gas in Thousands Tons",
                 options=list(region="US", displayMode="Regions", 
                              resolution="provinces",
                              width='auto', height='auto', colorAxis="{colors:[\'white',\'blue']}",
                              gvis.editor= "NATURAL GAS CONSUMPTION OVER YEARS (in thousand tons)"))
  })
  
  output$table <- renderGvis({
    data_filtered2 <- data %>% select(.,23,30) %>% group_by(state_name) %>%
      summarize(total=sum(yearly_sunlight_kwh_total)) %>% arrange(., desc(total)) %>% top_n(15) %>% mutate(Rank = dense_rank(desc(total))) %>% rename(., 'State'='state_name', 'Total Sunlight (KWH)'='total')

  gvisTable(data_filtered2)
  })

  #########################PV Cost Over Years###############################
  output$dyg <- renderDygraph({
    #res=res[is.na(res$Multi.c.Si.Module.Spot.Price...watt)!=1 | is.na(res$Thin.Film.Module.Spot.Price...watt)!=1]
    df1 <- res %>%
      rename(dt=Description, MultiPV=Multi.c.Si.Module.Spot.Price...watt, ThinPV=Thin.Film.Module.Spot.Price...watt) %>%
      mutate(date = as.Date(paste0('01/', c(as.character(dt))), format = "%d/%m/%Y")) %>%
      select(-dt) 
    xtdata <- xts(df1, order.by = df1$date) 
    xtdata$date <- NULL
    dygraph(xtdata, main='Prices of Photovoltaic Cells over Years ($/watt)', xlab='Time Series', ylab='Price in $/watt') %>% dyRangeSelector() %>%
      dySeries("MultiPV", label = "Multi Module PV", fillGraph=TRUE) %>%
      dySeries("ThinPV", label = "Thin Module PV ", fillGraph=TRUE) %>%
      dyRangeSelector(height = 50) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))
  
  })
 
  ###################Chloropleth Sunlight dist in US Counties########################## 
  output$map4 <- renderLeaflet({
    CountyData=CountyData[is.na(CountyData$yearly_sunlight_kwh_total)!=1,]
    TexasCountyData = CountyData %>%
      group_by(region_name) %>%
      summarise(CarbonOffset = sum(as.numeric(yearly_sunlight_kwh_total))/1000) %>%
      arrange(desc(CarbonOffset)) %>%
      mutate(region_name =  reorder(region_name,CarbonOffset))
    head(TexasCountyData)
    TexasCounties2 = sapply(str_split(TexasCountyData$region_name," "), head, 1)
    TexasCountyData$region_name = TexasCounties2
    USCounties@data$NAME = as.character(USCounties@data$NAME)
    USCounties@data = inner_join(USCounties@data, TexasCountyData, by = c("NAME" = "region_name"))
    bins = c(0,1000000,3000000, 6000000, 12000000, 100000000, 500000000, Inf)
    pal = colorBin("YlOrRd", domain = USCounties@data$CarbonOffset, bins = bins)
    labels = sprintf(
      "<strong>%s</strong><br/>%g",
      USCounties@data$NAME, USCounties@data$CarbonOffset
      ) %>% lapply(htmltools::HTML)
#View(USCounties@data)
    
    leaflet(data = USCounties) %>% setView(-95.613281, 37.203405, 4) %>%
        addPolygons(
          fillColor = ~pal(CarbonOffset),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "1",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
           leaflet::addLegend(pal = colorBin("YlOrRd", domain = USCounties@data$CarbonOffset, bins = bins), values = ~CarbonOffset, opacity = 0.7, title = "Sunlightin '000 KWH",
                  position = "bottomright")


  })
  
  output$map5 <- renderLeaflet({ 
  CarbonOffsetCities = CityData %>%
    arrange(desc(carbon_offset_metric_tons)) %>%
    head(50)
  
  center_lon = median(CarbonOffsetCities$lng_avg)
  center_lat = median(CarbonOffsetCities$lat_avg)
  
  leaflet(CarbonOffsetCities) %>% addProviderTiles("OpenMapSurfer.Roads") %>%
    addCircles(lng = ~lng_avg, lat = ~lat_avg,weight=1,radius = ~sqrt(carbon_offset_metric_tons)*50, 
               color = c("red"))  %>%
    setView(lng=center_lon, lat=center_lat, zoom=4)
  
  })
  
#######################Top City Opportunity################## 
  output$bar1 <- renderPlot({ 
  data_filtered3 <- data %>% select(.,2,3,23) %>% group_by(state_name) %>% 
    summarize(total1=sum(count_qualified), total2=sum(existing_installs_count)*15) %>% 
    mutate(., diffn=total1-total2) %>% arrange(.,desc(diffn)) %>% head(input$n) %>%
    select(.,state_name, 'Building Count Qualified'=total1, 'Existing Building Installs'=total2)
  data_melted<-melt(data_filtered3, id="state_name")
  ggplot(data_melted, aes(x=state_name, y=value, fill=variable)) + 
    geom_bar(stat="identity") + labs(x="State",y="Count Qualified with Existing Installs") +
     guides(fill=guide_legend(title=NULL))
  })

###################Pie Chart Data#######################
  
  output$plot <- renderPlotly({
    plot_ly(elec, labels = ~ElectricityUSA, values =~Percentage, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>%
                layout(title = 'United States Electricity Generation Percent Share',
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  })
})




