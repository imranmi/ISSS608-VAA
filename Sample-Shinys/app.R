#
#
# load R packages
pacman::p_load(shiny, shinydashboard, shinycssloaders, tidyverse, dplyr, leaflet, plotly, highcharter, 
               ggthemes, fresh, sf, spdep, tmap, tm, ggforce, ggraph, igraph, wordcloud, tidytext, DT)

# read data files
final <- readRDS("data/final.rds")
mapping_rates <- readRDS("data/mapping_rates.rds")


###############################   
########### MAIN BODY - START
###############################

#==========================================================  
# main header ---
header <- dashboardHeader(title = "Decoding Chaos")


# main sidebar ---
sidebar <- dashboardSidebar(
  tags$style(HTML("
      .main-sidebar{
        width: 250px;
      }
      .main-header > .navbar {
        margin-left: 250px;      
      }
      
      
      .box.box-solid.box-info>.box-header {
      color:#000000;
      background:#F8F8F8
                    }
      .box.box-solid.box-info{
      border-bottom-color:#9A3E41;
      border-left-color:#F8F8F8;
      border-right-color:#F8F8F8;
      border-top-color:#9A3E41;
      }
      .box.box-info>.box-header {
      color:black; 
      background:#F8F8F8
      }
      .box.box-info{
      border-bottom-color:#9A3E41;
      border-left-color:#F8F8F8;
      border-right-color:#F8F8F8;
      border-top-color:#9A3E41;
      background: #F8F8F8;
      }
                  ")),
  
  sidebarMenu(
    width = 100,
    menuItem("Exploratory", tabName = "Exploratory", icon = icon("globe")),
    menuItem("Clustering & Outlier Analysis", tabName = "Clustering", icon = icon("circle-nodes")),
    menuItem("Hot/ Cold Zone Analysis", tabName = "HotCold", icon = icon("magnifying-glass-chart")),
    menuItem("Confirmatory Analysis", tabName = "Confirmatory", icon = icon("clipboard-check")),
    menuItem("Visit ACLED data", icon = icon("send",lib='glyphicon'), 
             href = "https://acleddata.com/data-export-tool/")))


# customise theme ---
mytheme <- create_theme(
  adminlte_color(
    red = "#9A3E41"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#F8F8F8",
    dark_hover_bg = "#9A3E41",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF", 
    info_box_bg = "#FFF"
  )
)

# =============================    
########### EXPLORATORY - START
# =============================


#==========================================================  
# ExploreOverviewrow1 ---
#==========================================================  
ExploreOverviewrow1 <-  fluidRow(
  box(title = "Armed Conflict Incidents in Myanmar (2010 to 2023)",
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      column(9,
             box(width = 12,  # Automatically adjust to full width
                 align = "center",
                 withSpinner(leafletOutput("emap_eo1", height = "650px", width = "100%"))  # Set map width to 100% of box
             )   
      ),
      column(3,
             box(title = "Desired Characteristics",
                 status = "info",
                 solidHeader = FALSE, 
                 width = NULL,
                 selectizeInput(inputId = "EventSelect_eo1",
                                label = "Select Event(s)",
                                choices = unique(final$event_type),
                                multiple = TRUE,
                                options = list(maxItems = 6, placeholder = 'Enter event type',
                                               onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 hr(),
                 selectizeInput(inputId = "AdminSelect_eo1",
                                label = "Select Administrative Region(s)",
                                choices = unique(final$admin1),
                                multiple = TRUE,
                                options = list(maxItems = 18, placeholder = 'Enter Admin Region',
                                               onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 hr(),
                 sliderInput(inputId = "YearSlider_eo1", 
                             label = "Years:", 
                             min = 2010, 
                             max = 2023,
                             value = c(2010, 2023))
             ),
             hr(),
             box(title = "About",
                 status = "danger",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 width = NULL,
                 align = "justify",
                 textOutput("abouttext") 
             )
      )
  )
  
)



#==========================================================  
# ExploreGeospatialrow1 ---
#==========================================================  
ExploreGeospatialrow1 <-  fluidRow(
  
  column(2,
         box(title = "Desired Characteristics",
             status = "info",
             solidHeader = FALSE, 
             width = NULL, 
             helpText("Filter options are applicable for statistical distribution"),
             radioButtons(inputId = "InciFata_eg1",
                          label = "Display",
                          choices = c("No. of Incidents" = "total_inci", "No. of Fatalities" = "total_fata"),
                          selected = "No. of Incidents")
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "justify",
             textOutput("statisticaltext") 
         )
         
  ),
  column(4,
         box(title = "Statistical Distribution",
             status = "danger",
             solidHeader = TRUE,
             width = NULL,  # Automatically adjust to full width
             align = "center",
             withSpinner(tmapOutput("emap_eg1", height = "700px", width = "100%"))
         )   
  ),
  column(4,
         box(title = "Spatial Distribution",
             status = "danger",
             solidHeader = TRUE,
             width = NULL,
             align = "center",
             withSpinner(tmapOutput("emap_eg2", height = "700px", width = "100%"))
         )   
  ),
  column(2,
         box(title = "Desired Characteristics",
             status = "info",
             solidHeader = FALSE, 
             width = NULL, 
             helpText("Filter options are applicable for spatial distribution"),
             selectInput(inputId = "YearSelect_eg2", 
                         label = "Year:", 
                         choices = unique(final$year), 
                         # selected = 2023,
             ),
             hr(),
             radioButtons(inputId = "InciFata_eg2",
                          label = "Display",
                          choices = c("Incident Rate" = "total_inci", "Fatality Rate" = "total_fata"),
                          selected = "Incident Rate"),
             hr(),
             selectInput(inputId = "ClassificationSelect_eg2", 
                         label = "Classification Type:", 
                         choices = c("equal", "pretty", "quantile", "kmeans"),
                         selected = "kmeans"
             ),
             hr(),
             selectInput(inputId = "ClassSelect_eg2", 
                         label = "Number of Classes:", 
                         choices = c(2, 4, 6, 8, 10),
                         selected = 10
             )),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "justify",
             textOutput("spatialtext") 
         )
  )
  
)


#==========================================================  
# ExploreTrendrow1 ---
#==========================================================  
ExploreTrendrow1 <-  fluidRow(
  box(
    title = "Rising Armed Conflict Incidents in Myanmar (2010 to 2023)",
    status = "danger",
    solidHeader = TRUE, 
    collapsible = TRUE,
    width = 12,  # Use full width
    column(8,
           box(
             width = NULL,
             withSpinner(highchartOutput("line_et1", height = "400px"))  # Width is automatically 100% of the box
           )
    ),
    column(
      4,
      
      box(title = "Desired Characteristics",
          status = "info",
          solidHeader = FALSE, 
          width = NULL, 
          helpText("Filter options are applicable to line chart"),
          selectizeInput(inputId = "EventSelect_et1",
                         label = "Select Event(s)",
                         choices = unique(final$event_type),
                         multiple = TRUE,
                         options = list(maxItems = 6, placeholder = 'Enter event type',
                                        onInitialize = I('function() { this.setValue(""); }'))
          ),
          hr(),
          selectizeInput(inputId = "AdminSelect_et1",
                         label = "Select Administrative Region(s)",
                         choices = unique(final$admin1),
                         multiple = TRUE,
                         options = list(maxItems = 18, placeholder = 'Enter Admin Region',
                                        onInitialize = I('function() { this.setValue(""); }'))
          ),
          hr(),
          sliderInput(inputId = "YearSlider_et1", 
                      label = "Years:", 
                      min = 2010, 
                      max = 2023,
                      value = c(2010, 2023))
      )
    )
  )
)

#==========================================================  
# ExploreTrendrow2 ---
#==========================================================  
ExploreTrendrow2 <-  fluidRow(
  box(
    title = "Annual Calendar",
    status = "danger",
    solidHeader = TRUE, 
    collapsible = TRUE,
    width = 12,  # Use full width
    column(8,
           box(
             width = 12,  # Use full width
             withSpinner(plotlyOutput("calendar_et1", height = "400px"))
           )
    ),
    column(
      4,
      
      box(title = "Desired Characteristics",
          status = "info",
          solidHeader = FALSE, 
          width = 12,  # Use full width
          helpText("Filter options are applicable to calendar"),
          selectInput(inputId = "YearSelect_et2", 
                      label = "Year:", 
                      choices = unique(final$year), 
                      # selected = 2023,
          ),
          hr(),
          radioButtons(inputId = "InciFata_et2",
                       label = "Display",
                       choices = c("No. of Incidents" = "total_inci", "No. of Fatalities" = "total_fata"),
                       selected = "No. of Fatalities")
      )
    )
  )
)


#==========================================================  
# ExploreDistributionrow1 ---
#==========================================================  
ExploreDistributionrow1 <-  fluidRow(
  column(4
  ),
  column(6,
         sliderInput(inputId = "YearSlider_ed1", 
                     label = "Years:", 
                     min = 2010, 
                     max = 2023,
                     value = c(2010, 2023))
  ),
  column(4
  )
)

#==========================================================  
# ExploreDistributionrow2 ---
#========================================================== 
ExploreDistributionrow2 <- fluidRow(
  column(6,
         box(
           title = "Distribution of armed conflicts in Myanmar based on Sub-national Administrative Region 1",
           status = "danger",
           solidHeader = TRUE, 
           width = 12,
           height = 700,
           withSpinner(plotlyOutput("box_ed1", height = "600px"))  
         )),
  column(6,
         box(
           title = "Distribution of armed conflicts in Myanmar based on Events",
           status = "danger",
           solidHeader = TRUE, 
           width = 12,
           height = 700,
           withSpinner(plotlyOutput("box_ed2", height = "600px"))  # Width is automatically 100% of the box
         ))
)

#==========================================================  
# ExploreDistributionrow3 ---
#========================================================== 
ExploreDistributionrow3 <- fluidRow(
  column(12,
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "justify",
             textOutput("distributiontext") 
         ))
)

#==========================================================  
# ExploreNetworkrow1 ---
#==========================================================  
ExploreNetworkrow1 <-  fluidRow(
  box(
    title = "Network Relationship Among Actors in Myanmar",
    status = "danger",
    solidHeader = TRUE, 
    width = 12,  # Use full width
    column(9,
           box(width = NULL,
               align = "center",
               withSpinner(plotOutput("network_en1", height = "600px", width = "100%"))  # Set map width to 100% of box
           )
    ),
    column(3,
           box(title = "Desired Characteristics",
               status = "info",
               solidHeader = FALSE, 
               width = NULL, 
               selectizeInput(inputId = "EventSelect_en1",
                              label = "Select Event(s)",
                              choices = unique(final$event_type),
                              multiple = TRUE,
                              options = list(maxItems = 6, placeholder = 'Enter event type',
                                             onInitialize = I('function() { this.setValue(""); }'))
               ),
               hr(),
               selectizeInput(inputId = "AdminSelect_en1",
                              label = "Select Administrative Region(s)",
                              choices = unique(final$admin1),
                              multiple = TRUE,
                              options = list(maxItems = 18, placeholder = 'Enter Admin Region',
                                             onInitialize = I('function() { this.setValue(""); }'))
               ),
               hr(),
               selectInput(inputId = "YearSelect_en1", 
                           label = "Year:", 
                           choices = unique(final$year), 
                           # selected = 2023,
               ),
           ),
           box(title = "Chart Interpretation",
               status = "danger",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = NULL,  # Automatically adjust to full width
               align = "justify",
               textOutput("networktext") 
           )
    )
  ) 
)



#==========================================================  
# ExploreSummaryrow1 ---
#========================================================== 
ExploreSummaryrow1 <-  fluidRow(
  column(4,
         box(
           title = "Word Cloud - Incident Summary",
           status = "danger",
           solidHeader = TRUE, 
           collapsible = TRUE,
           width = NULL,
           height = 800,
           box(title = "Desired Characteristics",
               status = "info",
               solidHeader = FALSE, 
               width = NULL,
               helpText("Filter options are applicable to word cloud"),
               selectizeInput(inputId = "EventSelect_es1",
                              label = "Select Event(s)",
                              choices = unique(final$event_type),
                              multiple = TRUE,
                              options = list(maxItems = 6, placeholder = 'Enter event type',
                                             onInitialize = I('function() { this.setValue(""); }'))
               ),
               hr(),
               selectizeInput(inputId = "AdminSelect_es1",
                              label = "Select Region(s)",
                              choices = unique(final$admin1),
                              multiple = TRUE,
                              options = list(maxItems = 18, placeholder = 'Enter Admin Region',
                                             onInitialize = I('function() { this.setValue(""); }'))
               ),
               hr(),
               selectInput(inputId = "YearSelect_es1", 
                           label = "Year:", 
                           choices = unique(final$year), 
                           # selected = 2023,
               )),
           withSpinner(plotOutput("cloud_es1", height = "300px", width = "100%"))  # Set map width to 100% of box
           
         )),
  
  column(8,
         box(
           title = "Data Table",
           status = "danger",
           width = NULL,
           solidHeader = TRUE, 
           collapsible = TRUE,
           withSpinner(DT::dataTableOutput(outputId = "ExploreSummaryTable")),
           style = "height:750px; overflow-y: scroll;overflow-x: scroll;" # Set scrollbars
         )
         
  )
  
)


#==========================================================  
# ExploreSubTabs
#==========================================================  
ExploreSubTabs <- tabsetPanel(
  tabPanel("Overview", 
           ExploreOverviewrow1
  ),
  tabPanel("Geospatial Exploration", 
           ExploreGeospatialrow1
  ),
  tabPanel("Trends", 
           ExploreTrendrow1,
           ExploreTrendrow2
  ),  
  tabPanel("Distributions", 
           ExploreDistributionrow1,
           ExploreDistributionrow2,
           ExploreDistributionrow3
  ),
  tabPanel("Network Relationships", 
           ExploreNetworkrow1
  ),
  tabPanel("Incident Summary", 
           ExploreSummaryrow1
  )
)

# =============================    
########### EXPLORATORY - END
# =============================

# main body ---
body <- dashboardBody(
  
  # use theme
  use_theme(mytheme),
  
  
  # =============================
  # tabItems - All Pages
  # =============================
  tabItems(
    # 1st tab content ---
    tabItem(tabName = "Exploratory",
            ExploreSubTabs
    ),
    # 2nd tab content ---
    tabItem(tabName = "Cluster and Outlier Analysis"
            #/// replace with fluidRow Names
    ),
    # 3rd tab content ---
    tabItem(tabName = "Hot and Cold Zone Analysis",
            #/// replace with fluidRow Names  
    ),
    # 4th tab content ---
    tabItem(tabName = "Confirmatory Analysis"
            #/// replace with fluidRow Names
    )
  )
)  



###############################   
########### MAIN BODY - END
###############################


# =============================    
########### UI - START
# =============================

# UI dashboard ---
ui <- dashboardPage(title = 'Armed Conflicts in Myanmar (2010 to 2023)', 
                    header, sidebar, body, skin='red')    

# =============================    
########### UI - END
# =============================


# =============================    
########### SERVER - START
# =============================
server <- function(input, output) {
  
  # =============================    
  # EXPLORATORY
  # =============================
  
  #==========================================================
  # Explore Point Map ---
  #==========================================================    
  final_explorespatial <- reactive({
    req(input$YearSlider_eo1)
    req(input$EventSelect_eo1)
    req(input$AdminSelect_eo1)
    filter(final, event_type %in% input$EventSelect_eo1) %>%
      filter(final$admin1 %in% input$AdminSelect_eo1) %>%
      filter(final$year %in% input$YearSlider_eo1)
    
  })
  
  # Render Point Map --- 
  output$emap_eo1 <- renderLeaflet({
    
    incident_pts <- final %>%
      filter(fatalities > 0)
    
    cof <- colorFactor(c("#ff7e8a", "#394938", "#ffa500", 
                         "#0092ff", "#741b47", "#60dcb5"), 
                       domain=c("Battles", "Explosions/ Remote violence", 
                                "Protests", "Riots",
                                "Strategic developments", 
                                "Violence against civilians"))
    
    leaflet(final_explorespatial) %>% 
      addTiles('https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
      setView(100, 20, zoom = 5) %>%
      addCircles(data=incident_pts, lat= ~latitude, lng = ~longitude, 
                 color = ~cof(event_type), 
                 fillColor = "black",
                 popup = paste( "<strong>", incident_pts$event_date, "</strong>", 
                                "<br><strong>Country: </strong>",
                                incident_pts$country, 
                                "<br><strong>Sub-national Admin Region: </strong>",
                                incident_pts$admin1, "/", 
                                incident_pts$admin2, "/", 
                                incident_pts$admin3, 
                                "<br><strong>Event type: </strong>",
                                incident_pts$event_type, 
                                "<br><strong>Sub-event type: </strong>", 
                                incident_pts$sub_event_type, 
                                "<br><strong>Summary: </strong>", 
                                incident_pts$notes,
                                "<br><strong>Total Fatalities: </strong>",
                                incident_pts$fatalities))
  })
  
  output$abouttext <- renderText({ 
    "Armed conflicts due to political violence and coordinated attacks targeting innocent civilians, have been on the rise globally. 
      This threatens the public at both physical and psychological levels. 
      this visualisation dashboard helps: 
      (1) discover armed conflicts trends and 
      (2) conceptualise armed conflict spaces in Myanmar." 
  })
  
  #==========================================================
  # Explore Statistical Distribution Map ---
  #==========================================================   
  mapping_rates_explorestats <- reactive({
    req(input$InciFata_eg1)
  })
  
  # Render Statistical Distribution Map --- 
  output$emap_eg1 <- renderTmap({
    
    # create boxbreak function
    boxbreaks <- function(v,mult=1.5) {
      qv <- unname(quantile(v))
      iqr <- qv[4] - qv[2]
      upfence <- qv[4] + mult * iqr
      lofence <- qv[2] - mult * iqr
      # initialize break points vector
      bb <- vector(mode="numeric",length=7)
      # logic for lower and upper fences
      if (lofence < qv[1]) {  # no lower outliers
        bb[1] <- lofence
        bb[2] <- floor(qv[1])
      } else {
        bb[2] <- lofence
        bb[1] <- qv[1]
      }
      if (upfence > qv[5]) { # no upper outliers
        bb[7] <- upfence
        bb[6] <- ceiling(qv[5])
      } else {
        bb[6] <- upfence
        bb[7] <- qv[5]
      }
      bb[3:5] <- qv[2:4]
      return(bb)
    }
    
    # create get.var function
    get.var <- function(vname,df) {
      v <- df[vname] %>% st_set_geometry(NULL)
      v <- unname(v[,1])
      return(v)
    }
    
    # create boxmap function
    boxmap <- function(vnam, df, 
                       legtitle=NA,
                       mtitle= paste0("Box Map of ", vnam),
                       mult=1.5){
      var <- get.var(vnam,df)
      bb <- boxbreaks(var)
      tm_shape(df) +
        tm_polygons() +
        tm_shape(df) +
        tm_fill(vnam,title=legtitle,
                breaks=bb,
                palette="Reds",
                labels = c("lower outlier", 
                           "< 25%", 
                           "25% - 50%", 
                           "50% - 75%",
                           "> 75%", 
                           "upper outlier"))  +
        tm_layout(main.title = mtitle, 
                  title.position = c("left",
                                     "top"))
    }
    
    box_inci <- boxmap("total_inci", mapping_rates)
    
    
  })
  
  ########################################
  output$statisticaltext <- renderText({ 
    "A boxmap is a spatial representation akin to boxplot and histogram, 
    which is useful to detect outliers and visualise distribution of variables across Myanmar's
    different geographical subnational administrative region 2." 
  })
  
  #==========================================================
  # Explore Spatial Distribution Map ---
  #==========================================================  
  mapping_rates_exploredist <- reactive({
    req(input$InciFata_eg2)
    req(input$YearSelect_eg2)
    req(input$ClassSelect_eg2)
    req(input$ClassificationSelect_eg2)
    
  })
  
  
  # Render Spatial Distribution Map --- 
  output$emap_eg2 <- renderTmap({
    tmap_obj2 <- tm_shape(mapping_rates) +
      tm_fill("pct_fata_2023", 
              n = 10,             # to replace with input$ClassSelect_eg2
              style = "kmeans",   # to replace with input$ClassificationSelect_eg2
              palette = "Reds", 
              legend.hist = TRUE, 
              legend.is.portrait = TRUE, 
              legend.hist.z = 0.1) +
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_layout(main.title = "Distribution of Armed Conflict Incidents in Myanmar \nKmeans classification",
                title = "",
                main.title.size = 1,
                legend.height = 0.60,
                legend.width = 5.0,
                legend.outside = FALSE,
                legend.position = c("left", "bottom"))
    
    
  })
  
  ########################################  
  output$spatialtext <- renderText({ 
    "A choropleth map visualises spatial pattern or distribution across Myanmar's
    different geographical subnational administrative region 2, which can be further customised 
    by adjusting the desired data classification type and number of classes (or data ranges)." 
  })
  
  #==========================================================  
  # Explore Trends ---
  #==========================================================  
  final_exploretime <- reactive({
    req(input$YearSlider_et1)
    req(input$EventSelect_et1)
    req(input$AdminSelect_et1)
    filter(final, event_type %in% input$EventSelect_et1) %>%
      filter(final$admin1 %in% input$AdminSelect_et1) %>%
      filter(final$year %in% input$YearSlider_et1)
    
  })
  
  calendar <- final %>%
    filter(fatalities > 0) %>%
    group_by(year, event_date, admin1) %>%
    mutate(
      wkday = wday(event_date),
      day = mday(event_date),
      month = factor(months(event_date), levels = rev(month.name)),
      week = isoweek(event_date),
      year_month = format(zoo::as.yearmon(event_date), "%y-%m")
    ) %>%
    ungroup()
  
  
  # Render Line Chart --- 
  output$line_et1 <- renderHighchart({
    
    year_fata <- calendar %>%
      filter(fatalities > 0) %>%
      group_by(year_month) %>%
      select(year, month, year_month, fatalities) %>%
      summarise(total_fata = sum(fatalities),
                total_inci = n()) %>%
      ungroup()
    
    hc_plot1 <-  highchart() %>% 
      hc_add_series(year_fata, hcaes(x = year_month, y = total_fata), type = "line", 
                    name = "Total Fatalities", color = "lightcoral") %>%
      hc_add_series(year_fata, hcaes(x = year_month, y = total_inci), type = "line", 
                    name = "Total Incidents", color = "black") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", 
                 backgroundColor = "#FCFFC5",
                 borderWidth = 5,
                 pointFormat = "<b>20{point.year_month}</b> 
                                 <br> Fatalities: <b>{point.total_fata}</b>
                                 <br> Incidents: <b>{point.total_inci}</b>"
      ) %>%
      hc_title(text = "Armed Conflict Over The Years") %>% 
      hc_subtitle(text = "2010 to 2023") %>%
      hc_xAxis(title = list(text = "2010-2023"), labels = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Frequency"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "lightcoral", width = 1, dashStyle = "Dash",
                 value = mean(year_fata$total_fata),
                 label = list(text = paste("Average Monthly Fatalities:", round(mean(year_fata$total_fata))),
                              style = list(color = 'lightcoral', fontSize = 20))))) 
    hc_plot1
    
  })
  
  ########################################
  final_explorecalendar <- reactive({
    req(input$InciFata_et2)
    req(input$YearSelect_et2)
    filter(final, year == input$YearSelect_et2)
    
  })
  
  # Render calendar --- 
  # selected years
  # years <- c(2010:2023)
  years <- 2023
  
  cal_conflict <- calendar %>%
    group_by(year, day, month) %>%
    filter(year == years) %>%
    summarise(total_fata = sum(fatalities),
              total_inci = n()) %>%
    ungroup()
  
  output$calendar_et1 <- renderPlotly({
    # tooltip
    tooltip_heat <- paste("<b>", cal_conflict$day, " ", cal_conflict$month, " ",
                          cal_conflict$year, "</b>", 
                          "\nFatalities : ", cal_conflict$total_fata,
                          "\nIncidents : ", cal_conflict$total_inci)
    
    heat <- ggplot(cal_conflict, aes(x = day, y = month, fill = total_fata)) + 
      geom_tile(color = "white", size = 1, aes(text = tooltip_heat)) + 
      theme_tufte(base_family = "Helvetica") + 
      coord_equal() +
      scale_fill_gradient(name = "Total Fatalities", low = "#fff2f4", 
                          high = "lightcoral") +
      labs(x = "Days of Month", 
           y = "", 
           title = paste("Fatalities due to Armed Conflicts in Myanmar in ", years),
           caption = "Data Source: ACLED (2023)") +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 7),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6),
            legend.position = "top") +
      scale_x_continuous(breaks = seq(min(cal_conflict$day), max(cal_conflict$day), 
                                      by = 2),
                         labels = seq(min(cal_conflict$day), max(cal_conflict$day), 
                                      by = 2))
    heat_plotly <- ggplotly(heat, tooltip = "text")
    
    heat_plotly
  })
  
  ########################################
  output$trendtext <- renderText({ 
    "This space is saved for interpretation of charts." 
  })
  
  #==========================================================  
  # Explore Distributions ---
  #==========================================================  
  final_exploretime <- reactive({
    req(input$YearSlider_ed1)
    filter(final, year %in% input$YearSlider_ed1)
    
  })
  
  box_pts <- final %>%
    filter(fatalities >0)
  
  # Render Boxplot1 --- 
  
  output$box_ed1 <- renderPlotly({
    
    box1 <- ggplot(box_pts, aes(x = forcats::fct_infreq(admin1), y = event_date, 
                                color = factor(admin1))) +
      
      geom_boxplot(width = .2, color = "#000000", fill = NA, size = .5, 
                   outlier.shape = NA, position = position_nudge(.25)) +
      geom_sina(method = "density", alpha = .3) +
      coord_flip()+
      theme_minimal() +
      theme(legend.position = "none", 
            plot.title.position = "plot") +
      labs(title = "Frequency of Conflict Has Increased Over Time in the Largest Sub-national Administrative Region in Myanmar", subtitle = "Year 2010 to Year 2023") +
      labs(y = "Year (2010-2023)",
           x = "Adminstrative Region 1", 
           caption = "Data Source: ACLED (2023)")
    
    ggplotly(box1)
  })
  
  # Render Boxplot2 --- 
  output$box_ed2 <- renderPlotly({
    # tooltip_box <- paste("<b>", final$date, "</b>", "\nFatalities : ", final$fatalities)
    
    box2 <- ggplot(box_pts, aes(x = forcats::fct_infreq(event_type), y = event_date, 
                                color = factor(event_type), fill = factor(event_type))) +
      geom_sina(method = "density", alpha = .3) +
      geom_boxplot(width = .2, color = "#000000", fill = NA, size = .5, 
                   outlier.shape = NA, position = position_nudge(.25)) +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none", 
            plot.title.position = "plot") +
      labs(title = "Battles, Explosion & Violence against Civilian Have Been Happening in Myanmar Over Time\n With More Occurrence Happening From Year 2020 Onwards", subtitle = "Year 2010 to Year 2023") +
      labs(y = "Year (2010-2023)",
           x = "Event Types", 
           caption = "Data Source: ACLED (2023)")
    
    ggplotly(box2)
  })
  
  ########################################
  output$distributiontext <- renderText({ 
    "A jittered-cum-boxplot shows the distribution of the armed conflicts incidents in Myanamr and how it has changed over the years.
    It helps to determine how spread out the incidents were across the years, 
    how it started/ ended in certain years or high concentrations in certain years. 
    "  
  })
  
  #==========================================================
  # Explore Network Relationship ---
  #==========================================================    
  final_explorenetwork <- reactive({
    req(input$EventSelect_en1)
    req(input$AdminSelect_en1)
    req(input$YearSelect_en1)
    filter(final, event_type %in% input$EventSelect_en1) %>%
      filter(final$admin1 %in% input$AdminSelect_en1) %>%
      filter(final$year == input$YearSelect_en1)
    
  })
  
  # select year
  years <- 2023  # input$YearSelect_en1
  
  # calculate frequency
  conflict_count <- final %>% 
    group_by(actor1) %>% 
    filter (year == years) %>%
    summarise(frequency = n()) %>%
    arrange(desc(frequency)) 
  
  conflict_count2 <- final %>% 
    group_by(actor2) %>% 
    filter (year == years) %>%
    summarise(frequency = n()) %>%
    arrange(desc(frequency))
  
  colnames(conflict_count) = c("actor","Freq")
  colnames(conflict_count2) = c("actor","Freq")
  
  # combine both actor 1 & 2
  final_conflict_count = rbind(conflict_count,conflict_count2)
  
  final_conflict_count2 = final_conflict_count %>%
    group_by(actor) %>%
    summarise(FrequencyConflicts = sum(Freq)) %>%
    arrange(desc(FrequencyConflicts))
  
  # trim actor/ assoc_actor
  final$actor1 = trimws(str_replace(final$actor1, "[Õ]", ""))
  final$actor2  = trimws(str_replace(final$actor2, "[Õ]", ""))
  final$assoc_actor_1 = trimws(str_replace(final$assoc_actor_1, "[Õ]", ""))
  final$assoc_actor_2 = trimws(str_replace(final$assoc_actor_2, "[Õ]", ""))
  
  assoc1 = final %>%
    filter(!is.na(actor1)) %>%
    filter(!is.na(assoc_actor_1)) %>%
    filter(!(actor1 == "")) %>%
    filter(!(assoc_actor_1 == "")) %>%
    select(actor1,assoc_actor_1)
  
  assoc2 = final %>%
    filter(!is.na(actor2)) %>%
    filter(!is.na(assoc_actor_2)) %>%
    filter(!(actor2 == "")) %>%
    filter(!(assoc_actor_2 == "")) %>%
    select(actor2,assoc_actor_2)
  
  colnames(assoc1) = c("actor","assoc_actor") 
  colnames(assoc2) = c("actor","assoc_actor") 
  
  # combine both assoc 1 & 2
  combined1 = rbind(assoc1,assoc2)
  
  combined2 = combined1 %>%
    group_by(actor,assoc_actor) %>%
    tally(sort = TRUE) 
  
  
  final_conflict_count3 = trimws(final_conflict_count2$actor)
  
  combined3 = combined2 %>%
    filter(actor %in% final_conflict_count3)
  
  
  # network graph
  viz_actors_12 <- function(actors_12) {
    set.seed(1234)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    actors_12 %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "lightcoral") +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE,
                     point.padding = unit(0.5, "lines")) +
      theme_void()
  }
  
  # Render network --- 
  output$network_en1 <- renderPlot({
    combined3 %>%
      filter(n >= 300) %>%
      viz_actors_12
  })
  
  
  ########################################
  output$networktext <- renderText({ 
    "A network graph schematically depict the nodes and connections amongst the actors and their associations involved in the armed conflicts in Myanmar.
     The line thickness indicates the strength of connections (e.g. thicker line denotes stronger connection) among actors in the network." 
  })
  
  
  
}
# =============================    
########### SERVER - END
# =============================


# =============================    
########### RUN APPLICATION
# =============================
shinyApp(ui = ui, server = server)