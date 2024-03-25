# load R packages
pacman::p_load(shiny, shinydashboard, shinycssloaders, 
               tidyverse, dplyr, leaflet, plotly, highcharter, 
               ggthemes, fresh, sf, sfdep, tmap, tm, ggforce, 
               ggraph, igraph, wordcloud, tidytext, DT, spatstat,
               lubridate,viridis, ggplot2, readr, purrr, ggstatsplot, 
               vcd, ggmosaic, forcats)


ACLED_MMR <- read_csv("data/MMR.csv")

final <- readRDS("data/final.rds")
mapping_rates <- readRDS("data/mapping_rates.rds")

Events_2 <- read_csv("data/df_complete.csv")
Space_2 <- read_csv("data/df1_complete.csv")

event_type <- unique(final$event_type)
admin1 <- unique(final$admin1)



mmr_shp_mimu_1 <- sf::st_read(dsn = "data/geospatial3", layer = "mmr_polbnda2_adm1_250k_mimu_1")
mmr_shp_mimu_2 <- sf::st_read(dsn = "data/geospatial3", layer = "mmr_polbnda_adm2_250k_mimu")




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
    menuItem("Aspatial Analysis", tabName = "Exploratory", icon = icon("globe")),
    menuItem("Geospatial Analysis", tabName = "Cluster", icon = icon("circle-nodes")),
    #menuItem("Emerging Hot Spot Analysis", tabName = "EHSA", icon = icon("magnifying-glass-chart")),
    menuItem("Confirmatory Analysis", tabName = "ConfirmatoryAnalysis", icon = icon("clipboard-check")),
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
                                choices = event_type,
                                multiple = TRUE,
                                options = list(maxItems = 6, placeholder = 'Enter event type',
                                               onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 hr(),
                 selectizeInput(inputId = "AdminSelect_eo1",
                                label = "Select Administrative Region(s)",
                                choices = admin1,
                                multiple = TRUE,
                                options = list(maxItems = 18, placeholder = 'Enter Admin Region',
                                               onInitialize = I('function() { this.setValue(""); }'))
                 ),
                 hr(),
                 sliderInput(inputId = "YearSlider_eo1", 
                             label = "Years:", 
                             min = 2010, 
                             max = 2023,
                             value = c(2010, 2023)),
                 actionButton("resetButton", "Reset Selections")
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
                          choices = c("No. of Incidents" = "Total_Incidents", "No. of Fatalities" = "Total_Fatalities"),
                          selected = "Total_Incidents"),
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
                          choices = c("Incident Rate" = "Total_Incidents", "Fatality Rate" = "Total_Fatalities"),
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
                       choices = c("No. of Incidents" = "Total_Incidents", "No. of Fatalities" = "Total_Fatalities"),
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


# =============================    
########### GEOSPATIAL ANALYSIS - START
# =============================


#==========================================================  
##Geospatial Analysis, 1st Tab
#==========================================================  

#==========================================================  
##LISA
#========================================================== 

Cluster2 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Dataset"),
             selectInput("QtrMoransI", "Year-Quarter",
                         choices = c("2021-Q1" = "2021Q1",
                                     "2021-Q2" = "2021Q2",
                                     "2021-Q3" = "2021Q3",
                                     "2021-Q4" = "2021Q4",
                                     "2022-Q1" = "2022Q1",
                                     "2022-Q2" = "2022Q2",
                                     "2022-Q3" = "2022Q3",
                                     "2022-Q4" = "2022Q4",
                                     "2023-Q1" = "2023Q1",
                                     "2023-Q2" = "2023Q2",
                                     "2023-Q3" = "2023Q3",
                                     "2023-Q4" = "2023Q4"),
                         selected = "2021-Q1"),
             selectInput("eventType3", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Riots" = "Riots"),
                         selected = "Battles")
         ),
         box(title = "Options for Local Moran's I",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             selectInput("weightstyle1", "Spatial Weights Style",
                         choices = c("W: Row standardised" = "W",
                                     "B: Binary" = "B",
                                     "C: Globally standardised" = "C",
                                     "U: C / no of neighbours" = "U",
                                     "minmax" = "minmax",
                                     "S: Variance" = "S"),
                         selected = "W"),
             selectInput("numSims1", "Number of Simulations:",
                         choices = c(99, 199, 299, 399, 499),
                         selected = 99),
             selectInput("localmoranstats", "Show Local Moran Stat:",
                         choices = c("local moran(ii)" = "local moran(ii)",
                                     "expectation(eii)" = "expectation(eii)",
                                     "variance(var_ii)" = "variance(var_ii)",
                                     "std deviation(z_ii)" = "std deviation(z_ii)",
                                     "P-value" = "p_value"),
                         selected = "local moran(ii)"),
             selectInput("LisaClass", "Show Lisa Classification",
                         choices = c("mean" = "mean",
                                     "median" = "median",
                                     "pysal" = "pysal"),
                         selected = "mean")
         )
  ),
  column(4,
         box(title = "Local Moran's I - All Districts",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             plotOutput("LocalMoranMap", height = "700px", width = "100%")
         )
  ),
  column(4,
         box(title = "Local Indicator of Spatial Association (LISA) map",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             plotOutput("Lisa", height = "700px", width = "100%")
         )
  ),
  column(2,
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("MoransItext")
         )
  )
)

# Add this outside the first fluidRow to make it full width and below everything else
Cluster2 <- tagList(Cluster2, 
                    fluidRow(
                      column(6,
                             box(title = "Local Moran's I - All Districts",
                                 status = "danger",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = NULL,
                                 align = "center",
                                 dataTableOutput("localMoransTable1"),
                                 style = "height:600px; overflow-y: scroll; overflow-x: scroll;")),
                      column(6,       
                             box(
                               title = "LISA results (P-values < 0.05)",
                               status = "danger",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               width = NULL,
                               align = "center",
                               dataTableOutput("localMoransTable2"),
                               style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                      )
                    )
                    
)
                     

#==========================================================  
##Geospatial Analysis, 2nd Tab
#==========================================================  

#==========================================================  
##Hot and Cold Spot Analysis
#========================================================== 

HotCold1 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Dataset"),
             selectInput("QtrGI" , "Year-Quarter",
                         choices = c("2021-Q1" = "2021Q1",
                                     "2021-Q2" = "2021Q2",
                                     "2021-Q3" = "2021Q3",
                                     "2021-Q4" = "2021Q4",
                                     "2022-Q1" = "2022Q1",
                                     "2022-Q2" = "2022Q2",
                                     "2022-Q3" = "2022Q3",
                                     "2022-Q4" = "2022Q4",
                                     "2023-Q1" = "2023Q1",
                                     "2023-Q2" = "2023Q2",
                                     "2023-Q3" = "2023Q3",
                                     "2023-Q4" = "2023Q4"),
                         selected = "2021-Q1"),
             selectInput("eventType6", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Riots" = "Riots"),
                         selected = "Battles"),
             selectInput(inputId = "numSims2",
                         label = "Number of Simulations for Gi*:",
                         choices = c(99,199,299,399,499),
                         selected = 99),
             selectInput("localgistats", "Show Local GI Stat:",
                         choices = c("local gi*" = "local gi*",
                                     "expectation(e_gi)" = "expectation(e_gi)",
                                     "variance(var_gi)" = "variance(var_gi)",
                                     "std deviation" = "std deviation",
                                     "P-value" = "p_value"),
                         selected = "local gi*")
         )
         
  ),
  column(4,
         box(title = "GI* Statistics- All Districts"
             ,status = "danger"
             ,solidHeader = TRUE 
             ,collapsible = TRUE
             ,width = NULL
             ,align = "left"
             ,plotOutput("Gistarmap", height = "700px", width = "100%")
         )
  ),
  column(4,
         box(
           title = "Significant Hot & Cold spot areas",
           status = "danger",
           solidHeader = TRUE,
           collapsible = TRUE,
           width = NULL,
           align = "left",
           plotOutput("HotColdmap", height = "700px", width = "100%")
         )
  ),
  column(2,
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("HotColdText")
         )
  )  
  
  
)  

HotCold1 <- tagList(HotCold1,
                    fluidRow(
                      column(6,
                             box(
                               title = "GI* Statistics - All Districts",
                               status = "danger",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               width = NULL,
                               align = "center",
                               dataTableOutput("GiStat"),
                               style = "height:500px; overflow-y: scroll;overflow-x: scroll;")),
                      column(6,       
                              box(
                                  title = "GI* Statistics - Significant Hot & Cold spots (P-values < 0.05)",
                                  status = "danger",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  width = NULL,
                                  align = "center",
                                  dataTableOutput("GiStat2"),
                                  style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                      )
                    )
                    
)

#==========================================================  
##Geospatial Analysis, 3rd Tab
#==========================================================  

#==========================================================  
##Emerging Hot Spot Analysis
#==========================================================  

#EHSA1 <- fluidRow(
#  column(2,
#         box(title = "Analysis Period: 2021-2023, Quarterly",
#             status = "info",
#             solidHeader = FALSE,
#             width = NULL,
#             helpText("Filter options for Dataset"),
#             selectInput("eventType7", "Event Type:",
#                         choices = c("Battles" = "Battles",
#                                     "Violence against civilians" = "Violence against civilians",
#                                     "Protests" = "Protests",
#                                     "Explosions/Remote violence" = "Explosions/Remote violence",
#                                     "Riots" = "Riots" ),
#                         selected = "Battles"),
#             selectizeInput(inputId = "Admin2",
#                            label = "Select District",
#                            choices = unique(Space_2$DT),
#                            multiple = FALSE)
#             
#         ),
#         box(title = "Chart Interpretation",
#             status = "danger",
#             solidHeader = TRUE,
#             collapsible = TRUE,
#             width = NULL,
#             textOutput("GITrendText")
#         )
#  ),
#  column(10,
#         box(title = "GI* Trends per district"
#             ,status = "danger"
#             ,solidHeader = TRUE 
#             ,collapsible = TRUE
#             ,width = NULL
#             ,align = "left"
#             ,plotlyOutput("Giplot", height = "600px")
#         )
#  ),       
  
#  column(12,
#         box(title = "Mann Kendall Test results"
#             ,status = "danger"
#             ,solidHeader = TRUE 
#             ,collapsible = TRUE
#             ,width = NULL
#             ,align = "left"
#             ,dataTableOutput("MKtest"),
#             style = "height:600px; overflow-y: scroll;overflow-x: scroll;")
         
#  )
#)

EHSA2 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Options for ESHA map"),
             selectInput("eventType8", "Event Type:",
                          choices = c("Battles" = "Battles",
                                    "Violence against civilians" = "Violence against civilians",
                                    "Protests" = "Protests",
                                    "Explosions/Remote violence" = "Explosions/Remote violence"),
                          selected = "Battles"),
             selectInput(inputId = "numLags", 
                         label = "Time Lag of spatial neighbours:", 
                         choices = c(1, 2, 3, 4, 5),
                         selected = 1),
             selectInput(inputId = "numSims", 
                         label = "Number of Simulations:", 
                         choices = c(99, 199, 299, 399, 499),
                         selected = 99)
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("EHSAText")
         )
  ),
  column(4,
         box(title = "Emerging Hot Spot map",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             plotOutput("EHSAmap", height = "700px")
         )
  ), 
  column(4,
         box(title = "GI* Trends per district",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             plotlyOutput("Giplot2", height = "400px")
         )
  ),
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter options for Dataset"),
             selectInput("eventType9", "Event Type:",
                        choices = c("Battles" = "Battles",
                                "Violence against civilians" = "Violence against civilians",
                                "Protests" = "Protests",
                                "Explosions/Remote violence" = "Explosions/Remote violence"),
                        selected = "Battles"),
             selectizeInput(inputId = "Admin2_2",
                            label = "Select District",
                            choices = unique(Space_2$DT),
                            multiple = FALSE)
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("GITrend2Text")
         )
  ),
  column(6,
         box(title = "Distribution of EHSA classes",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             plotlyOutput("EHSAbar", height = "200px")
         )
  ),
  column(10,
         box(title = "Emerging Hot Spot Analysis results",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             dataTableOutput("MKtest2")
         )
  ),
  column(2,
         box(title = "Table Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("MKText")
         )
  )
)



#==========================================================  
##Geospatial Analysis - END
#==========================================================  


#==========================================================  
#Confirmation Analysis tab --- START
#==========================================================  


Confirm1 <- fluidRow(
  column(3,
         box(title = "Analysis Period: 2020-2023",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Options for Anova Test"),
             selectInput(inputId = "YearAnova",
                         label = "Year:",
                         choices = seq(2020,2023),
                         selected = 2021),
             #hr(),
             #selectizeInput(inputId = "Admin1_ggstat",
            #                label = "Select Administrative Region(s)",
            #                choices = unique(ACLED_MMR$admin1),
            #                multiple = TRUE,
            #                selected = c("Mon", "Yangon"), 
            #                options = list(maxItems = 18, placeholder = 'Enter Region/State')
            # ),
             
             hr(),
             selectizeInput(inputId = "event_ggstat",
                            label = "Select event type",
                            choices = unique(ACLED_MMR$event_type),
                            multiple = TRUE,
                            selected = c("Battles", "Violence against civilians"), 
                            options = list(maxItems = 6, placeholder = 'Enter Event Type')
             ),
             actionButton(inputId = "resetButton1", label = "Reset Selections"),
             hr(),
             selectInput(inputId = "Testtype",
                         label = "Test Type:",
                         choices = c("parametric" = "p",
                                     "non-parametric" = "np",
                                     "robust" = "r",
                                     "bayes" = "b"),
                         selected = "parametric"),
             hr(),
             selectInput(inputId = "Pairtype",
                         label = "Pairwise Display:",
                         choices = c("significant" = "s",
                                     "non-significant" = "ns",
                                     "all" = "all"),
                         selected = "significant"),
             hr(),
             selectInput(inputId = "Padjust",
                         label = "P-value adjustment method:",
                         choices = c("holm" = "holm",
                                     "hochberg" = "hochberg",
                                     "hommel" = "hommel",
                                     "bonferroni" = "bonferroni",
                                     "BH" = "BH",
                                     "BY" = "BY",
                                     "fdr" = "fdr",
                                     "none" = "none"),
                         selected = "holm"),
            # hr(),
            #radioButtons(inputId = "PlotType",
             #             label = "Plot Type",
              #           choices = c("box" = "box", 
               #                      "violin" = "violin",
                #                     "boxviolin" = "boxviolin"),
                 #        selected = "box"),
            hr(),
            radioButtons(inputId = "Conlevel",
                         label = "Confidence level",
                         choices = c("0.95" = 0.95, 
                                     "0.99" = 0.99),
                         selected = 0.95),
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("AnovaText")
         )
  ),
  column(9,
         box(title = "One-way Anova Test for Fatalities per event type",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             plotOutput("Anovaplot", height = "700px")
         )
  )
)


Confirm2 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2020-2023",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter Options for Dataset"),
             selectInput(inputId = "YearMosaic",
                         label = "Year:",
                         choices = seq(2020,2023),
                         selected = 2023)
             
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("MosaicText")
         )
  ),
  column(10,
         box(title = "Mosaic Plot for event type per Region/State",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             plotlyOutput("Mosaicplot", height = "1400px")
         )
  )
)


Confirm3 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2020-2023",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter Options for Dataset"),
             selectInput(inputId = "YearMosaic2",
                         label = "Year:",
                         choices = seq(2020,2023),
                         selected = 2023)
             
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("Mosaic2Text")
         )
  ),
  column(10,
         box(title = "Mosaic Plot for event type per Region/State",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             plotOutput("Mosaicplot2", height = "1400px")
         )
  )
)



#==========================================================  
#Confirmation Analysis tab --- End
#==========================================================  




#define the no of sub tabs needed

ClusterSubTabs <- tabsetPanel(
  
  tabPanel("Local Measures of Spatial Autocorrelation", 
           Cluster2),
  tabPanel("Hot & Cold Spot Analysis(HCSA)", 
           HotCold1),
  tabPanel("Emerging Hot Spot Analysis", 
           EHSA2)
)

#ESHASubTabs <- tabsetPanel(
  #tabPanel("Gi* trend and Mann Kendall test", 
           #EHSA1),
 # tabPanel("Emerging Hot Spot Analysis", 
  #         EHSA2)
  
#)


ConfirmSubTabs <- tabsetPanel(
  tabPanel("One-Way Anova Test", 
           Confirm1),
  tabPanel("Mosaic Plot",
           Confirm2),
  tabPanel("Mosaic Plot-VCD",
           Confirm3)
  #Confirm2)
  
)





body <- dashboardBody(
  
  # use theme
  use_theme(mytheme),
  
  
  # =============================
  # tabItems - All Pages
  # =============================
  
  tabItems(
    # 1st tab content
    tabItem(tabName = "Exploratory",
            ExploreSubTabs
    ),
    # 2nd tab content
    tabItem(tabName = "Cluster",
            
            ClusterSubTabs # add the sub tabs which was defined above
    ),
    #3rd tab content
    #tabItem(tabName = "EHSA",
            
      #      ESHASubTabs
   # ),
    #4th tab content
    tabItem(tabName = "ConfirmatoryAnalysis",
            
            ConfirmSubTabs)
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


# create the server functions for the dashboard  
server <- function(input, output, session) { 
  
  # =============================    
  #DATA Wrangling
  # =============================  
  
  
  ##Data subset for Choropleth Maps (Admin 2)
  #====================================================
  Data2 <- ACLED_MMR %>%
    group_by(year, admin2, event_type, sub_event_type) %>%
    summarise(Incidents = n(),
              Fatalities = sum(fatalities, na.rm = TRUE)) %>%
    
    ungroup()
  
  ##Spacial join between shape file and attribute file (admin 2)
  #=======================================================
  ACLED_MMR_admin2 <- left_join(mmr_shp_mimu_2, Data2,
                                by = c("DT" = "admin2"))
  
  ACLED_MMR_admin2 <- ACLED_MMR_admin2 %>%
    select(-OBJECTID, -ST, -ST_PCODE, -DT_PCODE, -DT_MMR, -PCode_V)
  
  
  #Data subset for Local Moran's & Gi* statistics
  #====================================================
  
  Events_admin2 <- left_join(mmr_shp_mimu_2, Events_2,
                             by = c("DT" = "admin2"))
  
  Events_admin2 <- Events_admin2 %>%
    select(-OBJECTID, -ST, -ST_PCODE, 
           -DT_PCODE, -DT_MMR, -PCode_V) %>%
    rename("District" = "DT")
  
  #Data subset for Confirmatory analysis
  #====================================================
  
  Summary_Data <- ACLED_MMR %>%
    group_by(year, admin1, event_type) %>%
    summarise(Total_incidents = n(),
              Total_Fatalities = sum(fatalities, na.rm=TRUE)) %>%
    
    ungroup()            
  
  Region_Summary <- ACLED_MMR %>%
    group_by(year, country, admin1,event_type, disorder_type) %>%
    summarize(
      Total_incidents = n(),
      Total_Fatalities = sum(fatalities, na.rm=TRUE)
    )
  
  
  # =============================    
  # START of EXPLORATORY Module
  # =============================
  
  #==========================================================
  # Explore Point Map ---
  #==========================================================    
  final_explorespatial <- reactive({
    
    req(input$YearSlider_eo1, input$EventSelect_eo1, input$AdminSelect_eo1)
    # Filter 'final' dataset based on user inputs
    filtered_data <- final %>%
      filter(event_type %in% input$EventSelect_eo1,
             admin1 %in% input$AdminSelect_eo1,
             year %in% range(input$YearSlider_eo1))
    return(filtered_data)
  })
  
  # Render Point Map --- 
  output$emap_eo1 <- renderLeaflet({
    # Use the filtered dataset
    incident_pts <- final_explorespatial() %>%
      filter(fatalities > 0)  
    
    cof <- colorFactor(c("#ff7e8a", "#394938", "#ffa500", 
                         "#0092ff", "#741b47", "#60dcb5"), 
                       domain=c("Battles", "Explosions/ Remote violence", 
                                "Protests", "Riots",
                                "Strategic developments", 
                                "Violence against civilians"))
    
    leaflet() %>% 
      addTiles('https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
      setView(100, 20, zoom = 5) %>%
      addCircles(data=incident_pts, lat= ~latitude, lng = ~longitude, 
                 color = ~cof(event_type), 
                 fillColor = ~cof(event_type),  # Use fillColor for consistency or adjust as necessary
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
  
  observeEvent(input$resetButton, {
    updateSelectizeInput(session, "EventSelect_eo1", selected = character(0))
    updateSelectizeInput(session, "AdminSelect_eo1", selected = character(0))
    updateSliderInput(session, "YearSlider_eo1", value = c(2010, 2023))
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
  
  boxbreaks <- function(v, mult=1.5) {
    qv <- unname(quantile(v, na.rm = TRUE))
    iqr <- qv[4] - qv[2]
    upfence <- qv[4] + mult * iqr
    lofence <- qv[2] - mult * iqr
    bb <- vector(mode="numeric", length=7)
    if (lofence < qv[1]) { 
      bb[1] <- lofence
      bb[2] <- floor(qv[1])
    } else {
      bb[2] <- lofence
      bb[1] <- qv[1]
    }
    if (upfence > qv[5]) { 
      bb[7] <- upfence
      bb[6] <- ceiling(qv[5])
    } else {
      bb[6] <- upfence
      bb[7] <- qv[5]
    }
    bb[3:5] <- qv[2:4]
    return(bb)
  }
  
  # Function to fetch variable data
  get.var <- function(vname, df) {
    v <- df[[vname]]
    return(v)
  } 
  
  # Function to create a box map
  boxmap <- function(vnam, df, legtitle=NA, mtitle="Box Map", mult=1.5) {
    var <- get.var(vnam, df)
    bb <- boxbreaks(var, mult)
    tm <- tm_shape(df) +
      tm_polygons(col = vnam, title = legtitle, breaks = bb, palette = "Reds", 
                  labels = c("Lower Outlier", "< 25%", "25% - 50%", "50% - 75%", "> 75%", "Upper Outlier")) +
      tm_layout(main.title = mtitle, main.title.position = "center", legend.outside = TRUE)
    return(tm)
  }
  
  # Render the map based on user input
  output$emap_eg1 <- renderTmap({
    selected_variable <- input$InciFata_eg1
    map_title <- if(selected_variable == "Total_Incidents") {"No. of Incidents"} else {"No. of Fatalities"}
    
    boxmap(selected_variable, mapping_rates, legtitle = map_title, mtitle = paste0("Box Map of ", map_title))
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
    
    
    filtered_emap_eg2 <- mapping_rates %>%
      filter(year == input$YearSelect_eg2) 
    
    # Check if the filtered data is empty to avoid errors
    if(nrow(filtered_emap_eg2) == 0) {
      return(NULL)  # Return NULL if there's no data to avoid plotting errors
    }
    
    # Determine the column to use for filling based on user selection
    fill_column <- ifelse(input$InciFata_eg2 == "Total_Fatalities", "Total_Fatalities", "Total_Incidents")
    
    
    return(list(filtered_data = filtered_emap_eg2, fill_column = fill_column))
  })
  
  
  # Render Spatial Distribution Map --- 
  output$emap_eg2 <- renderTmap({
    mapping_data <- mapping_rates_exploredist()
    if(is.null(mapping_data$filtered_data)) {
      return(NULL)  # Return NULL if there's no data to avoid plotting errors
    }
    tmap_obj2 <- tm_shape(mapping_data$filtered_data) +
      tm_fill(mapping_data$fill_column, 
              n = input$ClassSelect_eg2,
              style = input$ClassificationSelect_eg2,
              palette = "Reds", 
              legend.hist = TRUE, 
              legend.is.portrait = TRUE, 
              legend.hist.z = 0.1) +
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_layout(main.title = paste("Distribution of Armed Conflict Incidents in Myanmar", input$ClassificationSelect_eg2, "classification"),
                title = "",
                main.title.size = 1,
                legend.height = 0.60,
                legend.width = 5.0,
                legend.outside = FALSE,
                legend.position = c("left", "bottom"))
    return(tmap_obj2)
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
      summarise(Total_Fatalities = sum(fatalities),
                Total_Incidents = n()) %>%
      ungroup()
    
    hc_plot1 <-  highchart() %>% 
      hc_add_series(year_fata, hcaes(x = year_month, y = Total_Fatalities), type = "line", 
                    name = "Total Fatalities", color = "lightcoral") %>%
      hc_add_series(year_fata, hcaes(x = year_month, y = Total_Incidents), type = "line", 
                    name = "Total Incidents", color = "black") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", 
                 backgroundColor = "#FCFFC5",
                 borderWidth = 5,
                 pointFormat = "<b>20{point.year_month}</b> 
                                 <br> Fatalities: <b>{point.Total_Fatalities}</b>
                                 <br> Incidents: <b>{point.Total_Incidents}</b>"
      ) %>%
      hc_title(text = "Armed Conflict Over The Years") %>% 
      hc_subtitle(text = "2010 to 2023") %>%
      hc_xAxis(title = list(text = "2010-2023"), labels = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Frequency"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "lightcoral", width = 1, dashStyle = "Dash",
                 value = mean(year_fata$Total_Fatalities),
                 label = list(text = paste("Average Monthly Fatalities:", round(mean(year_fata$Total_Fatalities))),
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
    summarise(Total_Fatalities = sum(fatalities),
              Total_Incidents = n()) %>%
    ungroup()
  
  output$calendar_et1 <- renderPlotly({
    # tooltip
    tooltip_heat <- paste("<b>", cal_conflict$day, " ", cal_conflict$month, " ",
                          cal_conflict$year, "</b>", 
                          "\nFatalities : ", cal_conflict$Total_Fatalities,
                          "\nIncidents : ", cal_conflict$Total_Incidents)
    
    heat <- ggplot(cal_conflict, aes(x = day, y = month, fill = Total_Fatalities)) + 
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
  
  #==========================================================
  # END of Exploratory Module
  #==========================================================
  
  #==========================================================
  # START Geospatial Analysis Module
  #==========================================================   
  
  
  #==========================================================
  # Local Measures of Spatial Correlation
  #==========================================================   
  
  localMIResults <- reactive({
    # Filter the data based on the user's selection
    filteredData <- Events_admin2 %>%
      filter(quarter == input$QtrMoransI, event_type == input$eventType3) 
    
    
    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data
    
    # Computing Contiguity Spatial Weights
    wm_q <- filteredData %>%
      mutate(nb = st_contiguity(geometry),
             wt = st_weights(nb,
                             style = input$weightstyle1))
    
    
    
    # Computing Local Moran's I
    lisa <- wm_q %>%
      mutate(local_moran = local_moran(
        Incidents, nb, wt, nsim = as.numeric(input$numSims1)),
        .before = 5) %>%
      unnest(local_moran)
    
    lisa <- lisa %>%
      rename("local moran(ii)" = "ii", "expectation(eii)" = "eii",
             "variance(var_ii)" = "var_ii", "std deviation(z_ii)" = "z_ii",
             "p_value" = "p_ii")
    
    return(lisa)       
    
    
  })
  
  
  # Render the map of Local Moran's I values
  output$LocalMoranMap <- renderPlot({
    df <- localMIResults()
    
    if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
    
    # Map creation using tmap
    localMI_map <- tm_shape(df) +
      tm_fill(col = input$localmoranstats, style = "pretty", palette = "RdBu", title = input$localmoranstats) +
      tm_borders() 
    
    localMI_map 
  })
  
  
  
  #==========================================================
  # LISA Map in Cluster 2 
  #==========================================================  
  
  output$Lisa <- renderPlot({
    df <- localMIResults()
    if(is.null(df)) return()
    
    lisa_sig <- df  %>%
      filter(p_value < 0.05)
    
    
    lisamap <- tm_shape(df) +
      tm_polygons() +
      tm_borders() +
      
      tm_shape(lisa_sig) +
      tm_fill(col = input$LisaClass,  
              palette = "-RdBu",  
              title = (paste("Significance:", input$LisaClass))) +
      tm_borders(alpha = 0.4)
    
    
    lisamap #+ 
      #tm_view(set.zoom.limits = c(5,7))
    
    
  })
  
  
  
  #==========================================================
  # Local Morans's I Data Table in Cluster 2 
  #==========================================================  
  
  # Render the data table for Local Moran's I results
  output$localMoransTable1 <- renderDataTable({
    df <- localMIResults()
    
    # Check if data is available
    if (is.null(df)) return()
    
    df
      

  })
  
  output$localMoransTable2 <- renderDataTable({
    df2 <- localMIResults()
    
    # Check if data is available
    if (is.null(df2)) return()
    
    lisa_sig2 <- df2  %>%
      filter(p_value < 0.05)
      
    lisa_sig2
    
  })
  
  output$MoransItext <- renderText({ 
    "Local Moran's I, is a spatial statistic that detects and quantifies 
    spatial clustering or dispersion within a given geographic area.
    It assesses spatial patterns at a local level, determining if features form 
    significant clusters (high-high or low-low) or outliers (high-low or low-high) 
    compared to neighboring features.
    
    In our analysis, we're examining if certain areas (admin region 2) exhibit higher or 
    lower incident rates of a specific event type than expected by chance alone, 
    indicating deviations from a random spatial distribution." 
  })
  
  
  #==========================================================
  # Moran Scatter plot in Cluster 3 - KIV
  #==========================================================  
  
  
  #output$MoranScatter <- renderPlot({
  # Retrieve filtered data based on input selections for event type and year
  #filteredData1 <- Events_admin2 %>%
  #filter(year == input$YearMoranScat, event_type == input$eventType4)
  
  # Exit if no data is available for the selected criteria
  #if(nrow(filteredData1) == 0) {
  #return(NULL)
  #}
  
  # Standardize the Incidents variable
  #standardizedIncidents <- scale(filteredData1$Incidents) %>% 
  #as.vector 
  
  # Computing Contiguity Spatial Weights
  #wm_q <- poly2nb(filteredData1, queen = TRUE)
  #rswm_q <- nb2listw(wm_q, style = "W", zero.policy = TRUE)
  
  # Compute Moran's I values
  #moranValues <- localmoran(standardizedIncidents, rswm_q, na.action = na.exclude)
  
  #plotTitle <- paste("Moran Scatterplot for", input$eventType4, "in", input$YearMoranScat)
  
  # Create the Moran scatterplot
  #nci <- moran.plot(standardizedIncidents, rswm_q,
  #labels = as.character(filteredData1$DT),
  #xlab = "Standardized Incidents",
  #ylab = "Spatially Lagged Incidents",
  #main = plotTitle)
  
  
  #})
  
  #output$MoranScatText <- renderText({ 
  "The Moran scatterplot is divided into four areas, with each quadrant corresponding 
    with one of four categories: (1) High-High (HH) in the top-right quadrant; (2) High-Low (HL) 
    in the bottom right quadrant; (3) Low-High (LH) in the top-left quadrant; 
    (4) Low- Low (LL) in the bottom left quadrant. The top right corner belongs to areas that have high incidents of events and are surrounded by other areas 
    that have higher than the average level/number of battles This is the high-high locations." 
  #})
  
  
  
  #==========================================================
  # Hot & Cold Spot Analysis - GI* statistics
  #==========================================================
  
  
  GiData <- reactive({
    filtered_data2 <- Events_admin2 %>%
      filter(quarter == input$QtrGI, event_type == input$eventType6)
    
    
    #Derive a spatial weight matrix by using sfdep functions and tidyverse approach.
    wm_idw <- filtered_data2 %>%
      mutate(nb = st_contiguity(geometry),
             wts = st_inverse_distance(nb, geometry,
                                       scale = 1,
                                       alpha = 1))
    
    #compute the local Gi* by using the code chunk below
    
    HCSA <- wm_idw %>% 
      mutate(local_Gi = local_gstar_perm(
        Incidents, nb, wt, nsim = as.numeric(input$numSims2)),
        .before = 5) %>%
      unnest(local_Gi)
    
    HCSA <- HCSA %>%
      rename("local gi*" = "gi_star", "expectation(e_gi)" = "e_gi",
             "variance(var_gi)" = "var_gi", "std deviation" = "std_dev")
    
    return(HCSA)
    
  })
  
  output$Gistarmap <- renderPlot({
    df <- GiData()
    
    # Exit if there's no data to plot
    if(is.null(df) || nrow(df) == 0) return() #Exit if no data
    
    
    # Create the choropleth map for GI stats
    Gi_map <- tm_shape(df) +
      tm_fill(col = input$localgistats, 
              palette = "-RdBu", 
              title = input$localgistats) +
      tm_borders()
    
    Gi_map #+ 
      #tm_view(set.zoom.limits = c(5,7))
  })
  
  
  output$HotColdmap <-  renderPlot({
    df <- GiData()
    
    if(is.null(df) || nrow(df) == 0) return() #Exit if no data
    
    
    HCSA_sig <- df  %>%
      filter(p_value < 0.05)
    
    # Create the choropleth map for HSCA Map
    HSCAmap <- tm_shape(df) +
      tm_polygons() +
      tm_borders() +
      
      tm_shape(HCSA_sig) +
      tm_fill(col = "local gi*",  
              palette = "-RdBu",  
              title = "local gi*") +
      tm_borders(alpha = 0.4)
    
    HSCAmap 
  })
  
  
  
  
  
  output$GiStat <- renderDataTable({
    data_with_gi <- GiData()  
    if(is.null(data_with_gi)) return ()
    
    data_with_gi
  })
  
  output$GiStat2 <- renderDataTable({
    data_with_gi2 <- GiData()  
    if(is.null(data_with_gi2)) return ()
      
    HCSA_sig2 <- data_with_gi2  %>%
      filter(p_value < 0.05)  
      
    HCSA_sig2
  })
  
  
  output$HotColdText <- renderText({ 
    "HCSA uses spatial weights to identify locations of statistically significant 
    hot spots and cold spots in an spatially weighted attribute that are in proximity 
    to one another based on a calculated distance. 
    The analysis groups features when similar high (hot) or low (cold) values are found in a cluster.
    
    Here we are utilizing the Getis and Ord’s G statistics. 
    High positive G values indicate hot spots—areas where high values cluster together
    while low negative G values indicate cold spots—areas where low values cluster together." 
  })
  
  
  #==========================================================
  # END of Cluster & Outlier Analysis Module
  #==========================================================
  
  
  #==========================================================
  # START of Emerging Hot spot Analysis Module
  #==========================================================

  
  #==========================================================
  # EMERGING HOT SPOT ANALYSIS
  #==========================================================
  
  #==========================================================
  # Distribution of EHSA classes and EHSA Map
  #==========================================================
  
  EHSAData <- reactive({
    space_data <- Space_2 %>%
      filter(event_type == input$eventType8)
    
    
    Filtered_space <- space_data %>%
      select(-event_type, -year, -Fatalities)
    
    Quarterly_spt <- spacetime(Filtered_space, mmr_shp_mimu_2,
                               .loc_col = "DT",
                               .time_col = "quarter")
    
    ehsa3 <- emerging_hotspot_analysis(
      x = Quarterly_spt, 
      .var = "Incidents", 
      k = as.numeric(input$numLags),
      include_gi = TRUE, 
      nsim = as.numeric(input$numSims)
      
    )
    
    return(ehsa3)
    
  }) 
  
  output$EHSAbar <- renderPlotly({
    df <- EHSAData()
    
    df <- df %>%
      filter(p_value < 0.05) %>%
      group_by(classification) %>%
      summarise(count = n()) %>%
      ungroup() 
    
    # Reorder classification based on count in ascending order
    df <- df %>%
      mutate(classification = fct_reorder(classification, count))
    
    EHSAbar <- ggplot(data = df, aes(x = classification, y = count)) +
      geom_bar(stat = "identity") + 
      coord_flip() +
      theme_minimal() +
      theme(axis.title.y = element_blank())
    
    ggplotly(EHSAbar)
  })
  
  EHSAMapdata <- reactive({
    df <- EHSAData() 
    
    mmr3_ehsa <- mmr_shp_mimu_2 %>%
      left_join(df,
                by = join_by(DT == location))
    
    mmr3_ehsa <- mmr3_ehsa %>%
      select(-OBJECTID, -ST, -ST_PCODE)
    
    return(mmr3_ehsa)
  })
  
  output$EHSAmap <- renderPlot({
    df <- EHSAMapdata()
    if(is.null(df)) return()
    
    ehsa_sig3 <- df  %>%
      filter(p_value < 0.05)
    
    
    ehsamap <- tm_shape(df) +
      tm_borders() +
      
      tm_shape(ehsa_sig3) +
      tm_fill("classification") +
      tm_borders(alpha = 0.4)
    
    
    ehsamap 
    
    
  })
  
  output$MKtest2 <- renderDataTable({
    # Get the Mann-Kendall test results
    EHSATable <- EHSAMapdata()
    if(is.null(df)) return()
    
    
    ehsa_sig3 <- EHSATable  %>%
      filter(p_value < 0.05) %>%
      select(-DT_PCODE, -DT_MMR, -PCode_V) %>%
      rename("District" = "DT")
    
    
    # Return the results to render them as a table
  ehsa_sig3
  })
  
  
  output$EHSAText <- renderText({ 
    "Emerging Hot Spot Analysis identifies trends in spatial clustering 
      over a period of time. It combines the Getis-Ord Gi* statistic 
      with the Mann-Kendall trend test to determine if there 
    is a temporal trend associated with local clustering of hot and cold spots.
    
    This map shows results for P-values < 0.05."
  })
  
  
  
  
  
EHSAData2 <- reactive({
  space_data2 <- Space_2 %>%
    filter(event_type == input$eventType9)
  
  
  Filtered_space2 <- space_data2 %>%
    select(-event_type, -year, -Fatalities)
  
  Quarterly_spt2 <- spacetime(Filtered_space2, mmr_shp_mimu_2,
                             .loc_col = "DT",
                             .time_col = "quarter")
  
  Quarterly_nb2 <- Quarterly_spt2 %>%
    activate("geometry") %>%
    mutate(nb = include_self(st_contiguity(geometry)),
           wt = st_inverse_distance(nb, geometry,
                                    scale = 1,
                                    alpha = 1),
           .before = 1) %>%
    set_nbs("nb") %>%
    set_wts("wt")
  
  gi_stars <- Quarterly_nb2 %>% 
    group_by(quarter) %>% 
    mutate(gi_star = local_gstar_perm(
      Incidents, nb, wt)) %>% 
    tidyr::unnest(gi_star)
  
  return(gi_stars)
  
}) 


output$Giplot2 <- renderPlotly({
  
  df2 <- EHSAData2()
  
  # Exit if there's no data to plot
  if(is.null(df2) || nrow(df2) == 0) return() #Exit if no data
  
  
  filtered_df2 <- df2 %>%
    filter(DT == input$Admin2_2) %>%
    select(DT, quarter, gi_star)
  
  p2 <- ggplot(data = filtered_df2, 
               aes(x = quarter, 
                   y = gi_star)) +
    geom_line() +
    theme_light() +
    theme(axis.text.x = element_blank()) +
    ggtitle(paste("GI* Trends for District:", input$Admin2_2))
  
  ggplotly(p2)
  
  
})
  

output$GITrend2Text <- renderText({ 
  "GI* trend plot shows changes in the Local Gi* per district, for each event type from Q1 2021 to Q4 2023"

})  

#EHSADataMKTest2 <- reactive({
#  df2 <- EHSAData2() 
  
#  ehsa32 <- df2 %>%
#    group_by(DT) %>%
#    summarise(mk = list(
#      unclass(
#        Kendall::MannKendall(gi_star)))) %>%
#    tidyr::unnest_wider(mk)
  
#  return(ehsa32)
#})


#output$MKtest2 <- renderDataTable({
  # Get the Mann-Kendall test results
#  mkResults2 <- EHSADataMKTest2()
  
  # Return the results to render them as a table
#  mkResults2
#})

output$MKText <- renderText({ 
  "The Mann-Kendall test determines whether there is a 
    monotonic trend over time in the observed data.
  
  First the Gi* statistic for each location in each time period (time-slice) is calculated. 
  Next, the Mann-Kendall trend test is done to identify any temporal trend in Gi* values 
  over all time periods. Each location is then classified into one of 17 categories based on 
  ESRI's emerging hot spot classification criteria. This tables shows results for P-values < 0.05.
  
  Tau ranges between -1 and 1 where -1 is a perfectly decreasing series and 1 is a perfectly increasing series."
  
})  
  
  
  #==========================================================
  # END of Emerging Hot spot Analysis Module
  #==========================================================
  
  
  
  #==========================================================
  # START of Confirmatory Analysis Module
  #==========================================================
  
  # Anova test
  
  observeEvent(input$resetButton1, {
    #updateSelectizeInput(session, "Admin1_ggstat", selected = character(0))  
    updateSelectizeInput(session, "event_ggstat", selected = character(0))  
  })
  
  AnovaResults <- reactive({
    # Filter the data based on the user's selection
    filteredData <- Summary_Data %>%
      filter(year == input$YearAnova,
             #admin1 == input$Admin1_ggstat,
             event_type == input$event_ggstat) 
    
    
    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data    
    
    return(filteredData)  
    
  }) 

  
  output$Anovaplot <- renderPlot({
    
    dataForAnova <- AnovaResults()  
    
    if(is.null(dataForAnova)) return()  # Check if the data is NULL and exit if it is
    
    Anova <- ggbetweenstats(data = dataForAnova,
                            x = event_type, 
                            y = Total_Fatalities,
                            #plot.type = input$PlotType,
                            conf.level = input$Conlevel,
                            type = input$Testtype,
                            mean.ci = TRUE, 
                            pairwise.comparisons = TRUE, 
                            pairwise.display = input$Pairtype,
                            p.adjust.method = input$Padjust,
                            messages = TRUE,
                            title = paste("Fatalilites in",input$YearAnova)
    )
    
    Anova
    
  })
  
  
  #Mosaic Plot
  
  MosaicResults <- reactive({
    # Filter the data based on the user's selection
    filteredData <- Region_Summary %>%
      filter(year == input$YearMosaic) 
    
    
    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data    
    
    return(filteredData)  
    
  })        
  

  output$Mosaicplot <- renderPlotly({
    
    dataForMosaic <- MosaicResults()  
    
    if(is.null(dataForMosaic)) return()  # Check if the data is NULL and exit if it is
    
    gg5 <- ggplot(dataForMosaic) +
      geom_mosaic(aes(weight = Total_Fatalities,
                      x = product(event_type, country), fill = admin1)) +
      labs(x = "Myanmar",
           fill = "Regions") +
      theme(
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    # Converting the ggplot object to a plotly object
    ggplotly(gg5)
  })
  
  
  # VCD mosaic
  MosaicResults2 <- reactive({
    # Filter the data based on the user's selection
    filteredData <- ACLED_MMR %>%
      filter(year == input$YearMosaic2) 
    
    
    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data    
    
    return(filteredData)  
    
  })        
  
  
  
  output$Mosaicplot2 <- renderPlot({
    
      dataForMosaic2 <- MosaicResults2()  
    
      if(is.null(dataForMosaic2)) return()  # Check if the data is NULL and exit if it is
    
     Mosaic2 <- vcd::mosaic(~ admin1 + event_type,  data = dataForMosaic2, gp = shading_max, 
                            labeling = labeling_border(rot_labels = c(90,0,0,0), 
                                                       just_labels = c("left", 
                                                                       "center", 
                                                                       "center", 
                                                                       "right")))
    
      Mosaic2
    
     })
  
  
  
  
  #==========================================================
  # END of Confirmatory Analysis Module
  #==========================================================
  
  
}
# Run the app
shinyApp(ui = ui, server = server)
