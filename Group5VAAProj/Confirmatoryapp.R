# load R packages
pacman::p_load(shiny, shinydashboard, shinycssloaders, 
               tidyverse, dplyr, leaflet, leaflet.extras, plotly, 
               ggthemes, fresh, sf, sfdep, tmap, tm, 
               ggraph, DT, spatstat,
               lubridate,viridis, ggplot2, readr, purrr, ggstatsplot, 
               vcd, ggmosaic, forcats,
               ggridges, ggdist, highcharter)


#Import aspatial data 
#====================================================
final <- readRDS("data/final.rds")
joined <- readRDS("data/joined_data.rds")
mapping_rates <- readRDS("data/mapping_rates.rds")

joined2 <- readRDS("data/joined_data2.rds")
mapping_rates2 <- readRDS("data/mapping_rates2.rds")

ACLED_MMR <- read_csv("data/MMR.csv")

Events_2 <- read_csv("data/df_complete.csv")
Space_2 <- read_csv("data/df1_complete.csv")

event_type <- unique(final$event_type)
admin1 <- unique(final$admin1)

#Import geospatial data 
#====================================================
maplayer1 <- sf::st_read(dsn = "data/geospatial", layer = "geoBoundaries-MMR-ADM1")
maplayer2 <- sf::st_read(dsn = "data/geospatial", layer = "geoBoundaries-MMR-ADM2")

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
    menuItem("Aspatial Analysis",icon = icon("globe"),
             href = "https://group5vaa.shinyapps.io/Decoding-Chaos-Aspatial"),
    menuItem("Geospatial Analysis",icon = icon("circle-nodes"),
             href = "https://group5vaa.shinyapps.io/Decoding-Chaos-Geospatial"),
    menuItem("Confirmatory Analysis", tabName = "ConfirmatoryAnalysis", icon = icon("clipboard-check")),
    menuItem("ACLED Data Table", tabName = "ACLEDTable", icon = icon("table")),
    menuItem("ACLED Data Source", icon = icon("send",lib='glyphicon'), 
             href = "https://acleddata.com/data-export-tool/"),
    menuItem("User Guide", icon = icon("file"), 
             href = "https://decoding-chaos.netlify.app/RShiny/UserGuide.pdf"),
    menuItem("Home Page", icon = icon("house"), 
             href = "https://decoding-chaos.netlify.app/")))


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
########### ASPATIAL - START
# =============================


#==========================================================  
# AspatialOverviewrow1 ---
#==========================================================  

## commented out for aspatial
# AspatialOverviewrow1 <-  fluidRow(
#   box(title = "Armed Conflict Incidents in Myanmar (2010 to 2023)",
#       status = "danger",
#       solidHeader = TRUE,
#       width = 12,
#       column(4,
#              box(title = "Map Panel",
#                  status = "info",
#                  solidHeader = FALSE,
#                  width = NULL,
#                  sliderInput(inputId = "YearSlider_eo1", 
#                              label = "Years:", 
#                              min = 2010, 
#                              max = 2023,
#                              value = c(2010, 2023),
#                              step = 1,
#                              sep = ""),
#                  selectizeInput(inputId = "EventSelect_eo1",
#                                 label = "Select Event(s)",
#                                 choices = sort(unique(as.character(final$event_type))),
#                                 # selected = c("Battles", "Violence against civilians"),
#                                 options = list(placeholder = 'Enter Event (max: 3)'),
#                                 multiple = TRUE),
#                  
#                  radioButtons(inputId = "Admin_eo1",
#                               label = "Administrative Region",
#                               choices = c("Level 1", "Level 2"),
#                               selected = "Level 1",
#                               inline = TRUE),
#                  conditionalPanel(
#                    condition = "input.Admin_eo1 == 'Level 1'", 
#                    selectizeInput(inputId = "AdminSelect_eo1",
#                                   label = "Select Administrative Region Level 1",
#                                   choices = sort(unique(as.character(final$admin1))),
#                                   multiple = TRUE,
#                                   options = list(placeholder = 'Enter Region 1 (max: 5)'))
#                  ),
#                  conditionalPanel(
#                    condition = "input.Admin_eo1 == 'Level 2'", 
#                    selectizeInput(inputId = "AdminSelect_eo2",
#                                   label = "Select Administrative Region Level 2",
#                                   choices = sort(unique(as.character(final$admin2))),
#                                   multiple = TRUE,
#                                   options = list(maxItems = 5, placeholder = 'Enter Region 2 (max: 5)'))
#                  ),
#                  actionButton("emapButton", "Update"),
#                  # actionButton("emapresetButton", "Reset"),
#                  hr(),
#                  checkboxInput(inputId="ShowLine", label="Show Trend Line", value=FALSE),
#                  conditionalPanel(condition="input.ShowLine",
#                                   withSpinner(highchartOutput("line_eo1", height = "250px"))
#                  ),
#                  checkboxInput(inputId="ShowDataTable", label="Show Data Table", value=FALSE),
#                  hr(),
#                  checkboxInput(inputId="MoreInfo", label="About Map", value=FALSE),
#                  conditionalPanel(condition="input.MoreInfo",
#                                   textOutput("abouttext"))
#              )
#       ),
#       column(8,
#              box(width = 12,  # Automatically adjust to full width
#                  align = "left",
#                  withSpinner(leafletOutput("emap_eo1", height = "650px", width = "100%"))
#              )
#       )
#   )
#   #),
#   # absolutePanel(id = "controls", class = "panel panel-default",
#   #              bottom = -40, left = 300, width = 250, fixed=TRUE,
#   #              draggable = TRUE, height = "auto",
#   #              box(title = "About",
#   #                  status = "danger",
#   #                  solidHeader = TRUE,
#   #                  collapsible = TRUE,
#   #                  width = NULL,
#   #                  align = "justify",
#   #                  textOutput("abouttext") 
#   #              )
#   # )      
# )
# 
# #==========================================================  
# # AspatialOverviewrow2 ---
# #==========================================================  
# AspatialOverviewrow2 <-  fluidRow(
#   column(12,
#          conditionalPanel(
#            condition="input.ShowDataTable",
#            box(title = "Data Table: Armed Conflict Incidents in Myanmar",
#                status = "danger",
#                solidHeader = TRUE,
#                collapsible = TRUE,
#                width = NULL,
#                height = "600px",
#                align = "left",
#                withSpinner(DT::dataTableOutput("datatable_eo1")), 
#                style = "height: 600px; overflow-y: scroll; overflow-x: scroll;"
#            ))
#   )
#   
# )
# 
# 
# #==========================================================  
# # AspatialDistributionrow1 ---
# #==========================================================  
# AspatialDistributionrow1 <-  fluidRow(
#   column(2,
#          box(title = "Desired Characteristics",
#              status = "info",
#              solidHeader = FALSE, 
#              width = NULL,
#              height = "900px",
#              helpText("Filter options are applicable for both distribution"),
#              sliderInput(inputId = "YearSelect_eg2", 
#                          label = "Year:", 
#                          min = 2010, max = 2023,
#                          value = 2023, 
#                          sep = ""),
#              radioButtons(inputId = "Admin_eg2",
#                           label = "Administrative Region",
#                           choices = c("Level 1", "Level 2"),
#                           selected = "Level 1",
#                           inline = TRUE),
#              radioButtons(inputId = "InciFata_eg2",
#                           label = "Display (Rates)",
#                           choices = c("Incidents", "Fatalities"),
#                           selected = "Fatalities",
#                           inline = TRUE),
#              selectInput(inputId = "ColourSelect_eg2", 
#                          label = "Colour Palette:", 
#                          choices = c("Blues", "Greens", "Greys", "Oranges", "Reds"),
#                          selected = "Reds"
#              ),
#              hr(),
#              checkboxInput(inputId="ShowSDMStyle", label="Customise Spatial Map", value=FALSE),
#              conditionalPanel(condition="input.ShowSDMStyle",
#                               helpText("Filter options are applicable for spatial map distribution only"),
#                               selectInput(inputId = "ClassificationSelect_eg2", 
#                                           label = "Spatial Map Classification Type:",
#                                           choices = c("boxmap", "equal", "kmeans", "pretty", "quantile"),
#                                           selected = "kmeans"
#                               ),
#                               sliderInput(inputId = "ClassSelect_eg2", 
#                                           label = "Number of Classes:",
#                                           min = 3, max = 10,
#                                           value = 5)
#              ),
#              hr(),
#              checkboxInput(inputId="ShowRidgeStyle", label="Customise Density Ridge Plot", value=FALSE),
#              conditionalPanel(condition="input.ShowRidgeStyle",
#                               helpText("Filter option is applicable for density distribution only"),
#                               selectInput(inputId = "RidgeStyle_eg2", 
#                                           label = "Density Ridge Style:", 
#                                           choices = c("Default", "Quantile", "Probability", "Tail Probability"),
#                                           selected = "Default"
#                               )
#              )
#          )),
#   column(5,
#          box(title = "Spatial Map Distribution",
#              status = "danger",
#              solidHeader = TRUE,
#              width = NULL,
#              align = "left",
#              withSpinner(tmapOutput("emap_eg2", height = "850px", width = "100%"))
#          )   
#   ),
#   column(5,
#          box(title = "Density Distribution",
#              status = "danger",
#              solidHeader = TRUE,
#              width = NULL,  # Automatically adjust to full width
#              align = "left",
#              withSpinner(plotOutput("box_ed1", height = "850px", width = "100%"))
#              # withSpinner(plotOutput("hist_eg2", height = "850px", width = "100%"))
#          )   
#   )
# )
# 
# AspatialDistributionrow2 <-  fluidRow(
#   box(title = "Chart Interpretation",
#       status = "danger",
#       solidHeader = TRUE,
#       collapsible = TRUE,
#       width = NULL,
#       align = "justify",
#       textOutput("spatialtext") 
#   )
# )

#==========================================================  
# AspatialSubTabs
#==========================================================  
AspatialSubTabs <- tabsetPanel(
## commented out for aspatial  
  # tabPanel("Overview", 
  #          AspatialOverviewrow1,
  #          AspatialOverviewrow2
  # ),
  # tabPanel("Distribution Analysis", 
  #          AspatialDistributionrow1,
  #          AspatialDistributionrow2
  # )
)

# =============================    
########### ASPATIAL - END
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
## commented out for geospatial
# Cluster2 <- fluidRow(
#   column(2,
#          box(title = "Analysis Period: 2021-2023, Quarterly",
#              status = "info",
#              solidHeader = FALSE,
#              width = NULL,
#              helpText("Options for LISA Analysis"),
#              selectInput("QtrMoransI", "Year-Quarter",
#                          choices = c("2021-Q1" = "2021Q1",
#                                      "2021-Q2" = "2021Q2",
#                                      "2021-Q3" = "2021Q3",
#                                      "2021-Q4" = "2021Q4",
#                                      "2022-Q1" = "2022Q1",
#                                      "2022-Q2" = "2022Q2",
#                                      "2022-Q3" = "2022Q3",
#                                      "2022-Q4" = "2022Q4",
#                                      "2023-Q1" = "2023Q1",
#                                      "2023-Q2" = "2023Q2",
#                                      "2023-Q3" = "2023Q3",
#                                      "2023-Q4" = "2023Q4"),
#                          selected = "2021-Q1"),
#              selectInput("MoranEventType", "Event Type:",
#                          choices = c("Battles" = "Battles",
#                                      "Violence against civilians" = "Violence against civilians",
#                                      "Protests" = "Protests",
#                                      "Explosions/Remote violence" = "Explosions/Remote violence",
#                                      "Riots" = "Riots"),
#                          selected = "Battles"),
#              radioButtons(inputId = "Contiguity1",
#                           label = "Contiguity Method",
#                           choices = c("Queen" = TRUE, 
#                                       "Rook" = FALSE),
#                           selected = "TRUE",
#                           inline = TRUE),
#              selectInput("MoranWeights", "Spatial Weights Style",
#                          choices = c("W: Row standardised" = "W",
#                                      "B: Binary" = "B",
#                                      "C: Globally standardised" = "C",
#                                      "U: C / no of neighbours" = "U",
#                                      "minmax" = "minmax",
#                                      "S: Variance" = "S"),
#                          selected = "W"),
#              sliderInput(inputId = "MoranSims", 
#                          label = "Number of Simulations:", 
#                          min = 99, max = 499,
#                          value = 99, step = 100),
#              actionButton("MoranUpdate", "Update Plot"),
#              hr(),
#              radioButtons(inputId = "MoranConf",
#                           label = "Select Confidence level",
#                           choices = c("0.95" = 0.05, 
#                                       "0.99" = 0.01),
#                           selected = 0.05,
#                           inline = TRUE),
#              selectInput("LisaClass", "Select Lisa Classification",
#                          choices = c("mean" = "mean",
#                                      "median" = "median",
#                                      "pysal" = "pysal"),
#                          selected = "mean"),
#              selectInput("localmoranstats", "Select Local Moran's Stat:",
#                          choices = c("local moran(ii)" = "local moran(ii)",
#                                      "expectation(eii)" = "expectation(eii)",
#                                      "variance(var_ii)" = "variance(var_ii)",
#                                      "std deviation(z_ii)" = "std deviation(z_ii)",
#                                      "P-value" = "p_value"),
#                          selected = "local moran(ii)")
#              
#          )
#   ),
#   column(4,
#          box(title = "Local Moran's I - All Districts",
#              status = "danger",
#              solidHeader = TRUE,
#              collapsible = TRUE,
#              width = NULL,
#              align = "left",
#              withSpinner(plotOutput("LocalMoranMap", height = "700px", width = "100%"))
#          )
#   ),
#   column(4,
#          box(title = "Local Indicator of Spatial Association (LISA) map",
#              status = "danger",
#              solidHeader = TRUE,
#              collapsible = TRUE,
#              width = NULL,
#              align = "left",
#              withSpinner(plotOutput("Lisa", height = "700px", width = "100%"))
#          )
#   ),
#   column(2,
#          box(title = "Chart Interpretation",
#              status = "danger",
#              solidHeader = TRUE,
#              collapsible = TRUE,
#              width = NULL,
#              textOutput("MoransItext")
#          )
#   )
# )
# 
# # Add this outside the first fluidRow to make it full width and below everything else
# Cluster2 <- tagList(Cluster2, 
#                     fluidRow(
#                       column(12,
#                              box(title = "Local Moran's I - All Districts",
#                                  status = "danger",
#                                  solidHeader = TRUE,
#                                  collapsible = TRUE,
#                                  width = NULL,
#                                  align = "center",
#                                  withSpinner(dataTableOutput("localMoransTable1")),
#                                  style = "height:600px; overflow-y: scroll; overflow-x: scroll;"))
#                       #column(12,       
#                       #       box(
#                       #         title = "LISA results (P-values < 0.05)",
#                       #         status = "danger",
#                       #         solidHeader = TRUE,
#                       #         collapsible = TRUE,     # table with just isolated significant vals (KIV)
#                       #         width = NULL,
#                       #         align = "center",
#                       #         dataTableOutput("localMoransTable2"),
#                       #         style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
#                       #)
#                     )
#                     
# )
# 
# 
# #==========================================================  
# ##Geospatial Analysis, 2nd Tab
# #==========================================================  
# 
# #==========================================================  
# ##Hot and Cold Spot Analysis
# #========================================================== 
# 
# HotCold1 <- fluidRow(
#   column(2,
#          box(title = "Analysis Period: 2021-2023, Quarterly",
#              status = "info",
#              solidHeader = FALSE,
#              width = NULL,
#              helpText("Options for Hot Spot Analysis"),
#              selectInput("GIQtr" , "Year-Quarter",
#                          choices = c("2021-Q1" = "2021Q1",
#                                      "2021-Q2" = "2021Q2",
#                                      "2021-Q3" = "2021Q3",
#                                      "2021-Q4" = "2021Q4",
#                                      "2022-Q1" = "2022Q1",
#                                      "2022-Q2" = "2022Q2",
#                                      "2022-Q3" = "2022Q3",
#                                      "2022-Q4" = "2022Q4",
#                                      "2023-Q1" = "2023Q1",
#                                      "2023-Q2" = "2023Q2",
#                                      "2023-Q3" = "2023Q3",
#                                      "2023-Q4" = "2023Q4"),
#                          selected = "2021-Q1"),
#              selectInput("GIEventType", "Event Type:",
#                          choices = c("Battles" = "Battles",
#                                      "Violence against civilians" = "Violence against civilians",
#                                      "Protests" = "Protests",
#                                      "Explosions/Remote violence" = "Explosions/Remote violence",
#                                      "Riots" = "Riots"),
#                          selected = "Battles"),
#              radioButtons(inputId = "Contiguity2",
#                           label = "Contiguity Method",
#                           choices = c("Queen" = TRUE, 
#                                       "Rook" = FALSE),
#                           selected = "TRUE",
#                           inline = TRUE),
#              sliderInput(inputId = "GISims", 
#                          label = "Number of Simulations:", 
#                          min = 99, max = 499,
#                          value = 99, step = 100),
#              actionButton("GIUpdate", "Update Plot"),
#              hr(),
#              radioButtons(inputId = "GIConf",
#                           label = "Select Confidence level",
#                           choices = c("0.95" = 0.05, 
#                                       "0.99" = 0.01),
#                           selected = 0.05,
#                           inline = TRUE),
#              selectInput("localgistats", "Select Local GI Stat:",
#                          choices = c("local gi*" = "local gi*",
#                                      "expectation(e_gi)" = "expectation(e_gi)",
#                                      "variance(var_gi)" = "variance(var_gi)",
#                                      "std deviation" = "std deviation",
#                                      "P-value" = "p_value"),
#                          selected = "local gi*")
#          )
#          
#   ),
#   column(4,
#          box(title = "GI* Statistics- All Districts"
#              ,status = "danger"
#              ,solidHeader = TRUE 
#              ,collapsible = TRUE
#              ,width = NULL
#              ,align = "left"
#              ,withSpinner(plotOutput("Gistarmap", height = "700px", width = "100%"))
#          )
#   ),
#   column(4,
#          box(
#            title = "Significant Hot & Cold spot areas",
#            status = "danger",
#            solidHeader = TRUE,
#            collapsible = TRUE,
#            width = NULL,
#            align = "left",
#            withSpinner(plotOutput("HotColdmap", height = "700px", width = "100%"))
#          )
#   ),
#   column(2,
#          box(title = "Chart Interpretation",
#              status = "danger",
#              solidHeader = TRUE,
#              collapsible = TRUE,
#              width = NULL,
#              textOutput("HotColdText")
#          )
#   )  
#   
#   
# )  
# 
# HotCold1 <- tagList(HotCold1,
#                     fluidRow(
#                       column(12,
#                              box(
#                                title = "GI* Statistics - All Districts",
#                                status = "danger",
#                                solidHeader = TRUE,
#                                collapsible = TRUE,
#                                width = NULL,
#                                align = "center",
#                                withSpinner(dataTableOutput("GiStat")),
#                                style = "height:500px; overflow-y: scroll;overflow-x: scroll;"))
#                       #column(12,       
#                       #        box(
#                       #            title = "GI* Statistics - Significant Hot & Cold spots (P-values < 0.05)",
#                       #            status = "danger",
#                       #            solidHeader = TRUE,
#                       #            collapsible = TRUE,      # table with just isolated significant vals (KIV)
#                       #            width = NULL,
#                       #            align = "center",
#                       #            dataTableOutput("GiStat2"),
#                       #            style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
#                       #  )
#                     )
#                     
# )
# 
# #==========================================================  
# ##Geospatial Analysis, 3rd Tab
# #==========================================================  
# 
# #==========================================================  
# ##Emerging Hot Spot Analysis
# #==========================================================  
# 
# 
# EHSA2 <- fluidRow(
#   column(2,
#          box(title = "Analysis Period: 2021-2023, Quarterly",
#              status = "info",
#              solidHeader = FALSE,
#              width = NULL,
#              helpText("Options for Emerging Hot Spot Analysis"),
#              selectInput("EHSAEventType", "Event Type:",
#                          choices = c("Battles" = "Battles",
#                                      "Violence against civilians" = "Violence against civilians",
#                                      "Protests" = "Protests",
#                                      "Explosions/Remote violence" = "Explosions/Remote violence"),
#                          selected = "Battles"),
#              radioButtons(inputId = "Contiguity3",
#                           label = "Contiguity Method",
#                           choices = c("Queen" = TRUE, 
#                                       "Rook" = FALSE),
#                           selected = "TRUE",
#                           inline = TRUE),
#              sliderInput(inputId = "EHSANumLags", 
#                          label = "Time Lag of spatial neighbours:", 
#                          min = 1, max = 5,
#                          value = 1),
#              sliderInput(inputId = "EHSANumSims", 
#                          label = "Number of Simulations:", 
#                          min = 99, max = 499,
#                          value = 99, step = 100),
#              actionButton("EHSAUpdate", "Update Plot"),
#              hr(),
#              checkboxInput(inputId="ShowEHSA", label="Show EHSA classes", value=FALSE),
#              conditionalPanel(condition="input.ShowEHSA",
#                               withSpinner(plotlyOutput("EHSAbar", height = "200px"))
#              ),
#              checkboxInput(inputId="ShowGI", label="Show GI* trend plot", value=FALSE),
#              conditionalPanel(condition="input.ShowGI",
#                               selectizeInput(inputId = "EHSAAdmin2",
#                                              label = "Select District",
#                                              choices = unique(Space_2$DT),
#                                              multiple = FALSE),
#                               conditionalPanel(condition="input.EHSAAdmin2",
#                                                withSpinner(plotlyOutput("Giplot2", height = "400px")))
#              ),
#              hr(),
#              radioButtons(inputId = "EHSAConf",
#                           label = "Select Confidence level",
#                           choices = c("0.95" = 0.05, 
#                                       "0.99" = 0.01),
#                           selected = 0.05,
#                           inline = TRUE)
#          )
#   ),
#   column(4,
#          box(title = "Emerging Hot Spot map",
#              status = "danger",
#              solidHeader = TRUE,
#              collapsible = TRUE,
#              width = NULL,
#              height = "800px",
#              align = "left",
#              withSpinner(plotOutput("EHSAmap", height = "730px"))
#          ),
#          box(title = "Chart Interpretation",
#              status = "danger",
#              solidHeader = TRUE,
#              collapsible = TRUE,
#              width = NULL,
#              textOutput("EHSAText")
#          )
#   ), 
#   column(6,
#          box(title = "Emerging Hot Spot Analysis results",  # or "Mann Kendall Test results"
#              status = "danger",
#              solidHeader = TRUE,
#              collapsible = TRUE,
#              width = NULL,
#              height = "800px",
#              align = "left",
#              withSpinner(DT::dataTableOutput("MKtest2")), 
#              style = "height: 800px; overflow-y: scroll;"
#             ),
#          box(title = "Table Interpretation",
#              status = "danger",
#              solidHeader = TRUE,
#              collapsible = TRUE,
#              width = NULL,
#              textOutput("MKText")
#              )
#   )
# )
# 

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
  box(title = "Chart Interpretation",
      status = "danger",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = NULL,
      textOutput("AnovaText")
  )
)


#Confirm2 <- fluidRow(
#  column(12,
#         box(title = "Analysis Period: 2020-2023",
#             status = "info",
#             solidHeader = FALSE,
#             width = NULL,
#             helpText("Filter Options for Dataset"),
#             selectInput(inputId = "YearMosaic",
#                         label = "Year:",
#                         choices = seq(2020,2023),
#                         selected = 2023)

#         )
#         ,
#         box(title = "Chart Interpretation",
#             status = "danger",
#             solidHeader = TRUE,
#             collapsible = TRUE,
#             width = NULL,
#             textOutput("MosaicText")
#         )
#  ),
#  column(10,
#         box(title = "Mosaic Plot for event type per Region/State",
#             status = "danger",
#             solidHeader = TRUE,
#             collapsible = TRUE,
#             width = NULL,
#             align = "left",
#             plotlyOutput("Mosaicplot", height = "1400px")
#         )
#  )
#)


Confirm3 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2020-2023",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Filter Options for Dataset"),
             selectInput(inputId = "YearMosaic2",
                         label = "Year:",
                         choices = seq(2010,2023),
                         multiple = TRUE,
                         selected = 2023),
             selectizeInput(inputId = "Option1",
                            label = "Region: ",
                            choices = unique(ACLED_MMR$admin1),
                            multiple = TRUE),
             selectizeInput(inputId = "Option2",
                            label = "Event Type: ",
                            choices = unique(ACLED_MMR$event_type),
                            multiple = TRUE
             ),
             selectInput(inputId = "Option3",
                         label = "Include Data with or without Fatalities: ",
                         choices = c("No Fatalities", "Has Fatalities"),
                         multiple = TRUE
             ),
             actionButton("Mosaic2Update", "Update Plot")
         )
  ),
  column(10,
         box(title = "Mosaic Plot for event type per Region/State",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             plotOutput("Mosaicplot2", height = "700px")
         )
  )
)

Confirm4 <- fluidRow(
  box(title = "Chart Interpretation",
      status = "danger",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = NULL,
      textOutput("Mosaic2Text")
  )
)
  


#==========================================================  
#Confirmation Analysis tab --- End
#==========================================================  




#define the no of sub tabs needed

ClusterSubTabs <- tabsetPanel(
## commented out for geospatial  
  # tabPanel("Local Measures of Spatial Autocorrelation", 
  #          Cluster2),
  # tabPanel("Hot & Cold Spot Analysis(HCSA)", 
  #          HotCold1),
  # tabPanel("Emerging Hot Spot Analysis", 
  #          EHSA2)
)

#ESHASubTabs <- tabsetPanel(
#tabPanel("Gi* trend and Mann Kendall test", 
#EHSA1),
# tabPanel("Emerging Hot Spot Analysis", 
#         EHSA2)

#)


ConfirmSubTabs <- tabsetPanel(
  tabPanel("One-Way Anova Test", 
           Confirm1,
           Confirm2),
  #tabPanel("Mosaic Plot",
  #         Confirm2),
  tabPanel("Mosaic Plot-VCD",
           Confirm3,
           Confirm4)
  #Confirm2)
  
)


  
#==========================================================  
#ACLED Datatable tab --- START  
#==========================================================    
ACLEDDataTable <- fluidRow(
      column(12,
             box(title = "Table: Armed Conflict Incidents",
                 status = "danger",
                 solidHeader = TRUE,
                 width = NULL,
                 height = "700px",
                 align = "left",
                 withSpinner(DT::dataTableOutput("datatable_ACLED1")), 
                 style = "height: 700px; overflow-y: scroll;"
             ))
      )

#==========================================================  
#ACLED Datatable tab --- END
#==========================================================  



# main body ---
body <- dashboardBody(
  
  # use theme
  use_theme(mytheme),
  
  # =============================
  # tabItems - All Pages
  # =============================
  
  tabItems(
    # 1st tab content
    tabItem(tabName = "Aspatial",
            AspatialSubTabs
    ),
    # 2nd tab content
    tabItem(tabName = "Cluster",
            ClusterSubTabs 
    ),
    #3rd tab content
    #tabItem(tabName = "EHSA",  #seperate tab set for EHSA (KIV)
    
    #      ESHASubTabs
    # ),
    #4th tab content
    tabItem(tabName = "ConfirmatoryAnalysis",
            ConfirmSubTabs
    ),
    #5th tab content
    tabItem(tabName = "ACLEDTable",
            ACLEDDataTable
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


# create the server functions for the dashboard  
server <- function(input, output, session) { 
  
  # =============================    
  # START of ASPATIAL Module
  # =============================
  
  #==========================================================
  # Aspatial Map ---
  #==========================================================   
  final_explorespatial <- eventReactive(input$emapButton, {
    # req(input$YearSlider_eo1)
    # req(input$EventSelect_eo1)
    # req(input$AdminSelect_eo1)
    
    if(input$Admin_eo1 == "Level 1"){
      aspatialdata <- filter(final, year >= input$YearSlider_eo1[1], year <= input$YearSlider_eo1[2]) %>%
        filter(event_type %in% input$EventSelect_eo1,
               admin1 %in% input$AdminSelect_eo1)
      return(aspatialdata)
    }
    else{
      aspatialdata <- filter(final, year >= input$YearSlider_eo1[1], year <= input$YearSlider_eo1[2]) %>%
        filter(event_type %in% input$EventSelect_eo1,
               admin2 %in% input$AdminSelect_eo2)
      return(aspatialdata)
    }
    
  })
  
  
  final_maplayer <- eventReactive(input$emapButton, {
    if(input$Admin_eo1 == "Level 1"){
      maplayer1
    }
    else{
      maplayer2
    }
  })
  
  # Render Point Map --- 
  output$emap_eo1 <- renderLeaflet({
    
    incident_pts <- final_explorespatial()
    map <- final_maplayer()
    
    incident_pts %>%
      filter(fatalities > 0)
    
    cof <- colorFactor(c("#ff7e8a", "#394938", "#ffa500", 
                         "#0092ff", "#741b47", "#60dcb5"), 
                       domain=c("Battles", "Explosions/ Remote violence", 
                                "Protests", "Riots",
                                "Strategic developments", 
                                "Violence against civilians"))
    
    leaflet(incident_pts, options = leafletOptions(minZoom = 6, maxZoom = 8
                                                   # ,maxBounds = list(list(94, 19), list(102, 23))
    )) %>% 
      addTiles('https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
      addPolygons(data = map, color = "lightgrey", weight = 1, fillOpacity = 0.5) %>%
      setView(98, 21, zoom = 6) %>%
      addCircleMarkers(lat= ~latitude, lng = ~longitude, radius = ~(fatalities)^(1/2),
                       color = ~cof(event_type), weight = 1, fillOpacity = 0.1,
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
                                      incident_pts$fatalities)) %>% 
      addLegend("bottomright", pal = cof, values = incident_pts$event_type, title = "Event Type")
  })
  
  output$abouttext <- renderText({ 
    "Armed conflicts due to political violence and coordinated attacks targeting innocent civilians, have been on the rise globally. 
      This threatens the public at both physical and psychological levels. 
      This visualisation UI helps: 
      (1) discover armed conflicts trends and 
      (2) conceptualise armed conflict spaces in Myanmar." 
  })
  
  
  
  #==========================================================
  # Aspatial Line Chart ---
  #========================================================== 
  final_exploreline <- eventReactive(input$emapButton, {
    # req(input$YearSlider_eo1)
    # req(input$EventSelect_eo1)
    # req(input$AdminSelect_eo1)
    if(input$Admin_eo1 == "Level 1"){
      filter(final, year >= input$YearSlider_eo1[1], year <= input$YearSlider_eo1[2]) %>%
        filter(event_type %in% input$EventSelect_eo1) %>%
        filter(admin1 %in% input$AdminSelect_eo1)
    }
    else{
      filter(final, year >= input$YearSlider_eo1[1], year <= input$YearSlider_eo1[2]) %>%
        filter(event_type %in% input$EventSelect_eo1) %>%
        filter(admin2 %in% input$AdminSelect_eo2)
    }
    
  })
  
  output$line_eo1 <- renderHighchart({
    linedata <- final_exploreline()
    
    if(input$Admin_eo1 == "Level 1"){
      year_fata <- linedata %>%
        filter(fatalities > 0) %>%
        group_by(year) %>%
        select(year, fatalities, event_type, admin1) %>%
        summarise(total_fata = sum(fatalities),
                  total_inci = n()) %>%
        ungroup()
    }
    else{
      year_fata <- linedata %>%
        filter(fatalities > 0) %>%
        group_by(year) %>%
        select(year, fatalities, event_type, admin2) %>%
        summarise(total_fata = sum(fatalities),
                  total_inci = n()) %>%
        ungroup()
    }
    
    hc_plot1 <-  highchart() %>% 
      hc_add_series(year_fata, hcaes(x = as.factor(year), y = total_fata), type = "line", 
                    name = "Total Fatalities", color = "lightcoral") %>%
      hc_add_series(year_fata, hcaes(x = as.factor(year), y = total_inci), type = "line", 
                    name = "Total Incidents", color = "black") %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", 
                 backgroundColor = "#FCFFC5",
                 borderWidth = 5,
                 pointFormat = "<b>{point.year}</b> 
                                 <br> Fatalities: <b>{point.total_fata}</b>
                                 <br> Incidents: <b>{point.total_inci}</b>"
      ) %>%
      # hc_title(text = "Armed Conflict Over The Years") %>% 
      hc_subtitle(text = paste("Year ",input$YearSlider_eo1[1], "-", input$YearSlider_eo1[2])) %>%
      hc_xAxis(title = list(text = paste(input$YearSlider_eo1[1], "-", input$YearSlider_eo1[2])), labels = list(enabled = FALSE)) %>%
      hc_yAxis(title = list(text = "Frequency"),
               allowDecimals = FALSE,
               plotLines = list(list(
                 color = "lightcoral", width = 1, dashStyle = "Dash",
                 value = mean(year_fata$total_fata),
                 label = list(text = paste("Average fatalities:", round(mean(year_fata$total_fata))),
                              style = list(color = 'lightcoral', fontSize = 8)))))
    hc_plot1
  })
  
  #==========================================================
  # Aspatial Data Table ---
  #==========================================================
  output$datatable_eo1 <- DT::renderDataTable({
    
    table_pts <- final_explorespatial()
    
    DT::datatable(
      table_pts, 
      class = "compact",
      filter = "top", 
      extensions = c("Buttons"),
      options = list(pageLength = 5,
                     columnDefs = list(
                       list(targets = c(1:7, 9, 11:14, 16:23, 26), className = "dt-center"), 
                       list(targets = c(8, 10, 15:17, 24, 25), visible = FALSE)), 
                     buttons = c("colvis", "excel", "csv", "print"),
                     dom = "Bpiltf",
                     scrollX = TRUE,
                     scrolly = TRUE)
    )
  })
  
  #==========================================================
  # Aspatial Spatial Distribution Map ---
  #==========================================================  
  mapping_rates_exploredist <- reactive({
    req(input$InciFata_eg2)
    req(input$YearSelect_eg2)
    req(input$ClassSelect_eg2)
    req(input$ClassificationSelect_eg2)
    req(input$ColourSelect_eg2)
    req(input$Admin_eg2)
  })
  
  # Render Spatial Distribution Map --- 
  output$emap_eg2 <- renderTmap({
    
    if(input$InciFata_eg2 == "Incidents"){
      var <- paste("pct_inci_", input$YearSelect_eg2, sep = "")
    }
    else{
      var <- paste("pct_fata_", input$YearSelect_eg2, sep = "")
    }
    
    # Render boxmap classification
    if(input$ClassificationSelect_eg2 == "boxmap"){
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
                  palette=input$ColourSelect_eg2,
                  labels = c("lower outlier", 
                             "< 25%", 
                             "25% - 50%", 
                             "50% - 75%",
                             "> 75%", 
                             "upper outlier"))  +
          tm_layout(main.title = mtitle, 
                    title.position = c("left",
                                       "top")) +
          tm_view(set.zoom.limits = c(5,7))
      }
      
      if(input$Admin_eg2 == "Level 1"){
        box_inci <- boxmap(var, mapping_rates)
      }
      else{
        box_inci <- boxmap(var, mapping_rates2)
      }
      
    }
    # Render other classification types
    else{
      if(input$Admin_eg2 == "Level 1"){
        tmap_obj2 <- tm_shape(mapping_rates) +
          tm_fill(var, 
                  n = input$ClassSelect_eg2,
                  style = input$ClassificationSelect_eg2,
                  palette = input$ColourSelect_eg2, 
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
                    legend.position = c("left", "bottom")) +
          tm_view(set.zoom.limits = c(5,7))
      }
      else{
        tmap_obj2 <- tm_shape(mapping_rates2) +
          tm_fill(var, 
                  n = input$ClassSelect_eg2,
                  style = input$ClassificationSelect_eg2,
                  palette = input$ColourSelect_eg2, 
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
                    legend.position = c("left", "bottom")) +
          tm_view(set.zoom.limits = c(5,7))
      }
    }
  })
  
  ########################################  
  output$spatialtext <- renderText({ 
    paste(
      "A choropleth map (left) visualises spatial pattern across Myanmar's
    different geographical subnational administrative region 1 or 2, which can be further customised 
    by adjusting the desired data classification type and number of classes (or data ranges).",
      "A density ridge plot (right) provides insights to the distribution across Myanmar's
    different geographical subnational administrative region 1 or 2 based on the selected density style 
    (i.e. default, quantile, probability, tail probability)."
    )
    
  })
  
  #==========================================================  
  # Aspatial Density Distributions ---
  #==========================================================  
  mapping_rates_exploredens <- reactive({
    req(input$Admin_eg2)
    req(input$InciFata_eg2)
    req(input$YearSelect_eg2)
    req(input$RidgeStyle_eg2)
    req(input$ColourSelect_eg2)
  })
  
  # Render density --- 
  output$box_ed1 <- renderPlot({
    years <- input$YearSelect_eg2
    
    if(input$Admin_eg2 == "Level 1"){
      datatable <- joined
    }
    else{
      datatable <- joined2
    }
    
    dens <- datatable %>%
      group_by(event_date, year, shapeName) %>%
      filter(year == years & !is.na(shapeName)) %>%
      mutate(total_inci = n(),
             total_fata = sum(fatalities),
             total_political = sum(political_count),
             total_violenceagainstcivilian = sum(civilian_count)) %>%
      ungroup()
    
    if(input$InciFata_eg2 == "Incidents"){
      var <- dens$total_inci
    }
    else{
      var <- dens$total_fata
    }
    
    # set colour for density ridge plots
    if(input$RidgeStyle_eg2 == "Default"){
      
      if(input$ColourSelect_eg2 == "Reds"){
        col <- "#F8B3AE"
      }
      else if(input$ColourSelect_eg2 == "Blues"){
        col <- "#7FB9DA"
      }
      else if(input$ColourSelect_eg2 == "Greens"){
        col <- "#85CC84"
      }
      else if(input$ColourSelect_eg2 == "Greys"){
        col <- "#A5A5A5"
      }
      else if(input$ColourSelect_eg2 == "Oranges"){
        col <- "#FD9A4E"
      }
      
      # plot density ridge plots
      ggplot(dens,
             aes(x = var, 
                 y = reorder(shapeName, desc(shapeName)))) +
        geom_density_ridges(
          scale = 3,
          rel_min_height = 0.01,
          bandwidth = 3.4,
          alpha = 0.5,
          color = "white",
          fill = col
        ) +
        scale_x_continuous(
          name = "",
          expand = c(0, 0)
        ) +
        scale_y_discrete(name = NULL, expand = expansion(add = c(0.2, 2.6))) +
        facet_grid(year ~ ., scales = "free_y") +
        theme_ridges() +
        theme(axis.text.x = element_blank())
    }
    else if(input$RidgeStyle_eg2 == "Quantile"){
      ggplot(dens,
             aes(x = var, 
                 y = reorder(shapeName, desc(shapeName)), 
                 fill = factor(stat(quantile))
             )) +
        stat_density_ridges(
          geom = "density_ridges_gradient",
          calc_ecdf = TRUE, 
          quantiles = 4,
          quantile_lines = TRUE) +
        scale_fill_viridis_d(name = "Quartiles") +
        scale_x_continuous(
          name = "",
          expand = c(0, 0)
        ) +
        scale_y_discrete(name = NULL, expand = expansion(add = c(0.2, 2.6))) +
        facet_grid(year ~ ., scales = "free_y") +
        theme_ridges() +         
        theme(axis.text.x = element_blank())
    }
    else if(input$RidgeStyle_eg2 == "Probability"){
      ggplot(dens,
             aes(x = var, 
                 y = reorder(shapeName, desc(shapeName)), 
                 fill = factor(stat(quantile))
             )) +
        stat_density_ridges(
          geom = "density_ridges_gradient",
          calc_ecdf = TRUE, 
          quantiles = c(0.015, 0.985)
        ) +
        scale_fill_manual(
          name = "Probability",
          values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
          labels = c("(0, 0.015]", "(0.015, 0.985]", "(0.985, 1]")
        ) +
        scale_x_continuous(
          name = "",
          expand = c(0, 0)
        ) +
        scale_y_discrete(name = NULL, expand = expansion(add = c(0.2, 2.6))) +
        facet_grid(year ~ ., scales = "free_y") +
        theme_ridges() +         
        theme(axis.text.x = element_blank())
    }
    else{
      ggplot(dens,
             aes(x = var, 
                 y = reorder(shapeName, desc(shapeName)), 
                 fill = 0.5 - abs(0.5-stat(ecdf)))) +
        stat_density_ridges(geom = "density_ridges_gradient", 
                            calc_ecdf = TRUE) +
        scale_fill_viridis_c(name = "Tail probability",
                             direction = -1) +
        scale_x_continuous(
          name = "",
          expand = c(0, 0)
        ) +
        scale_y_discrete(name = NULL, expand = expansion(add = c(0.2, 2.6))) +
        facet_grid(year ~ ., scales = "free_y") +
        theme_ridges() +         
        theme(axis.text.x = element_blank())
    }
    
    
  })
  
  

  #==========================================================
  # END of ASPATIAL Module
  #==========================================================
  
  
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
  
  ACLED_MMR_Mosaic <- ACLED_MMR %>%
    group_by(event_id_cnty, year, country, admin1,event_type, disorder_type, fatalities) %>%
    summarize(
      Has_Fatalities = ifelse(fatalities > 0, "Has Fatalities", "No Fatalities")) %>%
    ungroup()
  
  
  
  
  # =============================    
  #DATA Wrangling - END
  # ============================= 
  
  
  #==========================================================
  # START of Geospatial Analysis Module
  #==========================================================   
  
  
  #==========================================================
  # Local Measures of Spatial AutoCorrelation
  #==========================================================   
  
  localMIResults <- eventReactive(input$MoranUpdate,{
    # Filter the data based on the user's selection
    filteredData <- Events_admin2 %>%
      filter(quarter == input$QtrMoransI, event_type == input$MoranEventType) 
    
    
    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data
    
    # Computing Contiguity Spatial Weights
    wm_q <- filteredData %>%
      mutate(nb = st_contiguity(geometry, queen = input$Contiguity1),
             wt = st_weights(nb,
                             style = input$MoranWeights))
    
    
    
    # Computing Local Moran's I
    lisa <- wm_q %>%
      mutate(local_moran = local_moran(
        Incidents, nb, wt, nsim = as.numeric(input$MoranSims)),
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
  
  
  
  
  # LISA Map in Cluster 2 
  
  output$Lisa <- renderPlot({
    df <- localMIResults()
    if(is.null(df)) return()
    
    
    lisa_sig <- df  %>%
      filter(p_value < as.numeric(input$MoranConf))  
    
    lisamap <- tm_shape(df) +
      tm_polygons() +
      tm_borders() +
      
      tm_shape(lisa_sig) +
      tm_fill(col = input$LisaClass,  
              palette = "-RdBu",  
              title = (paste("Significance:", input$LisaClass))) +
      tm_borders(alpha = 0.4)
    
    
    lisamap #+ 
    #tm_view(set.zoom.limits = c(5,7))  # for tmap lock zoom (KIV)
    
    
  })
  
  
  # Local Morans's I Data Table in Cluster 2 
  
  # Render the data table for Local Moran's I results
  output$localMoransTable1 <- renderDataTable({
    df <- localMIResults()
    
    # Check if data is available
    if (is.null(df)) return()
    
    df
    
    
  })
  
  ## Just to show table for LISA map values (KIV)
  
  # output$localMoransTable2 <- renderTable({
  #    df2 <- localMIResults()
  
  # Check if data is available
  #    if (is.null(df2)) return()   # For table with significant vals only(KIV)
  
  #    lisa_sig2 <- df2  %>%
  #      filter(p_value < 0.05)
  
  #    lisa_sig2
  
  #  })
  
  output$MoransItext <- renderText({ 
    "Local Moran's I assesses spatial patterns at a local level, 
    determining if features form significant clusters (high-high or low-low) or outliers 
    (high-low or low-high) in relation to neighboring features. 
    
    High and positive Local Moran's I values indicate clustering of similar values, reflecting a concentration of similar incidents. 
    Low or negative values point to outliers, where an area's incident rate significantly 
    differs from that of its neighbors. 
    
    The Lisa map plots significant areas (p-value < 0.05 or 0.01) where incident rates are 
    notably higher or lower than expected, thus deviating from 
    a random spatial distribution." 
  })
  
  
  
  
  #==========================================================
  # Moran Scatter plot in Cluster 3 using spdep package - (KIV)
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
  #  "The Moran scatterplot is divided into four areas, with each quadrant corresponding 
  #    with one of four categories: (1) High-High (HH) in the top-right quadrant; (2) High-Low (HL) 
  #    in the bottom right quadrant; (3) Low-High (LH) in the top-left quadrant; 
  #    (4) Low- Low (LL) in the bottom left quadrant. The top right corner belongs to areas that have high incidents of events and are surrounded by other areas 
  #    that have higher than the average level/number of battles This is the high-high locations." 
  #})
  
  
  
  #==========================================================
  # Hot & Cold Spot Analysis - GI* statistics
  #==========================================================
  
  
  GiData <- eventReactive(input$GIUpdate,{
    filtered_data2 <- Events_admin2 %>%
      filter(quarter == input$GIQtr, event_type == input$GIEventType)
    
    
    #Derive a spatial weight matrix by using sfdep functions and tidyverse approach.
    wm_idw <- filtered_data2 %>%
      mutate(nb = st_contiguity(geometry, queen = input$Contiguity2),
             wts = st_inverse_distance(nb, geometry,
                                       scale = 1,
                                       alpha = 1))
    
    #computing the local Gi* 
    
    HCSA <- wm_idw %>% 
      mutate(local_Gi = local_gstar_perm(
        Incidents, nb, wt, nsim = as.numeric(input$GISims)),
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
    #tm_view(set.zoom.limits = c(5,7)) # for tmap only
  })
  
  
  output$HotColdmap <-  renderPlot({
    df <- GiData()
    
    if(is.null(df) || nrow(df) == 0) return() #Exit if no data
    
    
    HCSA_sig <- df  %>%
      filter(p_value < as.numeric(input$GIConf))
    
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
  
  #isolating for just values in HSCA map (KIV)
  
  #  output$GiStat2 <- renderDataTable({
  #    data_with_gi2 <- GiData()  
  #    if(is.null(data_with_gi2)) return ()
  
  #    HCSA_sig2 <- data_with_gi2  %>%
  #      filter(p_value < 0.05)  
  
  #    HCSA_sig2
  #  })
  
  
  output$HotColdText <- renderText({ 
    "HCSA uses spatial weights to identify locations of statistically significant 
    hot & cold spots in an spatially weighted attribute, in proximity 
    to one another based on a calculated distance. 
    
    The analysis groups features when similar high (hot) or low (cold) values are found in a cluster.
    
    High positive Gi values indicate hot spots areas where high values cluster together,
    while low negative Gi values indicate cold spots—areas where low values cluster together.
    
    The Hot & Cold spot map plots significant areas where p-value < 0.05 or 0.01.
    
    GI* trend plot shows changes in the Local Gi* per district, for each event type.
    
    "
  })
  
  
  #==========================================================
  # EMERGING HOT SPOT ANALYSIS
  #==========================================================
  
  # Distribution of EHSA classes and EHSA Map
  
  
  EHSAData <- eventReactive(input$EHSAUpdate,{
    space_data <- Space_2 %>%
      filter(event_type == input$EHSAEventType)
    
    
    Filtered_space <- space_data %>%
      select(-event_type, -year, -Fatalities)
    
    Quarterly_spt <- spacetime(Filtered_space, mmr_shp_mimu_2,
                               .loc_col = "DT",
                               .time_col = "quarter")
    
    ehsa3 <- emerging_hotspot_analysis(
      x = Quarterly_spt, 
      .var = "Incidents", 
      k = as.numeric(input$EHSANumLags),
      nsim = as.numeric(input$EHSANumSims)
      
    )
    
    
    return(ehsa3)
    
  }) 
  
  output$EHSAbar <- renderPlotly({
    df <- EHSAData()
    
    df <- df %>%
      filter(p_value < as.numeric(input$EHSAConf)) %>%
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
      theme(axis.title.y = element_blank(),
            axis.text.x = element_blank())
    
    ggplotly(EHSAbar)
  })
  
  
  
  
  EHSAMapdata <- reactive({
    df <- EHSAData() 
    
    mmr3_ehsa <- mmr_shp_mimu_2 %>%
      left_join(df,
                by = join_by(DT == location))
    
    mmr3_ehsa <- mmr3_ehsa %>%
      select(-OBJECTID, -ST, -ST_PCODE,
             -DT_PCODE, -DT_MMR, -PCode_V) %>%
      rename("District" = "DT")
    
    return(mmr3_ehsa)
  })
  
  output$EHSAmap <- renderPlot({
    df <- EHSAMapdata()
    if(is.null(df)) return()
    
    ehsa_sig3 <- df  %>%
      filter(p_value < as.numeric(input$EHSAConf))
    
    
    ehsamap <- tm_shape(df) +
      tm_borders() +
      
      tm_shape(ehsa_sig3) +
      tm_fill("classification") +
      tm_borders(alpha = 0.4)
    
    
    ehsamap 
    
    
  })
  
  output$EHSAText <- renderText({ 
    "Emerging Hot Spot Analysis identifies trends in spatial clustering 
      over a period of time. It combines the Getis-Ord Gi* statistic 
      with the Mann-Kendall trend test to determine if there 
    is a temporal trend associated with local clustering of hot and cold spots.
    
    The Emerging Hot Spot map plots significant areas where p-values < 0.05 or 0.01.
    Each location is classified into one of 17 categories based on 
    ESRI's emerging hot spot classification criteria."
  })
  
  
  
  # For table with significant vals only   
  
  output$MKtest2 <- DT::renderDataTable({
    
    EHSATable <- EHSAMapdata()
    if(is.null(df)) return()
    
    
    ehsa_sig3 <- EHSATable  %>%
      filter(p_value < as.numeric(input$EHSAConf)) 
    
    # ehsa_sig3

    DT::datatable(
      ehsa_sig3, 
      class = "compact",
      filter = "top", 
      extensions = c("Buttons"),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrolly = TRUE
    ))
  })
  
  # For explanation of table with significant vals only  
  
  output$MKText <- renderText({ 
    "The Mann-Kendall test determines whether there is a 
      monotonic trend over time in the observed data. The Gi* values for each location in each time period (time-slice) 
      is calculated. Next, the Mann-Kendall trend test is done to identify any temporal trend in these Gi* values. 
      
    This tables shows results for P-values < 0.05 or 0.01. Tau ranges between -1 and 1 where -1 is a perfectly decreasing series and 1 is a perfectly increasing series."
    
  })    
  
  
  
  EHSAData2 <- eventReactive(input$EHSAUpdate,{
    space_data2 <- Space_2 %>%
      filter(event_type == input$EHSAEventType)
    
    
    Filtered_space2 <- space_data2 %>%
      select(-event_type, -year, -Fatalities)
    
    Quarterly_spt2 <- spacetime(Filtered_space2, mmr_shp_mimu_2,
                                .loc_col = "DT",
                                .time_col = "quarter")
    
    Quarterly_nb2 <- Quarterly_spt2 %>%
      activate("geometry") %>%
      mutate(nb = include_self(st_contiguity(geometry, queen = input$Contiguity3)),
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
      filter(DT == input$EHSAAdmin2) %>%
      select(DT, quarter, gi_star)
    
    p2 <- ggplot(data = filtered_df2, 
                 aes(x = quarter, 
                     y = gi_star)) +
      geom_line() +
      theme_light() +
      theme(axis.text.x = element_blank(), 
            plot.title = element_text(size = 10)) +
      ggtitle(paste("GI* Trends for District:", input$EHSAAdmin2))
    
    ggplotly(p2)
  })

  
  #For Mann Kendall Table 
  
  EHSADataMKTest2 <- reactive({
    df2 <- EHSAData2() 
    
    ehsa32 <- df2 %>%
      group_by(DT) %>%
      summarise(mk = list(
        unclass(
          Kendall::MannKendall(gi_star)))) %>%
      tidyr::unnest_wider(mk)
    
    return(ehsa32)
  })
  
  
  #For Mann Kendall Table only       
  
  #  output$MKtest2 <- renderDataTable({
  # Get the Mann-Kendall test results
  #    mkResults2 <- EHSADataMKTest2()
  
  #    mkResults2 <- mkResults2 %>%
  #      rename("District" = "DT")
  
  # Return the results to render them as a table
  #    mkResults2
  #  })
  
  
  
  #For Mann Kendall Table only 
  
  #  output$MKText <- renderText({ 
  #    "The Mann-Kendall test is a non-parametric statistical test used to identify trends 
  #      in a series of data. Its primary purpose is to determine whether there is a 
  #      monotonic trend over time in the observed data. 
  #      To view significant emerging hot/cold spots, users can sort 
  #      the tau & sl variables in descending order."
  
  #  })  
  
  
  
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
  
  output$AnovaText <- renderText({ 
    "Analysis of Variance (ANOVA) is a statistical method used to test differences between two or more means.
    It is similar to the t-test, but the t-test is generally used for comparing two means, 
    while ANOVA is used when comparing more than two means.
    
    ANOVA is based on comparing the variance (or variation) between the data samples to the variation within each particular sample.
    If the between-group variance is high and the within-group variance is low, 
    this provides evidence that the means of the groups are significantly different.
    
    In the options above, users can select the confidence interval, test type, p-adjust method and pair type. Filters available
    would be the year of interest and event type." 
  })
  
  
  #Mosaic Plot
  
  #  MosaicResults <- reactive({
  # Filter the data based on the user's selection
  #    filteredData <- Region_Summary %>%
  #      filter(year == input$YearMosaic) 
  
  
  #    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data    
  
  #    return(filteredData)  
  
  #  })        
  
  
  #  output$Mosaicplot <- renderPlotly({
  
  #    dataForMosaic <- MosaicResults()  
  
  #    if(is.null(dataForMosaic)) return()  # Check if the data is NULL and exit if it is
  
  #    gg5 <- ggplot(dataForMosaic) +
  #      geom_mosaic(aes(weight = Total_Fatalities,
  #                      x = product(event_type, country), fill = admin1)) +
  #      labs(x = "Myanmar",
  #           fill = "Regions") +
  #      theme(
  #        axis.text.x = element_blank(),
  #        axis.title.y = element_blank(),
  #        axis.ticks.x = element_blank()
  #      )
  
  # Converting the ggplot object to a plotly object
  #    ggplotly(gg5)
  #  })
  
  
  # VCD mosaic
  MosaicResults2 <- eventReactive(input$Mosaic2Update, {
    # Filter the data based on the user's selection
    filteredData <- ACLED_MMR_Mosaic  %>%
      filter(year == input$YearMosaic2, admin1 == input$Option1, event_type == input$Option2, Has_Fatalities == input$Option3)
    #filter(admin1 == input$Option1) %>%
    #filter(event_type == input$Option2) %>%
    #filter(Has_Fatalities == input$Option3)
    
    
    
    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data    
    
    return(filteredData)
    
    
  })        
  
  
  
  output$Mosaicplot2 <- renderPlot({
    
    dataForMosaic2 <- MosaicResults2()  
    
    if(is.null(dataForMosaic2)) return()  # Check if the data is NULL and exit if it is
    
    Mosaic2 <- vcd::mosaic(~ admin1 + event_type + Has_Fatalities, data = dataForMosaic2, gp = shading_Friendly, 
                           labeling = labeling_border(labels = TRUE, varnames = FALSE, 
                                                      rot_labels = c(90, 0, 0, 0), 
                                                      just_labels = c("left", "left", "center", "right")))
    Mosaic2
    
  })
  
  output$Mosaic2Text <- renderText({ 
    "A mosaic plot is a visualisation tool used to discover the association between two or more variables. 
    In the case above, a comparison of 3 variables is made - Region, Event Type and Fatalities.
    The first split on the left would divide the regions on the horizontal plane.
    The second split at the top would divide the event type on the vertical plane.
    The third split would divide each region plane into 2 based on whether the events have fatalities or not on the horizontal plane.
    
    The size of each tile represents the proportions of observations in the region.
    The colour of each tile would represent the magnitude of the residual where red tiles indicate significant negative residual where frequency
    is less than expected and blue tiles indicate positive significant positive residuals where frequency is greater than expected.
    The intensity of the colour represents the magnitude of the residuals as shown on the legend on the right."
  })
  
  
  
  #==========================================================
  # END of Confirmatory Analysis Module
  #==========================================================
  
  
  #==========================================================
  # START of ACLED Datatable Module
  #==========================================================
  output$datatable_ACLED1 <- DT::renderDataTable({
    afinal <- final %>%
      select(everything())
    
    DT::datatable(
      afinal, 
      class = "compact",
      filter = "top", 
      extensions = c("Buttons"),
      options = list(pageLength = 5,
                    columnDefs = list(
                      list(targets = c(1:7, 9, 11:14, 16:23, 26), className = "dt-center"), 
                      list(targets = c(8, 10, 15:17, 24, 25), visible = FALSE)), 
                     buttons = c("colvis", "excel", "csv", "print"),
                       dom = "Bpiltf",
                     scrollX = TRUE,
                     scrolly = TRUE)
    )
  })
  #==========================================================
  # END of ACLED Datatable Module
  #==========================================================s
  
}
# Run the app
shinyApp(ui = ui, server = server)