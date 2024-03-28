# load R packages
pacman::p_load(shiny, shinydashboard, shinycssloaders, 
               tidyverse, dplyr, leaflet, plotly, 
               ggthemes, fresh, sf, sfdep, tmap, tm, 
               ggraph, igraph, tidytext, DT, spatstat,
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
# ExploreSubTabs
#==========================================================  
ExploreSubTabs <- tabsetPanel(
  tabPanel("Overview", 
           ExploreOverviewrow1
  ),
  tabPanel("Geospatial Exploration", 
           ExploreGeospatialrow1
  )
  #tabPanel("Trends", 
  #ExploreTrendrow1,
  #ExploreTrendrow2
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
             helpText("Options for LISA Analysis"),
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
             selectInput("MoranEventType", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Riots" = "Riots"),
                         selected = "Battles"),
#         ),
#         box(title = "Options for Local Moran's I",    # a seperate filter box (KIV)
#             status = "info",
#             solidHeader = FALSE,
#             width = NULL,
             selectInput("MoranWeights", "Spatial Weights Style",
                         choices = c("W: Row standardised" = "W",
                                     "B: Binary" = "B",
                                     "C: Globally standardised" = "C",
                                     "U: C / no of neighbours" = "U",
                                     "minmax" = "minmax",
                                     "S: Variance" = "S"),
                         selected = "W"),
             selectInput("MoranSims", "Number of Simulations:",
                         choices = c(99, 199, 299, 399, 499),
                         selected = 99),
            actionButton("MoranUpdate", "Update Plot"),
            hr(),
             radioButtons(inputId = "MoranConf",
                          label = "Select Confidence level",
                          choices = c("0.95" = 0.05, 
                                      "0.99" = 0.01),
                          selected = 0.05,
                          inline = TRUE),
            selectInput("localmoranstats", "Select Local Moran's Stat:",
                         choices = c("local moran(ii)" = "local moran(ii)",
                                     "expectation(eii)" = "expectation(eii)",
                                     "variance(var_ii)" = "variance(var_ii)",
                                     "std deviation(z_ii)" = "std deviation(z_ii)",
                                     "P-value" = "p_value"),
                         selected = "local moran(ii)"),
             selectInput("LisaClass", "Select Lisa Classification",
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
             withSpinner(plotOutput("LocalMoranMap", height = "700px", width = "100%"))
         )
  ),
  column(4,
         box(title = "Local Indicator of Spatial Association (LISA) map",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             withSpinner(plotOutput("Lisa", height = "700px", width = "100%"))
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
                      column(12,
                             box(title = "Local Moran's I - All Districts",
                                 status = "danger",
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 width = NULL,
                                 align = "center",
                                 withSpinner(dataTableOutput("localMoransTable1")),
                                 style = "height:600px; overflow-y: scroll; overflow-x: scroll;"))
                      #column(12,       
                      #       box(
                      #         title = "LISA results (P-values < 0.05)",
                      #         status = "danger",
                      #         solidHeader = TRUE,
                      #         collapsible = TRUE,     # table with just isolated significant vals (KIV)
                      #         width = NULL,
                      #         align = "center",
                      #         dataTableOutput("localMoransTable2"),
                      #         style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                      #)
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
             helpText("Options for Hot Spot Analysis"),
             selectInput("GIQtr" , "Year-Quarter",
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
             selectInput("GIEventType", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence",
                                     "Riots" = "Riots"),
                         selected = "Battles"),
             selectInput(inputId = "GISims",
                         label = "Number of Simulations for Gi*:",
                         choices = c(99,199,299,399,499),
                         selected = 99),
             actionButton("GIUpdate", "Update Plot"),
             hr(),
             radioButtons(inputId = "GIConf",
                          label = "Select Confidence level",
                          choices = c("0.95" = 0.05, 
                                      "0.99" = 0.01),
                          selected = 0.05,
                          inline = TRUE),
             selectInput("localgistats", "Select Local GI Stat:",
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
             ,withSpinner(plotOutput("Gistarmap", height = "700px", width = "100%"))
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
           withSpinner(plotOutput("HotColdmap", height = "700px", width = "100%"))
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
                      column(12,
                             box(
                               title = "GI* Statistics - All Districts",
                               status = "danger",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               width = NULL,
                               align = "center",
                               withSpinner(dataTableOutput("GiStat")),
                               style = "height:500px; overflow-y: scroll;overflow-x: scroll;"))
                      #column(12,       
                      #        box(
                      #            title = "GI* Statistics - Significant Hot & Cold spots (P-values < 0.05)",
                      #            status = "danger",
                      #            solidHeader = TRUE,
                      #            collapsible = TRUE,      # table with just isolated significant vals (KIV)
                      #            width = NULL,
                      #            align = "center",
                      #            dataTableOutput("GiStat2"),
                      #            style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                      #  )
                    )
                    
)

#==========================================================  
##Geospatial Analysis, 3rd Tab
#==========================================================  

#==========================================================  
##Emerging Hot Spot Analysis
#==========================================================  


EHSA2 <- fluidRow(
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Options for Emerging Hot Spot Analysis"),
             selectInput("EHSAEventType", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence"),
                         selected = "Battles"),
             selectInput(inputId = "EHSANumLags", 
                         label = "Time Lag of spatial neighbours:", 
                         choices = c(1, 2, 3, 4, 5),
                         selected = 1),
             selectInput(inputId = "EHSANumSims", 
                         label = "Number of Simulations:", 
                         choices = c(99, 199, 299, 399, 499),
                         selected = 99),
             actionButton("EHSAUpdate", "Update Plot"),
             hr(),
             radioButtons(inputId = "EHSAConf",
                          label = "Select Confidence level",
                          choices = c("0.95" = 0.05, 
                                      "0.99" = 0.01),
                          selected = 0.05,
                          inline = TRUE),
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
             withSpinner(plotOutput("EHSAmap", height = "700px"))
         )
  ), 
  column(6,
         box(title = "Distribution of EHSA classes",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             withSpinner(plotlyOutput("EHSAbar", height = "200px"))
         )  
  ),
  column(4,
         box(title = "GI* Trends per district",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             withSpinner(plotlyOutput("Giplot2", height = "400px"))
         )
  ),
  column(2,
         box(title = "Analysis Period: 2021-2023, Quarterly",
             status = "info",
             solidHeader = FALSE,
             width = NULL,
             helpText("Options for Trend Plot"),
             selectInput("EHSAEventType2", "Event Type:",
                         choices = c("Battles" = "Battles",
                                     "Violence against civilians" = "Violence against civilians",
                                     "Protests" = "Protests",
                                     "Explosions/Remote violence" = "Explosions/Remote violence"),
                         selected = "Battles"),
             selectizeInput(inputId = "EHSAAdmin2",
                            label = "Select District",
                            choices = unique(Space_2$DT),
                            multiple = FALSE),
             actionButton("EHSAUpdate2", "Update Plot")
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("GITrend2Text")
         )
  ),
  column(8,
         box(title = "Emerging Hot Spot Analysis results",  # or "Mann Kendall Test results"
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             align = "left",
             withSpinner(dataTableOutput("MKtest2"))
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
             #                multiple = TRUE,                # insert input admin 1 (KIV)
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
             actionButton("AnovaUpdate", "Update Plot"),
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
             #             label = "Plot Type",        #change plot type (KIV)
             #           choices = c("box" = "box", 
             #                      "violin" = "violin",
             #                     "boxviolin" = "boxviolin"),
             #        selected = "box"),
             hr(),
             radioButtons(inputId = "Conlevel",
                          label = "Confidence level",
                          choices = c("0.95" = 0.95, 
                                      "0.99" = 0.99),
                          selected = 0.95)
            # actionButton("AnovaUpdate", "Update Plot")   #change position of Update plot
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
             withSpinner(plotOutput("Anovaplot", height = "700px"))
         )
  )
)

## for GG mosaic version (KIV)

#Confirm2 <- fluidRow(
#  column(2,
#         box(title = "Analysis Period: 2020-2023",
#             status = "info",
#             solidHeader = FALSE,
#             width = NULL,
#             helpText("Filter Options for Dataset"),
#             selectInput(inputId = "YearMosaic",
#                         label = "Year:",
#                         choices = seq(2020,2023),
#                         selected = 2023)

#         ),
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

## VCD Mosaic

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
                         selected = 2023),
             actionButton("Mosaic2Update", "Update Plot")
             
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
             withSpinner(plotOutput("Mosaicplot2", height = "700px"))
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

# seperate sidebar tab for emerging hot spot analytis (KIV)
#ESHASubTabs <- tabsetPanel(
#tabPanel("Gi* trend and Mann Kendall test", 
#EHSA1),
# tabPanel("Emerging Hot Spot Analysis", 
#         EHSA2)

#)


ConfirmSubTabs <- tabsetPanel(
  tabPanel("One-Way Anova Test", 
           Confirm1),
  #tabPanel("Mosaic Plot",
  #         Confirm2),
  tabPanel("Visualising Categorical Data",
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
            
            ClusterSubTabs 
    ),
    #3rd tab content
    #tabItem(tabName = "EHSA",  #seperate tab set for EHSA (KIV)
    
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
  # END of Exploratory Module
  #==========================================================
  
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
      mutate(nb = st_contiguity(geometry),
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
      mutate(nb = st_contiguity(geometry),
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
    
    The Hot & Cold spot map plots significant areas where p-value < 0.05 or 0.01." 
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
      theme(axis.title.y = element_blank())
    
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
  
    output$MKtest2 <- renderDataTable({
      
      EHSATable <- EHSAMapdata()
      if(is.null(df)) return()
  
  
      ehsa_sig3 <- EHSATable  %>%
        filter(p_value < as.numeric(input$EHSAConf)) 
  
    ehsa_sig3
    })
  
  # For explanation of table with significant vals only  
  
    output$MKText <- renderText({ 
      "The Mann-Kendall test determines whether there is a 
      monotonic trend over time in the observed data. The Gi* values for each location in each time period (time-slice) 
      is calculated. Next, the Mann-Kendall trend test is done to identify any temporal trend in these Gi* values. 
      
    This tables shows results for P-values < 0.05 or 0.01. Tau ranges between -1 and 1 where -1 is a perfectly decreasing series and 1 is a perfectly increasing series."
      
    })    
    
  
  
  EHSAData2 <- eventReactive(input$EHSAUpdate2,{
    space_data2 <- Space_2 %>%
      filter(event_type == input$EHSAEventType2)
    
    
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
      filter(DT == input$EHSAAdmin2) %>%
      select(DT, quarter, gi_star)
    
    p2 <- ggplot(data = filtered_df2, 
                 aes(x = quarter, 
                     y = gi_star)) +
      geom_line() +
      theme_light() +
      theme(axis.text.x = element_blank()) +
      ggtitle(paste("GI* Trends for District:", input$EHSAAdmin2))
    
    ggplotly(p2)
    
    
  })
  
  
  output$GITrend2Text <- renderText({ 
    "GI* trend plot shows changes in the Local Gi* per district, for each event type."
    
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
    #updateSelectizeInput(session, "Admin1_ggstat", selected = character(0))  #KIV for admin1 selection
    updateSelectizeInput(session, "event_ggstat", selected = character(0))  
  })
  
  AnovaResults <- eventReactive(input$AnovaUpdate,{
    # Filter the data based on the user's selection
    filteredData <- Summary_Data %>%
      filter(year == input$YearAnova,
             #admin1 == input$Admin1_ggstat,  #KIV for admin1 selection
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
                            #plot.type = input$PlotType,  #KIV for plot type change
                            conf.level = as.numeric(input$Conlevel),
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
  
  
  
  #GGMosaic (KIV)
  
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
  MosaicResults2 <- eventReactive(input$Mosaic2Update,{
    # Filter the data based on the user's selection
    filteredData <- ACLED_MMR %>%
      filter(year == input$YearMosaic2) 
    
    
    if(nrow(filteredData) == 0) return(NULL)  # Exit if no data    
    
    return(filteredData)  
    
  })        
  
  
  
  output$Mosaicplot2 <- renderPlot({
    
    dataForMosaic2 <- MosaicResults2()  
    
    if(is.null(dataForMosaic2)) return()  # Check if the data is NULL and exit if it is
    
    Mosaic2 <- vcd::mosaic(~ admin1 + event_type, data = dataForMosaic2, gp = shading_max, 
                           labeling = labeling_border(labels = TRUE, varnames = FALSE, 
                                                      rot_labels = c(90, 0, 0, 0), 
                                                      just_labels = c("left", "center", "center", "right")))
    
    
    Mosaic2
    
  })
  
  
  
  
  #==========================================================
  # END of Confirmatory Analysis Module
  #==========================================================
  
  
}
# Run the app
shinyApp(ui = ui, server = server)