# load R packages
pacman::p_load(shiny, shinydashboard, shinycssloaders, 
               tidyverse, dplyr, leaflet, leaflet.extras, plotly, 
               ggthemes, fresh, sf, sfdep, tmap, tm, 
               ggraph, DT, spatstat,
               lubridate,viridis, ggplot2, readr, purrr, ggstatsplot, 
               vcd, ggmosaic, forcats,
               ggridges, ggdist, highcharter)


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
    menuItem("Aspatial Analysis", tabName = "Aspatial", icon = icon("globe")),
    menuItem("Geospatial Analysis", tabName = "Cluster", icon = icon("circle-nodes")),
    menuItem("Confirmatory Analysis", tabName = "ConfirmatoryAnalysis", icon = icon("clipboard-check")),
    menuItem("ACLED Data Table", tabName = "ACLEDTable", icon = icon("table")),
    menuItem("Visit ACLED Data Source", icon = icon("send",lib='glyphicon'), 
             href = "https://acleddata.com/data-export-tool/"),
    menuItem("Return to Home Page", icon = icon("send",lib='glyphicon'), 
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


# left blank first



# =============================    
########### EXPLORATORY - END
# =============================

#==========================================================  
# AspatialSubTabs
#==========================================================  
AspatialSubTabs <- tabsetPanel(
  tabPanel("Overview", 
           #AspatialOverviewrow1,
           #AspatialOverviewrow2
  ),
  tabPanel("Aspatial Distribution Analysis", 
           #AspatialDistributionrow1,
           #AspatialDistributionrow2
  )
)


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
             radioButtons(inputId = "Contiguity1",
                          label = "Contiguity Method",
                          choices = c("Queen" = TRUE, 
                                      "Rook" = FALSE),
                          selected = "TRUE",
                          inline = TRUE),
              selectInput("MoranWeights", "Spatial Weights Style",
                         choices = c("W: Row standardised" = "W",
                                     "B: Binary" = "B",
                                     "C: Globally standardised" = "C",
                                     "U: C / no of neighbours" = "U",
                                     "minmax" = "minmax",
                                     "S: Variance" = "S"),
                         selected = "W"),
             sliderInput(inputId = "MoranSims", 
                         label = "Number of Simulations:", 
                         min = 99, max = 499,
                         value = 99, step = 100),
             actionButton("MoranUpdate", "Update Plot"),
             hr(),
             radioButtons(inputId = "MoranConf",
                          label = "Select Confidence level",
                          choices = c("0.95" = 0.05, 
                                      "0.99" = 0.01),
                          selected = 0.05,
                          inline = TRUE),
             selectInput("LisaClass", "Select Lisa Classification",
                         choices = c("mean" = "mean",
                                     "median" = "median",
                                     "pysal" = "pysal"),
                         selected = "mean"),
             selectInput("localmoranstats", "Select Local Moran's Stat:",
                         choices = c("local moran(ii)" = "local moran(ii)",
                                     "expectation(eii)" = "expectation(eii)",
                                     "variance(var_ii)" = "variance(var_ii)",
                                     "std deviation(z_ii)" = "std deviation(z_ii)",
                                     "P-value" = "p_value"),
                         selected = "local moran(ii)")
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
             radioButtons(inputId = "Contiguity2",
                          label = "Contiguity Method",
                          choices = c("Queen" = TRUE, 
                                      "Rook" = FALSE),
                          selected = "TRUE",
                          inline = TRUE),
             sliderInput(inputId = "GISims", 
                         label = "Number of Simulations:", 
                         min = 99, max = 499,
                         value = 99, step = 100),
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
             radioButtons(inputId = "Contiguity3",
                          label = "Contiguity Method",
                          choices = c("Queen" = TRUE, 
                                      "Rook" = FALSE),
                          selected = "TRUE",
                          inline = TRUE),
             sliderInput(inputId = "EHSANumLags", 
                         label = "Time Lag of spatial neighbours:", 
                         min = 1, max = 5,
                         value = 1),
             sliderInput(inputId = "EHSANumSims", 
                         label = "Number of Simulations:", 
                         min = 99, max = 499,
                         value = 99, step = 100),
             actionButton("EHSAUpdate", "Update Plot"),
             hr(),
             checkboxInput(inputId="ShowEHSA", label="Show EHSA classes", value=FALSE),
             conditionalPanel(condition="input.ShowEHSA",
                              withSpinner(plotlyOutput("EHSAbar", height = "200px"))
             ),
             checkboxInput(inputId="ShowGI", label="Show GI* trend plot", value=FALSE),
             conditionalPanel(condition="input.ShowGI",
                              selectizeInput(inputId = "EHSAAdmin2",
                                             label = "Select District",
                                             choices = unique(Space_2$DT),
                                             multiple = FALSE),
                              conditionalPanel(condition="input.EHSAAdmin2",
                                               withSpinner(plotlyOutput("Giplot2", height = "400px")))
             ),
             hr(),
             radioButtons(inputId = "EHSAConf",
                          label = "Select Confidence level",
                          choices = c("0.95" = 0.05, 
                                      "0.99" = 0.01),
                          selected = 0.05,
                          inline = TRUE)
         )
  ),
  column(4,
         box(title = "Emerging Hot Spot map",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             height = "800px",
             align = "left",
             withSpinner(plotOutput("EHSAmap", height = "730px"))
         ),
         box(title = "Chart Interpretation",
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             textOutput("EHSAText")
         )
  ), 
  column(6,
         box(title = "Emerging Hot Spot Analysis results",  # or "Mann Kendall Test results"
             status = "danger",
             solidHeader = TRUE,
             collapsible = TRUE,
             width = NULL,
             height = "800px",
             align = "left",
             withSpinner(DT::dataTableOutput("MKtest2")), 
             style = "height: 800px; overflow-y: scroll;"
         ),
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
             plotOutput("Mosaicplot2", height = "700px")
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



ConfirmSubTabs <- tabsetPanel(
  tabPanel("One-Way Anova Test", 
           Confirm1),
  #tabPanel("Mosaic Plot",
  #         Confirm2),
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
            #ACLEDDataTable
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
  # START of Aspatial Module
  # =============================
  
  #left blank for aspatial
  
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
    
    
    lisamap 
    
    
  })
  
  
# Local Morans's I Data Table in Cluster 2 
  
  # Render the data table for Local Moran's I results
  output$localMoransTable1 <- renderDataTable({
    df <- localMIResults()
    
    # Check if data is available
    if (is.null(df)) return()
    
    df
    
    
  })
  
  
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
    
    Gi_map 
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