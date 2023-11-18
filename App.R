library(shiny)
library(tidyverse)
library(plotly)
library(bs4Dash)
library(readxl)
library(DT)
#library(KoboconnectR)
library(broom)
# library(lubridate)
library(thematic)
library(waiter)
library(readxl)
require(dplyr)
require(highcharter) #to plot amazing time series plots
require(tidyr)
library(fresh)


####### User interface ############################

my <- readRDS("my.rds")   # Reading clean Data set from the storage 

#------------------------------------Read Data Files

inflation <- read_excel("inflation.xls")

#--------------------------------------------------Data Manipulation-------------------
year<-c(1980:2022) #making a vector consisting of all years
year<-as.character(year)#converting to character type to use in gather()


inf<-inflation %>% gather(year,key = "Year",value="InflationRate")
inf<-na.omit(inf) #omitting NA values

names(inf)<-c("region","year","inflation")

inf$year<-as.integer(inf$year)

India<-filter(inf,region=="India")
India$inflation<-as.numeric(India$inflation)
India$year<-as.numeric(India$year)

China<-filter(inf,region=="China, People's Republic of")
Ger<-filter(inf,region=="Germany")
Japan<-filter(inf,region=="Japan")
US<-filter(inf,region=="United States")
EU<-filter(inf,region=="European Union")
UK<-filter(inf,region=="United Kingdom")
Fr<-filter(inf,region=="France")
uae<-filter(inf,region=="United Arab Emirates")

#------------------------------------------------------End of Data Manipulation-------------------------



#---------------------------------------------------------------Data Filter-------------
#defining character vectors for select inputs
country<-c("India","United States","Mexico","Canada","China, People's Republic of","Japan","Russian Federation","Germany","United Kingdom","European Union",
           "ASEAN-5","New Zealand","Australia","Netherlands","Luxembourg",
           "France","Qatar","United Arab Emirates","Saudi Arabia")

unions<-c("Major advanced economies (G7)","European Union","Emerging and Developing Europe","ASEAN-5","Commonwealth of Independent States",
          "Emerging and Developing Asia","Latin America and the Caribbean",
          "Middle East, North Africa, Afghanistan, and Pakistan")
#-----------------------------------------------------------------------------


#--------------------------------------------------------------------Snakeyplot------
library(dplyr)
data(diamonds, package = "ggplot2")

diamonds2 <- select(diamonds, cut, color, clarity)

data_to_sankey(diamonds2)

#-------------------Quantom mode
aapl <- quantmod::getSymbols("AAPL",
                             src = "yahoo",
                             from = "2020-01-01",
                             auto.assign = FALSE
)

#-----------------------------------App Icons--------------------------------------

# toast options
toastOpts <- list(
  autohide = TRUE,
  icon = "fas fa-home",
  close = FALSE,
  position = "bottomRight"
)

# color statuses
statusColors <- c(
  "gray-dark",
  "gray",
  "secondary",
  "navy",
  "indigo",
  "purple",
  "primary",
  "lightblue",
  "info",
  "success",
  "olive",
  "teal",
  "lime",
  "warning",
  "orange",
  "danger",
  "fuchsia",
  "maroon",
  "pink",
  "white"
)



#-----------------------------------End of App Icons-------------------------------
#-----DRF Page for Online Reporting
DRF_page <- tabItem(
            tabName = "dashboard",
        fluidRow(
          box(width = 12,
              title = "Dataset is been refleccting on this table from remote online database",
              elevation = 4,
              closable = TRUE, 
              background = "primary",
              status = "teal", 
              solidHeader = TRUE, 
              collapsible = TRUE,
              maximizable = TRUE,
              tabItem("rawdata", 
                      width = 10,
                      title = "Data Table",
                      DTOutput("dtable",  width = "80%", height = 200)
              )
          )
        ),
        box(width = 12,
            elevation = 4,
            closable = TRUE, 
            status = "success", 
            solidHeader = TRUE, 
            collapsible = TRUE,
            maximizable = TRUE,
            plotlyOutput("Paymentcon", width = "100%", height = 450)
        )
)


#' basic_cards_tab ----
basic_cards_tab <- tabItem(
  tabName = "cards",
  fluidRow(
    box(
      title =  h3("World's Inflation Rates",align="center") ,
      elevation = 4,
      closable = TRUE, 
      width = 6,
      status = "warning", 
      solidHeader = TRUE, 
      collapsible = TRUE,
      maximizable = TRUE,
      label = boxLabel(
        text = 1,
        status = "danger"
      ),
      dropdownMenu = boxDropdown(
        boxDropdownItem("refreence", href = "https://www.r-bloggers.com/2017/10/making-a-shiny-dashboard-using-highcharter-analyzing-inflation-rates/"),
        boxDropdownItem("Item with inputId", id = "dropdown_item2"),
        dropdownDivider(),
        boxDropdownItem("item 3", href = "#", icon = icon("table-cells"))
      ),
      highchartOutput("hc3")
    ),
    box(
      title =h3("Sankey chart using a Highcharter Package",align="center"), 
      elevation = 4,
      closable = TRUE, 
      width = 6,
      status = "info", 
      solidHeader = TRUE, 
      collapsible = TRUE,
      maximizable = TRUE,
      label = boxLabel(
        text = 1,
        status = "pink"
      ),
      dropdownMenu = boxDropdown(
        boxDropdownItem("my website", href = "https://am-datasolution.com"),
        boxDropdownItem("Item with inputId", id = "dropdown_item2"),
        dropdownDivider(),
        boxDropdownItem("item 3", href = "#", icon = icon("table-cells"))
      ),
      highchartOutput("hc4")
    )
  )
)

#' card API
StockP <- tabItem(
  tabName = "Stock",
  box(
    id = "mycard",
    title = "World's Inflation Rates", 
    closable = TRUE, 
    maximizable = TRUE,
    width = 12,
    status = "warning", 
    solidHeader = TRUE, 
    collapsible = TRUE,
    highchartOutput("hc5")
  )
)

#' Country
Country_trend <- tabItem(
  tabName = "country",
  column(12,
         
         box(selectInput("country",label="Select Country",choices=country),width = 12) 
         
  ),
  
  column(12,
         box(
           id = "mycard",
           title = h3("Country's Inflation Rates",align="center"), 
           closable = TRUE, 
           maximizable = TRUE,
           width = 12,
           status = "warning", 
           solidHeader = TRUE, 
           collapsible = TRUE,
           highchartOutput("hcontainer")
         )
  )
)


#' Country
Economic_region <- tabItem(
  tabName = "region",
  column(12,
         
         box(title = h3("Select Economic Region"),
           selectInput("region",label="Select Economic Region",choices=unions),width = 12) 
         
  ),
  
  column(12,
         box(
           id = "mycard",
           title = h3("Region's Inflation Rates",h6(strong(
             'source : by Anish Singh Walia in R bloggers'))), 
           closable = TRUE, 
           maximizable = TRUE,
           width = 12,
           status = "warning", 
           solidHeader = FALSE, 
           collapsible = TRUE,
           highchartOutput("hcontainer2")
         )
  )
)

About <- tabItem(tabName="about",
                 # h2("What is Inflation ?",style="text-align:center"),
                 # br(),
                 # br(),
                carousel(
                  id = "mycarousel",
                  carouselItem(
                 box(width=12,height="400px",
                     title = "About ?",
                     elevation = 4,
                     closable = TRUE, 
                     status = "warning", 
                     solidHeader = TRUE, 
                     collapsible = TRUE,
                     p(style="font-size:20px",strong("A-Musa Data-solution"),"Hello everyone welcomes to A musa data-solution, 
                       the whole of this dashboard is been developed using R completely without using any third-party library like angular, react or node.js 
I hereby acknowledging the R community, R-studio, David-Granjohn , Anish Singh Walia: for providing the learning resource I can’t do without your support"),

p(style="font-size:20px",strong("I want to said "),"a big thank you to the David Granjohn for the amazing package Bs4dash"),

p(style="font-size:20px",strong("The source code and dataset")," for the inflation rate is been taken form Anish Singh Walia in R bloggers")

                 )
                ),
                carouselItem(
                 box(width=12,height="400px",
                     title = "What is Inflation ?",
                     elevation = 4,
                     closable = TRUE, 
                     status = "olive", 
                     solidHeader = TRUE, 
                     collapsible = TRUE,
                     p(style="font-size:20px",strong("Inflation"),"rates are the general rate at which price of the goods and services 
                          within a particular economy are rising and the purchasing power of the currency 
                          is declining due to the highly priced goods. High inflation is definately not good for an economy 
                          because it will always reduce the value for money.In genral central banks of an ecomony tries to and work towards reducing 
                          the inflation rate and avoiding deflation."),
                     
                     p(style="font-size:20px",strong("Deflation"), "is opposite of inflation. Delfation occurs when the inflation rates become negetive or are below 0. Deflation is more harmful and dangerous for an economy because it means that the prices of goods and services are going to decrease. Now this sounds amazing for consumers like us. But what actually happens is that the demand of goods and services have declined over a long term of time. 
                            This directly indicates that a recession is on its way. This brings job losses , declining wages and a big hit to the stock portfolio. Deflation slows economy's growth. As prices fall , people defer(postpone) purchases in hope of a better lower price deal. Due to this companies and firms have to cut 
                            down the cost of their goods and products which directly affects the wages of the employees which have to be lowered.",
                       'source : by Anish Singh Walia in R bloggers'
                     ) 
                 )
                )
                )
              )   


userR <- tabItem(tabName = "userR",
  fluidRow(
  box(width = 6,
      title = h3("User Registration Form A"),
      elevation = 4,
      closable = TRUE,
      background = "pink",
      status = "teal",
      solidHeader = TRUE,
      collapsible = TRUE,
      maximizable = TRUE,
      tabItem("userform",
              width = 10,
              title = "Data Table",
              textInput("caption", "Name", "Name"),
              textInput("caption", "Address", "Address"),
              textInput("caption", "Nationality", "Nationality"),
              textInput("caption", "State", "State"),
              textInput("caption", "Occupation", "Occupation"),
              textInput("caption", "Tribe", "Tribe"),
              selectInput("gender", label = h6("Gender"),
                          choices = list("Male" = 1, "Female" = 2),
                          selected = 1),
              dateInput("date", label = h6("Date of birth"), value = "2014-01-01"),
              #submitButton("Save"),
              verbatimTextOutput("value")
      )
  ),

  box(width = 6,
      title = h3("User Registration Form B"),
      elevation = 4,
      closable = TRUE,
      background = "info",
      status = "navy",
      solidHeader = TRUE,
      collapsible = TRUE,
      maximizable = TRUE,
      tabItem("rawdata",
              width = 10,
              title = "Data Table",
              textInput("caption", "Name", "Name"),
              textInput("caption", "Address", "Address"),
              textInput("caption", "Nationality", "Nationality"),
              textInput("caption", "State", "State"),
              textInput("caption", "Occupation", "Occupation"),
              textInput("caption", "Tribe", "Tribe"),
              selectInput("gender", label = h6("Gender"),
                          choices = list("Male" = 1, "Female" = 2),
                          selected = 1),
              dateInput("date", label = h6("Date of birth"), value = "2014-01-01"),
              #actionButton("Save")
      )
  )
 )
)



#--------------------------------------------Use-Interface--------------------------------------

ui <- dashboardPage(
  preloader = list(html = tagList(spin_3k(), "Loading ..."), color = "#e9eceb"),
  freshTheme = create_theme(
    bs4dash_vars(
      navbar_light_color = "#bec5cb",
      navbar_light_active_color = "#FFF",
      navbar_light_hover_color = "#FFF"
    ),
    bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF",
      text_light = "#272c30"
    ),
    bs4dash_layout(
      main_bg = "#d1dfeb"
    ),
    bs4dash_sidebar_light(
      bg = "#272c30",
      color = "#bec5cb",
      hover_color = "#FFF",
      submenu_bg = "#272c30",
      submenu_color = "#FFF",
      submenu_hover_color = "#FFF"
    ),
    bs4dash_status(
      primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
    ),
    bs4dash_color(
      gray_900 = "#FFF", white = "#272c30"
    )
  ),
  # dark = FALSE,
  help = TRUE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  #dashboardHeader(title = "DRF-Dashboard"),
  header = dashboardHeader(
    title = dashboardBrand(
      title = "A-Musa-Dat Scientist",
      color = "primary",
      href = "https://am-datasolution.com/",
      #image = "C:/Users/Abdulrahaman/Desktop/BS4DashNew/bs4app/Bs4Dashkobo/www/mypi.jpg",
      opacity = 0.8
    ),
    fixed = TRUE,
    tooltip(
      title = "This toggles the right sidebar",
      placement = "bottom",
      actionButton(inputId = "controlbarToggle", label = "show right bar", class = "mx-2")
    ),
    popover(
      title = "Toggle button",
      content = "This toggle the left sidebar",
      placement = "bottom",
      # `data-trigger` = "hover",
      actionButton(inputId = "sidebarToggle", label = "exapnd left sidebar", class = "mx-2")
    ),
    rightUi = tagList(
      dropdownMenu(
        badgeStatus = "danger",
        type = "messages",
        messageItem(
          inputId = "triggerAction1",
          message = "message 1",
          from = "Divad Nojnarg",
          image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
          time = "today",
          color = "lime"
        )
      ),
      userOutput("user")
    ),
    leftUi = tagList(
      dropdownMenu(
        badgeStatus = "info",
        type = "notifications",
        notificationItem(
          inputId = "triggerAction2",
          text = "Error!",
          status = "danger"
        )
      ),
      dropdownMenu(
        badgeStatus = "info",
        type = "tasks",
        taskItem(
          inputId = "triggerAction3",
          text = "My progress",
          color = "orange",
          value = 10
        )
      )
    )
  ),
   dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    id = "sidebar",
    customArea = fluidRow(
      actionButton(
        inputId = "myAppButton",
        label = "For-Room",
        icon = icon("users"),
        width = NULL,
        status = "info",
        style = "margin: auto",
        dashboardBadge(textOutput("btnVal"), color = "success")
      )
    ),
    sidebarUserPanel(
      image = "(images/mypi-01.jpg, width=58)",
      name = "Welcome A-musa DS!"
    ),
    selectInput("v_State", "State", choices = my %>%
                  select(State) %>%
                  distinct() %>%
                  arrange(State) %>%
                  drop_na()),
                  
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      sidebarHeader("Cards"),
      menuItem(
        "Health-DRF",
        tabName = "dashboard",
        icon = icon("hospital")
      ),
      
      menuItem(
        "Home",
        tabName = "cards",
        icon = icon("home")
      ),
      menuItem(
        "Stock",
        badgeLabel = "New",
        badgeColor = "success",
        tabName = "Stock",
        icon = icon("chart-simple")
      ),
      menuItem(
        "Country",
        tabName = "country",
        icon = icon("id-card")
      ),
      menuItem(
        "region",
        tabName = "region",
        icon = icon("earth")
      ),
      menuItem(
        "About",
        tabName = "about",
        icon = icon("object-ungroup")
      ),
      menuItem(
        "Register From",
        tabName = "userR",
        icon = icon("chart-area")
      ),
      # sidebarHeader("Other boxes"),
      # menuItem(
      #   "Register From",
      #   tabName = "userR",
      #   icon = icon("suitcase")
      # ),
      
      #sidebarHeader("Colors"),
      
      # menuItem(
      #   "Colors",
      #   tabName = "colors",
      #   icon = icon("droplet")
      # ),
      
     # sidebarHeader("BS4 gallery"),
      menuItem(
        text = "Galleries",
        icon = icon("cubes"),
        startExpanded = FALSE,
        menuSubItem(
          text = HTML(
            paste(
              "Gallery 2",
              dashboardBadge(
                "!",
                position = "right",
                color = "success"
              )
            )
          ),
          tabName = "gallery2"
        )
      )
    )
   ),

  dashboardBody(
    tabItems(
      DRF_page,
      basic_cards_tab,
      StockP,
      Country_trend,
      Economic_region,
      About,
      userR
      #   value_boxes_tab,
      #   colors_tab,
      #   gallery_1_tab,
      #   gallery_2_tab
    )
  ),
  controlbar = dashboardControlbar(
    id = "controlbar",
    skin = "light",
    pinned = TRUE,
    overlay = FALSE,
    controlbarMenu(
      id = "controlbarMenu",
      type = "pills",
      controlbarItem(
        "Skin",
        skinSelector()
      )
    )
  ),
  footer = dashboardFooter(
    fixed = FALSE,
    left = a(
      href = "https://am-datasolution.com",
      target = "https://am-datasolution.com", "@A-musa Data-solution"
    ),
    right = "2022"
  ),
  title = "Dashboard Showcase"
)
     tabItem("dashboard",
             fluidRow(box(
               width = 10,
               title = "Health Facilities",
               plotlyOutput("Paymentcon", width = "100%", height = 450)
             )
    )
   )
   
   
server <- function(input, output, session){
    output$dtable <- renderDT(server = FALSE,
                              {
          datatable(my <- readRDS("my.rds")%>% select(State, Operation.Unit, LGA, Health.Facilities, # cleaning the downloaded data set 
                                                Amount.per.Facility.in.a.Month, 
                                                Does.the.Facility.received.payment.from.NHQ.) %>% 
                      rename(Operation = Operation.Unit, HFacility = Health.Facilities, 
                             Amount = Amount.per.Facility.in.a.Month,
                             Received = Does.the.Facility.received.payment.from.NHQ.),
                    saveRDS(my,"my.rds"), # saving the clean data set.
    caption = 'Submitted Data by ATO ED:for each Health Facility per Month',
    rownames= T,
    filter = 'top',
    editable = F,
    extensions = 'Buttons',
    options = list(searching = TRUE,
                   scrollX = TRUE,
                   dom = '1Bfrtip',
                   buttons = c('pageLength', 'copy', 'excel', 'pdf', 'print'),
                   extend = "csv", text = " Download all data", filename = "data",
                   exportOptions = list( modifier = list(page = "all")
                                         
                   )
    )
    )
  })
  
    output$Paymentcon <- renderPlotly({my <- readRDS("my.rds") # Reading the data set for plot
           ggplotly( 
             Cdata <-my %>% 
               filter(State == input$v_State ) %>%
             ggplot(
              aes(x = HFacility, fill = Received)) +
              geom_bar(position = "fill") +
              scale_fill_hue(direction = 1) +
              labs(
                x = "Healthcare Facilities",
                y = " Month of the Year",
                title = "DRF Bill Payment for Healthcare Facilities")+
              coord_flip() +
              ggthemes::theme_base())
    })
    
    observeEvent(input$current_tab, {
      if (input$current_tab == "cards") {
        showModal(modalDialog(
          title = "Hello visitor welcomes to A-musa data-solution, 
          the whole of this dashboard is been developed using R 
          completely without using any third-party library like angular, react or node.js.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    
    # current theme info ---------------------------------------------------------
    
    observeEvent(input$dark_mode, {
      toast(
        title = if (input$dark_mode) "Dark theme on!" else "Light theme on",
        options = list(position = "topRight", class = "bg-warning", autohide = TRUE)
      )
    })
    
    #---------- Update right sidebar
    observeEvent(input$controlbarToggle, {
      updateControlbar(id = "controlbar")
    })
    
    
    #----- Update left sidebar
    observeEvent(input$sidebarToggle, {
      updateSidebar(id = "sidebar")
    })
    
    ######------------highcharter---------------opotion
    
    output$hc3<-renderHighchart({
      world<-inf %>% filter(region=="World")
      world$year<-as.numeric(world$year)
      world$inflation<-as.numeric(world$inflation)
      #plotting the plot
      hchart(world,hcaes(x=year,y=inflation),type="area",color="#ffffoo") %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#add8e6",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Time series plot of Inflation Rates for World",align="center") %>%
        hc_subtitle(text="Data Source: IMF",align="center") %>%
        hc_add_theme(hc_theme_elementary())
      
    })
    
    
    output$hc4<-renderHighchart({
      hchart(data_to_sankey(diamonds2), "sankey", name = "diamonds") %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Sankey plot",align="center") %>%
        hc_subtitle(text="Data Source: highchater inbuild",align="center") %>%
        hc_add_theme(hc_theme_elementary())
      
    })
    
    
    output$hc5<-renderHighchart({
      highchart(type = "stock") %>%
        hc_title(text = "AAPLE") %>%
        hc_add_series(aapl, yAxis = 0, showInLegend = FALSE) %>%
        hc_add_yAxis(nid = 1L, title = list(text = "Prices"), relative = 2) %>%
        hc_add_series(aapl[, "AAPL.Volume"], yAxis = 1, type = "column", showInLegend = FALSE) %>%
        hc_add_yAxis(nid = 2L, title = list(text = "Volume"), relative = 1) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Plot prices and volume with relative height",align="center") %>%
        hc_subtitle(text="Data Source: Apple",align="center") %>%
        hc_add_theme(hc_theme_elementary())
      
    })
    
    output$hcontainer <- renderHighchart ({
      
      #if(input$country==inf$region)
      #{
      df<-inf %>% filter(region==input$country)#making is the dataframe of the country
      
      df$inflation<-as.numeric(df$inflation)
      df$year<-as.numeric(df$year)
      
      #plotting the data
      hchart(df, "line",color="#DC270C",hcaes(x=year,y=inflation))  %>%
        
        hc_exporting(enabled = TRUE) %>% 
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Time series plot of Inflation Rates",align="center") %>%
        hc_subtitle(text="Data Source: IMF",align="center") %>%
        hc_add_theme(hc_theme_elementary()) 
      #to add 3-d effects
      #hc_chart(type = "column",
      #options3d = list(enabled = TRUE, beta = 15, alpha = 15))
      
    })  
    
    output$hcontainer2<-renderHighchart({
      
      union<-inf %>% filter(region==input$region)
      union$year<-as.numeric(union$year)
      union$inflation<-as.numeric(union$inflation)
      
      #plotting
      hchart(union,hcaes(x=year,y=inflation),type="area",color="#2B1F97") %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Time series plot of Inflation Rates for Economic Unions",align="center") %>%
        hc_subtitle(text="Data Source: IMF",align="center") %>%
        hc_add_theme(hc_theme_elementary())
      
      
      
      
    })
    
    
}

shinyApp(ui, server)
