

library(ggthemes)
library(extrafont)
library(ggrepel)    
library(gganimate)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(tidytext)
library(tm)
library(wordcloud)
library(wordcloud2)
library(shinyWidgets)
library(tableHTML)
library(shinyjs)
library(shinyalert)
library(plotly)
library(maps)
library(googleVis)


load('wine_data.RData')

#defining choices for the Select Country drop-down menu:

choices <- c('All', 'US', 'Italy', 'France', 'Spain', 'Portugal', 
             'Chile', 'Austria', 'Australia', 'Germany', 'Argentina')

ui <- dashboardPage(skin ='red',
                    
                    dashboardHeader(title = "Choose Your Wine"),
                    dashboardSidebar( 
                      
                      #creating tabs on the left:
                      sidebarMenu(
                        tags$style(HTML(".main-sidebar { font-size: 13px; background-color: #242C34}")),
                        menuItem("Introduction", tabName = "intro", icon = icon("book")),
                        menuItem("Fact File", tabName = "facts", icon = icon("glass")),
                        menuItem("Wine Comparison", tabName = "wine", icon = icon("signal")),
                        menuItem("Choose Your Wine", tabName = "search", icon = icon("search")),
                        menuItem("Wine Title", tabName = "taste", icon = icon("heart")),
                        menuItem("US States View", tabName = 'maps', icon = icon('globe-americas'))
                        
                      ),
                      
                      column(12, h4("Select Wine Parameters", style = "font-family: 'Helvetica';font-weight: 700; line-height: 2.1; 
                                                                       color: 'white'; text-align: centre;")),  
                      chooseSliderSkin('Flat'),
                      setSliderColor(c('Tomato', 'Tomato', 'Tomato') , c(1,2,3)),
                      
                      #defining sliders: 
                      sliderInput("price", h4("Price", style = "font-family: 'Helvetica'; font-weight: 700; line-height: 2.1; color: 'white'"),                        
                                  min = 10, max = 50, 
                                  value = c(10, 50), step = 5, sep = ""),
                      sliderInput('points', h4('Score', style = "font-family: 'Helvetica'; font-weight: 700; line-height: 2.1; color: 'white'"),
                                  min =  80, max = 100,
                                  value = c(80, 100), step = 1),
                      sliderInput('year', h4('Year', style = "font-family: 'Helvetica'; font-weight: 700; line-height: 2.1;  color: 'white'"),
                                  min =  1999, max = 2017,
                                  value = c(1999, 2017), step = 1),
                      selectInput ('myselect', h4('Select Country', style = "font-family: 'Helvetica'; font-weight: 700; line-height: 2.1;  color: 'white'"),
                                   choices =  choices, multiple = TRUE, selected = 'All')
                    ),
                    
                    dashboardBody(
                      tags$head(
                        tags$link(rel = 'stylesheet', type ='text/css', href = 'bootstrap.min.css')
                      ),
                      tabItems(
                        tabItem( tabName = 'intro', 
                                 fluidRow(
                                   
                                   #using HTML code to create text in the first tab:
                                   box(
                                     tags$style(".box { background-color: #242C34 !important; color: #f8f3f2 !important; }"),  
                                     tags$style(".box-header { color: #f8f3f2 !important; font-weight: bold : align = 'center'}"),
                                     h1('***Welcome to Wine Application***', style = 'text-align: center'),
                                     tags$br(),
                                     tags$p ('Hi! My name is Ekaterina Galin and I am a great wine lover. 
                                             I know how difficult it might be to choose a bottle of wine as the choice in Europe is really wide.',
                                             style = 'font-size: 15px;text-align: center'),
                                     tags$p ('That is why I created an application that helps wine lovers choose a bottle of wine that best suits them.',
                                             style = 'font-size: 15px;text-align: center'),
                                     tags$p ('Choose Your Wine Tab will help you find the bottle of wine that suits your requirements. All you need to do is to select price range, year, score and country in Wine Parametres.
                                             The application will do the rest.', 
                                             style = 'font-size: 17px; text-align: center; font-weight: bold; color: #dc4c34;' ),
                                     tags$p ('More information about wines presented in this application you can get by clicking on the tabs on the left-hand panel.
                                             ', style = 'font-size: 17px; text-align: center; font-weight: bold; color: #dc4c34;' ),
                                     tags$p ('Please pay attention to the fact that the prices of wine do not exceed 
                                             $50, so all the wines, which could be found with the help of this application, are reasonably priced and affordable', 
                                             style = 'font-size: 15px;text-align: center'),
                                     tags$p ('Have fun and add me on LinkedIn!', style = 'font-size: 15px;text-align: center'), width = 12,
                                     tags$br(),
                                     
                                     #inserting the image:
                                     HTML('<center><img src="shiny.jpg" width="250"></center>'),
                                     
                                     #inserting the hyperlink:
                                     tags$style(".fa-linkedin {color:#FF4b2b}"),
                                     tags$li(tags$a(href="https://www.linkedin.com/in/ekaterinagalin/", target="_blank", 
                                                    icon("linkedin", "fa-3x", lib = "font-awesome")  
                                     ),
                                     ),
                                     
                                   )
                                 ),
                        ),
                        
                        tabItem(tabName = "facts",
                                tabPanel('Number of Bottles',
                                         tags$style(".small-box { background-color: #242c34 !important; color: #f8f3f2 !important; }"),
                                         infoBoxOutput("box1"),
                                         infoBoxOutput("box2"),
                                         infoBoxOutput('box3'),
                                         infoBoxOutput('box4'),
                                         infoBoxOutput('box5'),
                                         infoBoxOutput('box6')),
                                tabBox(
                                  height = "520px", width = 12,
                                  tabPanel('Overview',
                                           tags$style (type='text/css', 
                                                       '.nav-tabs {font-size: 13px'),
                                           box(title = "Top Wine Producing Countries", width=6,solidHeader = TRUE,
                                               tabPanel("Top Wine Producing Countries",
                                                        plotOutput("plot", height = 400))),
                                           box(title = "Biggest Wine Producers", width=6,solidHeader = TRUE,
                                               tabPanel("Biggest Wine Producers", 
                                                        plotOutput("plot3", height = 400)))),
                                  
                                  tabPanel('Most Popular Wine Varieties',
                                           box(title= 'Most Popular Wine Varieties',  width=12,solidHeader = TRUE,
                                               tabPanel("Most Popular Wine Varieties", 
                                                        plotOutput("plot2", height = 400)))),
                                  
                                  tabPanel('Number of Bottles by Year',
                                           box(title = 'Number of Bottles by Year', width=12,solidHeader = TRUE,
                                               plotOutput('plot7', height = 400))))
                        ),
                        
                        tabItem(tabName = "wine",
                                tabPanel('test',
                                         tags$style(".small-box { background-color: #242c34 !important; color: #f8f3f2 !important; }"),
                                         infoBoxOutput("box7"),
                                         infoBoxOutput("box8"),
                                         infoBoxOutput("box9"),
                                         infoBoxOutput("box10"),
                                         infoBoxOutput("box11"),
                                         infoBoxOutput('box12')),
                                
                                tabBox(
                                  height = "520px", width = 12,
                                  tabPanel('Score & Price',
                                           tags$style (type='text/css', 
                                                       '.nav-tabs {font-size: 13px'),
                                           box(title ='Average Score by Country',width=6,solidHeader = TRUE,
                                               tabPanel('Score by Country',
                                                        plotOutput('plot4', height = 300))),
                                           box(title = 'Average Price by Country', width=6,solidHeader = TRUE,
                                               tabPanel('Price by Country',
                                                        plotOutput('plot5', height = 300)))),
                                  tabPanel('Price vs Score Overview',
                                           box(title = 'Price vs Score Overview', width=12,solidHeader = TRUE,
                                               plotOutput('plot8', height = 450))),
                                  #tabPanel('Price vs Score by Country',
                                           #plotOutput('plot6', height = 500)),
                                  tabPanel('Top Rated Wineries & Varieties',
                                           tags$style (type='text/css', 
                                                       '.nav-tabs {font-size: 13px'),
                                           tags$style(HTML(" .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, 
                                         .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate 
                                         .paginate_button.current:hover { color: #ffffff;}
                                         .dataTables_wrapper .dataTables_paginate 
                                         .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none 
                                         !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}
                                          .dataTables_length select {
                                          color: #000000; background-color: #000000}
                                           .dataTables_filter input {color: #000000; background-color: #0E334A} 
                                                           thead {color: #ffffff;} tbody {color: #000000;}"
                                           )),
                                           box(title = "Top Rated Wineries", width=6,solidHeader = TRUE,
                                               tabPanel("Top Rated Wineriess",
                                                        DT::dataTableOutput('pt2'))),
                                           box(title = "Top Rated Varieties", width=6,solidHeader = TRUE,
                                               tabPanel("Top Rated Varieties", 
                                                        DT::dataTableOutput('pt1'))))
                                )),
                        
                        tabItem(tabName = "search",
                                tags$style (type='text/css', 
                                            '.box{font-size: 13px'),
                                box(
                                  width = 12, background = "black",
                                  'Here you can choose the bottle of wine that suits you best.
                                  Just choose preferred price range, vintage, wine score
                                and origin on the left-hand pannel. The value column will help you assess the price/ score ratio of each bottle.
                                  The higher the value (max - 9), the better price/ quality ratio of the wine bottle.'
                                ),
                                tabBox( height = "620px", width = 12,
                                        tabPanel('Choose Your Wine',
                                                 box(title = 'Choose Your Wine', width = 12, solidHeader = TRUE, 
                                                     tabPanel ('Choose',
                                                               DT::dataTableOutput("pt"))
                                                     
                                                 )))),
                        
                        tabItem(tabName = 'taste',
                                tags$style (type='text/css', 
                                            '.box{font-size: 13px'),
                                box(width = 12, background = "black",solidHeader = TRUE,
                                    'Let us see which words are most frequently used in wine titles.'),
                                tabBox( height = "720px", width = 12,
                                        tabPanel('Wine Title',
                                                 box (title = 'Wine Title', width =12,solidHeader = TRUE,
                                                      tabPanel ('Word Cloud',
                                                                plotOutput ('wordcloud')))))),
                     tabItem(tabName ='maps',
                                
                                tabBox(height = "620px", width = 12,
                                       tabPanel('US Map Overview', 
                                                box(width=12, solidHeader = TRUE,
                                                    radioButtons(
                                                      "us_map_type",
                                                      label = ("Select map content:"),
                                                      choices = list("Number of wines by state" = "wps",
                                                                     "Number of varieties by state" = "nvs", 
                                                                     "Average price by state" = "aps", 
                                                                     "Average score by state" = "ascs"), 
                                                      selected = "wps"),     
                                                    box(width = 12, height = "100%", htmlOutput("us_map"))))))
                     
                   
                        
                      )
                    ))

server <- function(input, output, session){
  
  observe({
    if('All' %in% input$myselect)
      selected_choices = choices [-1]
    else
      selected_choices = input$myselect
    updateSelectInput (session, 'myselect', selected = selected_choices)
  })
  
  data <- reactive({                
    wine_dataset <- wine_data %>% 
      
      filter(price >= input$price[1], 
             price <= input$price[2],
             points >= input$points[1],
             points <= input$points[2],
             year >= input$year[1],
             year <= input$year[2],
             country %in% input$myselect) 
    return(wine_dataset)
  })
  
  output$selected <- renderText({
    paste (input$myselect, collapse =',')

  })
  
  #creating value boxes:  
  output$box1 <- renderInfoBox({
    wine_dataset <- data()
    prop <- 
      infoBox(
        value = sum(wine_dataset$country=="US"),
        title =  " Bottles produced in US",
        icon = icon("wine-glass", lib = "font-awesome"),
        color = "red",fill = TRUE,
        width = 4  
      )
  })
  
  output$box2 <- renderInfoBox({
    wine_dataset <- data()
    infoBox(
      value = sum(wine_dataset$country=="France"), 
      title =  "Bottles produced in France",
      icon = icon("wine-glass", lib = "font-awesome"),
      color = "red", 
      width = 4  
    )
  })
  
  output$box3 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = sum(wine_dataset$country=="Italy"), 
      title =  "Bottles produced in Italy",
      icon = icon("wine-glass", lib = "font-awesome"),
      color = "red", fill = TRUE,
      width = 4  
    )
  })
  
  output$box4 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = sum(wine_dataset$country=="Spain"), 
      title =  "Bottles produced in Spain",
      icon = icon("wine-glass", lib = "font-awesome"),
      color = "red", fill = TRUE,
      width = 4  )
  })
  
  output$box5 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = sum(wine_dataset$country=="Portugal"), 
      title =  "Bottles produced in Portugal",
      icon = icon("wine-glass", lib = "font-awesome"),
      color = "red",
      width = 4  )
  })
  
  output$box6 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = sum(wine_dataset$country=="Chile"), 
      title =  "Bottles produced in Chile",
      icon = icon("wine-glass", lib = "font-awesome"),
      color = "red", fill = TRUE,
      width = 4  )
  })
  
  output$box7 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = round(mean(wine_dataset$price)), 
      title =  "Average Price ($)",
      icon = icon("dollar-sign", lib = "font-awesome"),
      color = "red", 
      width = 4 )
  })
  
  output$box8 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = min(wine_dataset$price), 
      title =  "Minimum Price ($)",
      icon = icon("dollar-sign", lib = "font-awesome"),
      color = "red", fill = TRUE,
      width = 4 )
  })
  
  output$box9 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = max(wine_dataset$price), 
      title =  "Maximum Price ($)",
      icon = icon("dollar-sign", lib = "font-awesome"),
      color = "red", 
      width = 4 )
  })
  
  
  output$box10 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = round(mean(wine_dataset$points)), 
      title =  "Average Score",
      icon = icon("bullseye", lib = "font-awesome"),
      color = "red", 
      width = 4 )
  })
  
  output$box11 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = min(wine_dataset$points), 
      title =  "Minimum Score",
      icon = icon("bullseye", lib = "font-awesome"),
      color = "red", fill = TRUE,
      width = 4 )
  })
  
  output$box12 <- renderValueBox({
    wine_dataset <- data()
    infoBox(
      value = max(wine_dataset$points), 
      title =  "Maximum Score",
      icon = icon("bullseye", lib = "font-awesome"),
      color = "red", 
      width = 4 )
  })
  
  output$pt <- DT::renderDataTable({data() %>% 
      #mutating and adding a new column to the table. The value column represents score / price ratio of each bottle in the dataset.
      mutate (value = round(points/price))%>%
      select (country, price, points, value, year, title, variety, winery) %>%
      arrange (desc(points))
  })
  
  output$pt1 <- DT::renderDataTable({data() %>% group_by(variety) %>% 
      filter (n() > 100) %>%
      summarize(
        Average = round(mean(points),0),
        Min= min(points),
        Max = max(points),
        N_bottles = n()) %>% 
      arrange (desc(Average)) %>%
      rename (Variety = variety)
  }, options = list(   
    lengthMenu = list(c(5, 7, 99), c('5', '7', '9')),   
    pageLength = 5
  ))
  
  output$pt2 <- DT::renderDataTable({data() %>% group_by(winery) %>% 
      filter (n() > 60) %>%
      summarize(
        Average = round(mean(points),0),
        Min= min(points),
        Max = max(points),
        N_bottles = n()) %>% 
      arrange (desc(Average)) %>%
      rename (Winery = winery)
  }, options = list(   
    lengthMenu = list(c(5, 7, 99), c('5', '7', '9')),   
    pageLength = 5  
  ))
  
  colourCount <- (12) 
  getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
  
  output$plot <- renderPlot ({
    data() %>% 
      group_by(country) %>% 
      #filter(n() > 800) %>% 
      mutate(count = n()) %>%
      ggplot(aes(x = reorder(country, count), fill = country)) + 
      geom_bar() +
      coord_flip() + scale_fill_manual(values = colorRampPalette(brewer.pal(12, 
                                                                            "Reds"))(colourCount))  +
      theme_minimal() + theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0.5, #vjust = 0.5, 
                                                                                  color = '#51120A', face = 'bold'), axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15, vjust = 1.0)) +
      labs(x = 'Country', y = 'Number of Bottles' #title = "Top World Wine Producers"
      )
  }, height = 400)
  
  
  colourCount1 <- (17)
  
  output$plot2 <- renderPlot ({
    data() %>%
      group_by(variety) %>% 
      filter(n() > 1500) %>% 
      mutate(count = n()) %>%
      ggplot(aes(x = reorder(variety, count), fill = variety)) + 
      geom_bar() + 
      coord_flip() + scale_fill_manual(values = colorRampPalette(brewer.pal(17, 
                                                                            "Reds"))(colourCount1)) + theme_minimal() + 
      theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0.5, vjust = 1.0, color = '#51120A', face = 'bold'), axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15, vjust = 1.0)) +
      labs(x = 'Wine Variety', y = 'Number of Bottles') #title = "Most Popular Wine Varieties") 
  }, height = 400)
  
  output$plot3 <- renderPlot ({
    data() %>%
      group_by(winery) %>% 
      filter(n() > 90) %>% 
      mutate(count = n()) %>%
      ggplot(aes(x = reorder(winery, count), fill = country)) + 
      geom_bar() + 
      coord_flip() + scale_fill_manual(values = colorRampPalette(brewer.pal(17, 
                                                                            "RdGy"))(colourCount)) + 
      theme_minimal() + theme(plot.title = element_text(size = 20, hjust = 0.5, vjust = 1.0, color = '#51120A', face = 'bold'), 
                              axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15, vjust = 1.0)) +
      labs(x = 'Wine Producer', y = 'Number of Bottles') 
  }, height = 400)
  
  output$plot4 <- renderPlot ({
    data() %>%
      group_by(country) %>% 
      filter(n() > 1000, price <500) %>% 
      mutate(count = n()) %>%
      ggplot (aes ( x = country, y = points, fill = country)) + geom_boxplot(outlier.shape = NA) + coord_flip() + 
      #scale_fill_brewer(palette="Reds") + labs ( x= 'Country', y = 'Points') + theme(legend.position = "none")})
      scale_fill_manual(values = colorRampPalette(brewer.pal(12, 
                                                             "Reds"))(colourCount)) +
      theme(axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15, vjust = 1.0)) +
      labs(x = 'Country', y = 'Average Score') +
      theme(legend.position = "none") })
  
  output$plot5 <- renderPlot ({
    data() %>%
      group_by(country) %>% 
      filter(n() > 1000, price <50) %>% 
      mutate(count = n()) %>%
      
      ggplot (aes ( x = country, y = price, fill = country)) + geom_boxplot(outlier.shape = NA) + coord_flip() + 
      #scale_fill_brewer(palette="Reds") + labs ( x= 'Country', y = 'Price') + 
      scale_fill_manual(values = colorRampPalette(brewer.pal(12, 
                                                             "Reds"))(colourCount)) +
      theme(axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15, vjust = 1.0)) +
      labs(x = 'Country', y = 'Average Price') +
      theme(legend.position = "none") })
  
  
  farb <- c("#B80F0A", "#891D1B", '#D21F3C', "#C48DA8", "#81737A", '#844D68', "#FA8072", "#0B6623", '#891B50', "#679267")
  output$plot6 <- renderPlot ({
    data() %>%
      filter(points > 90, points < 100, price < 100) %>%
      ggplot(aes(price, points, color = country)) +
      geom_point(alpha = 0.5) +
      geom_count() +
      theme_minimal() + 
      labs(x = 'Price', y = 'Score', title = "Price Vs Ratings") +
      coord_flip() + facet_grid(country ~ . ) + theme(legend.position = "none") + theme(
        strip.text.x = element_text(
          size = 12, face = "bold"
        ),
        strip.text.y = element_text(
          size = 12, face = "bold")
      ) + scale_color_manual(values = farb)})
  
  tidy_wine <- reactive({
    custom_stop_words <- tribble(
      ~word, ~lexicon,
      'drink', "CUSTOM",'flavors', "CUSTOM",'red', 'CUSTOM',
      'dry', "CUSTOM",'palate', "CUSTOM",'character', 'CUSTOM',
      'ready', 'CUSTOM','notes', "CUSTOM",'black', "CUSTOM",
      'finish', "CUSTOM",'wine', 'CUSTOM','texture', 'CUSTOM',
      'structure', "CUSTOM",'aging', "CUSTOM",'age', "CUSTOM",
      'nose', 'CUSTOM', 'aromas', "CUSTOM", 'white', 'CUSTOM',
      'blend', 'CUSTOM', 'offers', "CUSTOM", 
     'touch', 'CUSTOM',
      'dark', "CUSTOM", 'de', 'CUSTOM', 
      'clean', 'CUSTOM', 'la', 'CUSTOM',
      'style', "CUSTOM", 'di', 'CUSTOM', 
      '2007', "CUSTOM", '2008', 'CUSTOM', '2009', "CUSTOM", 
      '2013', 'CUSTOM', '2014', 'CUSTOM',
      '2015', "CUSTOM", '2016', 'CUSTOM', '2017', "CUSTOM", 
      '2010', 'CUSTOM', '2011', 'CUSTOM',
      '2012', "CUSTOM", '2006', 'CUSTOM', 
      '2005', "CUSTOM", 'wa', 'CUSTOM', 'valley', "CUSTOM", 
      'del', 'CUSTOM')

    stop_words2 <- stop_words %>% 
      bind_rows(custom_stop_words)
    
    tidy_wine <- wine_data %>% 
      unnest_tokens(word, title) %>%
      anti_join(stop_words2) %>%
      filter(price >= input$price[1], 
             price <= input$price[2],
             points >= input$points[1],
             points <= input$points[2],
             year >= input$year[1],
             year <= input$year[2],
             country %in% input$myselect) 
    return(tidy_wine)
  })
  
  output$wordcloud <- renderPlot  ({
    cloud_data <- tidy_wine() %>% count(word) %>% filter(n > 1500)
    wordcloud(words = cloud_data$word,
              freq = cloud_data$n, min.freq = 30,
              max.words = 50, random.order = FALSE, rot.per = 0.30,
              colors = brewer.pal(8, 'Dark2'))
  })
  
  output$plot7 <- renderPlot ({
    data() %>%
      filter (year > 2002, year < 2016, country != 'US' ) %>%
      group_by(country, year) %>%
      summarise (number = n()) %>%
      ggplot (aes(x=factor(year), y = number, color = country)) + geom_line(aes(group = country)) + geom_point() +
      theme_minimal() + scale_color_manual(values=farb) +#scale_x_continuous(labels= lablist) +
      theme(plot.title = element_text(size = 20, hjust = 0.5, vjust = 1.0, color = '#51120A', face = 'bold'), axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15, vjust = 1.0)) +
      labs(x = 'Year', y = 'Number of Bottles') 
    
  })
  
  output$plot7 <- renderPlot ({
    data() %>%
      filter (year > 2002, year < 2016,country != 'US' ) %>%
      group_by(country, year) %>%
      summarise (number = n()) %>%
      ggplot(aes(x=factor(year), y = number, color = country)) + geom_line(aes(group = country), size = 0.7) + geom_point() +
      theme_minimal() + scale_color_manual(values=farb) +#scale_x_continuous(labels= lablist) +
      
      theme(plot.title = element_text(size = 20, hjust = 0.5, vjust = 1.0, color = '#51120A', face = 'bold'), axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15, vjust = 1.0)) +
      labs(x = 'Year', y = 'Number of Bottles') 
    
  })
  
  farb2 <- c('#891B50', "#B80F0A", '#770F28', "#891D1B", '#D21F3C', "#C23210", "#FF8A83", '#871325', "#FA8072", '#D65F59' )
  output$plot8 <- renderPlot ({
    data() %>% 
      group_by(country) %>%
      summarize (
        average = mean(price),
        avg_score = mean(points),
        number = n()) %>%
      ggplot(aes(x = average, y = avg_score)) +  
      #geom_text_repel(aes(label = country)) + 
      scale_size_continuous(range= c(5,30)) +
      geom_point(aes(size = number, color = country), alpha = 0.7)  +
      geom_text(size = 4.5, aes(label=country, vjust = 2, hjust = 1.2)) + 
      geom_smooth(method = "lm", se = FALSE, col = "#731F66", linetype="dashed") + 
      scale_color_tableau() +
      #theme(legend.position="bottom") +  
      scale_color_manual(values=farb2) +
      theme(legend.position = "none", plot.title = element_text(size = 20, hjust = 0.5, vjust = 1.0, color = '#51120A', face = 'bold'), axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15, vjust = 1.0)) +
      labs(x = 'Average Price', y = 'Average Score')
    
  })
  
  #df's for US map 
  
  wines_df_us <- wine_data %>% filter(country == 'US') 
  wines_df_wps <- as.data.frame(wines_df_us %>%  group_by(province) %>% summarise(number = n())) 
  wines_df_nvs <- as.data.frame(wines_df_us %>% group_by(province) %>% summarise(number = n_distinct(variety)))   
  wines_df_aps <- as.data.frame(wines_df_us %>% na.omit (price) %>%group_by(province) %>% summarise(avg_price = round(mean(price))))
  wines_df_apts <- as.data.frame(wines_df_us %>%  group_by(province) %>% summarise(avg_score = round(mean(points))))
  observe({ 
    
    
    #map view by US state: 
    if (input$us_map_type == 'wps'){
      output$us_map <- renderGvis({
        us_map <- gvisGeoChart(wines_df_wps, 'province', 'number', 
                               options = list(region = 'US', displayMode="regions", resolution="provinces", width = 1000, height = 500, colorAxis = "{colors:[ '#EADB9F', '#284475', 'purple', '#800020']}"))
      })
    }
    
    else if (input$us_map_type == 'nvs'){
      output$us_map <- renderGvis({
        us_map <- gvisGeoChart(wines_df_nvs, 'province', 'number', 
                               options = list(region = 'US', displayMode="regions", resolution="provinces", width = 1000, height = 500, colorAxis = "{colors:[ '#EADB9F', '#284475', 'purple', '#800020']}"))
      })
    }
    
    else if (input$us_map_type == 'aps'){
      output$us_map <- renderGvis({
        us_map <- gvisGeoChart(wines_df_aps, 'province', 'avg_price',
                               options = list(region = 'US', displayMode="regions", resolution="provinces", width = 1000, height = 500, colorAxis = "{colors:[ '#EADB9F', '#284475', 'purple', '#800020']}"))
      })
    }
    
    else if (input$us_map_type == 'ascs'){
      output$us_map <- renderGvis({
        us_map <- gvisGeoChart(wines_df_apts, 'province', 'avg_score', 
                               options = list(region = 'US', displayMode="regions", resolution="provinces", width = 1000, height = 500, colorAxis = "{colors:[ '#EADB9F', '#284475', 'purple', '#800020']}"))
      })
    }
  })
}  

shinyApp(ui = ui, server = server)





