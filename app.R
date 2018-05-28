#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(magrittr)
library(plyr)
library(dplyr)
library(htmltools)
library(comtradr)
#devtools::install_github("ropensci/comtradr")
library(networkD3)
library(circlize)



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("International Movement of Waste Plastic"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectizeInput('plot_type', 'Select Plot type', 
                        choices = list('Force Network' = 'forceNetwork',
                                       'Chord Diagram' = 'chordDiagram')),
         selectizeInput('date', 'Select Month',
                        choices = format(seq.Date(as.Date('2016-01-01'), 
                                                           Sys.Date(), 'month'), '%Y-%m'),
                        selected = '2017-12'
                        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        uiOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  input_date <- reactive({input$date})
  
  ## get data from UN Comtrade
  observe({
    
    inputfile <- paste0(gsub('-', '', input_date()), '.csv')
    
    if (inputfile %in% list.files('data')) {
      
      df <- read_csv(sprintf('data/%s', inputfile)) 
      
    } else {
      
      withProgress(message = sprintf('Getting Data for %s from UN Comtrade', 
                                     input_date()), {
        
        names <- countrycode::codelist$un.name.en %>%
          .[!is.na(.)] %>%
          ct_country_lookup(.) %>%
          .[!grepl('Fmr|excl.', ., ignore.case = TRUE)] %>%
          unique
        
        n <- floor(length(names) / 5) + 1
        split_by = as.vector(sapply(1:n, rep_len, length.out = 5))[1:length(names)]
        
        df <- lapply(split(names, split_by), 
                     function(nm){
                       incProgress(1/n)
                       ct_search(reporters = nm,
                                 partners = 'All',
                                 trade_direction = "exports", 
                                 freq = 'monthly', 
                                 start_date = input_date(), 
                                 end_date = input_date(),
                                 commod_codes = '3915')
                     }) %>% ldply
        
        write.csv(df, sprintf('data/%s', inputfile))
        
      })

      
    }
    
    
    
    df <- filter(df, partner_code != 0) %>%
      mutate_at(c('reporter', 'partner'), 
                funs(ifelse(grepl('china', ., ignore.case = TRUE), 'China', .))) %>%
      group_by(reporter, partner) %>%
      summarise(netweight_kg = sum(netweight_kg, na.rm = TRUE)) %>%
      ungroup
    
    
    # larger importers will have larger radii
    size <- df %>%
      group_by(partner) %>%
      summarise(size = sum(netweight_kg, na.rm = T)) %>%
      rename(name = partner) %>%
      mutate(size = log2(size),
             size = ifelse(is.na(size), 1, size))
    
    nodes <- data.frame(name = unique(c(df$reporter, df$partner)), 
                        group = 1) %>%
      mutate(group = ifelse(name == 'China', 1, 2)) %>%
      left_join(size)
    
    links <- df %>%
      select(source = reporter,
             target = partner,
             value = netweight_kg) %>%
      mutate_if(is.character, funs(as.numeric(factor(., levels = nodes$name)) - 1)) %>%
      mutate(value = value / 100000)
    
  })
 
  

  
   output$plot <- renderUI({
      # generate plot based on input$bins from ui.R
      if (input$plot_type == 'forceNetwork') {
        print(input$plot_type)
        
        tagList(h3('Force Network'),
                forceNetwork(Links = links, Nodes = nodes,
                             Source = "source", Target = "target",
                             Value = "value", NodeID = "name",
                             Group = "group", opacity = 0.8,
                             fontSize = 14, Nodesize = 'size',
                             zoom = TRUE, arrows = TRUE)
                )
      } else if (input$plot_type == 'chordDiagram') {
        tagList(h3('Chord Diagram'),
                renderPlot(height = 800,
                           chordDiagram(select(df, 
                                               reporter,
                                               partner,
                                               netweight_kg))
                           )
                )
      } else {
        tagList(tags$h2('please select a plot type'))
      }
     
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

