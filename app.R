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
library(visNetwork)

source('rss_read.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("International Movement of Waste Plastic"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectizeInput('plot_type', 'Select Plot type', 
                        choices = list('Force Network' = 'forceNetwork',
                                       'Chord Diagram' = 'chordDiagram',
                                       'Vis.js Network' = 'visNetwork')),
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
   
  ## google alerts text (migrate to db later)
  new_ga <- google_alert() %>%
    mutate_at(vars(published, updated), lubridate::as_datetime)
  
  if ('galerts.csv' %in% list.files('data')) {
    ga <<- read_csv('data/galerts.csv') %>%
      bind_rows(new_ga) %>%
      distinct()
  } else {
    ga <<- new_ga
  }
  
  write.csv(ga, 'data/galerts.csv', row.names = FALSE)
  
  input_date <- reactive({input$date})
  dat <- reactiveValues(df = data.frame())
  
  names <- countrycode::codelist$un.name.en %>%
    .[!is.na(.)] %>%
    ct_country_lookup(.) %>%
    .[!grepl('Fmr|excl.', ., ignore.case = TRUE)] %>%
    unique
  
  ## save list of world coords
  if ('world.csv' %in% list.files('data')) {
    wld <- read_csv('data/world.csv')
  } else {
    wld <- read.csv('http://worldmap.harvard.edu/download/wfs/34645/csv?outputFormat=csv&service=WFS&request=GetFeature&format_options=charset%3AUTF-8&typename=geonode%3Acountry_centroids_az8&version=1.0.0')
    write.csv(wld, 'data/world.csv')
  }
  
  # match to comtrade names (currently error on Austria)
  coords <- data.frame(cnames = names) %>%
    rowwise() %>%
    mutate(wld_name = agrep(cnames, wld$formal_en, value = TRUE)[1])
  
  ## get data from UN Comtrade
  observe({
    
    inputfile <- paste0(gsub('-', '', input_date()), '.csv')
    print(input_date())
    
    if (inputfile %in% list.files('data')) {
      print('inside standard read section')
      df <- read_csv(sprintf('data/%s', inputfile)) 
      
    } else {
      
      withProgress(message = sprintf('Getting Data for %s from UN Comtrade', 
                                     input_date()), {
        
        n <- floor(length(names) / 5) + 1
        split_by = as.vector(sapply(1:n, rep_len, length.out = 5))[1:length(names)]
        
        #tryCatch(
        df <- lapply(split(names, split_by), 
                     function(nm){
                       incProgress(1/n)
                       Sys.sleep(1)
                         ct_search(reporters = nm,
                                   partners = 'All',
                                   trade_direction = "exports", 
                                   freq = 'monthly', 
                                   start_date = input_date(), 
                                   end_date = input_date(),
                                   commod_codes = '3915')
                     }) %>% ldply
        #)
        
        write.csv(df, sprintf('data/%s', inputfile))
        
        #df <- df
        
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
    
    nodes <<- data.frame(name = unique(c(df$reporter, df$partner)), 
                        group = 1,
                        stringsAsFactors = FALSE) %>%
      mutate(group = ifelse(name == 'China', 1, 2),
             id = 1:nrow(.) - 1,
             label = name) %>%
      left_join(size)
    
    links <<- df %>%
      select(from = reporter,
             to = partner,
             value = netweight_kg) %>%
      mutate_if(is.character, funs(as.numeric(factor(., levels = nodes$name)) - 1)) %>%
      mutate(value = value / 100000)
    
    dat$df <- df
    dat$size <- size
    dat$nodes <- nodes
    dat$links <- links
    dat$names <- names
    
  })
 


  
   output$plot <- renderUI({
      # generate plot based on input$bins from ui.R
      if (input$plot_type == 'forceNetwork') {
        tagList(h3('Force Network'),
                forceNetwork(Links = dat$links, Nodes = dat$nodes,
                             Source = "from", Target = "to",
                             Value = "value", NodeID = "id",
                             Group = "group", opacity = 0.8,
                             fontSize = 14, Nodesize = 'size',
                             zoom = TRUE, arrows = TRUE)
                )
      } else if (input$plot_type == 'chordDiagram') {
        tagList(h3('Chord Diagram'),
                renderPlot(height = 800,
                           chordDiagram(select(dat$df, 
                                               reporter,
                                               partner,
                                               netweight_kg),
                                        directional = 1,
                                        direction.type = c("diffHeight", "arrows"), link.arr.type = "big.arrow"
                                        )
                           )
                )
      } else if (input$plot_type == 'visNetwork') {
        tagList(h3('Vis.js Network'),
                visNetwork(dat$nodes, dat$links, height = '800px', width = '100%') %>%
                  #visEdges(smooth = FALSE) %>% 
                  visEdges(arrows = "to") %>%
                  visOptions(#selectedBy = "group", 
                             highlightNearest = TRUE, 
                             nodesIdSelection = TRUE)
        )
      } else {
        tagList(tags$h2('please select a plot type'))
      }
     
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

