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
library(comtradr)
#devtools::install_github("ropensci/comtradr")


if ('2017.csv' %in% list.files('data')) {
  
    df <- read_csv('data/201712.csv') 
    
  } else {
    
    names <- countrycode::codelist$un.name.en %>%
      .[!is.na(.)] %>%
      ct_country_lookup(.) %>%
      .[!grepl('Fmr|excl.', ., ignore.case = TRUE)] %>%
      unique
    
    n <- floor(length(names) / 5) + 1
    split_by = as.vector(sapply(1:n, rep_len, length.out = 5))[1:length(names)]
    
    df <- lapply(split(names, split_by), 
                 function(n){
                   ct_search(reporters = n,
                             partners = 'All',
                             trade_direction = "exports", 
                             freq = 'annual', 
                             start_date = '2017', 
                             end_date = '2017',
                             commod_codes = '3915')
                 }) %>% ldply
  }

df <- filter(df, partner_code != 0) %>%
  mutate_at(c('reporter', 'partner'), 
            funs(ifelse(grepl('china', ., ignore.case = TRUE), 'China', .))) %>%
  group_by(reporter, partner) %>%
  summarise(netweight_kg = sum(netweight_kg, na.rm = TRUE)) %>%
  ungroup

nodes <- data.frame(name = unique(c(df$reporter, df$partner)), 
                    group = 1,
                     size = 1) %>%
  mutate(group = ifelse(name == 'China', 1, 2))

links <- df %>%
  select(source = reporter,
         target = partner,
         value = netweight_kg) %>%
  mutate_if(is.character, funs(as.numeric(factor(., levels = nodes$name)) - 1)) %>%
  mutate(value = value / 100000)

forceNetwork(Links = links, Nodes = nodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderUI({
      # generate plot based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

