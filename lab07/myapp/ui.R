library(shiny)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Pokemon Characteristics"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("characteristic", "Characteristic:", 
                  choices=colnames(pokedata)),
      hr(),
      helpText("Data of Pokemon from Generation 1-6")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("PokePlot")  
    )
    
  )
)