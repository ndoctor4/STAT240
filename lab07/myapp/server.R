library(shiny)

# Define a server for the Shiny app
function(input, output) {
  # Fill in the spot we created for a plot
  output$PokePlot <- renderPlot({
    poke_tbl <- with(pokedata,table(pokedata[,input$characteristic]))
    # Render a barplot
    barplot(poke_tbl, 
            main=input$characteristic,
            ylab="Number of Pokemon",
            xlab="Characteristc")
  })
}