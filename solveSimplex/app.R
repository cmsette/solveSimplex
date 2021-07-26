library(shiny)

ui <- fluidPage(
    # Application title
    titlePanel("solveSimplex Game Theory Solver"),
    # Game size selection
    fluidRow(
        selectInput("n", "How many strategies are in this system?", c("2 strategies"=2, "3 strategies"=3, "4 strategies"=4, "5 strategies"=5), selected=2)
    ),
    # Variable W-matrix
    uiOutput("Wentry")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$vars1 <- renderText({ input$vars1 })
    output$vars2 <- renderText({ input$vars2 })
    output$vars3 <- renderText({ input$vars3 })
    output$vars4 <- renderText({ input$vars4 })
    output$vars5 <- renderText({ input$vars5 })
    output$Wentry <- renderUI({
        if(input$n == 2){
            fluidRow(column(1, br(), br(), br(), h4("Payoffs"),
                            verbatimTextOutput("vars1", placeholder = T), verbatimTextOutput("vars2", placeholder = T)),
                     column(1, textInput("vars1", "Strategy 1", value="Rock"),
                            numericInput("W1", label="________________", value=0, min=NA, max=NA), numericInput("W6", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars2", "Strategy 2", value="Paper"),
                            numericInput("W2", label="________________", value=0, min=NA, max=NA), numericInput("W7", label=NULL, value=0, min=NA, max=NA))
            )
        } else if(input$n == 3){
            fluidRow(column(1, br(), br(), br(), h4("Payoffs"),
                            verbatimTextOutput("vars1", placeholder = T), verbatimTextOutput("vars2", placeholder = T), 
                            verbatimTextOutput("vars3", placeholder = T)),
                     column(1, textInput("vars1", "Strategy 1", value="Rock"),
                            numericInput("W1", label="________________", value=0, min=NA, max=NA), numericInput("W6", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W11", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars2", "Strategy 2", value="Paper"),
                            numericInput("W2", label="________________", value=0, min=NA, max=NA), numericInput("W7", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W12", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars3", "Strategy 3", value="Scissors"),
                            numericInput("W3", label="________________", value=0, min=NA, max=NA), numericInput("W8", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W13", label=NULL, value=0, min=NA, max=NA))
                     )
        } else if(input$n == 4){
            fluidRow(column(1, br(), br(), br(), h4("Payoffs"),
                            verbatimTextOutput("vars1", placeholder = T), verbatimTextOutput("vars2", placeholder = T), 
                            verbatimTextOutput("vars3", placeholder = T), verbatimTextOutput("vars4", placeholder = T)),
                     column(1, textInput("vars1", "Strategy 1", value="Rock"),
                            numericInput("W1", label="________________", value=0, min=NA, max=NA), numericInput("W6", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W11", label=NULL, value=0, min=NA, max=NA), numericInput("W16", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars2", "Strategy 2", value="Paper"),
                            numericInput("W2", label="________________", value=0, min=NA, max=NA), numericInput("W7", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W12", label=NULL, value=0, min=NA, max=NA), numericInput("W17", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars3", "Strategy 3", value="Scissors"),
                            numericInput("W3", label="________________", value=0, min=NA, max=NA), numericInput("W8", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W13", label=NULL, value=0, min=NA, max=NA), numericInput("W18", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars4", "Strategy 4", value="Lizard"),
                            numericInput("W4", label="________________", value=0, min=NA, max=NA), numericInput("W9", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W14", label=NULL, value=0, min=NA, max=NA), numericInput("W19", label=NULL, value=0, min=NA, max=NA))
                )
        } else if(input$n == 5){
            fluidRow(column(1, br(), br(), br(), h4("Payoffs"),
                            verbatimTextOutput("vars1", placeholder = T), verbatimTextOutput("vars2", placeholder = T), 
                            verbatimTextOutput("vars3", placeholder = T), verbatimTextOutput("vars4", placeholder = T), verbatimTextOutput("vars5", placeholder = T)),
                     column(1, textInput("vars1", "Strategy 1", value="Rock"),
                            numericInput("W1", label="________________", value=0, min=NA, max=NA), numericInput("W6", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W11", label=NULL, value=0, min=NA, max=NA), numericInput("W16", label=NULL, value=0, min=NA, max=NA), numericInput("W21", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars2", "Strategy 2", value="Paper"),
                            numericInput("W2", label="________________", value=0, min=NA, max=NA), numericInput("W7", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W12", label=NULL, value=0, min=NA, max=NA), numericInput("W17", label=NULL, value=0, min=NA, max=NA), numericInput("W22", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars3", "Strategy 3", value="Scissors"),
                            numericInput("W3", label="________________", value=0, min=NA, max=NA), numericInput("W8", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W13", label=NULL, value=0, min=NA, max=NA), numericInput("W18", label=NULL, value=0, min=NA, max=NA), numericInput("W23", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars4", "Strategy 4", value="Lizard"),
                            numericInput("W4", label="________________", value=0, min=NA, max=NA), numericInput("W9", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W14", label=NULL, value=0, min=NA, max=NA), numericInput("W19", label=NULL, value=0, min=NA, max=NA), numericInput("W24", label=NULL, value=0, min=NA, max=NA)),
                     column(1, textInput("vars5", "Strategy 5", value="Spock"),
                            numericInput("W5", label="____________", value=0, min=NA, max=NA), numericInput("W10", label=NULL, value=0, min=NA, max=NA),
                            numericInput("W15", label=NULL, value=0, min=NA, max=NA), numericInput("W20", label=NULL, value=0, min=NA, max=NA), numericInput("W25", label=NULL, value=0, min=NA, max=NA))
                )
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
