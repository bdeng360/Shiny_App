#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Regrex1 Scatter Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            actionButton("go", "Linear Regression"),
            
            tags$hr(),
            
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            tags$hr(),
            
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           
           textOutput("statistics"),
           
           plotOutput("distPlot_lm"),
           
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
# Make plot reactive to go button
    
    button <- eventReactive(input$go, {
        linmod <- lm(dataInput()$y~dataInput()$x)
        abline(linmod)
    })
    
    output$distPlot <- renderPlot({
        button()
    })
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
        
    })
    
    output$statistics <- renderPrint({
        linmod <- lm(dataInput()$y~dataInput()$x)
        summary(linmod)
        
    })
    
    output$distPlot_lm <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
        linmod <- lm(dataInput()$y~dataInput()$x)
        rmse <- round(sqrt(mean(resid(linmod)^2)), 2)
        coefs <- coef(linmod)
        b0 <- round(coefs[1], 2)
        b1 <- round(coefs[2],2)
        r2 <- round(summary(linmod)$r.squared, 2)
        eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
                          r^2 == .(r2) * "," ~~ RMSE == .(rmse))
        abline(linmod)
        text(1, 14, eqn, pos = 4)
    })
        
    }
    


# Run the application 
shinyApp(ui = ui, server = server)
