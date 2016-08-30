#
# Author: Ron Collins
# 
# Date: 8/19/2016
# 
# Purpose:  Shiny application to satisfy the project requirement for the Data Science spelication-
#   Developing Data Projects

library(shiny)

# Define UI for application that selects random samples
ui <- fluidPage(
    # Application title
    titlePanel("Randomization of Treatments for Field/greenhouse Research"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( "Data input for treatment randomization",
            helpText(" This app will produce a table of randomly located treatments."),
            helpText(" This is important for agricultural research.  Randomizing of treatments among "),
            helpText(" the experimental units is frequently not very vigorous."),
            helpText(" This shiny app provides an easy method for randomizing treatments."),
            helpText("Enter the total number of rows and columns in the expriment layout."),
            helpText("Enter the total number of treatments to be applied."),
            helpText("Enter the total number of replicates per treatment."),
            helpText("NOTE: The number of treatments applied cannot exceed the number of experimental units."),
            
            numericInput(inputId = "matrixWidth" , label = "Number of column in experiment layout:",  value = 2, 
                         min = 1, max = NA, step = NA, width = NULL),
            numericInput(inputId = "matrixLength" , label = "Number of rows in experiment layout:",  value = 2,
                         min = 1, max = NA, step = NA, width = NULL),
            numericInput(inputId = "noTreatments" , label = "Number of Treatments in experiment:",  value = 2, 
                         min = 1, max = NA, step = NA,width = NULL),
            numericInput(inputId = "noRepicates" , label = "Number of Replicates per treatment:",  value = 2,
                         min = 1, max = NA, step = NA, width = NULL),
            helpText("Entered the experiment information and then Submit. "),

            submitButton("Submit")
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(

            h3('Layout of Randomized field Plots'),
            tableOutput(outputId = 'matrixExp')
        )
    )
)



# Define server logic required to produce table with randomized tables
server <- function(input, output) {
    


    output$matrixExp <- renderTable({
        # initiate loop control counters
        trtNo <- 1 # loop control
        trt <- 0 # loop control
        x <- 0 # loop control
        

        # Calculate total number of cells in Matrix 
       totalCells <- input$matrixWidth * input$matrixLength

        
        # Create vector of randomized cell numbers for the total number of observatuins in the vector  
        Matrix <- sample(1:totalCells, totalCells,  replace=FALSE)
        
        matrixTrt <- seq(1:length(Matrix)) # vector of treatments
        
        matrixSeq <- seq(1:length(Matrix)) # Vector of experimental units
        
        # Initially set all units to 0. so that when there are more matrix cells
        # then experimental units.  Any unused cells will have treatment = 0
        matrixSeq[] <- 0 
        
        combMatrix <- c(matrixSeq,matrixTrt)
        dim(combMatrix) <- c(totalCells,2)
        
        # Assign the treatment number with appropriate number of replications to first column of combMatrix    
        # Assign the sequential plot numbers in the second column
       for( j in 1:input$noTreatments){

            trt <- trt +1
            for( k in 1:input$noRepicates){

                x <- x +1
                combMatrix[x,1] <- trt
            }
        }
        
        # The plot number are read in there randomized order.
        # The  plot numbers are read in order from the vector containing the randomized plot number.
        # The treatment number is read from the the matrix containing the treatments with associated plot number.
        # The corresponding treatments are assigned to a new vector.
        # This new vector is converted to a matrix and outputted.
        matrixExp <- vector(mode="integer", length= totalCells)   
        for(m in 1:totalCells){
            matrixExp[m] <- combMatrix[ Matrix[m],1]
        }
        dim(matrixExp) <- c(input$matrixLength,input$matrixWidth)
        storage.mode(matrixExp) <- "integer"
        matrixExp      
        
    })
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

