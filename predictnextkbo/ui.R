# library(shiny)

modelChoices <- list("Modified Katz Back-Off"=1, "Stupid Back-Off"=2)

fluidPage(
    titlePanel(title="Predict Next Word"),
    
    sidebarPanel(
        textInput('phrase', h4("Enter phrase:")),
        radioButtons('predModel', h4("Prediction Model"), modelChoices),
        sliderInput("discount",
                    label = h4("Discount (absolute: used for smoothing"),
                    min = 0.1, max = 0.9, value = 0.5),
        submitButton("Predict next word of phrase")
    ),
    mainPanel(
        h4('Predicted Word:'),
        verbatimTextOutput("sPredictedWord"),
        h4('Last two words of input phrase:'),
        verbatimTextOutput("sInputBigram"),
        h4('Prediction Model:'),
        verbatimTextOutput("sPredModel"),
        h4('Discount Selected:'),
        verbatimTextOutput("sDiscount"),
        h4('Algorithm Path:'),
        verbatimTextOutput("sAlgoPath")
    )
)