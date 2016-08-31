# library(shiny)

modelChoices <- list("Blogs"=1, "News"=2, "Twitter"=3)

fluidPage(
    titlePanel(title="Predict Next Word"),
    
    sidebarPanel(
        textInput('phrase', h4("Enter phrase:")),
        radioButtons('corpusToUse', h4("Corpus"), modelChoices),
        sliderInput("bigDiscount",
                    label = h4("Bigram Discount"),
                    min = 0.1, max = 1.9, value = 0.5, step = 0.1),
        sliderInput("trigDiscount",
                    label = h4("Trigram Discount"),
                    min = 0.1, max = 1.9, value = 0.5, step = 0.1),
        actionButton(inputId = "predictButton", "Predict next word of phrase")
    ),
    mainPanel(
        h4('Prediction Settings:'),
        verbatimTextOutput("sPredictionSettings"),
        h4('Last two words of input phrase:'),
        verbatimTextOutput("sInputBigram"),
        # h4('Algorithm Path:'),
        # verbatimTextOutput("sAlgoPath"),
        h4('Predicted Word:'),
        verbatimTextOutput("sPredictedWord"),
        plotOutput("pTop3Probs", width = 400, height = 300)
        # verbatimTextOutput("pTop3Probs")
    )
)