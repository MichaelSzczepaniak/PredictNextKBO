#library(shinyjs)
configUrl <- './data/non_ngram/config.csv'
config <- read.csv(configUrl)
ug_tab_content_file <- config[config$param == 'ug_tab_content',]$value

config_dir <- config[config$param == 'config_dir',]$value
# prepend dir of user guide content
ug_tab_content_file <- paste0(config_dir, ug_tab_content_file)
# read in the user guide content
ug_tab_content <- readChar(ug_tab_content_file,
                           file.info(ug_tab_content_file)$size)

modelChoices <- list("Blogs"=1, "News"=2, "Twitter"=3)

fluidPage(
    shinyjs::useShinyjs(),
    div(
        id = "loading_page",
        h1("Loading application data.  Be with you shortly...")
    ),
    
    # http://stackoverflow.com/questions/35599470#35665217
    shinyjs::hidden(
        div(
            id = "main_content",
            
            titlePanel(title="Predict Next Word"),
            
            sidebarPanel(
                
                # Let user know app is computing:
                # stackoverflow.com/questions/17325521#22475216
                tags$head(tags$style(type="text/css", "
                                     #load_compute_message {
                                     position: fixed;
                                     top: 0px;
                                     left: 0px;
                                     width: 100%;
                                     padding: 5px 0px 5px 0px;
                                     text-align: center;
                                     font-weight: bold;
                                     font-size: 100%;
                                     color: #000000;
                                     background-color: #CCFF66;
                                     z-index: 105;
                                     }
                                     ")),
                
                textInput('phrase', h4("Enter phrase:"), value="i love"),
                
                radioButtons('corpusToUse', h4("Corpus"), modelChoices,
                             selected=1, inline=TRUE),
                
                sliderInput("bigDiscount",
                            label = h4("Bigram Discount"),
                            min = 0.1, max = 1.9, value = 0.5, step = 0.1),
                
                sliderInput("trigDiscount",
                            label = h4("Trigram Discount"),
                            min = 0.1, max = 1.9, value = 0.5, step = 0.1),
                
                numericInput("histBars", label=h4("Probability Bars"), value=3,
                             width="150px"),
                
                checkboxInput("eosCheckbox", label = "Include EOS as prediction",
                              value = FALSE),
                
                actionButton("predictButton", "Predict next word of phrase"),
                
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div("Computing prediction...",
                                          id="load_compute_message"))
                
                ),
            mainPanel(
                tabsetPanel(
                    tabPanel("Predict",
                             h4('Prediction Settings:'),
                             verbatimTextOutput("sPredictionSettings"),
                             h4('Last two words of input phrase:'),
                             verbatimTextOutput("sInputBigram"),
                             h4('Predicted From:'),
                             verbatimTextOutput("sPredictedFrom"),
                             h4('Predicted Word:'),
                             verbatimTextOutput("sPredictedWord"),
                             plotOutput("pTopNProbs", width = 450, height = 300)
                    ),
                    tabPanel('User Guide', HTML(ug_tab_content)
                    )
                )
                
                
            )
        )
        
    )

)