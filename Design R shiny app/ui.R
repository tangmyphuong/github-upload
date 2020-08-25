shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Phuong Tang"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ),
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             
             tabsetPanel(
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "NullGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe"),
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("naomit","dummy")),
                                 bsTooltip(id = "GlmnetPreprocess", 
                                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "PlsPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "RpartPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               ),
               # maintenance point
               
               tabPanel("KernelPLS Model",
                        verbatimTextOutput(outputId = "KernelPLSModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "KernelPlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "KernelPlsPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "KernelPlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "KernelPlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "KernelPlsMetrics"),
                        hr(),
                        plotOutput(outputId = "KernelPlsModelPlots"),
                        verbatimTextOutput(outputId = "KernelPlsRecipe"),
                        verbatimTextOutput(outputId = "KernelPlsModelSummary2")
               ),
               
               tabPanel("GLMboost Model",
                        verbatimTextOutput(outputId = "glmboostModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "glmboostPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","nzv","dummy")),
                                 bsTooltip(id = "glmboostPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "glmboostGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "glmboostGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "glmboostMetrics"),
                        hr(),
                        plotOutput(outputId = "glmboostModelPlots"),
                        verbatimTextOutput(outputId = "glmboostRecipe"),
                        verbatimTextOutput(outputId = "glmboostModelSummary2")
               ),
               tabPanel("Krls Radial Model",
                        verbatimTextOutput(outputId = "krlsRadialModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "krlsRadialPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "krlsRadialPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "krlsRadialGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "krlsRadialGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "krlsRadialMetrics"),
                        hr(),
                        plotOutput(outputId = "krlsRadialModelPlots"),
                        verbatimTextOutput(outputId = "krlsRadialRecipe"),
                        verbatimTextOutput(outputId = "krlsRadialModelSummary2")
               ),
               tabPanel("Krls Poly Model",
                        verbatimTextOutput(outputId = "krlsPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "krlsPolyPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "krlsPolyPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "krlsPolyGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "krlsPolyGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "krlsPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "krlsPolyModelPlots"),
                        verbatimTextOutput(outputId = "krlsPolyRecipe"),
                        verbatimTextOutput(outputId = "krlsPolyModelSummary2")
               ),
               tabPanel("svm Radial Model",
                        verbatimTextOutput(outputId = "svmRadialModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svmRadialPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "svmRadialPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "svmRadialGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "svmRadialGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svmRadialMetrics"),
                        hr(),
                        plotOutput(outputId = "svmRadialModelPlots"),
                        verbatimTextOutput(outputId = "svmRadialRecipe"),
                        verbatimTextOutput(outputId = "svmRadialModelSummary2")
               ),
               tabPanel("svm Poly Model",
                        verbatimTextOutput(outputId = "svmPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svmPolyPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","center", "scale")),
                                 bsTooltip(id = "svmPolyPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "svmPolyGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "svmPolyGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svmPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "svmPolyModelPlots"),
                        verbatimTextOutput(outputId = "svmPolyRecipe"),
                        verbatimTextOutput(outputId = "svmPolyModelSummary2")
               ),
               tabPanel("rvm Radial Model",
                        verbatimTextOutput(outputId = "rvmRadialModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rvmRadialPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "rvmRadialPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rvmRadialGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rvmRadialGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rvmRadialMetrics"),
                        hr(),
                        plotOutput(outputId = "rvmRadialModelPlots"),
                        verbatimTextOutput(outputId = "rvmRadialRecipe"),
                        verbatimTextOutput(outputId = "rvmRadialModelSummary2")
               ),
               tabPanel("gaussprRadial Model",
                        verbatimTextOutput(outputId = "gaussprRadialModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprRadialPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "gaussprRadialPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprRadialGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprRadialGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprRadialMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprRadialModelPlots"),
                        verbatimTextOutput(outputId = "gaussprRadialRecipe"),
                        verbatimTextOutput(outputId = "gaussprRadialModelSummary2")
               ),
               tabPanel("Nnet Model",
                        verbatimTextOutput(outputId = "NnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "NnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("nzv","knnimpute","center","scale")),
                                 bsTooltip(id = "NnetPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "NnetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "NnetGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NnetMetrics"),
                        hr(),
                        # plotOutput(outputId = "NnetModelPlots"),
                        verbatimTextOutput(outputId = "NnetRecipe"),
                        verbatimTextOutput(outputId = "NnetModelSummary2")
               ),
               tabPanel("rbf Model",
                        verbatimTextOutput(outputId = "rbfModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rbfPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "rbfPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rbfGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rbfGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rbfMetrics"),
                        hr(),
                        plotOutput(outputId = "rbfModelPlots"),
                        verbatimTextOutput(outputId = "rbfRecipe"),
                        verbatimTextOutput(outputId = "rbfModelSummary1")
               ),
               tabPanel("Random Forest",
                        verbatimTextOutput(outputId = "rfModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rfPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "rfPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rfGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rfGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rfMetrics"),
                        hr(),
                        plotOutput(outputId = "rfModelPlots"),
                        verbatimTextOutput(outputId = "rfRecipe"),
                        plotOutput(outputId = "rfModelTree")
               ),
               tabPanel("Regularized Random Forest",
                        verbatimTextOutput(outputId = "rrfModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rrfPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "rrfPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rrfGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rrfGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rrfMetrics"),
                        hr(),
                        plotOutput(outputId = "rrfModelPlots"),
                        verbatimTextOutput(outputId = "rrfRecipe"),
                        plotOutput(outputId = "rrfModelTree")
               ),
               tabPanel("Extreme Gradient Boosting",
                        verbatimTextOutput(outputId = "xgbModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "xgbPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "xbgPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "xgbGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "xgbGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "xgbMetrics"),
                        hr(),
                        plotOutput(outputId = "xgbModelPlots"),
                        verbatimTextOutput(outputId = "xgbRecipe"),
                        verbatimTextOutput(outputId = "xgbModelSummary1")
               ),
               tabPanel("Elasticnet",
                        verbatimTextOutput(outputId = "enetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "enetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","center","scale")),
                                 bsTooltip(id = "enetPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "enetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "enetGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "enetMetrics"),
                        hr(),
                        plotOutput(outputId = "enetModelPlots"),
                        verbatimTextOutput(outputId = "enetRecipe"),
                        verbatimTextOutput(outputId = "enetModelSummary1")
               )
               # tabPanel("Ensemble",
               #          verbatimTextOutput(outputId = "ensembleModelSummary0"),
               #          fluidRow(
               #            column(width = 4, 
               #                   selectizeInput(inputId = "ensemblePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
               #                   bsTooltip(id = "ensemblePreprocess", 
               #                             title = "The order of preprocessing steps is important. The default steps were set to the best selection for this method.")
               #            ),
               #            column(width = 1, 
               #                   actionButton(inputId = "ensembleGo", label = "Train", icon = icon("play")),
               #                   bsTooltip(id = "ensembleGo", title = "This will train or retrain your model")
               #            )
               #          ),
               #          hr(),
               #          h3("Resampled performance:"),
               #          tableOutput(outputId = "ensembleMetrics"),
               #          hr(),
               #          verbatimTextOutput(outputId = "ensembleModelSummary1")
               # )
               
               
             )
             ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), selected = "krlsRadial", 
                          inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             plotOutput(outputId = "TestPlot")
    )
  )
))
