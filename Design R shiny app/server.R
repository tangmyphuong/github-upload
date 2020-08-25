shinyServer(function(input, output, session) {
  
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    models[[name]] <- readRDS(file = rdsfile)
  }

  ############################################################################## 
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final")
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  ############################################################################## 
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  ############################################################ NULL ########################################################
  
  
  
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  ##############################################################################  
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################  
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  
############################################################ GLMNET ########################################################
  
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ##############################################################################  
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  ############################################################################## 
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
############################################################ PLS ########################################################
  
  
    
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ##############################################################################  
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  ############################################################################## 
  output$PlsRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
############################################################ RPART ########################################################
  
  
    
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rpart")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ##############################################################################  
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel)
  })     
  

  
  
######################################################### maintenance point ####################################################
  
  ############################################################ KernelPLS ########################################################
  
  
  
  
  ##############################################################################  
  getKernelPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$KernelPlsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$KernelPlsGo,
    {
      library(pls)
      method <- "kernelpls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getKernelPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$KernelPLSModelSummary0 <- renderText({
    description("kernelpls")
  })
  
  ##############################################################################  
  output$KernelPlsMetrics <- renderTable({
    req(models$kernelpls)
    models$kernelpls$results[ which.min(models$kernelpls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$KernelPlsModelPlots <- renderPlot({
    req(models$kernelpls)
    plot(models$kernelpls)
  })     
  
  ############################################################################## 
  output$KernelPlsRecipe <- renderPrint({
    req(models$kernelpls)
    models$kernelpls$recipe
  })  
  
  ############################################################################## 
  output$KernelPlsModelSummary2 <- renderPrint({
    req(models$kernelpls)
    summary(models$kernelpls$finalModel)
  })
  
  
  ######################################## glmboost ########################################################
  
  
  
  
  ##############################################################################  
  getglmboostRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$glmboostPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$glmboostGo,
    {
      library(mboost)
      library(plyr)
      method <- "glmboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getglmboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$glmboostModelSummary0 <- renderText({
    description("glmboost")
  })
  
  ##############################################################################  
  output$glmboostMetrics <- renderTable({
    req(models$glmboost)
    models$glmboost$results[ which.min(models$glmboost$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$glmboostModelPlots <- renderPlot({
    req(models$glmboost)
    plot(models$glmboost)
  })     
  
  ############################################################################## 
  output$glmboostRecipe <- renderPrint({
    req(models$glmboost)
    models$glmboost$recipe
  })  
  
  ############################################################################## 
  output$glmboostModelSummary2 <- renderPrint({
    req(models$glmboost)
    summary(models$glmboost$finalModel)
  })
  
  ##################### Radial Basis Function Kernel Regularized Least Squares ########################################################
  
  #################################### krls Radial #############################
  
  
  ##############################################################################  
  getKrlsRadialRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$krlsRadialPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$krlsRadialGo,
    {
      library(KRLS)
      library(kernlab)
      method <- "krlsRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getKrlsRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$krlsRadialModelSummary0 <- renderText({
    description("krlsRadial")
  })
  
  ##############################################################################  
  output$krlsRadialMetrics <- renderTable({
    req(models$krlsRadial)
    models$krlsRadial$results[ which.min(models$krlsRadial$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$krlsRadialModelPlots <- renderPlot({
    req(models$krlsRadial)
    plot(models$krlsRadial)
  })     
  
  ############################################################################## 
  output$krlsRadialRecipe <- renderPrint({
    req(models$krlsRadial)
    models$krlsRadial$recipe
  })  
  
  ############################################################################## 
  output$krlsRadialModelSummary2 <- renderPrint({
    req(models$krlsRadial)
    summary(models$krlsRadial$finalModel)
  })
  
  #####################  Polynomial Kernel Regularized Least Squares  ########################################################
  
  #################################### krls Poly #############################
  
  
  ##############################################################################  
  getKrlsPolyRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$krlsPolyPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$krlsPolyGo,
    {
      library(KRLS)
      method <- "krlsPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getKrlsPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$krlsPolyModelSummary0 <- renderText({
    description("krlsPoly")
  })
  
  ##############################################################################  
  output$krlsPolyMetrics <- renderTable({
    req(models$krlsPoly)
    models$krlsPoly$results[ which.min(models$krlsPoly$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$krlsPolyModelPlots <- renderPlot({
    req(models$krlsPoly)
    plot(models$krlsPoly)
  })     
  
  ############################################################################## 
  output$krlsPolyRecipe <- renderPrint({
    req(models$krlsPoly)
    models$krlsPoly$recipe
  })  
  
  ############################################################################## 
  output$krlsPolyModelSummary2 <- renderPrint({
    req(models$krlsPoly)
    summary(models$krlsPoly$finalModel)
  })

  #####################  Support Vector Machines with Radial Basis Function Kernel  ########################################################
  
  #################################### svm Radial #############################
  
  
  ##############################################################################  
  getsvmRadialRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$svmRadialPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$svmRadialGo,
    {
      library(kernlab)
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getsvmRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$svmRadialModelSummary0 <- renderText({
    description("svmRadial")
  })
  
  ##############################################################################  
  output$svmRadialMetrics <- renderTable({
    req(models$svmRadial)
    models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$svmRadialModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })     
  
  ############################################################################## 
  output$svmRadialRecipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })  
  
  ############################################################################## 
  output$svmRadialModelSummary2 <- renderPrint({
    req(models$svmRadial)
    summary(models$svmRadial$finalModel)
  })

  #####################  Support Vector Machines with Radial Polynomial Kernel  ########################################################
  
  #################################### svmPoly #############################
  
  
  ##############################################################################  
  getsvmPolyRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$svmPolyPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$svmPolyGo,
    {
      library(kernlab)
      method <- "svmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getsvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$svmPolyModelSummary0 <- renderText({
    description("svmPoly")
  })
  
  ##############################################################################  
  output$svmPolyMetrics <- renderTable({
    req(models$svmPoly)
    models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$svmPolyModelPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })     
  
  ############################################################################## 
  output$svmPolyRecipe <- renderPrint({
    req(models$svmRadial)
    models$svmPoly$recipe
  })  
  
  ############################################################################## 
  output$svmPolyModelSummary2 <- renderPrint({
    req(models$svmPoly)
    summary(models$svmPoly$finalModel)
  })  
  ##########  Relevace Vector Machines with Radial Basis Function Kernel  ######################
  
  ####################################  rvm Radial  #############################
  
  
  ##############################################################################  
  getrvmRadialRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$rvmRadialPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$rvmRadialGo,
    {
      library(kernlab)
      method <- "rvmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getrvmRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$rvmRadialModelSummary0 <- renderText({
    description("rvmRadial")
  })
  
  ##############################################################################  
  output$rvmRadialMetrics <- renderTable({
    req(models$rvmRadial)
    models$rvmRadial$results[ which.min(models$rvmRadial$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$rvmRadialModelPlots <- renderPlot({
    req(models$rvmRadial)
    plot(models$rvmRadial)
  })     
  
  ############################################################################## 
  output$rvmRadialRecipe <- renderPrint({
    req(models$rvmRadial)
    models$rvmRadial$recipe
  })  
  
  ############################################################################## 
  output$rvmRadialModelSummary2 <- renderPrint({
    req(models$rvmRadial)
    summary(models$rvmRadial$finalModel)
  })
  
  ####################  Gaussian Process with Radial Basis Function Kernel  ######################
  
  #################################### gaussprRadial #############################
  
  
  ##############################################################################  
  getgaussprRadialRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gaussprRadialPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$gaussprRadialGo,
    {
      library(kernlab)
      method <- "gaussprRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgaussprRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$gaussprRadialModelSummary0 <- renderText({
    description("gaussprRadial")
  })
  
  ##############################################################################  
  output$gaussprRadialMetrics <- renderTable({
    req(models$gaussprRadial)
    models$gaussprRadial$results[ which.min(models$gaussprRadial$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$gaussprRadialModelPlots <- renderPlot({
    req(models$gaussprRadial)
    plot(models$gaussprRadial)
  })     
  
  ############################################################################## 
  output$gaussprRadialRecipe <- renderPrint({
    req(models$gaussprRadial)
    models$gaussprRadial$recipe
  })  
  
  ############################################################################## 
  output$gaussprRadialModelSummary2 <- renderPrint({
    req(models$gaussprRadial)
    summary(models$gaussprRadial$finalModel)
  })
  
  ############################################################ NeuralNetwork ########################################################
  
  
  
  
  ##############################################################################  
  getNNetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$NnetPreprocess)
  })

  ##############################################################################
  observeEvent(
    input$NnetGo,
    {
      library(nnet)
      method <- "nnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                               decay = seq(from = 0.1, to = 0.5, by = 0.1))
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getNNetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneGrid = nnetGrid)
        saveToRds(models[[method]], method)
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ##############################################################################
  output$NnetModelSummary0 <- renderText({
    description("nnet")
  })

  ##############################################################################
  output$NnetMetrics <- renderTable({
    req(models$nnet)
    models$nnet$results[ which.min(models$nnet$results[, "RMSE"]), ]
  })

  ##############################################################################
  output$NnetRecipe <- renderPrint({
    req(models$nnet)
    models$nnet$recipe
  })

  ##############################################################################
  output$NnetModelSummary2 <- renderPrint({
    req(models$nnet)
    summary(models$nnet$finalModel)
  })
  
  
  ########################### Radial Basis Function network rbf ########################################################
  
  
  
  
  ##############################################################################  
  getrbfRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$rbfPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$rbfGo,
    {
      library(RSNNS)
      method <- "rbf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getrbfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLenght=15)
        saveToRds(models[[method]], method)
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################
  output$rbfModelSummary0 <- renderText({
    description("rbf")
  })
  
  ##############################################################################
  output$rbfMetrics <- renderTable({
    req(models$rbf)
    models$rbf$results[ which.min(models$rbf$results[, "RMSE"]), ]
  })
  
  ##############################################################################
  output$rbfModelPlots <- renderPlot({
    req(models$rbf)
    plot(models$rbf)
  })
  
  ##############################################################################
  output$rbfRecipe <- renderPrint({
    req(models$rbf)
    models$rbf$recipe
  })
  
  
  ############################################################################## 
  output$rbfModelSummary1 <- renderPrint({
    
    req(models$rbf)
    print(models$rbf$finalModel)
    
  })
  ############################################################ Random Forest ########################################################
  
  
  
  
  ##############################################################################  
  getRfRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$rfPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$rfGo,
    {
      library(randomForest)
      method <- "rf"
      models[[method]] <- NULL
      rf_grid <- expand.grid(mtry = c(1:12))
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneGrid=rf_grid)
        saveToRds(models[[method]], method)
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################
  output$rfModelSummary0 <- renderText({
    description("rf")
  })
  
  ##############################################################################
  output$rfMetrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })
  
  ##############################################################################
  output$rfModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })
  
  ##############################################################################
  output$rfRecipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })
  
  
  ############################################################################## 
  output$rfModelTree <- renderPlot({
    library(randomForest)
    req(models$rf)
    varImpPlot(models$rf$finalModel)
    
  }) 
  
  ############################## Regularized Random Forest (RRFglobal) ########################################################

  
  
  
  ##############################################################################  
  getRRfRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$rrfPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$rrfGo,
    {
      library(RRF)
      method <- "RRFglobal"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLenght=15)
        saveToRds(models[[method]], method)
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################
  output$rrfModelSummary0 <- renderText({
    description("RRFglobal")
  })
  
  ##############################################################################
  output$rrfMetrics <- renderTable({
    req(models$RRFglobal)
    models$RRFglobal$results[ which.min(models$RRFglobal$results[, "RMSE"]), ]
  })
  
  ##############################################################################
  output$rrfModelPlots <- renderPlot({
    req(models$RRFglobal)
    plot(models$RRFglobal)
  })
  
  ##############################################################################
  output$rrfRecipe <- renderPrint({
    req(models$RRFglobal)
    models$RRFglobal$recipe
  })
  
  
  ############################################################################## 
  output$rrfModelTree <- renderPlot({
    library(RRF)
    req(models$RRFglobal)
    varImpPlot(models$RRFglobal$finalModel)
  })    
  ################################################ Extreme Gradient Boost ########################################################
  
  
  
  
  ##############################################################################  
  getXgbRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$xgbPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$xgbGo,
    {
      library(xgboost)
      method <- "xgbTree"
      models[[method]] <- NULL
      xgb_grid <- expand.grid(nrounds =c(100,200), #boosting iterations
                              max_depth = c(10, 15, 20, 25), #max tree depth
                              colsample_bytree = seq(0.5, 0.9, length.out = 5), #subsample Ratio of Columns
                              eta = 0.1, # shrinkage
                              gamma=0, #minimum loss reduction
                              min_child_weight = 1, #minimum sum of instance weight
                              subsample = 1)
                              
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getXgbRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneGrid=xgb_grid)
        saveToRds(models[[method]], method)
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################
  output$xgbModelSummary0 <- renderText({
    description("xgbTree")
  })
  
  ##############################################################################
  output$xgbMetrics <- renderTable({
    req(models$xgbTree)
    models$xgbTree$results[ which.min(models$xgbTree$results[, "RMSE"]), ]
  })
  
  ##############################################################################
  output$xgbModelPlots <- renderPlot({
    req(models$xgbTree)
    plot(models$xgbTree)
  })
  
  ##############################################################################
  output$xgbRecipe <- renderPrint({
    req(models$xgbTree)
    models$xgbTree$recipe
  })
  
  
  ############################################################################## 
  output$xgbModelSummary1 <- renderPrint({
    library(xgboost)
    req(models$xgbTree)
    print(models$xgbTree$finalModel)
    
  })      

  ####################################  Elastic Net  ########################################################
  
  
  
  
  ##############################################################################  
  getenetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$enetPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$enetGo,
    {
      library(elasticnet)
      method <- "enet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getenetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLenght=15)
        saveToRds(models[[method]], method)
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ##############################################################################
  output$enetModelSummary0 <- renderText({
    description("enet")
  })
  
  ##############################################################################
  output$enetMetrics <- renderTable({
    req(models$enet)
    models$enet$results[ which.min(models$enet$results[, "RMSE"]), ]
  })
  
  ##############################################################################
  output$enetModelPlots <- renderPlot({
    req(models$enet)
    plot(models$enet)
  })
  
  ##############################################################################
  output$enetRecipe <- renderPrint({
    req(models$enet)
    models$enet$recipe
  })
  
  
  ############################################################################## 
  output$enetModelSummary1 <- renderPrint({
    library(elasticnet)
    req(models$enet)
    print(models$enet$finalModel)
    
  })

  ####################################  Ensemble Model  ########################################################
  
  
  
  
  ##############################################################################  
  # getensembleRecipe <- reactive({
  #   recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$ensemblePreprocess)
  # })
  
  ##############################################################################
  # observeEvent(
  #   input$ensembleGo,
  #   {
  #     library(caretEnsemble)
  #     algorithmList  <- c('svmRadial', 'rf')
  #     method <- "ensemble"
  #     
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       models_list <- caretList(getensembleRecipe(), data = getTrainData(), methodList = algorithmList, metric = "RMSE", trControl = getTrControl(), tuneLenght=15)
  #       models[[method]] <- caretEnsemble(models_list, metric = "RMSE", trControl = getTrControl())
  #       saveToRds(models[[method]], method)
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  
  ##############################################################################
  # output$ensembleModelSummary0 <- renderText({
  #   "This is ensemble model between svmRadial (support vector machine Radial basis function) and rf (random forest)"
  # })
  
  ##############################################################################
  # output$ensembleMetrics <- renderTable({
  #   models$ensemble$ens_model$results
  # })

  # ############################################################################## 
  # output$ensembleModelSummary1 <- renderPrint({
  #   library(caretEnsemble)
  #   print(models$ensemble$ens_model$finalModel)
  # 
  # })
  
#####################################################################################################################  
  
  
    
  
  
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "Null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)

    
})
