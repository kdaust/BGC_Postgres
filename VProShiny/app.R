library(RPostgreSQL)
library(data.tree)
library(data.table)
library(shiny)
require(reshape2)
require(shinyWidgets)
require(ggplot2)
require(vegan)
require(shinythemes)
require(devtools)
require(rhandsontable)
require(magrittr)
require(dplyr)
library(shinyjs)
library(collapsibleTree)
require(DiagrammeR)
library(rpart)
library(randomForest)
library(visNetwork)
library(sparkline)
library(tree)
library(partykit)

source("reprtree/R/functions.R")
source("reprtree/R/plot.getTree.R")
source("reprtree/R/plot.reprtree.R")
source("reprtree/R/ReprTree.R")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "localhost", port = 5432, dbname = "plotdata")
datR <- list()
datSubstr <- data.frame(Type = c("Org Matter", "Dec Wood","Bedrock","Rocks","Mineral Soil","Water"), Percent = rep(0,6))
datTree <- matrix(data = NA, nrow = 4, ncol = 8) %>% as.data.frame(.) %>% mutate_all(as.numeric) %>%
  set_colnames(c("Trees", "A1","A2", "A3", "%(A)","B1", "B2", "%(B)")) %>%
  mutate(Trees = as.character(Trees))
datShrubs <- matrix(data = NA, nrow = 4, ncol = 4) %>% as.data.frame(.) %>% mutate_all(as.numeric) %>%
  set_colnames(c("Shrubs", "B1","B2","B")) %>%
  mutate(Shrubs = as.character(Shrubs))
datHerbs <- matrix(data = NA, nrow = 4, ncol = 2) %>% as.data.frame(.) %>% mutate_all(as.numeric) %>%
  set_colnames(c("Herbs","%(C)")) %>%
  mutate(Herbs = as.character(Herbs))
datMoss <- matrix(data = NA, nrow = 4, ncol = 2) %>% as.data.frame(.) %>% mutate_all(as.numeric) %>%
  set_colnames(c("Moss","%(D)")) %>%
  mutate(Moss = as.character(Moss))
plotHeader <- read.csv("PlotHeaders.csv")
plotHeader <- plotHeader[,-length(plotHeader)]
tableNames <- c("bec_veg", "bec_env")
species <- dbGetQuery(con, "SELECT DISTINCT species FROM bec_veg WHERE species LIKE '%-1' ORDER BY species")
typeCodes <- read.csv("SppCodes.csv")
##plots <- dbGetQuery(con, "SELECT DISTINCT plotnumber FROM bec_veg ORDER BY plotnumber")

sumVegDat <- function(datR){
  t1 <- datR[["tree"]] [,c("Trees","%(A)","%(B)")] %>% mutate(Cover = `%(A)` + `%(B)`) %>% extract(c("Trees","Cover")) %>% 
    set_colnames(c("Species","Cover"))
  t2 <- datR[["shrub"]] %>% extract(c("Shrubs","B")) %>% 
    set_colnames(c("Species","Cover"))
  t3 <- datR[["herb"]] %>% 
    set_colnames(c("Species","Cover"))
  t4 <- datR[["moss"]] %>% 
    set_colnames(c("Species","Cover"))
  dOut <- rbind(t1,t2,t3,t4)
  dOut <- dOut[!is.na(dOut$Species),]
  dOut
}

pf <- function(x){
  if(x$isLeaf && (is.null(x$cover)||is.na(x$cover))){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

pf2 <- function(x){
  if(x$level > 7){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

ui <- navbarPage(title = ("The New VPro"),theme = "css/bcgov.css",
  tabPanel(title = "Plot Data Entry",
           h1("Enter data in the fields below, then compile and check before submitting to database"),
           useShinyjs(),
           div(id = "form1",
               fluidRow(
                 splitLayout(offset = 1,
                             textInput("id","Plot No."),
                             textInput("surv","Surveyors"),
                             dateInput("date","Date"),
                             textInput("field","Field No."))
               ),
               fluidRow(
                 column(7,
                        h3("LOCATION"),
                        textInput("location", "General Location", value = "", width = "80%"),
                        fluidRow(
                          splitLayout(
                            textInput("fReg","Forest Region"),
                            textInput("utmz","UTM Zone"),
                            textInput("east","Easting"),
                            textInput("north","Northing"))
                        ),
                        fluidRow(
                          splitLayout(
                            textInput("airPh","Air Photo No."),
                            textInput("lat","Lat"),
                            textInput("long","Long"),
                            textInput("ecosec","Ecosec."))
                        ),
                        
                        h3("SITE INFORMATION"),
                        textInput("representing", "Plot Representing", value = "", width = "80%"),
                        fluidRow(
                          splitLayout(
                            textInput("bgc","BGC Unit"),
                            textInput("ss","Site Series"),
                            textInput("class","Class"),
                            textInput("distrib","Distrib"),
                            textInput("map","Map Unit"))
                        ),
                        fluidRow(
                          splitLayout(
                            textInput("smr","SMR"),
                            textInput("snr","SNR"),
                            textInput("success","Success Stat"),
                            textInput("struct","Struct. Stage"),
                            textInput("age","Stand Age"))
                        ),
                        fluidRow(
                          splitLayout(
                            textInput("elev","Elev."),
                            textInput("slope","Slope"),
                            textInput("aspect","Aspect"),
                            textInput("mslope","Meso Slope Pos"),
                            textInput("surfshp","Surface Shp"),
                            textInput("microtop","Microtopog"))
                        )
                 ),
                 column(5,
                        textAreaInput("notes", "Notes", rows = 3, resize = "both"),
                        rHandsontableOutput("hot")
                 )
               ),
               br(), br(),
               h2("VEGETATION"),br(),
               h4("Please enter vegetation data in the tables below."), br(),
               fluidRow(
                 column(8,
                        h4("Trees"),
                        rHandsontableOutput("hotTree"),
                        h4("Shrubs"),
                        rHandsontableOutput("hotShrub")),
                 column(4,
                        h4("Herbs"),
                        rHandsontableOutput("hotHerb"),
                        h4("Mosses/Lichens"),
                        rHandsontableOutput("hotMoss")),
                 br(),
                 actionButton("check1","Compile Data"),
                 h2("Please double check your data before submission"),
                 tableOutput("check"),
                 actionButton("submit","Submit to Database")
                 
               )),
           
           shinyjs::hidden(
             div(
               id = "thankyou_msg",
               h3("Your data has been successfully submitted to the database!"),
               actionLink("submit_another", "Enter another plot")
             )
           )),
  tabPanel(title = "View Data",
           h3("Use an SQL query to select data from the database"),
           textInput("sql", "Enter SQL expression"),
           h3("Or select from the dropdowns below:"),
           br(),
           pickerInput("table","Select Table", choices = tableNames, multiple = F),
           uiOutput("selectCols"),
           actionButton("submit2","Submit Query"),
           tableOutput("outTable")
           ),
  tabPanel(title = "Hierarchy",
            h3("Select a species to create a hierachy tree for:"),
            pickerInput("hierSpp", "Species", selected = "PINUPON-1", choices = species, multiple = FALSE),
           h4("Expand a subtree click on a node, and click 'Retrieve Data' to show all data below the selected node."),
            collapsibleTreeOutput("hierTree"),
           verbatimTextOutput("nodeStr"),
           actionButton("submitNode","Retrive data"),
           tableOutput("treeDat")
            ),
  tabPanel(title = "Analysis Tools",
           h2("Pairwise Diagnostic"),
           fluidRow(column(4,
                           h3("Select subtree to run diagnostic on"),
                           pickerInput("pdLevel","Select Hierarchy Level", choices = c("suborder","alliance","association","siteseries"), 
                                       selected = "alliance", multiple = F),
                           uiOutput("pdSelect1"),
                           uiOutput("pdSelect2"),
                           numericInput("pdTreeCol","Max value to highlight nodes", min = 0, max = 50, value = 15),
                           actionButton("runPD","Run Pairwise Diagnostic")
                           ),
                    column(8,
                           h3("Pairwise Scores"),
                           tableOutput("pdTable")
                           )
                    ),
           
           collapsibleTreeOutput("pdTree", height = "700px"),
           h3("Select pair(s) to show full data"),
           uiOutput("pdBigTblSelect1"),
           uiOutput("pdBigTblSelect2"),
           tableOutput("pdBigTbl")
           ),
  tabPanel(title = "Classification",
           h2("Random Forest and RPart Classification"),
           pickerInput("ClassGrp","Select Level to Create Model", choices = 
                         c("region", "class", "\"order\"", "suborder", "alliance", 
                           "suball", "association", "subassoc", "siteseries"), multiple = F),
           numericInput("numCtOff","Remove groups with < x plots",value = 5, min = 0),
           pickerInput("statType","Select value for classification",choices = c("MeanCov","Constancy","Goldstream"), selected = "Goldstream"),
           h2("Build Model"),
           fluidRow(column(6,
                           actionButton("buildRPart","Build RPart Model"),
                           visNetworkOutput("rpMod",height = "900px")
                           # ,
                           # selectizeInput("rpPlotNum","Select New Plots To Classify",plots, multiple = TRUE),
                           # actionButton("rpPred", "RPart Predict"),
                           # tableOutput("rpPredOut")
           ),column(6,
                    actionButton("buildRF","Build Random Forest Model"),
                    plotOutput("rfMod", height = "900px")
                    # ,
                    # selectizeInput("rfPlotNum","Select New Plots To Classify", plots, multiple = TRUE),
                    # actionButton("rfPred", "Random Forest Predict"),
                    # tableOutput("rfPredOut")
                    ))
           
           )
)

server <- function(input, output) {
  onStop(function() dbDisconnect(con))
  
  output$hot <- renderRHandsontable({
    rhandsontable(datSubstr, width = 550, height = 300) %>%
      hot_col("Type", readOnly = TRUE) %>%
      hot_validate_numeric(cols = 2, min = 0, max = 100)
  })
  
  output$hotTree <- renderRHandsontable({
    rhandsontable(datTree) %>%
      hot_validate_numeric(cols = 2:6, min = 0, max = 100) %>%
      hot_context_menu(allowColEdit = FALSE)
  })
  
  output$hotShrub <- renderRHandsontable({
    rhandsontable(datShrubs) %>%
      hot_validate_numeric(cols = 2:4, min = 0, max = 100) %>%
      hot_context_menu(allowColEdit = FALSE)
  })
  
  output$hotHerb <- renderRHandsontable({
    rhandsontable(datHerbs) %>%
      hot_validate_numeric(cols = 2, min = 0, max = 100) %>%
      hot_context_menu(allowColEdit = FALSE)
  })
  
  output$hotMoss <- renderRHandsontable({
    rhandsontable(datMoss) %>%
      hot_validate_numeric(cols = 2, min = 0, max = 100) %>%
      hot_context_menu(allowColEdit = FALSE)
  })
  
  prepVeg <- reactive({
    dat1 <- hot_to_r(input$hotTree) %>% mutate(Cover = sum(`%(A)`,`%(B)`, na.rm = TRUE)) %>% extract(c("Trees", "Cover")) %>% set_colnames(c("species", "cover"))
    dat2 <- hot_to_r(input$hotShrub) %>% extract(c("Shrubs", "B")) %>% set_colnames(c("species", "cover"))
    dat3 <- hot_to_r(input$hotHerb) %>% set_colnames(c("species", "cover"))
    dat4 <- hot_to_r(input$hotMoss) %>% set_colnames(c("species", "cover"))
    dat <- rbind(dat1,dat2,dat3,dat4)
   ## browser()
    dat[!is.na(dat$species),]
  })
  
  prepEnv <- reactive({
    plotHeader$plotnumber <- input$id; plotHeader$sitesurveyor <- input$surv
    plotHeader$date <- input$date; plotHeader$fieldnumber <- input$field
    plotHeader$location <- input$location; plotHeader$fsregiondistrict <- input$fReg
    plotHeader$utmzone <- input$utmz; plotHeader$utmeasting <- input$east
    plotHeader$utmnorthing <- input$north; plotHeader$airphotonum <- input$airPh
    plotHeader$latitude <- input$lat; plotHeader$longitude <- input$long
    plotHeader$ecosection <- input$ecosec; plotHeader$plotrepresenting <- input$representing
    plotHeader$becsiteunit <- input$bgc; plotHeader$siteseries <- input$ss
    plotHeader$transdistrib <- input$distrib; plotHeader$mapunit <- input$map
    plotHeader$moistureregime <- input$smr; plotHeader$nutrientregime <- input$snr
    plotHeader$successionalstatus <- input$success; plotHeader$structuralstage <- input$struct
    plotHeader$standage <- input$age; plotHeader$elevation <- input$elev
    plotHeader$slopegradient <- input$slope; plotHeader$aspect <- input$aspect
    plotHeader$mesoslopeposition <- input$mslope; plotHeader$surfaceshape <- input$surfshp
    
    substrDat <- hot_to_r(input$hot)
    plotHeader$substrateorganicmatter <- substrDat[1,2]
    plotHeader$substratedecwood <- substrDat[2,2]
    plotHeader$substratebedrock <- substrDat[3,2]
    plotHeader$substraterocks <- substrDat[4,2]
    plotHeader$substratemineralsoil <- substrDat[5,2]
    plotHeader$substratewater <- substrDat[6,2]
    plotHeader
  })
  
  observeEvent(input$check1,{
    output$check <- renderTable({
      dat <- prepVeg()
      dat
    })
    
    output$check2 <- renderTable({
      tab <- plotHeader
      tab[,!is.na(tab[1,])]
    })
  })
  
  observeEvent(input$submit2,{
    output$outTable <- renderTable({
      drv <- dbDriver("PostgreSQL")
      con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "localhost", port = 5432, dbname = "plotdata")
      dat <- dbGetQuery(con, input$sql)
      dat
    })
    
  })
  
  observeEvent(input$submit,{
    dat1 <- prepVeg() %>% mutate(PlotNo = input$id)
    dat1 <- dat1[,c("PlotNo","species","cover")] %>% set_colnames(c("plotnumber","species","cover"))
    dat2 <- prepEnv()
    dbWriteTable(con, "bec_veg", value = dat1, append = TRUE, row.names = F)
    dbWriteTable(con, "bec_env", value = dat2, append = TRUE, row.names = F)
    shinyjs::reset("form1")
    shinyjs::hide("form1")
    shinyjs::show("thankyou_msg")
    
  })
  
  observeEvent(input$submit_another,{
    show("form1")
    hide("thankyou_msg")
  })   
  
  output$selectCols <- renderUI({
    choices <- dbGetQuery(con, paste("SELECT column_name FROM information_schema.columns WHERE table_name = '",input$table, 
                                     "' ORDER BY ordinal_position", sep = ""))
    output <- tagList()
    output[[1]] = div(style="display: inline-block;vertical-align:top; width: 150px;", 
                      pickerInput("colNames", "SELECT", choices = choices$column_name, multiple = TRUE))
    output[[2]] = div(style="display: inline-block;vertical-align:top; width: 150px;",
                      pickerInput("colNames2","WHERE", choices = choices$column_name, multiple = F))
    output[[3]] = div(style="display: inline-block;vertical-align:top; width: 150px;",
                      pickerInput("logOp", "", choices = c("=", "!=", "<",">", "IS NULL","IS NOT NULL", "LIKE"), multiple = F))
    output[[4]] = div(style="display: inline-block;vertical-align:top; width: 150px;",
                      textInput("value","Value or String", value = ""))
    # output[[7]] = p("or")
    # output[[8]] = pickerInput("colNames3",label = NULL, choices = c("", choices$column_name), inline = TRUE, selected = "", multiple = F)
    output
    
  })
  
  observeEvent(input$submit2,{
    output$outTable <- renderTable({
      if(input$sql == ""){
        sql <- paste("SELECT", paste(input$colNames, collapse = ","), "FROM", input$table, "WHERE", input$colNames2, input$logOp, input$value, 
                     input$colNames3, sep = " ")
        dat <- dbGetQuery(con, sql)
      }else{
        dat <- dbGetQuery(con, input$sql)
      }
      dat
    })
    
  })
  
  output$hierTree <- renderCollapsibleTree({
    vegData <- dbGetQuery(con, paste("SELECT * FROM bec_hier WHERE species = '",input$hierSpp,"'", sep = ""))
    vegData$pathString <- paste("vegData",
                                vegData$region, vegData$class, vegData$order,
                                vegData$suborder, vegData$alliance, vegData$association, 
                                vegData$siteseries, vegData$plotnumber, vegData$species, sep = "_")
    vegTree <- as.Node(vegData, pathDelimiter = "_", na.rm = FALSE, colLevels = list(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,"cover"))
    treeSmall <- Clone(vegTree, pf)
    treeSmall$Do(function(x) if(is.null(x$cover)) x$cover <- 0)
    ##browser()
    treeSmall$Do(function(x){
      x$cover <- Aggregate(node = x,
                           attribute = "cover",
                           aggFun = mean)
    }, traversal = "post-order")
    Prune(treeSmall, pf2)
    collapsibleTree(treeSmall, nodeSize = "cover", attribute = "cover", tooltip = TRUE, inputId = "node")
    
  })
  
  output$nodeStr <- renderPrint({
    temp <- input$node
    str(temp)
  })
  
  observeEvent(input$submitNode,{
    output$treeDat <- renderTable({
      colNames <- c("region","class","order","suborder","alliance","association","siteseries")
      temp <- input$node
      name <- temp[[length(temp)]]
      len <- length(temp)
      dat <- dbGetQuery(con, paste("SELECT plotnumber,species,cover FROM bec_hier WHERE ", colNames[len]," LIKE '",name,"'", sep = ""))
      head(dat, n = 40)
    })
  })
  
  output$pdSelect1 <- renderUI({
    vegData <- dbGetQuery(con, "SELECT DISTINCT region,class,\"order\",suborder,alliance,association,siteseries FROM bec_hier_small")
    num <- grep(input$pdLevel, colnames(vegData))
    rtLev.choose <- colnames(vegData)[1:num]
    pickerInput("rtLev","Select Root Level", choices = rtLev.choose, multiple = F, selected = "order")
  })
  
  output$pdSelect2 <- renderUI({
    vegData <- dbGetQuery(con, "SELECT DISTINCT region,class,\"order\",suborder,alliance,association,siteseries FROM bec_hier_small")
    root.choose <- vegData[,input$rtLev] %>%
      as.vector() %>%
      unlist() %>%
      unique()
    pickerInput("pdRoot","Select Root of Subtree", choices = root.choose, multiple = F)
  })
  
  pdOut <- eventReactive(input$runPD,{
    vegData <- dbGetQuery(con, paste("SELECT ",input$pdLevel, ",species,variable,value FROM bec_hier_small WHERE \"",
                                     input$rtLev, "\" = '",input$pdRoot,"'", sep = ""))
    colnames(vegData)[1:2] <- c("Group","Species")
    vegData <- dcast(vegData, Group + Species ~ variable, value.var = "value", fun.aggregate = mean) %>% melt()
    selectUnits <- unique(vegData$Group)
    len <- length(selectUnits)
    
    domDiffCls <- data.frame(SigDiff = c(5,4,3,2), DomDiff = c("dd1","dd2","dd3","dd4"))
    scoreVals <- data.frame(Code = c("d1","d2","d3","dd1","dd2","dd3","dd4","c","cd","cm"),
                            Value = c(3,2,0,6,4,3,2,1,2,0))
    CovConst <- vegData
    ##browser()
    
    out <- foreach(j = (1:(len-1)), .combine = rbind, .packages = c("foreach","reshape2")) %:% 
      foreach(k = ((j+1):len), .combine = rbind, .packages = c("foreach","reshape2")) %do% {
        select <- selectUnits[c(j,k)]
        CovTemp <- CovConst[CovConst$Group %in% select,] ##subset
        
        for(i in 1:2){###backwards and forwards
          Cov1 <- dcast(CovTemp, Species ~ Group + variable, value.var = "value")
          Cov1[is.na(Cov1)] <- 0
          Cov1 <- Cov1[,c(1,3,2,5,4)]
          if(i == 2){
            Cov1 <- Cov1[,c(1,4,5,2,3)]
          }
          Cov1 <- Cov1[Cov1[,3] >= 60 & !is.na(Cov1[,3]),]###remove < 60% constancy
          Cov1$ConstDiff <- abs(Cov1[,3] - Cov1[,5])
          Cov1$Differential <- ifelse(Cov1$ConstDiff >= 80,"d1",
                                      ifelse(Cov1$ConstDiff >= 60,"d2",
                                             ifelse(Cov1$ConstDiff >= 40,"d3",NA)))
          Cov1$SigA <- ifelse(Cov1[,2] <= 0.1,0,
                              ifelse(Cov1[,2] <= 0.3,1,
                                     ifelse(Cov1[,2] <= 1,2,
                                            ifelse(Cov1[,2] <= 2.2,3,
                                                   ifelse(Cov1[,2] <= 5,4,
                                                          ifelse(Cov1[,2] <= 10, 5,
                                                                 ifelse(Cov1[,2] <= 20,6,
                                                                        ifelse(Cov1[,2] <= 33,7,
                                                                               ifelse(Cov1[,2] <= 50,8,
                                                                                      ifelse(Cov1[,2] <= 75,9,10))))))))))
          Cov1$SigB <- ifelse(Cov1[,4] <= 0.1,0,
                              ifelse(Cov1[,4] <= 0.3,1,
                                     ifelse(Cov1[,4] <= 1,2,
                                            ifelse(Cov1[,4] <= 2.2,3,
                                                   ifelse(Cov1[,4] <= 5,4,
                                                          ifelse(Cov1[,4] <= 10, 5,
                                                                 ifelse(Cov1[,4] <= 20,6,
                                                                        ifelse(Cov1[,4] <= 33,7,
                                                                               ifelse(Cov1[,4] <= 50,8,
                                                                                      ifelse(Cov1[,4] <= 75,9,10))))))))))
          Cov1$SigDiff <- Cov1$SigA - Cov1$SigB
          Cov1 <- merge(Cov1, domDiffCls, by = "SigDiff", all.x = TRUE)
          Cov1$DomDiff <- ifelse(Cov1$SigDiff < 6, NA, Cov1$SigDiff)
          Cov1$Const <- ifelse(Cov1$SigA >= 6,"cd",
                               ifelse(Cov1$SigA >= 3, "c","cm"))
          
          ###sum values
          Cov1$Value <- apply(Cov1[,c(8,11,12)],1,FUN = function(x){sum(scoreVals$Value[scoreVals$Code %in% x])})
          Cov1$Value <- Cov1$Value*(Cov1[,4]/100)
          Cov1 <- merge(Cov1, typeCodes, by = "Species", all.x = TRUE)
          Cov1$Value <- ifelse(Cov1$Type %in% c(9,10,11,13), Cov1$Value/2, Cov1$Value)
          
          if(i == 1){
            sumA <- sum(Cov1$Value)
            if(all(is.na(Cov1$Differential))){sumA <- sumA - 2}
          }else{
            sumB <- sum(Cov1$Value)
            if(all(is.na(Cov1$Differential))){sumB <- sumB - 2}
          }
          
        }
        totDiff <- sumA+sumB
        Cov1$Total <- totDiff
        Cov1$Group1 <- select[1]
        Cov1$Group2 <- select[2]
        colnames(Cov1)[3:6] <- c("G2Cov","G2Const","G1Cov","G1Const")
        Cov1
      }
    out
  })
  
  observeEvent(input$runPD, {
    
    output$pdTable <- renderTable({
      pdOut()[,c("Group1","Group2","Total")] %>% unique()
    })
    
    output$pdBigTblSelect1 <- renderUI({
      dat <- pdOut()[,c("Group1","Group2","Total")] %>% unique()
      pickerInput("bigTblG1","Select First Group", choices = unique(dat$Group1), multiple = T)
    })
    
    output$pdBigTblSelect2 <- renderUI({
      dat <- pdOut()[,c("Group1","Group2","Total")] %>% unique()
      pickerInput("bigTblG2","Select Second Group", choices = unique(dat$Group2[dat$Group1 == input$bigTblG1]), multiple = T)
    })
    
    output$pdBigTbl <- renderTable({
      dat <- pdOut()
      dat <- dat[dat$Group1 %in% input$bigTblG1 && dat$Group2 %in% input$bigTblG2,]
      dat
    })
    
    output$pdTree <- renderCollapsibleTree({
      temp <- pdOut()[,c("Group1","Group2","Total")] %>% unique()
      temp2 <- data.frame(Group = c(temp$Group1,temp$Group2), Group2 = c(temp$Group2,temp$Group1), Total = c(temp$Total,temp$Total))
      vegData <- dbGetQuery(con, paste("SELECT * FROM bec_hier_small WHERE \"",
                                       input$rtLev, "\" = '",input$pdRoot,"'", sep = ""))
      vegData <- merge(vegData,temp2, by.x = input$pdLevel, by.y = "Group")
      cols <- c("region","class","order","suborder","alliance","association","siteseries")
      num <- grep(input$pdLevel, cols)
      startNum <- grep(input$rtLev, cols)
      vegData$pathString <- do.call(paste,c(vegData[,cols[startNum:num]], sep = "_")) 
      vegData$pathString <- paste("vegData_",vegData$pathString)
      ##browser()
      vegData <- vegData[,!colnames(vegData) %in% c("species","variable","value")] %>% unique()
      vals <- vegData[,c(colnames(vegData)[1],"Group2","Total")]
      colnames(vals)[1] <- "Group1"
      vals <- dcast(vals, Group1 ~ Group2, fun.aggregate = mean)
      rownames(vals) <- vals$Group1
      vals <- vals[,-1]
      minTot <- apply(vals, 2, FUN = min, na.rm = TRUE)
      minGrp <- apply(vals, 2, FUN = function(x) {names(x)[x == min(x, na.rm = T)]}) %>% .[!is.na(.)]
      names(minGrp) <- names(minTot)
 
      vegTree <- as.Node(vegData, pathDelimiter = "_")
      ##vegTree$Set(Total = minTot, Group = minGrp, filterFun = function(x) x$isLeaf, traversal = "pre-order")
      vegTree$Do(function(x){
        if(x$isLeaf){
          x$Total = minTot[which(names(minTot) == x$name)]
          x$Group = minGrp[which(names(minGrp) == x$name)]
        }
      })
      ##browser()
   
      vegTree$Do(function(x){
        x$Colour = 'purple'
        if (x$isLeaf){
          x$Label = paste(x$Group,x$Total, sep = ": ")
          if(x$Total < input$pdTreeCol){
            x$Colour = "red"
          }
        } })
      
      ##browser()
      ##grViz(generate_dot(ToDiagrammeRGraph(vegTree)))
      collapsibleTree(vegTree,tooltip = TRUE, size = "Total", attribute = "Label", fill = "Colour", inputId = "node2", collapsed = F)
    })
    
  })
  
  rollUp <- eventReactive(input$Classify,{###not currently used
    vegData <- dbGetQuery(con,"SELECT * FROM bec_hier")
    colnames(vegData)[grep("siteseries",colnames(vegData))] <- "Group"
    constCut <- 0.05
    
    require(doParallel)
    coreNo <- makeCluster(detectCores() - 1)
    registerDoParallel(coreNo, cores = detectCores() - 1)
 
    temp <- foreach(SS = unique(vegData$Group), .combine = rbind, .packages = "foreach") %dopar% {
      sub <- vegData[vegData$Group == SS,]
      num <- length(unique(sub$plotnumber))
      foreach(Spp = unique(sub$species), .combine = rbind) %do% {
        sub2 <- sub[sub$species == Spp,]
        numSpp <- dim(unique(sub2[,1:2]))[1]
        covsum <- sum(sub2$cover)
        mean <- covsum/num
        const <- numSpp/num
        if(const >= constCut){
          out <- data.frame(Group = SS, Species = Spp, MeanCov = mean, Constancy = const*100, NoPlots = num)
        }
        
      }
    }
    temp$Goldstream <- temp$Constancy*sqrt(temp$MeanCov)
    temp <- temp[temp$NoPlots > input$numCtOff,c("Group","Species",input$statType)]
    temp <- dcast(temp, Group ~ Species)
    temp[is.na(temp)] <- 0
    temp
  })
  
  prepClass <- function(){
    vegData <- dbGetQuery(con,paste("SELECT ",input$ClassGrp,",siteseries,species,variable,value FROM bec_hier_small"))
    colnames(vegData)[1] <- "Group"
    vegData <- dcast(vegData, Group + siteseries + species ~ variable) %>% mutate(Goldstream = Constancy*sqrt(MeanCov))
    plotNum <- aggregate(siteseries ~ Group, data = vegData, FUN = function(x)length(unique(x))) %>% set_colnames(c("Group","Number"))
    vegData <- merge(vegData,plotNum, by = "Group", all.x = T)
    vegData <- vegData[vegData$Number > input$numCtOff,]
    maxConst <- aggregate(Constancy ~ species, vegData, FUN = max)
    SpKeep <- maxConst$species[maxConst$Constancy >= 60] %>% as.character()
    vegData <- vegData[vegData$species %in% SpKeep,]
    vegData <- vegData[,c("Group","siteseries","species",input$statType)]
    temp <- dcast(vegData, Group + siteseries ~ species)
    temp[is.na(temp)] <- 0
    temp <- temp[,-2]
    temp
  }
  
  observeEvent(input$buildRPart,{
    RPart <- eventReactive(input$buildRPart,{
      dat <- prepClass()
      dat$Group <- as.factor(dat$Group)
      ##RPcontrol <- rpart.control(maxcompete = 1, maxsurrogate = 2, usesurrogate = 1, surrogatestyle = 1)
      fit.rp <- rpart(Group ~ ., data = dat, method = "class")
      fit.rp
    })
    
    output$rpMod <- renderVisNetwork({
      visTree(RPart(), main = "Veg Classification Tree", width = "100%")
    })
  })
  
  observeEvent(input$buildRF,{
    RF <- eventReactive(input$buildRF,{
      dat <- prepClass()
      dat <- dat[dat$Group != "",]
      dat$Group <- as.factor(dat$Group)
      fit.rf <- randomForest(Group ~ .,data=dat, nodesize = 2, 
                             do.trace = 10, ntree=101, na.action=na.fail)
      
      fit.rf2 <- ReprTree(fit.rf, dat[,-1])
      fit.rf2
    })
    
    output$rfMod <- renderPlot({
      model <- RF()
      ##browser()
      plot.reprtree(model, depth = 5)
    })
  })
  
  # observeEvent(input$rpPred,{
  #   output$rpPredOut <- renderTable({
  #     newDat <- dbGetQuery(con,paste("SELECT plotnumber,species,cover FROM bec_veg WHERE plotnumber LIKE ",input$rpPlotNum, sep = ""))
  #     newDat <- dcast(newDat, plotnumber ~ species)
  #     browser()
  #   })
  # })
  
  
  
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
