library(RPostgreSQL)
library(data.tree)
library(data.table)
library(collapsibleTree)
library(rhandsontable)

test <- Node$new("Root", value = 3)
test$AddChild("C1", value = 4)
test$AddChild("C2", value = 5)
test$C1$AddChild("C1.4", value = 7)

hierTable <- read.csv("C:/Users/Kiri Daust/Desktop/Vegetation_Classification/FormTemplate.csv", header = F, stringsAsFactors = F)
rhandsontable(hierTable, selectCallback = TRUE) %>%
  hot_cols(renderer = "function (instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if(row == 2 || row == 9){
        if(col < 6){
          td.style.background = 'red';
        }else{
          td.style.background = 'purple';
        }
      }else if(value){
        td.style.background = 'lightgreen';
      }
  }")
  
hierTable <- hierTable[hierTable$Alliance != "",]
colnames(hierTable) <- dbSafeNames(colnames(hierTable))

dat$pathString <- paste("plotDat",
                        dat$Region, dat$Class, dat$Order, dat$Suborder,
                        dat$Alliance, dat$Association, dat$SiteSeries,
                        dat$PlotNumber, sep = "/")
plotHier <- as.Node(dat)

dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "smithersresearch.ca", 
                 port = 5432, dbname = "bgc_climate_data")

setwd("C:/Users/kirid/Desktop/BGC_Climates_Prod")
temp <- fread("ClimateSummaryCurrent_v11_5.6.csv", data.table = F)
temp <- fread("AllStnModDat.csv", data.table = F)
colnames(temp) <- dbSafeNames(colnames(temp))
dbWriteTable(con, "allstnmoddat", temp, row.names = FALSE, overwrite = TRUE)


setwd("C:/Users/Kiri Daust/Desktop/Vegetation_Classification/Kiri/VProShiny")
suitTable <- fread(file.choose(), sep = " ", header = TRUE, data.table = F)

colnames(vegSave) <- dbSafeNames(colnames(vegSave))
dbWriteTable(con, "bec_hier_small", hierTemp, row.names = FALSE, overwrite = TRUE)

dbGetQuery(con, "SELECT column_name FROM information_schema.columns WHERE table_name = 'bec_env' ORDER BY ordinal_position")
dbGetQuery(con, "SELECT * FROM bec_env WHERE plotnumber LIKE '%-test'")
dbGetQuery(con, "SELECT DISTINCT codes FROM edatopic WHERE special IS NOT NULL")
temp <- dbGetQuery(con, "SELECT * FROM bec_hier")
save(temp, file = "HierTable.RData")

setwd("C:/Users/Kiri Daust/Desktop/Vegetation_Classification")
vegData <- fread("BECMasterVeg_Feb7_2019.txt", sep = " ", data.table = FALSE)
hierTable <- fread(file.choose(), data.table = F)
vegData <- merge(hierTable, vegData, by = "PlotNumber")
vegData[vegData == ""] <- NA
vegData <- vegData[!is.na(vegData$Alliance),]
vegSave <- vegData
vegSub <- vegData[vegData$Species == "PINUPON-1",]
vegData <- vegSub

vegData$pathString <- paste("vegData",
                            vegData$Region, vegData$Class, vegData$Order,
                            vegData$SubOrder, vegData$Alliance, vegData$Association, 
                            vegData$SiteSeries, vegData$PlotNumber, vegData$Species, sep = "_")
vegTree <- as.Node(vegData, pathDelimiter = "_", na.rm = FALSE, colLevels = list(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,"Cover"))
collapsibleTree(vegTree)

data(acme, package="data.tree")
acme$Do(function(node) node$cost <- data.tree::Aggregate(node, attribute = "cost", aggFun = sum))
collapsibleTree(acme, nodeSize = "cost", attribute = "cost", tooltip = TRUE)

pf <- function(x){
  if(x$isLeaf & x$name != "PINUCON2"){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

pf <- function(x){
  if(x$isLeaf && (is.null(x$Cover)||is.na(x$Cover))){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

treeSmall <- Clone(vegTree, pf)
Prune(treeSmall, pf)

treeSmall$Do(function(x) if(is.null(x$Cover)) x$Cover <- 0)

treeSmall$Do(function(x){
  x$Cover <- Aggregate(node = x,
                      attribute = "Cover",
                      aggFun = mean)
}, traversal = "post-order")



collapsibleTree(treeSmall, nodeSize = "Cover", attribute = "Cover", tooltip = TRUE)

tree2 <- Traverse(treeSmall, pruneFun = pf, traversal = "post-order")
pf <- function(x){
  if(x$level > 7){
    return(FALSE)
  }else{
    return(TRUE)
  }
}


vegTreeSave <- vegTree
vegTree <- vegTreeSave


vegTree$totalCount
vegTree$averageBranchingFactor
vegTree$count
vegTree$height
vegTree$`FdiPy WarmInteriorandDry Forest Region`$siblings

print(treeSmall, "Cover")
print(vegTree, "Cover", limit = 100)
treeSmall$Get("Cover", traversal = "post-order", filterFun = function(x) if(x$level == 3) TRUE else FALSE)

###Test data.tree library##################
GenerateChildrenTree <- function(children = 2, 
                                 probSex = c(male = 0.52, female = 0.48), 
                                 probInherit = c(male = 0.8, female = 0.5),
                                 probDevelop = c(male = 0.05, female = 0.01),
                                 generations = 5, 
                                 parent = NULL) {
  
  if (is.null(parent)) {
    parent <- Node$new("1")
    parent$sex <- 1
    parent$feature <- TRUE
    parent$develop <- FALSE
  }
  
  #sex of descendants
  #1 = male
  #2 = female
  sex <- sample.int(n = 2, size = children, replace = TRUE, prob = probSex)
  for (i in 1:children) child <- parent$AddChild(i)
  Set(parent$children, sex = sex)
  
  #inherit
  if (parent$feature == TRUE) {
    for (i in 1:2) {
      subPop <- Traverse(parent, filterFun = function(x) x$sex == i)
      inherit <- sample.int(n = 2, 
                            size = length(subPop), 
                            replace = TRUE, 
                            prob = c(1 - probInherit[i], probInherit[i]))
      
      Set(subPop, feature = as.logical(inherit - 1))
    }
  } else {
    Set(parent$children, feature = FALSE)
  }
  
  #develop
  Set(parent$children, develop = FALSE)
  for (i in 1:2) {
    subPop <- Traverse(parent, filterFun = function(x) x$sex == i && !x$feature)
    develop <- sample.int(n = 2, 
                          size = length(subPop), 
                          replace = TRUE, 
                          prob = c(1 - probDevelop[i], probDevelop[i]))
    Set(subPop, feature = as.logical((develop - 1)), develop = as.logical((develop - 1)))
  }
  
  #recursion to next generation
  if (generations > 0) for (i in 1:children) GenerateChildrenTree(children, 
                                                                  probSex, 
                                                                  probInherit, 
                                                                  probDevelop, 
                                                                  generations - 1, 
                                                                  parent$children[[i]])
  
  return (parent)
}

tree <- GenerateChildrenTree()
print(tree, "sex", "feature", "develop", limit = 20)
length(Traverse(tree, filterFun = function(x) x$feature))

FreqLastGen <- function(tree) {
  l <- tree$leaves
  sum(sapply(l, function(x) x$feature))/length(l)
}

FreqLastGen(tree)
x <- sapply(1:500, function(x) FreqLastGen(GenerateChildrenTree(generations = 3)))
hist(x, probability = TRUE, main = "Frequency of feature in last generation")

data(acme)
#Tree
x <- ToDataFrameTree(acme, "pathString", "p", "cost")
x
xN <- as.Node(x)
print(xN, "p", "cost")
#Table
x <- ToDataFrameTable(acme, "pathString", "p", "cost")
as.Node.dendrogram
x
xN <- FromDataFrameTable(x)
print(xN, "p", "cost")
#More complex Table structure, using colLevels
acme$Set(floor = c(1, 2, 3), filterFun = function(x) x$level == 2)
x <- ToDataFrameTable(acme, "pathString", "floor", "p", "cost")
x
xN <- FromDataFrameTable(x, colLevels = list(NULL, "floor", c("p", "cost")), na.rm = TRUE)
print(xN, "floor", "p", "cost")


temp <- dbGetQuery(con, "SELECT DISTINCT date FROM bec_env")
temp <- dbGetQuery(con, "SELECT DISTINCT * FROM bec_env WHERE zone SIMILAR TO '%(SBS|BWBS|SBPS|ESSF|SWB)%'")
temp2 <- dbGetQuery(con, "SELECT * FROM bec_veg INNER JOIN bec_env ON (bec_veg.plotnumber = bec_env.plotnumber) WHERE 
                    bec_env.zone SIMILAR TO '%(SBS|BWBS|SBPS|ESSF|SWB)%'")

dat <- dbGetQuery(con, paste0("SELECT ",paste(c("bgc","period","var", annual,seasonal),collapse = ","),
                              " FROM climsum_curr_v11 WHERE var = 'mean' AND period = '",
                              "1961 - 1990", "' AND bgc LIKE '",zn.pick,"%'"))
