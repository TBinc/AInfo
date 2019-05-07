#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("D:/WD")

pkg = read.csv("package.csv")
pdt = read.csv("product.csv")

#Normalizacion Maxima
NormMaxima = function(dataset1){
    if (is.integer(dataset1)) {
        t = dataset1
    }
    else{
        t = as.integer(factor(dataset1, 
                              levels = sort(unique(dataset1, incomparables = FALSE)),
                              labels = c(0:(length(unique(dataset1, incomparables = FALSE))-1))))
    }
    
    aux = t / max(t)
    return(aux)
}
#Normalizacion Lineal
NormLineal = function(dataset1){
    if (is.integer(dataset1)) {
        t = dataset1
    }
    else{
        t = as.integer(factor(dataset1, 
                              levels = sort(unique(dataset1, incomparables = FALSE)),
                              labels = c(0:(length(unique(dataset1, incomparables = FALSE))-1))))
    }
    mini = min(t)
    maxi = max(t)
    
    aux = ((t - mini)/(maxi - mini))
    
    return(aux)
}
#Normalizacion Categorica
NormCategorica = function(dataset1){
    if (is.integer(dataset1)) {
        return()
    }
    t = as.integer(factor(dataset1, 
                          levels = sort(unique(dataset1, incomparables = FALSE)),
                          labels = c(0:(length(unique(dataset1, incomparables = FALSE))-1))))
    return(t)
}
#Normalizacuion de Package
PackageNorm = function(){
    
    PRODUCTID = pkg$PRODUCTID
    NDCPACKAGECODE = pkg$NDCPACKAGECODE
    STARTMARKETINGYEAR = trunc(cpyPk$STARTMARKETINGDATE[] / 10000)
    NCD_EXCLUDE_FLAG = (NormCategorica(pkg$NDC_EXCLUDE_FLAG)-1)
    SAMPLE_PACKAGE = (NormCategorica(pkg$SAMPLE_PACKAGE) - 1)
    
    pack = data.frame(PRODUCTID, NDCPACKAGECODE,
                      STARTMARKETINGYEAR, NCD_EXCLUDE_FLAG, 
                      SAMPLE_PACKAGE)
    write.csv(pack, file = "package.mat", row.names = FALSE)
    
    return(pack)
}
PkgN = PackageNorm()
#Normalizacion de Product
ProductNorm = function(){
    PRODUCTID = pdt$PRODUCTID
    PRODUCTYPE = NormCategorica(pdt$PRODUCTTYPENAME)
    PROPIETARY = NormCategorica(pdt$PROPRIETARYNAME)
    NONPROPIETARYNAME = pdt$NONPROPRIETARYNAME
    DOSAGEFORM = NormCategorica(pdt$DOSAGEFORMNAME)
    ROUTNAME = NormCategorica(pdt$ROUTENAME)
    STARTMARKETINGYEAR = trunc(pdt$STARTMARKETINGDATE / 10000)
    MARKETINGCATEGORYNAME = NormCategorica(pdt$MARKETINGCATEGORYNAME)
    LABELERNAME = NormCategorica(pdt$LABELERNAME)
    SUBSTANCENAME = NormCategorica(pdt$SUBSTANCENAME)
    NDC_EXCLUDE_FLAG = NormCategorica(pdt$NDC_EXCLUDE_FLAG) - 1
    
    prod = data.frame(PRODUCTID,PRODUCTYPE,
                      PROPIETARY,NONPROPIETARYNAME,
                      DOSAGEFORM,ROUTNAME,
                      STARTMARKETINGYEAR,MARKETINGCATEGORYNAME,
                      LABELERNAME, SUBSTANCENAME,NDC_EXCLUDE_FLAG)
    write.csv(prod, file = "product.mat", row.names = FALSE)
    return(prod)
}
PdtN = ProductNorm()

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tarea Academica 1"),
    tabsetPanel(
        tabPanel("Base",
                 sidebarLayout(
                     sidebarPanel(
                         wellPanel(selectInput(inputId = "dt1", 
                                               label = "Seleccione un dataset", 
                                               choices = c("Package", "Product")))
                     ),
                     mainPanel(dataTableOutput("tabla"))
                 )    
        ),
        tabPanel("Graficos",
            sidebarLayout(
                sidebarPanel(
                    wellPanel(selectInput(inputId = "hist",
                            label = NULL,
                            choices = c("Product Type",
                                        "Propietary")
                        )
                    )
                ),
                mainPanel(plotOutput("graf"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    datasetInput1 = reactive({
     switch(input$dt1,
            "Package" = pkg,
            "Product" = pdt)   
    })
    
    output$tabla = renderDataTable(datasetInput1())
    
    datasetInput2 = reactive({
        switch(input$hist,
               "Product Type" = hist(PdtN$PRODUCTYPE,col = c("blue","lightblue", "grey", "red")),
               "Propietary" = hist(PdtN$PROPIETARY)
               )
    })
    
    output$graf = renderPlot(datasetInput2())
}

# Run the application 
shinyApp(ui = ui, server = server)
