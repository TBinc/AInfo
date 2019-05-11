#setwd("C:/Users/Mariano/Desktop/Ainfo trabajo")
#setwd("C:/Users/nilto/Desktop/Trabajo Parcial Adminfo")
setwd("C:/Users/josem/Desktop/Jose/UPC/Ciclo VI/Admin Info/Parcial")
d1 = read.table("Australian Marriage Dataset.csv",sep=";",header=TRUE)
d2 = read.table("FINANCIAL.csv",sep = ",",header=TRUE)
options(max.print=999999)

#DISTANCIAS FUNCIONES
MikowskiDistance = function(r, dataBase, mLength, posArray,dsLevels){
  rinf = 0
  if(r == 0){
    r = 2
    rinf = 1
  }
  resultMatrix = matrix(1:(mLength*mLength),nrow = mLength, dimnames = list(c(dsLevels),c(dsLevels)))
  for (i in 1:mLength){
    for (j in 1:mLength){
      distance = 0
      for (column in posArray)
        distance = distance + abs(dataBase[i,column] - dataBase[j,column])**r
      distance = distance**(1/r)
      if(rinf == 1) distance = trunc(distance)
      resultMatrix[j,i] = distance
    }
  }
  return(resultMatrix)
}

ChebyshovDistance = function(dataBase, mLength, posArray,dsLevels){
  resultMatrix = matrix(1:(mLength*mLength),nrow = mLength, dimnames = list(c(dsLevels),c(dsLevels)))
  for (i in 1:mLength){
    for (j in 1:mLength){
      chebDist = 0
      for(k in 1:length(posArray)){
        if(abs(dataBase[i,posArray[k]] - 
               dataBase[j,posArray[k]])>chebDist){
          chebDist = abs(dataBase[i,posArray[k]] - 
                           dataBase[j,posArray[k]])
        }
      }
      resultMatrix[j,i] = chebDist
    }
  }
  return(resultMatrix)
}


#NORMALIZACIONES FUNCIONES
LinealNormalization = function(dataBase, mLength,dsLevels){
  LMatrix = dataBase
  for(column in dsLevels){
    minVal = min(dataBase[,column])
    maxVal = max(dataBase[,column])
    sust = maxVal - minVal
    for(i in 1:mLength)
      LMatrix[i,column] = (dataBase[i,column] - minVal)/ sust
  }
  return (LMatrix)
}

StanDesviNormalizat = function(dataBase, mLength,DS_Levels){
  SDMatrix = dataBase
  for(column in DS_Levels){
    meanVal = mean(dataBase[,column])
    stdVal = sd(dataBase[,column])
    for(i in 1:mLength)
      SDMatrix[i,column] = (dataBase[i,column] -meanVal)/stdVal
  }
  return (SDMatrix)
}

MaxValNormalizat = function(dataBase, mLength,DS_Levels){
  MVMatrix = dataBase
  for(column in DS_Levels){
    maxVal = max(dataBase[,column])
    for(i in 1:mLength)
      MVMatrix[i,column] = (dataBase[i,column])/maxVal
  }
  return (MVMatrix)
}

#MAIN FUNCTION DATASET 1
#positionsToEvaluate = c(3,5,7,9,11,13,15)
positionsToEvaluate = c(2,3,5,7,9,11,13,15)
matrixLength = length(d1[,1])
DS_Levels = as.character(d1[1:matrixLength,1])

#MAPEO DE CATEGÓRICO A NUMÉRICO DATASET 1
d1$Federal.Electoral.Division = factor(d1$Federal.Electoral.Division,
          levels = c('New South Wales Divisions',
                     'Victoria Divisions',
                     'Queensland Divisions',
                     'South Australia Divisions',
                     'Western Australia Divisions',
                     'Tasmania Divisions',
                     'Northern Territory Divisions',
                     'Australian Capital Territory Divisions'),
          labels = c(1,2,3,4,5,6,7,8))

d1$Federal.Electoral.Division = as.numeric(as.character(d1$Federal.Electoral.Division))

d1

#Matrices de distancia antes de normalizar DATASET 1
manhattanMatrixBN = MikowskiDistance(1,d1,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = manhattanMatrixBN, file = "1.ManhattanMatrixBN.dmat")

euclideanMatrixBN = MikowskiDistance(2,d1,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = euclideanMatrixBN, file = "2.EuclideanMatrixBN.dmat")

mikowskiP3MatrixBN = MikowskiDistance(3,d1,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = mikowskiP3MatrixBN, file = "3.MikowskiP3MatrixBN.dmat")

suprerumMatrixBN = MikowskiDistance(0,d1,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = suprerumMatrixBN, file = "4.SuprerumMatrixBN.dmat")

chebyshovMatrixBN = ChebyshovDistance(d1,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = chebyshovMatrixBN, file = "5.ChebyshovMatrixBN.dmat")

#Matrices de Normalizacion DATASET 1
linealNormalizMatrix = LinealNormalization(d1,matrixLength,positionsToEvaluate)
write.table(x = linealNormalizMatrix,file = "6.LinealNormalizationMatrix.dat")

desvStandNormalizMatrix = StanDesviNormalizat(d1,matrixLength, positionsToEvaluate)
write.table(x = desvStandNormalizMatrix, file = "7.SDNormalizationMatrix.dat")

maxValNormalizMatrix = MaxValNormalizat(d1,matrixLength, positionsToEvaluate)
write.table(x = maxValNormalizMatrix, file = "8.MaxValNormalizationMatrix.dat")

#Matrices de distancia después de normalizar por desviación estándar DATASET 1
manhattanMatrixAN = MikowskiDistance(1,desvStandNormalizMatrix,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = manhattanMatrixAN, file = "9.ManhattanMatrixAN.dmat")

euclideanMatrixAN = MikowskiDistance(2,desvStandNormalizMatrix,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = euclideanMatrixAN, file = "10.EuclideanMatrixAN.dmat")

mikowskiP3MatrixAN = MikowskiDistance(3,desvStandNormalizMatrix,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = mikowskiP3MatrixAN, file = "11.MikowskiP3MatrixAN.dmat")

suprerumMatrixAN = MikowskiDistance(0,desvStandNormalizMatrix,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = suprerumMatrixAN, file = "12.SuprerumMatrixAN.dmat")

chebyshovMatrixAN = ChebyshovDistance(desvStandNormalizMatrix,matrixLength,positionsToEvaluate,DS_Levels)
write.table(x = chebyshovMatrixAN, file = "13.ChebyshovMatrixAN.dmat")

#MAIN FUNCTION DATASET 2

#ELIMINAR NA POR 0
matrixLengthD2 = length(d2[,1])

for(i in 1:matrixLengthD2){
  if(is.na(d2[i,5])){
    d2[i,5] = 0
  }
}

#REDUCIR DATASET A DETERMINADO PERIODO
shortD2 = d2[1:4470,1:8]

#OBTENER PERSONAS DEL DATASET
matrixLengthD2 = length(shortD2[,1])
tmp = levels(factor(as.character(shortD2[1,2])))
politicsVec = vector()
for(i in 1:matrixLengthD2){
  if(tmp != levels(factor(as.character(shortD2[i,2])))){
    politicsVec = c(politicsVec,tmp)
    tmp = levels(factor(as.character(shortD2[i,2])))
  }
  if(i == matrixLengthD2){
    politicsVec = c(politicsVec,tmp)
  }
}
#View(politicsVec)

#CREAR NUEVO VECTOR DE PERSONAS Y DONACIONES
newD2Length = length(politicsVec)
newD2Matrix = matrix(1:(newD2Length),nrow = newD2Length,
                     dimnames = list(c(politicsVec),c('amount')))

#CALCULAR Y GUARDAR DONACIONES DE CADA PERSONA
donationTmp = 0
initialPos = 1
for(i in 1:length(politicsVec)){
  for(j in initialPos:matrixLengthD2){
    tmp = levels(factor(as.character(shortD2[j,2])))
    if(politicsVec[i] == tmp){
      donationTmp = donationTmp + shortD2[j,5]
    }else if(politicsVec[i] != tmp){
      initialPos = j
      #print(j)
      newD2Matrix[i,1] = donationTmp
      donationTmp = 0
      break
    }
    if(j == matrixLengthD2){
      newD2Matrix[i,1] = donationTmp
    }
  }
}
#View(newD2Matrix)

positionsToEvaluateDist2 = c(1)

#Matrices de distancia antes de normalizar DATASET 2
d2ManhattanMatrixBN = MikowskiDistance(1,newD2Matrix,newD2Length,
                                       positionsToEvaluateDist2,
                                       politicsVec)
write.table(x = d2ManhattanMatrixBN, file = "21.D2ManhattanMatrixBN.dmat")

d2EuclideanMatrixBN = MikowskiDistance(2,newD2Matrix,newD2Length,
                                       positionsToEvaluateDist2,
                                       politicsVec)
write.table(x = d2EuclideanMatrixBN, file = "22.D2EuclideanMatrixBN.dmat")

d2MikowskiP3MatrixBN = MikowskiDistance(3,newD2Matrix,newD2Length,
                                        positionsToEvaluateDist2,
                                        politicsVec)
write.table(x = d2MikowskiP3MatrixBN, file = "23.D2MikowskiP3MatrixBN.dmat")

d2SuprerumMatrixBN = MikowskiDistance(0,newD2Matrix,newD2Length,
                                      positionsToEvaluateDist2,
                                      politicsVec)
write.table(x = d2SuprerumMatrixBN, file = "24.D2SuprerumMatrixBN.dmat")

d2ChebyshovMatrixBN = ChebyshovDistance(newD2Matrix,newD2Length,
                                        positionsToEvaluateDist2,
                                        politicsVec)
write.table(x = d2ChebyshovMatrixBN, file = "25.D2ChebyshovMatrixBN.dmat")

#Matrices de Normalizacion DATASET 2
d2LinealNormalizMatrix = LinealNormalization(newD2Matrix,newD2Length,
                                             positionsToEvaluateDist2)
write.table(x = d2LinealNormalizMatrix,file = "26.D2LinealNormalizationMatrix.dat")

d2DesvStandNormalizMatrix = StanDesviNormalizat(newD2Matrix,
                                                newD2Length,
                                                positionsToEvaluateDist2)
write.table(x = d2DesvStandNormalizMatrix, file = "27.D2SDNormalizationMatrix.dat")

d2MaxValNormalizMatrix = MaxValNormalizat(newD2Matrix,newD2Length,
                                          positionsToEvaluateDist2)
write.table(x = d2MaxValNormalizMatrix, file = "28.D2MaxValNormalizationMatrix.dat")

#Matrices de distancia después de normalizar por desviación estándar DATASET 2
d2ManhattanMatrixAN = MikowskiDistance(1,d2DesvStandNormalizMatrix,
                                       newD2Length,
                                       positionsToEvaluateDist2,
                                       politicsVec)
write.table(x = d2ManhattanMatrixAN, file = "29.ManhattanMatrixAN.dmat")

d2EuclideanMatrixAN = MikowskiDistance(2,d2DesvStandNormalizMatrix,
                                       newD2Length,
                                       positionsToEvaluateDist2,
                                       politicsVec)
write.table(x = d2EuclideanMatrixAN, file = "30.EuclideanMatrixAN.dmat")

d2MikowskiP3MatrixAN = MikowskiDistance(3,d2DesvStandNormalizMatrix,
                                        newD2Length,
                                        positionsToEvaluateDist2,
                                        politicsVec)
write.table(x = d2MikowskiP3MatrixAN, file = "31.MikowskiP3MatrixAN.dmat")

d2SuprerumMatrixAN = MikowskiDistance(0,d2DesvStandNormalizMatrix,
                                       newD2Length,
                                       positionsToEvaluateDist2,
                                       politicsVec)
write.table(x = d2SuprerumMatrixAN, file = "32.SuprerumMatrixAN.dmat")

d2ChebyshovMatrixAN = ChebyshovDistance(d2DesvStandNormalizMatrix,
                                        newD2Length,
                                        positionsToEvaluateDist2,
                                        politicsVec)
write.table(x = d2ChebyshovMatrixAN, file = "33.ChebyshovMatrixAN.dmat")



#INTENTO CREAR MATRIZ DE TODOS LOS NOMBRES DEL DATASET 2
# matrixLengthD2 = length(d2[,1])
# t = levels(factor(as.character(d2[1:matrixLengthD2,2])))
# newD2Length = length(t)
# newD2Matrix = matrix(1:(newD2Length),nrow = newD2Length,
#                      dimnames = list(c(t),c('amount')))

#ELIMINAR VACÍOS POR 0
# for(i in 1:matrixLengthD2){
#   if(is.na(d2[i,5])){
#     d2[i,5] = 0
#   }
# }

#COMPARAR NOMBRES DE PERSONAS
# tmp = levels(factor(as.character(d2[1,2])))
# print(tmp)
# if(t[2] == tmp){
#   print(TRUE)
# }

#INTENTOS POR ORDENAR LA MATRIZ POR NOMBRE DE PERSONA
#newD2 = d2[order(d2$member_name),]
#View(newD2)

#INTENTOS POR ORDENAR LA MATRIZ POR NOMBRE DE PERSONA
# newD2 = sort(d2[,2])
# View(newD2)

#INTENTOS POR OBTENER EL TOTAL DE DONACIONES POR PERSONAS
# donationTmp = 0
# initialPos = 1
# for(i in 1:length(t)){
#   for(j in initialPos:matrixLengthD2){
#     tmp = levels(factor(as.character(d2[j,2])))
#     if(t[i] == tmp){
#       donationTmp = donationTmp + d2[j,5]
#     }else if(t[i] != tmp){
#       initialPos = j
#       print(j)
#       newD2Matrix[i,1] = donationTmp
#       donationTmp = 0
#       break
#     }
#   }
# }

#INTENTOS POR ORDENAR LA MATRIZ POR NOMBRE DE PERSONA
# alt = c("hola","adios","que","besos")
# n = sort(alt)
# print(alt)
# print(n)

#INTENTOS POR ORDENAR LA MATRIZ POR NOMBRE DE PERSONA
# tmp = levels(factor(as.character(d2[1,2])))
# politicsVec = vector()
# for(i in 1:matrixLengthD2){
#   if(tmp != levels(factor(as.character(d2[i,2])))){
#     tmp = levels(factor(as.character(d2[i,2])))
#     politicsVec = c(politicsVec,tmp)
#   }
# }

#INTENTOS POR OBTENER EL TOTAL DE DONACIONES POR PERSONAS
# for(i in 1:matrixLengthD2){
#   tmp = levels(factor(as.character(d2[i,2])))
#   for(j in initialPos:length(t)){
#     if(tmp == t[j]){
#       donationTmp = donationTmp + d2[i,5]
#     }else{
#       initialPos = j
#       newD2Matrix[j,1] = donationTmp
#       donationTmp = 0
#       break
#     }
#   }
# }


#Visualización
library(shiny)
runExample("01_hello")
install.packages("dplyr")
library(dplyr)

#grafico manual
vars=table(d2$amount,d2$type_code)
d2$amount = ifelse(is.na(d2$amount),0,d2$amount)
a = d2 %>% group_by(session,type_code) %>% summarise(amount = sum(amount, is.na = FALSE))

barplot(
        a$type_code,
        height = a$amount, 
        #legend = unique(c(a$type_code)),
        col =c("red","blue","green",
               "yellow","cyan","black",
               "purple","magenta","lightblue",
               "darkgray","gold3"),
        ylab = "CANTIDAD",xlab = "Sesion",names.arg = a$session,
        main = "Distribucion de donaciones segun sesion por codigo",
        horiz = FALSE,
        border = "black",
        legend.text = unique(c(a$type_code))
       )

#title(main="Distribucion de donaciones segun sesion por codigo", sub=NULL, xlab="sesion", ylab="cantidad")


# Define UI for application that draws a histogram
# ui <- fluidPage(
#   # Application title
#   titlePanel("Trabajo Parcial"),
#   tabsetPanel(
#     tabPanel("Base de Datos",
#              sidebarLayout(
#                sidebarPanel(
#                  wellPanel(selectInput(inputId = "dt1", 
#                                        label = "Seleccione un Dataset", 
#                                        choices = c("Database 8", "Database 13")))
#                ),
#                mainPanel(dataTableOutput("tabla"))
#              )    
#     ),
#     tabPanel("Graficos",
#              sidebarLayout(
#                sidebarPanel(
#                  wellPanel(selectInput(inputId = "hist",
#                                        label = NULL,
#                                        choices = c("Product Type",
#                                                    "Propietary")
#                  )
#                  )
#                ),
#                mainPanel(plotOutput("graf"))
#              )
#     ),
#     tabPanel("Matriz Distancia", mainPanel(dataTableOutput("tabla"))),
#     tabPanel("Matriz Normalizada", mainPanel(dataTableOutput("tabla")))
#   )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#   datasetInput1 = reactive({
#     switch(input$dt1,
#            "Database 8" = d1,
#            "Database 13" = d1)   
#   })
#   
#   output$tabla = renderDataTable(datasetInput1())
#   
#   datasetInput2 = reactive({
#     switch(input$hist,
#            "Product Type" = hist(PdtN$PRODUCTYPE,col = c("blue","lightblue", "grey", "red")),
#            "Propietary" = hist(PdtN$PROPIETARY)
#     )
#   })
#   
#   output$graf = renderPlot(datasetInput2())
# }

# Run the application 
shinyApp(ui = ui, server = server)
