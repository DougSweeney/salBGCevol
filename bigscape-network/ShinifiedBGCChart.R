library(shiny)
library(shinythemes)

setwd("./")
xcoord <- read.csv("x axis.txt", sep = "\t", col.names = c("X1", "X2", "BGC"), header = F)
ycoord <- read.csv("y axis.txt", sep = "\t", col.names = c("y1", "y2", "Strain"), header = F)
datafile <- read.csv("salinisporaBGC-clustering.csv", col.names = c("AntiSmash5_GBK", "Strain", "Species", "GCF", "BGC_T1", "BGC_T2", "BGC_T3", "BGC_T4"))
datafile$Strain <- sub("_.{2,3}_...$", "", datafile$Strain)

ui<-fluidPage(
    titlePanel("Salinispora GeneClusterFamilies by Phylogeny"),
    tabsetPanel(type="tabs",
                tabPanel("Image", fixedPanel(verbatimTextOutput("info")),imageOutput("image1", click="image_click")),
                tabPanel("GCF Table", column(12, tableOutput("table1")))
                )
)

server <- function(input, output, session){

  output$image1 <- renderImage({
    width  <- session$clientData$output_image1_width
    height <- session$clientData$output_image1_height
    list(
      src = "GCF2mod.abundance-01.png",
      contentType = "image/png",
      width = "width",
      height = "height"
    )
  })
    
    charZ <- character(0)
  
    output$info <- renderPrint({
    xcoord <- filter(filter(xcoord, X1 <= as.numeric(input$image_click$x)), X2 > as.numeric(input$image_click$x))
    ycoord <- filter(filter(ycoord, y1 <= as.numeric(input$image_click$y)), y2 > as.numeric(input$image_click$y))
    
    #Filter datafile for xcoord$BGC and verify ycoord$Strain is there. If yes, return bgc link, else return "ycoord$Strain doesnot contain xcoord$BGC"
    dat1 <- filter(datafile, as.character(datafile$GCF) == as.character(xcoord$BGC))
    dat <- filter(dat1, dat1$Strain == as.character(ycoord$Strain))
    
    print(paste("Strain:", as.character(ycoord$Strain)))
    print(paste("BGC Family:", as.character(xcoord$BGC)))

    if (identical(as.character(dat$AntiSmash5_GBK), charZ)) {
      print(paste("Strain", ycoord$Strain, "does not contain", xcoord$BGC))
    } else {
      print(paste("BGC GBK File: ", as.character(dat$AntiSmash5_GBK), ".gbk", sep=""))
    }
    print(paste("Strain AntiSmash5 File: ", "https://github.com/alex-b-chase/salBGCevol/blob/master/antismash5/",ycoord$Strain,".tar.gz", sep=""))
  })
    
    output$table1 <- renderTable({
      xcoord <- filter(filter(xcoord, X1 <= as.numeric(input$image_click$x)), X2 > as.numeric(input$image_click$x))
      dat1 <- filter(datafile, as.character(datafile$GCF) == as.character(xcoord$BGC))
      dat1
      })
}

shinyApp(ui, server)