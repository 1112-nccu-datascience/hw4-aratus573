library(shiny)
library(ggbiplot)
library(ca)
library(factoextra)
data(iris)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("NCCU_DS2023_hw4_108703011"),
  
  tabsetPanel(
    tabPanel("Correspondence Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "X",
                             label = "X:",
                             choices = c("PC1", "PC2", "PC3", "PC4"),
                             selected = "PC1"),
                 
                 selectInput(inputId = "Y",
                             label = "Y:",
                             choices = c("PC1", "PC2", "PC3", "PC4"),
                             selected = "PC2")
               ),
               mainPanel(
                 plotOutput(outputId = "pcaPlot")
               )
             )
    ),
    tabPanel("Correspondence Analysis",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "centers",
                             label = "centers:",
                             min = 3,
                             max = 10,
                             value = 3)
               ),
               mainPanel(
                 plotOutput(outputId = "caPlot")
               )
             )
    ),
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$pcaPlot <- renderPlot({
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    
    # Get the selected component indices
    x_component <- switch(input$`X`,
                          "PC1" = 1,
                          "PC2" = 2,
                          "PC3" = 3,
                          "PC4" = 4)
    
    y_component <- switch(input$`Y`,
                          "PC1" = 1,
                          "PC2" = 2,
                          "PC3" = 3,
                          "PC4" = 4)
    
    # Perform PCA using the selected components
    ir.pca <- prcomp(log.ir[, c(x_component, y_component)], center = TRUE, scale. = TRUE)
    
    g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    g <- g + xlab(paste0("PC", x_component))
    g <- g + ylab(paste0("PC", y_component))
    
    return(g)
  })
  
  output$caPlot <- renderPlot({
    # Perform Correspondence Analysis
    ca <- ca(iris[, 1:4])
    plot(ca, invisible = "ind")
    k <- kmeans(iris[, 1:4], centers = input$centers)
    fviz_cluster(k, data = iris[, 1:4], geom = "point", frame.type = "norm")
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
