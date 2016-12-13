
#
#    http://shiny.rstudio.com/
#asdasd

library(ggplot2)
library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Resistencia Ferroviaria"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    tabsetPanel(
      tabPanel(
        "Locomotiva",
        numericInput(
          "nL",
          "Numero de locomotivas:",
          min = 0,
          max = 50,
          value = 2
        ),
        numericInput(
          "gL",
          "Peso [kN]:",
          min = 0,
          max = 50,
          value = 1300
        ),
        numericInput(
          "xL",
          "Numero de eixos:",
          min = 1,
          max = 50,
          value = 4
        ),
        numericInput(
          "aL",
          "Area frontal [m2]",
          min = 0,
          max = 50,
          value = 10
        ),
        numericInput(
          "P",
          "Potencia motor [hp]:",
          min = 0,
          max = 90000,
          value = 3000
        ),
        numericInput(
          "lC1",
          "c1:",
          min = 0,
          max = 90000,
          value = 0.65
        ),
        numericInput(
          "lC2",
          "c2:",
          min = 0,
          max = 90000,
          value = 125
        ),
        numericInput(
          "lC3",
          "c3:",
          min = 0,
          max = 90000,
          value = 0.009
        ),
        numericInput(
          "lCa",
          "ca:",
          min = 0,
          max = 90000,
          value = 0.009
        ),
        numericInput(
          "f",
          "Aderencia das rodas:",
          min = 0,
          max = 10,
          value = 0.2
        )
      ),
      tabPanel(
        "Vagoes",
        numericInput(
          "nV",
          "Numero de vagoes:",
          min = 0,
          max = 500,
          value = 80
        ),
        numericInput(
          "gV",
          "Peso [kN]:",
          min = 0,
          max = 50000,
          value = 1100
        ),
        numericInput(
          "xV",
          "Quantidade de eixos:",
          min = 1,
          max = 50,
          value = 4
        ),
        numericInput(
          "aV",
          "Area frontal [m2]",
          min = 0,
          max = 50,
          value = 8.5
        ),
        numericInput(
          "vC1",
          "c1:",
          min = 0,
          max = 90000,
          value = 0.65
        ),
        numericInput(
          "vC2",
          "c2:",
          min = 0,
          max = 90000,
          value = 125
        ),
        numericInput(
          "vC3",
          "c3:",
          min = 0,
          max = 90000,
          value = 0.009
        ),
        numericInput(
          "vCa",
          "ca:",
          min = 0,
          max = 90000,
          value = 0.009
        )
      ),
      tabPanel(
        "Curva",
        numericInput(
          "r",
          "Raio da rampa [m]:",
          min = 0,
          max = 50000,
          value = 0
        )
      ),
      tabPanel(
        "Rampa",
        numericInput(
          "i",
          "Inclininacao da rampa:",
          min = 0,
          max = 50,
          value = 0
        )
      ),
      tabPanel(
        "Velocidade",
        numericInput(
          "vmax",
          "Velocidade maxima:",
          min = 0,
          max = 9000,
          value = 120
        ),
        numericInput(
          "vmin",
          "Velocidade minima:",
          min = 0,
          max = 5000,
          value = 10
        )
      )
    )
  ),
  # Show a plot of the generated distribution
  mainPanel(plotlyOutput("distPlot")))
))
