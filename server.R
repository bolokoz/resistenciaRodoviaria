#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlotly({
    rLr = function(x)
      (input$lC1 + (input$lC2 * input$xL / input$gL) + input$lC3 * x)
    rVr = function(x)
      (input$vC1 + (input$vC2 * input$xV / input$gV) + input$vC3 * x)
    rr = function(x)
      ((input$vC1 + (input$vC2 * input$xV / input$gV) + input$vC3 * x) * input$nV
       + (input$lC1 + (input$lC2 * input$xL / input$gL) + input$lC3 * x) * input$nL)

    rLa = function(x)
      (input$lCa * x ^ 2 * input$aL)
    rVa = function(x)
      (input$vCa * x ^ 2 * input$aV)
    ra = function(x)
      (input$vCa * x ^ 2 * input$aV) + (input$lCa * x ^ 2 * input$aL)

    rLg = (input$gL * 10 * input$i)
    rVg = (input$gV * 10 * input$i)
    rg = rLg + rVg

    if (input$r != 0) {
      rLc = (698 * input$gL / input$r)
      rVc = (698 * input$gV / input$r)
      rc = rLc + rVc

    } else{
      rc = 0
    }


    pot = function(x)
      (input$nL * input$P * 2175 / x)


    vel = seq(input$vmin, input$vmax, 0.1)
    po = pot(vel) / 1000
    rt = rr(vel) + ra(vel) + rc + rg

    data = data.frame(vel, po, rt)
    
    layout_settings_x = list(
      title = " Velocidade [kmh]",
      dtick = 10
      
      
    )
    
    layout_settings_y = list(
      title = " Forca [kN]",
      exponentformat = "E",
      dtick = 200
      
    )


    plot_ly(data, x = ~ vel) %>%
      add_trace(y = ~ po, mode = 'lines', type='scatter', name = "Forca Motriz") %>%
      add_trace(y = ~ rt, mode = 'lines', type='scatter', name = "Resistencia total ") %>%
      layout(title = " Forca x Velocidade",
             xaxis = layout_settings_x,
             yaxis = layout_settings_y)

  })
  #########
  # HOVER FRAME

  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d))
      "Hover events appear here (unhover to clear)"
    else
      d
  })
  
  output$table <- renderTable({
    
    resistences = c("Rolamento", "Aerodinamica", "Curva", "Rampa")
    tipos = c("1 Locomotiva","1 Vagao")
    
    txt_rrl = paste(formatC(input$lC1 + (input$lC2 * input$xL / input$gL))," + ", input$lC3, "* v")
    txt_ral = paste(input$lCa * input$aL, "* v^2")
    txt_rgl = paste(input$gL * 10 * input$i)
    
    txt_rrv = paste(formatC(input$vC1 + (input$vC2 * input$xV / input$gV))," + ", input$vC3, "* v")
    txt_rav = paste(input$vCa * input$aV, "* v^2")
    txt_rgv = paste(input$gV * 10 * input$i)
    
    
    if (input$r != 0) {
      txt_rcl = (698 * input$gL / input$r)
      txt_rcv = (698 * input$gV / input$r)
    } else{
      txt_rcl = 0
      txt_rcv = 0
    }
    
    locomotiva = c(txt_rrl, txt_ral, txt_rgl, txt_rcl)
    vagao = c(txt_rrv, txt_rav, txt_rgv, txt_rcv)
    
    Rolamento = c(txt_rrl, txt_rrv)
    Aero = c(txt_ral, txt_rav)
    Rampa = c(txt_rgl, txt_rgv)
    Curva = c(txt_rcl, txt_rcv)
    
    
    df = data.frame(Rolamento,Aero,Curva,Rampa)
    row.names(df) = tipos
    df
    
  },rownames=TRUE)
  
  #######
  # SHOW RL

  output$rL <- renderText({

    if (input$r != 0) {
      rLc = (698 * input$gL / input$r)
    } else{
      rLc = 0
    }

    paste("Resistencia de uma locomotiva",
          formatC(input$lC1 + (input$lC2 * input$xL / input$gL) + rLc + (input$gL * 10 * input$i)),
          " + ",
          input$lC3 , "*v",
          " + ",
          input$lCa * input$aL, "*v^2")
  })
  
  #######
  # SHOW RV

  output$rV <- renderText({

    if (input$r != 0) {
      rLc = (698 * input$gL / input$r)
    } else{
      rLc = 0
    }
    
    paste("Resistencia de um vagao",
          formatC(input$lC1 + (input$lC2 * input$xL / input$gL) + rLc + (input$gL * 10 * input$i)),
          " + ",
          input$lC3 , "*v",
          " + ",
          input$lCa * input$aL, "*v^2")
  })
  
  #######
  # SHOW Rt FORMULA
  
  output$rr <- renderText({
    rr = function(x)
      (input$vC1 + (input$vC2 * input$xV / input$gV) + input$vC3 * x) + (input$lC1 + (input$lC2 * input$xL / input$gL) + input$lC3 * x)
    vel = seq(input$vmin, input$vmax)
    
    rv_c = (input$vC1 + (input$vC2 * input$xV / input$gV)) * input$nV
    rv_b = input$vC3 * input$nV
    rv_a = (input$vCa * input$aV * input$nV)
    
    rl_c = (input$lC1 + (input$lC2 * input$xL / input$gL)) * input$nL
    rl_b = input$lC3 * input$nL
    rl_a = (input$lCa * input$aL * input$nL)
    
    paste0("Resistencia de rolamento: ",
           formatC(rv_c + rl_c), " + ",
           rl_b + rv_b, " * v + ",
           rl_a + rv_a, " v^2")
  })
  
}
