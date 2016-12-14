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
      (input$lC1 + (input$lC2 * input$xL / input$gL) + input$lC3 * x) * input$gL
    rVr = function(x)
      (input$vC1 + (input$vC2 * input$xV / input$gV) + input$vC3 * x)* input$gV
    rr = function(x)
      ((input$vC1 + (input$vC2 * input$xV / input$gV) + input$vC3 * x) * input$nV * input$gV
       + (input$lC1 + (input$lC2 * input$xL / input$gL) + input$lC3 * x) * input$nL* input$gL)

    rLa = function(x)
      (input$lCa * x ^ 2 * input$aL)
    rVa = function(x)
      (input$vCa * x ^ 2 * input$aV)
    ra = function(x)
      (input$vCa * x ^ 2 * input$aV) * input$nV + (input$lCa * x ^ 2 * input$aL) *input$nL

    rLg = (input$gL * 10 * input$i)*input$nL
    rVg = (input$gV * 10 * input$i)*input$nV
    rg = rLg + rVg

    if (input$r != 0) {
      rLc = (698 * input$gL / input$r)*input$nL
      rVc = (698 * input$gV / input$r)*input$nV
      rc = rLc + rVc

    } else{
      rc = 0
    }


    ft1 = function(x)
      (input$nL * input$P * 2175 / x)


    vel = seq(input$vmin, input$vmax*1.1, 0.1)
    ft = ft1(vel) / 1000
    rt = (rr(vel)  + ra(vel)  + rc  + rg)/1000
    
    vel_lim = c(input$vmin,input$vmax)
    ftmax = input$nL * input$gL * input$f

    data = data.frame(vel, ft, rt)
    
    layout_settings_x = list(
      title = " Velocidade [kmh]",
      dtick = 10,
      rangemode = "nonnegative"
    )
    
    layout_settings_y = list(
      title = " Forca [kN]",
      exponentformat = "E",
      dtick = 200,
      rangemode = "nonnegative",
      range = c(0, ftmax*1.2 )
    )
    



    plot_ly(data, x = ~ vel) %>%
      add_trace(y = ~ ft, mode = 'lines', type='scatter', name = "Forca Motriz") %>%
      add_trace(y = ~ rt, mode = 'lines', type='scatter', name = "Resistencia total ") %>%

      add_trace(x = c(0,30), y = ftmax, name = "Limite aderencia",
                mode = "lines", type='scatter',
                line = list(dash='dash')) %>%
      
      add_trace(x = input$vmax, y = c(0,300), name = "Limite velocidade",
                mode = "lines",type='scatter',
                line = list(dash='dash')) %>%
      # add_trace(x = c(0,110), y = c(800,800),name = "Limite Corrente Eletrica",
      #           mode = "lines", type='scatter') %>%
      
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
    tipos = c("1 Locomotiva","1 Vagao", "Todas locomotivas", "Todos vagoes")
    
    txt_rrl = paste(formatC((input$lC1 + (input$lC2 * input$xL / input$gL))* input$gL)," + ", input$lC3 * input$gL, "* v")
    txt_ral = paste(input$lCa * input$aL, "* v^2")
    txt_rgl = paste(input$gL * 10 * input$i)
    
    txt_rrv = paste(formatC((input$vC1 + (input$vC2 * input$xV / input$gV))* input$gL)," + ", input$vC3* input$gL, "* v")
    txt_rav = paste(input$vCa * input$aV, "* v^2")
    txt_rgv = paste(input$gV * 10 * input$i)
    
    ##calculo para todos os veiculos
    
    txt_trrl = paste(formatC((input$lC1 * input$nL * input$gL+ (input$lC2 * input$xL / input$gL)* input$gL* input$nL))," + ", input$lC3 * input$gL * input$nL, "* v")
    txt_tral = paste(input$lCa * input$aL* input$nL, "* v^2") 
    txt_trgl = paste(input$gL * 10 * input$i * input$nL) 
    
    txt_trrv = paste(formatC((input$vC1 * input$nV * input$gV+ (input$vC2 * input$xV / input$gV)* input$gV* input$nV))," + ", input$vC3 * input$gV * input$nV, "* v")
    txt_trav = paste(input$vCa * input$aV * input$nV, "* v^2") 
    txt_trgv = paste(input$gV * 10 * input$i * input$nV)
    
    
    if (input$r != 0) {
      txt_rcl = (698 * input$gL / input$r) 
      txt_rcv = (698 * input$gV / input$r)
      txt_trcl = (698 * input$gL / input$r) * input$nL
      txt_trcv = (698 * input$gV / input$r) * input$nV
    } else{
      txt_rcl = 0
      txt_rcv = 0
      txt_trcl = 0
      txt_trcv = 0
    }
    
    
    Rolamento = c(txt_rrl, txt_rrv, txt_trrl, txt_trrv)
    Aero = c(txt_ral, txt_rav, txt_tral, txt_trav)
    Rampa = c(txt_rgl, txt_rgv, txt_trgl, txt_trgv)
    Curva = c(txt_rcl, txt_rcv, txt_trcl, txt_trcv)
    
    
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

    paste("Resistencia de todas locomotiva",
          formatC(
            (
              (input$lC1 + input$lC2 * input$xL / input$gL) * input$gL +
                  + rLc + (input$gL * 10 * input$i)
              )* input$nL
            ),
          " + ",
          (input$lC3 * input$nL* input$gL) , "*v",
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
