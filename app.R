source(file = 'www/R/main.R')
source(file = 'modulo_indicadores.R')


# mt2data_df<-readxl::read_xlsx(path = 'www/data/Data_Cassio.xlsx')
#
# mt2data_df<- mt2data_df %>% mutate(Date = as_date(Date))
#
# mt2data_df <- mt2data_df %>% mutate(months = month(Date)) %>%
#   mutate(Quarter = case_when(months %in% c(1,2,3) ~ '1º - Tri',
#                              months %in% c(4,5,6) ~ '2º - Tri',
#                              months %in% c(7,8,9) ~ '3º - Tri',
#                              months %in% c(10,11,12) ~'4º - Tri'))%>%
#   mutate(dias = 1:228)


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(

    sidebarMenu(
      menuItem("Informações", tabName = "informacao", icon = icon("dashboard")),
      menuItem("Modelagem", tabName = "modelagem", icon = icon("th")),
      menuItem("Relatório - Modelagem", tabName = "relatorio", icon = icon("th"))
    )

  ),
  dashboardBody(

    includeCSS(path = 'www/css/css_style.css'),
    tabItems(
      tabItem(
        tabName = 'informacao',
        fluidRow(id = 'chart_left_right',
          column(6,
                 box(width = NULL,height = NULL,
                     status = "primary",solidHeader = TRUE,
                     title = 'O Mercado de Caminhões - ÚLTIMOS 3 ANOS',
                     radioGroupButtons(
                       inputId = "periodos",
                       label = "",

                       choices = c("Mensal",
                                   "Trimestral"),
                       individual = TRUE,
                       checkIcon = list(
                         yes = tags$i(class = "fa fa-circle",
                                      style = "color: steelblue"),
                         no = tags$i(class = "fa fa-circle-o",
                                     style = "color: steelblue"))
                     ),

                     box(width = NULL,height = NULL,
                         status = "primary",solidHeader = TRUE,


                         highchartOutput('chart1'),

                         sliderInput("range",
                                     "",
                                     min = as.Date(as.yearmon("2003-01-01")),
                                     max = as.Date(as.yearmon("2021-12-01")),


                                     value= c(as.Date(as.yearmon("2003-01-01")),
                                              as.Date(as.yearmon("2021-12-13"))),
                                     timeFormat="%Y-%m-%d")


                     ))),

                 column(6,
                 box(width = NULL,

                     id = 'map_pie',
                     style = 'display: flex;',
                     collapsible = T,

                     title = 'Brasil - Mercado de Caminhões',
                     status = "primary",
                     solidHeader = TRUE,

                     highchartOutput('maps',width = '400px'),
                     highchartOutput('pie_chart'),

                     ),


                 )),
######################################################
#########Modulo Princiapis Indicadores ###############
######################################################


        indicadores_UI("indicadores"),

######################################################
#########Modulo Princiapis Indicadores ###############
######################################################

# Modelagem

        fluidRow(id = 'modelagem',
                 column(12,
                 box(width = NULL,
                     title = 'Modelagem',
                     status = "primary",solidHeader = TRUE,
                     tabsetPanel(
                       tabPanel("Ajuste do Modelo",
                                awesomeRadio(

                                  inputId = "models",
                                  label = "",
                                  choices = c("Modelo-1",
                                              "Modelo-2"),
                                  selected = "Modelo-1",
                                  inline = TRUE
                                ),

                                highchartOutput('modelo_1')%>% withSpinner()
                                ),

                       tabPanel("Previsão Com Indicadores Escolhidos",

                                awesomeRadio(

                                  inputId = "models_previsao",
                                  label = "",
                                  choices = c("Modelo-1",
                                              "Modelo-2"),
                                  selected = "Modelo-1",
                                  inline = TRUE
                                ),

                                highchartOutput('previsao')%>% withSpinner()
                                ),
                       tabPanel("Comentários Finais",

                    tags$div(id = "previsao_comment",
                       highchartOutput('previsao_coment', width = '550px')%>% withSpinner(),



                         HTML(
                           '<iframe width="460" height="315" id = "youtube"
                                 src="https://www.youtube.com/embed/Jx4GQQ0ROWo"
                                 title="YouTube video player"
                                 frameborder="0" allow="accelerometer; autoplay;
                                 clipboard-write; encrypted-media; gyroscope;
                                 picture-in-picture" allowfullscreen></iframe>'
                         )))
                     )))


                 ))

        )




  ))

server <- function(input, output) {

######################################################
#########Modulo Princiapis Indicadores ###############
######################################################

indicadores_Server("indicadores")

######################################################
#########Modulo Princiapis Indicadores ###############
######################################################

  periodos <- reactive({input$periodos}) %>% bindCache(input$periodos)



    output$chart1 <- renderHighchart({


      if(input$periodos == 'Mensal'){

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))


        three_year_data <- mt2data_df %>% filter(Date > input$range[1] & Date < input$range[2])


        three_year_data%>%

          hchart(type = 'column', hcaes(x = Date, y = Trucks))%>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_colors(c( "#014D64"))%>%
          hc_title(
            text = "<span style = 'color: black;'>Venda de Caminhões</span>",
            align = "center",
            style = list(fontSize = "24px", color = "black",
                         fontWeight = "bold", useHTML = TRUE)
          ) %>%
          hc_subtitle(
            text = "Ultimos 3 anos - Série Mensal",
            align = "center",color = '#D6E1E5',
            style = list(fontSize = "12px",  fontStyle = "italic")
          ) %>%

          hc_xAxis(

            plotBands = list(
              list(
                from = three_year_data$Date[three_year_data$Date == '2020-01-01'] %>% datetime_to_timestamp(),                        # Start of the plotband (first game # of injury)
                to = three_year_data$Date[three_year_data$Date == '2020-12-01'] %>% datetime_to_timestamp(),                            # End of the plotband (last game missed)
                color = "rgba(85, 37, 131, 0.2)", # RGB specification of the lakers purple color with a 30% alpha (transparency)
                label = list(
                  text = "Pandemia<br>2020",          # Text for the plotBand
                  style = list(fontWeight = "bold", color = "black",
                               fontSize = "16px")
                )
              ),

                list(
                  from = three_year_data$Date[three_year_data$Date == '2011-01-01'] %>% datetime_to_timestamp(),                        # Start of the plotband (first game # of injury)
                  to = three_year_data$Date[three_year_data$Date == '2015-12-01'] %>% datetime_to_timestamp(),                            # End of the plotband (last game missed)
                  color = "rgba(85, 37, 131, 0.2)", # RGB specification of the lakers purple color with a 30% alpha (transparency)
                  label = list(
                    text = "Governo Dilma<br>2011-2015",          # Text for the plotBand
                    style = list(fontWeight = "bold", color = "black",
                                 fontSize = "16px")
                  )
                )
            ))

  } else{

    three_year_data <- mt2data_df %>% filter(Date > input$range[1] & Date < input$range[2])

      tri_df <- three_year_data  %>%
        group_by(Quarter,year = year(Date)) %>%
        summarise(Trucks = sum(Trucks))


      tri_df <- tri_df %>% filter(year < year(input$range))


      tri_df%>%

        hchart(type = 'column', hcaes(x = year, y = Trucks,
                                      fill = Quarter, group = Quarter))%>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_title(
          text = "Venda de Caminhões",
          align = "center",
          style = list(fontSize = "24px",  fontWeight = "bold")
        ) %>%
        hc_subtitle(
          text = "Série Trimestral",
          align = "center",
          style = list(fontSize = "12px",  fontStyle = "bold")
        ) %>%

        hc_xAxis(
          # Injury Plot Area
          plotBands = list(
            list(
              from = three_year_data$Date[13] %>% datetime_to_timestamp(),                        # Start of the plotband (first game # of injury)
              to = three_year_data$Date[24] %>% datetime_to_timestamp(),                            # End of the plotband (last game missed)
              color = "rgba(85, 37, 131, 0.2)", # RGB specification of the lakers purple color with a 30% alpha (transparency)
              label = list(
                text = "Pandemia - 2020",          # Text for the plotBand
                style = list(fontWeight = "bold", color = "black",
                             fontSize = "16px")
              )
            )
          ))


  }

    })


  output$maps <- renderHighchart({

    hcmap("countries/br/br-all") %>%
      hc_title(text = "<span style = 'color: white;'>Brasil</span>") %>%
      hc_subtitle(text = 'Como cada Estado contribui para o Mercado de Caminhões')

  })

  output$pie_chart <- renderHighchart({
    df <- data.frame(
      x = c(0, 1, 2, 3, 4),
      y = c(10, 19.4, 21.1, 14.4, 6.4),
      name = as.factor(c("Minas Gerais", "Mato Grosso<br>do Sul", "Mato Grosso", "São<br>Paulo", "Goiás"))
    )

    df%>%
      hchart(
        "pie", hcaes(x = name, y = y),
        name = "% do Mercado de Cada Estado"
      )%>% hc_colors(c("#6794A7", "#014D64", "#76C0C2", "#00A2D9"))
  })


# Modelagem
##############################################################################
##############################################################################

    modelos_reactive <- reactive({input$models}) %>% bindCache(input$models)



 output$modelo_1 <- renderHighchart({

   if(modelos_reactive() == 'Modelo-1'){

     hchart(mt2data_df_final ,'spline',
            hcaes(x = Date, y = Trucks), color = 'darkblue')%>%
       hc_title(
         text = "Modelo - 1: ARIMA(1,1,2)(0,0,1)[12]",
         align = "center",
         style = list(fontSize = "24px", color = "black",
                      fontWeight = "bold", useHTML = TRUE)
       )%>%
       hc_add_theme(hc_theme_economist())%>%

       hc_add_series(mt2data_df_final,
                     "line", hcaes(x = Date,
                                   y = arima_model),color = 'red',
                     name = "Modelo Ajustado",
                     showInLegend = TRUE, dashStyle = "ShortDot")

   }else{

     hchart(mt2data_df_final ,'spline',
            hcaes(x = Date, y = Trucks), color = 'black')%>%
       hc_add_theme(hc_theme_economist()) %>%

       hc_title(
         text = "Modelo - 2: Holt-Winters",
         align = "center")%>%

       hc_add_series(mt2data_df_final,
                     "line", hcaes(x = Date,
                                   y = holt_model),color = 'brown',
                     name = "Modelo <id = 'ajustado' span style = 'border: 5px double black;'>Ajustado</span>",
                     showInLegend = TRUE, dashStyle = "ShortDot")
   }



 })

 modelos_previsao_reactive <- reactive({input$models_previsao}) %>% bindCache(input$models_previsao)

 output$previsao <-renderHighchart({

   if(modelos_previsao_reactive() == 'Modelo-1'){

     hchart(mt2data_df_final %>% filter(Date > '2020-01-01'),'spline',
            hcaes(x = Date, y = Trucks), color = 'darkblue')%>%
       hc_title(
         text = "Modelo - 1: ARIMA(1,1,2)(0,0,1)[12]",
         align = "center",
         style = list(fontSize = "24px", color = "black",
                      fontWeight = "bold", useHTML = TRUE)
       )%>%
       hc_add_theme(hc_theme_economist())%>%

       hc_add_series(mt2data_df_final %>% filter(Date > '2020-01-01'),
                     "line", hcaes(x = Date,
                                   y = arima_model),color = 'red',
                     name = "Modelo Ajustado",
                     showInLegend = TRUE, dashStyle = "ShortDot")%>%

       hc_add_series(mt2data_df_final %>% filter(Date > '2020-01-01'),
                     "line", hcaes(x = Date,
                                   y = forecast_autoarima),color = 'yellow',
                     name = "Truck Forecasting",
                     showInLegend = TRUE)

   }else{

     hchart(mt2data_df_final %>% filter(Date > '2020-01-01'),'spline',
            hcaes(x = Date, y = Trucks), color = 'black')%>%
       hc_add_theme(hc_theme_economist()) %>%

       hc_title(
         text = "Modelo - 2: Holt-Winters",
         align = "center")%>%

       hc_add_series(mt2data_df_final %>% filter(Date > '2020-01-01'),
                     "line", hcaes(x = Date,
                                   y = holt_model),color = 'brown',
                     name = "Truck Forecasting",
                     showInLegend = TRUE, dashStyle = "ShortDot") %>%

       hc_add_series(mt2data_df_final %>% filter(Date > '2020-01-01'),
                     "line", hcaes(x = Date,
                                   y = forecast_holt),color = 'yellow',
                     name = "Truck Forecasting",
                     showInLegend = TRUE)

   }

 })


 output$previsao_coment <- renderHighchart({

   hchart(mt2data_df_final %>% filter(Date > '2020-01-01'),'spline',
          hcaes(x = Date, y = Trucks), color = 'darkblue')%>%
     hc_title(
       text = "Modelo - 1: ARIMA(1,1,2)(0,0,1)[12]",
       align = "center",
       style = list(fontSize = "24px", color = "black",
                    fontWeight = "bold", useHTML = TRUE)
     )%>%
     hc_add_theme(hc_theme_economist())%>%

     hc_add_series(mt2data_df_final %>% filter(Date > '2020-01-01'),
                   "line", hcaes(x = Date,
                                 y = arima_model),color = 'red',
                   name = "Modelo Ajustado",
                   showInLegend = TRUE, dashStyle = "ShortDot")%>%

     hc_add_series(mt2data_df_final %>% filter(Date > '2020-01-01'),
                   "line", hcaes(x = Date,
                                 y = forecast_autoarima),color = 'yellow',
                   name = "Truck Forecasting",
                   showInLegend = TRUE)

 })





}

shinyApp(ui, server)
