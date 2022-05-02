indicadores_UI <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(id = 'principais_indicadores',
        column(12,
               box(width = NULL,
                   height = NULL,
                   title = 'Principais Indicadores',
                   collapsible = T,

                   status = "primary",solidHeader = TRUE,
                   glide(id = 'carrossel',
                     height = "200px",


                   screen(
                     box(width = 8,
                       highchartOutput(ns('chart2'))),

                     box(width = 4,

                       infoBoxOutput(ns("progressBox1"),width = "30%"),
                       infoBoxOutput(ns("progressBox2"),width = "30%"),
                       infoBoxOutput(ns("progressBox3"),width = "30%"),
                       infoBoxOutput(ns("progressBox4"),width = "30%")


                     )),

                   screen(
                     box(width = 8,
                       highchartOutput(ns('chart3')) ),

                     box(width = 4,
                         infoBoxOutput(ns("progressBox5"),width = "30%"),
                         infoBoxOutput(ns("progressBox6"),width = "30%"),
                         infoBoxOutput(ns("progressBox7"),width = "30%"),
                         infoBoxOutput(ns("progressBox8"),width = "30%")


                     )),


                   screen(
                     box(width = 8,
                       highchartOutput(ns('chart4')),

                     box(width = 4,
                       infoBoxOutput(ns("progressBox9"),width = "30%"),
                       infoBoxOutput(ns("progressBox10"),width = "30%"),
                       infoBoxOutput(ns("progressBox11"),width = "30%"),
                       infoBoxOutput(ns("progressBox12"),width = "30%")
                     )),

                   screen(
                     box(width = 8,
                       highchartOutput(ns('chart5'))),

                     box(width = 4,
                       infoBoxOutput(ns("progressBox13"),width = "30%"),
                       infoBoxOutput(ns("progressBox14"),width = "30%"),
                       infoBoxOutput(ns("progressBox15"),width = "30%"),
                       infoBoxOutput(ns("progressBox16"),width = "30%")

                     ))

               )))
        )



  ))
}

indicadores_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      output$chart2 <- renderHighchart({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))

        three_year_data  %>%  select(Date, 'Industrial Production')%>%

          hchart("areaspline", hcaes(x = Date, y = `Industrial Production`),
                 color = 'lightblue',name = "Industrial Production", showInLegend = TRUE) %>%
          hc_add_series(three_year_data,
                        "line", hcaes(x = Date, y = `Retail Sales`),
                        name = "Retail Sales", showInLegend = TRUE) %>%



          hc_title(text = "<span style = 'color : black;'>Industrial Produtcion  x Retail Sales<span>",
                   style = list(fontWeight = "bold", color = "black",
                                fontSize = "16px", useHTML = TRUE)) %>%
          hc_subtitle(text = "<span style = 'color: #D6E1E5; '>Ultimos 3 anos</span>") %>%
          hc_add_theme(hc_theme_economist())

      })

      output$chart3 <- renderHighchart({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))


        three_year_data  %>%  select(Date, 'Business Confidence Index')%>%

          hchart("areaspline", hcaes(x = Date, y = `Business Confidence Index`),
                 color = 'lightblue',name = "Business Confidence Index", showInLegend = TRUE) %>%
          hc_add_series(three_year_data,
                        "line", hcaes(x = Date, y = `Commodity Price Index`),
                        name = "Commodity Price Index", showInLegend = TRUE) %>%


          hc_title(text = "<span style = 'color : black;'>Business Confidence Index  x Commodity Price Index<span>",
                   style = list(fontWeight = "bold", color = "black",
                                fontSize = "16px", useHTML = TRUE)) %>%
          hc_subtitle(text = "<span style = 'color: blue; '>Ultimos 3 anos</span>") %>%
          hc_add_theme(hc_theme_economist())

      })

      output$chart4 <- renderHighchart({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))

        three_year_data  %>%  select(Date, 'USD/BRL')%>%

          hchart("areaspline", hcaes(x = Date, y = `USD/BRL`),
                 color = 'lightblue',name = "Taxa de Câmbio", showInLegend = TRUE) %>%
          hc_add_series(three_year_data,
                        "line", hcaes(x = Date, y = `Base Interest Rate`),
                        name = "Base Interest Rate", showInLegend = TRUE) %>%



          hc_title(text = "<span style = 'color : black;'>USD/BRL  x Base Interest Rate<span>",
                   style = list(fontWeight = "bold", color = "black",
                                fontSize = "16px", useHTML = TRUE)) %>%
          hc_subtitle(text = "<span style = 'color: blue; '>Ultimos 3 anos</span>") %>%
          hc_add_theme(hc_theme_economist())

      })


      output$chart5 <- renderHighchart({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))


        three_year_data  %>%  select(Date, 'Index of Employed Persons - Industry')%>%

          hchart("areaspline", hcaes(x = Date, y = `Index of Employed Persons - Industry`),
                 color = 'lightblue',name = "Index of Employed Persons - Industry", showInLegend = TRUE) %>%
          hc_add_series(three_year_data,
                        "line", hcaes(x = Date, y = `Uncertainty Index`),
                        name = "Uncertainty Index", showInLegend = TRUE) %>%

          hc_title(text = "<span style = 'color : black;'>Index of Employed Persons - Industry  x Uncertainty Index<span>",
                   style = list(fontWeight = "bold", color = "black",
                                fontSize = "16px", useHTML = TRUE)) %>%
          hc_subtitle(text = "Ultimos 3 anos") %>%
          hc_add_theme(hc_theme_economist())
      })

      output$progressBox1 <- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))

        infoBox(
          glue::glue("{names(three_year_data)[4]}"),
          glue::glue("Em Média {round(mean(three_year_data$`Industrial Production`,na.rm=TRUE))}mil de Prod. Industrial"),
          glue::glue("O Máximo foi de {max(three_year_data$`Industrial Production`,na.rm=TRUE)} Milhões"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })


      output$progressBox2 <- renderInfoBox({
        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[2]}"),
          glue::glue("80%"),
          icon = icon("thumbs-up", lib = "glyphicon"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox3 <- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[2]}"),
          glue::glue("80%"),
          icon = icon("thumbs-up", lib = "glyphicon"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox4 <- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[2]}"),
          glue::glue("80%"),
          icon = icon("thumbs-up", lib = "glyphicon"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox5 <- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[3]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox6 <- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[4]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox7 <- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[5]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox8<- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[6]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })
      output$progressBox9<- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[6]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })
      output$progressBox10<- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[6]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox11<- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[6]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox12<- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[6]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox13<- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[6]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox14<- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[6]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox15<- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[6]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

      output$progressBox16<- renderInfoBox({

        three_year_data <- mt2data_df %>%
          filter(year(Date) %in% c(2019,2020,2021))



        infoBox(
          glue::glue("Média Variáve {names(three_year_data)[6]}"),
          glue::glue("80%"),
          icon = icon("list"),
          color = "light-blue", fill = TRUE
        )
      })

    }
  )
}
