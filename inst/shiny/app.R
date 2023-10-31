library(shiny)
library(DT)
library(tidyverse)
library(bslib)
library(markdown)


ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  # theme = bslib::bs_theme(
  #   bg = "#0b3d91",
  #   fg = "white",
  #   primary = "#FCC780",
  #   base_font = font_google("Lato"),
  #   code_font = font_google("Lato")
  # ),
  "WEPPR",
  tabPanel("Soil and Slope",
           sidebarLayout(
             sidebarPanel(
               fileInput(
                 inputId = "data",
                 label = "Upload WEPP files",
                 buttonLabel = "Browse...",
                 multiple = TRUE,
                 accept = c(".slp", ".sol"),
               )
             ),
             mainPanel(
               h1("Slope data"),
               DT::dataTableOutput('slp'),
               h2("Slope plot"),
               plotOutput("slp_plt"),
               h1("Soil data"),
               DT::dataTableOutput('sol'),
               h2("Soil plot"),
               shinycssloaders::withSpinner(plotOutput("sol_plt")),
             ),
           )),
  tabPanel("Predictions", "precipitation today: "),
  tabPanel("About", includeMarkdown("about.md")),
)

server <- function(input, output, session) {
  fpaths <- reactive(input$data$datapath)

  dfs <- reactive({
    req(fpaths())
    for (fpath in fpaths()) {
      ftype <- tools::file_ext(fpath)
      if (ftype == "slp") {
        slp <- read_slp(fpath)
      }
      if (ftype == "sol") {
        sol <- read_sol(fpath)
      }
    }
    dfs <- list(slp, sol)

    return(dfs)
  })

  output$slp <-
    DT::renderDataTable(dfs()[[1]],
                        options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                                       searching = FALSE))

  output$sol <-
    DT::renderDataTable(dfs()[[2]],
                        options = list(lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                                       searching = FALSE))


  output$slp_plt <- renderPlot({
    plot(dfs()[[1]])
  })

  output$sol_plt <- renderPlot({
    slp <- dfs()[[1]]
    sol <- dfs()[[2]]

    slp_sol <- merge_slp_sol(slp, sol)

    plot(slp_sol)
  })
}

shinyApp(ui, server)
