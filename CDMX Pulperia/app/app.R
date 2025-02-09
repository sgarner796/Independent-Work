library(shiny)
library(tidyverse)
library(gt)
library(openxlsx)

# Load data
abrrts <- read.xlsx("abrrts_analisis.xlsx")

# Prepare summary data
summary_data <- abrrts %>%
  group_by(Alcaldía) %>%
  summarise(
    avg_telefono = mean(Telefono_binary, na.rm = TRUE),
    avg_correo = mean(Correo_binary, na.rm = TRUE),
    avg_website = mean(Pagina_binary, na.rm = TRUE),
    avg_count = mean(Count_non_missing_percentage, na.rm = TRUE) / 100,
    PEA = as.numeric(first(PEA)) / 100,
    Ingresos = as.numeric(first(ingresos))
  ) %>%
  arrange(desc(PEA))

# Create details data
details_data <- abrrts %>%
  select(Alcaldía, nom_estab, Telefono) %>%
  filter(!is.na(Telefono))

# Define UI
ui <- fluidPage(
  titlePanel("Análisis Abarrotes CDMX"),
  sidebarLayout(
    sidebarPanel(
      h4("Haz clic en una Alcaldía para desplegar la lista de negocios")
    ),
    mainPanel(
      h3("Resumen de Accesibilidad Digital por Alcaldía"),
      gt_output("summary_table"),
      uiOutput("details_ui")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  output$summary_table <- render_gt({
    summary_data %>%
      gt() %>%
      tab_header(
        title = "Resumen de Accesibilidad Digital por Alcaldía",
        subtitle = "Promedios de disponibilidad de contacto y variables socioeconómicas"
      ) %>%
      cols_label(
        Alcaldía = "Alcaldía",
        avg_telefono = "Teléfono (%)",
        avg_correo = "Correo (%)",
        avg_website = "Página Web (%)",
        avg_count = "Promedio de Comunicación (%)",
        PEA = "PEA (%)",
        Ingresos = "Ingresos Promedio"
      ) %>%
      fmt_percent(
        columns = c(avg_telefono, avg_correo, avg_website, avg_count, PEA),
        decimals = 2
      ) %>%
      fmt_number(
        columns = c(Ingresos),
        decimals = 2,
        use_seps = TRUE
      ) %>%
      text_transform(
        locations = cells_body(columns = Alcaldía),
        fn = function(x) {
          paste0("<a href='#' onclick=\"Shiny.setInputValue('selected_alcaldia', '", x, "', {priority: 'event'})\">", x, "</a>")
        }
      )
  })
  
  output$details_ui <- renderUI({
    req(input$selected_alcaldia)
    tagList(
      h3(paste("Detalles para", input$selected_alcaldia)),
      gt_output("details_table")
    )
  })
  
  output$details_table <- render_gt({
    req(input$selected_alcaldia)
    details_data %>%
      filter(Alcaldía == input$selected_alcaldia) %>%
      gt() %>%
      tab_header(
        title = paste("Detalles para", input$selected_alcaldia),
        subtitle = "Negocios y teléfonos disponibles"
      ) %>%
      cols_label(
        Alcaldía = "Alcaldía",
        nom_estab = "Negocio",
        Telefono = "Teléfono"
      )
  })
}

# Run App
shinyApp(ui = ui, server = server)

