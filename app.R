library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(report)

ui <- fluidPage(
  titlePanel("Análisis T-test / ANOVA con Reporte"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Sube tu archivo (.csv o .xlsx)",
                accept = c(".csv", ".xlsx")),
      actionButton("analizar", "Analizar"),
      helpText("Cada columna representa un grupo. Se transforma a formato largo."),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resultados", verbatimTextOutput("resultado")),
        tabPanel("Gráfico de Cajas", plotOutput("grafico")),
        tabPanel("Campana de Gauss", plotOutput("campana")),
        tabPanel("Reporte (ANOVA)", verbatimTextOutput("anova_report"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Función reactiva para leer el archivo según extensión
  datos <- reactive({
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    
    df <- switch(ext,
                 csv = read.csv(input$datafile$datapath, sep=";", dec=".", stringsAsFactors = FALSE),
                 xlsx = read_excel(input$datafile$datapath),
                 stop("Formato no soportado"))
    
    # Eliminar columnas vacías
    df <- df[, colSums(is.na(df)) < nrow(df)]
    
    # Transformar a largo
    df_long <- pivot_longer(df, cols = everything(), names_to = "grupo", values_to = "valor") %>%
      mutate(valor = as.numeric(valor)) %>%
      filter(!is.na(valor)) %>%
      mutate(grupo = as.factor(grupo))
    
    return(df_long)
  })
  
  # Análisis principal
  analisis <- eventReactive(input$analizar, {
    df <- datos()
    n_groups <- n_distinct(df$grupo)
    
    if(n_groups == 2){
      t_res <- t.test(valor ~ grupo, data = df)
      list(tipo = "T-test", res = t_res, stat = as.numeric(t_res$statistic), df = df)
      
    } else if(n_groups >= 3){
      aov_res <- aov(valor ~ grupo, data = df)
      list(tipo = "ANOVA", res = summary(aov_res), model = aov_res, stat = NA, df = df)
      
    } else {
      list(tipo = "Error", res = "Se requieren al menos 2 grupos.", stat = NA)
    }
  })
  
  output$resultado <- renderPrint({
    req(analisis())
    cat("Prueba:", analisis()$tipo, "\n\n")
    print(analisis()$res)
  })
  
  output$grafico <- renderPlot({
    req(analisis())
    df <- analisis()$df
    ggplot(df, aes(x=grupo, y=valor)) +
      geom_boxplot(fill="lightblue") +
      theme_minimal() +
      labs(title="Gráfico de Cajas", x="Grupo", y="Valor")
  })
  
  output$campana <- renderPlot({
    res <- analisis()
    if(res$tipo != "T-test"){
      plot.new()
      title("Solo aplica a T-test")
      return()
    }
    t_val <- res$stat
    x <- seq(-4, 4, length.out = 400)
    y <- dnorm(x)
    alfa <- 0.05
    lim_inf <- qnorm(alfa/2)
    lim_sup <- qnorm(1 - alfa/2)
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(color = "blue", size = 1.2) +
      geom_area(data = subset(data.frame(x,y), x <= lim_inf),
                aes(x,y), fill = "red", alpha = 0.3) +
      geom_area(data = subset(data.frame(x,y), x >= lim_sup),
                aes(x,y), fill = "red", alpha = 0.3) +
      geom_vline(xintercept = t_val, color = "darkgreen", linetype = "dashed", size = 1) +
      geom_vline(xintercept = lim_inf, color = "red", linetype = "dashed") +
      geom_vline(xintercept = lim_sup, color = "red", linetype = "dashed") +
      labs(title = "Campana de Gauss", subtitle = paste("t =", round(t_val, 2)),
           x = "Estadístico t", y = "Densidad") +
      theme_minimal()
  })
  
  output$anova_report <- renderPrint({
    res <- analisis()
    if(res$tipo != "ANOVA") return("Este reporte solo se genera para ANOVA con ≥3 grupos.")
    
    # Creamos una variable extra para simular interacción como en el ejemplo de iris
    df <- res$df
    df$Cat1 <- rep(c("A", "B"), length.out = nrow(df))
    model <- aov(valor ~ grupo * Cat1, data = df)
    
    cat("Modelo con interacción: valor ~ grupo * Cat1\n\n")
    print(report(model))
    cat("\nResumen del modelo:\n")
    print(summary(model))
  })
}

shinyApp(ui, server)
