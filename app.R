library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

ui <- fluidPage(
  titlePanel("T-test / ANOVA"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Sube tu archivo (.csv)", accept=".csv"),
      actionButton("analizar", "Analizar"),
      helpText("Cada columna es un grupo. T-test si 2 grupos; ANOVA si ≥3.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resultados", verbatimTextOutput("resultado")),
        tabPanel("Cajas", plotOutput("grafico")),
        tabPanel("Gauss", plotOutput("campana"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Lee y transforma el archivo CSV (separador ";" y decimales con ".")
  datos <- reactive({
    req(input$datafile)
    df <- read.csv(input$datafile$datapath, sep=";", dec=".", stringsAsFactors = FALSE)
    # Eliminar columnas vacías
    df <- df[, colSums(is.na(df)) < nrow(df)]
    # Transformar de ancho a largo, convertir a numérico y factor
    pivot_longer(df, cols=everything(), names_to="grupo", values_to="valor") %>%
      mutate(valor = as.numeric(valor)) %>%
      filter(!is.na(valor)) %>%
      mutate(grupo = as.factor(grupo))
  })
  
  # Realiza el análisis estadístico: T-test si 2 grupos; ANOVA si ≥3.
  analisis <- eventReactive(input$analizar, {
    df <- datos()
    n_groups <- n_distinct(df$grupo)
    if(n_groups == 2){
      t_res <- t.test(valor ~ grupo, data=df)
      list(tipo="T-test", res=t_res, stat=as.numeric(t_res$statistic))
    } else if(n_groups >= 3){
      aov_res <- aov(valor ~ grupo, data=df)
      list(tipo="ANOVA", res=summary(aov_res), stat=NA)
    } else list(tipo="Error", res="Se requieren al menos 2 grupos.", stat=NA)
  })
  
  output$resultado <- renderPrint({
    req(analisis())
    res <- analisis()
    cat("Prueba:", res$tipo, "\n\n")
    print(res$res)
  })
  
  output$grafico <- renderPlot({
    ggplot(datos(), aes(x=grupo, y=valor)) +
      geom_boxplot(fill="skyblue") +
      theme_minimal() +
      labs(title="Gráfico de Cajas", x="Grupo", y="Valor")
  })
  
  # Panel de la Campana de Gauss (sólo para T-test)
  output$campana <- renderPlot({
    res <- analisis()
    if(res$tipo != "T-test"){
      plot.new()
      title("Campana de Gauss solo para T-test")
      return()
    }
    t_val <- res$stat
    x <- seq(-4, 4, length.out=400)
    y <- dnorm(x)
    alfa <- 0.05
    lim_inf <- qnorm(alfa/2)
    lim_sup <- qnorm(1-alfa/2)
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(color="blue", size=1.2) +
      geom_area(data = subset(data.frame(x,y), x <= lim_inf),
                aes(x,y), fill="red", alpha=0.3) +
      geom_area(data = subset(data.frame(x,y), x >= lim_sup),
                aes(x,y), fill="red", alpha=0.3) +
      geom_vline(xintercept=t_val, color="darkgreen", linetype="dashed", size=1) +
      geom_vline(xintercept=lim_inf, color="red", linetype="dashed", size=1) +
      geom_vline(xintercept=lim_sup, color="red", linetype="dashed", size=1) +
      labs(title="Curva Normal Estándar",
           subtitle=paste("t =", round(t_val,2), "| α =", alfa),
           x="Estadístico t", y="Densidad") +
      theme_minimal()
  })
}

shinyApp(ui, server)
