# Cargar librerías necesarias
library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(RVAideMemoire)
library(nortest)
library(tseries)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Pruebas estadísticas: Selección manual"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "test",
        label   = "Seleccione la prueba:",
        choices = c(
          "Chi-cuadrado"               = "chi2",
          "McNemar"                    = "mcnemar",
          "Cochran's Q"                = "cochran",
          "T de Student"               = "t",
          "ANOVA"                      = "anova",
          "Wilcoxon"                   = "wilcox",
          "Correlación Pearson"        = "pearson",
          "Correlación Spearman"       = "spearman",
          "Cociente de varianzas (F)"  = "varratio",
          "Shapiro-Wilk"               = "shapiro",
          "Kolmogorov-Smirnov"         = "ks",
          "Lilliefors"                 = "lillie",
          "Jarque-Bera"                = "jb"
        )
      ),
      fileInput("file", "Subir archivo (.csv o .xlsx)", accept = c(".csv", ".xlsx")),
      uiOutput("varselect_ui"),
      verbatimTextOutput("warning")
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("result")
    )
  )
)

# Lógica del servidor
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv  = read_csv(input$file$datapath),
           xlsx = read_excel(input$file$datapath),
           stop("Formato no soportado: use .csv o .xlsx")
    )
  })
  
  vars <- reactive({
    df <- data()
    list(
      cat = names(df)[sapply(df, function(x) is.factor(x) || is.character(x))],
      num = names(df)[sapply(df, is.numeric)]
    )
  })
  
  output$varselect_ui <- renderUI({
    v <- vars()
    req(input$test, input$file)
    output$warning <- renderText("")
    switch(input$test,
           chi2    = tagList(
             selectInput("var1", "Categoría 1:", choices = v$cat),
             selectInput("var2", "Categoría 2:", choices = v$cat)
           ),
           mcnemar = tagList(
             selectInput("var1", "Variable binaria 1:", choices = v$cat),
             selectInput("var2", "Variable binaria 2:", choices = v$cat)
           ),
           cochran = checkboxGroupInput("items", "Seleccione columnas (binarias):", choices = v$cat),
           t = tagList(
             selectInput("group", "Grupo (2 niveles):", choices = v$cat),
             selectInput("value", "Respuesta numérica:", choices = v$num)
           ),
           anova = tagList(
             selectInput("group", "Grupo (>=3 niveles):", choices = v$cat),
             selectInput("value", "Respuesta numérica:", choices = v$num)
           ),
           wilcox = tagList(
             selectInput("group", "Grupo (2 niveles):", choices = v$cat),
             selectInput("value", "Respuesta numérica:", choices = v$num)
           ),
           pearson = tagList(
             selectInput("xvar", "Variable X:", choices = v$num),
             selectInput("yvar", "Variable Y:", choices = v$num)
           ),
           spearman = tagList(
             selectInput("xvar", "Variable X:", choices = v$num),
             selectInput("yvar", "Variable Y:", choices = v$num)
           ),
           varratio = tagList(
             selectInput("xvar", "Variable 1 (numérica):", choices = v$num),
             selectInput("yvar", "Variable 2 (numérica):", choices = v$num),
             selectInput("alternative", "Hipótesis alternativa:",
                         choices = c("Dos lados" = "two.sided", "Mayor" = "greater", "Menor" = "less")),
             numericInput("alpha", "Nivel de significancia (α):", value = 0.05, min = 0.001, max = 0.2, step = 0.005)
           ),
           shapiro = selectInput("numvar", "Variable numérica:", choices = v$num),
           ks      = selectInput("numvar", "Variable numérica:", choices = v$num),
           lillie  = selectInput("numvar", "Variable numérica:", choices = v$num),
           jb      = selectInput("numvar", "Variable numérica:", choices = v$num)
    )
  })
  
  test_res <- reactive({
    req(input$test, input$file)
    df <- data()
    switch(input$test,
           shapiro = {
             x <- df[[input$numvar]]
             res <- shapiro.test(x)
             list(type = "Shapiro-Wilk", res = res)
           },
           ks = {
             x <- df[[input$numvar]]
             res <- ks.test(x, "pnorm", mean = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE))
             list(type = "Kolmogorov-Smirnov", res = res)
           },
           lillie = {
             x <- df[[input$numvar]]
             res <- lillie.test(x)
             list(type = "Lilliefors", res = res)
           },
           jb = {
             x <- df[[input$numvar]]
             res <- jarque.bera.test(x)
             list(type = "Jarque-Bera", res = res)
           },
           varratio = {
             x <- df[[input$xvar]]
             y <- df[[input$yvar]]
             res <- var.test(x, y, alternative = input$alternative, conf.level = 1 - input$alpha)
             list(type = "Cociente de varianzas (F)", res = res,
                  df1 = res$parameter[1], df2 = res$parameter[2],
                  statistic = res$statistic, p.value = res$p.value,
                  conf.int = res$conf.int)
           },
           stop("Prueba no implementada aún.")
    )
  })
  
  output$plot <- renderPlot({
    tr <- test_res()
    df <- data()
    if (tr$type %in% c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Lilliefors", "Jarque-Bera")) {
      x <- df[[input$numvar]]
      qqnorm(x, main = paste(tr$type, "- Q-Q Plot"))
      qqline(x)
      return()
    }
    if (tr$type == "Cociente de varianzas (F)") {
      x <- seq(0, qf(0.999, tr$df1, tr$df2), length = 1000)
      dens <- df(x, tr$df1, tr$df2)
      plot(x, dens, type = "l", ylab = "Densidad", xlab = "F", main = tr$type)
      alpha <- input$alpha
      alt <- input$alternative
      if (alt == "two.sided") {
        low <- qf(alpha / 2, tr$df1, tr$df2)
        high <- qf(1 - alpha / 2, tr$df1, tr$df2)
        polygon(x[x <= low], dens[x <= low], col = "#FFCCCC", border = NA)
        polygon(x[x >= high], dens[x >= high], col = "#FFCCCC", border = NA)
        abline(v = low, lty = 2)
        abline(v = high, lty = 2)
      } else if (alt == "greater") {
        crit <- qf(1 - alpha, tr$df1, tr$df2)
        polygon(x[x >= crit], dens[x >= crit], col = "#FFCCCC", border = NA)
        abline(v = crit, lty = 2)
      } else {
        crit <- qf(alpha, tr$df1, tr$df2)
        polygon(x[x <= crit], dens[x <= crit], col = "#FFCCCC", border = NA)
        abline(v = crit, lty = 2)
      }
      abline(v = tr$statistic, col = "red", lwd = 2)
      legend("topright", legend = c("Estadístico F", "Región de rechazo"),
             col = c("red", "#FFCCCC"), lwd = c(2, NA), pch = c(NA, 15), pt.cex = 2)
    }
  })
  
  output$result <- renderPrint({
    tr <- test_res()
    cat("Prueba aplicada:", tr$type, "\n")
    if (tr$type %in% c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Lilliefors", "Jarque-Bera")) {
      print(tr$res)
    } else if (tr$type == "Cociente de varianzas (F)") {
      cat("Estadístico F:", round(tr$statistic, 4), "\n")
      cat("Grados de libertad: df1 =", tr$df1, ", df2 =", tr$df2, "\n")
      cat("p-valor:", round(tr$p.value, 4), "\n")
      cat("Intervalo de confianza (", 100 * (1 - input$alpha), "%):", 
          round(tr$conf.int[1], 4), "-", round(tr$conf.int[2], 4), "\n")
    } else {
      print(tr$res)
    }
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
