library(shiny)
library(shinydashboard)
library(dplyr)
library(scales)
library(htmltools)
#library(DT)

## Header:
hd <- dashboardHeader(titleWidth = 1450, title = "2a - Verteilung und Dichte von Zufallsvariablen: Beispiele")

## Sidebar:
sb <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem("Deskriptive Statistiken", tabName = "descriptive", icon = icon("table")),
        menuItem("Ausfallwahrscheinlichkeiten", tabName = "survival", icon = icon("line-chart"))
    ),
    conditionalPanel(condition = "input.tabs == 'descriptive'",
                     uiOutput("descriptive_inputs")
                     ),
    conditionalPanel(condition = "input.tabs == 'survival'",
                     numericInput("mean2", "Mittelwert Lebensdauer", 5.5, step = 0.5),
                     numericInput("std3", "Standardabweichung Lebensdauer", 0.3, min = 0, step = 0.1),
                     numericInput("nsamples2", "Anzahl Kunden", value = 10, step = 5),
                     numericInput("m1", "Mittelwert Kundengruppe 1", value = 2.5, step = 0.1),
                     numericInput("std1", "Standardabweichung Kundengruppe 1", value = 0.9,
                                  step = 0.1),
                     numericInput("m2", "Mittelwert Kundengruppe 2", value = 3, step = 0.1),
                     numericInput("std2", "Standardabweichung Kundengruppe 2", value = 0.6,
                                  step = 0.1),
                     sliderInput("pm", "Anteil der Kundengruppe 1 an Population", value = 0.75, min = 0, max = 1, step = 0.05)
                     )
)


## Box for table output:
descriptive.output <- fluidPage(
    width = NULL,
    tabsetPanel(
        id = "descriptive_tab_selection",
        tabPanel("Simulation", value = "sim",
                 box(title = "Beispielcode", width = NULL, height = 100,
                     div(textOutput("code_table"), style = "font-family: 'Courier New'")
                     ),
                 box(title = "Simulierte Messwerte (X1)", width = NULL, align = "center",
                     tableOutput("table"),
                     #4 Make the final row bold using tags$style
                     tags$style(
                         type="text/css", "#table tr:last-child {font-weight:bold;}",
                         "#table td:first-child {font-style:italic;}",
                         #"#table tr:nth-child(2) td:nth-child(1) {color:red;}",
                         "#table {font-size:16px}"
                     )
                     )
                 ),
        tabPanel("Plots", value = "plots", align = "center",
                 h2(textOutput("stat_plot_name")),
                 plotOutput("stat_plot")
                 ),
        tabPanel("Empirische Verteilung", value = "ecdf", align = "center",
                 h2("Empirische Verteilung für simulierte Messungen"),
                 plotOutput("plot_ecdf")
                 ),
        tabPanel("Boxplot", value = "box", align = "center",
                 h2("Boxplot für simulierte Messungen"),
                 plotOutput("boxplot")
                 ),
        tabPanel("Überlebenswahrscheinlichkeit", value = "survival", align = "center",
                 h2("Überlebenswahrscheinlichkeiten für simulierte Versuchsreihe"),
                 box(title = "Schätzung der Überlebenswahrscheinlichkeit", width = 6, align = "center",
                     plotOutput("survival_est")
                     ),
                 box(title = "Wahre Verteilung der Überlebenswahrscheinlichkeit", width = 6, align = "center",
                     plotOutput("survival_true")
                     )
                 )
    )
)

survival.output <- fluidPage(width = NULL, align = "center",
    tabsetPanel(
        id = "survival_tab_selection",
        tabPanel("Verteilungen", align = "center",
                 plotOutput("customer_distributions")
                 ),
        tabPanel("Wahre Ausfallwahrscheinlichkeit",
                 box("Beispielcode", width = NULL, height = 100,
                     div(textOutput("survival_code"), style = "font-family: 'Courier New'")),
                 box("Dichten", width = NULL, align = "center",
                     plotOutput("customer_densities"))
                 )
    )
)



## Tab items:
tab.1 <- tabItem(
    tabName = "descriptive",
    h2("Deskriptive Statistiken"),
    fluidPage(descriptive.output)
)

tab.2 <- tabItem(
    tabName = "survival",
    h2("Ausfallwahrscheinlichkeiten"),
    fluidPage(survival.output)
)

## Putting the body together:
body <- dashboardBody(
    ## Also add some custom CSS to make the title background area the same
    ## color as the rest of the header.
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #179C7D;
                              }
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #179C7D;
                              }
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #179C7D;
                              }
                                      '))),
    tabItems(tab.1, tab.2)
)

## Defining the UI:
ui <- dashboardPage(hd, sb, body)



## Define the server function:
server <- function(input, output, session) {

    descriptive_tab_selection <- reactive({
        value <- input$descriptive_tab_selection
        if(is.null(value))
            value <- NULL
        return (value)
    })

    output$descriptive_inputs <- renderUI({

        which_tab <- descriptive_tab_selection()

        if(is.null(which_tab))
            return(NULL)

        if (which_tab == "sim" || which_tab == "plots") {
            fluidPage(
            numericInput("mean", "Mittelwert wahre Verteilung", 5.5, step = 0.5),
            numericInput("std", "Standardabweichung wahre Verteilung", 0.3, min = 0, step = 0.1),
            numericInput("nsamples", "Anzahl an Messwerten pro Versuch", 10, min = 0, step = 50),
            selectInput("stat", "Interessierende Statistik", c("Mittelwert", "Standardabweichung", "5%-Quantil", "10%-Quantil"), selected = "Mittelwert"),
            actionButton("simulate_button", "Simuliere Daten")
            )
        } else {
            fluidPage(
            numericInput("mean", "Mittelwert wahre Verteilung", 5.5, step = 0.5),
            numericInput("std", "Standardabweichung wahre Verteilung", 0.3, min = 0, step = 0.1),
            numericInput("nsamples", "Anzahl an Messwerten pro Versuch", 10, min = 0, step = 50),
            actionButton("simulate_button", "Simuliere Daten")
            )
        }
    })

    survival_tab_selection <- reactive({
        value <- input$survival_tab_selection
        if(is.null(value))
            value <- NULL
        return (value)
    })

    output$survival_inputs <- renderUI({

        which_tab <- survival_tab_selection()

        if(is.null(which_tab))
            return(NULL)

        if (which_tab == "count_plot" || which_tab == "plots") {
            fluidPage(
            numericInput("mean", "Mittelwert wahre Verteilung", 5.5, step = 0.5),
            numericInput("std", "Standardabweichung wahre Verteilung", 0.3, min = 0, step = 0.1),
            numericInput("nsamples", "Anzahl an Messwerten pro Versuch", 10, min = 0, step = 50),
            selectInput("stat", "Interessierende Statistik", c("Mittelwert", "Standardabweichung", "5%-Quantil", "10%-Quantil"), selected = "Mittelwert")
            )
        } else {
            fluidPage(
            numericInput("mean", "Mittelwert wahre Verteilung", 5.5, step = 0.5),
            numericInput("std", "Standardabweichung wahre Verteilung", 0.3, min = 0, step = 0.1),
            numericInput("nsamples", "Anzahl an Messwerten pro Versuch", 10, min = 0, step = 50)
            )
        }
})

    data <- reactive({
        input$simulate_button
        req(input$nsamples, input$std > 0, input$nsamples > 0)
        df <- rnorm(input$nsamples * 10, input$mean, input$std) %>%
                matrix(ncol = 10) %>%
                as.data.frame
        names(df) <- c("Versuch 1", "Versuch 2", "Versuch 3", "Versuch 4", "Versuch 5",
                       "Versuch 6", "Versuch 7", "Versuch 8", "Versuch 9", "Versuch 10")
        row.names(df) <- lapply(1:input$nsamples, function(x) paste0("Messung ", x)) %>%
            unlist
        df
    })

    output$stat_plot_name <- renderText({
        if (input$stat == "Mittelwert") {
            "Visualisierung der Mittelwerte"
        } else if (input$stat == "Standardabweichung") {
           "Visualisierung der Standardabweichungen"
        } else if (input$stat == "5%-Quantil") {
            "Visualisierung der 5%-Quantile"
        } else if (input$stat == "10%-Quantil") {
            "Visualisierung der 10%-Quantile"
        }
    })

    output$code_table <- renderText({
        paste0("X1 <- normrnd(", input$mean, ", ", input$std, ", ", input$nsamples, ", ", "10)")
    })

    statistik <- reactive({
        req(input$nsamples)
        if (input$stat == "Mittelwert") {
            s <- apply(data(), 2, mean)
        } else if (input$stat == "Standardabweichung") {
            s <- apply(data(), 2, var) %>% sqrt
        } else if (input$stat == "5%-Quantil") {
            s <- apply(data(), 2, quantile, probs = 0.05, type = 1)
        } else if (input$stat == "10%-Quantil") {
            s <- apply(data(), 2, quantile, probs = 0.10, type = 1)
        }
        df <- t(data.frame(s))
        row.names(df) <- input$stat
        df
    })

    output$table <- renderTable({
        req(input$nsamples)
        rbind(data(), statistik())},
        include.rownames = TRUE
        )

    output$stat_plot <- renderPlot({

        if (input$stat == "Mittelwert"){
            col.means <- apply(data(), 2, mean)
            par(cex = 1.2, lwd = 2)
            plot(NA, xlim = c(0, 11), ylim = c(input$mean - 0.5, input$mean + 0.5), xlab = paste0("Versuch (mit je ", input$nsamples, " Messungen)"), ylab = "geschätzter Mittelwert")
            grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
            points(col.means, pch = 19, col = "blue")
            abline(h = input$mean, lty = 5, col = "red")
            legend("topright", legend = c(paste0("Mittelwerte aus je ", input$nsamples, " Messwerten"), "Wahrer Mittelwert"), lty = c(0, 5), col = c("blue", "red"), pch = c(19, NA))

        } else if (input$stat == "Standardabweichung"){

            col.std <- apply(data(), 2, var) %>%
                sqrt
            par(cex = 1.2, lwd = 2)
            plot(NA, xlim = c(0, 11), ylim = c(0, input$std + 0.5), xlab = paste0("Versuch (mit je ", input$nsamples, " Messungen)"), ylab = "geschätzte Standardabweichung")
            grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
            points(col.std, pch = 19, col = "blue")
            abline(h = input$std, lty = 5, col = "red")
            legend("topright", legend = c(paste0("Standardabweichungen aus je ", input$nsamples, " Messwerten"), "Wahre Standardabweichung"), lty = c(0, 5), col = c("blue", "red"), pch = c(19, NA))

        } else if (input$stat == "5%-Quantil") {
            col.q <- apply(data(), 2, quantile, probs = c(0.05), type = 1)
            q.true <- qnorm(0.05, input$mean, input$std)
            par(cex = 1.2, lwd = 2)
            plot(NA, xlim = c(0, 11), ylim = c(q.true - 1, q.true + 1), xlab = paste0("Versuch (je ", input$nsamples, " Messungen)"), ylab = "geschätzes 5%-Quantile")
            grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
            points(col.q, pch = 19, col = "blue")
            abline(h = q.true, lty = 5, col = "red")
            legend("topright", legend = c(paste0("5%-Quantile aus je ", input$nsamples, " Messwerten"), "Wahres 5%-Quantile"), lty = c(0, 5), col = c("blue", "red"), pch = c(19, NA))

        } else if (input$stat == "10%-Quantil") {
            col.q <- apply(data(), 2, quantile, probs = c(0.1), type = 1)
            q.true <- qnorm(0.1, input$mean, input$std)
            q.5 <- qnorm(0.05, input$mean, input$std)
            par(cex = 1.2, lwd = 2)
            plot(NA, xlim = c(0, 11), ylim = c(q.5 - 1, q.5 + 1), xlab = paste0("Versuch (mit je ", input$nsamples, " Messungen)"), ylab = "geschätztes 10%-Quantile")
            grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
            points(col.q, pch = 19, col = "blue")
            abline(h = q.true, lty = 5, col = "red")
            legend("topright", legend = c(paste0("10%-Quantil aus je ", input$nsamples, " Messwerten"), "Wahres 10%-Quantil"), lty = c(0, 5), col = c("blue", "red"), pch = c(19, NA))

        }
    })

    output$plot_ecdf <- renderPlot({
        F1 <- ecdf(data()[[1]])
        GW <- seq(input$mean - 3 * input$std, input$mean + 3 * input$std, 0.001)
        FW <- pnorm(GW, input$mean, input$std)

        par(lwd = 2, cex = 1.2)
        plot(NA, type = "l", ylab = "Verteilungsfunktion", xlab = "Wertebereich (Log10)", col = "blue", xlim = c(min(GW), max(GW)), ylim = c(0,1)) # Grafik: empirische Verteilungsfunktionen
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        lines(GW, F1(GW), col = "blue")
        lines(GW, FW, lty = 5, col = "red")
        legend("topleft", legend = c(paste0("Empirische Verteilung aus ", input$nsamples), "Wahre Verteilung"), lty = c(1, 5), col = c("blue", "red"))
    })

    output$boxplot <- renderPlot({
        par(lwd = 2, cex = 1.2)
        boxplot(data(), xlab = paste0("Boxplots (je ", input$nsamples, " Messungen)"), ylab = "Wertebereich", boxcol = "blue", medcol = "red")
        abline(h = input$mean, col = "green", lwd = 2)
    })

    output$survival_est <- renderPlot({
        C <- log10(0.6 * 10 ^ input$mean) # Ziellebensdauer

        c1 <- colSums(data() >= C) # Durchläufer bis C Schwingspiele
        p1 <- c1 / input$nsamples  # empirische Überlebenswahrscheinlichkeit
        pW <- 1 - pnorm(C, input$mean, input$std) # wahre Überlebenswahrscheinlichkeit

        par(mar = c(5.1, 4.1, 7.1, 2.1), lwd = 2, cex = 1.2)
        plot(x = NA, ylim = c(0,1), xlim = c(0,11), xlab = paste0("Versuch (je ", input$nsamples, " Messungen)"), ylab = "Überlebensws.") # Grafik: Lage der Überlebensws.
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        points(p1, col = "blue", pch = 19)
        abline(h = pW, lty = 5, col = "red")
        legend("bottomright", legend = c(paste0('Überlebensws. aus ', input$nsamples), 'Wahrer Wert'), pch = c(19, NA), lty = c(NA, 5), col = c("blue", "red"))
    })

    ## output$mark_quantiles <- renderText({
    ##     if (input$stat == "5%-Quantil" || input$stat == "10%-Quantil") {

    ## })

    output$survival_true <- renderPlot({
        pW <- 1 - pnorm(C, input$mean, input$std)
        pdf.n <- dbinom(0:input$nsamples, input$nsamples, pW) # Wahrscheinlichkeiten für 0 bis 10 Durchläufer unter 10 Versuchen
        cdf.n <- cumsum(pdf.n) # kumulative Ws = Verteilungsfunktion

        par(mar = c(5.1, 4.1, 7.1, 2.1), lwd = 2, cex = 1.2)
        barplot(rbind(pdf.n, cdf.n), xlab = "K = Anzahl der Durchläufer", ylab = "Wahrscheinlichkeit", beside = TRUE, col = c("darkblue", "darkred"), width = 0.25, space = c(0.2, 1.5), names.arg = 0:input$nsamples, ylim = c(-0.1, 1))
        abline(v = pW * input$nsamples, lty = 5, col = "green")
        par(xpd = NA)
        legend("topleft", legend = c(paste0('Wahrscheinlichkeit für K Durchläufer bei ', input$nsamples, ' Versuchen'), 'Verteilungsfunktion', 'Erwartete Anzahl Durchläufer'), lty = c(NA, NA, 5), col = c("darkblue", "darkred", "green"), fill = c("darkblue", "darkred", NA), border = c("black", "black", NA), inset = c(0, -0.4))
    })

    GW_C = seq(0, 7, 0.01) # Gitter für Kundenverteilung


    output$customer_distributions <- renderPlot({
        X_C1 = rnorm(input$nsamples2, input$m1, input$std1)
        X_C2 = rnorm(input$nsamples2, input$m2, input$std2)
        S = rbinom(input$nsamples2, 1, input$pm)
        X_C = X_C1 * S + X_C2 * (1 - S)

        FW_C = input$pm * pnorm(GW_C, input$m1, input$std1) + (1 - input$pm) * pnorm(GW_C, input$m2, input$std2) # Verteilungsfunktion der Kundenverteilung

        GW = seq(input$mean2 - 3 * input$std3, input$mean2 + 3 * input$std3, 0.001)
        FW = pnorm(GW, input$mean2, input$std3)
        X1 <- rnorm(input$nsamples2, input$mean2, input$std3)
        par(lwd = 2, cex = 1.2)
        plot(x = GW_C, pch = NA, xlim = c(0, 7), ylim = c(0, 1), xlab = "Wertebereich (Log10)", ylab = "Verteilungsfunktion") ## # Verteilungsfunktionen im Vergleich
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        lines(GW_C, ecdf(X_C)(GW_C), col = "blue")
        lines(GW_C, FW_C, col = "green")
        lines(GW_C, ecdf(X1)(GW_C), col = "red")
        lines(GW, FW, col = "magenta")
        legend("topleft", legend = c('Empirische Kundenbeanspruchung', 'Exakte Kundenbeanspruchung', 'Empirische Lebensdauer', 'Exakte Lebensdauer'), col = c("blue", "green", "red", "magenta"), lty = rep(1, 4))
    })

    output$customer_densities <- renderPlot({
        fW_C1 = dnorm(GW_C, input$m1, input$std1) # Dichte KG 1
        fW_C2 = dnorm(GW_C, input$m2, input$std2) # Dichte KG 2
        fW_C = input$pm * fW_C1 + (1 - input$pm) * fW_C2 # Dichte der Kundenverteilung
        GW = seq(input$mean2 - 3 * input$std3, input$mean2 + 3 * input$std3, 0.001)
        fW = dnorm(GW, input$mean2, input$std3)

        par(lwd = 2, cex = 1.2)
        plot(x = GW_C, pch = NA, xlim = c(0, 7), ylim = c(0, max(fW_C1, fW_C2, fW_C, fW)), xlab = "Wertebereich (Log10)", ylab = "Dichte") # Dichten im Vergleich
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        lines(GW_C, fW_C1, lty = 5, col = "green")
        lines(GW_C, fW_C2, lty = 5, col = "cyan")
        lines(GW_C, fW_C, col = "blue")
        lines(GW, fW, col = "red")
        legend("topleft", legend = c('Kundengruppe 1', 'Kundengruppe 2', 'Kundenbeanspruchung', 'Lebensdauer'), lty = c(5, 5, 1, 1), col = c("green", "cyan", "blue", "red"))
    })

    output$survival_code <- renderText({
        "Ein wenig Code"
    })
}


shinyApp(ui, server)
