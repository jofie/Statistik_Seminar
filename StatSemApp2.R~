library(shiny)
library(shinydashboard)
library(dplyr)
library(scales)
library(htmltools)
library(plotrix)

## Header:
hd <- dashboardHeader(titleWidth = 1450, title = "8 - Statistische Testmethoden: Beispiele")

## Sidebar:
sb <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem("Gauß-Test", tabName = "gauss", icon = icon("table")),
        menuItem("Binomialtest", tabName = "binomial", icon = icon("line-chart")),
        menuItem("Verteilungstest", tabName = "distribution", icon = icon("line-chart"))
    ),

    conditionalPanel(condition = "input.tabs == 'gauss'",
                     checkboxInput("show", "Zeige Simulationsparameter"),
                     conditionalPanel("input.show",
                                      numericInput("mean1", "Mittelwert Versuch 1", 295, step = 5),
                                      numericInput("mean2", "Mittelwert Versuch 2", 305, step = 5),
                                      numericInput("mean3", "Mittelwert Versuch 3", 315, step = 5),
                                      numericInput("std", "Standardabweichung in allen Versuchen", 15, step = 5)
                                      ),
                     numericInput("mean", "Forderung an Mittelwert (H0)", 300, step = 5)
                     ),
    conditionalPanel(condition = "input.tabs == 'binomial' || input.tabs == 'distribution'",
                     conditionalPanel("input.binomial_tab_selection != 'weibull_vis'",
                                      numericInput("b", "Skalenparameter", 2, step = 0.1),
                                      numericInput("t", "Lebensdauer T", 500000, step = 25000)
                                      )
                     ),
    conditionalPanel(condition = "input.tabs == 'binomial'",
                     conditionalPanel("input.binomial_tab_selection != 'weibull_vis'",
                                      numericInput("q1", "Forderung an 1%-Quantil", 60000, step = 5000),
                                      numericInput("q2", "Forderung an 5%-Quantil", 120000, step = 5000),
                                      numericInput("q3", "Forderung an 10%-Quantil", 180000, step = 5000)
                                      )
                     ),
    uiOutput("nsamples"),
    conditionalPanel("input.tabs == 'binomial' || input.tabs == 'gauss'",
                     numericInput("niveau", "Testniveau", 0.05, step = 0.01)
                     ),
    actionButton("simulate", "Simuliere Daten neu")
)


## Box for table output:
gauss.output <- fluidPage(
    width = NULL, align = "center",
    tabsetPanel(
        id = "gauss_tab_selection",
        tabPanel("Simulation", value = "sim",  align = "center",
                 box(title = "R-Beispielcode", width = NULL, height = 100, align = "center", solidHeader = TRUE,
                     div(textOutput("code_table"), style = "font-family: 'Courier New'")
                     ),
                 box(title = "Simulierte Messwerte", width = NULL, align = "center",
                     tabBox(width = NULL, id = "messreihe",
                            tabPanel(title = textOutput("mean1"), width = NULL, height = 300, value = "eins",
                                     tableOutput("table1"),
                                     tags$style(
                                         type="text/css",
                                         "#table1 td:first-child {font-style:italic;}",
                                         "#table1 {font-size:16px}"
                                     )
                                     ),
                            tabPanel(title = textOutput("mean2"), width = NULL, height = 300, value = "zwei",
                                     tableOutput("table2"),
                                     tags$style(
                                         type="text/css",
                                         "#table2 td:first-child {font-style:italic;}",
                                         "#table2 {font-size:16px}"
                                     )
                                     ),
                            tabPanel(title = textOutput("mean3"), width = NULL, height = 300, value = "drei",
                                     tableOutput("table3"),
                                     tags$style(
                                         type="text/css",
                                         "#table3 td:first-child {font-style:italic;}",
                                         "#table3 {font-size:16px}"
                                     )
                                     )
                            )
                     )
                 ),
        tabPanel("Plots", value = "plots",
                 h2("Visualisierung der Testergebnisse des Gauß-Tests"),
                 box(width = NULL,
                     plotOutput("stat_plot")
                     ),
                 box(width = NULL,
                     plotOutput("test_result_plot")
                     )
                 )
    )
)

binomial.output <- fluidPage(
    width = NULL, align = "center",
    tabsetPanel(
        id = "binomial_tab_selection",
        tabPanel("Quantile", value = "quantiles", align = "center",
                 box(title = textOutput("weibull_dist"), width = NULL, align = "center",
                     tableOutput("table_quantiles"),
                                     tags$style(
                                         type="text/css",
                                         "#table_quantiles td:first-child {font-style:italic;}",
                                         "#table_quantiles {font-size:16px}"
                                     )
                     )
                 ),
        tabPanel("Simulation", value = "weibull_sim", align = "center",
                 box(title = textOutput("weibull_dist_sim"), width = NULL, align = "center",
                     tableOutput("table_weibull"),
                                     tags$style(
                                         type="text/css",
                                         "#table_weibull td:first-child {font-style:italic;}",
                                         "#table_weibull {font-size:16px}"
                                     )
                     ),
                 box(title = "Anzahl Durchläufer", width = NULL, align = "center",
                     tableOutput("table_survivors"),
                                     tags$style(
                                         type="text/css",
                                         "#table_survivors td:first-child {font-style:italic;}",
                                         "#table_survivors {font-size:16px}"
                                     )
                     )
                 ),
        tabPanel("Visualisierungen", value = "weibull_vis", align = "center",
                 tabBox(width = NULL,
                        tabPanel("quantile_plot_1",
                                 plotOutput("quantile_plot_1")
                                 ),
                        tabPanel("quantile_plot_2",
                                 plotOutput("quantile_plot_2")
                                 )
                        )
                 )
    )
)

distribution.output <- fluidPage(
    width = NULL, align = "center",
    tabsetPanel(
        id = "distribution_tab_selection",
        tabPanel("Empirische Verteilungsfunktionen", align = "center", value = "ecdf",
                 box(title = textOutput("title_ecdf"), width = NULL, align = "center",
                     box(title = "R-Beispielcode", width = NULL, height = 100, align = "center", solidHeader = TRUE,
                         div("bla", style = "font-family: 'Courier New'")
                         ),
                     box("Empirische Verteilungsfunktionen", width = 6, align = "center",
                         plotOutput("plot_ecdf")
                         ),
                     box("Dichten", width = 6, align = "center",
                         plotOutput("plot_densities")
                         )
                     )
                 ),
         tabPanel("Histogramme", align = "center", value = "hist",
                  box(title = textOutput("title_hist"), width = NULL, align = "center",
                       box(title = "R-Beispielcode", width = NULL, height = 100, align = "center",
                         div("blaa", style = "font-family: 'Courier New'")
                         ),
                      box("Histogramme", width = 6, align = "center",
                          plotOutput("plot_hist")
                          ),
                      box("Dichten", width = 6, align = "center",
                          plotOutput("plot_densities_2")
                          )
                      )
                  ),
        tabPanel("Boxplot", align = "center", value = "boxplot",
                 box(title = textOutput("title_boxplot"), width = NULL, align = "center",
                     box(title = "R-Beispielcode", width = NULL, height = 100, align = "center", solidHeader = TRUE,
                         div("bla", style = "font-family: 'Courier New'")
                         ),
                     box("Histogramme", width = 6, align = "center",
                         plotOutput("plot_boxplot")
                         ),
                     box("Dichten", width = 6, align = "center",
                         plotOutput("plot_densities_3")
                         )
                     )
                 )

    )
)



## Tab items:
tab.gauss <- tabItem(
    tabName = "gauss",
    h2("Gauß-Test"),
    fluidRow(gauss.output)
)

tab.binomial <- tabItem(
    tabName = "binomial",
    h2("Binomialtest"),
    fluidRow(binomial.output)
)

tab.distribution <- tabItem(
    tabName = "distribution",
    h2("Vergleich von Weibull- mit lognormal-Verteilung"),
    fluidRow(distribution.output)
)

## Putting the body together:
body <- dashboardBody(
    fluidRow(
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
        tabItems(tab.gauss, tab.binomial, tab.distribution)
        )
)

## Defining the UI:
ui <- dashboardPage(hd, sb, body)



## Define the server function:
server <- function(input, output, session) {

    gauss_tab_selection <- reactive({
        value <- input$gauss_tab_selection
        if(is.null(value))
            value <- NULL
        return (value)
    })

    output$nsamples <- renderUI({
        value <- 6 * (input$tabs == "gauss") + 10 * (input$tabs == "binomial") + 1000 * (input$tabs == "distribution")
        step <- 1 * (input$tabs == "gauss" || input$tabs == "binomial") + 100 * (input$tabs == "distribution")
        numericInput("nsamples", "Anzahl an Messungen pro Messreihe", value, 0, step = step)
    })

    output$code_table <- renderText({
        if (input$messreihe == "eins") {
            paste0("X1 <- normrnd(", input$mean1, ", ", input$std, ", ", input$nsamples, ", ", "10)")
        } else if (input$messreihe == "zwei") {
            paste0("X2 <- normrnd(", input$mean2, ", ", input$std, ", ", input$nsamples, ", ", "10)")
        } else if (input$messreihe == "drei") {
            paste0("X3 <- normrnd(", input$mean3, ", ", input$std, ", ", input$nsamples, ", ", "10)")
        }
    })

    data <- function(nsamples, mean, std) {
        df <- rnorm(nsamples * 10, mean, std) %>%
            matrix(ncol = 10) %>%
            as.data.frame
        names(df) <- c("Messreihe 1", "Messreihe 2", "Messreihe 3", "Messreihe 4", "Messreihe 5",
                       "Messreihe 6", "Messreihe 7", "Messreihe 8", "Messreihe 9", "Messreihe 10")
        row.names(df) <- lapply(1:nsamples, function(x) paste0("Messung ", x)) %>%
            unlist
        s <- apply(df, 2, mean) %>% data.frame %>% t
        row.names(s) <- "Mittelwert"
        return (rbind(df, s))
    }

    output$mean1 <- renderText({
        paste0("Versuch 1 mit Mittelwert ", input$mean1, " (X1)")
    })

    output$mean2 <- renderText({
        paste0("Versuch 2 mit Mittelwert ", input$mean2, " (X2)")
    })

    output$mean3 <- renderText({
        paste0("Versuch 3 mit Mittelwert ", input$mean3, " (X3)")
    })

    output$table1 <- renderTable({
        req(input$nsamples)
        input$simulate
        data(input$nsamples, input$mean1, input$std)},
        include.rownames = TRUE
        )

    output$table2 <- renderTable({
        req(input$nsamples)
        input$simulate
        data(input$nsamples, input$mean2, input$std)},
        include.rownames = TRUE
        )

    output$table3 <- renderTable({
        req(input$nsamples)
        input$simulate
        data(input$nsamples, input$mean3, input$std)},
        include.rownames = TRUE
        )

    output$stat_plot <- renderPlot({
        input$simulate
        q_crit <- qnorm(0.95, input$mean, input$std / sqrt(input$nsamples))
        X1 <- matrix(rnorm(10 * input$nsamples, input$mean1, input$std), input$nsamples)
        X2 <- matrix(rnorm(10 * input$nsamples, input$mean2, input$std), input$nsamples)
        X3 <- matrix(rnorm(10 * input$nsamples, input$mean3, input$std), input$nsamples)
        p1 <- sum(colMeans(X1) <= q_crit) / 10
        p1_exact <- pnorm(q_crit, input$mean1, input$std / sqrt(input$nsamples))
        p2 <- sum(colMeans(X2) > q_crit) / 10
        p2_exact <- 1 - pnorm(q_crit, input$mean2, input$std / sqrt(input$nsamples))
        p3 <- sum(colMeans(X3) > q_crit) / 10
        p3_exact <- 1 - pnorm(q_crit, input$mean3, input$std / sqrt(input$nsamples))

        V <- cbind(1:10, 1:10, 1:10)
        M <- cbind(colMeans(X1), colMeans(X2), colMeans(X3))
        R <- cbind(
            as.logical((colMeans(X1) <= q_crit) * (input$mean1 <= input$mean) + (colMeans(X1) > q_crit) * (input$mean1 > input$mean)),
            as.logical((colMeans(X2) > q_crit) * (input$mean2 > input$mean) + (colMeans(X2) <= q_crit) * (input$mean2 <= input$mean)),
            as.logical((colMeans(X3) > q_crit) * (input$mean3 > input$mean) + (colMeans(X3) <= q_crit) * (input$mean3 <= input$mean))
        ) # richtige Klassifikation
        V <- V[R] # Auswahl aller richtig klassifizierten Werte
        M <- M[R]

        par(mar = c(5.1, 4.1, 4.1, 15), cex = 1.2, lwd = 2)
        plot(NA, xlim = c(0, 11), ylim = c(input$mean - 15, input$mean + 28), xlab = paste0("Messreihe (je ", input$nsamples, " Messungen)"), ylab = "Mittelwert") # Mittelwerte im Vergleich
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        points(colMeans(X1), col = "blue", pch = 4)
        points(colMeans(X2), col = "green", pch = 4)
        points(colMeans(X3), col = "red", pch = 4)
        points(V, M, pch = 1, cex = 1.6)
        abline(h = q_crit, lty = 5)
        abline(h = input$mean, col = "magenta", lty = 5)
        par(xpd = NA)
        legend("topright", legend = c(paste0('Wahrer Mittelwert ', input$mean1), paste0('Wahrer Mittelwert ', input$mean2), paste0('Wahrer Mittelwert ', input$mean3), 'Korrekt klassifiziert', 'Kritischer Wert', paste0('Hypothese: mu_0 = ', input$mean)), col = c("blue", "green", "red", "black", "black", "magenta"), pch = c(4, 4, 4, 1, NA, NA), lty = c(NA, NA, NA, NA, 5, 5), inset = c(-0.25, 0))
    })

    output$test_result_plot <- renderPlot({
        muW <- seq(290, 320, 0.01) # wahre Mittelwerte
        q_crit <- qnorm(1 - input$niveau, input$mean, input$std / sqrt(input$nsamples))
        p_H0 <- pnorm(q_crit, muW, input$std / sqrt(input$nsamples)) # Akzeptanzws. fuer H0 als Funktion des wahren Mittelwerts
        p_H1 <- 1 - p_H0 # Akzeptanzws. fuer H1 als Funktion des wahren Mittelwerts

        par(mar = c(5.1, 4.1, 4.1, 15), cex = 1.2, lwd = 2)
        plot(NA, xlab = "Wahrer Mittelwert", ylab = "Wahrscheinlichkeit", main = paste0("Versuch mit ", input$nsamples, " Messungen"), xlim = c(290, 320), ylim = c(0, 1))
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        lines(muW, p_H0, col = "blue")
        lines(muW, p_H1, col = "green")
        abline(v = q_crit, lty = 5)
        abline(v = input$mean, lty = 5, col = "magenta")
        abline(h = input$niveau, col = "red")
        par(xpd = NA)
        legend("right", legend = c('Akzeptanz von H0', 'Ablehnung von H0', 'Kritischer Wert', paste0('Hypothese: mu_0 = ', input$mean), paste0('Niveau ', input$niveau * 100, "%")), inset = c(-0.25, 0), col = c("blue", "green", "black", "magenta", "red"), lty = c(1, 1, 5, 5, 1))
    })


    output$weibull_dist <- renderText({
        paste0("Geforderte Quantile und wahre Quantile der Beispielverteilung (Weibull mit T = ", input$t, " und b = ", input$b, ")")
    })

    claimed_quantiles <- reactive({
        q_claimed <- c(input$q1, input$q2, input$q3) %>% data.frame %>% t %>% round
        colnames(q_claimed) <- c("1%-Quantil", "5%-Quantil", "10%-Quantil")
        row.names(q_claimed) <- "geforderter Wert"
        return (q_claimed)
    })

    true_quantiles <- reactive({
        q_true <- qweibull(c(0.01, 0.05, 0.1), input$b, input$t) %>% data.frame %>% t %>% round
        colnames(q_true) <- c("1%-Quantil", "5%-Quantil", "10%-Quantil")
        row.names(q_true) <- "wahrer Wert"
        return (q_true)
    })

    output$table_quantiles <- renderTable({
        rbind(claimed_quantiles(), true_quantiles())},
        include.rownames = TRUE,
        digits = 0)

    data_weibull <- reactive({
        input$simulate
        df <- rweibull(input$nsamples * 10, input$b, input$t) %>%
            matrix(ncol = 10) %>%
            as.data.frame
        names(df) <- c("Messreihe 1", "Messreihe 2", "Messreihe 3", "Messreihe 4", "Messreihe 5",
                       "Messreihe 6", "Messreihe 7", "Messreihe 8", "Messreihe 9", "Messreihe 10")
        row.names(df) <- lapply(1:input$nsamples, function(x) paste0("Messung ", x)) %>%
            unlist
        return (df)
    })

    output$weibull_dist_sim <- renderText({
        paste0("10 simulierte Messreihen mit je ", input$nsamples, " Messwerten (Weibull-Verteilung mit T = ", input$t, " und b = ", input$b, ")")
    })

    output$table_weibull <- renderTable({
        data_weibull()},
        include.rownames = TRUE,
        digits = 0
        )

    output$table_survivors <- renderTable({
        survivor_q1 <- colSums(data_weibull() >= input$q1)
        survivor_q2 <- colSums(data_weibull() >= input$q2)
        survivor_q3 <- colSums(data_weibull() >= input$q3)
        df <- t(data.frame(survivor_q1, survivor_q2, survivor_q3))
        names(df) <- c("Messreihe 1", "Messreihe 2", "Messreihe 3", "Messreihe 4", "Messreihe 5",
                       "Messreihe 6", "Messreihe 7", "Messreihe 8", "Messreihe 9", "Messreihe 10")
        row.names(df) <- c("für gefordertes 1%-Quantil", "für gefordertes 5%-Quantil", "für gefordertes 10%-Quantil")
        df},
        include.rownames = TRUE,
        digits = 0
        )

    p_quantile <- seq(0, 0.4, 0.0005) # diverse Quantil-Ws.
    a_quantile <- (1 - p_quantile) ^ 10 # Niveau des Tests fuer 10 Bauteile

    q_min <- reactive({
        1 - input$niveau ^ (1 / input$nsamples)
    })

    output$quantile_plot_1 <- renderPlot({
        par(mar = c(5.1, 4.1, 4.1, 20.1), cex = 1.1, lwd = 2)
        plot(NA, xlab = "Nachzuweisende Quantilwahrscheinlichkeit", ylab = "Niveau", main = paste0(input$nsamples, " Messungen, keine Ausfälle erlaubt"), xlim = c(0, 0.4), ylim = c(0, 1))
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        lines(p_quantile, a_quantile, col = "blue")
        abline(h = input$niveau, col = "green")
        abline(v = 0.01, col = "red")
        abline(v = 0.05, col = "red")
        abline(v = 0.1, col = "red")
        abline(v = q_min(), col = "black", lty = 5)
        text(x = q_min() + 0.01, y = 0.5, labels = paste0(round(q_min(), 3) * 100, "%"), cex = 0)
        par(xpd = NA)
        legend("topright", legend = c('Niveau', paste0(100 * input$niveau, "%-Niveau"), 'Quantilws. aus Beispiel', paste0("minimal nachweisbares Quantil\nzum Niveau ", 100 * input$niveau, "%")), lty = c(1, 1, 1, 5), col = c("blue", "green", "red", "black"), inset = c(-0.30, 0))
    })

    output$quantile_plot_2 <- renderPlot({
        input$simulate
        N_quantile <- ceiling(log(input$niveau) / log(1 - p_quantile)) # Anzahl Bauteile fuer Niveau 5%
        N_max <- 350 * (input$nsamples <= 300) + 1000 * (input$nsamples > 300 && input$nsamples <= 900) + 1750 * (input$nsamples > 900 && input$nsamples <= 1500)

        par(mar = c(5.1, 4.1, 4.1, 20.1), cex = 1.1, lwd = 2) # Anzahl Versuche in Abhaengigkeit von der Quantilws.
        plot(NA, xlab = "Nachzuweisende Quantilwahrscheinlichkeit", ylab = "Anzahl Messungen", main = paste0("Niveau ", 100 * input$niveau, "%, keine Ausfälle erlaubt"), xlim = c(0, 0.4), ylim = c(0, N_max))
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        lines(p_quantile, N_quantile, col = "blue")
        abline(h = input$nsamples, col = "green")
        abline(v = 0.01, col = "red")
        abline(v = 0.05, col = "red")
        abline(v = 0.1, col = "red")
        abline(v = q_min(), col = "black", lty = 5)
        text(x = q_min() + 0.01, y = N_max / 2, labels = paste0(round(q_min(), 3) * 100, "%"), cex = 0)
        par(xpd = NA)
        legend("topright", legend = c('Anzahl', paste0(input$nsamples, ' Messungen'), 'Quantilws. aus Beispiel', paste0("minimal nachweisbares Quantil\nzum Niveau ", 100 * input$niveau, "%")), lty = c(1, 1, 1, 5), col = c("blue", "green", "red", "black"), inset = c(-0.30, 0))
    })

    output$title_ecdf <- renderText({
        paste0("Vergleich der empirischen Verteilungsfunktionen von Weibull- und lognormal-Verteilung")
    })

    output$title_hist <- renderText({
        paste0("Histogrammvergleiche für ", input$nsamples, " simulierte Weibull- und lognormal-verteilter Messergebnisse")
    })

    output$title_boxplot <- renderText({
        paste0("Boxplot für ", input$nsamples, " simulierte Weibull- und lognormal-verteilter Messergebnisse")
    })


    X_wbl <- reactive({
        input$simulate
        log10(rweibull(input$nsamples, input$b, input$t))
    })

    m_wbl <- reactive({
        mean(X_wbl())
    })

    s_wbl <- reactive({
        sqrt(var(X_wbl()))
    })

    X_norm <- reactive({
        rnorm(input$nsamples, m_wbl(), s_wbl())
    })

    G <- seq(3.5, 7, 0.01)

    output$plot_ecdf <- renderPlot({
        req(input$nsamples, input$b, input$t)
        par(cex = 1.1, lwd = 2) # Verteilungen im Vergleich
        plot(NA, xlim = c(3.5, 7), ylim = c(0, 1), xlab = "Wertebereich", ylab = "Empirische Verteilung")
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        lines(G, ecdf(X_wbl())(G), col = "blue")
        lines(G, ecdf(X_norm())(G), col = "red")
        legend("topleft", legend = c('Weibull', 'Lognormal'), lty = c(1, 1), col = c("blue", "red"), cex = 1.2)
    })

    output$plot_hist <- renderPlot({
        n_max <- input$nsamples / 2.5
        multhist(list(X_wbl(), X_norm()), col = c("darkblue", "darkred"), ylim = c(0, round(n_max)), xlab = "Wertebereich", ylab = "Histogramm")
        legend("topleft", legend = c('Weibull', 'Lognormal'), col = c("darkblue", "darkred"), fill = c("darkblue", "darkred"), cex = 1.2)
    })

    output$plot_boxplot <- renderPlot({
        boxplot(cbind("Weibull" = X_wbl(), "Lognormal" = X_norm()), boxcol = "blue", medlwd = 2, medcol = "red", outpch = 3, outcol = "red", ylab = "Wertebereich", ylim = c(3.5, 7))})

    output$plot_densities <- output$plot_densities_2 <- output$plot_densities_3 <- renderPlot({
        req(input$t, input$b)
        f_wbl <- log(10) * dweibull(10 ^ G, input$b, input$t) * 10 ^ G # Weibull-Dichte transformiert auf Log-Skala
        f_norm <- dnorm(G, m_wbl(), s_wbl())
        y_max <- 1.2 * max(f_norm, f_wbl)
        par(lwd = 2, cex = 1.1)
        plot(NA, xlim = c(min(G), max(G)), ylim = c(0, y_max), xlab = "Wertebereich", ylab = "Exakte Dichte")
        grid(lty = 3, col = alpha("black", 0.25), lwd = 0.5)
        lines(G, f_wbl, col = "blue")
        lines(G, f_norm, col = "red")
        legend("topleft", legend = c('Weibull', 'Lognormal'), col = c("blue", "red"), lty = c(1, 1), cex = 1.2)
    })

    }


shinyApp(ui, server)
