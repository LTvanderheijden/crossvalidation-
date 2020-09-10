#Load required libraries 
library(shiny)
library(shinythemes)
library(ggplot2)
library(blandr)
library(rhandsontable)
library(mcr)


#Bland-altman function
proportional_BA <- function(method1, method2, sig.level = 0.95, LoA.mode = 1) 
{
        ba.data <- blandr.data.preparation(method1, method2, sig.level)
        means <- (ba.data$method1 + ba.data$method2)/2
        differences <- ((ba.data$method1 - ba.data$method2))/means*100 #aanpassing 
        bias <- mean(differences)
        biasStdDev <- sd(differences)
        alpha <- 1 - sig.level
        sig.level.two.tailed <- 1 - (alpha/2)
        sig.level.convert.to.z <- qnorm(sig.level.two.tailed)
        if (LoA.mode == 2) {
                LoA.multiplier <- 2
        }
        else {
                LoA.multiplier <- 1.96
        }
        upperLOA <- bias + (LoA.multiplier * biasStdDev)
        lowerLOA <- bias - (LoA.multiplier * biasStdDev)
        biasSEM <- sd(differences)/sqrt(length(differences))
        biasCI <- qt(sig.level.two.tailed, df = length(differences) - 
                             1) * biasSEM
        biasUpperCI <- bias + biasCI
        biasLowerCI <- bias - biasCI
        LOAVariance <- ((1/length(differences)) + ((sig.level.convert.to.z^2)/(2 * 
                                                                                       (length(differences) - 1)))) * biasStdDev^2
        LOA_SEM <- sqrt(LOAVariance)
        LOA_CI <- qt(sig.level.two.tailed, df = length(differences) - 
                             1) * LOA_SEM
        upperLOA_upperCI <- upperLOA + LOA_CI
        upperLOA_lowerCI <- upperLOA - LOA_CI
        lowerLOA_upperCI <- lowerLOA + LOA_CI
        lowerLOA_lowerCI <- lowerLOA - LOA_CI
        proportion <- differences/means * 100
        no.of.observations <- length(means)
        m <- lm(differences ~ means)
        a <- signif(coef(m)[1], digits = 2)
        b <- signif(coef(m)[2], digits = 2)
        regression.equation <- paste("y(differences) = ", b, " x(means) + ", 
                                     a, sep = "")
        return(list(means = means, differences = differences, method1 = method1, 
                    method2 = method2, sig.level = sig.level, sig.level.convert.to.z = sig.level.convert.to.z, 
                    bias = bias, biasUpperCI = biasUpperCI, biasLowerCI = biasLowerCI, 
                    biasStdDev = biasStdDev, biasSEM = biasSEM, LOA_SEM = LOA_SEM, 
                    upperLOA = upperLOA, upperLOA_upperCI = upperLOA_upperCI, 
                    upperLOA_lowerCI = upperLOA_lowerCI, lowerLOA = lowerLOA, 
                    lowerLOA_upperCI = lowerLOA_upperCI, lowerLOA_lowerCI = lowerLOA_lowerCI, 
                    proportion = proportion, no.of.observations = no.of.observations, 
                    regression.equation = regression.equation, regression.fixed.slope = b, 
                    regression.fixed.intercept = a))
}

#Rounding function that round 0.5 to highest number. The R base ruond function
#rounds to the lowest number. 
round2 = function(x, n) {
        posneg = sign(x)
        z = abs(x)*10^n
        z = z + 0.5
        z = trunc(z)
        z = z/10^n
        z*posneg
}

#Shiny sever -----------------------------------------------------------
server <- shinyServer(function(input, output){
        #Time of analysis 
        analysis_time <- reactive({return(Sys.time())})
        
        #Create data.frame to display in the rhandsontable
        x <- runif(10)
        y <- x*runif(10, min = -1, max = 2)
        df <- data.frame(x, y)
        names(df) <- c('Methode 1', "Methode 2")
        
        #Maak de waarden in de datafram reactief 
        values <- reactiveValues(data = df)
        
        
        observe({
                if(!is.null(input$hot)){
                        values$data <- as.data.frame(hot_to_r(input$hot))
                }
        })
        
        output$hot <- renderRHandsontable({
                #Decimals of the input 
                d <- input$decimals
                dec <- format(round(0, 2), nsmall = d)
                rhandsontable(values$data, readOnly = FALSE) %>% 
                        hot_cols(format = dec)
        })
        
        lab1 <- eventReactive(input$go, {
                return(values$data[,1])
        })
        lab2 <- eventReactive(input$go, {
                return(values$data[,2])
        })
        
        #Names of the two methods 
        naam1 <- reactive({
                return(input$name1)
        })
        naam2 <- reactive({
                return(input$name2)
        })
        
        #Recreation of data input table
        datainput <- reactive({
                DF <- data.frame(lab1(), lab2()) 
                names(DF) <- c(naam1(), naam2())
                return(DF)
        })
        
        
        #Bland-Altmand 
        plot_BA <- reactive({
                DF <- data.frame(lab1(), lab2())
                #Calculate BA statistics 
                results_BA <- proportional_BA(DF[,1], DF[,2])
                
                x_BA <- paste("(", naam1(), " - ", naam2(), ")", "/average (%)", sep = "")
                y_BA <- paste("(", naam1(), " + ", naam2(), ")", "/2", sep = "")
                
                #Plot BA plot 
                BA <- blandr.plot.ggplot(results_BA,
                                         plotTitle = "Bland-Altman plot" ,
                                         ciDisplay = FALSE, ciShading = FALSE)+
                        ylab(x_BA)+
                        xlab(y_BA)+
                        theme_classic()
                return(BA)
        })
        #Display output 
        output$plot1 <- renderPlot({
                plot_BA()
        }, width = 500, height = 400)
        
        #Calculate number of observations 
        nobs <- reactive({ 
                DF <- data.frame(lab1(), lab2())
                #Calculate BA statistics 
                results_BA_BA <- proportional_BA(DF[,1], DF[,2])
                
                n <- paste("N", results_BA_BA$no.of.observations, sep = " | ")
                return(n)
        })
        
        output$n_obs <- renderText({
                nobs()
        })
        
        results_decimalen <- eventReactive(input$decimals2, {return(input$decimals2)})
        
        #Make summary of descriptive statistics
        sum_stat <- reactive({
                DF <- data.frame(lab1(), lab2())
                #Calculate descriptive statistics 
                minima <- c(min(DF[,1], na.rm = TRUE), min(DF[,2], na.rm = TRUE),
                            min((DF[,1] + DF[,2])/2, na.rm = TRUE))
                maxima <- c(max(DF[,1], na.rm = TRUE), max(DF[,2], na.rm = TRUE),
                            max((DF[,1] + DF[,2])/2, na.rm = TRUE))
                method <- c(naam1(), naam2(), "Gemiddelde")
                
                #Afronding resultaat
                results_dec <- input$decimals2
                minima <- round2(minima, results_dec)
                maxima <- round2(maxima, results_dec)
                
                #Create data frame
                sum_dat <- data.frame(method, minima, maxima)
                names(sum_dat) <- c("Methode", "Min", "Max")
                return(sum_dat)
        })
        
        output$summary <- renderTable({
                sum_stat()
                
        }, digits = results_decimalen)
        
        #Recreate table of the statistics
        precision_sum <- reactive({
                #Get data input
                DF <- data.frame(lab1(), lab2())
                
                #Calculate BA statistics
                results_BA <- results_BA <- proportional_BA(DF[,1], DF[,2])
                
                #Afronding resultaat
                results_dec <- input$decimals2
                
                #Parameter estimates
                estimates <- round2(c(results_BA$bias, results_BA$lowerLOA, results_BA$upperLOA),
                                    results_dec)
                
                #95% CI of estimates
                lower_interval <- c(
                        round2(results_BA$biasLowerCI, results_dec),
                        round2(results_BA$lowerLOA_lowerCI, results_dec),
                        round2(results_BA$upperLOA_lowerCI, results_dec)
                )
                upper_interval <- c(
                        round2(results_BA$biasUpperCI, results_dec),
                        round2(results_BA$upperLOA_lowerCI, results_dec),
                        round2(results_BA$upperLOA_upperCI, results_dec)
                )
                
                ci <- paste(lower_interval, upper_interval, sep = " to ")
                
                #Standard error
                SE <- round2(
                        c(results_BA$biasSEM, results_BA$LOA_SEM, results_BA$LOA_SEM),
                        results_dec)
                
                #Parameter names
                Parameter <- c("Mean difference", "95% Lower LoA", "95% Upper LoA")
                
                #Dataframe assembly 
                tab <- data.frame(Parameter, estimates, ci , SE)
                names(tab) <- c("Parameter", "Estimate", "95% CI", "SE")
                return(tab)
        })
        
        output$betrouwbaarheid <- renderTable({
                precision_sum()
                
        })
        
        #Passing-Bablock regressie
        results_PB <- reactive({
                #Add input observations together in one dataset 
                DF <- data.frame(lab1(), lab2())
                
                fit <- mcreg(x = DF[,1], y = DF[,2], error.ratio = 1, 
                             alpha = 0.05, method.reg = "PaBa", method.ci = "bootstrap", 
                             nsamples = 999, method.bootstrap.ci = "quantile",
                             na.rm = TRUE)
                return(fit)
        })
        
        #Calculate number of observations 
        output$n_obs_PB <- renderText({
                nobs()
        })
        
        #Assembly of summary of descriptive statistics 
        sum_PB <- reactive({
                DF <- data.frame(lab1(), lab2())
                #Calculate descriptive statistics 
                minima <- c(min(DF[,1], na.rm = TRUE), min(DF[,2], na.rm = TRUE))
                maxima <- c(max(DF[,1], na.rm = TRUE), max(DF[,2], na.rm = TRUE))
                method <- c(naam1(), naam2())
                
                #Rounding of the results 
                results_dec <- input$decimals2
                minima <- round2(minima, results_dec)
                maxima <- round2(maxima, results_dec)
                
                #Create data frame
                PB_sum <- data.frame(method, minima, maxima)
                names(PB_sum) <- c("Methode", "Min", "Max")
                return(PB_sum)
        })
        output$summary_PB <- renderTable({sum_PB()}, digits = results_decimalen)
        
        #Equation 
        formule <- reactive({ 
                results_dec <- input$decimals2
                
                fit_PB <- results_PB()
                intercept <- round2(fit_PB@para[1,1], results_dec)
                slope <- round2(fit_PB@para[2,1], results_dec)
                form <- paste(naam2(),"=", intercept, "+", slope, 
                              naam1(), sep = " ")
                return(form)
        })
        
        output$equation <- renderText({formule()})
        
        #Confidence interval of passing_bablock
        ci_pb <- reactive({
                results_dec <- input$decimals2
                
                fit_PB <- results_PB()
                intercept <- round2(fit_PB@para[1,1], results_dec)
                slope <- round2(fit_PB@para[2,1], results_dec)
                estimates_PB <- c(intercept, slope)
                
                boots_95CI <- c(paste(round2(fit_PB@para[1,3], results_dec),
                                      round2(fit_PB@para[1,4], results_dec),
                                      sep = " to "), 
                                paste(round2(fit_PB@para[2,3], results_dec),
                                      round2(fit_PB@para[2,4], results_dec),
                                      sep = " to ")
                )
                rownames <- c("Inercept", "Slope")
                dat_ci_PB <- data.frame(rownames, estimates_PB, boots_95CI)
                names(dat_ci_PB) <- c("", "Estimate", "Bootstrap 95% CI")
                return(dat_ci_PB)
        })
        
        output$betrouwbaarheid_PB <- renderTable({ci_pb()}, digits = results_decimalen)
        
        #Passing bablock plot
        plot_PB <- reactive({ 
                fit_PB <- results_PB()
                
                #Create plot
                plot2 <- ggplot()+
                        #Add datapoints method 1 vs method 2
                        geom_point(aes(x = lab1(), y = lab2()))+
                        #Add identity line
                        geom_abline(aes(slope = 1, intercept = 0, color = "lightgrey"))+
                        #Add passing bablock regression line
                        geom_abline(aes(slope = fit_PB@para[2,1], intercept = fit_PB@para[1,1], 
                                        color = "red"))+
                        scale_color_identity(labels=c("Identity", formule()), guide="legend", name = " ")+
                        scale_x_continuous(name = naam1(), expand = c(0,0))+
                        scale_y_continuous(name = naam2(), expand = c(0,0))+
                        theme_classic()
                return(plot2)
        })
        output$plot2 <- renderPlot({
                plot_PB()
        }, width = 500, height = 400)
        
        
        #Download report
        output$download <- downloadHandler(
                filename = function(){
                        paste("Cross-validatie", Sys.Date(), ".doc", sep = "")
                },
                content = function(file){
                        src <- normalizePath("report.Rmd")
                        
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, 'report.Rmd')
                        
                        library(rmarkdown)
                        out <- render(input = 'report.Rmd',
                                      output_format =  word_document()
                        )
                        file.rename(out, file)
                }
        )
}
)

#--- ui --------------------------------------------------------------------------
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        title = "Cross-validatie van bioanalytise methoden",
                        navbarPage("Cross-validatie van bioanalyse methoden",
                                   tabPanel(value = "tab1", title = "Data input",
                                            sidebarLayout(
                                                    sidebarPanel(
                                                            textInput("name1", label = "Naam methode 1", placeholder = NULL),
                                                            textInput("name2", label = "Naam methode 2", placeholder = NULL),
                                                            sliderInput("decimals", "Aantal decimalen input:",
                                                                        min = 0, max = 5, step = 1, value = 2),
                                                            sliderInput("decimals2", "Aantal decimalen output:",
                                                                        min = 0, max = 5, step = 1, value = 2),
                                                            actionButton("go", "Start cross-validatie"),
                                                            downloadButton(outputId = "download", 
                                                                           label = "Download cross-validatie report")
                                                    ),
                                                    mainPanel(
                                                            rHandsontableOutput("hot")
                                                    )
                                            )
                                   ), 
                                   tabPanel(value = "tab2", title = "Bland-Altman analyse",
                                            h2("Bland-Altman plot"),
                                            plotOutput("plot1"),
                                            h2("Bland-Altman statistiek"),
                                            h3("Aantal observaties"),
                                            textOutput("n_obs"),
                                            h3("Beschrijvende statistiek"),
                                            tableOutput("summary"),
                                            h3("95% betrouwbaarheidsintervallen parameters"),
                                            tableOutput("betrouwbaarheid")
                                   ),
                                   tabPanel(value = "tab3", title = "Passing-Bablock regressie",
                                            h2("Passing-Bablock plot"),
                                            plotOutput("plot2"),
                                            h3("Aantal observaties"),
                                            textOutput("n_obs_PB"),
                                            h3("Beschrijvende statistiek"),
                                            tableOutput("summary_PB"),
                                            h3("Formule"),
                                            textOutput("equation"),
                                            h3("95% betrouwbaarheidsintervallen regressie"),
                                            tableOutput("betrouwbaarheid_PB")
                                   )
                        )
)
)

# Run the application 
shinyApp(ui = ui, server = server)

