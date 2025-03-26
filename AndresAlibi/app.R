library(shiny)
library(bslib)
library(ggplot2)
library(DT)

# UI definition
ui <- page_navbar(
    title = "Optimum Yield Cap Analysis",
    theme = bs_theme(bootswatch = "flatly"),

    # Application tab
    nav_panel(
        title = "Application",
        page_sidebar(
            sidebar = sidebar(
                title = "Parameters",
                numericInput("oycap", "OY Cap:", 200000, min = 1, max = 1000000),
                hr(),
                h5("Species ABCs:"),
                numericInput("abc1", "Species 1 ABC:", 100000, min = 0),
                numericInput("abc2", "Species 2 ABC:", 80000, min = 0),
                numericInput("abc3", "Species 3 ABC:", 60000, min = 0),
                numericInput("abc4", "Species 4 ABC:", 40000, min = 0),
                numericInput("abc5", "Species 5 ABC:", 40000, min = 0),
                hr(),
                h5("Weighting Factors:"),
                numericInput("w1", "Species 1 Weight:", 4, min = 0),
                numericInput("w2", "Species 2 Weight:", 5, min = 0),
                numericInput("w3", "Species 3 Weight:", 1, min = 0),
                numericInput("w4", "Species 4 Weight:", 0.5, min = 0),
                numericInput("w5", "Species 5 Weight:", 2, min = 0),
                hr(),
                h5("Bycatch Ratios:"),
                numericInput("by13", "Bycatch Species 1 → 3:", 0.3, min = 0, max = 1),
                numericInput("by24", "Bycatch Species 2 → 4:", 0.3, min = 0, max = 1),
                hr(),
                actionButton("run", "Run Analysis", class = "btn-primary")
            ),
            layout_columns(
                card(
                    card_header("Optimization Results"),
                    tableOutput("resultTable")
                ),
                card(
                    card_header("Visualization"),
                    plotOutput("barPlot", height = "400px")
                )
            )
        )
    ),

    # Documentation tab
    nav_panel(
        title = "Documentation",
        card(
            card_header("App Documentation"),
            p("This application implements an Optimum Yield (OY) Cap analysis with species-specific options."),
            h4("Features:"),
            tags$ul(
                tags$li("Set OY Cap and species-specific parameters"),
                tags$li("Define species ABCs (Allowable Biological Catches)"),
                tags$li("Adjust weighting factors for each species"),
                tags$li("Configure bycatch ratios between species"),
                tags$li("View optimization results in table form"),
                tags$li("Visualize results with comparative bar charts")
            ),
            h4("How the Algorithm Works:"),
            p("The algorithm iteratively adjusts species allocations to fit within the OY Cap while accounting for bycatch relationships. It uses the following steps:"),
            tags$ol(
                tags$li("Initial allocation based on species weights and the OY cap"),
                tags$li("Calculate bycatch for each species based on the current allocation"),
                tags$li("Identify species where bycatch exceeds allocation"),
                tags$li("For these species, fix their allocation to the bycatch amount"),
                tags$li("Recalculate allocations for the remaining species"),
                tags$li("Repeat until a stable solution is found")
            ),
            h4("Inputs:"),
            tags$ul(
                tags$li("OY Cap: The overall catch limit across all species"),
                tags$li("Species ABCs: The Allowable Biological Catch for each species"),
                tags$li("Weighting Factors: Relative importance of each species"),
                tags$li("Bycatch Ratios: The proportion of one species caught when targeting another")
            ),
            h4("Outputs:"),
            tags$ul(
                tags$li("Adjusted ABC: The optimized allocation for each species"),
                tags$li("Bycatch: The calculated bycatch for each species"),
                tags$li("Comparison: Original ABC vs. Adjusted ABC")
            )
        )
    )
)

# Server logic
server <- function(input, output, session) {

    # Create reactive values to store results
    results <- reactiveVal(NULL)

    # Function to run the OY Cap optimization
    observeEvent(input$run, {
        # Get inputs
        OYcap <- input$oycap
        ABCs <- c(input$abc1, input$abc2, input$abc3, input$abc4, input$abc5)
        w <- c(input$w1, input$w2, input$w3, input$w4, input$w5)

        # Initialize bycatch ratio matrix
        BycatchRatio <- matrix(0, nrow = 5, ncol = 5)
        BycatchRatio[1, 3] <- input$by13
        BycatchRatio[2, 4] <- input$by24

        # Initialize variables
        Nspec <- length(ABCs)
        OYcapUse <- OYcap
        Bycatch <- rep(0, Nspec)
        Include <- rep(0, Nspec)
        AdjustedABC <- rep(0, Nspec)

        # Define optimization function
        ABCPred <- function(mult) {
            r <- (OYcapUse/sum(ABCs))^(1/(w*mult))
            return(ABCs * r)
        }

        fn <- function(mult) {
            ABC1 <- ABCPred(mult)
            AdjustedABC <<- rep(0, Nspec)
            for (Jspec in 1:Nspec)
                if (Include[Jspec] == 0) AdjustedABC[Jspec] <<- ABC1[Jspec]
            for (Jspec in 1:Nspec)
                if (Include[Jspec] == 1) AdjustedABC[Jspec] <<- Bycatch[Jspec]
            UseABC <- 0
            for (Ispec in 1:Nspec)
                if (Include[Ispec] == 0) UseABC <- UseABC + ABC1[Ispec]
            obj <- (OYcapUse - UseABC)^2
            return(obj)
        }

        # Run optimization algorithm
        OK <- FALSE
        Iloop <- 0

        while (OK == FALSE && Iloop < 10) {  # Added iteration limit for safety
            Iloop <- Iloop + 1
            r <- OYcapUse/sum(ABCs)
            xx <- optimize(f = fn, interval = c(-1, 10))
            mult <- xx$minimum
            r <- (OYcapUse/sum(ABCs))^(1/(w*mult))

            Bycatch <- rep(0, Nspec)
            for (Jspec in 1:Nspec)
                for (Ispec in 1:Nspec)
                    Bycatch[Jspec] <- Bycatch[Jspec] + BycatchRatio[Ispec, Jspec] * AdjustedABC[Ispec]

            OK <- TRUE
            OYcapUse <- OYcap
            for (Jspec in 1:Nspec)
                if (Bycatch[Jspec] > AdjustedABC[Jspec]) {
                    Include[Jspec] <- 1
                    OYcapUse <- OYcapUse - Bycatch[Jspec]
                    OK <- FALSE
                }
        }

        # Store results
        results(list(
            ABCs = ABCs,
            AdjustedABC = AdjustedABC,
            Bycatch = Bycatch,
            MultiplierValue = mult,
            RatioValue = r,
            Total = sum(AdjustedABC)
        ))
    })

    # Output table
    output$resultTable <- renderTable({
        req(results())
        res <- results()
        data.frame(
            Species = paste("Species", 1:5),
            Original_ABC = res$ABCs,
            Adjusted_ABC = round(res$AdjustedABC, 2),
            Bycatch = round(res$Bycatch, 2),
            Percent_of_Original = round(100 * res$AdjustedABC / res$ABCs, 1)
        )
    })

    # Output plot
    output$barPlot <- renderPlot({
        req(results())
        res <- results()

        # Create data frame for plotting
        plot_data <- data.frame(
            Species = rep(paste("Species", 1:5), 2),
            Value = c(res$ABCs, res$AdjustedABC),
            Type = rep(c("Original ABC", "Adjusted ABC"), each = 5)
        )

        ggplot(plot_data, aes(x = Species, y = Value, fill = Type)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_text(aes(label = round(Value, 0)),
                      position = position_dodge(width = 0.9),
                      vjust = -0.5,
                      size = 3.5) +
            labs(y = "Value", title = "Comparison of Original vs. Adjusted ABC") +
            theme_minimal() +
            scale_fill_brewer(palette = "Set1")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
