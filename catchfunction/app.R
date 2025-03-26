    # Using local package instead of installing from GitHub
    library(shiny)
    library(bslib)
    library(ggplot2)
    library(DT)
    # Load the locally installed catchfunction package
    library(catchfunction)

    ui <- page_fluid(
        title = "ATTACH Model - Catch Function Analysis",

        navset_tab(
            # Application tab with inputs and outputs
            nav_panel(
                title = "Application",
                page_sidebar(
                    sidebar = sidebar(
                        title = "Input Parameters",

                        # Scenario selector
                        selectInput("scenario", "Scenario:",
                                    choices = c("Status Quo (Scenario 1)" = 1,
                                                "Whitefish Preference (Scenario 2)" = 2,
                                                "Flatfish Preference (Scenario 3)" = 3,
                                                "Remove Species - Method 1 (Scenario 5.1)" = 5.1,
                                                "Remove Species - Method 2 (Scenario 5.2)" = 5.2),
                                    selected = 1),

                        # Fish species catch limits inputs
                        numericInput("pollock", "Pollock Catch Limit (ABC):",
                                     value = 2e6, min = 0, step = 1e5),

                        numericInput("arrowtooth", "Arrowtooth Catch Limit (ABC):",
                                     value = 2e5, min = 0, step = 1e4),

                        numericInput("yellowfin", "Yellowfin Catch Limit (ABC):",
                                     value = 2e5, min = 0, step = 1e4),

                        # Show for scenarios 5.x
                        conditionalPanel(
                            condition = "input.scenario == 5.1 || input.scenario == 5.2",
                            h5("Species to Remove from Cap:"),
                            checkboxInput("arrowtooth_exclude", "Arrowtooth", TRUE),
                            conditionalPanel(
                                condition = "input.arrowtooth_exclude",
                                sliderInput("arrowtooth_mult", "Arrowtooth Catch Multiplier:", 0, 1, 0.5, step = 0.05)
                            ),
                            checkboxInput("yellowfin_exclude", "Yellowfin", TRUE),
                            conditionalPanel(
                                condition = "input.yellowfin_exclude",
                                sliderInput("yellowfin_mult", "Yellowfin Catch Multiplier:", 0, 1, 1.0, step = 0.05)
                            )
                        ),

                        # Additional parameters
                        checkboxInput("showTables", "Show Detailed Tables", TRUE),
                        checkboxInput("showTAC", "Show TAC Results", FALSE),

                        actionButton("runAnalysis", "Run Analysis", class = "btn-primary")
                    ),

                    # Main panel content
                    card(
                        card_header("Results"),
                        card_body(
                            tabsetPanel(
                                tabPanel("Summary",
                                         h4("Catch Function Results"),
                                         verbatimTextOutput("resultSummary"),
                                         conditionalPanel(
                                             condition = "input.showTAC == true",
                                             h4("TAC Results"),
                                             verbatimTextOutput("tacResults")
                                         ),
                                         conditionalPanel(
                                             condition = "input.showTables == true",
                                             h4("Input Parameters"),
                                             DTOutput("paramTable")
                                         )
                                ),
                                tabPanel("Visualization",
                                         selectInput("plotType", "Plot Type:",
                                                     choices = c("Comparison by Species",
                                                                 "ABC vs Catch",
                                                                 "Percentage Distribution")),
                                         plotOutput("resultPlot", height = "500px")
                                ),
                                tabPanel("Raw Output",
                                         verbatimTextOutput("rawOutput"))
                            )
                        )
                    )
                )
            ),

            # Documentation tab
            nav_panel(
                title = "Documentation",
                card(
                    card_body(
                        tabsetPanel(
                            tabPanel("Introduction",
                                     h3("The ATTACH Model"),
                                     p("The catchfunction package (which is also referred to as the ABC To TAC and Commercial Harvest, aka ATTACH, model) was created for the Alaska Climate Integrated Modeling Project (ACLIM) by Amanda Faig (University of Washington) and Alan Haynie (NOAA/NMFS)."),
                                     p("This function, in a nutshell, takes Bering Sea (BS) acceptable biological catch (ABC) as input and uses a series of regression estimates to predict total allowable catch (TAC) and from that the commercial harvest in the Bering Sea, based on ABC, TAC, and catch data from 1992 to 2017."),
                                     h4("Basic Usage Example:"),
                                     code("AP_BS_ABC = 2e6\nATF_BS_ABC = 2e5\nYFS_BS_ABC = 2e5\ncatch_function(scenario = 1, Pollock = AP_BS_ABC, Arrowtooth = ATF_BS_ABC, Yellowfin = YFS_BS_ABC)")
                            ),
                            tabPanel("TAC Prediction",
                                     h3("Predicting Total Allowable Catch (TAC)"),
                                     p("ATTACH is a two-step model. In the first step, TAC is predicted from ABC. The user passes, as inputs, the Bering Sea ABCs into the model for as many species as are defined in their biological models; up to the 22 species under the BSAI 2 million ton ecosystem cap."),
                                     p("Since the ecosystem cap is for the entire BSAI, but the ABC input is only for the Bering Sea, the ATTACH model first calculates BSAI ABCs from the BS ABC inputs."),
                                     p("The entire set of BSAI ABCs is then passed into the first step of the model: estimating TAC. Each year the North Pacific Fishery Management Council sets the TAC of individual stocks based on the ABC estimates for the individual stocks."),
                                     p("TACs for each stock were estimated statistically using a log-linear model. For details on the model specification, see the original documentation.")
                            ),
                            tabPanel("Catch Prediction",
                                     h3("Predicting Catch"),
                                     p("The output from the TAC prediction step is then passed to another sub-model, the catch prediction step. Catch estimates are based on TAC data and catch data from the Catch Accounting System."),
                                     p("Catches for each stock were estimated statistically using a log-linear model. For details on the model specification, see the original documentation."),
                                     p("The catch estimate for a given species can exceed it's own TAC. This is because the TAC measure used is the TAC set at the beginning of the season, and in-season management can adjust TAC to an extent. ATTACH checks that catch does not exceed the BSAI wide ABC and that the ecosystem cap, but otherwise allows predicted catch to exceed TAC.")
                            ),
                            tabPanel("Ensemble",
                                     h3("The Ensemble Model"),
                                     p("ATTACH is an ensemble of three models that include different explanatory variables that fit data better for different species. The ensemble better captures possible environmental and policy uncertainty and is therefore more likely to be robust to ABC combinations and individual ABC levels outside of historical bounds."),
                                     p("The three models in the ensemble differ in the error structures in both the TAC and catch estimation equations. In all of the models, the errors in the TAC estimation stage are linked via Seemingly Unrelated Regressions (SUR), a common econometric modeling technique."),
                                     p("The ensemble averages the estimated catch of the three models equally before returning the estimated Bering Sea catch to the user. Only the estimates for the species whose ABCs were specified by the user are returned.")
                            ),
                            tabPanel("Scenarios",
                                     h3("Available Scenarios"),
                                     h4("Scenario 1: Status Quo"),
                                     p("The base scenario assuming everything about the world stays as it was in 2017 (A80 and AFA alive and well, Steller Sea Lion closure partially reopened, etc.)."),
                                     h4("Scenario 2: Whitefish Preference"),
                                     p("In this scenario, the Council allocates an extra 10% TAC to whitefish (Alaska pollock and Pacific cod), decreasing flatfish TAC by up to 50% to comply with the ecosystem cap."),
                                     h4("Scenario 3: Flatfish Preference"),
                                     p("In this scenario, the Council decreases whitefish TAC by up to 10%, and increases flatfish TAC by that amount."),
                                     h4("Scenarios 5.1 & 5.2: Remove Species from Cap"),
                                     p("These scenarios explore what happens if one or more species are removed from the ecosystem cap."),
                                     p("In scenario 5.1, we assume the ABC still influences the TAC of other species, even if the species is not managed under the cap. This means the tons that would otherwise have been allocated to the removed species are not reallocated to other species."),
                                     p("In scenario 5.2, we assume the cap remains 2 million metric tons, and the council only uses this extra leeway when ABCs are unusually high."),
                                     p("In both scenarios, we assume TAC = ABC for the removed species, and catch is some assumed fraction based on the multiplier provided by the user.")
                            )
                        )
                    )
                )
            ),

            # About tab
            nav_panel(
                title = "About",
                card(
                    card_header("About"),
                    card_body(
                        p("This Shiny app demonstrates the functionality of the catchfunction package."),
                        h4("Citation:"),
                        p("Alan Haynie, Amanda Faig, Kirstin Holsman, Anne Hollowed, Jonathan Reum, Steve Kasperski, and Mary Furuness. \"Predicting future management allocations and catch under the Bering Sea and Aleutian Islands Ecosystem Cap.\" In Prep: contact alan.haynie@noaa.gov."),
                        p("This app is using a locally installed version of the catchfunction package.")
                    )
                )
            )
        )
    )

    server <- function(input, output, session) {
        # Run analysis when button is clicked
        results <- eventReactive(input$runAnalysis, {
            # Call the catch_function with user inputs
            tryCatch({
                catch_function(
                    scenario = as.numeric(input$scenario),
                    Pollock = input$pollock,
                    Arrowtooth = input$arrowtooth,
                    Yellowfin = input$yellowfin,
                    improvedcatchscale=1
                )
            }, error = function(e) {
                # Return error message if function fails
                return(list(error = TRUE, message = e$message))
            })
        })

        # Output summary of results
        output$resultSummary <- renderPrint({
            req(results())

            if (is.list(results()) && "error" %in% names(results())) {
                cat("Error in analysis:\n")
                cat(results()$message)
                return()
            }

            # Print the results (modify as needed based on actual function output)
            print(results())
        })

        # Show raw output
        output$rawOutput <- renderPrint({
            req(results())

            if (is.list(results()) && "error" %in% names(results())) {
                cat("Error in analysis:\n")
                cat(results()$message)
                return()
            }

            # Raw output - depending on what catch_function returns
            str(results())
        })
    # Create visualizations based on results
    output$resultPlot <- renderPlot({
        req(results())

        if (is.list(results()) && "error" %in% names(results())) {
            return(NULL)
        }

        # This visualization needs to be adapted based on what catch_function actually returns
        # Here we're making an assumption about the structure
        result_data <- results()

        # Check if result_data is a data frame or can be converted to one
        if (!is.data.frame(result_data)) {
            # Try to convert to data frame if it's a list
            if (is.list(result_data)) {
                # Attempt to convert list to data frame
                # This is a placeholder and would need to be customized based on actual output
                result_data <- as.data.frame(result_data)
            } else {
                # If we can't plot the data, show a message
                plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(0, 0, "Cannot visualize result data in this format", cex = 1.5)
                return()
            }
        }

        # Create different plot types based on user selection
        if (input$plotType == "Comparison by Species") {
            # This is a placeholder - modify based on actual data structure
            species_data <- data.frame(
                Species = c("Pollock", "Arrowtooth", "Yellowfin"),
                Catch_Limit = c(input$pollock, input$arrowtooth, input$yellowfin)
            )

            ggplot(species_data, aes(x = Species, y = Catch_Limit, fill = Species)) +
                geom_col() +
                scale_y_log10() +
                labs(title = paste("Catch Limits by Species (Scenario", input$scenario, ")"),
                     x = "Species", y = "Catch Limit (log scale)") +
                theme_minimal(base_size = 14)

        } else if (input$plotType == "Scenario Impact") {
            # This is a placeholder - would need multiple runs with different scenarios
            # to actually show the impact of different scenarios
            scenario_data <- data.frame(
                Scenario = factor(1:5),
                Impact = c(0.8, 0.9, 1.0, 1.1, 1.2)  # Placeholder values
            )

            ggplot(scenario_data, aes(x = Scenario, y = Impact, group = 1)) +
                geom_line(size = 1, color = "blue") +
                geom_point(size = 3, color = "red") +
                labs(title = "Impact of Different Scenarios",
                     x = "Scenario", y = "Relative Impact") +
                theme_minimal(base_size = 14)

        } else { # Percentage Distribution
            # Placeholder for percentage distribution
            percentages <- c(input$pollock, input$arrowtooth, input$yellowfin)
            percentages <- percentages / sum(percentages) * 100

            dist_data <- data.frame(
                Species = c("Pollock", "Pacific cod", "Arrowtooth", "Yellowfin"),
                Percentage = percentages
            )

            ggplot(dist_data, aes(x = "", y = Percentage, fill = Species)) +
                geom_bar(stat = "identity", width = 1) +
                coord_polar("y", start = 0) +
                labs(title = "Percentage Distribution of Catch Limits",
                     x = NULL, y = NULL) +
                theme_minimal(base_size = 14) +
                theme(axis.text = element_blank(),
                      axis.ticks = element_blank())
        }
    })
}

# Run the app
shinyApp(ui, server)

