####################### Start

df1 <- read_csv("Shiny.csv")
# Replace NA values in DEM_sameCityFacility column with 'Unknown'
df1$DEM_sameCityFacility <- replace(df1$DEM_sameCityFacility, is.na(df1$DEM_sameCityFacility), 'Unknown')


server <- function(input, output, session) {
    
    # Filtering the dataset based on the selection from the sidebar
    set_base <- reactive({
        input$confirm # confirm button needs to be pressed to initiate this code
        isolate({
            set_base <- df1 %>%
                filter(
                    outcomeStats %in% input$treatmentInput, 
                    DEM_Sexe %in% input$genderInput,
                    DEM_age %in% c(min(input$ageInput):max(input$ageInput)),
                    Diag_year %in% c(min(input$diagnosisInput):max(input$diagnosisInput))
                )
        })
    })
    
    
    
    #show intro modal
    observeEvent("", {
        showModal(modalDialog(
            includeHTML("intro_text.html"),
            easyClose = TRUE,
            footer = tagList(
                actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
            )
        ))
    })
    
    observeEvent(input$intro,{
        removeModal()
    })
    
    # show intro tour
    observeEvent(input$intro,
                 introjs(session, options = list("nextLabel" = "Continue",
                                                 "prevLabel" = "Previous",
                                                 "doneLabel" = "Alright. Let's go"))
    )
    
    
    # use action buttons as tab selectors
    update_all <- function(x) {
        updateSelectInput(session, "tab",
                          choices = c("", "Patients", "Dispensations", "Visits", "Diagnostics"),
                          label = "",
                          selected = x
        )
    }
    
    observeEvent(input$patients, {
        update_all("Patients")
    })
    observeEvent(input$dispensations, {
        update_all("Dispensations")
    })
    observeEvent(input$visits, {
        update_all("Visits")
    })
    observeEvent(input$diagnostics, {
        update_all("Diagnostics")
    })
    
    # update confirm button
    
    observeEvent(input$confirm, {
        updateButton(
            session, 
            inputId = "confirm", 
            label = "CONFIRM SELECTION", 
            icon = icon("bar-chart"), 
            style = "primary")
    })
    
    # hide the underlying selectInput in sidebar for better design
    observeEvent("", {
        hide("tab")
    })
    
    # DYNAMIC RENDER RULES ----------------------------------------------------
    
    observeEvent("", {
        show("patients_panel")
        hide("dispensations_panel")
        hide("diagnostics_panel")
        hide("visits_panel")
    }, once = TRUE)
    
    observeEvent(input$patients, {
        show("patients_panel")
        hide("diagnostics_panel")
        hide("dispensations_panel")
        hide("visits_panel")
    })
    observeEvent(input$dispensations, {
        show("dispensations_panel")
        hide("diagnostics_panel")
        hide("visits_panel")
        hide("patients_panel")
    })
    observeEvent(input$visits, {
        show("visits_panel")
        hide("diagnostics_panel")
        hide("dispensations_panel")
        hide("patients_panel")
    })
    observeEvent(input$diagnostics, {
        show("diagnostics_panel")
        hide("visits_panel")
        hide("dispensations_panel")
        hide("patients_panel")
    })
    
    # show active button with color
    
    observeEvent(input$tab, {
        x <- input$tab
        updateButton(session, "patients", style = {
            if (x == "Patients") {
                paste("warning")
            } else {
                paste("success")
            }
        })
        updateButton(session, "dispensations", style = {
            if (x == "Dispensations") {
                paste("warning")
            } else {
                paste("success")
            }
        })
        updateButton(session, "visits", style = {
            if (x == "Visits") {
                paste("warning")
            } else {
                paste("success")
            }
        })
        updateButton(session, "diagnostics", style = {
            if (x == "Diagnostics") {
                paste("warning")
            } else {
                paste("success")
            }
        })
    })
    
    # UI - PATIENTS - 1  --------------
    output$box1 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box1",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Number by Diagnosed Year - table",
                    htmlOutput("patients_total"),
                    withSpinner(
                        DT::dataTableOutput("table1"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "Number by Diagnosed Year - plot",
                    withSpinner(
                        plotlyOutput("plot1", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$patients_total <- renderText({
        HTML(
            paste("Total number of patients:", 
                  strong(
                      unique(
                          paste(set_base()$jointID) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    # UI - PATIENTS - 2 -------------------------------------------------------
    
    output$box2 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box2",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Number by Age Band - table",
                    htmlOutput("patients_total1"),
                    withSpinner(
                        DT::dataTableOutput("table2"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "Number by Age Band - plot",
                    withSpinner(
                        plotlyOutput("plot2", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$patients_total1 <- renderText({
        HTML(
            paste("Total number of patients:", 
                  strong(
                      unique(
                          paste(set_base()$jointID) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    # UI - PATIENTS - 3 -------------------------------------------------------
    
    output$box3 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box3",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Years since Diagnosis",
                    withSpinner(
                        plotlyOutput("plot3", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    # UI - PATIENTS - 4 -------------------------------------------------------
    
    output$box4 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box4",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Same Facility Situation",
                    withSpinner(
                        plotlyOutput("plot4", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    # UI - DISPENSATIONS - 1  --------------
    output$box5 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box5",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Historical Dispensation Times",
                    withSpinner(
                        plotlyOutput("plot5", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "Duration since First Dispensation",
                    withSpinner(
                        plotlyOutput("plot6", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "Duration since Previous Dispensation",
                    withSpinner(
                        plotlyOutput("plot7", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "Duration to Next Dispensation",
                    withSpinner(
                        plotlyOutput("plot8", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    # UI - DISPENSATIONS - 2 -------------------------------------------------------
    
    output$box6 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box6",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Average Early/Late Pick-up",
                    withSpinner(
                        plotlyOutput("plot9", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "Average Delay",
                    withSpinner(
                        plotlyOutput("plot10", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    # UI - DISPENSATIONS - 3  --------------
    output$box7 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box7",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "% of Early Pick-up",
                    withSpinner(
                        plotlyOutput("plot11", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "% of On-time Pick-up",
                    withSpinner(
                        plotlyOutput("plot12", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "% of Late Pick-up",
                    withSpinner(
                        plotlyOutput("plot13", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    # UI - DISPENSATIONS - 4 -------------------------------------------------------
    
    output$box8 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box8",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Dispensation Gap",
                    withSpinner(
                        plotlyOutput("plot14", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                ),
                tabPanel(
                    title = "Years for Continuous Pick-ups ",
                    withSpinner(
                        plotlyOutput("plot15", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    # UI - VISITS - 1  --------------
    output$box9 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box9",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Historical Visit Times",
                    withSpinner(
                        plotlyOutput("plot16", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    # UI - VISITS - 2 -------------------------------------------------------
    
    output$box10 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box10",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Average Visit Gap",
                    withSpinner(
                        plotlyOutput("plot17", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    # UI - VISITS - 3  --------------
    output$box11 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box11",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Average Appointment Gap",
                    withSpinner(
                        plotlyOutput("plot18", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    
    
    # UI - DIAGNOSTICS - 1  --------------
    output$box13 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box13",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Distribution of Viral Test Situation",
                    withSpinner(
                        plotlyOutput("plot19", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    # UI - DIAGNOSTICS - 2 -------------------------------------------------------
    
    output$box14 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box14",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Distribution of Viral Test Result",
                    withSpinner(
                        plotlyOutput("plot20", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    # UI - DIAGNOSTICS - 3  --------------
    output$box15 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box15",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Historical Viral Test Times",
                    withSpinner(
                        plotlyOutput("plot21", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    
    
    
    
    # BOX PATIENTS - 1 ------------------------------------------------------------
    table1 <- reactive({
        set_base() %>% 
            count(Diag_year) %>% 
            arrange(desc(n)) %>% 
            rename("Diagnosed Year" = Diag_year)
    })
    
    output$table1 <- DT::renderDataTable({
        datatable(table1(), rownames = FALSE, options = list(
            dom = 'frtp',
            style = "bootstrap",
            lengthMenu = c(seq(5, 150, 5))
        ))
    })
    
    
    plot1 <- reactive({
        diag_select <- set_base() %>%
            group_by(Diag_year) %>%
            summarise(n = n()) %>% 
            arrange(desc(n))
        
        plot_ly(diag_select, x = ~Diag_year, y = ~n, type = "bar", 
                marker = list(color = "#9FC131", opacity = 0.9)) %>%
            layout(
                xaxis = list(title = "Diagnosis Year", 
                             tickfont = list(family = "Times New Roman", size = 12)),
                yaxis = list(title = "Number", 
                             tickfont = list(family = "Times New Roman", size = 12)),
                title = "Number of New Diagnosed Patients by Year",
                font = list(family = "Times New Roman", size = 14),
                margin = list(l = 50, r = 50, t = 50, b = 50)
            )
    })
    
    output$plot1 <- renderPlotly({
        plot1()
    })
    
    
    
    # BOX PATIENTS - 2 ----------------------------------------------------------
    
    table2 <- reactive({
        set_base() %>% 
            count(DEM_ageBand) %>% 
            arrange(desc(n)) %>% 
            rename("Age Band" = DEM_ageBand)
    })
    
    output$table2 <- DT::renderDataTable({
        datatable(table2(), rownames = FALSE, options = list(
            dom = 'frtp',
            style = "bootstrap",
            lengthMenu = c(seq(5, 150, 5))
        ))
    })
    
    
    plot2 <- reactive({
        diag_select <- set_base() %>%
            group_by(DEM_ageBand) %>%
            summarise(n = n()) %>% 
            arrange(desc(n))
        
        plot <- plot_ly(diag_select, x = ~DEM_ageBand, y = ~n, type = "bar", 
                        marker = list(color = "#9FC131", opacity = 0.9)) %>%
            layout(
                xaxis = list(title = "Diagnosis Year", 
                             tickfont = list(family = "Times New Roman", size = 12)),
                yaxis = list(title = "Number", 
                             tickfont = list(family = "Times New Roman", size = 12)),
                title = "Number of Patients by Age Band",
                font = list(family = "Times New Roman", size = 14),
                margin = list(l = 50, r = 50, t = 50, b = 50)
            )
    })
    
    output$plot2 <- renderPlotly({
        plot2()
    })
    
    # BOX PATIENTS - 3 ----------------------------------------------------------
    # Calculate mean for Actif and PIT groups
    mean_actif <- reactive({
        mean(set_base()$DX_yearsSinceDx[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit <- reactive({
        mean(set_base()$DX_yearsSinceDx[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot3 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DX_yearsSinceDx, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Year(s)", y = "Density", title = "Density Plot of Years since Diagnosis") +  # Set axis labels and title
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif(), y = 0.5, 
                     label = paste("Actif mean:", round(mean_actif(), 2), "years"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit(), y = 0.7, 
                     label = paste("PIT mean:", round(mean_pit(), 2), "years"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot3 <- renderPlotly({
        plot3()
    })
    
    
    # BOX PATIENTS - 4 ------------------------------------------------------------
    
    plot4 <- reactive({
        diag_select <- set_base() %>%
            group_by(DEM_sameCityFacility) %>%
            summarise(n = n()) %>% 
            arrange(desc(n))
        
        plot_ly(diag_select, x = ~DEM_sameCityFacility, y = ~n, type = "bar", 
                marker = list(color = "#9FC131", opacity = 0.9)) %>%
            layout(
                xaxis = list(title = "Facility Situation", 
                             tickfont = list(family = "Times New Roman", size = 12)),
                yaxis = list(title = "Number", 
                             tickfont = list(family = "Times New Roman", size = 12)),
                title = "Distribution of Patients' Same City Facility Situation",
                font = list(family = "Times New Roman", size = 14),
                margin = list(l = 50, r = 50, t = 50, b = 50)
            )
    })
    
    output$plot4 <- renderPlotly({
        plot4()
    })
    
    
    # BOX Dispensation - 1 ----------------------------------------------------------
    # Calculate mean for Actif and PIT groups
    mean_actif1 <- reactive({
        mean(set_base()$DISP_totalNumDispHist[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit1 <- reactive({
        mean(set_base()$DISP_totalNumDispHist[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    
    plot5 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_totalNumDispHist, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Time(s)", y = "Density", title = "Density Plot of Total Number of Dispensation History") +  # Set axis labels and title
            theme_minimal() +  # Apply a minimal theme
            xlim(0,100) + ylim(0,0.2)+ 
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif1()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit1()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif1(), y = 0.06, 
                     label = paste("Actif mean:", round(mean_actif1(), 2), "times"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit1(), y = 0.12, 
                     label = paste("PIT mean:", round(mean_pit1(), 2), "times"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot5 <- renderPlotly({
        plot5()
    })
    
    # Calculate mean for Actif and PIT groups
    mean_actif2 <- reactive({
        mean(set_base()$DISP_firstDispDistYrs[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit2 <- reactive({
        mean(set_base()$DISP_firstDispDistYrs[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot6 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_firstDispDistYrs, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Year(s)", y = "Density", title = "Density Plot of Duration Since first Dispensation") +  # Set axis labels and title
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif2()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit2()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif2(), y = 0.3, 
                     label = paste("Actif mean:", round(mean_actif2(), 2), "years"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit2(), y = 0.45, 
                     label = paste("PIT mean:", round(mean_pit2(), 2), "years"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot6 <- renderPlotly({
        plot6()
    })
    
    # Calculate mean for Actif and PIT groups
    mean_actif3 <- reactive({
        mean(set_base()$DISP_timeSincePrevDispYrs[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit3 <- reactive({
        mean(set_base()$DISP_timeSincePrevDispYrs[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    
    plot7 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_timeSincePrevDispYrs, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Year(s)", y = "Density", title = "Density Plot of Duration Since Previous Dispensation") +  # Set axis labels and title
            theme_minimal() +  # Apply a minimal theme
            xlim(0,10) +
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif3()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit3()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif3()+1.2, y = 1, 
                     label = paste("Actif mean:", round(mean_actif3(), 2), "years"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit3()+1.2, y = 2, 
                     label = paste("PIT mean:", round(mean_pit3(), 2), "years"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot7 <- renderPlotly({
        plot7()
    })
    
    # Calculate mean for Actif and PIT groups
    mean_actif4 <- reactive({
        mean(set_base()$DISP_daysTilNextDisp[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit4 <- reactive({
        mean(set_base()$DISP_daysTilNextDisp[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot8 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_daysTilNextDisp, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Day(s)", y = "Density", title = "Density Plot of Duration to Next Dispensation") +  # Set axis labels and title
            xlim(0,2000) + ylim(0,0.04)+ 
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif4()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit4()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif4()+300, y = 0.012, 
                     label = paste("Actif mean:", round(mean_actif4(), 2), "days"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit4()+300, y = 0.02, 
                     label = paste("PIT mean:", round(mean_pit4(), 2), "days"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot8 <- renderPlotly({
        plot8()
    })
    
    # BOX Dispensation - 2 ----------------------------------------------------------
    # Calculate mean for Actif and PIT groups
    mean_actif5 <- reactive({
        mean(set_base()$DISP_numDaysEarlyLate[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit5 <- reactive({
        mean(set_base()$DISP_numDaysEarlyLate[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot9 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_numDaysEarlyLate, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Day(s)", y = "Density", title = "Density Plot of Number of Days Early or Late to Pick up Drugs") + 
            ylim(0, 0.05) + xlim(-700,1500)+
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif5()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit5()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif5()+50, y = 0.02, 
                     label = paste("Actif mean:", round(mean_actif5(), 2), "days"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit5()+50, y = 0.035, 
                     label = paste("PIT mean:", round(mean_pit5(), 2), "days"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot9 <- renderPlotly({
        plot9()
    })
    
    # Calculate mean for Actif and PIT groups
    mean_actif6 <- reactive({
        mean(set_base()$DISP_averageDaysDispenseDelay[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit6 <- reactive({
        mean(set_base()$DISP_averageDaysDispenseDelay[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot10 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_averageDaysDispenseDelay, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Day(s)", y = "Density", title = "Density Plot of Average Days of Dispensation Delay") + 
            ylim(0, 0.1) +
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif6()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit6()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif6(), y = 0.04, 
                     label = paste("Actif mean:", round(mean_actif6(), 2), "days"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit6(), y = 0.07, 
                     label = paste("PIT mean:", round(mean_pit6(), 2), "days"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot10 <- renderPlotly({
        plot10()
    })
    
    
    # BOX Dispensation - 3 ----------------------------------------------------------
    # Calculate mean for Actif and PIT groups
    mean_actif7 <- reactive({
        mean(set_base()$DISP_pEarlyDisp[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit7 <- reactive({
        mean(set_base()$DISP_pEarlyDisp[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot11 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_pEarlyDisp, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "%", y = "Density", title = "Density Plot of Times Patients are Early for Pick-ups") + 
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif7()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit7()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif7(), y = 0.15, 
                     label = paste("Actif mean:", round(mean_actif7(), 2), "%"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit7(), y = 0.25, 
                     label = paste("PIT mean:", round(mean_pit7(), 2), "%"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot11 <- renderPlotly({
        plot11()
    })
    
    # Calculate mean for Actif and PIT groups
    mean_actif8 <- reactive({
        mean(set_base()$DISP_pOnTimeDisp[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit8 <- reactive({
        mean(set_base()$DISP_pOnTimeDisp[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot12 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_pOnTimeDisp, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "%", y = "Density", title = "Density Plot of Times Patients are On-time for Pick-ups") + 
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif8()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit8()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif8(), y = 0.04, 
                     label = paste("Actif mean:", round(mean_actif8(), 2), "%"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit8(), y = 0.07, 
                     label = paste("PIT mean:", round(mean_pit8(), 2), "%"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot12 <- renderPlotly({
        plot12()
    })
    
    # Calculate mean for Actif and PIT groups
    mean_actif9 <- reactive({
        mean(set_base()$DISP_pLateDisp[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit9 <- reactive({
        mean(set_base()$DISP_pLateDisp[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot13 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_pLateDisp, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "%", y = "Density", title = "Density Plot of Times Patients are Late for Pick-ups") +
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif9()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit9()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif9(), y = 0.04, 
                     label = paste("Actif mean:", round(mean_actif9(), 2), "%"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit9(), y = 0.07, 
                     label = paste("PIT mean:", round(mean_pit9(), 2), "%"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot13 <- renderPlotly({
        plot13()
    })
    
    
    # BOX Dispensation - 4 ----------------------------------------------------------
    # Calculate mean for Actif and PIT groups
    mean_actif10 <- reactive({
        mean(set_base()$DISP_aveTimeBetweenDisp[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit10 <- reactive({
        mean(set_base()$DISP_aveTimeBetweenDisp[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot14 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_aveTimeBetweenDisp, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Day(s)", y = "Density", title = "Density Plot of Average Days between Dispensations") + 
            ylim(0, 0.02) + xlim(-700,1500)+
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif10()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit10()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif10(), y = 0.006, 
                     label = paste("Actif mean:", round(mean_actif10(), 2), "days"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit10(), y = 0.012, 
                     label = paste("PIT mean:", round(mean_pit10(), 2), "days"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot14 <- renderPlotly({
        plot14()
    })
    
    # Calculate mean for Actif and PIT groups
    mean_actif11 <- reactive({
        mean(set_base()$DISP_numYearsActiveContinuous[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit11 <- reactive({
        mean(set_base()$DISP_numYearsActiveContinuous[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot15 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = DISP_numYearsActiveContinuous, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Year(s)", y = "Density", title = "Density Plot of Years for Continuous Pick-ups") + 
            xlim(0,16) +
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif11()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit11()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif11(), y = 0.5, 
                     label = paste("Actif mean:", round(mean_actif11(), 2), "years"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit11()+2, y = 0.7, 
                     label = paste("PIT mean:", round(mean_pit11(), 2), "years"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot15 <- renderPlotly({
        plot15()
    })
    
    
    
    # BOX Visit - 1 ----------------------------------------------------------
    # Calculate mean for Actif and PIT groups
    mean_actif12 <- reactive({
        mean(set_base()$VISITS_visitCount[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit12 <- reactive({
        mean(set_base()$VISITS_visitCount[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot16 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = VISITS_visitCount, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Time(s)", y = "Density", title = "Density Plot of Total Number of Visit History") + 
            ylim(0,0.1)+
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif12()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit12()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif12(), y = 0.03, 
                     label = paste("Actif mean:", round(mean_actif12(), 2), "times"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit12()+20, y = 0.07, 
                     label = paste("PIT mean:", round(mean_pit12(), 2), "times"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot16 <- renderPlotly({
        plot16()
    })
    
    
    # BOX Visit - 2 ----------------------------------------------------------
    # Calculate mean for Actif and PIT groups
    mean_actif13 <- reactive({
        mean(set_base()$VISITS_meanVisGap[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit13 <- reactive({
        mean(set_base()$VISITS_meanVisGap[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot17 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = VISITS_meanVisGap, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Day(s)", y = "Density", title = "Density Plot of Average Visit Gap") + 
            ylim(0,0.02) + xlim(0,800)+
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif13()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit13()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif13()+100, y = 0.012, 
                     label = paste("Actif mean:", round(mean_actif13(), 2), "days"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit13()+100, y = 0.008, 
                     label = paste("PIT mean:", round(mean_pit13(), 2), "days"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot17 <- renderPlotly({
        plot17()
    })
    
    
    # BOX Visit - 3 ----------------------------------------------------------
    # Calculate mean for Actif and PIT groups
    mean_actif14 <- reactive({
        mean(set_base()$VISITS_meanAppGap[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit14 <- reactive({
        mean(set_base()$VISITS_meanAppGap[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot18 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = VISITS_meanAppGap, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Day(s)", y = "Density", title = "Density Plot of Average Appointment Gap") + 
            ylim(0,0.015) + xlim(-400,800)+
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif14()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit14()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif14(), y = 0.004, 
                     label = paste("Actif mean:", round(mean_actif14(), 2), "days"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit14()+2, y = 0.009, 
                     label = paste("PIT mean:", round(mean_pit14(), 2), "days"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot18 <- renderPlotly({
        plot18()
    })
    
    
    # BOX Diagnostic - 1 ----------------------------------------------------------
    # Generate a new dataframe containing the proportion of Actif or PIT patients' recent test status
    df1$VLS_testWithinYr <- ifelse(df1$VLS_testWithinYr == TRUE, "True", "False")
    plot19 <- reactive({
        df_rr <- set_base() %>%
            group_by(VLS_testWithinYr, outcomeStats) %>%
            summarise(count = n()) %>%
            ungroup() %>%
            mutate(group_sum = ave(count, outcomeStats, FUN = sum),
                   proportion = round(count / group_sum * 100, 2)) %>%
            rename(`Test within 1 Year` = VLS_testWithinYr)
        # Create a ggplot object
        p <- ggplot(df_rr, aes(x = outcomeStats, y = proportion, fill = `Test within 1 Year`)) +
            geom_bar(stat = "identity") +
            labs(x = "Outcome Status", y = "Proportion (%)",
                 title = "Proportion of Patients Took Test within 1 Year by Treatment Status") +
            scale_fill_manual(values = c("True" = "lightcoral", "False" = "darkturquoise")) +
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot19 <- renderPlotly({
        plot19()
    })
    
    
    # BOX Diagnostic - 2 ----------------------------------------------------------
    # Generate a new dataframe containing the proportion of Actif or PIT patients' recent test status
    plot20 <- reactive({
        df_rro <- set_base() %>%
            group_by(VLS_recentResult, outcomeStats) %>%
            summarise(count = n()) %>%
            ungroup() %>%
            mutate(group_sum = ave(count, outcomeStats, FUN = sum),
                   proportion = round(count / group_sum * 100, 2)) %>%
            rename(`Recent HIV Test Result` = VLS_recentResult)
        # Create a ggplot object
        p <- ggplot(df_rro, aes(x = outcomeStats, y = proportion, fill = `Recent HIV Test Result`)) +
            geom_bar(stat = "identity") +
            labs(x = "Outcome Status", y = "Proportion (%)",
                 title = "Proportion of Patients by Recent HIV Test Result and Treatment Status") +
            theme_minimal() +
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot20 <- renderPlotly({
        plot20()
    })
    
    
    # BOX Diagnostic - 3 ----------------------------------------------------------
    # Calculate mean for Actif and PIT groups
    mean_actif15 <- reactive({
        mean(set_base()$VLS_totalVLTests[set_base()$outcomeStats == 'Actif'], na.rm = TRUE)
    })
    
    mean_pit15 <- reactive({
        mean(set_base()$VLS_totalVLTests[set_base()$outcomeStats == 'PIT'], na.rm = TRUE)
    })
    
    plot21 <- reactive({
        # Create a ggplot object
        p <- ggplot(set_base(), aes(x = VLS_totalVLTests, fill = outcomeStats)) +
            geom_density(alpha = 0.5) +  # Add density plot
            labs(x = "Time(s)", y = "Density", title = "Density Plot of Total Number of Viral Test History") + 
            theme_minimal() +  # Apply a minimal theme
            theme(axis.text = element_text(family = "Times New Roman", size = 6),
                  axis.title = element_text(family = "Times New Roman", size = 10),
                  plot.title = element_text(family = "Times New Roman", size = 12),
                  legend.text = element_text(family = "Times New Roman", size = 8))
        
        # Add mean lines and annotations
        p <- p +
            geom_vline(aes(xintercept = mean_actif15()), color = "lightcoral", linetype = "dashed") +
            geom_vline(aes(xintercept = mean_pit15()), color = "darkturquoise", linetype = "dashed") +
            annotate("text", x = mean_actif15(), y = 0.42, 
                     label = paste("Actif mean:", round(mean_actif15(), 2), "times"), 
                     color = "lightcoral", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0) +
            annotate("text", x = mean_pit15()+2, y = 0.6, 
                     label = paste("PIT mean:", round(mean_pit15(), 2), "times"), 
                     color = "darkturquoise", 
                     family = "Times New Roman", 
                     size = 2,  # Adjust the size of the text here
                     hjust = 0)
        
        
        # Convert ggplot object to plotly
        ggplotly(p)
    })
    
    output$plot21 <- renderPlotly({
        plot21()
    })
    
    
    
    
}