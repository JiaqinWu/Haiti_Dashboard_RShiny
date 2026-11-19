# setup -------------------------------------------------------------------
library(checkpoint)
library(AMR)
library(data.table)
library(DT)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggpubr)
library(survminer)
library(tidyverse)
library(viridis)
library(zoo)
library(readxl)


# Input dataset
df <- read_csv("Shiny.csv")
intro <- read_csv2('intro.csv')
steps <- read_csv2('help.csv')


# Define a function for design
fluid_design <- function(id, w, x, y, z) {
    fluidRow(
        div(
            id = id,
            column(
                width = 6,
                uiOutput(w),
                uiOutput(y)
            ),
            column(
                width = 6,
                uiOutput(x),
                uiOutput(z)
            )
        )
    )
}

# User interface ----------------------------------------------------------
ui <- dashboardPage(
    skin = "black",
    title = "HAITI HIV",
    
    # HEADER ------------------------------------------------------------------
    dashboardHeader(
        title = HTML(paste0('<img src="CGHPI.png" height="35" style="margin-right: 15px;" />', " Haiti HIV")), 
        disable = FALSE, 
        titleWidth  = 350,
        dropdownMenu(
            type = "notifications",
            headerText = strong("HELP"),
            icon = icon("question"),
            badgeStatus = NULL,
            notificationItem(
                text = (steps$text[1]),
                icon = icon("spinner")
            ),
            notificationItem(
                text = (steps$text[2]),
                icon = icon("address-card")
            ),
            notificationItem(
                text = (steps$text[3]),
                icon = icon("calendar")
            ),
            notificationItem(
                text = strong(steps$text[7]),
                icon = icon("exclamation")
            )
        ),
        tags$li(
            a(strong("ABOUT Dashboard"),
              height = 40,
              href = "",
              title = "",
              target = "_blank"
            ),
            class = "dropdown"
        )
    ),
    
    # SIDEBAR -----------------------------------------------------------------
    dashboardSidebar(
        width = 300,
        introBox(data.step = 3, data.intro = intro$text[3], #  intro tour
                 div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
                 sidebarMenu(
                     introBox(data.step = 1, data.intro = intro$text[1], # intro tour
                              div(id = "sidebar_button",
                                  bsButton(inputId = "confirm", 
                                           label = "START DASHBORAD", 
                                           icon = icon("play-circle"), 
                                           style = "danger")
                              )
                     ),
                    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
                    menuItem("TREATMENT", tabName = "treatment", icon = icon("spinner"),
                            checkboxGroupInput(
                                inputId = "treatmentInput",
                                label = "Treatment Status",
                                choices = unique(df$outcomeStats),
                                selected = unique(df$outcomeStats),
                                inline = TRUE
                            )
                    ),
                    br(),
                    br(),
                    menuItem("PATIENT", tabName = "patients", icon = icon("address-card"),
                            checkboxGroupInput(
                                inputId = "genderInput",
                                label = "Gender",
                                choices = unique(df$DEM_Sexe),
                                selected = unique(df$DEM_Sexe),
                                inline = TRUE
                            ),
                            sliderInput(
                                inputId = "ageInput",
                                label = "Age",
                                value = c(as.integer(min(df$DEM_age, na.rm = TRUE)), as.integer(max(df$DEM_age, na.rm = TRUE))),
                                min = as.integer(min(df$DEM_age, na.rm = TRUE)),
                                max = as.integer(max(df$DEM_age, na.rm = TRUE)),
                                step = 1,
                                sep = ""
                            )
                    ),
                    br(),
                    br(),
                    menuItem("YEAR", tabName = 'diagnosis', icon = icon("calendar"),
                            sliderInput(
                                inputId = "diagnosisInput",
                                label = "Diagnosis Year",
                                value = c(min(df$Diag_year), max(df$Diag_year)),
                                min = min(df$Diag_year),
                                max = max(df$Diag_year),
                                step = 1L,
                                sep = ""
                            )
                    ),
                    br(),
                    br()
            # Add more menu items as needed
        )
    )),
    
    # BODY --------------------------------------------------------------------
    dashboardBody(
        tags$head(
            tags$link(
                rel = "stylesheet",
                type = "text/css",
                href = "radar_style.css")
        ),
        
        useShinyjs(),
        introjsUI(),
        
        # MAIN BODY ---------------------------------------------------------------
        fluidRow(
            column(
                width = 12,
                introBox(
                    bsButton("patients", 
                             label = "PATIENTS", 
                             icon = icon("user"), 
                             style = "success"),
                    bsButton("dispensations", 
                             label = "DISPENSATIONS", 
                             icon = icon("medkit", class = "medkit-box"), 
                             style = "success"),
                    bsButton("visits", 
                             label = "VISITS", 
                             icon = icon("user-md"), 
                             style = "success"),
                    bsButton("diagnostics", 
                             label = "DIAGNOSTICS", 
                             icon = icon("flask", class = "flask-box"), 
                             style = "success"),
                    data.step = 2, data.intro = intro$text[2])
            )
        ),
        
        fluid_design("patients_panel", "box1", "box2", "box3", "box4"),
        fluid_design("dispensations_panel", "box5", "box6", "box7", "box8"),
        fluid_design("visits_panel", "box9", "box10", "box11", NULL),
        fluid_design("diagnostics_panel", "box13", "box14", "box15", NULL)
    )
)
