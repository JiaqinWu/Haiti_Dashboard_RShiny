# Haiti HIV Dashboard (R Shiny)

An interactive R Shiny dashboard for descriptive analysis of people living with HIV in Haiti. It compares patients who are **active on treatment (Actif)** with patients who have **interrupted treatment (PIT)** across demographics, ARV dispensations, clinic visits, and viral load testing.

🔗 **Live app:** [https://jw2104.shinyapps.io/Haiti_HIV/](https://jw2104.shinyapps.io/Haiti_HIV/)

> ⚠️ **Data notice:** The dashboard uses HIV patient data from Haiti collected by the Center for Global Health Practice and Impact (CGHPI), Georgetown University, for internal use. The patient-level dataset (`Shiny.csv`) is **not** included in this repository.

---

## Contents

- [Features](#features)
- [Dashboard panels](#dashboard-panels)
- [Repository structure](#repository-structure)
- [Getting started](#getting-started)
- [Data requirements](#data-requirements)
- [Deployment](#deployment)
- [Contact](#contact)

---

## Features

- **No coding needed.** Users explore the data through point-and-click filters.
- **Sidebar filters:**
  - Treatment status (Actif / PIT)
  - Gender
  - Age range
  - Diagnosis year
- **Confirm-to-update.** Changes to the filters apply only after clicking **CONFIRM SELECTION**, which keeps the app responsive.
- **Interactive charts.** All plots are built with `plotly`, so you can hover, zoom, pan, and reset.
- **Side-by-side comparison.** Density plots show the Actif and PIT distributions together, with dashed lines and labels marking each group's mean.
- **Guided onboarding.** An intro modal and a step-by-step tour (`rintrojs`) walk new users through the app. Help tips are in the **?** menu in the header.

## Dashboard panels

| Panel | Visualizations |
|---|---|
| **Patients** | Patients by diagnosis year (table and plot); patients by age band (table and plot); years since diagnosis; same-city facility status |
| **Dispensations** | Total number of dispensations; time since first, previous, and to next dispensation; average days early or late for pick-up; average dispensation delay; % early, on-time, and late pick-ups; average days between dispensations; years of continuous pick-ups |
| **Visits** | Total number of visits; average gap between visits; average gap between appointments |
| **Diagnostics** | Viral load test within the past year, by treatment status; most recent viral load result, by treatment status; total number of viral load tests |

## Repository structure

```
Haiti_Dashboard_RShiny/
├── ui.R                 # User interface: header, sidebar filters, panel layout
├── server.R             # Server logic: data filtering, tables, and plots
├── intro_text.html      # Welcome modal shown when the app loads
├── intro.csv            # Text for the guided introduction tour (rintrojs)
├── help.csv             # Text for the help (?) dropdown menu
├── www/
│   ├── CGHPI.png        # Logo
│   └── radar_style.css  # Custom styles
├── Shiny.csv            # Patient-level dataset (NOT tracked, see Data requirements)
└── README.md
```

## Getting started

### Prerequisites

- R (developed with **R 4.2.1**)
- RStudio (recommended)

### Install packages

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyBS", "shinyjs",
  "shinycssloaders", "rintrojs", "plotly", "DT", "tidyverse",
  "data.table", "lubridate", "readxl", "ggridges", "ggpubr",
  "viridis", "zoo", "survival", "survminer", "qicharts2",
  "AMR", "checkpoint"
))
```

<details>
<summary>Package versions used in the deployed app</summary>

| Package | Version |
|---|---|
| shiny | 1.7.5.1 |
| shinydashboard | 0.7.2 |
| shinyWidgets | 0.8.0 |
| shinyBS | 0.61.1 |
| shinyjs | 2.1.0 |
| shinycssloaders | 1.0.0 |
| rintrojs | 0.3.3 |
| plotly | 4.10.3 |
| ggplot2 | 3.4.1 |
| tidyverse | 2.0.0 |
| dplyr | 1.1.0 |
| readr | 2.1.4 |
| DT | 0.30 |
| lubridate | 1.9.2 |

</details>

### Run locally

1. Clone the repository:
   ```bash
   git clone https://github.com/<your-username>/Haiti_Dashboard_RShiny.git
   cd Haiti_Dashboard_RShiny
   ```
2. Put `Shiny.csv` in the project root (see [Data requirements](#data-requirements)).
3. Launch the app in R:
   ```r
   shiny::runApp()
   ```
4. Click **START DASHBOARD**, then use the sidebar filters and **CONFIRM SELECTION** to explore.

## Data requirements

The app reads one patient-level file, `Shiny.csv`, with one row per patient. These are the columns it uses:

| Column | Description |
|---|---|
| `jointID` | Unique patient identifier |
| `outcomeStats` | Treatment status: `Actif` or `PIT` |
| `DEM_Sexe` | Sex |
| `DEM_age` | Age (years) |
| `DEM_ageBand` | Age group |
| `DEM_sameCityFacility` | Whether the patient lives in the same city as the facility (missing values recoded to `Unknown`) |
| `Diag_year` | Year of HIV diagnosis |
| `DX_yearsSinceDx` | Years since diagnosis |
| `DISP_totalNumDispHist` | Total number of dispensations |
| `DISP_firstDispDistYrs` | Years since first dispensation |
| `DISP_timeSincePrevDispYrs` | Years since previous dispensation |
| `DISP_daysTilNextDisp` | Days until next scheduled dispensation |
| `DISP_numDaysEarlyLate` | Days early (negative) or late (positive) for pick-up |
| `DISP_averageDaysDispenseDelay` | Average days of dispensation delay |
| `DISP_pEarlyDisp` / `DISP_pOnTimeDisp` / `DISP_pLateDisp` | % of pick-ups that were early, on time, or late |
| `DISP_aveTimeBetweenDisp` | Average days between dispensations |
| `DISP_numYearsActiveContinuous` | Years of continuous pick-ups |
| `VISITS_visitCount` | Total number of clinic visits |
| `VISITS_meanVisGap` | Average days between visits |
| `VISITS_meanAppGap` | Average days between appointments |
| `VLS_testWithinYr` | Viral load test in the past year (`TRUE`/`FALSE`) |
| `VLS_recentResult` | Most recent viral load result |
| `VLS_totalVLTests` | Total number of viral load tests |

> 🔒 Keep patient-level data out of version control. Add this line to `.gitignore`:
> ```
> Shiny.csv
> ```

## Deployment

The app is hosted on [shinyapps.io](https://www.shinyapps.io/). To redeploy:

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp(appName = "Haiti_HIV")
```

## Contact

**Jiaqin Wu**, Center for Global Health Practice and Impact (CGHPI), Georgetown University
📧 [jw2104@georgetown.edu](mailto:jw2104@georgetown.edu)

Suggestions and feedback are welcome. Please open an issue or get in touch.
