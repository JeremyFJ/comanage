library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(shinyTime)
library(shinymanager)
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(tidyr)
library(exifr)
library(DT)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(digest)

source("R_scripts/tabs.R")
source("R_scripts/functions.R")

tunisian_cities <- read.csv("./www/data/port_positions.csv")
gear_list = data.frame(gear = c("Purse Seines", "Trawlers", "Drifting Longlines",
                              "Set Longlines", "Tuna Purse Seines",
                              "Set Gillnets", "Fixed Gear", "Dredge Fishing",
                              "Other"))

Sys.setenv(TZ = "America/New_York")
current_time <- Sys.time()
date_str <- format(current_time, "%Y-%m-%d")
current_date <- as.Date(date_str)

# UI definition
ui <- navbarPage(
  title = tags$div(
    class = "navbar-right",
    tags$img(src = "tunisia-flag-round-shape-png.png"),
    "Tunisia Fisheries Platform"
    ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      $(document).on('click', 'a[href^=\"#\"]', function(event) {
        event.preventDefault();
        var target = this.getAttribute('href').substring(1);
        $('a[data-value=\"' + target + '\"]').tab('show');
      });
    ")) # JavaScript for tab navigation
  ),
  tabPanel(
    title = "Home",
    icon = icon("home"),
    fluidPage(
      uiOutput("user_info"),
      h2("Welcome to the Tunisian App for Fisheries Monitoring!"),
      h2("Bienvenue sur l'application tunisienne de surveillance des pêches!"),
      p("Use the navigation bar to access different sections of the app."),
      br(),
      h3("Mission"),
      p("This application provides catch reporting for Tunisian fishermen, vessel activity maps, and emerging news in ocean transparency initiatives."),
      p("Cette application fournit des rapports de captures pour les pêcheurs tunisiens, des cartes d'activité des navires et des nouvelles émergentes sur les initiatives de transparence des océans."),
      p("يوفر هذا التطبيق تقارير عن الصيد للصيادين التونسيين وخرائط نشاط السفن والأخبار الناشئة في مبادرات الشفافية في المحيطات."),
      tags$ul(
        tags$li(tags$a(href = "#BlueCrabTab", "Invasive Species - How are invasive species impacting the fisheries economy? Learn about the African blue crab and more!")),
        tags$li(tags$a(href = "#CatchAndObservations", "Reporting - log your fishing activity or observations")),
        tags$li(tags$a(href = "#Maps", "Maps - View real-time fishing activity")),
        tags$li(tags$a(href = "#TunisianSpecies", "Tunisian Species - Learn about fish species caught in Tunisia")),
        tags$li(tags$a(href = "#Observers", "Observers - If you are monitoring ports, report what you are seeing!")),
        tags$li(tags$a(href = "#User", "User Login - Register and login on our platform"))
      ),
      br(),
      h4("Recent News"),
      fluidRow(
        column(3,
          tags$div(
            class = "news-item",
            tags$a(
              href = "https://www.theguardian.com/environment/article/2024/may/07/scaling-up-the-app-thats-transforming-lives-in-south-african-fishing-communities",
              tags$img(src = "https://i.guim.co.uk/img/media/f2c1e23b0dba091fd7276bb284344c1f4be7fb73/0_0_1600_2000/master/1600.jpg?width=1140&dpr=2&s=none", class = "top-image"),
              tags$figcaption("Scaling up the app that's transforming lives in South African fishing communities - The Guardian")
            )
          )
        ),
        column(3,
          tags$div(
            class = "news-item",
            tags$a(
              href = "https://english.elpais.com/international/2024-01-14/the-blue-crab-in-tunisia-from-invasive-threat-to-godsend.html",
              tags$img(src = "https://imagenes.elpais.com/resizer/v2/P5QDZXOSCJCUPOYCGQSXBDEJIA.jpg?auth=3adf3e37394c85677e1914b16280a6b0c0aa9437b5d90dd704eb26c53623c27d&width=1200", class = "top-image"),
              tags$figcaption("The Blue Crab in Tunisia: From Invasive Threat to Godsend - El País")
            )
          )
        ),
        column(3,
          tags$div(
            class = "news-item",
            tags$a(
              href = "https://www.sharkproject.org/en/protection/white-shark-chase/#:~:text=Among%20the%20most%20heavily%20over,one%20category%20away%20from%20extinction.",
              tags$img(src = "https://www.sharkproject.org/media/y5blqrzb/herbert_futterknecht_white_shark8.jpg?crop=0.063541666666666663,0,0.31145833333333334,0&cropmode=percentage&width=800&height=800&rnd=132800870252900000", class = "top-image"),
              tags$figcaption("White Shark Chase - An international collaboration to find and protect the last remaining white sharks of the Mediterranean Sea.")
            )
          )
        ),
        column(3,
          tags$div(
            class = "news-item",
            tags$a(
              href = "https://www.fao.org/gfcm/news/detail/en/c/1683407/",
              tags$img(src = "https://gfcmsitestorage.blob.core.windows.net/website/6.News/5-june-24/MOR_20190423_Fnideq_PDA_SSF@FAO_GFCM_Claudia_Amico_DSC01430.jpg", class = "top-image"),
              tags$figcaption("Strengthening collective efforts to eradicate IUU fishing and ensure compliance - FAO")
            )
          )
        )
      ),
      br(),
      h4("Sponsors")
    )
  ),
  # New tab for Blue Crab Map with year range and species toggle
  tabPanel(
    title = "Invasive Species",
    icon = icon("frog"),
    value = "BlueCrabTab",  # Set a value for this tab so we can track it in observeEvent
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Invasive African Blue Crab"),
            # Add an image below the title
            tags$div(
    style = "display: flex; align-items: center;",
            tags$img(src = "species/both_crabs.png", 
           alt = "African blue crab", 
           width = "50%", height = "auto"),
               # Text on the right
              tags$div(
                p("On the left, ", tags$em("Callinectes sapidus"), ", On the right ", tags$em("Portunus segnis"), ""),
                p("© G. Marchessaux")
              )),
          tags$ul(
            tags$div(tags$a(href = "#CatchAndObservations", "Report a Blue Crab!"))
          ),
          p("The invasive African swimming blue crab ", tags$em("(Portunus segnis)"), " entered the Mediterranean via the Suez Canal in 1898. Initially seen as a threat to Tunisian fisheries, especially for small-scale operations, it caused ecological disruption and damaged fishing gear."),
          p("With support from the FAO and the Tunisian government, fishermen were trained to harvest the crab using specialized traps. The species quickly transformed from a nuisance to a valuable commodity, with over 8,100 tonnes exported in 2022, generating 90.5 million dinars (~$33 million), a 200% growth in just four years."),
          p("Although similar to the native Atlantic blue crab ", tags$em("(Callinectes sapidus)"), ", ", tags$em("Portunus segnis"), " plays a different role in the ecosystem. While its economic importance has grown, overfishing has led to concerns, with fishermen now advocating for sustainable practices like closed seasons."),
          p("This case highlights the adaptive responses of Tunisian fisheries, turning environmental challenges into economic opportunities while emphasizing the need for sustainable management.",tags$em("(El País, 2024)"))
        ),
        mainPanel(
          h3("Blue Crab Observations"),
          htmlOutput("blue_crab")  # Map output
        )
      )
    )
  ),
tabPanel(
  title = "Reporting",
  icon = icon("pencil-square"),
  value = "CatchAndObservations",
  
  fluidPage(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    
    tabsetPanel(
      id = "form_tabs",
            # Blue Crab Observation form
      tabPanel(
        title = "Blue Crab Observation",
        value = "BlueCrabForm",
        tags$div(
          class = "centered-form",
          f7Card(
            title = h1("Report a Blue Crab Observation"),
            fluidRow(
              f7List(
                mode = "simple",
                column(4,
                  textInput("observer_name", "Observer's Name"),
                  shiny::dateInput("observation_date", "Observation Date", value = current_date),
                  numericInput("crab_size", "Crab Size (cm)", value = NULL),
                  
                  # Dropdown for species with an 'Other' option
                  selectizeInput("species_crab", "Species", choices = c("Portunus segnis", "Callinectes sapidus", "Other"), options = list(create = TRUE)),
                  textAreaInput("species_other", "If 'Other', specify species:", "", rows = 2, cols = 40)
                ),
                column(4,
                  fileInput("file_crab", "Upload an Image of the Crab!", accept = c("image/jpeg", "image/jpg", "image/png", "image/JPG", "image/JPEG", "image/heic", "image/HEIC", "image/heif", "image/HEIF")),
                  textAreaInput("observation_notes", "Comments or Observations", "", rows = 5, cols = 40)
                ),
                column(4,
                  h3("Location Details"),
                  br(),
                  numericInput("latitude2", "Latitude", value = NULL),
                  numericInput("longitude2", "Longitude", value = NULL),
                  
                  # Leaflet map for blue crab observation
                  leafletOutput("crab_map", height = 300)
                )
              )
            ),
            style = "font-size: 18px;"
          )
        ),
        actionButton("submit_crab", "Submit Observation")
      ),
      # New Catch form
      tabPanel(
        title = "Fish Landing",
        value = "NewCatchForm",
        tags$div(
          class = "centered-form",
          f7Card(
            title = h1("Record A New Catch"),
            fluidRow(
              f7List(
                mode = "simple",
                column(4,
                  textInput("angler", "Your Name"),
                  textInput("boat", "Boat Name"),
                  shiny::dateInput("catch_date", "Date", value = current_date),
                  selectizeInput("port", "Port of Use", choices = NULL, options = list(placeholder = 'Type or Select', create = TRUE)),
                  selectizeInput("gear", "Fishing Gear", choices = NULL, options = list(placeholder = 'Type or Select', create = TRUE)),
                  sliderInput("species_richness", "How many species did you catch?", min = 0, max = 50, value = 0, step = 1),
                  numericInput("weight", "Weight of Total Catch (kg)", value = NULL),
                  sliderInput("fishing_hours", "How many hours were you fishing?", min = 0, max = 24, value = 0, step = 0.5)
                ),
                column(4,
                  radioButtons("ais", "Are you using AIS/VMS?", width = "100%", choices = c("Yes" = "TRUE", "No" = "FALSE"), selected = character(0)),
                  fileInput("file", "Upload an Image of Your Catch!", accept = c("image/jpeg", "image/jpg", "image/png", "image/JPG", "image/JPEG", "image/heic", "image/HEIC", "image/heif", "image/HEIF")),
                  textAreaInput("notes", "Comments", "", rows = 5, cols = 40)
                ),
                column(4,
                  h3("Where were you fishing?"),
                  br(),
                  numericInput("latitude1", "Latitude", value = NULL),
                  numericInput("longitude1", "Longitude", value = NULL),
                  leafletOutput("map", height = 300)
                )
              )
            ),
            style = "font-size: 18px;"
          )
        ),
        actionButton("submit_catch", "Submit Catch")
      )
    )
  )
),


  tabPanel(
    title = "Maps",
    icon = icon("map"),
    value = "Maps", # Value for tab navigation
fluidPage(
  tabsetPanel(
    tabPanel(h5("Fishing Activity"),
             htmlOutput("fishing_activity_map")
    ),
    tabPanel(h5("Fishing Effort"),
             htmlOutput("fishing_effort_grid")
    ),
    tabPanel(h5("Water Temperature"),
          htmlOutput("temperature_map")
    ),
    tabPanel(h5("Chlorophyll A"),
          htmlOutput("chl_map")
    ),
    tabPanel(h5("Wave Height"),
      htmlOutput("wave_map")
    )
  )
  )
),
common_fish,
tabPanel(
      "Observers",
      icon = icon("binoculars"),
      htmlOutput("form_iframe")
    ),
tabPanel("User", 
icon = icon("user"),
uiOutput("auth_profile_ui"))
)
# SERVER ########################################################################################### SERVER #
server <- function(input, output, session) {

  output$form_iframe <- renderUI({
    includeHTML("www/observer_form.html")
  })

  # Reactive value to store user information
  user_data <- reactiveVal(NULL)
  
  # Combined Registration, Login, and Profile UI
  output$auth_profile_ui <- renderUI({
    if (is.null(user_data())) {
      tagList(
        selectInput("auth_type", "Choose Action:", choices = c("Login", "Register")),
        conditionalPanel(
          condition = "input.auth_type == 'Login'",
          textInput("login_email", "Email"),
          passwordInput("login_password", "Password"),
          actionButton("login_btn", "Login")
        ),
        conditionalPanel(
          condition = "input.auth_type == 'Register'",
          textInput("register_email", "Email"),
          radioButtons("stakeholder", "What profession do you identify with?", width = "70%", 
                       choices = c("Fisher", "Manager", "Seafood Vendor", "Scientist", "Other"), selected = character(0)),
          textInput("register_username", "Username"),
          passwordInput("register_password", "Password"),
          actionButton("register_btn", "Register")
        )
      )
    } else {
      tagList(
        h3("User Profile"),
        p(paste("Logged in as:", user_data()$username)),
        p(paste(user_data()$stakeholder)),
        img(src = paste0("icons/", user_data()$icon), height = "50px", style = "margin-top: 20px;"),
        br(),
        br(),
        actionButton("logout_btn", "Logout")
      )
    }
  })
  
  # Registration process
observeEvent(input$register_btn, {
  con <- connectMed_monitoring("med_monitoring")
  on.exit(dbDisconnect(con))  # Ensure the connection is closed
  
  hashed_password <- digest(input$register_password, algo = "sha256")
  icon <- switch(input$stakeholder,
                 "Fisher" = "fisher.png",
                 "Manager" = "manager.png",
                 "Seafood Vendor" = "vendor.png",
                 "Scientist" = "scientist.png",
                 "Other" = "other.png",
                 "other.png"  # Default case (also handles NA values)
  )
  
  if (input$register_email == "" || input$register_username == "" || input$register_password == "") {
    showNotification("Please fill in all fields.", type = "error")
  } else {
    tryCatch({
      dbExecute(con, paste0("INSERT INTO tun_users (email, username, password, stakeholder, icon) VALUES ('",
                            input$register_email, "', '", input$register_username, "', '", hashed_password, "', '", input$stakeholder, "', '", icon, "')"))
      showNotification("Registration successful!", type = "message")
    }, error = function(e) {
      showNotification(paste("Registration failed:", e$message), type = "error")
    })
  }
})

  
  # Login process
  observeEvent(input$login_btn, {
    credentials <- check_credentials(input$login_email, input$login_password)
    
    if (!is.null(credentials)) {
      user_data(credentials)  # Store user information
      showNotification("Login successful!", type = "message")
    } else {
      showNotification("Invalid email or password.", type = "error")
    }
  })
  
  # Logout process
  observeEvent(input$logout_btn, {
    user_data(NULL)
    showNotification("You have logged out.", type = "message")
  })
  
  # Display user information on the Home tab
  output$user_info <- renderUI({
    req(user_data())
  tagList(
    tags$p(
      style = "display: flex; align-items: center;",
      tags$span(paste("Logged in as:", user_data()$username)),
      tags$img(src = paste0("icons/", user_data()$icon), height = "30px", style = "margin-left: 10px;")
      )
    )
  })


  output$blue_crab <- renderUI({
      tags$iframe(src = "maps/blue_crab_map.html", width = "100%", height = "600px")
    })
  output$fishing_activity_map <- renderUI({
      tags$iframe(src = "maps/fishing_activity_map.html", width = "100%", height = "600px")
    })
  
  output$fishing_effort_grid <- renderUI({
    tags$iframe(src = "maps/fishing_effort_grid.html", width = "100%", height = "600px")
  })

  output$temperature_map <- renderUI({
    tags$iframe(src = "maps/tunisian_sst_map_celsius.html", width = "100%", height = "600px")
  })

  output$chl_map <- renderUI({
    tags$iframe(src = "maps/tunisian_chl_map.html", width = "100%", height = "600px")
  })

  output$wave_map <- renderUI({
    tags$iframe(src = "maps/tunisian_wave_map.html", width = "100%", height = "600px")
  })

  # Server-side selectize for species
  # updateSelectizeInput(session, 'species', choices = fetchSpeciesNames(), server = TRUE)
  updateSelectizeInput(session, 'port', choices = c("", as.character(tunisian_cities$port), "Other"), server = TRUE)
  updateSelectizeInput(session, 'gear', choices = c("", as.character(gear_list$gear), "Other"), server = TRUE)

 observeEvent(input$submit, {
      # session$reload()
      # Initialize lat and lon with NA
      ais = if (!is.null(input$ais) && input$ais != "") input$ais else NA
      lat = if (!is.null(input$latitude)) input$latitude else NA
      lon = if (!is.null(input$longitude)) input$longitude else NA
      imageName <- NA
      port = if (!is.null(input$port) && input$port != "") input$port else NA
      gear = if (!is.null(input$gear) && input$gear != "") input$gear else NA
      angler = if (!is.null(input$angler) && input$angler != "") input$angler else NA
      boat = if (!is.null(input$boat) && input$boat != "") input$boat else NA
      notes = if (!is.null(input$notes) && input$notes != "") input$notes else NA
      species_richness = if (input$species_richness != 0) input$species_richness else NA
      fishing_hours = if (input$fishing_hours != 0) input$fishing_hours else NA

      if (!is.null(input$file)) {
        uploadedFile <- input$file
        destPath <- paste0(getwd(), "/www/submissions/", uploadedFile$name)
        file.rename(uploadedFile$datapath, destPath)
        imageName <- as.character(uploadedFile$name)
        
        # Attempt to extract EXIF data
        exifData <- read_exif(destPath)
        
        # Check if GPSLatitude and GPSLongitude exist in the exifData
        if ("GPSLatitude" %in% names(exifData) && "GPSLongitude" %in% names(exifData)) {
          lat <- exifData$GPSLatitude
          lon <- exifData$GPSLongitude
        }
      }

      new_entry <- data.frame(
        date = as.Date(input$catch_date),
        port = port,
        species_richness = species_richness,
        weight = input$weight,
        img_name = imageName,
        gear = gear,
        fishing_hours = fishing_hours,
        latitude = lat,
        longitude = lon,
        angler = angler,
        boat = boat,
        notes = notes,
        ais = ais,
        stringsAsFactors = FALSE
      )
      # write.csv(new_entry, "/srv/shiny-server/comanage/test.csv", row.names = FALSE)

      con <- connectMed_monitoring("med_monitoring")
      tryCatch({
        dbWriteTable(con, "tun_catch", new_entry, append = TRUE, row.names = FALSE)
        dbDisconnect(con)
        shiny::showNotification("Catch logged successfully!", type = "message")
      }, error = function(e) {
        shiny::showNotification(as.character(e), type = "error")
      })
    })

observeEvent(input$submit_crab, {
  con <- dbConnect(RPostgres::Postgres(), dbname = "your_db_name", host = "localhost", user = "your_username", password = "your_password")
  
  # Insert into the blue_crab_observations table
  dbExecute(con, "INSERT INTO blue_crab_observations (observer_name, observation_date, crab_size, species, species_other, latitude, longitude, image_path, notes) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)", 
            params = list(input$observer_name, input$observation_date, input$crab_size, input$species_crab, input$species_other, input$latitude2, input$longitude2, input$file_crab$datapath, input$observation_notes))
  
  dbDisconnect(con)
  showNotification("Blue Crab Observation Submitted!", type = "message")
})

# New Catch map
output$map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    setView(lng = 11, lat = 35.8, zoom = 6.5) %>%  # Centered around the Tunisian coast
    addDrawToolbar(
      targetGroup = 'selected',
      editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
      polylineOptions = FALSE,
      polygonOptions = FALSE,
      rectangleOptions = FALSE,
      circleOptions = FALSE,
      markerOptions = drawMarkerOptions(repeatMode = FALSE),
      circleMarkerOptions = FALSE
    ) %>%
    addLayersControl(
      overlayGroups = c('selected'),
      options = layersControlOptions(collapsed = FALSE)
    )
})

# Event for New Catch map
observeEvent(input$map_draw_new_feature, {
  leafletProxy("map") %>% clearGroup("selected")
  
  feature <- input$map_draw_new_feature
  if (!is.null(feature)) {
    lat <- as.numeric(feature$geometry$coordinates[2])
    lon <- as.numeric(feature$geometry$coordinates[1])
    updateNumericInput(session, "latitude1", value = lat)
    updateNumericInput(session, "longitude1", value = lon)
    
    leafletProxy("map") %>% addMarkers(lng = lon, lat = lat, group = "selected")
  }
})

# Blue Crab Observation map
output$crab_map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    setView(lng = 11, lat = 35.8, zoom = 6.5) %>%  # Centered around the Tunisian coast
    addDrawToolbar(
      targetGroup = 'selected_crab',
      editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
      polylineOptions = FALSE,
      polygonOptions = FALSE,
      rectangleOptions = FALSE,
      circleOptions = FALSE,
      markerOptions = drawMarkerOptions(repeatMode = FALSE),
      circleMarkerOptions = FALSE
    ) %>%
    addLayersControl(
      overlayGroups = c('selected_crab'),
      options = layersControlOptions(collapsed = FALSE)
    )
})

# Event for Blue Crab Observation map
observeEvent(input$crab_map_draw_new_feature, {
  leafletProxy("crab_map") %>% clearGroup("selected_crab")
  
  feature <- input$crab_map_draw_new_feature
  if (!is.null(feature)) {
    lat <- as.numeric(feature$geometry$coordinates[2])
    lon <- as.numeric(feature$geometry$coordinates[1])
    updateNumericInput(session, "latitude2", value = lat)
    updateNumericInput(session, "longitude2", value = lon)
    
    leafletProxy("crab_map") %>% addMarkers(lng = lon, lat = lat, group = "selected_crab")
  }
})


}

shinyApp(ui, server)
