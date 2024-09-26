# R/functions.R

# Function to connect to the med_monitoring database
connectMed_monitoring <- function(dbname) {
  dbConnect(PostgreSQL(), user = 'spr', password = 'spr_pass', dbname = dbname, host = 'localhost', port = 5432)
}

# Fetch distinct species names from taxonomy3 table
fetchSpeciesNames <- function() {
  con <- connectMed_monitoring("med_monitoring")
  if (!is.null(con)) {
    query <- "SELECT DISTINCT main_common_name FROM taxonomy3 ORDER BY main_common_name"
    species_data <- dbGetQuery(con, query)
    species_names <- c("", species_data$main_common_name)
    dbDisconnect(con)
    return(species_names)
  }
  return(character(0))
}

# Check credentials function
  check_credentials <- function(email, password) {
    con <- connectMed_monitoring("med_monitoring")
    res <- dbGetQuery(con, paste0("SELECT * FROM tun_users WHERE email='", email, "'"))
    dbDisconnect(con)
    if (nrow(res) == 1 && digest(password, algo = "sha256") == res$password) {
      return(list(
        username = res$username,
        email = res$email,
        stakeholder = res$stakeholder,
        icon = res$icon
      ))
    } else {
      return(NULL)
    }
  }
