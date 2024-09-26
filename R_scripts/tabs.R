# Sample data and images
fish_info <- data.frame(
  species = c("Thunnus thynnus", "Xiphias gladius", "Mustelus mustelus",
              "Aetomylaeus bovinus", "Dasyatis pastinaca", "Rhinobatos rhinobatos"),
  common_name = c("Bluefin Tuna", "Swordfish", "Common Smooth-Hound",
                  "Bovine Eagle Ray", "Common Stingray", "Common Guitarfish"),
  description = c(
    "The Bluefin Tuna is a large species of tuna that is found in the Atlantic Ocean and the Mediterranean Sea. It is known for its speed and strength, making it a prized catch.",
    "Swordfish are large, predatory fish characterized by their long, flat bill. They are highly migratory and are found in both tropical and temperate parts of the ocean.",
    "The Common Smooth-Hound is a type of houndshark. It is found in the eastern Atlantic Ocean and the Mediterranean Sea and is known for its smooth skin and docile nature.",
    "The Bovine Eagle Ray is a large species of ray found in the eastern Atlantic Ocean and Mediterranean Sea. It is known for its distinctive shape and ability to leap out of the water.",
    "The Common Stingray is found in coastal waters of the northeastern Atlantic Ocean and Mediterranean Sea. It has a venomous stinger and is often caught as bycatch.",
    "The Common Guitarfish is a species of cartilaginous fish that resembles both sharks and rays. It is commonly found in the Mediterranean Sea and the eastern Atlantic Ocean."
  ),
  season = c(
    "June - October", "April - September", "May - November",
    "April - September", "June - October", "May - August"
  ),
  stats = c(
    "Frequently caught, highly consumed especially in export markets.",
    "Moderately caught, prized in both local and export markets.",
    "Less frequently caught, mainly consumed locally.",
    "Rarely caught, often bycatch in trawling operations.",
    "Frequently caught, both targeted and as bycatch.",
    "Occasionally caught, targeted for both local consumption and export."
  ),
  image = c(
    "species/Tthunnus.jpg",
    "species/Xgladius.jpg",
    "species/Mmustelus.jpg",
    "species/Abovinus.jpg",
    "species/Dpastinaca.jpg",
    "species/Rrhinobatos.jpg"
  )
)

# Define the tabPanel
common_fish <- tabPanel(
  title = "Tunisian Species",
  icon = icon("fish"),
  value = "TunisianSpecies",
  fluidRow(
    column(4,
           h3(fish_info$common_name[1]),
           img(src = fish_info$image[1], height = "150px"),
           h4(paste("Scientific Name:", fish_info$species[1])),
           p(fish_info$description[1]),
           p(strong("Season:"), fish_info$season[1]),
           p(strong("Catch/Consumption:"), fish_info$stats[1])
    ),
    column(4,
           h3(fish_info$common_name[2]),
           img(src = fish_info$image[2], height = "150px"),
           h4(paste("Scientific Name:", fish_info$species[2])),
           p(fish_info$description[2]),
           p(strong("Season:"), fish_info$season[2]),
           p(strong("Catch/Consumption:"), fish_info$stats[2])
    ),
    column(4,
           h3(fish_info$common_name[3]),
           img(src = fish_info$image[3], height = "150px"),
           h4(paste("Scientific Name:", fish_info$species[3])),
           p(fish_info$description[3]),
           p(strong("Season:"), fish_info$season[3]),
           p(strong("Catch/Consumption:"), fish_info$stats[3])
    )
  ),
  fluidRow(
    column(4,
           h3(fish_info$common_name[4]),
           img(src = fish_info$image[4], height = "150px"),
           h4(paste("Scientific Name:", fish_info$species[4])),
           p(fish_info$description[4]),
           p(strong("Season:"), fish_info$season[4]),
           p(strong("Catch/Consumption:"), fish_info$stats[4])
    ),
    column(4,
           h3(fish_info$common_name[5]),
           img(src = fish_info$image[5], height = "150px"),
           h4(paste("Scientific Name:", fish_info$species[5])),
           p(fish_info$description[5]),
           p(strong("Season:"), fish_info$season[5]),
           p(strong("Catch/Consumption:"), fish_info$stats[5])
    ),
    column(4,
           h3(fish_info$common_name[6]),
           img(src = fish_info$image[6], height = "150px"),
           h4(paste("Scientific Name:", fish_info$species[6])),
           p(fish_info$description[6]),
           p(strong("Season:"), fish_info$season[6]),
           p(strong("Catch/Consumption:"), fish_info$stats[6])
    )
  )
)
