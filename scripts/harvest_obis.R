# Gather OBIS records for fish killing hab taxa of interest

source("setup.R")

species <- c("Margalefidinium polykrikoides",
             "Heterosigma akashiwo",
             "Noctiluca scintillans", 
             "Karenia mikimotoi",
             "Karenia brevis")


for (s in species) {
  if (!file.exists(file.path(ROOT_DATA_PATH, sprintf("%s.gpkg", gsub(" ", "_", s, fixed = TRUE))))) {
    fetch_obis(s)
  }
}


hs <- read_obis("Heterosigma akashiwo")

kb <- read_obis("Karenia brevis")

ns <- read_obis("Noctiluca scintillans")
