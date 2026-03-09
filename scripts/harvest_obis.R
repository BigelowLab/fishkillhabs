# Gather OBIS records for fish killing HAB taxa of interest

source("setup.R")

species <- c("Margalefidinium polykrikoides",
             "Heterosigma akashiwo",
             "Noctiluca scintillans", 
             "Karenia mikimotoi",
             "Karenia brevis",
             "Alexandrium catenella",
             "Chattonella marina",
             "Pseudochattonella verruculosa",
             "Prymnesium polylepis",
             "Chrysochromulina leadbeateri")


for (s in species) {
  cat(s, "\n")
  if (!file.exists(file.path(data_path("obis"), 
                             sprintf("%s.gpkg", gsub(" ", "_", s, fixed = TRUE))))) {
    cat("downloading", s, "\n")
    fetch_obis(s)
  }
}


hs <- read_obis("Heterosigma akashiwo")

kb <- read_obis("Karenia brevis")

ns <- read_obis("Noctiluca scintillans")
