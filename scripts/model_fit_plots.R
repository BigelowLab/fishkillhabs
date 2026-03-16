# Saves diagnostic plots from distribution model fitting

source("setup.R")

species = "Alexandrium catenella"
model_v = "v3"

cfg = read_configuration(scientificname = species,
                         version = model_v, 
                         path = data_path("models"))

file = gsub(" ", "-", sprintf("%s-%s-model_fits", species, model_v))

model_fits = read_model_fit(filename = file)

model_fits

model_fit_metrics(model_fits)

# Confusion Matrix

cm = model_fit_confmat(model_fits)

filename = sprintf("%s_%s_%s.png",
                   species = "ac",
                   mtype = "v3",
                   type = "confmat")

ggsave(file.path(data_path("model_diagnostic"), filename), cm, width = 9.5, height = 8, units="in")

# ROC AUC curves

rocauc = model_fit_roc_auc(model_fits)

filename = sprintf("%s_%s_%s.png",
                   species = "ac",
                   mtype = "v3",
                   type = "rocauc")

ggsave(file.path(data_path("model_diagnostic"), filename), rocauc, width = 9.5, height = 8, units="in")

# Permutation Importance Plot

pdp = model_fit_pdp(model_fits, wid = "default_rf", title = "Random Forest")

filename = sprintf("%s_%s_%s.png",
                   species = "ac",
                   mtype = "v3",
                   type = "pdp")

ggsave(file.path(data_path("model_diagnostic"), filename), pdp, width = 9.5, height = 8, units="in")
