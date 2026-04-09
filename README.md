Risk Maps for Fish Killing (Mass Mortality) Harmful Algal Bloom Events
================

## Setup

Single script handles all package dependency installation and loading.

Variable `ROOT_DATA_PATH` must be updated to your preferred location for
saving model input, fitted models, predictions, etc.

    source("setup.R")

## Modeling workflow

Scripts:

1.  background_points.R

    - Collects OBIS observations for a specific species
    - Filters observations by depth cutoff (ie \<= 500m)
    - Gathers background points for each month (average number of
      presence observations per month)
    - Writes observations (presence and background) to file

2.  covariates.R

    - Reads in model input
    - Loads environmental covariates as stars object
    - Matches each point in model input with covariates
    - Writes model input to file

3.  models.R

    - Loads model input (presence and background points matched with
      covariates)
    - Splits input using 80% training 20% testing ratio
    - Splits training set into 5 validation folds
    - Defines model types along with hyperparameters to tune
    - Fits each model using cross-validation while also tuning
      hyperparameters through search
    - Saves fitted version of highest performing model from each type

4.  predictions.R

    - Loads fitted models and monthly climatologies
    - Predicts probability of species occurence across each month for
      full domain of cimatology

## Requirements

### CRAN

- [remotes](https://CRAN.R-project.org/package=remotes)
- [ggplot2](https://CRAN.R-project.org/package=ggplot2)
- [readr](https://CRAN.R-project.org/package=readr)
- [tidyr](https://CRAN.R-project.org/package=tidyr)
- [rnaturalearth](https://CRAN.R-project.org/package=rnaturalearth)
- [robis](https://CRAN.R-project.org/package=robis)
- [sf](https://CRAN.R-project.org/package=sf)  
- [dplyr](https://CRAN.R-project.org/package=dplyr)
- [patchwork](https://CRAN.R-project.org/package=patchwork)
- [stars](https://CRAN.R-project.org/package=stars)
- [tidysdm](https://CRAN.R-project.org/package=tidysdm)
- [effectplots](https://CRAN.R-project.org/package=effectplots)
- [caret](https://CRAN.R-project.org/package=caret)
- [ranger](https://CRAN.R-project.org/package=ranger)

### Github (Bigelow)

- [andreas](https://github.com/BigelowLab/andreas)
- [cofbb](https://github.com/BigelowLab/cofbb)
