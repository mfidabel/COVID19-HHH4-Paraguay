# Tracking and predicting the South American COVID-19 pandemic

This code was tested with R version 3.6.3 on Ubuntu 18.04 LTS and Windows and version 4.0.2 on Mac OS

Set the working directory to the one with these source files, and run these files:

- `01_data-processing-model.R`: cleans and processes the input data in `data/original` and gets it ready for modelling. The output of this script goes into the `data/processed` folder.

- `01_data-processing-plot.R`: cleans and processes the input data in `data/original` and gets it ready for plotting. The output of this script goes into the `data/processed` folder.

- `02_modelfitting.R`: uses the processed data to fit the models and saves them in `output/models/`. Summary tables are also produced and saved in `output/tables`.

- `03_figures-model.R`: uses fitted model to create figures in the `figs` folder.

- `03_figures_aux.R`: use processed data and fitted model to create other figures in the `figs` folder.

The `R` folder contains the `functions.R` file that has a set of costum functions needed to run the code.

