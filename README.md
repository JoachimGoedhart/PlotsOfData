# PlotsOfData
A Shiny App for comparison of samples

### About PlotsOfData

PlotsOfData visulizes data and statistics to enable the comparison of (experimental) conditions. The philosophy of the approach is that plotting the raw data (instead of a summary) improves transparency and interpretation (see also [this blog](http://thenode.biologists.com/leaving-bar-five-steps/)). To further facilitate the comparison, summary statistics (mean, median, boxplot) and inferential statistics (confidence intervals) can be added. The user has full control over the visibility of the raw data and statistics by adjustment of the transparency (alpha).

Bootstrapping is used to calculate the (asymmetric) 95% CI of medians. More information in [this blog](http://thenode.biologists.com/a-better-bar/education/)

The data can be supplied in spreadsheet/long format (e.g. by copy-pasting from excel) or in tidy format. For more information on the conversion of spreadsheet data to tidy data see [this blog](http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/).

The plot can be saved as a PDF file, which can be opened and edited with Adobe Illustrator to allow for fine adjustments of the lay-out.

### Running the App

Currently, the app can only run locally using Rstudio:
-download the app.R and csv files with example data.
-Run RStudio and load app.R
-Select 'Run All' (shortcut is command-option-R on a Mac)

This should launch a web browser with the Shiny app.
Note that the app depends on several R packages that need to be installed (shiny, ggplot2, dplyr, tidyr, readr, magrittr, ggbeeswarm, readxl)


### Credits

PlotsOfData is inspired on [BoxPlotR](http://shiny.chemgrid.org/boxplotr/). See [this link](https://www.nature.com/articles/nmeth.2813) for background information on boxplots.  
The code for the shiny app is partially derived from [ggplotGUI](https://github.com/gertstulp/ggplotgui) by [Gert Stulp](https://www.gertstulp.com)  
The colorblind safe palletes were developed by [Paul Tol](https://personal.sron.nl/~pault/).

PlotsOfData is created and maintained by Joachim Goedhart ([@joachimgoedhart](https://twitter.com/joachimgoedhart))and Marten Postma
