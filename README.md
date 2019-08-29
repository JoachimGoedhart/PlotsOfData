# PlotsOfData
A Shiny App for comparison of samples

[![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/JoachimGoedhart/Plotsofdata/master?urlpath=shiny)

### About PlotsOfData

PlotsOfData visualizes data and statistics to enable the comparison of (experimental) conditions. The philosophy of the approach is that plotting the raw data (instead of a summary) improves transparency and interpretation (see also [this blog](http://thenode.biologists.com/leaving-bar-five-steps/)). To further facilitate the comparison, summary statistics (mean, median, boxplot) and inferential statistics (confidence intervals) can be added. The user has full control over the visibility of the raw data and statistics by adjustment of the transparency (alpha). Details are described in a [publication](https://doi.org/10.1371/journal.pbio.3000202)

Bootstrapping is used to calculate the (asymmetric) 95% CI of medians. More information in [this blog](http://thenode.biologists.com/a-better-bar/education/)

The data can be supplied in spreadsheet/long format (e.g. by copy-pasting from excel) or in tidy format. For more information on the conversion of spreadsheet data to tidy data see [this blog](http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/).

The plot can be saved as a PDF file, which can be opened and edited with Adobe Illustrator to allow for fine adjustments of the lay-out.

### Running the App

Currently, the app can only run from R/Rstudio.

Give it a quick try by running it directly from Github. In the command line (in R or Rstudio) type
shiny::runGitHub('PlotsOfData', 'JoachimGoedhart')

Or download it to use it offline:

-download the app.R and csv files with example data.

-Run RStudio and load app.R

-Select 'Run All' (shortcut is command-option-R on a Mac) or click on "Run App" (upper right button on the window)

This should launch a web browser with the Shiny app.
Note that the app depends on several R packages that need to be installed (shiny, ggplot2, dplyr, tidyr, readr, magrittr, ggbeeswarm, readxl, RCurl)
Run this command in R/Rstudio to download and install all the packages at once:
-install.packages("shiny", "ggplot2", "dplyr", "tidyr", "readr", "readxl", "magrittr", "DT", "ggbeeswarm", "RCurl")

### Background info

Videos that highlight several of the features of PlotsOfData are [available on YouTube](https://bit.ly/2FteV34)

Some aspects of the app are explained in [blogs at The Node](http://thenode.biologists.com/author/joachimg/)

### Credits

PlotsOfData is inspired on [BoxPlotR](http://shiny.chemgrid.org/boxplotr/). See [this link](https://www.nature.com/articles/nmeth.2813) for background information on boxplots.  
The code for the shiny app is partially derived from [ggplotGUI](https://github.com/gertstulp/ggplotgui) by [Gert Stulp](https://www.gertstulp.com)  
The colorblind safe palettes were developed by [Paul Tol](https://personal.sron.nl/~pault/).

PlotsOfData is created and maintained by Joachim Goedhart ([@joachimgoedhart](https://twitter.com/joachimgoedhart))and Marten Postma

### Example output

![alt text](https://github.com/JoachimGoedhart/Plotsofdata/blob/master/ComparisonPlot_example1.png "Output")
