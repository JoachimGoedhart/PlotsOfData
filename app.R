# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PlotsOfData: Shiny app for plotting and comparing the data
# Created by Joachim Goedhart (@joachimgoedhart), first version 2018
# Takes non-tidy, spreadsheet type data as input or tidy format
# Non-tidy data is converted into tidy format
# For tidy data the x and y variables need to be selected
# Raw data is displayed with user-defined visibility (alpha)
# Summary statistics are displayed with user-defined visibility (alpha)
# Inferential statistics (95%CI) can be added
# The 95%CI of the median is determined by resampling (bootstrap)
# A plot and a table with stats are generated
# Colors can be added to the data and/or the stats
# Several colorblind safe palettes are available
# Ordering of the categorial data is 'as is, based on median or alphabetical
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Copyright (C) 2018  Joachim Goedhart
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(ggbeeswarm)
library(readxl)
library(DT)
library(RCurl)

#Uncomment for sinaplot
#library(ggforce)

source("themes.R")

#Function that resamples a vector (with replacement) and calculates the median value
boot_median = function(x) {
  median(sample(x, replace = TRUE))
}

i=0
#Number of bootstrap samples
nsteps=1000

#Confidence level
Confidence_Percentage = 95
Confidence_level = Confidence_Percentage/100

alpha=1-Confidence_level
lower_percentile=(1-Confidence_level)/2
upper_percentile=1-((1-Confidence_level)/2)


#Code to generate vectors in R to use these palettes

#From Paul Tol: https://personal.sron.nl/~pault/
Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')

Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')

Tol_light <- c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD')

#From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")


#Read a text file (comma separated values)

df_wide_example <- read.csv("Data_wide_example.csv", na.strings = "")
df_tidy_example <- read.csv("Data_tidy_example.csv", na.strings = "")

#df_wide_example <- data.frame(X=c(1,2,3),Y=c(3,4,5),Z=c(2,5,6))
#df_tidy_example <- data.frame(X=c(1,2,3),Y=c(3,4,5),Z=c(2,5,6))

# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)


###### UI: User interface #########

ui <- fluidPage(
  
  titlePanel("PlotsOfData - Plots all Of the Data"),
  sidebarLayout(
    sidebarPanel(width=3,
      conditionalPanel(
        condition = "input.tabs=='Plot'",
        radioButtons("jitter_type", "Data offset", choices = list("Quasirandom" = "quasirandom", 
#Uncomment for sinaplot                                           "Sinaplot" = "sina", 
                                                                  "Random" = "random", 
                                                                  "None; stripes" = "stripes",
                                                                  "None (for small n)" = "none"), selected = "quasirandom"),
        
        sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.3),

        radioButtons("summaryInput", "Statistics", choices = list("Median" = "median", "Mean" = "mean", "Boxplot (minimum n=10)" = "box", "Violin Plot (minimum n=10)" = "violin"), selected = "median"),
        checkboxInput(inputId = "add_CI", label = HTML("Add 95% CI <br/> (minimum n=10)"), value = FALSE),
        conditionalPanel(
          condition = "input.add_CI == true && input.summaryInput !='box'",
          checkboxInput(inputId = "ugly_errors", label = "Classic error bars", value = FALSE)),
  
        #Uncomment for grey box that indicates range
        conditionalPanel(
              condition = "input.summaryInput == 'median' || input.summaryInput == 'mean'",

        checkboxInput(inputId = "add_bar", label = HTML("Add a box that shows the range"), value = FALSE)),

        sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),

        radioButtons(inputId = "ordered",
             label= "Order of the conditions:",
             choices = list("As supplied" = "none", "By median value" = "median", "By alphabet/number" = "alphabet"),
             selected = "none"),

        h4("Plot Layout"),      

        checkboxInput(inputId = "rotate_plot",
              label = "Rotate plot 90 degrees",
              value = FALSE),

        checkboxInput(inputId = "no_grid",
                      label = "Remove gridlines",
                      value = FALSE),

        checkboxInput(inputId = "change_scale",
                      label = "Change scale",
                      value = FALSE),
        conditionalPanel(condition = "input.change_scale == true",
          checkboxInput(inputId = "scale_log_10",
                        label = "Log scale",
                        value = FALSE),

          textInput("range", "Range of values (min,max)", value = "")),

        checkboxInput("color_data", "Use color for the data", value=FALSE),
        checkboxInput("color_stats", "Use color for the stats", value=FALSE),

        conditionalPanel(
            condition = "input.color_data == true || input.color_stats == true",
            ########## Choose color from list
            # selectInput("colour_list", "Colour:", choices = ""),

          radioButtons("adjustcolors", "Color palette:", choices = 
            list(
              "Standard" = 1,
              "Okabe&Ito; CUD" = 6,
              "Tol; bright" = 2,
              "Tol; muted" = 3,
              "Tol; light" = 4,
              "User defined"=5),
            selected =  6),
      
              conditionalPanel(condition = "input.adjustcolors == 5",
                 textInput("user_color_list", "Names or hexadecimal codes separated by a comma (applied to conditions in alphabetical order):", value = "turquoise2,#FF2222,lawngreen"), 
                 
                 h5("",
                    a("Click here for more info on color names",
                      href = "http://www.endmemo.com/program/R/color.php", target="_blank"))
                 
        )),
        checkboxInput(inputId = "dark", label = "Dark Theme", value = FALSE),
        numericInput("plot_height", "Height (# pixels): ", value = 480),
        numericInput("plot_width", "Width (# pixels):", value = 480),

        h4("Labels/captions"),

        checkboxInput(inputId = "add_title",
                        label = "Add title",
                        value = FALSE),
        conditionalPanel(
        condition = "input.add_title == true",
        textInput("title", "Title:", value = "")
        ),

        checkboxInput(inputId = "label_axes",
              label = "Change labels",
              value = FALSE),
        conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = ""),
              textInput("lab_y", "Y-axis:", value = "")),
        checkboxInput(inputId = "adj_fnt_sz",
              label = "Change font size",
              value = FALSE),
       conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_ttl", "Size axis titles:", value = 24),
              numericInput("fnt_sz_ax", "Size axis labels:", value = 18)),
        checkboxInput(inputId = "add_description",
              label = "Add figure description",
              value = FALSE),
        NULL

    ),

    conditionalPanel(
        condition = "input.tabs=='Data upload'",
        h4("Data upload"),
        radioButtons(
          "data_input", "",
          choices = 
            list("Example 1 (wide format)" = 1,
                 "Example 2 (tidy format)" = 2,
                 "Upload file" = 3,
                 "Paste data" = 4,
                 "URL (csv files only)" = 5
                 )
          ,
          selected =  1),
        conditionalPanel(
          condition = "input.data_input=='1'"
          
        ),
        conditionalPanel(
          condition = "input.data_input=='3'",
          
          fileInput("upload", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),
          # selectInput("file_type", "Type of file:",
          #             list(".csv or .txt" = "text",
          #                  ".xls or .xlsx" = "excel"
          #             ),
          #             selected = "text"),
          
          #   radioButtons(
          #     "upload_delim", "Delimiter",
          #     choices =
          #       list("Comma" = ",",
          #            "Tab" = "\t",
          #            "Semicolon" = ";",
          #            "Space" = " ")),
          #     selected = ","),         
          
          selectInput("upload_delim", label = "Select Delimiter (for text file):", choices =list("Comma" = ",",
                                                                                                 "Tab" = "\t",
                                                                                                 "Semicolon" = ";",
                                                                                                 "Space" = " ")),
          
          
          selectInput("sheet", label = "Select sheet (for excel workbook):", choices = " ")
          
          
          
          
          # actionButton("submit_datafile_button", "Submit datafile")
          
        ),
        conditionalPanel(
          condition = "input.data_input=='4'",
          h5("Paste data below:"),
          tags$textarea(id = "data_paste",
                        placeholder = "Add data here",
                        rows = 10,
                        cols = 20, ""),
          actionButton("submit_data_button", "Submit data"),
              radioButtons(
                "text_delim", "Delimiter",
                choices = 
                    list("Tab (from Excel)" = "\t",
                         "Space" = " ",
                         "Comma" = ",",
                         "Semicolon" = ";"),
                          selected = "\t")),
        
### csv via URL as input      
              conditionalPanel(
                condition = "input.data_input=='5'",
      #         textInput("URL", "URL", value = "https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"), 
                textInput("URL", "URL", value = ""), 
                NULL
                ),
        
        checkboxInput(inputId = "tidyInput",
                      label = "These data are Tidy",
                      value = FALSE),
        
        conditionalPanel(
          condition = "input.tidyInput==false", selectInput("data_remove", "Select columns to remove", "", multiple = TRUE)),
        
        conditionalPanel(
          condition = "input.tidyInput==true",

          selectInput("x_var", "Conditions to compare:", choices = ""),
          selectInput("y_var", "Variables:", choices = ""),
#         selectInput("h_facet", "Separate horizontal:", choices = ""),
#         selectInput("v_facet", "Separate vertical:", choices = ""),

          NULL
          ), 
        
        
        
        conditionalPanel(
          condition = "input.tidyInput==false", (downloadButton("downloadData", "Download in tidy format (csv)"))),

        hr(),

        checkboxInput(inputId = "info_data",
                      label = "Show information on data formats",
                      value = FALSE),

        conditionalPanel(
          condition = "input.info_data==true",
              img(src = 'Data_format.png', width = '100%'), h5(""), a("Background info for converting wide data to tidy format", href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")
          )

      ),
      
      conditionalPanel(
        condition = "input.tabs=='About'",
        
        #Session counter: https://gist.github.com/trestletech/9926129
        h4("About"),  "There are currently", 
        verbatimTextOutput("count"),
        "session(s) connected to this app.",
        hr(),
        h4("Find our other dataViz apps at:"),a("https://huygens.science.uva.nl/", href = "https://huygens.science.uva.nl/")
      ),

      conditionalPanel(
        condition = "input.tabs=='Data Summary'",
        h4("Data summary") ,
        checkboxGroupInput("stats_select", label = h5("Statistics for table:"), 
                           choices = list("mean", "sd", "sem","95CI mean", "median", "MAD", "IQR", "Q1", "Q3", "95CI median"),
                           selected = "sem"),
        actionButton('select_all1','select all'),
        actionButton('deselect_all1','deselect all'),
        numericInput("digits", "Digits:", 2, min = 0, max = 5)
#        ,
#        selectInput("stats_hide2", "Select columns to hide", "", multiple = TRUE, choices=list("mean", "sd", "sem","95CI mean", "median", "MAD", "IQR", "Q1", "Q3", "95CI median")
      )   
      
    ),
    mainPanel(
 
       tabsetPanel(id="tabs",
                  tabPanel("Data upload", h4("Data as provided"),
                  dataTableOutput("data_uploaded")),
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"),
                           downloadButton("downloadPlotSVG", "Download svg-file"), 
                           downloadButton("downloadPlotEPS", "Download eps-file"), 
                           downloadButton("downloadPlotPNG", "Download png-file"), 
                           actionButton("settings_copy", icon = icon("clone"),
                                        label = "Clone current setting"),
                           actionButton("legend_copy", icon = icon("clone"),
                                        label = "Copy Legend"),
                                        
                                        div(`data-spy`="affix", `data-offset-top`="10", plotOutput("coolplot", height="100%"),
                                            htmlOutput("LegendText", width="200px", inline =FALSE),
#                                            htmlOutput("HTMLpreset"),
                                            NULL)
                  ), 
                  tabPanel("Data Summary",dataTableOutput('data_summary')
                           ),
                  tabPanel("About", includeHTML("about.html")
                           )
        )
    )
  )         
)


server <- function(input, output, session) {

  
  isolate(vals$count <- vals$count + 1)
  ###### DATA INPUT ###################

df_upload <- reactive({
    
    if (input$data_input == 1) {
      data <- df_wide_example
    }  else if (input$data_input == 2) {
        data <- df_tidy_example 
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Click 'Browse...' to select a datafile or drop file onto 'Browse' button"))
        # } else if (input$submit_datafile_button == 0) {
        #   return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        
        #Isolate extenstion and convert to lowercase
        filename_split <- strsplit(file_in$datapath, '[.]')[[1]]
        fileext <- tolower(filename_split[length(filename_split)])
        
        # observe({print(fileext)})
        
        # isolate({
        # data <- read.csv(file=file_in$datapath, sep = input$upload_delim, na.strings=c("",".","NA", "NaN", "#N/A", "#VALUE!"))
        
        if (fileext == "txt" || fileext=="csv") {
          
          data <- read.csv(file=file_in$datapath, sep = input$upload_delim, na.strings=c("",".","NA", "NaN", "#N/A", "#VALUE!"))
          updateSelectInput(session, "sheet", choices = " ", selected = " ")
        } else if (fileext=="xls" || fileext=="xlsx") {
          names <- excel_sheets(path = input$upload$datapath)
          # updateSelectInput(session, "sheet_names", choices = names)
          sheet.selected <<- input$sheet 
          updateSelectInput(session, "sheet", choices = names, selected = sheet.selected)
          
          if (input$sheet %in% names)
          {
            n <- which(names==input$sheet)
            # sheet.selected <<- input$sheet 
          } else {
            n <- 1
            #Ensures update and selection of first sheet upon loading the data
            updateSelectInput(session, "sheet", choices = names)
          }
          
          # names <- excel_sheets(path = input$upload$datapath)
          # updateSelectInput(session, "sheet_names", choices = names)
          data <- read_excel(file_in$datapath, sheet = n , na = c("",".","NA", "NaN", "#N/A", "#VALUE!"))
        } 
        
        # })
      }
      
    } else if (input$data_input == 5) {
      
      #Read data from a URL
      #This requires RCurl
      if(input$URL == "") {
        return(data.frame(x = "Enter a full HTML address, for example: https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"))
      } else if (url.exists(input$URL) == FALSE) {
         return(data.frame(x = paste("Not a valid URL: ",input$URL)))
      } else {data <- read_csv(input$URL)}
    
      #Read the data from textbox
    } else if (input$data_input == 4) {
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          isolate({
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
          })
        }
      }
    }
    updateSelectInput(session, "data_remove", choices = names(data))
    
    #Replace space and dot of header names by underscore
    data <- data %>%  
      select_all(~gsub("\\s+|\\.", "_", .))
    
    return(data)
})
  
  
##### REMOVE SELECTED COLUMNS #########
df_filtered <- reactive({     
  
  if (!is.null(input$data_remove)) {
    columns = input$data_remove
    df <- df_upload() %>% select(-one_of(columns))
  } else if (is.null(input$data_remove)) {
  df <- df_upload()}
  
})

##### CONVERT TO TIDY DATA ##########
  
#Need to tidy the data?!
#Untidy data will be converted to long format with two columns named 'Condition' and 'Value'
#The input for "Condition" will be taken from the header, i.e. first row
#Tidy data will be used as supplied
df_upload_tidy <- reactive({
    if(input$tidyInput == FALSE ) {
      klaas <- df_upload()

      klaas <- df_filtered() %>% gather(Condition, Value)
    }
    else if(input$tidyInput == TRUE ) {
      klaas <- df_upload()
    }
  return(klaas)
})

##### Get Variables from the input ##############

observe({ 
        var_names  <- names(df_upload())
        varx_list <- c("none", var_names)

        # Get the names of columns that are factors. These can be used for coloring the data with discrete colors        
        nms_fact <- names(Filter(function(x) is.factor(x) || is.integer(x) ||
                                   is.logical(x) ||
                                   is.character(x),
                                 df_upload_tidy()))
        nms_var <- names(Filter(function(x) is.integer(x) ||
                                  is.numeric(x) ||
                                  is.double(x),
                                df_upload_tidy()))
        
        vary_list <- c("none",nms_var)
        
        facet_list <- c(".",nms_fact)

        # updateSelectInput(session, "colour_list", choices = nms_fact)
        updateSelectInput(session, "y_var", choices = vary_list)
        updateSelectInput(session, "x_var", choices = varx_list)
        updateSelectInput(session, "h_facet", choices = facet_list)
        updateSelectInput(session, "v_facet", choices = facet_list)
        
 #       if (input$add_bar == TRUE) {
#          updateSelectInput(session, "alphaInput", min = 0.3)
#       }

    })

# observeEvent(input$add_bar, {
#   showNotification("clicked!", type = "default")
# },ignoreNULL = F)

###### When a bar is added, make sure that the data is still visible
observeEvent(input$add_bar, {
  if (input$add_bar==TRUE)  {
    updateSliderInput(session, "alphaInput", min=0.2, max=1)

  } else if (input$add_bar==FALSE)  {
    updateSliderInput(session, "alphaInput", min=0, max=1)
    
  }
})

########### GET INPUT VARIABLEs FROM HTML ##############

observe({
  
  ############ ?data ################
  
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query[['data']])) {
    presets_data <- query[['data']]
    presets_data <- unlist(strsplit(presets_data,";"))
    observe(print((presets_data[1])))
#    observe(print(("hello")))    
    updateRadioButtons(session, "data_input", selected = presets_data[1])    
    updateCheckboxInput(session, "tidyInput", value = presets_data[2])
    
    #To Implement:
    #presets_data[3], x_var
    #presets_data[4], y_var
    #presets_data[5], h_facet
    #presets_data[6], v_facet
    }

  ############ ?vis ################
  
  if (!is.null(query[['vis']])) {

  presets_vis <- query[['vis']]
  presets_vis <- unlist(strsplit(presets_vis,";"))
  observe(print((presets_vis)))
  
  #radio, slider, radio, check, slider
  updateRadioButtons(session, "jitter_type", selected = presets_vis[1])
  updateSliderInput(session, "alphaInput", value = presets_vis[2])
  updateRadioButtons(session, "summaryInput", selected = presets_vis[3])
  updateCheckboxInput(session, "add_CI", value = presets_vis[4])
  updateSliderInput(session, "alphaInput_summ", value = presets_vis[5])
  updateRadioButtons(session, "ordered", selected = presets_vis[6])
#  updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?layout ################
  
  if (!is.null(query[['layout']])) {
    
    presets_layout <- query[['layout']]
    presets_layout <- unlist(strsplit(presets_layout,";"))
    observe(print((presets_layout)))

    updateCheckboxInput(session, "rotate_plot", value = presets_layout[1])
    updateCheckboxInput(session, "no_grid", value = (presets_layout[2]))

    updateCheckboxInput(session, "change_scale", value = presets_layout[3])
    updateCheckboxInput(session, "scale_log_10", value = presets_layout[4])
     updateTextInput(session, "range", value= presets_layout[5])
     updateCheckboxInput(session, "color_data", value = presets_layout[6])
     updateCheckboxInput(session, "color_stats", value = presets_layout[7])
     updateRadioButtons(session, "adjustcolors", selected = presets_layout[8])    
     updateCheckboxInput(session, "add_description", value = presets_layout[9])
     if (length(presets_layout)>10) {
       updateNumericInput(session, "plot_height", value= presets_layout[10])
       updateNumericInput(session, "plot_width", value= presets_layout[11])
     }
    #  updateTabsetPanel(session, "tabs", selected = "Plot")
  }

  ############ ?color ################
  
  if (!is.null(query[['color']])) {
    
    presets_color <- query[['color']]
    presets_color <- unlist(strsplit(presets_color,";"))

    # updateSelectInput(session, "colour_list", selected = presets_color[1])
    updateTextInput(session, "user_color_list", value= presets_color[2])
  }

    ############ ?label ################

  if (!is.null(query[['label']])) {
    
    presets_label <- query[['label']]
    presets_label <- unlist(strsplit(presets_label,";"))
    observe(print((presets_label)))
    
    
    updateCheckboxInput(session, "add_title", value = presets_label[1])
    updateTextInput(session, "title", value= presets_label[2])

    updateCheckboxInput(session, "label_axes", value = presets_label[3])
    updateTextInput(session, "lab_x", value= presets_label[4])
    updateTextInput(session, "lab_y", value= presets_label[5])
    
    updateCheckboxInput(session, "adj_fnt_sz", value = presets_label[6])
    updateNumericInput(session, "fnt_sz_ttl", value= presets_label[7])
    updateNumericInput(session, "fnt_sz_ax", value= presets_label[8])
    updateCheckboxInput(session, "add_description", value = presets_label[9])
    }
  
  ############ ?url ################
  
  if (!is.null(query[['url']])) {
    updateRadioButtons(session, "data_input", selected = 5)  
    updateTextInput(session, "URL", value= query[['url']])
    observe(print((query[['url']])))
    updateTabsetPanel(session, "tabs", selected = "Plot")
  }
})

########### RENDER URL ##############

output$HTMLpreset <- renderText({
  url()
  })

######### GENERATE URL with the settings #########

url <- reactive({

  base_URL <- paste(sep = "", session$clientData$url_protocol, "//",session$clientData$url_hostname, ":",session$clientData$url_port, session$clientData$url_pathname)
  
  data <- c(input$data_input, input$tidyInput, input$x_var, input$y_var, input$h_facet, input$v_facet)

  vis <- c(input$jitter_type, input$alphaInput, input$summaryInput, input$add_CI, input$alphaInput_summ, input$ordered)
  layout <- c(input$rotate_plot, input$no_grid, input$change_scale, input$scale_log_10, input$range, input$color_data, input$color_stats,
              input$adjustcolors, input$add_description, input$plot_height, input$plot_width)

  #Hide the standard list of colors if it is'nt used
   if (input$adjustcolors != "5") {
     color <- c("", "none")
   } else if (input$adjustcolors == "5") {
     color <- c("", input$user_color_list)
   }
  
  label <- c(input$add_title, input$title, input$label_axes, input$lab_x, input$lab_y, input$adj_fnt_sz, input$fnt_sz_ttl, input$fnt_sz_ax, input$add_description)

  #replace FALSE by "" and convert to string with ; as seperator
  data <- sub("FALSE", "", data)
  data <- paste(data, collapse=";")
  data <- paste0("data=", data) 
  
  vis <- sub("FALSE", "", vis)
  vis <- paste(vis, collapse=";")
  vis <- paste0("vis=", vis) 
  
  layout <- sub("FALSE", "", layout)
  layout <- paste(layout, collapse=";")
  layout <- paste0("layout=", layout) 
  
  color <- sub("FALSE", "", color)
  color <- paste(color, collapse=";")
  color <- paste0("color=", color) 
  
  label <- sub("FALSE", "", label)
  label <- paste(label, collapse=";")
  label <- paste0("label=", label) 
  
  if (input$data_input == "5") {url <- paste("url=",input$URL,sep="")} else {url <- NULL}

  parameters <- paste(data, vis,layout,color,label,url, sep="&")
  
  preset_URL <- paste(base_URL, parameters, sep="?")

 observe(print(parameters))
 observe(print(preset_URL))  
 return(preset_URL)
  })

############# Pop-up that displays the URL to 'clone' the current settings ################

observeEvent(input$settings_copy , {
  showModal(urlModal(url=url(), title = "Use the URL to launch PlotsOfData with the current setting"))
})

observeEvent(input$legend_copy , {
  showModal(urlModal(url=Fig_legend(), title = "Legend text"))
})


############# Pop-up appears when a boxplot or violinplot is selected when n<10 ###########

observeEvent(input$summaryInput , {
  df_temp <- df_summary_mean()
  min_n <- min(df_temp$n)
  if (input$summaryInput == "box" && min_n<10) {
  showModal(modalDialog(
    title = NULL,
    "You have selected a boxplot as summary, but one of the conditions has less than 10 datapoints - For n<10 the boxplot is not a suitable summary", easyClose=TRUE, footer = modalButton("Click anywhere to dismiss")
  ))
  } else if (input$summaryInput == "violin" && min_n<10) {
    showModal(modalDialog(
      title = NULL,
      "You have selected a violinplot as summary, but one of the conditions has less than 10 datapoints - For n<10 the violinplot is not a suitable summary", easyClose=TRUE, footer = modalButton("Click anywhere to dismiss")
    ))
  }
  })

############# Pop-up appears when the 95%CI is selected when n<10 ###########

observeEvent(input$add_CI , {
  df_temp <- df_summary_mean()
  min_n <- min(df_temp$n)

  if (input$add_CI == TRUE && min_n<10) {
    showModal(modalDialog(
      title = NULL,
      "Confidence Intervals are used to make inferences, but one of the conditions has less than 10 datapoints - It is not recommended to show inferential statistics (CI, sem) for n<10", easyClose=TRUE, footer = modalButton("Click anywhere to dismiss")
    ))
  }  
})

######## ORDER the Conditions ####### 

df_sorted <- reactive({
  
  klaas <-  df_selected()

   if(input$ordered == "median") {
     klaas$Condition <- reorder(klaas$Condition, klaas$Value, median, na.rm = TRUE)

   } else if (input$ordered == "none") {
      klaas$Condition <- factor(klaas$Condition, levels=unique(klaas$Condition))

   } else if (input$ordered == "alphabet") {
     klaas$Condition <- factor(klaas$Condition, levels=unique(sort(klaas$Condition)))
   }  
  
    return(klaas)
  
})

######## Extract the data for display & summary stats #######  

df_selected <- reactive({
    if(input$tidyInput == TRUE ) {
    df_temp <- df_upload_tidy() 
    x_choice <- input$x_var
    y_choice <- input$y_var

    koos <- df_temp %>% select(Condition = !!x_choice , Value = !!y_choice) %>% filter(!is.na(Value))
    koos$Condition <- factor(koos$Condition)


    } else if (input$tidyInput == FALSE ) {
      koos <- df_upload_tidy() %>% filter(!is.na(Value))
    }
  
    return(koos)
})


#### DISPLAY UPLOADED DATA (as provided) ##################

output$data_uploaded <- renderDataTable(

#    observe({ print(input$tidyInput) })
  df_filtered(),
  rownames = FALSE,
  options = list(pageLength = 100, autoWidth = FALSE,
                  lengthMenu = c(10, 100, 1000, 10000)),
  editable = FALSE,selection = 'none'
)
  
########### Caluclate stats for the MEAN ############

df_summary_mean <- reactive({
  koos <- df_selected()
  koos$Condition <- factor(koos$Condition)

  koos %>%
    group_by(Condition) %>% 
    summarise(n = n(),
            mean = mean(Value, na.rm = TRUE),
#            median = median(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE)) %>%
      mutate(sem = sd / sqrt(n - 1),
             mean_CI_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
             mean_CI_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem)

  })

############ Caluclate stats for the MEDIAN ##########

df_summary_median <- reactive({
    
    kees <- df_selected()
    kees$Condition <- factor(kees$Condition)
 
 #   df_booted <- data.frame(Condition=levels(factor(kees$Condition)), n=tapply(kees$Value, kees$Condition, length), median=tapply(kees$Value, kees$Condition, median))
    df_booted <- kees %>%
                  group_by(Condition) %>%
                    summarise(
#                            n= n(),
                         median= median(Value, na.rm = TRUE),
                            MAD= mad(Value, na.rm = TRUE, constant=1),
                            IQR= IQR(Value, na.rm = TRUE),
                            Q1=quantile(Value, probs=0.25),
                            Q3=quantile(Value, probs=0.75))
    
    i=0
    df_new_medians <- data.frame(Condition=levels(factor(kees$Condition)), resampled_median=tapply(kees$Value, kees$Condition, boot_median))
    
    #Perform the resampling nsteps number of times (typically 1,000-10,000x)
    for (i in 1:nsteps) {
      
      #Caclulate the median from a boostrapped sample (resampled_median) and add to the dataframe
      df_boostrapped_median <- data.frame(Condition=levels(factor(kees$Condition)), resampled_median=tapply(kees$Value, kees$Condition, boot_median))
      
      #Add the new median to a datafram that collects all the resampled median values
      df_new_medians <- bind_rows(df_new_medians, df_boostrapped_median)
    }
    
    df_booted$median_CI_lo <- tapply(df_new_medians$resampled_median, df_new_medians$Condition, quantile, probs=lower_percentile)
    df_booted$median_CI_hi <- tapply(df_new_medians$resampled_median, df_new_medians$Condition, quantile, probs=upper_percentile)

#    observe({ print(df_booted) })

    return(df_booted)
  })


######### DEFINE DOWNLOAD BUTTONS ###########

##### Set width and height of the plot area
width <- reactive ({ input$plot_width })
height <- reactive ({ input$plot_height })

output$downloadPlotPDF <- downloadHandler(
  filename <- function() {
    paste("PlotsOfData_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    pdf(file, width = input$plot_width/72, height = input$plot_height/72)
    plot(plotdata())
    dev.off()
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadPlotSVG <- downloadHandler(
  filename <- function() {
    paste("PlotsOfData_", Sys.time(), ".svg", sep = "")
  },
  content <- function(file) {
    svg(file, width = input$plot_width/72, height = input$plot_height/72)
    plot(plotdata())
    dev.off()
  },
  contentType = "application/svg" # MIME type of the image
)

output$downloadPlotEPS <- downloadHandler(
  filename <- function() {
    paste("PlotsOfData_", Sys.time(), ".eps", sep = "")
  },
  content <- function(file) {
    cairo_ps(file, width = input$plot_width/72, height = input$plot_height/72)
    plot(plotdata())
    dev.off()

  },
  contentType = "application/eps" # MIME type of the image
)

output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("PlotsOfData_", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    png(file, width = input$plot_width*4, height = input$plot_height*4, res=300)
    plot(plotdata())
    dev.off()
  },
  contentType = "application/png" # MIME type of the image
)



######## PREPARE PLOT FOR DISPLAY ##########


plotdata <- reactive({

  
####### Read the order from the ordered dataframe #############  
    koos <- df_sorted()
    koos$Condition <- factor(koos$Condition)
#   observe({ print(koos) })
    
    custom_order <-  levels(factor(koos$Condition))
#    custom_labels <- levels(factor(koos$label))
    
   observe({ print(custom_order) })
#   observe({ print(custom_labels) })    
    width_column <- 0.7
    
    # Change linecolor in case of dark mode
    if (input$dark) {
      line_color="grey80"
    } else if (input$dark==FALSE) {
      line_color="black"
    } 
  
  ########## Define alternative color palettes ##########
  
    newColors <- NULL
    
    if (input$adjustcolors == 2) {
      newColors <- Tol_bright
    } else if (input$adjustcolors == 3) {
      newColors <- Tol_muted
    } else if (input$adjustcolors == 4) {
      newColors <- Tol_light
    } else if (input$adjustcolors == 6) {
      newColors <- Okabe_Ito
    } else if (input$adjustcolors == 5) {
      newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
    }

    ### Set default to Plotting "Condition" and "Value"
    if (input$x_var == "none") {
      x_choice <- "Condition"
    } else if (input$x_var != "none") {
      x_choice <- as.character(input$x_var)
    }  
    
    if (input$y_var == "none") {
      y_choice <- "Value"
    } else if (input$y_var != "none") {
      y_choice <- as.character(input$y_var)
    }

  # Define if color is used for the data
  # observe({ print(class(input$colour_list)) })
  if (input$color_data == FALSE) {
    kleur <- NULL
  } else if (input$color_data == TRUE) {
        # kleur <- as.character(input$colour_list)
        kleur <- "Condition"
  }
  

    if (input$color_data == TRUE || input$color_stats == TRUE) {    
    #Determine the number of colors that are necessary
      max_colors <- nlevels(as.factor(koos$Condition))
      # max_colors <- nlevels(as.factor(klaas[,kleur]))
      
    
        #If unsufficient colors available, repeat
        if(length(newColors) < max_colors) {
          newColors<-rep(newColors,times=(round(max_colors/length(newColors)))+1)
        }
    }
    
  ########## Define if/how color is used for the stats ############
  #    observe({ print(class(input$colour_list)) })
  if (input$color_stats == FALSE) {
    kleur_stats <- NULL
  } else if (input$color_stats == TRUE && input$summaryInput == "box") {
    kleur_stats <- x_choice
  } else if (input$color_stats == TRUE && input$summaryInput == "violin") {
    kleur_stats <- x_choice
  } else if (input$color_stats == TRUE) {
    kleur_stats <- "Condition"
  }  
    
  
    # Define minimal n - only plot box/violinplots for min_n>9
    df_temp <- df_summary_mean()
    min_n <- min(df_temp$n)
    
    ######## Change appearance of plot width under for non-jittered points or segments when n is low #########
    if (input$jitter_type == "none") {width_column <- width_column/2}
#    if (input$jitter_type == "stripes" && min_n <10) {width_column <- width_column/2}


############## GENERATE PLOT LAYERS #############

    
    p <- ggplot(data=df_selected(), aes_string(x="Condition")) 
    
    # Setting the order of the x-axis
    p <- p + scale_x_discrete(limits=custom_order)
    
    
  ##### Add bar/box as visual aid ######  
    if (input$add_bar == TRUE && input$summaryInput == "median") {
      p <- p + stat_summary(data=df_selected(), aes_string(x="Condition", y=y_choice), fun.y = median, fun.ymin = min, fun.ymax = max, geom = "crossbar", width=width_column, color = NA,fill="grey", alpha=input$alphaInput_summ/2)
    }

    if (input$add_bar == TRUE && input$summaryInput == "mean") {
      p <- p + stat_summary(data=df_selected(), aes_string(x="Condition", y=y_choice), fun.y = mean, fun.ymin = min, fun.ymax = max, geom = "crossbar", width=width_column, color = NA,fill="grey", alpha=input$alphaInput_summ/2)
    }
    
    
  ##### plot selected data summary (bottom layer) ####
    if (input$summaryInput == "box" && min_n>9) {
      p <- p + geom_boxplot(data=koos, aes_string(x='Condition', y='Value', fill=kleur_stats, group='Condition'), color=line_color, notch = input$add_CI, outlier.color=NA, width=width_column, size=0.5, alpha=input$alphaInput_summ)
  
    } else if (input$summaryInput == "violin" && min_n>9) {
      p <- p + geom_violin(data=koos, aes_string(x='Condition', y='Value', fill=kleur_stats, group='Condition'), color=line_color,scale = "width", 
 #                          draw_quantiles = c(0.5),
                           width=width_column, size=0.5, alpha=input$alphaInput_summ) 
    }

   #### plot individual measurements (middle layer) ####
    if (input$jitter_type == "quasirandom") {
      p <- p + geom_quasirandom(data=koos, aes_string(x='Condition', y='Value', colour = kleur), shape = 16, varwidth = TRUE, cex=3.5, alpha=input$alphaInput)

#Uncomment for sinaplot    } else if (input$jitter_type == "sina") {
#Uncomment for sinaplot p <- p + geom_sina(data=klaas, aes_string(x=x_choice, y=y_choice, colour = kleur), method="density", maxwidth = .8, cex=3, alpha=input$alphaInput)
      
      
    } else if (input$jitter_type == "random") {
      p <- p + geom_jitter(data=koos, aes_string(x='Condition', y='Value', colour = kleur), shape = 16, width=0.3, height=0.0, cex=3.5, alpha=input$alphaInput)
    } else if (input$jitter_type == "stripes") {

      p <- p + geom_segment(data=df_sorted(), aes(x=match(Condition, levels(Condition))-((width_column/2)-0.1), xend=match(Condition, levels(Condition))+((width_column/2)-0.1), y=Value, yend=Value, colour = kleur), size=1, alpha=input$alphaInput)

      
    } else if (input$jitter_type == "none") {
      p <- p + geom_jitter(data=koos, aes_string(x='Condition', y='Value', colour = kleur), shape = 16, width=0, height=0.0, cex=3.5, alpha=input$alphaInput)
    }
    
  ##### plot selected data summary (top layer) ####
    if (input$summaryInput == "median"  && input$add_CI == TRUE && min_n>9) {
      
      if (!input$ugly_errors) {
      p <-  p + geom_point(data=df_summary_median(), aes_string(x="Condition", y = "median", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)+
         geom_linerange(data=df_summary_median(), aes_string(x="Condition", ymin = "median_CI_lo", ymax = "median_CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)
      } else {
      p <-  p + geom_errorbar(data=df_summary_median(), aes_string(x="Condition", ymin="median", ymax="median", colour = kleur_stats), width=width_column, size=1, alpha=input$alphaInput_summ) +
                 geom_errorbar(data=df_summary_median(), aes_string(x="Condition", ymin="median_CI_lo", ymax="median_CI_hi", colour = kleur_stats), width=width_column/2, size=1, alpha=input$alphaInput_summ)
      
    }
    
    }

    else if (input$summaryInput == "median"  && min_n<10) {
      p <-  p + geom_errorbar(data=df_summary_median(), aes_string(x="Condition", ymin="median", ymax="median", colour = kleur_stats), width=width_column, size=1, alpha=input$alphaInput_summ)

    } else if (input$summaryInput == "mean"  && input$add_CI == TRUE && min_n>9) {
      if (!input$ugly_errors) {
      p <- p + geom_linerange(data=df_summary_mean(), aes_string(x="Condition", ymin = "mean_CI_lo", ymax = "mean_CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)+
        geom_point(data=df_summary_mean(), aes_string(x="Condition", y = "mean", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)
      } else {
        p <-  p + geom_errorbar(data=df_summary_mean(), aes_string(x="Condition", ymin="mean", ymax="mean", colour = kleur_stats), width=width_column, size=1, alpha=input$alphaInput_summ) +
          geom_errorbar(data=df_summary_mean(), aes_string(x="Condition", ymin="mean_CI_lo", ymax="mean_CI_hi", colour = kleur_stats), width=width_column/2, size=1, alpha=input$alphaInput_summ)
        
      }

    } else if (input$summaryInput == "mean"  && min_n<10) {
      p <- p + geom_errorbar(data=df_summary_mean(), aes_string(x="Condition", ymin="mean", ymax="mean", colour=kleur_stats), width=width_column, size=1, alpha=input$alphaInput_summ)
    }  
    else if (input$summaryInput == "median"  && min_n>9 && input$add_CI == FALSE) {
      p <-  p + geom_errorbar(data=df_summary_median(), aes_string(x="Condition", ymin="median", ymax="median", colour = kleur_stats), width=width_column, size=2, alpha=input$alphaInput_summ)
    }
    else if (input$summaryInput == "mean"  && min_n>9 && input$add_CI == FALSE) {
      p <-  p + geom_errorbar(data=df_summary_mean(), aes_string(x="Condition", ymin="mean", ymax="mean", colour = kleur_stats), width=width_column, size=2, alpha=input$alphaInput_summ)
    }
    else if (input$summaryInput == "violin"  && min_n>9 && input$add_CI == FALSE) {
      p <-  p + geom_point(data=df_summary_median(), aes_string(x="Condition", y = "median"), colour="black", shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)
      
    }
    else if (input$summaryInput == "violin"  && min_n>9 && input$add_CI == TRUE) {
      if (!input$ugly_errors) {
        p <-  p + geom_point(data=df_summary_median(), aes_string(x="Condition", y = "median"), colour="black", shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)
        p <- p + geom_linerange(data=df_summary_median(), aes_string(x="Condition", ymin = "median_CI_lo", ymax = "median_CI_hi"), colour="black", size =3,alpha=input$alphaInput_summ)
        
      } else {
        p <-  p + geom_errorbar(data=df_summary_median(), aes_string(x="Condition", ymin="median", ymax="median"), width=width_column*0.95, size=1, alpha=input$alphaInput_summ) +
          geom_errorbar(data=df_summary_median(), aes_string(x="Condition", ymin="median_CI_lo", ymax="median_CI_hi"), width=width_column/2, size=1, alpha=input$alphaInput_summ)
        
      }
      
      
    }
    
    
    
    


########### Do some formatting of the lay-out ###########

     p <- p+ theme_light(base_size = 16)
    if (input$dark) {p <- p+ theme_darker(base_size = 16)}
    

     # if log-scale checked specified
     if (input$scale_log_10)
       p <- p + scale_y_log10() 
     
    #Adjust scale if range (min,max) is specified
    if (input$range != "" &&  input$change_scale == TRUE) {
         rng <- as.numeric(strsplit(input$range,",")[[1]])
         
         #If min>max invert the axis
         if (rng[1]>rng[2]) {p <- p+ scale_y_reverse()}
    
    #Autoscale if rangeis NOT specified
     } else if (input$range == "" || input$change_scale == FALSE) {
       rng <- c(NULL,NULL)
     }

     p <- p + coord_cartesian(ylim=c(rng[1],rng[2]))
      #### If selected, rotate plot 90 degrees CW ####
     if (input$rotate_plot == TRUE) { p <- p + coord_flip(ylim=c(rng[1],rng[2]))}
    
    # if title specified
    if (input$add_title)
      p <- p + ggtitle(input$title)
    
    # if labels specified
    if (input$label_axes)
      p <- p + labs(x = input$lab_x, y = input$lab_y)
    
    # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_ttl))
    }
     
    #remove legend (if selected)
    if (input$add_description == FALSE) {  
      p <- p + theme(legend.position="none")
    }

     #remove gridlines (if selected)
     if (input$no_grid == TRUE) {  
       p <- p+ theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
     }
     
          
   if (input$adjustcolors >1) {
       p <- p+ scale_color_manual(values=newColors)
       p <- p+ scale_fill_manual(values=newColors)
   }
    
     # if(input$tidyInput == TRUE && input$h_facet !="." || input$v_facet !=".") {
     #   p <- p + facet_grid(reformulate(input$h_facet,input$v_facet))
     # }
     
     
    ### Output the plot ######
    return(p)
    
  }) #close plotdata


##### Render the plot ############

output$coolplot <- renderPlot(width = width, height = height, {
  plot(plotdata())
}
)


#### Export the data in tidy format ###########

output$downloadData <- downloadHandler(
  filename = function() {
    paste("PlotsOfData_Tidy", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(df_selected(), file, row.names = FALSE)
  }
)


#### Combine the statistics in one table and filter ###########

df_filtered_stats <- reactive({

  digits <- as.numeric(input$digits)
  
  #Combine the numbers from the 95% CI for the mean to show the interval
  klaas <- df_summary_mean() %>% mutate(mean_CI_lo = round(mean_CI_lo, digits), mean_CI_hi = round(mean_CI_hi, digits)) %>% unite("95CI mean", c("mean_CI_lo","mean_CI_hi"), sep=" - ")
  observe({ print((klaas)) }) 
  
  #Combine the numbers from the 95% CI for the median to show the interval
  koos <- df_summary_median() %>% mutate(median_CI_lo = round(median_CI_lo, digits), median_CI_hi = round(median_CI_hi, digits)) %>% unite("95CI median", c("median_CI_lo","median_CI_hi"), sep=" - ")
    
  klaas  <- full_join(klaas, koos,by="Condition")

    # Round down to the number of selected digits
    klaas <- klaas %>% mutate_at(c(3:5, 7:11), round, input$digits)

  ##### Show the statistics selected by the user ############  
    
  if (!is.null(input$stats_select)) {
    columns = input$stats_select
    columns <- c("Condition", "n", columns)
    df <- klaas %>% select(one_of(columns))
  } else if (is.null(input$stats_select)) {
    df <- klaas %>% select("Condition", "n")}
})


#### A predined selection of stats for the table  ###########

observeEvent(input$summaryInput, {
  if (input$summaryInput=="mean")  {
    updateSelectInput(session, "stats_select", selected = list("mean", "sd", "95CI mean"))
  }
  else if (input$summaryInput=="median")  {
    updateSelectInput(session, "stats_select", selected = list("median", "MAD", "95CI median"))
  }
  else if (input$summaryInput=="box")  {
    updateSelectInput(session, "stats_select", selected = list("median", "IQR", "95CI median"))
  }
  else if (input$summaryInput=="violin")  {
    updateSelectInput(session, "stats_select", selected = list("median", "95CI median"))
  }
  
})

observeEvent(input$select_all1, {
  updateSelectInput(session, "stats_select", selected = list("mean", "sd", "sem", "95CI mean","median", "MAD", "IQR", "Q1", "Q3", "95CI median"))
  })

observeEvent(input$deselect_all1, {
  updateSelectInput(session, "stats_select", selected = "")
})


#### Render the data summary as a table ###########

output$data_summary <- renderDataTable(
 datatable(
  df_filtered_stats(),
#  colnames = c(ID = 1),
  selection = 'none',
  extensions = c('Buttons', 'ColReorder'),
  options = list(dom = 'Bfrtip', pageLength = 100,
             buttons = c('copy', 'csv','excel', 'pdf'),
    editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ) 
  ) 
#   %>% formatRound(n, digits=0)
) 


###### Figure legend #########
Fig_legend <- renderText({
  

  
  if (input$rotate_plot == FALSE) {
    x <- "horizontal"
    y <- "vertical"
    }
  else if (input$rotate_plot == TRUE) {
    x <- "vertical"
    y <- "horizontal"
    }
  
  df_temp <- df_summary_mean()
  min_n <- min(df_temp$n)

  if (input$jitter_type == "quasirandom" || input$jitter_type == "random") { jitter <- c("jittered dots")}
  else if (input$jitter_type == "stripes") {jitter <- c("stripes")}
  else if (input$jitter_type == "none") {jitter <- ("dots")}
  
  if (input$summaryInput == "median" && input$add_CI == FALSE)  { stats <- paste(x," line indicating the median ")}
  else if (input$summaryInput == "mean" && input$add_CI == FALSE) {stats <- paste(x," line indicating the mean ")}
  else if (input$summaryInput == "box" && input$add_CI == FALSE && min_n>9) {stats <- paste("a boxplot, with the box indicating the IQR, the whiskers showing the range of values that are within 1.5*IQR and a ", x," line indicating the median ")}
  else if (input$summaryInput == "violin" && min_n>9) {stats <- paste("a violinplot reflecting the data distribution and an open circle indicating the median of the data ")}  
  
  
  else if (input$summaryInput == "median" && input$add_CI == TRUE)  { stats <- c("an open circle indicating the median ")}
  else if (input$summaryInput == "mean" && input$add_CI == TRUE) {stats <- c("an open circle indicating the mean ")}
  else if (input$summaryInput == "box" && input$add_CI == TRUE) {stats <- paste("a boxplot, with the box indicating the IQR, the whiskers showing the range of values that are within 1.5*IQR and a ", x," line indicating the median ")}

  if (input$add_CI == TRUE && min_n>9 && input$summaryInput != "box" && input$summaryInput != "mean") {stat_inf <- paste(" A ", y," bar indicates for each median the 95% confidence interval determined by bootstrapping. ")}
  else if (input$add_CI == TRUE && min_n>9 && input$summaryInput == "box") {stat_inf <- c("The notches represent for each median the 95% confidence interval (approximated by 1.58*IQR/sqrt(n)). ")}
  else if (input$add_CI == TRUE && min_n>9 && input$summaryInput == "mean") {stat_inf <- c(" A ", y," bar indicates for each mean the 95% confidence interval. ")}
  else {stat_inf <- NULL}
  
  
  Legend <- c('</br></br><h4>Figure description</h4>')
  
  #The width of the legend (style as a paragraph between <p></p>) is adjusted to the width of the plot
  
  Legend<-append(Legend, paste('<p style="width:',input$plot_width,'px;padding: 0px 15px 0px 40px">', sep=""))

  Legend <- NULL
  
  Legend<-append(Legend, paste("Graph that shows the data as ", jitter," (visibility: ", input$alphaInput, "). ", sep=""))
  
  #Concatenate the condition with n
  koos <- df_summary_mean() %>% unite(label_n, c(Condition, n), sep="=", remove = FALSE)
  label_n <- paste(koos$label_n,collapse=", ")
  Legend <-append(Legend, paste("The number of samples per condition is: " ,label_n, ". ",sep=""))

  Legend<-append(Legend, paste("The summary of the data is shown as ", stats," (visibility: ", input$alphaInput_summ, "). ", sep=""))
  
  Legend <-append(Legend, paste(stat_inf, sep=""))
  
  if (input$color_data ==TRUE || input$color_stats) {Legend<-append(Legend, "The color coding is indicated in the legend next to the plot. ")		}
  
  if (input$ordered =="median") {Legend<-append(Legend, "The data are ordered according to their median value. ")		}
  
  if (input$scale_log_10) {Legend<-append(Legend, "The values are plotted on a log10 scale. ")		}
  
  return(Legend)
  
  })

######## Figure Legend in HTML format #############

output$LegendText <- renderText({
  
  if (input$add_description == FALSE) {return(NULL)}

  HTML_Legend <- c('</br></br><h4>Figure description</h4>')
  
  #The width of the legend (style as a paragraph between <p></p>) is adjusted to the width of the plot
  
  HTML_Legend <-append(HTML_Legend, paste('<p style="width:',input$plot_width,'px;padding: 0px 15px 0px 40px">', sep=""))
  
  HTML_Legend <- append(HTML_Legend, Fig_legend())
  
  HTML_Legend <- append(HTML_Legend, paste("</p>"))

  })

      ########### Update count #########
      # Reactively update the client.
      output$count <- renderText({
        vals$count
      })



    
    # When a session ends, decrement the counter.
    session$onSessionEnded(function(){

      isolate(vals$count <- vals$count - 1)
      # End R-session when browser closed
#      stopApp()
    })
    

    

######## The End; close server ########################

} #close "server"

shinyApp(ui = ui, server = server)