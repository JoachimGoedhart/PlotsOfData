##############################################################################
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
##############################################################################
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
##############################################################################

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(ggbeeswarm)
library(readxl)
library(DT)

#Uncomment for sinaplot
#library(ggforce)

################

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


#Several qualitative color palettes that are colorblind friendly
#From Paul Tol: https://personal.sron.nl/~pault/
#Code to generate vectors in R to use these palettes

#Red, Green, Blue, yellow, cyan, purple, grey
Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')
Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#999933', '#DDCC77', '#CC6677', '#882255', '#AA4499', '#332288', '#DDDDDD')
Tol_light <- c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD')


#Read a text file (comma separated values)
df_wide_example <- read.csv("Data_wide_example.csv", na.strings = "")
df_tidy_example <- read.csv("Data_tidy_example.csv", na.strings = "")

 #######################################
###### Define the User interface #########

ui <- fluidPage(
  titlePanel("PlotsOfData - Plots all Of the Data"),
  sidebarLayout(
    sidebarPanel(width=3,
      conditionalPanel(
        condition = "input.tabs=='Plot'",
        radioButtons("jitter_type", "Data offset", choices = list("Quasirandom" = "beeswarm", 
#Uncomment for sinaplot                                           "Sinaplot" = "sina", 
                                                                  "Random" = "random", 
                                                                  "None (for small n)" = "none"), selected = "beeswarm"),
        
        
        
        sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.3),

        # conditionalPanel(
        #   condition = "input.adjust_jitter == true",
        #   sliderInput("jitter_width", "Width:", 0,0.5,0.3),
        #   checkboxInput(inputId = "random_jitter", label = ("Randomize Jitter"), value = TRUE)
        # ),
          
        radioButtons("summaryInput", "Statistics", choices = list("Median" = "median", "Mean" = "mean", "Boxplot (minimal n=10)" = "boxplot", "Violin Plot (minimal n=10)" = "violin"), selected = "median"),
#        sliderInput("Input_CI", "Confidence Level", 90, 100, 95),
        checkboxInput(inputId = "add_CI", label = HTML("Add 95% CI <br/> (minimal n=10)"), value = FALSE),
        sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),

        radioButtons(inputId = "ordered",
             label= "Order of the data/statistics:",
             choices = list("As supplied" = "none", "By median value" = "median", "By alphabet/number" = "alphabet"),
             selected = "none"),
        h4("Plot Layout"),      

        checkboxInput(inputId = "rotate_plot",
              label = "Rotate plot 90 degrees",
              value = FALSE),

        checkboxInput(inputId = "no_grid",
                      label = "Remove gridlines",
                      value = FALSE),

        checkboxInput(inputId = "adjust_scale",
                      label = "Adjust scale",
                      value = FALSE),
        conditionalPanel(
          condition = "input.adjust_scale == true",
          textInput("range", "Range of values (min,max)", value = "0,2")),
        
                checkboxInput("color_data", "Use color for the data", value=FALSE),
        checkboxInput("color_stats", "Use color for the stats", value=FALSE),

        conditionalPanel(
            condition = "input.color_data == true || input.color_stats == true",
            ########## Choose color from list
            selectInput("colour_list", "Colour:", choices = ""),

          radioButtons("adjustcolors", "Color palette:", choices = list("Standard" = 1,"Colorblind safe (bright)" = 2,"Colorblind safe (muted)" = 3,"Colorblind safe (light)" = 4, "User defined"=5) , selected =  1),
              conditionalPanel(condition = "input.adjustcolors == 5",
                 textInput("user_color_list", "List of names or hexadecimal codes", value = "turquoise2,#FF2222,lawngreen"), 
                 
                 h5("",
                    a("Click here for more info on color names",
                      href = "http://www.endmemo.com/program/R/color.php", target="_blank"))
                 
        )),

        numericInput("plot_height", "Height (# pixels): ", value = 480),
        numericInput("plot_width", "Width (# pixels):", value = 480),

        h4("Labels"),

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
        conditionalPanel(
          condition = "input.color_data == true || input.color_stats == true",
          checkboxInput(inputId = "add_legend",
                        label = "Add legend",
                        value = FALSE))

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
                 "Paste data" = 4)
          ,
          selected =  1),
        conditionalPanel(
          condition = "input.data_input=='1'"
          
        ),
        conditionalPanel(
          condition = "input.data_input=='3'",
          h5("Upload file: "),
          fileInput("upload", "", multiple = FALSE),
          selectInput("file_type", "Type of file:",
                      list("text (csv)" = "text",
                           "Excel" = "Excel"
                      ),
                      selected = "text"),
          conditionalPanel(
            condition = "input.file_type=='text'",

          radioButtons(
              "upload_delim", "Delimiter",
              choices = 
                list("Comma" = ",",
                     "Tab" = "\t",
                     "Semicolon" = ";",
                     "Space" = " "),
              selected = ",")),
          
          actionButton("submit_datafile_button",
                       "Submit datafile")),
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
        conditionalPanel(
          condition = "input.tidyInput==false",        selectInput("data_remove", "Select columns to remove", "", multiple = TRUE)),
        
        checkboxInput(inputId = "tidyInput",
                      label = "These data are Tidy",
                      value = FALSE),
        conditionalPanel(
          condition = "input.tidyInput==true",
          h5("",
             a("Click here for more info on tidy data",
               href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")),
          selectInput("x_var", "Conditions to compare:", choices = ""),
          selectInput("y_var", "Variables:", choices = ""),
          selectInput("h_facet", "Separate horizontal:", choices = ""),
          selectInput("v_facet", "Separate vertical:", choices = "")  
          ), 
        
        
        
        conditionalPanel(
          condition = "input.tidyInput==false", (downloadButton("downloadData", "Download in tidy format (csv)")))
      ),
      
      conditionalPanel(
        condition = "input.tabs=='About'",
        h4("About")    
      ),
      
      conditionalPanel(
        condition = "input.tabs=='Data Summary'",
        h4("Data summary") ,
        checkboxGroupInput("stats_select", label = h5("Statistics for table:"), 
                           choices = list("mean", "sd", "sem","95CI mean", "median", "MAD","IQR", "95CI median"),
                           selected = "sem"),
        actionButton('select_all1','select all'),
        actionButton('deselect_all1','deselect all'),
        numericInput("digits", "Digits:", 2, min = 0, max = 5)
#        ,
#        selectInput("stats_hide2", "Select columns to hide", "", multiple = TRUE, choices=list("mean", "sd", "sem","95CI mean", "median", "MAD","IQR", "95CI median")
      )   
      
        
    ),
    mainPanel(
 
       tabsetPanel(id="tabs",
                  tabPanel("Data upload", h4("Data as provided"),
                  dataTableOutput("data_uploaded")),
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"), downloadButton("downloadPlotPNG", "Download png-file"), plotOutput("coolplot")
                  ), 
                  tabPanel("Data Summary", dataTableOutput('data_summary')
                           ),
                  tabPanel("About", includeHTML("about.html")
                           )
                  
      )
    )
  )         
)

 #######################################

server <- function(input, output, session) {

  #####################################
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
        return(data.frame(x = "Select your datafile"))
      } else if (input$submit_datafile_button == 0) {
        return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        isolate({
          if (input$file_type == "text") {
            data <- read_delim(file_in$datapath,
                               delim = input$upload_delim,
                               col_names = TRUE)
          } else if (input$file_type == "Excel") {
            data <- read_excel(file_in$datapath)
          } 
        })
      }
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
    return(data)
})
  
  
df_filtered <- reactive({     
  
  if (!is.null(input$data_remove)) {
    columns = input$data_remove
    df <- df_upload() %>% select(-one_of(columns))
  } else if (is.null(input$data_remove)) {
  df <- df_upload()}
  
})

 #####################################
  
 ####################################
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
      #Convert the integers to factors, to enable adding discrete colors
      #klaas <- df_upload() %>% mutate_if(is.integer, factor)
      klaas <- df_upload()
    }
  return(klaas)
})
 ###################################


 ####################################
##### Get the Variables ##############

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

        updateSelectInput(session, "colour_list", choices = nms_fact)
        updateSelectInput(session, "y_var", choices = vary_list)
        updateSelectInput(session, "x_var", choices = varx_list)
        updateSelectInput(session, "h_facet", choices = facet_list)
        updateSelectInput(session, "v_facet", choices = facet_list)

    })
 ###################################    

###########################################################  
######## Determine and set the order of the Conditions #######  
df_sorted <- reactive({
  
#  klaas <- df_upload_tidy()
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

########################################################### 


 ###########################################################  
######## Extract the data for display & summary stats #######  

df_selected <- reactive({
    if(input$tidyInput == TRUE ) {
    df_temp <- df_upload_tidy() 
    x_choice <- input$x_var
    y_choice <- input$y_var
 #   kleur_choice <- input$colour_list
    

    koos <- df_temp %>% select(Condition = !!x_choice , Value = !!y_choice) %>% filter(!is.na(Value))
#    koos$Kleur <- as.factor(koos$Kleur)

    } else if (input$tidyInput == FALSE ) {
      koos <- df_upload_tidy() %>% filter(!is.na(Value))
    }

    return(koos)
})
 ###########################################################  

  
 #############################################################
#### DISPLAY UPLOADED DATA (exactly as provided) ##################


output$data_uploaded <- renderDataTable(

#    observe({ print(input$tidyInput) })
  df_filtered(),
  rownames = FALSE,
  options = list(pageLength = 100, autoWidth = FALSE,
                  lengthMenu = c(10, 100, 1000, 10000)),
  editable = FALSE,selection = 'none'
)
  
  
 #############################################################


 ##################################################
#### Caluclate Summary of the DATA for the MEAN ####

df_summary_mean <- reactive({
  koos <- df_selected()

  koos %>%
    group_by(Condition) %>% 
    summarise(n = n(),
            mean = mean(Value, na.rm = TRUE),
#            median = median(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE)) %>%
  mutate(sem = sd / sqrt(n - 1),
         mean_CI_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
         mean_CI_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem)
  
#  observe({ print(koos) })
  
  })

 #################################################

 ####################################################
#### Caluclate Summary of the DATA for the Median ####

df_summary_median <- reactive({
    
    kees <- df_selected()
 
 #   df_booted <- data.frame(Condition=levels(factor(kees$Condition)), n=tapply(kees$Value, kees$Condition, length), median=tapply(kees$Value, kees$Condition, median))
    df_booted <- kees %>%
                  group_by(Condition) %>%
                    summarise(
#                            n= n(),
                         median= median(Value, na.rm = TRUE),
                            MAD= mad(Value, na.rm = TRUE, constant=1),
                            IQR= IQR(Value, na.rm = TRUE))
    
    
    
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
 ###################################################
  

 ###########################################
######### DEFINE DOWNLOAD BUTTONS ###########

##### Set width and height of the plot area
width <- reactive ({ input$plot_width })
height <- reactive ({ input$plot_height })

output$downloadPlotPDF <- downloadHandler(
  filename <- function() {
    paste("PlotsOfData_", Sys.time(), ".pdf", sep = "")
#    paste("PlotsOfData.pdf")
  },
  content <- function(file) {
    pdf(file, width = input$myWidth/72, height = input$myHeight/72)
    ## ---------------
    plot(plotdata())
    ## ---------------
    dev.off()
    # ggsave(file, width = input$plot_width/72,
    #        height = input$plot_height/72, dpi="retina")
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("PlotsOfData_", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    png(file, width = input$plot_width*4, height = input$plot_height*4, res=300)
    ## ---------------
    plot(plotdata())
    ## ---------------
    dev.off()
    
    
    # ggsave(file, width = input$plot_width/72,
    #        height = input$plot_height/72)
  },
  contentType = "application/png" # MIME type of the image
)

 ###########################################


 ###########################################
######## PREPARE PLOT FOR DISPLAY ##########
 ###########################################

plotdata <- reactive({

  
####### Read the order from the ordered dataframe #############  
    koos <- df_sorted()
    custom_order <-  levels(factor(koos$Condition))
    
#    observe({ print(custom_order) })

    


  
  ########## Define alternative color palettes ##########
  
  newColors <- NULL
  
  if (input$adjustcolors == 2) {
    newColors <- Tol_bright
  } else if (input$adjustcolors == 3) {
    newColors <- Tol_muted
  } else if (input$adjustcolors == 4) {
    newColors <- Tol_light
  } else if (input$adjustcolors == 5) {
    newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
  }

########## Set default to Plotting "Condition" and "Value"
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

  ########## Define if color is used for the data
  #    observe({ print(class(input$colour_list)) })
  if (input$color_data == FALSE) {
    kleur <- NULL
#    observe({ print("Kleur <- NULL") })
  } else if (input$color_data == TRUE) {
        kleur <- as.character(input$colour_list)
  }
  

    ######## The df_upload_tidy is used for defining colors, needed for compatibility with tidy data and for coloring factors
    klaas <- df_upload_tidy() 
    klaas <- as.data.frame(klaas)

    
    if (input$color_data == TRUE || input$color_stats == TRUE) {    
    #### Used to convert integers to factors, compatible with a discrete color scale
    klaas[,kleur] <- as.factor(klaas[,kleur])

    #Determine the number of colors that are necessary
    max_colors <- nlevels(as.factor(klaas[,kleur]))
    
    
        #If unsufficient colors available, repeat
        if(length(newColors) < max_colors) {
          newColors<-rep(newColors,times=(round(max_colors/length(newColors)))+1)
        }
    
    
    }
  ########## Define if/how color is used for the stats ############
  #    observe({ print(class(input$colour_list)) })
  if (input$color_stats == FALSE) {
    kleur_stats <- NULL
  } else if (input$color_stats == TRUE && input$summaryInput == "boxplot") {
    kleur_stats <- x_choice
  } else if (input$color_stats == TRUE && input$summaryInput == "violin") {
    kleur_stats <- x_choice
  } else if (input$color_stats == TRUE) {
    kleur_stats <- "Condition"
  }  
    
  
  ########## Define minimal n - only plot box/violinplots for min_n>9
    df_temp <- df_summary_mean()
    min_n <- min(df_temp$n)

 ###############################################
############## GENERATE PLOT LAYERS #############

    p <- ggplot(data=df_selected(), aes_string(x="Condition")) 
    
    # Setting the order of the x-axis
    p <- p + scale_x_discrete(limits=custom_order)

  ##### plot selected data summary (bottom layer) ####
    if (input$summaryInput == "boxplot" && min_n>9) {
      p <- p + geom_boxplot(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, fill=kleur_stats), notch = input$add_CI, outlier.color=NA, width=0.8, size=0.5, alpha=input$alphaInput_summ)
  
    } else if (input$summaryInput == "violin" && min_n>9) {
      p <- p + geom_violin(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, fill=kleur_stats),scale = "width", draw_quantiles = c(0.5), width=0.8, size=0.5, alpha=input$alphaInput_summ) 
      if (input$add_CI == TRUE) {
        p <- p + geom_linerange(data=df_summary_median(), aes_string(x="Condition", ymin = "median_CI_lo", ymax = "median_CI_hi"), colour="black", size =3,alpha=input$alphaInput_summ)
       
      }
    }

   #### plot individual measurements (middle layer) ####
    if (input$jitter_type == "beeswarm") {
      p <- p + geom_quasirandom(data=klaas, aes_string(x=x_choice, y=y_choice, colour = kleur), varwidth = TRUE, cex=3, alpha=input$alphaInput)

#Uncomment for sinaplot    } else if (input$jitter_type == "sina") {
#Uncomment for sinaplot p <- p + geom_sina(data=klaas, aes_string(x=x_choice, y=y_choice, colour = kleur), method="density", maxwidth = .8, cex=3, alpha=input$alphaInput)
      
      
    } else if (input$jitter_type == "random") {
      p <- p + geom_jitter(data=klaas, aes_string(x=x_choice, y=y_choice, colour = kleur), width=0.3, height=0.0, cex=3, alpha=input$alphaInput)
    } else if (input$jitter_type == "none") {
      p <- p + geom_jitter(data=klaas, aes_string(x=x_choice, y=y_choice, colour = kleur), width=0, height=0.0, cex=3, alpha=input$alphaInput)
    }
    
  ##### plot selected data summary (top layer) ####
    if (input$summaryInput == "median"  && input$add_CI == TRUE && min_n>9) {
    p <-  p + geom_point(data=df_summary_median(), aes_string(x="Condition", y = "median", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)+
              geom_linerange(data=df_summary_median(), aes_string(x="Condition", ymin = "median_CI_lo", ymax = "median_CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)
    }

    else if (input$summaryInput == "median"  && input$add_CI == FALSE || min_n<10) {
      p <-  p + geom_errorbar(data=df_summary_median(), aes_string(x="Condition", ymin="median", ymax="median", colour = kleur_stats), width=.8, size=2, alpha=input$alphaInput_summ)

    } else if (input$summaryInput == "mean"  && input$add_CI == TRUE && min_n>9) {
      p <- p + geom_linerange(data=df_summary_mean(), aes_string(x="Condition", ymin = "mean_CI_lo", ymax = "mean_CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)+
        geom_point(data=df_summary_mean(), aes_string(x="Condition", y = "mean", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)

    } else if (input$summaryInput == "mean"  && input$add_CI == FALSE || min_n<10) {
      p <- p + geom_errorbar(data=df_summary_mean(), aes_string(x="Condition", ymin="mean", ymax="mean", colour=kleur_stats), width=.8, size=2, alpha=input$alphaInput_summ)
    }  

########### Do some formatting of the lay-out

     p <- p+ theme_light(base_size = 16)
    
    #### If selected, rotate plot 90 degrees CW ####
     rng <- as.numeric(strsplit(input$range,",")[[1]])
 
     # if the range of values is specified    
     if (input$adjust_scale == TRUE) { 
       p <- p + coord_cartesian(ylim=c(rng[1],rng[2]))
     } else if (input$adjust_scale == FALSE)
     {
       rng <- c(NULL,NULL)
     }
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
    if (input$add_legend == FALSE) {  
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
    
     if(input$tidyInput == TRUE && input$h_facet !="." || input$v_facet !=".") {
#       x <- as.character(input$h_facet)
       p <- p + facet_grid(reformulate(input$h_facet,input$v_facet))
     }
     
     
    ### Output the plot ######
    return(p)
    
  }) #close plotdata



 ########################################
##### Make actual plot ############

output$coolplot <- renderPlot(width = width, height = height, {
  plot(plotdata())
}
)
##########################################

###########################################
#### Export the data in tidy format ###########

output$downloadData <- downloadHandler(
  filename = function() {
    paste("PlotsOfData_Tidy", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(df_selected(), file, row.names = FALSE)
  }
)


###########################################


 ###########################################################
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
    klaas <- klaas %>% mutate_at(c(3:5, 7:9), round, input$digits)

  ##### Show the statistics selected by the user ############  
    
  if (!is.null(input$stats_select)) {
    columns = input$stats_select
    columns <- c("Condition", "n", columns)
    df <- klaas %>% select(one_of(columns))
  } else if (is.null(input$stats_select)) {
    df <- klaas %>% select("Condition", "n")}
})
 ###########################################

 ###########################################################
#### A predined selection of stats for the table  ###########

observeEvent(input$summaryInput, {
  if (input$summaryInput=="mean")  {
    updateSelectInput(session, "stats_select", selected = list("mean", "sd", "95CI mean"))
  }
  else if (input$summaryInput=="median")  {
    updateSelectInput(session, "stats_select", selected = list("median", "MAD", "95CI median"))
  }
  else if (input$summaryInput=="boxplot")  {
    updateSelectInput(session, "stats_select", selected = list("median", "IQR", "95CI median"))
  }
  else if (input$summaryInput=="violin")  {
    updateSelectInput(session, "stats_select", selected = list("median", "95CI median"))
  }
  
})

observeEvent(input$select_all1, {
  updateSelectInput(session, "stats_select", selected = list("mean", "sd", "sem", "95CI mean","median", "MAD", "IQR", "95CI median"))
  })

observeEvent(input$deselect_all1, {
  updateSelectInput(session, "stats_select", selected = "")
})


 ###########################################


###########################################
#### Render the data summary as a table ###########


output$data_summary <- renderDataTable(
 datatable(
  df_filtered_stats(),
#  colnames = c(ID = 1),
  selection = 'none',
  extensions = c('Buttons', 'ColReorder'),
  options = list(dom = 'Bfrtip',
             buttons = c('copy', 'csv', 'pdf'),
    editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ) 
  ) 
#   %>% formatRound(n, digits=0)
) 

###########################################

} #close "server"

shinyApp(ui = ui, server = server)