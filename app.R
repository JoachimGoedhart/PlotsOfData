##############################################################################
# Shiny app for plotting and comparing the data
# Created by Joachim Goedhart (@joachimgoedhart), first version 2018
# Takes non-tidy, spreadsheet type data as input or tidy format
# Non-tidy data is converted into tidy format
# For tidy data the x and y variables need to be selected
# Raw data is displayed with user-defined visibility (alpha)
# Summary statistics are displayed with user-defined visibility (alpha)
# Inferential statistics (95%CI) can be added
# The 95%CI of the median is determined by resampling (bootstrap)
# A plot and a table with stats are generated
##############################################################################


library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(ggbeeswarm)

################


samplemedian <- function (x, d) {
  return(median(x[d]))}

i=0
#Number of bootstrap samples
nsteps=1000

#Confidence level
Confidence_Percentage = 95
Confidence_level = Confidence_Percentage/100

alpha=1-Confidence_level
lower_percentile=(1-Confidence_level)/2
upper_percentile=1-((1-Confidence_level)/2)


#Read a text file (comma separated values)
df_wide_example <- read.csv("Data_wide_example.csv", na.strings = "")
df_tidy_example <- read.csv("Data_tidy_example.csv", na.strings = "")

 #######################################
###### Define the User interface #########

ui <- fluidPage(
  titlePanel("ComPlotta - Comparison by Plotting the Data"),
  sidebarLayout(
    sidebarPanel(width=3,
      conditionalPanel(
        condition = "input.tabs=='Plot'",
        radioButtons("jitter_type", "Data offset", choices = list("Beeswarm" = "beeswarm", "Random" = "random", "None (for small n)" = "none"), selected = "beeswarm"),
        selectInput("colour_list", "Colour:", choices = ""),
        sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.3),

        # conditionalPanel(
        #   condition = "input.adjust_jitter == true",
        #   sliderInput("jitter_width", "Width:", 0,0.5,0.3),
        #   checkboxInput(inputId = "random_jitter", label = ("Randomize Jitter"), value = TRUE)
        # ),
          
        radioButtons("summaryInput", "Statistics", choices = list("Median" = "median", "Mean" = "mean", "Boxplot" = "boxplot", "Violin Plot" = "violin"), selected = "median"),
#        sliderInput("Input_CI", "Confidence Level", 90, 100, 95),
        checkboxInput(inputId = "add_CI", label = HTML("Add 95% CI <br/> (unadvisable for n<10)"), value = FALSE),
        sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),

  

        h4("Plot Layout"),      
        numericInput("plot_height", "Height (# pixels): ", value = 480),
        numericInput("plot_width", "Width (# pixels):", value = 480),

        checkboxInput(inputId = "adjust_scale",
              label = "Adjust scaling",
              value = FALSE),
        conditionalPanel(
              condition = "input.adjust_scale == true",
              textInput("range", "Range of values (min,max)", value = "0,2")
              
        ),
        checkboxInput(inputId = "ordered",
                      label = "Order data based on median value",
                      value = FALSE),
        checkboxInput(inputId = "rotate_plot",
              label = "Rotate plot 90 degrees",
              value = FALSE),
        
        conditionalPanel(
            condition = "input.colour_list != 'none'",
        checkboxInput(inputId = "add_legend",
              label = "Add legend",
              value = FALSE)),
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
              textInput("lab_y", "Y-axis:", value = "")
              
        ),


        checkboxInput(inputId = "adj_fnt_sz",
              label = "Change font size",
              value = FALSE)
        ),


        conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_ttl", "Size axis titles:", value = 24),
              numericInput("fnt_sz_ax", "Size axis labels:", value = 18)
 
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
          selected =  4),
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
        checkboxInput(inputId = "tidyInput",
                      label = "These data are Tidy",
                      value = FALSE),
        conditionalPanel(
          condition = "input.tidyInput==true",
          h5("",
             a("Click here for more info on tidy data",
               href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")),
          selectInput("x_var", "Conditions to compare:", choices = ""),
          selectInput("y_var", "Variables:", choices = "")
          
          )
      ),
      
      conditionalPanel(
        condition = "input.tabs=='About'",
        h4("About")    
      ),
      
      conditionalPanel(
        condition = "input.tabs=='Data Summary'",
        h4("Data summary")    
      )
      
      
      
    ),
    mainPanel(
 
       tabsetPanel(id="tabs",
                  tabPanel("Data upload", h4("Data as provided"), dataTableOutput("data_uploaded")),
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"), downloadButton("downloadPlotPNG", "Download png-file"), plotOutput("coolplot")
                  ), 
                  tabPanel("Data Summary", tableOutput('data_summary')),
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
    return(data)
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
      klaas <- gather(df_upload(), Condition, Value)
    }
    else if(input$tidyInput == TRUE ) {
      klaas <- df_upload()
    }
  
  
  
    })
 ###################################


 ####################################
##### Get the Variables ##############

observe({ 
        var_names  <- names(df_upload_tidy())
        var_list <- c("none", var_names)
        updateSelectInput(session, "colour_list", choices = var_list)
        updateSelectInput(session, "y_var", choices = var_list)
        updateSelectInput(session, "x_var", choices = var_list)})
 ###################################    


 ###########################################################  
######## Extract the data for display & summary stats #######  

df_selected <- reactive({
    if(input$tidyInput == TRUE ) {
    df_temp <- df_upload_tidy() 
    x_choice <- input$x_var
    y_choice <- input$y_var
    
    koos <- df_temp %>% select(Condition = !!x_choice , Value = !!y_choice) %>% filter(!is.na(Value))

    } else if (input$tidyInput == FALSE ) {
      koos <- df_upload_tidy() %>% filter(!is.na(Value))
    }
  
    #If reorder according to median values
    if(input$ordered == TRUE) {
      koos <- koos %>% mutate(Condition = reorder(Condition, Value, median))
    }
  
    observe({ print(head(koos)) })
  
    return(koos)
      
      
})
 ###########################################################  

  
 #############################################################
#### DISPLAY UPLOADED DATA (exactly as provided ##################

    
output$data_uploaded <- renderDataTable({
    
#    observe({ print(input$tidyInput) })
      df_upload()
  })
 #############################################################


 ##################################################
#### Caluclate Summary of the DATA for the MEAN ####

df_summary_mean <- reactive({
  koos <- df_selected()

  koos %>%
    group_by(Condition) %>% 
    summarise(n = n(),
            mean = mean(Value, na.rm = TRUE),
            median = median(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE)) %>%
  mutate(sem = sd / sqrt(n - 1),
         ci_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
         ci_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem)
  
  })

 #################################################

  
 ####################################################
#### Caluclate Summary of the DATA for the Median ####

df_summary_median <- reactive({
    
    kees <- df_selected()
 
    df_booted <- data.frame(Condition=levels(factor(kees$Condition)), n=tapply(kees$Value, kees$Condition, length), median=tapply(kees$Value, kees$Condition, median))

    # df_booted <- kees %>%  group_by(Condition) %>% 
    #             summarise(n = n(),
    #             median = median(Value, na.rm = TRUE))
        
    i=0
    df_new_medians <- data.frame(Condition=levels(factor(kees$Condition)), resampled_median=tapply(kees$Value, kees$Condition, boot_median))
    
    #Perform the resampling nsteps number of times (typically 1,000-10,000x)
    for (i in 1:nsteps) {
      
      #Caclulate the median from a boostrapped sample (resampled_median) and add to the dataframe
      df_boostrapped_median <- data.frame(Condition=levels(factor(kees$Condition)), resampled_median=tapply(kees$Value, kees$Condition, boot_median))
      
      #Add the new median to a datafram that collects all the resampled median values
      df_new_medians <- bind_rows(df_new_medians, df_boostrapped_median)
    }
    
    if(input$ordered == TRUE) {
    df_new_medians <- df_new_medians %>% mutate(Condition = reorder(Condition, resampled_median, median))
    }

    df_booted$ci_lo <- tapply(df_new_medians$resampled_median, df_new_medians$Condition, quantile, probs=lower_percentile)
    df_booted$ci_hi <- tapply(df_new_medians$resampled_median, df_new_medians$Condition, quantile, probs=upper_percentile)

#    observe({ print(df_booted) })

    return(df_booted)
  })
 ###################################################
  

##################################################
#### Caluclate Summary of the DATA for Box (&Violin) ####

df_summary_box <- reactive({
  df_selected() %>%
    group_by(Condition) %>% 
    summarise(n = n(),
              mean = mean(Value),
              median = median(Value),
              sd = sd(Value),
              IQR = IQR(Value))
})

#################################################


 ###########################################
######### DEFINE DOWNLOAD BUTTONS ###########

##### Set width and height of the plot area
width <- reactive ({ input$plot_width })
height <- reactive ({ input$plot_height })

output$downloadPlotPDF <- downloadHandler(
  filename <- function() {
    paste("ComparisonPlot_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    ggsave(file, width = input$plot_width/72,
           height = input$plot_height/72, dpi="retina")
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("ComparisonPlot_", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    ggsave(file, width = input$plot_width/72,
           height = input$plot_height/72)
  },
  contentType = "application/png" # MIME type of the image
)

 ###########################################


 ###########################################
######## PREPARE PLOT FOR DISPLAY ##########
 ###########################################

output$coolplot <- renderPlot(width = width, height = height, {
  

########## Define kleur
    observe({ print(class(input$colour_list)) })
    if (input$colour_list == "none") {
      kleur <- NULL
    } else if (input$colour_list != "none") {
      kleur <- as.character(input$colour_list)
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

 ###############################################
############## GENERATE PLOT LAYERS #############

    p <- ggplot(data=df_selected(), aes_string(x="Condition"))

    ### HACK TO GET RE-ORDERING ACCORDING TO MEDIAN RIGHT
    p <- p + geom_boxplot(data=df_selected(), aes(x=Condition, y=Value), alpha=0, fill=NA, size=0)
    
    observe({ print(x_choice) })
    observe({ print(y_choice) })    
    
    # if (input$adjust_jitter == FALSE || input$random_jitter == TRUE) {
    #   #Remove the seed
    #   rm(.Random.seed, envir=globalenv())
    # } else if (input$random_jitter == FALSE) {
    #   set.seed(2)
    # }

    #### plot individual measurements (1st layer) ####
    if (input$jitter_type == "beeswarm") {
      p <- p + geom_quasirandom(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, colour = kleur), varwidth = TRUE, cex=3, alpha=input$alphaInput)
    } else if (input$jitter_type == "random") {
      p <- p + geom_jitter(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, colour = kleur), width=0.3, height=0.0, cex=3, alpha=input$alphaInput)
    } else if (input$jitter_type == "none") {
      p <- p + geom_jitter(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, colour = kleur), width=0, height=0.0, cex=3, alpha=input$alphaInput)
    }
    
    ##### plot selected data summary (2nd layer) ####
    
    if (input$summaryInput == "median"  && input$add_CI == TRUE) {
    p <-  p + geom_point(data=df_summary_median(), aes(x=Condition, y = median), shape = 21,color = "black",fill=NA,size = 8,alpha=input$alphaInput_summ)+
              geom_linerange(data=df_summary_median(), aes(ymin = ci_lo, ymax = ci_hi), color="black", size =3,alpha=input$alphaInput_summ)
    }

    else if (input$summaryInput == "median"  && input$add_CI == FALSE) {
      p <-  p + geom_errorbar(data=df_summary_median(), aes(x=Condition, ymin=median, ymax=median), width=.8, size=2, alpha=input$alphaInput_summ)
      
    } else if (input$summaryInput == "mean"  && input$add_CI == TRUE) {
      p <- p + geom_linerange(data=df_summary_mean(), aes(ymin = ci_lo, ymax = ci_hi), color="black", size =3,alpha=input$alphaInput_summ)+
        geom_point(data=df_summary_mean(), aes(y = mean), shape = 21,color = "black",fill=NA,size = 8,alpha=input$alphaInput_summ)

    } else if (input$summaryInput == "mean"  && input$add_CI == FALSE) {
      p <- p + geom_errorbar(data=df_summary_mean(), aes(x=Condition, ymin=mean, ymax=mean), width=.8, size=2, alpha=input$alphaInput_summ)
      
    } else if (input$summaryInput == "boxplot") {
     p <- p + geom_errorbar(data=df_summary_mean(), aes(x=Condition, ymin=median, ymax=median), width=.2, size=2, alpha=0)+
       geom_boxplot(data=df_selected(), aes(x=Condition, y=Value), fill = "grey50", notch = input$add_CI, outlier.color=NA, width=0.8, size=0.5, alpha=input$alphaInput_summ)
      
    } else if (input$summaryInput == "violin") {
      p <- p + geom_errorbar(data=df_summary_mean(), aes(x=Condition, ymin=median, ymax=median), width=.2, size=2, alpha=0) +
        geom_violin(data=df_selected(), aes(x=Condition, y=Value), draw_quantiles = c(0.5), fill = "grey50", width=0.8, size=0.5, alpha=input$alphaInput_summ) 
    }

########### Do some formatting of the lay-out

     p <- p+ theme_light(base_size = 16)
    
    #### If selected, rotate plot 90 degrees CW ####
    if (input$rotate_plot == TRUE) { p <- p + coord_flip()}
    
    # if the range of values is specified    
    if (input$adjust_scale == TRUE) { 
      rng <- as.numeric(strsplit(input$range,",")[[1]])
      p <- p + ylim(rng[1],rng[2])}

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
    
    ### Output the plot ######
    p
    
  }) #close output$coolplot


###########################################
#### Render the data summary as a table ###########

output$data_summary <- renderTable({

  df_out <- NULL
  if (input$summaryInput == "mean") {
    df_out <- df_summary_mean()
    df_out$median <- NULL
  } else if (input$summaryInput == "median") {
    df_out <- df_summary_median()
  }  else if (input$summaryInput == "boxplot") {
    df_out <- df_summary_box()
  } else if (input$summaryInput == "violin") {
    df_out <- df_summary_box()
  } 
  return(df_out)
})
###########################################

} #close "server"

shinyApp(ui = ui, server = server)