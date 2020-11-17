theme_darker <- function(base_size = 11, base_family = "",  base_line_size = base_size/22, base_rect_size = base_size/22)
  {
  half_line <- base_size/2
  theme_light() %+replace%
    theme(line = element_line(colour = "grey100", size = 0.5, linetype = 1, lineend = "butt"),
          
          rect = element_rect(fill = "grey0", colour = "grey100", size = 0.5, linetype = 1),
          
          text = element_text(family = base_family, face = "plain", colour = "grey100", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
          
          axis.line = element_line(),
          axis.line.x = element_blank(), 
          axis.line.y = element_blank(),
          
          axis.text = element_text(size = rel(0.8), colour = "grey80"),
          # axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
          # axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1),
          
          axis.ticks = element_line(colour = "grey20"), 
          # axis.ticks.length = unit(half_line/2, "pt"),
          
          # axis.title.x = element_text(margin = margin(t = 0.8 * half_line, b = 0.8 * half_line/2)),
          # axis.title.y = element_text(angle = 90, margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2)),
          
          legend.background = element_rect(colour = NA),
          # legend.margin = unit(0.2, "cm"),
          legend.key = element_rect(fill = NULL, colour = "grey0"),
          # legend.key.size = unit(1.2, "lines"), 
          # legend.key.height = NULL, legend.key.width = NULL,
          # legend.text = element_text(size = rel(0.8)), 
          # legend.text.align = NULL,
          # legend.title = element_text(hjust = 0), 
          # legend.title.align = NULL,
          # legend.position = "right", 
          # legend.direction = NULL,
          # legend.justification = "center", 
          # legend.box = NULL,
          
          panel.background = element_rect(fill = "grey10", colour = NA),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "grey0"), 
          panel.grid.minor = element_line(colour = "grey0", size = 0.25), 
          # panel.margin = unit(half_line, "pt"),
          # panel.margin.x = NULL, 
          # panel.margin.y = NULL,
          panel.ontop = FALSE,
          
          strip.background = element_rect(fill = "grey40", colour = NA),
          strip.text = element_text(colour = "grey10", size = rel(0.8)),
          strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
          strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
          strip.switch.pad.grid = unit(0.1, "cm"),
          strip.switch.pad.wrap = unit(0.1, "cm"),
          
          #To avoid white line around plot panel
          plot.background = element_rect(colour = "black"), 
          # plot.title = element_text(size = rel(1.2), margin = margin(b = half_line * 1.2)),
          # plot.margin = margin(half_line, half_line, half_line, half_line),
          complete = TRUE)
  
}

theme_light_dark_bg <- function(base_size = 11, base_family = "",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22) {
  half_line <- base_size / 2
  
  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      plot.background = element_rect(colour = "black", fill="black"), 
      text = element_text(family = base_family, face = "plain", colour = "grey100", size = base_size,
                          lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE) ,
      axis.text = element_text(size = rel(0.8),colour = "grey80"),
      
      
      # axis.text = element_text(colour = "grey80"),
      
      # white panel with light grey border
      panel.background = element_rect(fill = "grey5", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey80", size = rel(1)),
      # light grey, thinner gridlines
      # => make them slightly darker to keep acceptable contrast
      panel.grid       = element_line(colour = "grey30"),
      panel.grid.major = element_line(size = rel(0.5)),
      panel.grid.minor = element_line(size = rel(0.25)),
      
      # match axes ticks thickness to gridlines and colour to panel border
      axis.ticks       = element_line(colour = "grey80", size = rel(0.5)),
      
      # match legend key to plot.background
      legend.background = element_rect(fill= "black", colour = NULL),
      legend.key       = element_rect(fill = "black", colour = NULL),
      
      # dark strips with light text (inverse contrast compared to theme_grey)
      strip.background = element_rect(fill = "grey40", colour = NA),
      strip.text       = element_text(
        colour = "grey10",
        size = rel(0.8),
        margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
      ),
      
      complete = TRUE
    )
  
}



############## https://debruine.github.io/posts/plot-comparison/

bgcolor <- "black"
textcolor <- "white"


theme_debruine <- function(base_size = 11, base_family = ""){
  half_line <- base_size/2
  theme_light() %+replace%

theme(line = element_line(colour = "grey100", size = 0.5, linetype = 1, lineend = "butt"),
      
      rect = element_rect(fill = "grey0", colour = "grey100", size = 0.5, linetype = 1),
  plot.background = element_rect(fill = bgcolor, colour = bgcolor),
  panel.background = element_rect(fill = NA),
  legend.background = element_rect(fill = NA),
  legend.position="none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  text = element_text(family=base_family, colour = textcolor, size=20),
  axis.text = element_text(family=base_family, colour = textcolor, size=15),
  complete = TRUE)
}



######## https://emanuelaf.github.io/own-ggplot-theme.html
blue_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "blue", fill = NA, linetype = 2),
    # color background 2)
    panel.background = element_rect(fill = "aliceblue"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "steelblue", face = "italic", family = "Times New Roman"),
    axis.title = element_text(colour = "steelblue", family = "Times New Roman"),
    axis.ticks = element_line(colour = "steelblue"),
    # legend at the bottom 6)
    legend.position = "bottom"
  )
}





####### Definition of the 'official' ggplot2 theme_grey


# theme_grey <- function (base_size = 11, base_family = "") 
# {
#   half_line <- base_size/2
#   theme(line = element_line(colour = "black", size = 0.5, linetype = 1, 
#                             lineend = "butt"), rect = element_rect(fill = "white", 
#                                                                    colour = "black", size = 0.5, linetype = 1), text = element_text(family = base_family, 
#                                                                                                                                     face = "plain", colour = "black", size = base_size, lineheight = 0.9, 
#                                                                                                                                     hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
#                                                                                                                                     debug = FALSE), axis.line = element_line(), axis.line.x = element_blank(), 
#         axis.line.y = element_blank(), axis.text = element_text(size = rel(0.8), 
#                                                                 colour = "grey30"), axis.text.x = element_text(margin = margin(t = 0.8 * 
#                                                                                                                                  half_line/2), vjust = 1), axis.text.y = element_text(margin = margin(r = 0.8 * 
#                                                                                                                                                                                                         half_line/2), hjust = 1), axis.ticks = element_line(colour = "grey20"), 
#         axis.ticks.length = unit(half_line/2, "pt"), axis.title.x = element_text(margin = margin(t = 0.8 * 
#                                                                                                    half_line, b = 0.8 * half_line/2)), axis.title.y = element_text(angle = 90, 
#                                                                                                                                                                    margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2)), 
#         legend.background = element_rect(colour = NA), legend.margin = unit(0.2, 
#                                                                             "cm"), legend.key = element_rect(fill = "grey95", 
#                                                                                                              colour = "white"), legend.key.size = unit(1.2, "lines"), 
#         legend.key.height = NULL, legend.key.width = NULL, legend.text = element_text(size = rel(0.8)), 
#         legend.text.align = NULL, legend.title = element_text(hjust = 0), 
#         legend.title.align = NULL, legend.position = "right", 
#         legend.direction = NULL, legend.justification = "center", 
#         legend.box = NULL, panel.background = element_rect(fill = "grey92", 
#                                                            colour = NA), panel.border = element_blank(), panel.grid.major = element_line(colour = "white"), 
#         panel.grid.minor = element_line(colour = "white", size = 0.25), 
#         panel.margin = unit(half_line, "pt"), panel.margin.x = NULL, 
#         panel.margin.y = NULL, panel.ontop = FALSE, strip.background = element_rect(fill = "grey85", 
#                                                                                     colour = NA), strip.text = element_text(colour = "grey10", 
#                                                                                                                             size = rel(0.8)), strip.text.x = element_text(margin = margin(t = half_line, 
#                                                                                                                                                                                           b = half_line)), strip.text.y = element_text(angle = -90, 
#                                                                                                                                                                                                                                        margin = margin(l = half_line, r = half_line)), strip.switch.pad.grid = unit(0.1, 
#                                                                                                                                                                                                                                                                                                                     "cm"), strip.switch.pad.wrap = unit(0.1, "cm"), plot.background = element_rect(colour = "white"), 
#         plot.title = element_text(size = rel(1.2), margin = margin(b = half_line * 
#                                                                      1.2)), plot.margin = margin(half_line, half_line, 
#                                                                                                  half_line, half_line), complete = TRUE)
# }


####### Definition of the ggplot2 theme_light
# 
# theme_light <- function(base_size = 11, base_family = "",
#                         base_line_size = base_size / 22,
#                         base_rect_size = base_size / 22) {
#   half_line <- base_size / 2
#   
#   # Starts with theme_grey and then modify some parts
#   theme_grey(
#     base_size = base_size,
#     base_family = base_family,
#     base_line_size = base_line_size,
#     base_rect_size = base_rect_size
#   ) %+replace%
#     theme(
#       # white panel with light grey border
#       panel.background = element_rect(fill = "white", colour = NA),
#       panel.border     = element_rect(fill = NA, colour = "grey70", size = rel(1)),
#       # light grey, thinner gridlines
#       # => make them slightly darker to keep acceptable contrast
#       panel.grid       = element_line(colour = "grey87"),
#       panel.grid.major = element_line(size = rel(0.5)),
#       panel.grid.minor = element_line(size = rel(0.25)),
#       
#       # match axes ticks thickness to gridlines and colour to panel border
#       axis.ticks       = element_line(colour = "grey70", size = rel(0.5)),
#       
#       # match legend key to panel.background
#       legend.key       = element_rect(fill = "white", colour = NA),
#       
#       # dark strips with light text (inverse contrast compared to theme_grey)
#       strip.background = element_rect(fill = "grey70", colour = NA),
#       strip.text       = element_text(
#         colour = "white",
#         size = rel(0.8),
#         margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
#       ),
#       
#       complete = TRUE
#     )
#   
# }
