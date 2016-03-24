library(shiny)

shinyUI(fluidPage(
  
  # suppresses spurious 
  # 'progress' error messages after all the debugging 
  # is done:
#tags$style(type="text/css",
#             ".shiny-output-error { visibility: hidden; }",
#             ".shiny-output-error:before { visibility: hidden; }"
#  ),
  HTML("<hr>"),
  # the main stuff:
  fluidPage( 
      column(
        width=3,
        plotOutput("naviplot",
                   click = clickOpts(id="plot_click"),
                   hover = hoverOpts(id="plot_hover", delayType="throttle")
        ),
        textOutput("coordinates")
     ),
    column(
        width=4,
        plotOutput("xentplot"),
        selectInput("ce_direction", 
                    label = "CE direction:",
                    choices = c("d(clicked, .)",
                                "d(., clicked)",
                                "average"),
                    selected = "d(clicked, .)")
     ),
    column(
      width=4,
      plotOutput("euclplot"),
      helpText("Clicked (both plots):"),
      textOutput("click_coords")
    )
  ),
    HTML("<hr>")
  )
)