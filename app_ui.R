##################################
###### USER INTERFACE SCRIPT #####
##################################

# Allows all 5 pages to be part of the user interface
ui <- navbarPage(
  "Tennis Analyses Project",
  tabPanel(
    titlePanel("Intro"),
    p("remember to cite data source")
  ),
  tabPanel(
    titlePanel("Player Statistics"),
    sidebarLayout(
      sidebarPanel(
        p("Player Statistics")
      ),
      mainPanel(
        tableOutput(
          outputId = "winpcttable"
        )
      )
    )
  ),
  tabPanel(
    titlePanel("Court Conditions"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "surface_type",
          label = "Please select a surface type",
          choices = c("Carpet", "Clay", "Grass", "Hard")
        )
      ),
      mainPanel(
        tableOutput(
          outputId = "surfacetable"
        )
      )
    )
    
  ),
  tabPanel(
    titlePanel("Impact of Shots"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "shot_type",
          label = "Type of Tennis Shot",
          choices = c("ace", "1stIn", "svpt")
        )
      ),
      mainPanel(
        plotOutput(
          outputId = "impactplots",
        ),
        tableOutput(
          outputId = "impacttable"
        )
        # plotOutput(
        #   outputId = "aceplot"
        # ),
        # plotOutput(
        #   outputId = "fsiplot"
        # )
      )
    )
  ),
  tabPanel(
    titlePanel("Logistic Regression"),
    tableOutput(
    outputId = "log_reg_summary"
    ),
    p("Note: may want to make intercept default to 0 as that makes more sense"),
    p("Here are the logistic regression values for some of the most common types of tennis shots. Here, we see
      that the largest coefficient value of 0.09 belongs to aces, whereas the most negative coefficient value of 
      -0.13 belongs to double faults. Since aces and double faults result in the instant win or loss of
      the point respectively, it makes sense that these two shot types would have the largest impacts in determining
      the outcome of the match. On the other hand......")
  )
)
