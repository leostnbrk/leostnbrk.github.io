# app.R
library(shiny)
library(igraph)
source("functions_sequence.R")   # your init_world(), run_step(), plot_world()

ui <- fluidPage(
  titlePanel("Opinion Dynamics ABM 1.0"),
  sidebarLayout(
    sidebarPanel(
      #### Network controls ####
      sliderInput("N", "Number of agents (N):",
                  min = 10, max = 500, value = 100, step = 5),
      selectInput("network", "Network topology:",
                  choices = c("Preferential Attachment" = "pa",
                              "Random, growing"   = "growing",
                              "Random, geometric"   = "grg"
                            )),
      conditionalPanel(
        "input.network == 'pa' || input.network == 'growing'",
        sliderInput("network_m", "Edges per new agent (m):",
                    min = 1, max = 10, value = 1, step = 1)
      ),
      conditionalPanel(
        "input.network == 'grg'",
        sliderInput("network_r", "Radius (r):",
                    min = 0, max = 1, value = 0.05, step = 0.01)
      ),
      #### Opinion space ####
      sliderInput("opinions_num", "Number of opinion dimensions:",
                  min = 1, max = 5, value = 1, step = 1),
      selectInput("opinions_type", "Opinion type:",
                  choices = c("Categorical" = "category",
                              "Interval (continuous)" = "interval")),
      selectInput("opinions_minmax", "Opinion Min/Max:",
                  choices = c("(0, 1)"  = "0,1",
                              "(-1, 1)" = "-1,1")),
      
      #### Influence model ####
      selectInput("influence", "Influence rule:",
                  choices = c("Bounded confidence" = "bounded_confidence",
                              "Linear decline"     = "linear_decline")),
      selectInput("metric", "Influence metric:",
                  choices = c("Dimension wise" = "dimension_wise",
                              "Taxicab"        = "taxicab",
                              "Euclidean"      = "euclidean")),
      sliderInput("mu", "μ (learning rate):",
                  min = 0, max = 1, value = 0.4, step = 0.1),
      conditionalPanel(
        "input.influence == 'bounded_confidence'",
        sliderInput("epsilon", "ε (confidence bound):",
                    min = 0, max = 1, value = 1, step = 0.01)
      ),
      
      #### Controls ####
      actionButton("reset", "Reset"),
      actionButton("step",  "Step"),
      actionButton("run",   "Start"),
      checkboxInput("fixed_seed", "Fixed seed", value = TRUE),
      
      #### Dynamic opinion selector ####
      conditionalPanel(
        "input.opinions_num > 1",
        sliderInput("plot_opinion", "Plot opinion:",
                    min = 1, max = 5, value = 1, step = 1)
      ),
      
      width = 3
    ),
    
    mainPanel(
      textOutput("status"),
      plotOutput("networkPlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  # 1) Seed only on reset (or when fixed_seed flips)
  seed_spec <- eventReactive(
    list(input$reset, input$fixed_seed),
    {
      if (input$fixed_seed) {
        12345
      } else {
        sample.int(1e6, 1)
      }
    },
    ignoreNULL = FALSE
  )
  
  # 2) Build the reactive model spec
  model <- reactive({
    bounds <- strsplit(input$opinions_minmax, ",")[[1]] |> as.numeric()
    list(
      seed             = seed_spec(),
      N                = input$N,
      network          = input$network,
      network_m        = input$network_m,
      network_r        = input$network_r,
      opinions_num     = input$opinions_num,
      opinions_type    = input$opinions_type,
      opinions_min     = bounds[1],
      opinions_max     = bounds[2],
      influence        = input$influence,
      influence_metric = input$metric,
      influence_mu     = input$mu,
      influence_epsilon= input$epsilon,
      plot_opinion     = input$plot_opinion
    )
  })
  
  # 2) Store world state, layout, run flag, and step counter
  vals <- reactiveValues(
    world   = NULL,
    layout  = NULL,
    step    = 0,
    run     = 0
  )
  
  # 3) Fully define your reset helper
  reset_world <- function() {
    spec <- model()
    vals$world   <- init_world(spec)
    vals$layout  <- layout_with_fr(vals$world$agents)
    vals$step    <- 1
    
    vals$run     <- 0
    vals$running <- FALSE
    
    updateActionButton(session, "run", label = "Start")
  }
  
  # 4) Wire the reset button to it
  observeEvent(
    list(input$reset, input$N, input$network_m, input$opinions_num),
    {
      reset_world()
    },
    ignoreInit = TRUE
  )
  

  # 5) Single‐step
  observeEvent(input$step, {
    req(vals$world)
    vals$world <- run_step(vals$world, model())
    vals$step  <- vals$step + 1
  })
  
  observe({
    req(input$run)
    
    isolate({
      if (vals$run != input$run) {
        vals$run <- input$run
        if (vals$running) {
          vals$running <- FALSE
          updateActionButton(session, "run", label = "Resume")
        } else {
          vals$running <- TRUE
          updateActionButton(session, "run", label = "Stop")
        }
      }
    })
    
    # 3) If running, do one step and schedule the next
    if (isolate(vals$running)) {
      Sys.sleep(0)                                                 
      isolate({
        vals$world <- run_step(vals$world, model())             
        vals$step  <- vals$step + 1
      })
      invalidateLater(200, session)                               
    }
  })

  # Render the graph at each shiny redraw
  output$networkPlot <- renderPlot({
    req(vals$world)
    plot_world(vals$world, layout = vals$layout, opinion = input$plot_opinion)
  }, res = 96)                                                  
  
  # Status text
  output$status <- renderText({
    paste("You're on iteration number:", vals$step)
  })
}

shinyApp(ui, server)
