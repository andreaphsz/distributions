library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
    
  # Application title
  headerPanel(HTML("Distributions in the stats package [<a href='http://stat.ethz.ch/R-manual/R-patched/library/stats/html/Distributions.html' target=_blank>R-manual</a>]"), "Distributions in the stats package"),
  #helpText("http://stat.ethz.ch/R-manual/R-patched/library/stats/html/Distributions.html"), 
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    selectInput("dist", "Choose a distribution:",
                    list("Beta" = "dbeta", 
                         "Binomial" = "dbinom", 
                         "Cauchy" = "dcauchy",
                         "Chisquare" = "dchisq",
                         "Exponential" = "dexp",
                         "FDist" = "df",
                         "GammaDist" = "dgamma",
                         "Geometric" = "dgeom",
                         "Hypergeometric" = "dhyper",
                         "Lognormal" = "dlnorm",
                         #"Multinom" = "dmultinom",
                         "NegBinomial" = "dnbinom",
                         "Normal" = "dnorm",
                         "Poisson" = "dpois",
                         "TDist" = "dt",
                         "Uniform" = "dunif",
                         "Weibull" = "dweibull",
                         "SignRank" = "dsignrank",
                         "Wilcoxon" = "dwilcox"
                         )
               ),
    br(),
   
    checkboxInput("fixxbool", "Fix x-axis?", value = FALSE),
    conditionalPanel(
      condition = "input.fixxbool == true",
      sliderInput("fixx", "", value=c(-2,2), min=-10, max=10, step=0.5)
    ),

    sliderInput("seed", "Set seed", value=77, min=7, max=777, step=7),

    sliderInput("n", "n: number of observations", value = 50, min = 10, max = 1000, step=10),

    conditionalPanel(
      condition = "input.dist == 'dbeta'",
        sliderInput("shape1", "a: positive parameter of the Beta distribution", value=5, min=0.1, max=10, step=0.1),
        sliderInput("shape2", "b: positive parameter of the Beta distribution", value=5, min=0.1, max=10, step=0.1),
        sliderInput("ncp", "ncp: non-centrality parameter", value=0, min=0, max=10)
    ),
    conditionalPanel(
      condition = "input.dist == 'dbinom'",
        sliderInput("size", "size: number of trials (zero or more)", value=5, min=0, max=100),
        sliderInput("prob", "prob: probability of success on each trial", value=0.5, min=0, max=1, step=0.05)
    ),
    conditionalPanel(
      condition = "input.dist == 'dcauchy'",
        sliderInput("location", "location: location parameter", value=0, min=0, max=10, step=0.1),
        sliderInput("scale", "scale: scale parameter", value=1, min=0, max=2, step=0.1)
    ),
    conditionalPanel(
      condition = "input.dist == 'dchisq'",
        sliderInput("df", "df: degrees of freedom (non-negative, but can be non-integer)", value=5, min=0.1, max=10, step=0.1),
        sliderInput("ncp1", "ncp: non-centrality parameter (non-negative)", value=1, min=0, max=10, step=0.1)
    ),
    conditionalPanel(
      condition = "input.dist == 'dexp'",
        sliderInput("rate", "rate: [vector of] rates", value=1, min=0.1, max=10, step=0.1)
    ),
    conditionalPanel(
      condition = "input.dist == 'df'",
        sliderInput("df1", "df1: degrees of freedom", value=5, min=0.1, max=10, step=0.1),
        sliderInput("df2", "df2: degrees of freedom", value=5, min=0.1, max=10, step=0.1),
        sliderInput("ncp2", "ncp: non-centrality parameter", value=0, min=0, max=10)
    ),
    conditionalPanel(
      condition = "input.dist == 'dgamma'",
        sliderInput("shape", "shape: shape parameter, must be positive", value=1, min=0, max=10, step=0.1),
        sliderInput("scale1", "scale: scale parameter, must be strictly positive", value=1, min=0.1, max=10, step=0.1)
    ),
    conditionalPanel(
      condition = "input.dist == 'dgeom'",
        sliderInput("prob1", "prob: probability of success in each trial", value=0.5, min=0.1, max=1, step=0.05)
    ),
    conditionalPanel(
      condition = "input.dist == 'dhyper'",
        sliderInput("m", "m: the number of white balls in the urn", value=20, min=0, max=50),
        sliderInput("n_", "n: the number of black balls in the urn", value=10, min=0, max=50),
        uiOutput("slieder3")
    ),
    conditionalPanel(
      condition = "input.dist == 'dlnorm'",
        sliderInput("meanlog", "meanlog: mean of the distribution on the log scale", value=0, min=-3, max=3, step=0.05),
        sliderInput("sdlog", "sdlog: standard deviation of the distribution on the log scale", value=1, min=0, max=3, step=0.05)
    ),
    conditionalPanel(
      condition = "input.dist == 'dmultinom'",
        sliderInput("size2", "size", value=0, min=0, max=10),
        sliderInput("prob2", "prob", value=0, min=0, max=1)
    ),
    conditionalPanel(
      condition = "input.dist == 'dnbinom'",
        sliderInput("size3", "size: target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution)", value=5, min=0.1, max=10, step=0.1),
        radioButtons("p_or_mu", "parametrization via prob or mean?", list("prob","mean"), selected = "prob")
    ),
    conditionalPanel(
      condition = "input.p_or_mu == 'prob' && input.dist == 'dnbinom'",
        sliderInput("prob3", "prob: probability of success in each trial", value=0.5, min=0.05, max=1, step=0.05, ticks = FALSE)
    ),
    conditionalPanel(
      condition = "input.p_or_mu == 'mean' && input.dist == 'dnbinom'",
        sliderInput("mu", "mu: alternative parametrization via mean", value=5, min=0, max=10, step=0.1, ticks = FALSE)
    ),
    conditionalPanel(
      condition = "input.dist == 'dnorm'",
        sliderInput("mean", "mean: vector of means", value=0, min=-5, max=5, step=0.1),
        sliderInput("sd", "sd: vector of standard deviations", value=1, min=0, max=10, step=0.1)
    ),
    conditionalPanel(
      condition = "input.dist == 'dpois'",
       sliderInput("lambda", "lambda: vector of (non-negative) means", value=1, min=0, max=10, step=0.1)
    ),
    conditionalPanel(
      condition = "input.dist == 'dt'",
        sliderInput("df3", "df: degrees of freedom (> 0, maybe non-integer)", value=5, min=0.1, max=10, step=0.1),
        sliderInput("ncp3", "ncp: non-centrality parameter delta", value=0, min=-5, max=5, step=0.1)
    ),
    conditionalPanel(
      condition = "input.dist == 'dunif'",
        sliderInput("minmax", "min, max: lower and upper limits of the distribution", value=c(0,1), min=-5, max=5, step=0.1)
    ),
    conditionalPanel(
      condition = "input.dist == 'dweibull'",
        sliderInput("shape4", "shape: shape parameter", value=5, min=0.1, max=10, step=0.1),
        sliderInput("scale4", "scale: scale parameter", value=1, min=0, max=10, step=0.1)
    ),
    conditionalPanel(
      condition = "input.dist == 'dsignrank'",
        sliderInput("n_1", "n: number of observations in the sample", value=10, min=0, max=100, step=1)
   ),
    conditionalPanel(
      condition = "input.dist == 'dwilcox'",
        sliderInput("m_2", "m: numbers of observations in the first sample", value=10, min=0, max=100),
        sliderInput("n_2", "n: numbers of observations in the second sample", value=10, min=0, max=100)
    )
 ),

  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")), 
      tabPanel("Summary", verbatimTextOutput("summary")) 
      #tabPanel("Table", tableOutput("table"))
    )
  )
))

