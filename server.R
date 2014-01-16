library(shiny)
library(ggplot2)

# Define server logic for random distribution application
shinyServer(function(input, output) {
 
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output functions defined 
  # below then all use the value computed from this expression
  data <- reactive({
    set.seed(input$seed)
    dist <- switch(input$dist,
                   dbeta = rbeta(input$n, input$shape1, input$shape2, input$ncp),
                   dbinom = rbinom(input$n, input$size, input$prob),
                   dcauchy = rcauchy(input$n, input$location, input$scale),
                   dchisq = rchisq(input$n, input$df, input$ncp1),
                   dexp = rexp(input$n, input$rate),
                   df = rf(input$n, input$df1, input$df2, input$ncp2),
                   dgamma = rgamma(input$n, input$shape, scale=input$scale1),
                   dgeom = rgeom(input$n, input$prob1),
                   dhyper = rhyper(input$n, input$m, input$n_, input$k),
                   dlnorm = rlnorm(input$n, input$meanlog, input$sdlog),
                   dmultinom = rmultinom(input$n, input$size, input$prob),
                   dnbinom = rnbinom(input$n, input$size, input$prob, input$mu),
                   dnorm = rnorm(input$n, input$mean, input$sd),
                   dpois = rpois(input$n, input$lambda),
                   dt = rt(input$n, input$df, input$ncp),
                   dunif = runif(input$n, input$min, input$max),
                   dweibull = rweibull(input$n, input$shape, input$scale),
                   dsignrank = rsignrank(input$n, input$n_),
                   dwilcox = rwilcox(input$n, input$m, input$n_)
                   )
    
  })
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    #hist(data(), main=paste(dist, '(', n, ')', sep=''))
    #print(qplot(data(), geom="histogram", main=dist) + geom_density())
    d <- as.data.frame(data())
    colnames(d) <- "x"
    m <- ggplot(d, aes(x))
    print(m + geom_histogram(aes(y=..density..)) + geom_density(color="blue"))
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    list("summary"=summary(data()),
      "var"=var(data()),
      "sd"=sd(data()),
      "se"=sd(data())/sqrt(input$n),
      "percent inside one sd" = 100*sum(mean(data()) - sd(data()) < data() & data() <  mean(data()) + sd(data()))/input$n,
     "percent inside two sd" = 100*sum(mean(data()) - 2*sd(data()) < data() & data() <  mean(data()) + 2*sd(data()))/input$n
    )
  })
  
  # Generate an HTML table view of the data
  #output$table <- renderTable({
  #  data.frame(x=data())
  #})
  
})
