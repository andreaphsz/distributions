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
                   dbeta = list(
                     r=r<-rbeta(input$n, input$shape1, input$shape2, input$ncp),
                     x=x<-seq(0,1,length.out=input$n),
                     d=dbeta(x, input$shape1, input$shape2, input$ncp)
                   ),
                   dbinom = list(
                     r=r<-rbinom(input$n, input$size, input$prob),
                     x=x<-round(seq(0,ceiling(max(r)),length.out=input$n)),
                     #x=x<-round(seq(0,input$size,length.out=input$n)),
                     d=dbinom(x, input$size, input$prob)
                   ),
                   dcauchy = list(
                     r=r<-rcauchy(input$n, input$location, input$scale),
                     x=x<-seq(ceiling(min(r)),ceiling(max(r)),length.out=input$n),
                     #x=x<-seq(-20*(input$location+1),20*(input$location+1),length.out=input$n),
                     d=dcauchy(x, input$location, input$scale)
                   ),
                   dchisq = list(
                     r=r<-rchisq(input$n, input$df, input$ncp1),
                     x=x<-seq(0,ceiling(max(r)),length.out=input$n),
                     #x=x<-seq(0,30+2*input$ncp1,length.out=input$n),
                     d=dchisq(x, input$df, input$ncp1)
                   ),
                   dexp = list(
                     r=r<-rexp(input$n, input$rate),
                     x=x<-seq(0,ceiling(max(r)),length.out=input$n),
                     #x=x<-seq(0,8/input$rate, length.out=input$n),
                     d=dexp(x, input$rate)
                   ),
                   df = list(
                     r=r<-rf(input$n, input$df1, input$df2, input$ncp2),
                     x=x<-seq(0,ceiling(max(r)),length.out=input$n),
                     d=df(x, input$df1, input$df2, input$ncp2)
                   ),
                   dgamma = list(
                     r=r<-rgamma(input$n, input$shape, scale=input$scale1),
                     x=x<-seq(0,ceiling(max(r)),length.out=input$n),
                     d=dgamma(x,input$shape, scale=input$scale1)
                   ),
                   dgeom = list(
                     r=r<-rgeom(input$n, input$prob1),
                     x=x<-round(seq(0,ceiling(max(r)),length.out=input$n)),
                     d=dgeom(x,input$prob1)
                   ),
                   dhyper = list(
                     r=r<-rhyper(input$n, input$m, input$n_, input$k),
                     x=x<-round(seq(0,ceiling(max(r)),length.out=input$n)),
                     d=dhyper(x, input$m, input$n_, input$k)
                   ),
                   dlnorm = list(
                     r=r<-rlnorm(input$n, input$meanlog, input$sdlog),
                     x=x<-seq(0,ceiling(max(r)),length.out=input$n),
                     d=dlnorm(x, input$meanlog, input$sdlog)
                   ),
                   #dmultinom = rmultinom(input$n, input$size, input$prob),
                   dnbinom = if(input$p_or_mu=="prob") {
                     list(
                       r=r<-rnbinom(input$n, input$size3, input$prob3),
                       x=x<-round(seq(0,ceiling(max(r)),length.out=input$n)),
                       d=dnbinom(x, input$size3, input$prob3)
                     )
                     }else {
                       list(
                         r=r<-rnbinom(input$n, input$size3, mu=input$mu),
                         x=x<-round(seq(0,ceiling(max(r)),length.out=input$n)),
                         d=dnbinom(x, input$size3, mu=input$mu)
                       )
                   },
                   dnorm = list(
                     r=r<-rnorm(input$n, input$mean, input$sd),
                     x=x<-seq(ceiling(min(r)),ceiling(max(r)),length.out=input$n),
                     d=dnorm(x, input$mean, input$sd)
                   ),
                   dpois = list(
                     r=r<-rpois(input$n, input$lambda),
                     x=x<-round(seq(0,ceiling(max(r)),length.out=input$n)),
                     d=dpois(x, input$lambda)
                   ),
                   dt = list(
                     r=r<-rt(input$n, input$df3, input$ncp3),
                     x=x<-seq(ceiling(min(r)),ceiling(max(r)),length.out=input$n),
                     d=dt(x, input$df3, input$ncp3)
                   ),
                   dunif = list(
                     r=r<-runif(input$n, input$minmax[1], input$minmax[2]),
                     x=x<-seq(input$minmax[1],input$minmax[2],length.out=input$n),
                     d=dunif(x, input$minmax[1], input$minmax[2])
                   ),
                   dweibull = list(
                     r=r<-rweibull(input$n, input$shape4, input$scale4),
                     x=x<-seq(round(min(r)),ceiling(max(r)),length.out=input$n),
                     d=dweibull(x, input$shape4, input$scale4)
                   ),
                   dsignrank = list(
                     r=r<-rsignrank(input$n, input$n_1),
                     x=x<-round(seq(round(min(r)),ceiling(max(r)),length.out=input$n)),
                     d=dsignrank(x, input$n_1)
                   ),
                   dwilcox = list(
                     r=r<-rwilcox(input$n, input$m_2, input$n_2),
                     x=x<-round(seq(round(min(r)),ceiling(max(r)),length.out=input$n)),
                     d=dwilcox(x, input$m_2, input$n_2)
                   )
                   )
    
  })
  
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    data. <- data()
    r <- data.frame(x=data.$r, d=data.$d, x.=data.$x)
    #dd <- data.$d
    #xx <- data.$x
    m <- ggplot(r, aes(x)) 
    if(input$fixxbool==TRUE) m <- m + scale_x_continuous(limits=input$fixx)
    m <- m + geom_histogram(aes(y=..density..))
    m <- m + geom_line(aes(x.,d), color="blue")
    #m <- m + geom_point(aes(x.,d), color="blue")
    print(m)
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

  output$slieder3 <- renderUI({
    m <- input$m
    n <- input$n_
    sliderInput("k", "k: the number of balls drawn from the urn", value=min(15, m+n), min=0, max=max(m+n, 1), step=1)
  })


})

