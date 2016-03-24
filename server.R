
shinyServer(
  function(input, output, session) {
    
    N <- 200
    X <- t(as.matrix(expand.grid(0:N, 0:N)))
    X <- X[, colSums(X) <= N]
    X <- rbind(X, N:N - colSums(X))
    dimnames(X) <- list(letters[1:3], NULL)
    ncol <- 128
    
    x <- reactive({ input$plot_click$x })
    y <- reactive({ input$plot_click$y })
    
    Z.eucl <- reactive({
      prob <- c(x(), y(), 1 - x() - y())
      dist <- apply(
        X/colSums(X),
        2, # i.e. apply to columns
        function(x){ 
            sqrt( sum( (x - prob)^2 ) ) 
        }
      )
      out <- matrix(nrow=N+1, ncol=N+1)
      for(i in 1:ncol(X)){ out[1+X[1,i],1+X[2,i]] = dist[i] }
      out
    })
    Z.xent <- reactive({
      prob <- c(x(), y(), 1 - x() - y())
      dist <- apply(
        X/colSums(X),
        2, # i.e. apply to columns
        switch(input$ce_direction,
               "d(clicked, .)" = function(x){ - sum(prob*log(x)) },
               "d(., clicked)" = function(x){ - sum(x*log(prob)) },
               "average" =  function(x){ - sum(prob*log(x)) - sum(x*log(prob)) }
               )
      )
      out <- matrix(nrow=N+1, ncol=N+1)
      for(i in 1:ncol(X)){ out[1+X[1,i],1+X[2,i]] = dist[i] }
      out
    })
    # finite values of Z:
    Zval.eucl <- reactive({
      do.call(c, sapply(2:(N-1), function(i) Z.eucl()[i, 2:(N-i+1)]))
    })
    Zval.xent <- reactive({
      do.call(c, sapply(2:(N-1), function(i) Z.xent()[i, 2:(N-i+1)]))
    })
    breaks.eucl <- reactive({
      as.vector( quantile(Zval.eucl(), probs=(0:ncol)/ncol) )
    })
    breaks.xent <- reactive({
      as.vector( quantile(Zval.xent(), probs=(0:ncol)/ncol) )
    })
    levels.eucl <- reactive({
      as.vector( quantile(Zval.eucl(), probs=(0:10)/10) )
    })
    levels.xent <- reactive({
      as.vector( quantile(Zval.xent(), probs=(0:10)/10) )
    })
    output$naviplot <- renderPlot({
      par(mai=c(0,0,1,0))
      plot(0:1,0:1, xlab="", ylab="", axes=FALSE, type='n')
      segments(0,1,1,0, col='grey')
      segments(0,0,1,0, col='grey')
      segments(0,0,0,1, col='grey')
      text(0.35,0.3, labels=sprintf("Click\nfor a\ntrinomial\ndistribution"),
           col="grey", cex=2)    
    })
    output$euclplot <- renderPlot({
      par(mai=c(0,0,1,0))
      plot(0:1,0:1, xlab="", ylab="", axes=FALSE, type='n', main="Squared error")
      segments(0,1,1,0, col='grey')
      segments(0,0,1,0, col='grey')
      segments(0,0,0,1, col='grey')
      image(Z.eucl(), col = heat.colors(ncol), breaks=sort(breaks.eucl()), add=TRUE)
      contour(Z.eucl(), col="black", levels=levels.eucl(), drawlabels=FALSE, add=TRUE)
      points(x(), y(),  pch=19)
    })
    output$xentplot <- renderPlot({
      par(mai=c(0,0,1,0))
      plot(0:1,0:1, xlab="", ylab="", axes=FALSE, type='n', main="Cross entropy")
      segments(0,1,1,0, col='grey')
      segments(0,0,1,0, col='grey')
      segments(0,0,0,1, col='grey')
      image(Z.xent(), col = heat.colors(ncol), breaks=sort(breaks.xent()), add=TRUE)
      contour(Z.xent(), col="black", levels=levels.xent(), drawlabels=FALSE, add=TRUE)
      points(x(), y(),  pch=19)
    })
    output$plot_click_info <- renderPrint({
      str(input$plot_click)
    })
    output$coordinates <- renderText({
      x <- input$plot_hover$x
      y <- input$plot_hover$y
      z <- 1 - x - y
      sprintf("(%g, %g, %g)", x,y,z)
    })
    output$click_coords <- renderText({
      x <- input$plot_click$x
      y <- input$plot_click$y
      z <- 1 - x - y
      sprintf("(%g, %g, %g)", x,y,z)
    })
  }
)

