library("mvtnorm") # для многомерного нормального
library("ggplot2") # для графиков
library("plotly")

shinyServer( function(input, output) {
  K_number_of_flowers <- reactive({
    number_of_flowers <- rlnorm(input$Number_of_days, mean = input$mean_log_flowers, sd = input$std_log_flowers)
    df <- data.frame(number_of_flowers)
    days <- seq(from = 1, to = input$Number_of_days, by = 1)
    df <- cbind(days,df)
    namevector <- c("her_smile")
    df[,namevector] <- NA
    df <- rbind(c(0,1), df)
    for(i in 1:input$Number_of_days+1){
      smile <- log(df$number_of_flowers[i]) + input$coef*log(df$number_of_flowers[i-1])
      df$her_smile[i] <- smile 
    }
    df$number_of_flowers <- df$number_of_flowers * 1000
    df
  })
  Values <- reactive({
    cov_Y <- input$coef*input$std_log_flowers^2
    var_e_t1 <- input$std_log_flowers^2 + (input$coef)^2*input$std_log_flowers^2
    corr <- cov_Y/var_e_t1
    a <- corr
    E_e <- (input$mean_log_flowers+input$coef*input$mean_log_flowers)
    E_Z <- E_e - a*E_e
    if (cov_Y!= 0 &&
        E_Z == 0) {
      cond_E <- c(a, '*Yt-1')
      table <- data.frame(
        Name = c("Corr(Yt, Yt-1)", 
               "E(Yt|Yt-1)"),
        Value = as.character(c(corr, paste(cond_E, collapse="")), 
        stringsAsFactors=FALSE))
      table
    } else if (cov_Y!= 0 &&
               E_Z != 0) {
      cond_E <- c(a, '*Yt-1 + ', E_Z)
      table <- data.frame(
        Name = c("Corr(Yt, Yt-1)", 
                 "E(Yt|Yt-1)"),
        Value = as.character(c(corr, paste(cond_E, collapse="")), 
                             stringsAsFactors=FALSE))
    } else {
      table <- data.frame(
        Name = c("Corr(Yt, Yt-1)", 
                 "E(Yt|Yt-1)"),
        Value = as.character(c(corr, input$mean_log_flowers + input$coef*input$mean_log_flowers),
        stringsAsFactors=FALSE))
      table
    }
  })
  output$path1 <- renderPlotly({
    df <- K_number_of_flowers()
    days = df[,1]
    number_of_flowers = df[,2]
    g <- ggplot(df, aes(x = days, y = number_of_flowers)) + geom_line(size = 0.1, colour = "#006600") + 
      geom_point(colour = "#000000", alpha = 0.5)+ xlab("Номер дня в истории отношений") + ylab("Количество цветов") +
      ggtitle("Изменение количества цветов с течением времени") + 
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    gg <- ggplotly(g)
    gg
  })
  output$path2 <- renderPlotly({
    df <- K_number_of_flowers()
    days = df[,1]
    smile_level = df[,3]
    g <- ggplot(df, aes(x = days, y = smile_level)) + geom_line(size = 0.1, colour = "#990000") + 
      geom_point(colour = "#000000", alpha = 0.5) + stat_smooth(size = input$line_size) + xlab("Номер дня в истории отношений") + ylab("Уровень улыбки") + 
      ggtitle("Изменение уровня улыбки с течением времени") + theme(plot.title = element_text(lineheight=.8, face="bold"))
    gg <- ggplotly(g)
    gg
  })
  output$dotplot1 <- renderPlotly({
    df <- K_number_of_flowers()
    number_of_flowers = df[,2]
    smile_level = df[,3]
    g <- ggplot(df, aes(x = number_of_flowers, y = smile_level)) + geom_point(colour = "#000000", alpha = 0.5) + stat_smooth(size = 0.1) + xlab("Количество цветов") + ylab("Уровень улыбки") + 
      ggtitle("Зависимость уровня улыбки от количества подаренных цветов") + theme(plot.title = element_text(lineheight=.8, face="bold"))
    gg <- ggplotly(g)
    gg
  })
  output$histogram <- renderPlotly({
    df <- K_number_of_flowers()
    smile_level = df[,3]
    g <-ggplot(df, aes(smile_level)) + geom_density(fill="#0000FF", colour="#000000", alpha = 0.5) + xlab("Уровень улыбки") + 
      ylab("Плотность") + ggtitle("Плотность уровня улыбки") + theme(plot.title = element_text(lineheight=.8, face="bold"))
    gg <- ggplotly(g)
    gg
  })
  output$values <- renderTable({
    Values()
  })
})

