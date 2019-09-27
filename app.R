library(shiny)
library(dplyr)
library(ggplot2)

data <- read.csv('Data/player_rosters.csv')
player_list <- unique(data$Player)

ui <- fluidPage(
    includeCSS("www/bg.css"),
    includeScript('www/bg.js'),
    tags$h1("IPL STATS",align="center"),
    tags$br(),tags$br(),
    fluidRow(
      column(2),
      column(4,selectInput(inputId = "batsman",label='Select Batsman:',choices = player_list,selected = 'Sachin Tendulkar'),align="center"),
      column(4,selectInput(inputId = "bowler",label='Select Bowler:',choices = player_list,selected = 'Ravichandran Ashwin') ,align="center")
    ),
    
    tags$br(),
    
    fluidRow(column(2),
            column(3,plotOutput(outputId = 'img_batsman',height = 100,width=100),align='center'),
            column(1),
            column(3,plotOutput(outputId = 'img_bowler',height = 100,width = 100),align='center')
            ),
   
    tags$br(),tags$br(),
    
    fluidRow(
      column(4),
      column(4, tags$h3(tags$p(textOutput(outputId = "text"))),align="center")
    ),
    
    tags$div(
      fluidRow(
      tags$h4(
          column(2),
          column(4,verticalLayout(tags$p(textOutput(outputId = 'ballsfaced')),
          tags$p(textOutput(outputId = 'dots')),
          tags$p(textOutput(outputId = 'ones')),
          tags$p(textOutput(outputId = 'twos')),
          tags$p(textOutput(outputId = 'threes')),
          tags$p(textOutput(outputId = 'fours')),
          tags$p(textOutput(outputId = 'sixes')),
          tags$p(textOutput(outputId = 'SRrate'))),align="center")
        
      ),
      column(4,plotOutput(outputId = 'facetoface',width = 400,height = 300),align='center')
    ),
    
    tags$br(),
    fluidRow(
      column(4),
      column(4,
             tags$h3(textOutput(outputId = 'outtext')),
             tags$h4(tableOutput(outputId = 'dismissals')),align='center')
    )
    )
  )


server <- function(input, output, session) {
  data <- read.csv('Data/ball_by_ball_data.csv',na.strings = "")
  state <- reactiveValues()
  observe({
    state$x <- input$batsman
    state$y <- input$bowler
    state$z <- ifelse(state$x == state$y, "I GUESS NO ONE CAN BOWL TO HIMSELF!!!!!!!", "FACE TO FACE ")
    state$a <- ifelse(state$x != state$y, 1, 0)
  })
  
  output$img_batsman <- renderImage({
    filename <- normalizePath(file.path('./www',
                                        paste(input$batsman, '.jpg', sep='')))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$img_bowler <- renderImage({
    filename <- normalizePath(file.path('./www',
                              paste(input$bowler, '.jpg', sep='')))
                              # Return a list containing the filename
                              list(src = filename)
    }, deleteFile = FALSE)
  
  output$text <- renderText({
    if((state$a == 1 & nrow(head_to_head_runs()) > 0) | (state$a == 0)){
      state$z
    }else{
      "NO ENCOUNTER BETWEEN THESE TWO!!!!!!!!!"
    }
    })
  
  head_to_head_runs <- reactive( { 
                    temp <- data[data$batsman == input$batsman & data$bowler == input$bowler,]
                    temp1 <- as.data.frame(table(temp$batsman_runs))
                    Var1 <- c(0,1,2,3,4,6)
                    Freq <- c(0,0,0,0,0,0)
                    temp2 <- data.frame(Var1,Freq)
                    if(nrow(temp1) > 0){
                      temp3 <- merge(temp2,temp1,by='Var1',all=T)
                      for (i in 1:6){
                        temp2$Freq <- ifelse(is.na(temp3$Freq.y),temp3$Freq.x,temp3$Freq.y)
                      }
                      temp2
                    }else{
                      temp1
                    }
                  })
  
  head_to_head_dismissals <- reactive({
    temp <- data[data$batsman == input$batsman & data$bowler == input$bowler,]
    temp <- as.data.frame(table(temp[temp$dismissal_kind != 'run out' & !is.na(temp$dismissal_kind),'dismissal_kind']))
    temp <- temp[temp$Freq>0,]
    
  })
  
  output$ballsfaced <- renderText({
    temp <- data[data$batsman == input$batsman & data$bowler == input$bowler,]
    if(state$a == 1 & nrow(temp) > 0 ){
      paste(" Total : ",nrow(temp))
    }
  })
  output$dots <- renderText({
    if(state$a == 1 & nrow(head_to_head_runs()) > 0 ){
      paste("  Dots : ",head_to_head_runs()[head_to_head_runs()$Var1 == 0,]$Freq)
    }else{
      paste("  Dots : ",0)
    }
  })
  
  output$ones <- renderText({
    if(state$a == 1 & nrow(head_to_head_runs()) > 0 ){
      paste("  Ones : ",head_to_head_runs()[head_to_head_runs()$Var1 == 1,]$Freq)
    }else{
      paste("  Ones : ",0)
    }
  })
  
  output$twos <- renderText({
    if(state$a == 1 & nrow(head_to_head_runs()) > 0 ){
      paste("  Twos : ",head_to_head_runs()[head_to_head_runs()$Var1 == 2,]$Freq)
    }else{
      paste("  Twos : ",0)
    }
  })
  
  output$threes <- renderText({
    if(state$a == 1 & nrow(head_to_head_runs()) > 0 ){
      paste("Threes : ",head_to_head_runs()[head_to_head_runs()$Var1 == 3,]$Freq)
    }else{
      paste("  Threes : ",0)
    }
  })
  
  output$fours <- renderText({
    if(state$a == 1 & nrow(head_to_head_runs()) > 0 ){
      paste(" Fours : ",head_to_head_runs()[head_to_head_runs()$Var1 == 4,]$Freq)
    }else{
      paste("  Fours : ",0)
    }
  })
  
  output$sixes <- renderText({
    if(state$a == 1 & nrow(head_to_head_runs()) > 0 ){
      paste(" Sixes : ",head_to_head_runs()[head_to_head_runs()$Var1 == 6,]$Freq)
    }else{
      paste("  Sixes : ",0)
    }
  })
  
  output$SRrate <- renderText({
    temp <- data[data$batsman == input$batsman & data$bowler == input$bowler,]
    
    if(state$a == 1 & nrow(temp) > 0 ){
      SRrate <- (sum(temp$batsman_runs)/nrow(temp))*100
      paste("Strike Rate : ",SRrate)
    }
  })
  
  output$facetoface <- renderPlot({
    temp <- head_to_head_runs()
    temp$Var1 <- as.factor(temp$Var1)
    if(state$a == 1 & nrow(head_to_head_runs()) > 0 ){
        ggplot(temp, aes(Var1, Freq)) + geom_histogram(stat = 'identity')+ggtitle("Runs By Category") + xlab("Runs Scored as ") + ylab("Count") +
        theme(panel.background = element_rect(fill = 'cyan'),plot.background = element_rect(fill = 'cyan'),plot.title = element_text(color="blue", size=25, face="bold"),axis.title.x = element_text(color="#993333", size=14, face="bold"),axis.title.y = element_text(color="#993333", size=14, face="bold"))
    }else{
      Var1 <- c(0,1,2,3,4,6)
      Freq <- c(0,0,0,0,0,0)
      temp <- data.frame(Var1,Freq)
      temp$Var1 <- as.factor(temp$Var1)
      ggplot(temp, aes(Var1, Freq)) + geom_histogram(stat = 'identity')+ggtitle("Runs By Category") + xlab("Runs Scored as ") + ylab("Count") +
        theme(panel.background = element_rect(fill = 'cyan'),plot.background = element_rect(fill = 'cyan'),plot.title = element_text(color="blue", size=25, face="bold"),axis.title.x = element_text(color="#993333", size=14, face="bold"),axis.title.y = element_text(color="#993333", size=14, face="bold"))
    }
  })
  
  output$outtext <- renderText({
    if(state$a == 1 & nrow(head_to_head_dismissals()) > 0){
        paste("DISMISSALS ","")
      }
  })
  
  output$dismissals <- renderTable ({
    if(state$a == 1 & nrow(head_to_head_dismissals()) > 0 ){
      head_to_head_dismissals()
    }
  },colnames = FALSE)
}

shinyApp(ui, server)