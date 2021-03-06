library(shiny)
library(ggplot2)

dta <- read.csv(file = "data/MLB2008 .csv",
                header = TRUE)
dta$POS <- as.factor(dta$POS)
dta$FR[dta$G <= 163 & dta$G >= 120] <- 'high'
dta$FR[dta$G < 120 & dta$G >= 60] <- 'mid'
dta$FR[dta$G < 60] <- 'low'

dta$PAY[dta$SALARY <= 5000000] <- 'low'
dta$PAY[dta$SALARY <= 10000000 & dta$SALARY > 5000000] <- 'mid'
dta$PAY[dta$SALARY <= 20000000 & dta$SALARY > 10000000] <- 'high'
dta$PAY[dta$SALARY > 20000000] <- 'great'

dta$POS <- as.factor(dta$POS)
dta$PAY <- as.factor(dta$PAY)
dta$PAY <- factor(dta$PAY, levels = c("low", "mid", "high", "great"))
dta$FR <- as.factor(dta$FR)
dta$SALARY1 <- as.numeric(dta$SALARY)

choice.type <-
  c('FR', 'POS', 'PAY')
choice.value <-
  c(
    'AVG',
    'OBP',
    'SLG'
  )

# UI
ui <- navbarPage(
  "棒球分析",
  tabPanel(
    "Introduction",
    tags$h1("主旨"),
    tags$img(height = 168, width = 300, src = "data:image/jpeg;base64,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"),
    tags$h2("　　這是我們的作品，裏頭的內容是關於2008年的MLB球員數據，我們組別希望透過分析數據來幫助球隊老闆們組建自己的球隊。首先我們先用球員在球隊之中擔任的位子來做區分，我們想看一下各個位子的球員以及他們薪水的回歸直線。
            一方面也是給予選手一個參考，正值高中的棒球選手，其實打的位置以及方式還未定型，所以還是有機會改變或者朝不同方向發展，透過對比不一樣位子、不一樣風格球員的薪資，可以使年輕球員們知道往哪個方向發展可以獲得較高的薪資。
            ")
    #HTML("<img height=600 src=\"https://78.media.tumblr.com/aa70ed84a7cd2e7b83c36118dfb2c0e5/tumblr_p8xzq1U8ik1qhy6c9o1_500.gif\"/>")
    ),
  tabPanel(
    "原始數據",
    tags$h1("數據呈現"),
    br(),
    fluidRow(column(
      8,
      tabPanel("Table",
               DT::dataTableOutput("data.raw"))
    ))
  ),
  
  tabPanel(
    "各項數據",
    tags$h1("各項數據的人數"),
    tags$h2("這裡我們有五項可以觀看的數值:"),
    tags$h2("(1)Fr:球員上場的頻率 -> 分為low(登場次數163~120), mid(登場次數120~60), high(登場次數60~0)"),
    tags$h2("(2)POS:球員的守備位置 -> "),
    tags$img(height = 300, width = 300, src = "https://upload.wikimedia.org/wikipedia/commons/8/88/Baseball_positions.svg"),
    tags$h2("而10則代表代打，11代表代跑"),
    tags$h2("(3)AVG:球員的打擊率 -> 選手擊出的安打數除以打數"),
    tags$h2("(4)OBP:球員的上壘率 ->（安打+四壞保送+觸身球）/（打數+四壞保送+觸身球+高飛犧牲打）"),
    tags$h2("(5)SLG:球員的長打率 -> 壘打數/打數"),
    tags$h2("(6)Pay:球員薪水 -> 他分成low(年薪500萬以下) mid(年薪500~1000萬) high(年薪1000~2000萬) great(年薪2000萬以上)"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('SV.input', 'type', c(choice.type, choice.value), selectize = TRUE)
      ),
      mainPanel(plotOutput("SV.plot"))
    ),
    
    verbatimTextOutput("summary")
  ),
  
  tabPanel(
    "Comparing",
    tags$h1("Box Plot"),
    tags$h2("　　The main idea of this page is to inspesct the relationship between Fr, POS, Pay and the others. Including the plot, we can know that which position's player usually have better performance at their AVG, SLG and OBP. Also we can know that if the player perform as the Team expect."),
    sidebarLayout(
      sidebarPanel(
        selectInput('PA.type', 'type', choice.type, selectize = TRUE),
        selectInput('PA.value', 'Value', choice.value, selectize =
                      TRUE)
      ),
      mainPanel(plotOutput("PA.plot"))
    )
  ),
  #NEW
  tabPanel(
    "Advanced",
    tags$h1("regression analysis"),
    tags$h2("　　The main idea of this tool is to saperate different salary level. By using the website, you can figure out that AVG data doesn't change with salary obviously as OBP and SLG.
If you spend more money in the great or high level of salary you can get more benefit than it in the low or mid level, because of the big change of  slope in OBP and SLG."),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('LN.input', 'Value', choice.value, selectize = TRUE)
      ),
      mainPanel(plotOutput("LN.plot"))
    )
  ),
  
  tabPanel("Conclusion",
           tags$h1("Sum up"),
           tags$h2("　　If I'm a boss of a baseball team, I can use this website to help me know more about the true value of my players. I can check their performance and 
compare with other players in the league. The thing I care the most is that do they play the game as I expected. Using box plot I can know  what performance 
should players in each levels act like."),
           tags$h2("　　If I'm a player, I can use this website to know my true value in the league. Maybe I'm not the top of the players in the whole league. But I'm the best player in my position.
Being the best player of the position means a lot, because baseball player couldn't change their position easily. It needs a lot of time to be experienced.
So it gives me a big chance to bargain with the boss."),
           tags$img(height = 300, width = 500, src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQ2RuetU80I2Szh75mh_5Ha_SVqG-GzT_UR0Fg6A5Tf7Ha5sCwDlg")
           ),
  
  tabPanel("more",
           tags$h1("data source"),
           tags$a(href = "https://legacy.baseballprospectus.com/glossary/index.php?mode=viewstat&stat=397", "Baseball Prospectus"),
           tags$br(),
           tags$a(href = "http://www.exploredata.net/Downloads/Baseball-Data-Set", "Data")
  )
  )

# server
server <- function(input, output, session) {
  output$SV.plot <- renderPlot({
    if( is.element(input$SV.input, choice.type) ){
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_bar() +
        labs(y = "count", x = input$SV.input)
    }
    else{
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_histogram() +
        labs(y = "count", x = input$SV.input)
    }
  })
  
  output$PA.plot <- renderPlot({
    ggplot(data = dta, aes_string(x = input$PA.type, y = input$PA.value)) +
      geom_boxplot() + coord_flip() +
      labs(y = input$PA.value, x = input$PA.type)
    
  })
  
  output$summary <- renderPrint({
    summary(dta)
  })
  
  #NEW
  output$LN.plot <- renderPlot({
    ggplot(data = dta, aes_string(group = dta$PAY, x = dta$SALARY, y = input$LN.input)) +
      geom_point() + geom_smooth(method = lm) +
      labs(y = input$LN.input, x = "SALARY")
    
  })
  
  output$data.raw <- DT::renderDataTable({
    DT::datatable(dta)
  })
  
  output$data.summary <- DT::renderDataTable({
    DT::datatable(summary(dta))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

