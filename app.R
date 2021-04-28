# ANALYSIS
library(plotly)
library(shiny)

BACKGROUND <- function(){
  tags$head(
    tags$style(paste0(
"body {
content: '';
background-color: WhiteSmoke
}
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: black;
border-color: black;
}
*:focus:not(.focus-visible) {
outline: 0 !important;
box-shadow: none !important;
}
.progress-bar {
background-color:#34495E;
}
thead { color: #34495E;
}
tbody { color: #34495E;
}
tr:hover {
background-color: #D5D8DC !important;
}
.col-sm-6 {
    width: 100%;
    font-size: 11px;
    text-align: right;
}
.dataTables_paginate {
text-align: right !important;
}
.dataTables_paginate a {
color: #34495E !important;
}
.pagination .active a {
background-color: lightgray !important;
border-color: lightgray !important;
}
footer {
background: black;
width: 100%;
height: 30px;
position: fixed;
z-index: 9999;
margin: auto;
bottom: 0;
left: 0;
text-align: right;
color: darkgray;
line-height: 30px;
font-size: 11px;
}
")))
}

ui <- fluidPage(
  
  tags$head(tags$style(
    "#titulo{font-size:14px;
    font-weight:bold; text-align:center;
    width:670px}"
  )),
  
  BACKGROUND(),
  
  HTML("<br>"),
  
  mainPanel(
    HTML("<br>"),
    tags$style(type="text/css",
               "#DTO tfoot {display:none;}"),
    tags$style(paste0(
      "#DTO .table th { text-align: center; }
       #DTO .table td { text-align: center; }
       #DTO { text-align:center }")),
    verbatimTextOutput("titulo"),
    uiOutput("uiOutput"),
    uiOutput("uiOutput2"),
    tags$head(tags$style("
       #VTO{font-size:15px;
       font-weight: bold;background-color: transparent;
       border-color:black; display: table-cell;
       vertical-align: middle }")),
    verbatimTextOutput("VTO"),
    HTML("<br>"),
    uiOutput("uiOutput3"),
    uiOutput("uiOutput4"),
    HTML("<br><br>"),
    div(style='margin-bottom:25px',dataTableOutput("DTO")),
    tableOutput("TO"),
    HTML("<br><br>"),
    fluid = F,width = 12,style='left:25px; right:25px; width:700px;')
)

server <-
  function(input, output,session) {
    
    output$titulo <- renderPrint({cat("Win-Loss Analysis")})
    
    options(scipen = 999)

    word <- function(x,y) {
      
      x <- as.character(x)
      
      final <- NULL
      for (i in 1:length(x)){
        words <- strsplit(x[i]," ")[[1]]
        if (y > 0){
          final <- append(final, words[y])
        }
        else {
          contrario <- length(words) + 1 + y
          final <- append(final,words[contrario])
        }
      }
      final
    }
    
    values <- reactiveValues(
      AAA = NULL,
      buy_in = NULL
    )
    
    ### Análise dos torneios pela auditoria ###
    auditoria <- function(audit){
      
      # Filtra apenas o que interessa
      AAA <- audit[grepl("Tournament Registration|Tournament Unregistration|Tournament Won",audit$V2),]
      
      # Identifica as linhas que informam que um torneio foi desregistrado
      AAAun <- AAA[grepl("Tournament Unregistration",AAA$V2),]
      
      # Identifica o nome dos torneios que foram desregistrados
      AAAun <- AAAun$V3
      
      # Pega cada nome identificado e exclui o registro e o desregistro do jogo
      for (i in 1:length(AAAun)){
        
        # pega o nome e localiza o registro e o desregistro
        ppp <- grep(AAAun[i],AAA$V3,fixed = T)[1:2]
        
        # marca o registro e o desregistro da tabela geral para apagar
        AAA[ppp,] <- "APAGAR"
      }
      
      # apaga definitivamente o que não interessa
      AAA <- AAA[!grepl("APAGAR",AAA$V1),]
      
      #rownames(AAA) <- 1:nrow(AAA)
      torneio <- AAA
      
      # transforma de character para numeric
      torneio[,6] <- sapply(torneio[,6], function(x) as.numeric(x), USE.NAMES = F)
      
      # estoca a média do buy-in geral
      values$buy_in <- mean(torneio$V6[torneio$V2 == "Tournament Registration"])*(-1)
      
      # transforma a primeira linha só para as datas, sem horários
      torneio$V1 <- word(torneio$V1, 1)
      
      # Identifica as linhas que informam que um torneio foi vencido
      AAAun <- torneio[torneio$V2 %in% "Tournament Won",]
      
      # Identifica o nome dos torneios que foram vencidos
      AAAun <- AAAun$V3
      
      if (length(AAAun>0)){
        
        # Pega cada nome identificado e mescla o registro com a vitoria
        for (i in 1:length(AAAun)){
          
          # pega o nome e localiza o registro e a vitória
          ppp <- grep(AAAun[i],torneio$V3,fixed = T)
          
          # marca o registro e o desregistro da tabela geral para apagar
          torneio$V6[ppp[1]] <- sum(as.numeric(torneio$V6[ppp]))
          torneio[ppp[2],1] <- "APAGAR"
        }
      }
      
      # apaga definitivamente o que não interessa
      torneio <- torneio[!grepl("APAGAR",torneio$V1),]
      torneio <- torneio[!grepl("</font>",torneio$V3),]
      torneio <- torneio[!grepl("Freeroll",torneio$V3),]
      
      rownames(torneio) <- 1:nrow(torneio)
      
      # estoca torneio
      values$AAA <- torneio
      
      # lista o nome dos torneios
      tourns <- gsub(".*\\$","",torneio$V3)
      
      # finaliza a lista sem duplicações
      tourns <- tourns[!duplicated(tourns)]
      
      # remove sujeira
      tourns <- gsub("<b> ","",tourns)
      tourns <- gsub("</b>","",tourns)
      tourns
    }
    
    # comando para subir o balanço de ganhos
    my_file <- reactive({
      inFile <- input$auditFile
      if (is.null(inFile)) return(NULL)
      if (grepl("audit",inFile$name) & grepl(".csv",inFile$name)){
        df<-read.csv(inFile$datapath,header=F)
        output$uiOutput <- renderUI({})
        output$uiOutput2 <- renderUI({ selectInput("SI",NULL,NULL,width = 670) })
        output$uiOutput3 <- renderUI({plotlyOutput("PO")})
      }
      df
    })
    
    output$TO <-
      reactive({
        if(length(input$auditFile)>0) {
          inFile <- input$auditFile
          if (is.null(inFile)) return(NULL)
          if (grepl("audit",inFile$name) & grepl(".csv",inFile$name)){
            tourns <- auditoria( my_file() )
            tourns <- tourns[order(as.numeric(word(tourns,1)))]
            tourns <- c("NL",tourns)
            names(tourns) <- c("All",tourns[-1])
            updateSelectInput(session,"SI",NULL,tourns)
            NULL
          }
          else {
            showModal(modalDialog(
              title = "ERROR",
              paste0("Not compatible! Please, upload a file \"Playing history audit.csv\" with the original name."),
              easyClose = TRUE,
              footer = NULL
            ))
          }
        }
      })
    
    output$uiOutput <- renderUI({
      fileInput("auditFile","",
                buttonLabel = "Audit file",
                placeholder = "upload a \"Playing history audit\" file (.csv) with the original name",
                accept = ".csv", multiple = F, width = 670)
    })
    
    plotagem <- function(){
      
      AAA <- isolate(values$AAA)
      
      x<-input$SI
      
      # captura apenas um torneio determinado
      p<-grep(x,AAA$V3,fixed=T)
      
      # isola esse torneio
      torneio <- AAA[p,]
      
      # identifica o buy-in
      if (x != "NL"){
        buy_in <- as.numeric(word(x,1))
      }
      
      else { buy_in <- values$buy_in }
      
      g <- torneio$V6

      ganhos <- 0
      ganhos = cumsum(g)
      
      df_plot <- data.frame(dias=1:nrow(torneio),
                            ganhos=ganhos,
                            valor=torneio$V6,
                            datas=torneio$V1)
      
      # Número de vitórias
      v <- sum(torneio$V6>0)
      
      # Número de jogos
      j <- nrow(torneio)
      
      lucro <- ganhos[length(ganhos)]
      
      output$VTO <- renderPrint({
        if(input$SI > 0) {
          cat(" Games:",j,'\n')
          cat(" Wins/Bubbles:",v,'\n')
          cat(" Success:",paste0(round(v/j,4)*100,"%"),'\n')
          cat(" Profits:",lucro,"dollars",'\n')
          cat(" Roi:",paste0(round((lucro/j)/buy_in,4)*100,"%"),'\n')
        } })
      
      output$DTO <- renderDataTable({ 
        if(input$SI > 0) {
          
          dia<-torneio$V1
          lucro<-torneio$V6
          vitoria<-NULL
          njogos<-nrow(torneio)
          for (i in lucro){
            if (i > 0) {vitoria <- append(vitoria,1)
            }
            else {
              vitoria <- append(vitoria,0)
            }
          }
          
          torneio<-data.frame(dia=dia,jogos=rep(1,njogos),vitorias=vitoria,lucro=lucro)
          
          torneio <- aggregate(list(torneio[2],torneio[3],torneio[4]),torneio[1],FUN = function(y) sum(y))
          
          success <- torneio$vitorias/torneio$jogos
          
          torneio$aproveitamento <- round(success,4)*100
          
          torneio$lucro <- round(torneio$lucro,4)
          
          colnames(torneio) <- c("day","games","wins/bubbles","profits","success(%)")
          
          torneio <- torneio[,c(1,2,3,5,4)]
          
          torneio
          
        } },options = list(pageLength = 10, filter = "top", lengthChange = FALSE,
                           autowidth = T,
                           columnDefs = list(list(width = '100px', targets = 0),
                                             list(width = '50px',targets = 1:4))
        ), searchDelay = 1000
        )
      
      a <- c(0,df_plot$dias)
      b <- c(0,df_plot$ganhos)
      titulos <- paste("<b>",c("",df_plot$datas),"</b>\n <i>T",gsub("$","</i> \n <b>$",c("",torneio$V3),fixed=T),paste0("</b> \n Profit: ",format(c(0,torneio$V6),nsmall = 2)),"dollars")

      fig<-plot_ly(
          type = 'scatter',
          mode = 'lines',
          size = I(1),
          x = a,
          y = b,
          color = I("#34495E"),
          text = c(titulos),
          hovertemplate = paste(
                                '%{text}',
                                '<extra></extra>'),
          
          showlegend = FALSE
        )
      fig <- fig %>%
        layout(hovermode = "x unified",
               plot_bgcolor='#FBFCFC',
               paper_bgcolor='#FBFCFC ',
               xaxis=list(tickformat=',d')
               )
      if (sum(torneio$V6[torneio$V6 > 0])>0){
        
        vitorias <- torneio$V6[torneio$V6 > 0]
        vitorias <- max(vitorias)
        
        fig <- fig %>% add_annotations(x = a[c(0,torneio$V6) >= vitorias],
                                       y = b[c(0,torneio$V6) >= vitorias],
                                       text = "[$]",
                                       bgcolor = "yellow",
                                       opacity = 1,
                                       xref = "x",
                                       yref = "y",
                                       showarrow = T,
                                       arrowcolor="green",
                                       arrowhead = 6,
                                       arrowsize = 2,
                                       arrowwidth = 0.8,
                                       ax = -10,
                                       ay = -40
        )
      }
      else { fig }
    }
    
    observeEvent(input$SI,{
      output$PO <- renderPlotly({
        if(input$SI > 0) {
          plotagem()
        }
      })
      })
    
  }

shinyApp(ui, server)
