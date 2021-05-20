# ANALYSIS
library(plotly)
library(shiny)

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

CSS <- function(){
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

#VTO {font-size:15px;
font-weight: bold;background-color: transparent;
border-color:black; display: table-cell;
vertical-align: middle }

#titulo {font-size:14px;
font-weight:bold; text-align:center;
width:670px}
    
#DTO tfoot {display:none;}
    
#DTO .table th { text-align: center; }
#DTO .table td { text-align: center; }
#DTO { text-align:center }
       
")))
}

ui <- fluidPage(
  
  CSS(),
  HTML("<br>"),
  
  mainPanel(
    HTML("<br>"),
    verbatimTextOutput("titulo"),
    uiOutput("uiOutput"),
    uiOutput("uiOutput2"),
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
    
    values <- reactiveValues(
      total = NULL,
      buy_in = NULL
    )
    
    ### Função para organizar a tabela e separar os nomes dos torneios para o selectinput ###
    auditoria <- function(audit){
      
      # Filtra apenas o que interessa
      total <- audit[grepl("Tournament Registration|Tournament Unregistration|Tournament Won",audit$V2),]
      
      # Identifica os torneios que foram desregistrados
      unregistration <- total[grepl("Tournament Unregistration",total$V2),]
      
      # Separa só os nomes desses torneios
      unregistration <- unregistration$V3
      
      # Exclui todas as linhas relativas aos torneios desregistrados
      torneio <- total[!total$V3 %in% unregistration,]
      
      # Transforma coluna 6 em numeric
      torneio$V6 <- as.numeric(torneio$V6)
      
      # Estoca a média do buy-in geral
      values$buy_in <- mean(torneio$V6[torneio$V2 == "Tournament Registration"])*(-1)
      
      # Transforma a primeira linha só para as datas (sem horários)
      torneio$V1 <- word(torneio$V1,1)
      
      # Identifica as linhas que informam que um torneio foi vencido
      won <- torneio[torneio$V2 %in% "Tournament Won",]
      # Separa os nomes desses torneios
      won <- won$V3
      
      if (length(won>0)){
        # Pega cada nome para juntar o registro com a vitoria (soma o buy-in [negativo] com o prêmio)
        for (i in 1:length(won)){
          n <- grep(won[i],torneio$V3,fixed = T)
          torneio$V6[n[1]] <- sum(as.numeric(torneio$V6[n]))
          torneio[n[2],1] <- "APAGAR"
        }
      }
  
      # Apaga definitivamente o que não interessa
      torneio <- torneio[!grepl("APAGAR",torneio$V1),]
      torneio <- torneio[!grepl("</font>",torneio$V3),]
      torneio <- torneio[!grepl("Freeroll",torneio$V3),]
      
      rownames(torneio) <- 1:nrow(torneio)
      
      values$total <- torneio
      
      # Lista os nomes dos torneios
      tourns <- gsub(".*\\$","",torneio$V3)
      
      # Finaliza a lista sem duplicações
      tourns <- tourns[!duplicated(tourns)]
      
      # Remove sujeira
      tourns <- gsub("<b> ","",tourns)
      tourns <- gsub("</b>","",tourns)
      tourns
    }
    
    # Comando para subir o balanço de ganhos
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
      
      total <- isolate(values$total)
      
      x<-input$SI
      
      # Separa apenas o torneio selecionado para plotar
      p<-grep(x,total$V3,fixed=T)
      torneio <- total[p,]
      
      # Identifica o buy-in
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
          
          torneio<-data.frame(dia=dia,
                              jogos=rep(1,njogos),
                              vitorias=vitoria,
                              lucro=lucro)
          
          torneio <- aggregate(list(torneio[2],torneio[3],torneio[4]),torneio[1],FUN = function(y) sum(y))
          
          success <- torneio$vitorias/torneio$jogos
          
          torneio$aproveitamento <- round(success,4)*100
          
          torneio$lucro <- round(torneio$lucro,4)
          
          colnames(torneio) <- c("day","games","wins/bubbles","profits","success(%)")
          
          torneio <- torneio[,c(1,2,3,5,4)]
          
          torneio
        }
      },
      options = list(pageLength = 10, filter = "top", lengthChange = FALSE,
                     autowidth = T,columnDefs = list(list(width = '100px', targets = 0),
                                                     list(width = '50px',targets = 1:4))
      ), searchDelay = 1000)
      
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
               xaxis=list(tickformat=',d',title = "games\n"),
               yaxis=list(title = "win-loss")
        )
      if (sum(torneio$V6[torneio$V6 > 0])>0){
        vitorias <- torneio$V6[torneio$V6 > 0]
        vitorias <- max(vitorias)
        maximo <- c(0,torneio$V6) >= vitorias
        
        fig <- fig %>% add_annotations(x = a[maximo],
                                       y = b[maximo],
                                       text = "$", 
                                       bgcolor = "yellow",
                                       opacity = 1,
                                       xref = "x",
                                       yref = "y",
                                       showarrow = T,
                                       arrowcolor="green",
                                       arrowhead = 6,
                                       arrowsize = 1,
                                       arrowwidth = 0.9,
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
