library(shiny)
library(ggplot2)
library(DT)
library(openxlsx)

FdeGraybill_ <- function(df, Y1, Yj, alpha = 0.05, Tab = 3) {
  
  Y1 <- df[[Y1]]
  Yj <- df[[Yj]]
  
  fit <- lm(Yj ~ Y1)
  QMRes <- sum(residuals(fit)^2)/fit$df.residual
  beta_theta <- coef(fit) - c(0,1)
  Y1linha_Y1 <- cbind(c(length(Y1), sum(Y1)), c(sum(Y1), sum(Y1^2)))
  FH0 <- round((t(beta_theta)%*%Y1linha_Y1%*%beta_theta)/(2*QMRes),4)
  
  Ftab <- round(qf(p=alpha, df1=2, df2=fit$df.residual, lower.tail = FALSE),4)
  pvalor <- signif(pf(FH0,2,fit$df.residual,lower=F),4)
  
  if(FH0 > Ftab){Resultado <- "*"}else(Resultado <- "ns")  
  if(FH0 > Ftab){Conclusao <- "V. Proposto é estatisticamente diferente de V.Padrão, para o n. de significância estabelecido"}else{Conclusao <- "V. Proposto é estatisticamente igual a V. Padrão, para o n. de significância estabelecido"}
  
  Tab_Res_Simp <-  data.frame("F_H0"    = FH0, 
                              "F_crit"  = Ftab, 
                              "P_valor" = pvalor, 
                              "Alpha"   = alpha,
                              "Teste"   = Resultado) 
  
  Tab_Res_Med <- data.frame(Resultado = rbind(mean(Y1), mean(Yj), var(Y1), var(Yj), sd(Y1), sd(Yj), length(Y1), 2, fit$df.residual, Ftab, FH0, alpha, pvalor),
                            row.names = c("Media_Y1", "Media_Yj", "Variancia_Y1", "Variancia_Yj", "Desvio_Padrao_Y1", "Desvio_Padrao_Yj", "Observacoes", "g.l.1", "g.l.2", "F_crit", "F_H0", "alpha",  "p valor") )
  
  aux1 <- c(round(mean(Y1),2), round(var(Y1),2), round(sd(Y1),2),  length(Y1), 2, Ftab, FH0, alpha, pvalor, Resultado, Conclusao)
  aux2 <- c(round(mean(Yj),2), round(var(Yj),2), round(sd(Yj),2), length(Yj), fit$df.residual, " ", " ", " ", " ", " ", " ")
  
  Tab_Res_Comp <- data.frame("Valor Padrão" = aux1,
                             "Valor Proposto" = aux2, 
                             row.names = c("Média", "Variância", "Desvio Padrão", "Observações", "grau de liberdade", "F-Crítico", "F(H0)", "Nível de significância", "P-valor" ,"Teste", "Conclusão") )
  
  if(Tab==1)
  {
    return(Tab_Res_Simp)
  } 
  else if(Tab==2)
  {
    return(Tab_Res_Med)
  }
  else(return(Tab_Res_Comp))
}

shinyServer( function(input, output,session) { # como estamos usando reactive, cria-se session
  
  outVar <- reactive({ # iremos salvar os nomes das variaveis do objeto carregado em uma funcao reativa
    
    if(input$Load == 0){return()} # se o botao load nao for pressionado, retornar nada
    inFile <- input$file1 
    if(is.null(inFile)){return(NULL)} # se o arquivo nao for carregado, retornar null
    
    # Carregar o arquivo com base em input
    if(input$excel==F)
    {
      mydata <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec)
      
    }else {
      file.copy(inFile$datapath,
                paste(inFile$datapath, "xlsx", sep="."));
      mydata <- openxlsx::read.xlsx(paste(inFile$datapath, "xlsx", sep="."), 1)       
    }

    names(mydata) # nomes das variaveis do arquivo carregado
  })  
  
  observe({ # Com observe iremos atualizar a lista de variaveis em selectizeInput

      updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "columnY1", # Id do SelecizeInput que sera atualizado
      choices = c("Selecione a coluna"="",outVar() ) ) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    })
  
  observe({ # Com observe iremos atualizar a lista de variaveis em selectizeInput
    
    updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "columnYj", # Id do SelecizeInput que sera atualizado
      choices = c("Selecione a coluna"="",outVar() ) ) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
  })
  
  observe({ # este observe muda a tab selecionada para dados
    # caso o usuário carregue os dados (clicando no action button Load)
    if (input$Load) updateTabsetPanel(session, "tabs", selected = "Dados")
    if (input$run) updateTabsetPanel(session, "tabs", selected = "Resultado")
    
     
  })
  
  vals <- reactiveValues()
  
  newData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    
    if(input$Load==0){return()} # se o botao load nao for pressionado(==0), retornar nada
    else(inFile <- input$file1) # caso contrario, salvar o caminho do arquivo carregado em inFile

    # input$file1 sera NULL inicialmente. apos o usuario selecionar
    # e upar um arquivo, ele sera um data frame com as colunas
    # 'size', 'type', e 'datapath' . A coluna 'datapath' 
    # ira conter os nomes dos arquivos locais onde o dado pode ser encontrado
    
    if (is.null(inFile)){return(NULL)} # se o arquivo nao for carregado, retornar null
    else if(input$excel == F)
      {
      raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    } else {
      file.copy(inFile$datapath,
                paste(inFile$datapath, "xlsx", sep="."));
      raw_data <- readxl::read_excel(paste(inFile$datapath, "xlsx", sep="."), 1)       
    }
    # Carregamos o arquivo em um objeto

    subset_data <- raw_data # Criamos uma copia do arquivo
    # este sera mostrado enquanto o usuario nao seleciona colunas com input$columns

    if(input$run) # se o botao input#subset for apertado
    {
    subset_data <- raw_data[, c(input$columnY1, input$columnYj) ] # filtar colunas com base em input$columns
    }
    
    vals$keeprows = rep(TRUE, nrow(raw_data) )
    
    
    subset_data # tabela final a ser mostrada. 
    # Se o botao input$columns nao for pressionado, mostra os arquivos inalterados
    # caso contrario, este se torna filtrado, e o arquivo se altera
    
  })
 
  output$data <- renderDataTable({ # renderizamos uma DT::DataTable
  
      
   # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
  df <- newData() 
  
  df <- df[vals$keeprows, ]
  
  datatable(df, options = list(searching = FALSE,
                                  paging=TRUE,pageLength = 30)) # Criamos uma DT::datatable com base no objeto
  
  # Este arquivo e reativo, e ira se alterar caso o usuario
  # aperte o botao input$columns
  
  })
  
  tabgraybill <- reactive({ # renderizamos uma tabela normal
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    if(is.null(newData() ) ){return()}
    df <- newData() 
    
    df <- df[vals$keeprows, ]
    
    x <- FdeGraybill_(df, input$columnY1, input$columnYj, alpha = input$alpha)
    
    x
    
  })
  
  output$tablegraybill <-renderDataTable({ 
    
    x <- tabgraybill() 
    
    if(is.null(x)){return()} # se o arquivo nao for carregado, retornar null
    
    datatable(x, options = list(searching = FALSE,
                                          paging=FALSE ) )
    
    })

  output$plot1 <- renderPlot({ 
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    
    df <- newData()
    
    if(is.null(df)){return()} # se o arquivo nao for carregado, retornar null
    # evita mensagens de erro cas o o arquivo nao esteja carregado ainda
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    # Plot the kept and excluded points as two separate data sets
    keep    <- df[ vals$keeprows, , drop = FALSE]
    exclude <- df[!vals$keeprows, , drop = FALSE]

    # O resto deste script se baseia no nome das colunas, Yj e Y1
    # ou seja, equanto o usuario nao apertar input$columns,
    # este codigo nao ira funcionar
    
    # utilizando o pacote ggplot2, renderizamos um grafico de dispersao simples
    
    graph <- ggplot(data = keep, aes_string(input$columnY1, input$columnYj) ) + # dados e variaveis utilizadas
      geom_smooth(method="lm", colour="red",se=F) + # linha do ajuste
      geom_point(size=5) +
      geom_point(data=exclude, fill=NA,col="black",alpha=0.75,shape=21, size=5) + # grafico de dispersao
      labs(x="Valor Padrão", # titulo eixo x
           y="Valor Proposto", # titulo eixo y
           title = "Comparação \n (Valor Proposto x Padrão)") + # titulo do grafico
      theme(axis.title=element_text(size=12, face= "bold" ),  # tamanho da letra e tipo da letra dos eixos
            plot.title=element_text(size=16,face="bold", hjust = 0.5) ) #+ # tamanho da letra e tipo da letra do titulo
    #  coord_cartesian(xlim = c(0, max(dados$Y1 + 0.3)), # alteracao da escala
    # ylim = c(0, max(dados$Yj + 0.3)))
    
    graph     
    
    
    
    
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    df <- newData()
    res <- nearPoints(df, input$plot1_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })

  # Reset all points
  observeEvent(input$exclude_reset, {
    df <- newData()
    vals$keeprows <- rep(TRUE, nrow(df) )
  })

  
  output$texto <- renderUI({
    
    paste("Clique nos pontos para removê-los do teste.")
    
  })
  
  output$exludeded_rows <- renderDataTable({
    
    df <- newData() 
    
    if(is.null(df)){return()} # se o arquivo nao for carregado, retornar null
    
    res <- nearPoints(df, input$plot1_click, allRows = TRUE)
    
    validate(need( !xor(vals$keeprows, res$selected_) , "" )  )
    
    
    df <- df[!vals$keeprows, ]
    
    datatable(df, options = list(searching = FALSE,
                                 paging = FALSE)) # Criamos uma DT::datatable com base no objeto

  })
    
  output$downloadData <- downloadHandler(
    filename = function() { 
      
      
        "f_graybill_teste.xlsx"
      
    },
    
    content = function(file) {

      openxlsx::write.xlsx(as.data.frame( tabgraybill() ), file)
      
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() { 
      
      
      "grafico_teste_f_graybill.png"
      
    },
    
    content = function(file) {
      
      
      ggsave(file, graph(), width = 12, height = 6)
      
      
      
      
    }
  )

  })
  

  
