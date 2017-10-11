library(shiny)
library(DT)
library(markdown)
library(readxl)
#library(shinythemes)

shinyUI( # cria a interface de usuario
  #shinythemes::themeSelector(),
  fluidPage(  # layout utilizado
    #theme = shinytheme("flatly"), # seleciona um tema utilizando pacote
    theme = "bootstrap8.css", # seleciona um tema contido na pasta www
    
    titlePanel("Teste F de Graybill"), # titulo do app
    
    sidebarLayout( # barra lateral
      
      sidebarPanel( # painel lateral
        
        fileInput( # input de arquivos
          inputId = "file1", # Id
          
          label = "Selecione o arquivo: (.csv, .txt ou .xlsx)", # nome que sera mostrado na UI
          
          accept=c('text/csv/xlsx','.csv', ".txt", ".xlsx")), # tipos de arquivos aceitos
        
        checkboxInput(inputId = "excel",
                      label = "Excel (.xls ou .xslx) ?",
                      value = F),
        
        radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
          inputId='sep',  #Id
          label='Selecione o separador:', # nome que sera mostrado na UI
          choices=c(Virgula=',', "Ponto e virgula"=';', Tab='\t'), # opcoes e seus nomes
          selected=';'), # valor que sera selecionado inicialmente
        
        radioButtons( # esta da ao usuario opcoes para clicar. Apenas uma e selecionada
          inputId='dec', # Id
          label='Decimal', # nome que sera mostrado na UI
          choices=c(Ponto=".", Virgula=","), # opcoes e seus nomes
          selected=","), # valor que sera selecionado inicialmente
        
        actionButton( # botao que o usuario clica, e gera uma acao no server
          "Load", # Id
          "Carregue o arquivo"),  # nome que sera mostrado na UI
        
        # texto mostrado na UI
        h4("Selecione as colunas que serão utilizadas no teste:"),
        
        selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
          'columnY1', # Id
          "selecione a coluna do valor padrão:", # nome que sera mostrado na UI
          choices = "" # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                       ), 

        selectizeInput( # cria uma lista de opcoes em que o usuario pode clicar
          'columnYj', # Id
          "selecione a coluna do valor proposto:", # nome que sera mostrado na UI
          choices = "" # como as opcoes serao atualizadas de acordo com o arquivo que o usuario insere, deixamos este campo em branco
                      ), 
        
                
        sliderInput("alpha","Selecione o nivel de significância", 0.01, 0.1, 0.05, 0.01),
        
        
        actionButton(# botao que o usuario clica, e gera uma acao no server
          "run", # Id
          "Selecionar e realizar o teste"), # nome que sera mostrado na UI
        
        width = 3 ), # largura da barra lateral
      
      mainPanel( # painel principal
        
        
        tabsetPanel( # cria um painel com varias tabs, que o usuario seleciona qua deseja visualizar
          id = "tabs", # id, caso ele seja referenciado em output
          tabPanel("Intro",  includeMarkdown("about.md") )  , # painel para um arquivo markdown que foi criado separadamente, contendo texto.
          tabPanel("Dados",    DT::dataTableOutput("data") )        , # painel para #output$data; mostra os dados inseridos pelo usuario
          tabPanel("Análise Gráfica",   plotOutput("plot1",click = "plot1_click") , 
                   htmlOutput("texto", inline = T),
                   actionButton("exclude_reset", "Resetar pontos"),
                   downloadButton('downloadPlot', 'Download'),
                  DT::dataTableOutput("exludeded_rows")  ) , # painel para #output$plot; mostra o grafico gerado pelo ggplot
          tabPanel("Resultado", DT::dataTableOutput("tablegraybill", "70%"),  downloadButton('downloadData', 'Download') )      # painel para #output$tabgraybill; mostra o resultado do teste F de Graybill
          
        )#, # fecha tabsetPanel
        
        # withMathJax(), # utilizando a funcao MathJax
        #  uiOutput("formula") #renderizar output$formula
        
      ) # fecha mainPanel
    ) # fecha sidebarLayout
  ) #fecha fluidPage
) # fecha shinyUI
