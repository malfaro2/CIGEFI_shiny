source("global.r")

header = dashboardHeader(title = '', titleWidth = 350)

sidebar = dashboardSidebar(width = "350px",
                           sidebarMenu(
                             tags$style(
                               ".main-sidebar {float:center; margin-top:10px; padding-left:10px; padding-right:10px ;padding-bottom:10px}"
                             ), 
                             
                             h2('Central American Trends') ,
                             
                             br(),
                             
                             selectInput("variable", 'Variable', choices = c("Temperature", "Precipitation")),
                             
                             br(),
                             
                             selectInput("index",'Index',choices = NULL),
                             
                             br(),
                             
                             selectInput('time', 'Choose a time domain:',choices =  c("Yearly"="year", 'Bimonthly' = 'bimonth' , 'Monyhly' = 'month')),
                             
                             br(),
                             
                             selectInput('option', 'Time period', choices = NULL),
                             
                             br(),
                             
                             actionButton('update', 'Update now'),
                             
                             br(),
                             
                             h3('Shiny app developed by',style = 'text-align: center'),
                             h4('Sebastian Fuentes Gonzalez for', style = 'text-align: center'),
                             
                             br(),
                             
                             imageOutput("logo")
                             
                             
                           ) 
                           
)



body =   dashboardBody(
  
  fluidRow(
    # h2('Index name (Updated):', style = "font-weight: 500; color: #4d3a7d; position:relative; left:10px; top:2px"),
    h2(textOutput('index_name'), style = 'position:relative; left:10px; top:2px'),
    h3(textOutput('name_selected'), style = 'position:relative; left:10px; top:2px'),
    h4(textOutput("text_selected"), style = 'position:relative; left:10px; top:2px'),
    h4(textOutput('units'), style = 'position:relative; left:10px; top:2px')
  ),
  
  
  
  fluidRow(
    tags$img(),
    div(imageOutput("maps", width = '100%'), style = 'text-align: center;')
  ),
  
  fluidRow(
    
    box(
      title = 'Boxplot',solidHeader = TRUE,status = 'info', width = 6,
      withSpinner(imageOutput("boxplot")),
      actionButton('show_boxplot', 'Zoom in')
    ),
    
    
    
    box(
      title = 'Hovmoller diagram', solidHeader = TRUE, status = 'info', width = 6,
      withSpinner(imageOutput('holvmoller')),
      actionButton('show_holvmoller', 'Zoom in')
    )
    
  )
  
  
  
)




page = dashboardPage(
  
  header,
  sidebar,
  body
  
)





