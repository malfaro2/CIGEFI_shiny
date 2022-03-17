library(dplyr)
library(shiny)
library(stringr)
source("global.R")
source("pages.R")

shinyServer(
  function(input, output, session) {
    
    observe({
      x = data %>% filter(option == input$variable) %>% select(Index)
      updateSelectInput(session, "index", "Index", choices = unique(x))
    })
    
  
    observe({
      if(input$index == 'R99p'){
        updateSelectInput(session, "time", "Time", choices  = c("Yearly"="year", 'Bimonthly' = 'bimonth'))
      } else if(input$index == 'SDII'){
        updateSelectInput(session, "time", "Time", choices  = c("Yearly"="year"))
      } else {
        updateSelectInput(session, "time", "Time", choices  = c("Yearly"="year", 'Bimonthly' = 'bimonth' , 'Monyhly' = 'month'))
      }
    })
    
    
    
    
    observe({
      dates = date$when1[date$period == input$time]
      updateSelectInput(session, "option", "Time period", choices  = dates)
    })
    
    output$logo = renderImage({
      filename = 'logo_CIGEFI.png'
      list(src = filename, width = 300, heigth = 400)
    }, deleteFile = FALSE)
    
    
    output$index_name = renderText({
      
      if(input$update){paste('Index name: ')}
      else{paste('Update variables on the left, and click update')}
      
    })
    
    output$name_selected = renderText({
      
      input$update
      isolate(
        paste(data$names_index[data$Index == input$index])
      )
      
    })
    
    output$text_selected = renderText({
      
      input$update
      isolate(
        
        paste(data$text[data$Index == input$index])
        
      )
    })
    
    
    
    output$units = renderText({
      if(input$update){isolate(paste('Index units:', data$units[data$Index == input$index]))}
      else{paste(' ')}
    })
    
    
    output$maps <- renderImage({
      
      input$update
      isolate(
        
        if(input$time == 'bimonth' && input$variable == 'Temperature'){
          
          index_for_photo = data$index_number[data$Index == input$index]
          
          date_fixed = date$when[date$when1 == input$option]
          
          filename = paste0('maps/', 'temp', '_bimonth_index', index_for_photo, '_', date_fixed,'_', input$index, '.png')
          
          list(src = filename , width = '500px', height = '350px')
          
          
        } else if (input$time == 'bimonth' && input$variable == 'Precipitation') {
          
          index_for_photo = data$index_number[data$Index == input$index]
          
          date_fixed = date$when[date$when1 == input$option]
          
          filename = paste0('Prec_bimonth/',date_fixed,'/', 'prec', '_bimonth_index', index_for_photo,'_', date_fixed,'_', input$index,'.png')
          
          list(src = filename , width = '500px', height = '350px')
          
        } else if (input$time == 'month' && input$variable == 'Temperature') {
          
          index_for_photo = data$index_number[data$Index == input$index]
          
          date_fixed = date$when[date$when1 == input$option]
          
          filename = paste0('maps/', 'temp', '_month_index', index_for_photo, '_', date_fixed,'_', input$index, '.png')
          
          list(src = filename , width = '500px', height = '350px')
          
        } else if (input$time == 'month' && input$variable == 'Precipitation') {
          
          index_for_photo = data$index_number[data$Index == input$index]
          
          date_fixed = date$when[date$when1 == input$option]
          
          if(index_for_photo <= 7){
            
            filename = paste0('Prec_monthly/',str_to_title(date_fixed),'/', 'prec', '_month_index', index_for_photo,'_', date_fixed,'_',input$index, '.png')
            
            list(src = filename , width = '500px', height = '350px')
            
          } else {
            
            filename = paste0('Prec_monthly/',str_to_title(date_fixed),'/', 'prec', '_month_index', index_for_photo - 1,'_', date_fixed,'_',input$index, '.png')
            
            list(src = filename , width = '500px', height = '350px')
            
          }
          
          
        } else if (input$time == 'year' && input$variable == 'Temperature'){
          
          index_for_photo = data$index_number[data$Index == input$index]
          
          
          filename = paste0('yearly_maps_temp/','temp_year_index', index_for_photo , '_', input$index, '.png')
          
          list(src = filename , width = '500px', height = '350px')
          
        } else if (input$time == 'year' && input$variable == 'Precipitation'){
          
          index_for_photo = data$index_number[data$Index == input$index]
          
          filename = paste0('yearly_maps_prec/','prec_year_index', index_for_photo , '_', input$index, '.png')
          
          list(src = filename , width = '500px', height = '350px')
          
        }
        
      )
      
      
    }, deleteFile = FALSE)
    
    
    output$boxplot = renderImage({
      
      input$update
      isolate(
        if(input$time == 'month'){
          
          
          number_of_month = number_months$num_month[number_months$month_oficial == input$option]
          variable        = data %>% filter(option == input$variable) %>% select(option2)
          
          filename2 = paste0('boxplot_monthly/','boxplot_' , unique(variable) ,'_', input$index,'_mes', number_of_month, '.png')
          
          Sys.sleep(1)
          list(src = filename2, width = '100%', height = '85%', aligin = 'center')
          
        } else if (input$time == 'year'){
          
          variable        = data %>% filter(option == input$variable) %>% select(option2)
          
          filename2 = paste0('boxplot_yearly/' ,'boxplot_', unique(variable), '_', input$index, '_year.png')
          
          Sys.sleep(1)
          list(src = filename2, width = '100%', height = '85%', aligin = 'center')
          
        } else {
          
          
          variable        = data %>% filter(option == input$variable) %>% select(option2)
          option          = date$when[date$when1 == input$option]
          filename2 = paste0('boxplot_bimonthly/' ,'boxplot_', unique(variable), '_', input$index, '_bimonth_',option ,'.png')
          Sys.sleep(1)
          list(src = filename2, width = '100%', height = '85%', aligin = 'center')  
        }
        
      )
      
      
    }, deleteFile = FALSE)
    
    ########################################################################################
    #### NOTA: SI FUNCIONA, pero algunos indicadores no aparece el holvmoller (ej. PRCTOT) #
    #######################################################################################
    
    output$holvmoller = renderImage({
      
      input$update
      
      isolate(
        
        if(input$variable == 'Temperature'){
          
          index_for_photo_holvmoller = data$index_number[data$Index == input$index]
          
          filename3 = paste0('hovmoller_temp/','index_', index_for_photo_holvmoller, '.png')
          
          Sys.sleep(1)
          list(src = filename3, width = '100%', height = '85%', aligin = 'center')
          
        } else {
          
          filename3 = paste0('hovmoller_prec/' ,input$index, '.png')
          
          Sys.sleep(1)
          list(src = filename3, width = '100%', height = '85%', aligin = 'center')
          
        }
        
        
        
      )
      
    }, deleteFile = FALSE)
    
    
    
    observeEvent(input$show_boxplot, {
      
      if(input$time == 'month'){
        
        
        number_of_month = number_months$num_month[number_months$month == input$option]
        variable        = data %>% filter(option == input$variable) %>% select(option2)
        
        filename4 = paste0('https://raw.githubusercontent.com/malfaro2/CIGEFI_shiny/main/','boxplot_monthly/','boxplot_' , unique(variable) ,'_', input$index,'_mes', number_of_month, '.png')
        url4=  paste0('<img src="', filename4 ,'" width="850" height="600">')
        showModal(modalDialog(
          title = 'Boxplot',
          HTML(url4), easyClose = TRUE, footer = NULL,size = c("l")))
        
        
        
      } else if (input$time == 'year'){
        
        variable        = data %>% filter(option == input$variable) %>% select(option2)
        
        filename4 = paste0('https://raw.githubusercontent.com/malfaro2/CIGEFI_shiny/main/','boxplot_yearly/' ,'boxplot_', unique(variable), '_', input$index, '_year.png')
        url4=  paste0('<img src="', filename4 ,'" width="850" height="600" >')
        showModal(modalDialog(
          title = 'Boxplot',
          HTML(url4), easyClose = TRUE, footer = NULL,size = c("l")))
        
      } else {
        
        variable        = data %>% filter(option == input$variable) %>% select(option2)
        
        filename4 = paste0('https://raw.githubusercontent.com/malfaro2/CIGEFI_shiny/main/','boxplot_bimonthly/' ,'boxplot_', unique(variable), '_', input$index, '_bimonth_',input$option ,'.png')
        url4=  paste0('<img src="', filename4 ,'" width="850" height="600" >')
        showModal(modalDialog(
          title = 'Boxplot',
          HTML(url4), easyClose = TRUE, footer = NULL,size = c("l")))
        
      }
      
    })
    
    
    
    observeEvent(input$show_holvmoller, {
      
      
      if(input$variable == 'Temperature'){
        
        index_for_photo_holvmoller = data$index_number[data$Index == input$index]
        
        filename5 = paste0('https://raw.githubusercontent.com/malfaro2/CIGEFI_shiny/main/','hovmoller_temp/','index_', index_for_photo_holvmoller, '.png')
        url5 =  paste0('<img src="', filename5 ,'"  width="850" height="600"  >')
        showModal(modalDialog(
          title = 'Boxplot',
          HTML(url5), easyClose = TRUE, footer = NULL, size = c("l")))
        
      } else {
        
        
        index_for_photo_holvmoller = data$index_number[data$Index == input$index]
        filename4 = paste0('https://raw.githubusercontent.com/malfaro2/CIGEFI_shiny/main/','hovmoller_prec/' ,input$index, '.png')
        url4 =  paste0('<img src="', filename4 ,'" width="850" height="600" >')
        showModal(modalDialog(
          title = 'Boxplot',
          HTML(url4), easyClose = TRUE, footer = NULL, size = c("l")))
      }
      
    })
    
    
  }
)


