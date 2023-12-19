

library(sf)
library(rjson)
library(data.table)
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(lubridate)
library(plotly)
library(promises)
library(future)
library(RQuantLib)
library(viridis)

plan(multisession)
# if(.Platform$GUI=="RStudio"){
#   library(rstudioapi)
#   getActiveDocumentContext()$path %>%
#     dirname() %>%
#     setwd()
# }

dados_p1<-try(fread("https://drive.google.com/uc?export=download&id=1IVSzlqVI5hEhTbsDD3Z4UbkQ-s_GKcbd"))
if("try-error"%in%class(dados_p1)){
  dados_p1<-fread("dados_p1.csv")
}

dados_p23<-fread("https://drive.google.com/uc?export=download&id=1IVbGofkJRNsgajLYdYTLZRGI7i96KGCr") %>% 
  filter(!is.na(dia)) %>% 
  mutate(bday=dia%in%getBusinessDayList("Brazil",from=min(dia),to=max(dia)))
if("try-error"%in%class(dados_p23)){
  dados_p23<-fread("dados_p23.csv") %>% 
    mutate(bday=dia%in%getBusinessDayList("Brazil",from=min(dia),to=max(dia)))
}
dados_p1<-dados_p1 %>% 
  mutate(Camera=ifelse(
    str_detect(str_to_upper(Camera),"BEZERRA|PARCIVAL|BEIRA MA"),
    paste0(id_camera,"\t",Camera),
    Camera
  ))

dados_p23<-dados_p23 %>% 
  mutate(Camera=ifelse(
    str_detect(str_to_upper(Camera),"BEZERRA|PARCIVAL|BEIRA MA"),
    paste0(id_camera,"\t",Camera),
    Camera
  ))

get_diario<-function(){
  diario<-fromJSON(file="https://us-central1-contagem-ec7ed.cloudfunctions.net/contagemDiaria") %>% 
    rbindlist()
  diario$sentido[str_detect(diario$camera,"HERACLITO G")]<-c("LO1","LO2")
  diario %>% 
    mutate(camera=ifelse(str_detect(camera,"Beiramar"),"Av Beira Mar",camera)) %>% 
    mutate(Via=str_sub(camera,start=str_locate(camera,"\t")[,1]+1)) %>% 
    mutate(Via=ifelse(is.na(Via),camera,Via)) %>% 
    mutate(Via=str_to_title(Via)) %>% 
    group_by(Via,sentido) %>% 
    reframe(n=median(value)) %>% 
    group_by(Via) %>% 
    reframe(Volume=sum(n)) %>% 
    mutate(Via=fct_reorder(Via,Volume)) %>% 
    return()
}


get_today<-function(){
  leitura<-rjson::fromJSON(file=paste0("https://us-central1-contagem-ec7ed.cloudfunctions.net/dadosBrutos?start=",today("America/Sao_Paulo"),"&end=",today("America/Sao_Paulo")))
  
  leitura<-as.data.frame(do.call(rbind,leitura))
  leitura$hora<-unlist(lapply(leitura$hora,function(x){
    as.POSIXct(as.numeric(paste0(x$`_seconds`,".",format(x$`_nanoseconds`,scientific=F))),origin = "1970-01-01",tz="America/Sao_Paulo")
  }))
  leitura$id_camera<-unlist(leitura$id_camera)
  leitura<-leitura %>% 
    mutate(hora=as.POSIXct(hora,origin = "1970-01-01",tz="America/Sao_Paulo")) %>% 
    mutate(interval=floor_date(hora,"15 minutes")) %>% 
    group_by(id_camera,interval) %>% 
    reframe(n=n())
  left_join(
    leitura,
    dados_p23 %>% select(id_camera,Camera),
    by="id_camera"
  ) %>% 
    return()
}

ui<-navbarPage(
  title = "Acompanhamento analíticos",
  useShinyjs(),
  tabPanel(
    title = "Rankings",
    fluidRow(
      plotlyOutput(outputId = "rank_now")
    ),
    fluidRow(
      plotlyOutput(outputId = "rank_last7")
    )
  ),
  tabPanel(
    title = "Consistência por câmera",
    selectInput(inputId = "cam_id",label = "Câmera",choices = dados_p23$Camera,
                selected = 2),
    fluidRow(
      plotlyOutput(outputId = "tile_day_time",height = "800px")
    ),
    fluidRow(
      plotlyOutput(outputId = "line_bday")
    ),
    fluidRow(
      plotlyOutput(outputId = "line_fday")
    ),
    fluidRow(
      plotlyOutput(outputId = "line_today")
    )
    
  )
)

per_mean<-function(x){
  mean(x[x>quantile(x,0.4,na.rm=T)],na.rm=T)
}
per_sd<-function(x){
  sd(x[x>quantile(x,0.4,na.rm=T)],na.rm=T)
}



server<-function(input,output,session){
  
  data<-reactiveValues()
  
  observeEvent(input$cam_id,{
    data$today<-get_today()
  },ignoreInit = T,once = TRUE,priority = 10)
  
    
  
  ##Consistência
  
  observeEvent(input$cam_id,{
    p1<-dados_p1 %>% 
      filter(Camera==input$cam_id) %>% 
      mutate(interval=as.POSIXct(interval,format="%H:%M")) %>% 
      mutate(interval=as.numeric(paste0(hour(interval),".",minute(interval)))) %>% 
      ggplot(aes(x=dia,y=interval))+
      geom_tile(aes(fill=n),height = 0.5)+
      scale_fill_viridis(name="Observações")+
      scale_x_date(date_breaks = "1 day",date_labels = "%d/%b",expand = c(0,0))+
      theme_minimal()+
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 89,color = "black"),
        axis.text.y = element_text(color = "black")
      )+
      labs(title = paste0("Consitência ao longo dos dias\n",input$cam_id),
           x="Data",y="Hora")
    
    output$tile_day_time<-renderPlotly({
      ggplotly(p1)
    })
    
    
    p2<-dados_p23 %>% 
      filter(Camera==input$cam_id) %>%
      filter(bday) %>% 
      mutate(n_mean=per_mean(n),
             n_sd=per_sd(n),
             n_1sd=n_mean-n_sd,
             n_2sd=n_mean-2*n_sd) %>% 
      ggplot(aes(x=dia,y=n))+
      geom_line(aes(color="Dados observados"),size=1)+
      geom_point()+
      geom_line(aes(x=dia,y=n_1sd,color="1º desvio padrão"))+
      geom_line(aes(y=n_2sd,color="2º desvio padrão"))+
      theme_minimal()+
      scale_color_manual(breaks = c("Dados observados","1º desvio padrão","2º desvio padrão"),
                         values = c("#00BFC4","#F88A27","#C02942"),
                         name="Legenda")+
      scale_x_date(breaks = seq(min(dados_p23$dia),max(dados_p23$dia),by="2 days"),
                   date_labels = "%d/%b")+
      theme(
        axis.text.x = element_text(angle = 89,color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_blank(),
        legend.position = "bottom"
      )+
      labs(title = paste0("Volume diário dia útil\n",input$cam_id),y="Observações")
    
    output$line_bday<-renderPlotly({
      ggplotly(p2)
    })
    
    
    p3<-dados_p23 %>% 
      filter(Camera==input$cam_id) %>%
      filter(!bday) %>% 
      mutate(n_mean=per_mean(n),
             n_sd=per_sd(n),
             n_1sd=n_mean-n_sd,
             n_2sd=n_mean-2*n_sd) %>% 
      ggplot(aes(x=dia,y=n))+
      geom_line(aes(color="Dados observados"),size=1)+
      geom_point()+
      geom_line(aes(x=dia,y=n_1sd,color="1º desvio padrão"))+
      theme_minimal()+
      scale_color_manual(breaks = c("Dados observados","1º desvio padrão"),values = 
                           c("#00BFC4","#F88A27"),name="Legenda")+
      scale_x_date(breaks = seq(min(dados_p23$dia),max(dados_p23$dia),by="2 days"),
                   date_labels = "%d/%b")+
      theme(
        axis.text.x = element_text(angle = 89,color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_blank(),
        legend.position = "bottom"
      )+
      labs(title = paste0("Volume diário dia não útil\n",input$cam_id),y="Observações")
    
    output$line_fday<-renderPlotly({
      ggplotly(p3)
    })
    
    p4<-data$today %>% 
      filter(Camera==input$cam_id)
    if(nrow(p4)==0){
      p4<-ggplot(p4,aes(x=1,y=1))+geom_text(aes(label="Sem dados"),color="red")+theme_void()
    }else{
      p4<-ggplot(p4,aes(x=interval,y=n)) +
        geom_line(aes(color="Dados observados"),size=1,color="#F88A27")+
        geom_point(size=0.75)+
        theme_minimal()+
        scale_x_datetime(breaks = seq(min(data$today$interval)+1,max(data$today$interval),by=30*60)-1,
                         date_labels = "%H:%M")+
        theme(
          axis.text.x = element_text(angle = 89,color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.title.x = element_blank(),
          legend.position = "bottom"
        )+
        labs(title = paste0("Volume de hoje\n",input$cam_id),y="Observações")
    }
      
    
    output$line_today<-renderPlotly({
      ggplotly(p4)
    })
    
  },ignoreInit = T)
  
  
  
  ### Tempo real
  
  
  timer<-reactiveTimer(15000)
  
  observe({
    timer()
    
    
    
    output$rank_now<-renderPlotly({
      future_promise({
        get_diario()
      }) %...>% {
        b<-ggplot(., aes(x=Volume,y=Via))+
          geom_bar(stat = 'identity',alpha=.6,position = position_dodge2(width = 1.4),fill="#FF7900")+
          geom_text(aes(label=Volume))+
          theme_minimal()+
          labs(title = " ",subtitle = " ",x="",y="")+
          theme(title = element_text(size=16,margin=margin(0,0,0,0)),
                plot.title = element_text(margin = margin(0,0,0,0)),
                axis.text.y = element_text(size=15),
                axis.text.x = element_blank(),
                panel.grid = element_blank(),
                legend.position = "bottom") 
        ggplotly(b,tooltip = c("Via","Volume")) %>%
          layout( margin=list(t = 75),
                  legend=list(orientation="h",x=-0.2),
                  title = list(text = paste0('Ranking Volumétrico ',str_to_title(lubridate::wday(today(tzone = "America/Sao_Paulo"),label = T,abbr = F)),
                                             " ",
                                             format(today(tzone = "America/Sao_Paulo"),"%d-%b"),
                                             '<br>',
                                             '<sup>',
                                             "Ultima atualização: ",format(now(tzone = "America/Sao_Paulo"),format="%H:%M:%S"),
                                             '</sup>')))
      }
      
    })
    
    
  })
  
  ##7dias
  output$rank_last7<-renderPlotly({
    p2<-dados_p23 %>% 
      filter(dia%in%getBusinessDayList("Brazil",from=Sys.Date()-10,to=Sys.Date())) %>% 
      mutate(Via=ifelse(str_detect(Camera,"\t"), str_sub(Camera,start=str_locate(Camera,"\t")[,2]+1) ,Camera) ) %>% 
      mutate(Via=str_to_title(Via)) %>% 
      group_by(Via,Sentido) %>% 
      reframe(Volume=mean(n)) %>% 
      group_by(Via) %>% 
      reframe(Volume=sum(Volume)) %>% 
      filter(!is.na(Via)) %>% 
      mutate(Volume=trunc(Volume)) %>% 
      mutate(Via=fct_reorder(Via,Volume)) %>% 
      ggplot( aes(x=Volume,y=Via))+
      geom_bar(stat = 'identity',alpha=.6,position = position_dodge2(width = 1.4),fill="#FF7900")+
      geom_text(aes(label=Volume))+
      theme_minimal()+
      labs(title = " ",subtitle = " ",x="",y="")+
      theme(title = element_text(size=16,margin=margin(0,0,0,0)),
            plot.title = element_text(margin = margin(0,0,0,0)),
            axis.text.y = element_text(size=15),
            axis.text.x = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom") 
    ggplotly(p2,tooltip = c("Via","Volume")) %>%
      layout( margin=list(t = 75),
              legend=list(orientation="h",x=-0.2),
              title = list(text = paste0('Ranking Volumétrico últimos 7 dias')))
  })
  
  
}


shinyApp(ui, server)








