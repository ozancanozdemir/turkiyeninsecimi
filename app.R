# Seçim 2023 Verileri 

library(rvest)
library(htmlTable)
library(tidyverse)
library(ggthemes)
library(shiny)
library(shinythemes)
library(rvest)
library(htmltools)
library(htmlTable)
library(shinyLP)
library(lubridate)
library(ggrepel)
library(coalitions)
anket_sonuc<-openxlsx::read.xlsx("https://github.com/ozancanozdemir/ozancanozdemir.github.io/raw/master/anket_sonuc.xlsx")
il_il_vekil<-openxlsx::read.xlsx("https://github.com/ozancanozdemir/ozancanozdemir.github.io/raw/master/il_il_vekil.xlsx")
cb_son<-openxlsx::read.xlsx("https://github.com/ozancanozdemir/ozancanozdemir.github.io/raw/master/cb_son.xlsx")
cb_2tur<-openxlsx::read.xlsx("https://github.com/ozancanozdemir/ozancanozdemir.github.io/raw/master/cb_son2.xlsx")
sayısal<-c("Ocak"=1,"Şubat"=2,"Mart"=3,"Nisan"=4,"Mayıs"=5,"Haziran"=6,
           "Temmuz"=7,"Ağustos"=8,"Eylül"=9,"Ekim"=10,"Kasım"=11,"Aralık"=12)
colnames(anket_sonuc)[23]<-"Ay_Yil"
anket_sonuc$Ay_Sayısal<-as.vector(sayısal[anket_sonuc$Ay])
anket_sonuc$Tarih1<-strftime(paste(anket_sonuc$Yıl,anket_sonuc$Ay_Sayısal, 1,sep="-"), "%Y-%m-%d")
#anket_sonuc$Tarih1<-lubridate::dmy(paste(1,anket_sonuc$Ay_Yıl))
#anket_sonuc$Tarih1<-seq(as.Date("1910/1/1"), as.Date("1911/2/7"), "days")
anket_sonuc$Ay<- factor(anket_sonuc$Ay,levels = 
                          c("Ocak","Şubat","Mart","Nisan","Mayıs","Haziran","Temmuz","Ağustos","Eylül","Ekim","Kasım","Aralık"))


#anket_sonuc1<-anket_sonuc[,c(4:20,24)]%>%pivot_longer(!Tarih1, names_to = "Parti", values_to = "Oy Oranı")
anket_sonuc1<-anket_sonuc[,c(2,4:20,24,25)]%>%pivot_longer(!c(Tarih1,Anket.şirketi), names_to = "Parti", values_to = "Oy Oranı")
#anket_sonuc1$Tarih2<-lubridate::dmy(paste(1,anket_sonuc$Ay_Yıl))
#anket_sonuc1$x<-rank(anket_sonuc1$Tarih1)
#anket_sonuc1$Tarih1<-lubridate::dmy(paste(1,anket_sonuc1$Ay_Yıl))
parti_isim <- colnames(anket_sonuc)[4:20]
aday_isim<-colnames(cb_son)[4:7]
mean_fnc <- function(x){mean(x,na.rm=T)}
oy_tablosu_ortalama <- aggregate(anket_sonuc1$`Oy Oranı`,list(anket_sonuc1$Tarih1,anket_sonuc1$Parti),mean_fnc)
colnames(oy_tablosu_ortalama)<-c("Tarih1","Parti","Ortalama")
#oy_tablosu_ortalama<- oy_tablosu_ortalama%>%right_join(anket_sonuc1[,c(1,4)],by="Tarih1",multiple = "all")
oy_tablosu_ortalama<-oy_tablosu_ortalama%>%distinct()
oy_tablosu_ortalama$Tarih2<-lubridate::ymd(oy_tablosu_ortalama$Tarih1)
oy_tablosu_ortalama$Ay_Yıl <- format(oy_tablosu_ortalama$Tarih2, "%Y-%m")
oy_tablosu_ortalama<-oy_tablosu_ortalama%>%select(-Tarih2)

#cb_son1<-cb_son[,c(4:7,9)]%>%pivot_longer(!Tarih1, names_to = "Aday", values_to = "Oy Oranı")
cb_son1<-cb_son[,c(2,4:7,9)]%>%pivot_longer(!c(Tarih1, Anket.Şirketi),names_to = "Aday", values_to = "Oy Oranı")
sirket_listesi1<-unique(cb_son1$Anket.Şirketi)
names(sirket_listesi1)<-unique(cb_son1$Anket.Şirketi)
cb_son2<-cb_son[,c(4:7,10)]%>%pivot_longer(!Ay, names_to = "Aday", values_to = "Oy Oranı")
cb_ortalama<-aggregate(cb_son2$`Oy Oranı`,list(cb_son2$Ay,cb_son2$Aday),mean_fnc)
colnames(cb_ortalama)<-c("Ay","Aday","Ortalama")
cb_ortalama$Ay<-factor(cb_ortalama$Ay,levels =c("Mart","Nisan","Mayıs"))

cb_2tur1<-cb_2tur[,c(2,4,5,6)]%>%pivot_longer(!c(Tarih1, Anket.Şirketi),names_to = "Aday", values_to = "Oy Oranı")
sirket_listesi11<-unique(cb_2tur1$Anket.Şirketi)
names(sirket_listesi11)<-unique(cb_2tur1$Anket.Şirketi)
cb_2tur2<-cb_2tur[,c(4,5,7)]%>%pivot_longer(!Ay, names_to = "Aday", values_to = "Oy Oranı")
cb_ortalama2<-aggregate(cb_2tur2$`Oy Oranı`,list(cb_2tur2$Ay,cb_2tur2$Aday),mean_fnc)
colnames(cb_ortalama2)<-c("Ay","Aday","Ortalama")
cb_ortalama2$Ay<-factor(cb_ortalama2$Ay,levels =c("Mart","Nisan","Mayıs"))
## İttifak 

#oy_tablosu_ortalama$Tarih1<-as.Date(oy_tablosu_ortalama$Tarih1,format = "%d.%m.%Y")
#oy_tablosu_ortalama$Tarih2<-rep(seq(as.Date("2023/1/1"), as.Date("2023/2/8"), "days"),17)
#oy_tablosu_ortalama$x1<-oy_tablosu_ortalama$x
#oy_tablosu1<-unique(oy_tablosu_ortalama)
#oy_tablosu_ortalama$Tarih1<-with_tz(oy_tablosu_ortalama$Tarih1, tzone = "America/Los_Angeles") 

#anket_sonuc1$Tarih1<-with_tz(anket_sonuc1$Tarih1, tzone = "America/Los_Angeles") 
#oy_tablosu_ortalama$Ay_Yıl <- format(oy_tablosu_ortalama$Tarih1, "%Y-%m")
#oy_tablosu_ortalama$Parti<-factor(oy_tablosu_ortalama$Parti, levels = names(oy_tablosu)[2:14])


chp_ort<-oy_tablosu_ortalama%>%filter(Parti=="CHP")
iyi_ort<-oy_tablosu_ortalama%>%filter(Parti=="İYİ")
sp_ort<-oy_tablosu_ortalama%>%filter(Parti=="SP")
deva_ort<-oy_tablosu_ortalama%>%filter(Parti=="DEVA")
gp_ort<-oy_tablosu_ortalama%>%filter(Parti=="GP")
dp_ort<-oy_tablosu_ortalama%>%filter(Parti=="DP")

mi_oy<-cbind(chp_ort$Ortalama,iyi_ort$Ortalama,sp_ort$Ortalama,deva_ort$Ortalama,gp_ort$Ortalama,dp_ort$Ortalama)
mi_oy1<-data.frame(cbind(chp_ort$Tarih1,round(rowSums(mi_oy,na.rm = T),3),chp_ort$Ay_Yıl))
colnames(mi_oy1)<-c("Tarih1","Ortalama","Ay_Yıl")
mi_oy1$İttifak<-rep("Millet İttifakı",nrow(mi_oy1))

akp_ort<-oy_tablosu_ortalama%>%filter(Parti=="AKP")
mhp_ort<-oy_tablosu_ortalama%>%filter(Parti=="MHP")
bbp_ort<-oy_tablosu_ortalama%>%filter(Parti=="BBP")
yrp_ort<-oy_tablosu_ortalama%>%filter(Parti=="YRP")


ci_oy<-cbind(akp_ort$Ortalama,mhp_ort$Ortalama,bbp_ort$Ortalama,yrp_ort$Ortalama)
ci_oy1<-data.frame(cbind(akp_ort$Tarih1,round(rowSums(ci_oy,na.rm = T),3),akp_ort$Ay_Yıl))
colnames(ci_oy1)<-c("Tarih1","Ortalama","Ay_Yıl")
ci_oy1$İttifak<-rep("Cumhur İttifakı",nrow(ci_oy1))

ysgp_ort<-oy_tablosu_ortalama%>%filter(Parti=="YSP")
tip_ort<-oy_tablosu_ortalama%>%filter(Parti=="TİP")


eöi_oy<-cbind(ysgp_ort$Ortalama,tip_ort$Ortalama)
eöi_oy1<-data.frame(cbind(ysgp_ort$Tarih1,round(rowSums(eöi_oy,na.rm = T),3),ysgp_ort$Ay_Yıl))
colnames(eöi_oy1)<-c("Tarih1","Ortalama","Ay_Yıl")
eöi_oy1$İttifak<-rep("Emek ve Özgürlük İttifakı",nrow(eöi_oy1))


zp_ort<-oy_tablosu_ortalama%>%filter(Parti=="ZP")


ata_oy<-cbind(zp_ort$Ortalama)
ata_oy1<-data.frame(cbind(zp_ort$Tarih1,round(rowSums(ata_oy,na.rm = T),3),zp_ort$Ay_Yıl))
colnames(ata_oy1)<-c("Tarih1","Ortalama","Ay_Yıl")
ata_oy1$İttifak<-rep("Ata İttifakı",nrow(ata_oy1))

ittifak_ortalama_data<-rbind(mi_oy1,ci_oy1,eöi_oy1,ata_oy1)
ittifak_ortalama_data$Ortalama<-as.numeric(ittifak_ortalama_data$Ortalama)

ittifak_listesi<-c("Millet İttifakı","Cumhur İttifakı","Emek ve Özgürlük İttifakı","Ata İttifakı")
sirket_listesi<-unique(anket_sonuc1$Anket.şirketi)
names(sirket_listesi) <- unique(anket_sonuc1$Anket.şirketi)
# UI tasarımı oluştur
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(title = "Türkiye Seçim 2023",
                tabPanel("Partilerin Oy Oranları",
                sidebarLayout(
                  sidebarPanel(
                #selectInput("parti_secim", "Parti Seçimi:", parti_isim),
                selectizeInput("parti_secim", "Parti Seçimi:", choices = parti_isim, multiple = TRUE)
                ,checkboxGroupInput("anket", label = h3("Anket Şirketi"), 
                                     choices = sirket_listesi,
                                     selected = as.vector(unique(anket_sonuc1$Anket.şirketi)))),
                mainPanel(
                  textOutput("countdown"),
                  plotOutput("oy_oranlari_grafik",width  =1000,height =500)
                )
                )
                ),
                tabPanel("Partilerin Oy Ortalaması",
                         sidebarLayout(
                           sidebarPanel(
                             #selectInput("parti_secim", "Parti Seçimi:", parti_isim),
                             selectizeInput("parti_secim", "Parti Seçimi:", choices = parti_isim, multiple = TRUE)
                           ),
                           mainPanel(
                             plotOutput("ortalama_grafik",width  = 1000,height =800)
                           )
                         )
                ),
                tabPanel("İttifakların Oy Ortalaması",
                         sidebarLayout(
                           sidebarPanel(
                             #selectInput("parti_secim", "Parti Seçimi:", parti_isim),
                             selectizeInput("ittifak_secim", "İttifak Seçimi:", choices = ittifak_listesi, multiple = TRUE)
                           ),
                           mainPanel(
                             textOutput("countdown1"),
                             plotOutput("ortalama_grafik_ittifak",width  = 800,height =1000)
                           )
                         )
                ),
                tabPanel("Partilerin Oy Ortalaması(Tablo)",
                         tableOutput("ortalama")),
                         
                tabPanel("Cumhurbaşkanlığı Seçimi",
                         sidebarLayout(
                           sidebarPanel(
                             #selectInput("parti_secim", "Parti Seçimi:", parti_isim),
                             selectizeInput("aday_secim", "Aday Seçimi:", choices = aday_isim, multiple = TRUE),
                             checkboxGroupInput("sirket1", label = h3("Anket Şirketi"), 
                                                choices = sirket_listesi1,
                                                selected = as.vector(unique(cb_son1$Anket.Şirketi)))
                           ),
                           mainPanel(
                             textOutput("countdown2"),
                             plotOutput("cb"),
                             plotOutput("cb1"),
                             plotOutput("cb2"),
                             plotOutput("cb3")
                           )
                         )
               ),
                
               
                tabPanel("D'Hondt",
                         sidebarLayout(
                           sidebarPanel(
                         selectInput("il", "İl Seçin",
                                     il_il_vekil$İl),
                         numericInput("akp", "AKP", 0, min = 0, max = 100),
                         numericInput("chp", "CHP", 0, min = 0, max = 100),
                         numericInput("iyi", "İYİ Parti", 0, min = 0, max = 100),
                         numericInput("hdp", "HDP", 0, min = 0, max = 100),
                         numericInput("mhp", "MHP", 0, min = 0, max = 100),
                         numericInput("tip", "TİP", 0, min = 0, max = 100),
                         numericInput("gp", "Gelecek", 0, min = 0, max = 100),
                         numericInput("deva", "DEVA", 0, min = 0, max = 100),
                         numericInput("saadet", "SP", 0, min = 0, max = 100),
                         numericInput("dp", "DP", 0, min = 0, max = 100),
                         numericInput("yrp", "YRP", 0, min = 0, max = 100),
                         numericInput("mp", "MP", 0, min = 0, max = 100),
                         numericInput("zp", "ZP", 0, min = 0, max = 100),
                           ),
                         mainPanel(
                           textOutput("toplamkoltuk"),
                           fluidRow(
                             column(6,
                                    tableOutput("dagilim"),
                             ),
                             column(5,
                                    plotOutput("koltukgrafik"))
                          
                         ))
                         
                )),
               tabPanel("Verisi Kullanılan Kamuoyu Şirketleri",
                        tableOutput("sirket")
               ),
                  #textOutput("countdown"),
                  #tabsetPanel(type = "tabs",
                # tabPanel("Partilerin Oy Oranları", value  = 4,style = "font-size: 8px;",plotOutput("oy_oranlari_grafik",width  =1000,height =500)),
                #tabPanel("Partilerin Oy Oran Ortalaması/Aylara Göre",plotOutput("ortalama_grafik",width  = 1000,height =800)),
                # tabPanel("Partilerin Oy Oran Ortalaması/Aylara Göre(Tablo)",tableOutput("ortalama")),
                #tabPanel("Verisi Kullanılan Kamuoyu Şirketleri",tableOutput("sirket"))),
                  br(),
                  div(h3(strong("Hakkinda"),style="color:darkred")),
                  p("Bu uygulama Wikipedia'da yer alan güncel seçim anketleri sayfasında yer alan  verileri kullanmaktadır. Veriler her hafta güncellenecektir."),
                  p(paste("Son güncellenme tarihi",Sys.Date())),
                  a(p("Sayfayı incelemek için tıklayın."),href="https://tr.wikipedia.org/wiki/Bir_sonraki_T%C3%BCrkiye_genel_se%C3%A7imleri_i%C3%A7in_yap%C4%B1lan_anketler"),
                  div(h5(strong("Gelistiren"),style="color:darkred")),
                  a(h5("Ozancan Ozdemir"),href="https://users.metu.edu.tr/ozancan",target="_blank"),
                  p("Orta Dogu Teknik Universitesi Istatistik Bolumu Arastirma Gorevlisi & Doktora ogrencisi"),
                  div(h5(strong("Soru ve Gorusleriniz Icin")),style="color:darkred"),
                  p("ozancan@metu.edu.tr"))
                
)


# Server fonksiyonu oluştur
server <- function(input, output,session) {
  
  # Seçilen partiler için oy oranı verilerini filtrele
   filtered_data <- reactive({
      anket_sonuc1 %>%
  dplyr::filter(Parti %in% input$parti_secim)%>%filter(Anket.şirketi %in% input$anket)
   })
   
   filtered_data1 <- reactive({
     oy_tablosu_ortalama %>%
       dplyr::filter(Parti %in% input$parti_secim)
   })
  
   
   filtered_data3 <- reactive({
     ittifak_ortalama_data %>%
       dplyr::filter(İttifak %in% input$ittifak_secim)
   })
   
    filtered_data2 <-reactive({
    oy_tablosu_ortalama %>%
        dplyr::filter(Parti %in% input$parti_secim)
      
    data_end <- reactive({
        oy_tablosu_ortalama %>%
          dplyr::filter(Parti %in% input$parti_secim)%>%last()
      })
  })
    
    cb_filtered<-reactive({
      cb_son1 %>%
        dplyr::filter(Aday %in% input$aday_secim)%>%filter(Anket.Şirketi %in% input$sirket1)
    })
    
    cb_filtered1<-reactive({
     cb_ortalama %>%
        dplyr::filter(Aday %in% input$aday_secim)
    })
    
    
    cb_filtered2<-reactive({
      cb_2tur1 %>%
        dplyr::filter(Aday %in% input$aday_secim)
    })
    
    cb_filtered3<-reactive({
      cb_ortalama2 %>%
        dplyr::filter(Aday %in% input$aday_secim)
    })
  # Oy oranları grafiğini çiz
    
  output$countdown<-renderText({
    paste("Seçime Kalan Süre",as.period(as.Date("2023-05-14") - Sys.Date(), unit = "day"))
  })
  output$countdown1<-renderText({
    paste("Seçime Kalan Süre",as.period(as.Date("2023-05-14") - Sys.Date(), unit = "day"))
  })
  output$countdown2<-renderText({
    paste("Seçime Kalan Süre",as.period(as.Date("2023-05-14") - Sys.Date(), unit = "day"))
  })
  
  output$oy_oranlari_grafik <- renderPlot({
    ggplot(filtered_data(),aes(x=Tarih1, y = `Oy Oranı`, color = Parti))+geom_point(alpha = 0.5) +
    #ggplot(data = filtered_data1(), aes(x = Tarih1, y =Ortalama, group= Parti,color = Parti))+geom_line()+
     geom_line(data = filtered_data1(), aes(x = Tarih1, y =Ortalama, group= Parti,color = Parti), linewidth=1.25)+
      geom_point(data = filtered_data1(),aes(x =Tarih1, y =Ortalama),color = "black",size =1.2)+
      labs(x = "Tarih", y = "Oy Oranı", color = "Parti") +
      theme_fivethirtyeight()+ 
      #geom_text(filtered_data1(),mapping=aes(x = tail(Tarih1, n = 1), y = tail(Ortalama, n = 1),  label = paste("%",round(tail(Ortalama, n = 1),2))),
      #                                   vjust =-0.35,size = 5,color ="black",fontface="bold",hjust = 0.75)+
      labs(title = "Partilerin Seçim Anketlerine Göre Oy Dağılımı",subtitle = "Bu grafikte Ocak 2020'den itibaren  kamuya açık paylaşılan araştırma sonuçları ve onları aylara göre ortalamaları gösterilmiştir.\nSiyah nokta ile gösterilen değerler ortalama değerleridir.",caption = "@OzancanOzdemir")+xlab("Tarih")+ylab("Oy Oranı")+
      scale_y_continuous(labels= function(x)paste("%",x))+
      theme(rect = element_rect(fill = "white",linetype = 0, colour = NA),
            axis.title = element_text(size =10,face ="bold"),
            axis.text = element_text(face ="bold"),
            plot.caption = element_text(face ="bold"),plot.title = element_text(size= 18),
            legend.position = "top",axis.text.x =  element_text(size = 7,angle = 90))+ geom_hline(yintercept = 0, size = 1, colour = "black")
    
    
  })
  
  output$ortalama_grafik<-renderPlot({
    ggplot(filtered_data1(), aes(x = Ay_Yıl, y =  Ortalama, fill = Parti))+geom_bar(stat = "identity",position = position_dodge(width = 0.8))+
      geom_text(aes(label=paste("%",round(Ortalama,1))),fontface ="bold",size = 3,position = position_dodge(width = 1))+
      labs(title ="Ortalama Oy Oranları",subtitle = "Ocak 2020'den itibaren her ay için bulabilirsiniz.",caption = "@OzancanOzdemir")+labs(x = "Ay", y = "Oy Oranı", color = "Parti") +
      theme_fivethirtyeight()+scale_y_continuous(labels= function(x)paste("%",x))+
      theme(rect = element_rect(fill = "white",linetype = 0, colour = NA),
            axis.title = element_text(size =12,face ="bold"),
            axis.text = element_text(face ="bold"),axis.text.x = element_text(angle = 90),
            plot.caption = element_text(face ="bold"),plot.title = element_text(size= 18),
            legend.position = "top")+ geom_hline(yintercept = 0, size = 1, colour = "black")
    
  })
  
  output$ortalama_grafik_ittifak<-renderPlot({
    ggplot(filtered_data3(),aes(x=Tarih1, y = Ortalama, color = İttifak))+geom_point(color = "black",size =1.25) +
      #ggplot(data = filtered_data1(), aes(x = Tarih1, y =Ortalama, group= Parti,color = Parti))+geom_line()+
      geom_line(data = filtered_data3(),aes(x=Tarih1, y = Ortalama, group=İttifak,color = İttifak), linewidth=1.25)+
      labs(x = "Tarih", y = "Oy Oranı", color = "İttifak") +
      theme_fivethirtyeight()+ 
      #geom_text(filtered_data1(),mapping=aes(x = tail(Tarih1, n = 1), y = tail(Ortalama, n = 1),  label = paste("%",round(tail(Ortalama, n = 1),2))),
      #                                   vjust =-0.35,size = 5,color ="black",fontface="bold",hjust = 0.75)+
      labs(title = "İttifakların Ortalama Oy Oranları",subtitle = "Bu grafikte Ocak 2020'den itibaren  kamuya açık paylaşılan araştırma sonuçları ve onları aylara göre ortalamaları gösterilmiştir.\nSiyah nokta ile gösterilen değerler ortalama değerleridir.",caption = "@OzancanOzdemir")+xlab("Tarih")+ylab("Oy Oranı")+
      scale_y_continuous(labels= function(x)paste("%",x))+
      theme(rect = element_rect(fill = "white",linetype = 0, colour = NA),
            axis.title = element_text(size =10,face ="bold"),
            axis.text = element_text(face ="bold"),
            plot.caption = element_text(face ="bold"),plot.title = element_text(size= 18),
            legend.position = "top",axis.text.x = element_text(size = 7,angle = 90))+ geom_hline(yintercept = 0, size = 1, colour = "black")
    
  })
  # Ortalama oy oranları tablosunu göster
  output$ortalama<- renderTable({
    oy_tablosu_ortalama %>%
    filter(Parti %in% input$parti_secim)%>% spread(Parti,Ortalama)
  })
  
  output$sirket<- renderTable({
    unique(anket_sonuc$Anket.şirketi)
  })
  
  output$toplamkoltuk<-renderText({
    paste(paste(input$il,"ilinin toplam vekil sayısı"),il_il_vekil%>%filter(İl %in% input$il)%>%select(Koltuk)%>%as.numeric())
  })
  
  output$dagilim<- renderTable({
   parti<-c("AKP", "CHP", "İYİ Parti", "HDP",  "MHP",  "TİP", "Gelecek", "DEVA", "SP",  "DP", "YRP", 
                        "MP",  "ZP")
   oy<-c(input$akp,input$chp,  input$iyi, input$hdp, input$mhp, input$tip, input$gp,  input$deva,input$saadet, 
    input$dp,  input$yrp, input$mp, input$zp)
 renk<-c("darkorange","darkred","steelblue","purple","red","brown","green","blue","pink","yellow",
      "darkpink","darkblue","grey")
s<-data.frame(parti,oy,renk)
koltuk<-dHondt(s$oy,s$parti, n_seats = il_il_vekil%>%filter(İl %in% input$il)%>%select(Koltuk)%>%as.numeric())
cbind(parti,koltuk)
 })
  
  output$koltukgrafik<- renderPlot({
     parti<-c("AKP", "CHP", "İYİ Parti", "HDP",  "MHP",  "TİP", "Gelecek", "DEVA", "SP",  "DP", "YRP", 
              "MP",  "ZP")
     oy<-c(input$akp,input$chp,  input$iyi, input$hdp, input$mhp, input$tip, input$gp,  input$deva,input$saadet, 
           input$dp,  input$yrp, input$mp, input$zp)
     renk<-c("darkorange","darkred","steelblue","purple","red","brown","green","blue","pink","yellow",
            "darkpink","darkblue","grey")
    s<-data.frame(parti,oy,renk)
    koltuk<-dHondt(s$oy,s$parti, n_seats = il_il_vekil%>%filter(İl %in% input$il)%>%select(Koltuk)%>%as.numeric())
    data_for_plot<-data.frame(parti,koltuk)
    ggplot(data_for_plot,aes(x=parti,y=koltuk,fill = parti))+geom_bar(stat = "identity")+
      theme_fivethirtyeight()+labs(title = paste(input$il,"ilindeki koltuk dağılımı"),x = "Parti",y ="Koltuk")+
      theme(rect = element_rect(fill = "white",linetype = 0, colour = NA),
           axis.title = element_text(size =12,face ="bold"),
            axis.text = element_text(face ="bold"),axis.text.x = element_text(angle = 90),
            plot.caption = element_text(face ="bold"),plot.title = element_text(size= 18),
            legend.position = "none")+ geom_hline(yintercept = 0, size = 1, colour = "black")
  })
  
  output$cb <- renderPlot({
    ggplot(cb_filtered(),aes(x=Tarih1, y = `Oy Oranı`, color = Aday))+geom_point(alpha = 1) +
      #ggplot(data = filtered_data1(), aes(x = Tarih1, y =Ortalama, group= Parti,color = Parti))+geom_line()+
      labs(x = "Tarih", y = "Oy Oranı", color = "Aday") +
      theme_fivethirtyeight()+ 
      #geom_text(filtered_data1(),mapping=aes(x = tail(Tarih1, n = 1), y = tail(Ortalama, n = 1),  label = paste("%",round(tail(Ortalama, n = 1),2))),
      #                                   vjust =-0.35,size = 5,color ="black",fontface="bold",hjust = 0.75)+
      labs(title = "Cumhurbaşkanlığı Seçim Anketlerine Göre Oy Dağılımı",subtitle = "Bu grafikte Mart 2023'den itibaren  kamuya açık paylaşılan araştırma sonuçları gösterilmiştir.",caption = "@OzancanOzdemir")+xlab("Tarih")+ylab("Oy Oranı")+
      scale_y_continuous(labels= function(x)paste("%",x))+
      theme(rect = element_rect(fill = "white",linetype = 0, colour = NA),
            axis.title = element_text(size =10,face ="bold"),
            axis.text = element_text(face ="bold"),
            plot.caption = element_text(face ="bold"),plot.title = element_text(size= 18),
            legend.position = "top",axis.text.x = element_blank())+ geom_hline(yintercept = 0, size = 1, colour = "black")
    
    
  })
  
  output$cb1 <- renderPlot({
    ggplot(cb_filtered1(), aes(x = Ay, y =  Ortalama, fill = Aday))+geom_bar(stat = "identity",position = position_dodge(width = 0.8),width = 0.5)+
      geom_text(aes(label=paste("%",round(Ortalama,3))),fontface ="bold",size = 5,position = position_dodge(width = 0.75))+
      labs(title ="Cumhurbaşkanlığı Seçim Anketlerine Göre Ortalama Oy Oranları",subtitle = "Bu grafikte Mart 2023'den itibaren kamuya açık paylaşılan araştırma sonuçlarının aylara göre ortalaması gösterilmiştir.",caption = "@OzancanOzdemir")+labs(x = "Ay", y = "Oy Oranı", color = "Aday") +
      theme_fivethirtyeight()+scale_y_continuous(labels= function(x)paste("%",x))+
      theme(rect = element_rect(fill = "white",linetype = 0, colour = NA),
            axis.title = element_text(size =12,face ="bold"),
            axis.text = element_text(face ="bold"),axis.text.x = element_text(angle = 90),
            plot.caption = element_text(face ="bold"),plot.title = element_text(size= 18),
            legend.position = "top")+ geom_hline(yintercept = 0, size = 1, colour = "black")
    
  })
  
  
  output$cb2 <- renderPlot({
    ggplot(cb_filtered2(),aes(x=Tarih1, y = `Oy Oranı`, color = Aday))+geom_point(alpha = 1) +
      #ggplot(data = filtered_data1(), aes(x = Tarih1, y =Ortalama, group= Parti,color = Parti))+geom_line()+
      labs(x = "Tarih", y = "Oy Oranı", color = "Aday") +
      theme_fivethirtyeight()+ 
      #geom_text(filtered_data1(),mapping=aes(x = tail(Tarih1, n = 1), y = tail(Ortalama, n = 1),  label = paste("%",round(tail(Ortalama, n = 1),2))),
      #                                   vjust =-0.35,size = 5,color ="black",fontface="bold",hjust = 0.75)+
      labs(title = "Cumhurbaşkanlığı Seçim Anketlerine Göre 2. Tur Oy Dağılımı",subtitle = "Bu grafikte Mart 2023'den itibaren  kamuya açık paylaşılan araştırma sonuçları gösterilmiştir.",caption = "@OzancanOzdemir")+xlab("Tarih")+ylab("Oy Oranı")+
      scale_y_continuous(labels= function(x)paste("%",x))+
      theme(rect = element_rect(fill = "white",linetype = 0, colour = NA),
            axis.title = element_text(size =10,face ="bold"),
            axis.text = element_text(face ="bold"),
            plot.caption = element_text(face ="bold"),plot.title = element_text(size= 18),
            legend.position = "top",axis.text.x = element_blank())+ geom_hline(yintercept = 0, size = 1, colour = "black")
    
    
  })
  
  output$cb3 <- renderPlot({
    ggplot(cb_filtered3(), aes(x = Ay, y =  Ortalama, fill = Aday))+geom_bar(stat = "identity",position = position_dodge(width = 0.8),width = 0.5)+
      geom_text(aes(label=paste("%",round(Ortalama,3))),fontface ="bold",size = 5,position = position_dodge(width = 0.75))+
      labs(title ="Cumhurbaşkanlığı Seçim Anketlerine Göre 2. Tur Ortalama Oy Oranları",subtitle = "Bu grafikte Mart 2023'den itibaren kamuya açık paylaşılan araştırma sonuçlarının aylara göre ortalaması gösterilmiştir.",caption = "@OzancanOzdemir")+labs(x = "Ay", y = "Oy Oranı", color = "Aday") +
      theme_fivethirtyeight()+scale_y_continuous(labels= function(x)paste("%",x))+
      theme(rect = element_rect(fill = "white",linetype = 0, colour = NA),
            axis.title = element_text(size =12,face ="bold"),
            axis.text = element_text(face ="bold"),axis.text.x = element_text(angle = 90),
            plot.caption = element_text(face ="bold"),plot.title = element_text(size= 18),
            legend.position = "top")+ geom_hline(yintercept = 0, size = 1, colour = "black")
    
  })
}





# Uygulamayı çalıştır
shinyApp(ui = ui, server = server)

