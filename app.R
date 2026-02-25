library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(reshape2)
library(forecast)
options(scipen=999999)

fmcg_forecast <- function(df, in.values, input, country, sector, n, s.type){
  
  forecasted_df <- df
  input.values <- in.values
  
  if(s.type=="sector"){
    final.input <- c(as.numeric(unlist(input.values[1,][input$fmcgCCI])),
                     as.numeric(unlist(input.values[2,][input$fmcgUR])),
                     as.numeric(unlist(input.values[3,][input$fmcgCS])),
                     as.numeric(unlist(input.values[4,][input$fmcgGDP])),
                     as.numeric(unlist(input.values[5,][input$fmcgIR])),
                     as.numeric(unlist(input.values[6,][input$fmcgFCPI]))
    )
  }else if(s.type%in%c("ses","ses.sector")){
    final.input <- c(as.numeric(unlist(input.values[1,][input$fmcgCCI_ses])),
                     as.numeric(unlist(input.values[2,][input$fmcgUR_ses])),
                     as.numeric(unlist(input.values[3,][input$fmcgCS_ses])),
                     as.numeric(unlist(input.values[4,][input$fmcgGDP_ses])),
                     as.numeric(unlist(input.values[5,][input$fmcgIR_ses])),
                     as.numeric(unlist(input.values[6,][input$fmcgFCPI_ses]))
    )
  }
  
  xy <- forecasted_df[,4:ncol(forecasted_df)]
  xx <- xy
  
  xregg<- ts(xy[1:n,c(1:8)], start=c(2021, 1), end = c(2025, 13),frequency=13)
  xregf<- ts(xy[(n+1):nrow(xy),c(1:8)], start=c(2026, 1), end = c(2026, 13),frequency=13)
  
  xregf <- forecasted_df[(n+1):nrow(xy),6:11]
  
  xregf2 <- xregf
  for(i in 1:ncol(xregf2)){
    
    xregf2[,i] <- xregf2[,i] + final.input[i]
    
  }
  
  xregf2 <- cbind(forecasted_df[(n+1):nrow(xy),4:5],xregf2)
  xregf_sim<- ts(xregf2, start=c(2026, 1), end = c(2026, 13),frequency=13)
  
  res2 <- as.data.frame(rep(0,13))
  
  for (i in 9:length(xx)){
    myts <- ts(xx[1:n,i], start=c(2021, 1), end = c(2025, 13),frequency=13)
    myts[myts<=1] <- 1
    
    if(names(xx)[i]%in%c("BEVERAGES")){
      set.seed(66)
      m <- nnetar(myts, xreg= xregg, maxit=150, decay=0.001, repeats = 10)
      f<- forecast(m,h=13, xreg =xregf_sim)
      s<- sd(na.omit(m$fitted))
      error <- qnorm(0.95)*s/sqrt(13)
      fcast<- f$mean + 0.5*error
      #fcast<- f$mean
    }else if(names(xx)[i]%in%c("PACKAGED GROCERY")){
      set.seed(66)
      m <- nnetar(myts, xreg= xregg, maxit=150, decay=0.001, repeats = 10)
      f<- forecast(m,h=13, xreg =xregf_sim)
      s<- sd(na.omit(m$fitted))
      error <- qnorm(0.95)*s/sqrt(13)
      fcast<- c(f$mean[1:6], f$mean[7:13] + error)
      #fcast<- f$mean
    }else if(names(xx)[i]%in%c("SES HIGH")){
      set.seed(66)
      m <- nnetar(myts, xreg= xregg, maxit=150, decay=0.001, repeats = 10)
      f<- forecast(m,h=13, xreg =xregf_sim)
      s<- sd(na.omit(m$fitted))
      error <- qnorm(0.95)*s/sqrt(13)
      fcast<- c(f$mean[1:3], f$mean[4:6] + error, f$mean[7:13] + 2*error)
      #fcast<- f$mean
    }else if(names(xx)[i]%in%c("SES MEDIUM")){
      set.seed(66)
      m <- nnetar(myts, xreg= xregg, maxit=150, decay=0.001, repeats = 10)
      f<- forecast(m,h=13, xreg =xregf_sim)
      s<- sd(na.omit(m$fitted))
      error <- qnorm(0.95)*s/sqrt(13)
      fcast<- c(f$mean[1:6], f$mean[7:13] + error)
      #fcast<- f$mean
    }else{
      set.seed(66)
      m <- nnetar(myts, xreg= xregg, maxit=150, decay=0.001, repeats = 10)
      f<- forecast(m,h=13, xreg =xregf_sim)
      s<- sd(na.omit(m$fitted))
      error <- qnorm(0.95)*s/sqrt(13)
      fcast<- f$mean
    }
    
    # set.seed(66)
    # m <- nnetar(myts, xreg= xregg, maxit=150, decay=0.001, repeats = 10)
    # f<- forecast(m,h=13, xreg =xregf_sim)
    # s<- sd(na.omit(m$fitted))
    # error <- qnorm(0.95)*s/sqrt(13)
    # fcast<- f$mean
    
    res2<- data.frame(cbind(res2, as.data.frame(fcast)))
  }
  
  names(res2) <- c("x",names(forecasted_df)[12:ncol(forecasted_df)])
  
  res2 <- res2[,3:11]
  
  if(s.type%in%c("sector","ses.sector")){
    res2$`IN-HOME FMCG` <- rowSums(res2[,1:5]) 
    out <- select(forecasted_df, 1:3, 13:17)
    out$`IN-HOME FMCG` <- forecasted_df$`IN-HOME FMCG`
    
    out[(n+1):nrow(out),which(names(out)=="BEVERAGES")] <- select(res2, BEVERAGES)
    out[(n+1):nrow(out),which(names(out)=="DAIRY")] <- select(res2, DAIRY)
    out[(n+1):nrow(out),which(names(out)=="PACKAGED GROCERY")] <- select(res2, `PACKAGED GROCERY`)
    out[(n+1):nrow(out),which(names(out)=="PERSONAL CARE")] <- select(res2, `PERSONAL CARE`)
    out[(n+1):nrow(out),which(names(out)=="HOME CARE")] <- select(res2, `HOME CARE`)
    out[(n+1):nrow(out),which(names(out)=="IN-HOME FMCG")] <- select(res2, `IN-HOME FMCG`)
    
  }else if(s.type=="ses"){
    res2$`IN-HOME FMCG` <- rowSums(res2[,6:8])
    out <- select(forecasted_df, 1:3, 18:20)
    out$`IN-HOME FMCG` <- forecasted_df$`TOTAL FMCG_SES`
    
    out[(n+1):nrow(out),which(names(out)=="SES HIGH")] <- select(res2, `SES HIGH`)
    out[(n+1):nrow(out),which(names(out)=="SES MEDIUM")] <- select(res2, `SES MEDIUM`)
    out[(n+1):nrow(out),which(names(out)=="SES LOW")] <- select(res2, `SES LOW`)
    out[(n+1):nrow(out),which(names(out)=="IN-HOME FMCG")] <- select(res2, `IN-HOME FMCG`)
    
  }
  
  return(out)
  
}

############################################################################# DATA ############################################################################

######################################################################## Multivariate Forecast ##################################################################33

# Philippines
PH_forecasted_df <- read_xlsx("./data/PH_Multivariate_Forecast_Data.xlsx",sheet=1)
PH_forecasted_df$`TOTAL FMCG_SES` <- PH_forecasted_df$`SES HIGH` + PH_forecasted_df$`SES MEDIUM` + PH_forecasted_df$`SES LOW`

for(i in 6:ncol(PH_forecasted_df)){
  
  PH_forecasted_df[,i] <- round(PH_forecasted_df[,i],2)
  
}

PH.input.values <- read_xlsx("./data/PH_Multivariate_Forecast_Data.xlsx",sheet=2)

PH_gdp_toDisplay <- read_xlsx("./data/PH_GDP_toDisplay.xlsx", sheet=1)

############################################################################### UI ############################################################################

frow1 <- fluidRow(column(2,
                         pickerInput(inputId = "sector",
                                     label = div(style='color: black; font-size: 1.5rem; font-weight: bold; font-family: Roboto Condensed;', HTML("Select Sectors")), 
                                     choices = names(PH_forecasted_df)[c(12:17)],
                                     options = list(`live-search` = FALSE),
                                     selected=NULL
                         ),
                         radioButtons("switchplot1", div(style='color: black; font-size: 1.5rem; font-weight: bold; font-family: Roboto Condensed;', HTML("Change Plot")),
                                      c("Time Series" = "ts",
                                        #"Compare Quarters" = "q",
                                        "Compare Sectors" = "s")
                         ),
                         radioButtons("tp1", div(style='color: black; font-size: 1.5rem; font-weight: bold; font-family: Roboto Condensed;', HTML("Choose Time Period")),
                                      c("Compare Quarter" = "quarter",
                                        "Compare Half" = "half")
                         )
),
conditionalPanel(condition="input.switchplot1=='ts'",
                 column(8, plotOutput("plot1",width="100%",height="350px")%>% withSpinner()),
                 column(2, htmlOutput("growth.text.1", height="200px"))
),
# conditionalPanel(condition="input.switchplot1=='q'",
#                  column(8, plotOutput("plot1_quarter",width="100%",height="350px")%>% withSpinner()),
#                  column(2, htmlOutput("growth.text.2", height="200px"))
#                  ),
conditionalPanel(condition="input.switchplot1=='s'",
                 column(8, plotOutput("plot1_sector",width="100%",height="350px")%>% withSpinner()),
                 column(2, htmlOutput("sector.contri.text", height="200px"))
)
)

frow2 <- fluidRow(column(12, tags$h6("-", style="color:#D3D3D3; text-align:center; background-color: #D3D3D3; font-weight:400;")),
                  #chooseSliderSkin("Round", "#6495ED"),
                  column(4, sliderTextInput(inputId = "fmcgIR",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Inflation Rate (SQ = ",round(mean(PH_forecasted_df$Inflation[66:78]),1),"%):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgUR",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Cash Remittances (SQ = ",round(sum(PH_forecasted_df$`Cash Remittances`[66:78])/1000000,2)," USD Million):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgGDP",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("GDP Annual Growth Rate (SQ = ",round(mean(PH_gdp_toDisplay$GDP),1),"%):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgFCPI",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Food Inflation (SQ = ",round(mean(PH_forecasted_df$`Food Inflation`[66:78]),1),"%):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgCS",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Consumer Spending (SQ = ",round(mean(PH_forecasted_df$`Consumer spending`[66:78]),0)," PHP Million):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgCCI",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Consumer Confidence Index (SQ = ",round(mean(PH_forecasted_df$CCI[66:78]),1),"):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4,actionBttn("reset","Reset Controllers to Status Quo",style="jelly",size="s"))
)



frow3 <- fluidRow(column(2,
                         pickerInput(inputId = "ses",
                                     label = div(style='color: black; font-size: 1.5rem; font-weight: bold; font-family: Roboto Condensed;', HTML("Select SES")), 
                                     choices = names(PH_forecasted_df)[c(18:20)],
                                     options = list(`live-search` = FALSE),
                                     selected=NULL
                         ),
                         # materialSwitch(inputId = "switchplot", 
                         #                label = div(style='color: black; font-size: 1.5rem; font-weight: bold; font-family: Roboto Condensed;', HTML("Change Plot")), 
                         #                status = "info", 
                         #                width="100%"
                         #                )
                         radioButtons("switchplot2", div(style='color: black; font-size: 1.5rem; font-weight: bold; font-family: Roboto Condensed;', HTML("Change Plot")),
                                      c("Time Series" = "ts",
                                        #"Compare Quarters" = "q",
                                        "Compare SES" = "s")
                         ),
                         radioButtons("tp2", div(style='color: black; font-size: 1.5rem; font-weight: bold; font-family: Roboto Condensed;', HTML("Choose Time Period")),
                                      c("Compare Quarter" = "quarter",
                                        "Compare Half" = "half")
                         )
),
conditionalPanel(condition="input.switchplot2=='ts'",
                 column(8, plotOutput("plot2",width="100%",height="350px")%>% withSpinner()),
                 column(2, htmlOutput("growth.text.ses.1", height="200px"))
),
# conditionalPanel(condition="input.switchplot2=='q'",
#                  column(8, plotOutput("plot2_quarter",width="100%",height="350px")%>% withSpinner()),
#                  column(2, htmlOutput("growth.text.ses.2", height="200px"))
#                  ),
conditionalPanel(condition="input.switchplot2=='s'",
                 column(8, plotOutput("plot2_ses",width="100%",height="350px")%>% withSpinner()),
                 column(2, htmlOutput("ses.contri.text", height="200px"))
)
)

frow4 <- fluidRow(column(12, tags$h6("-", style="color:#D3D3D3; text-align:center; background-color: #D3D3D3; font-weight:400;")),
                  #chooseSliderSkin("Round", "#6495ED"),
                  column(4, sliderTextInput(inputId = "fmcgIR_ses",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Inflation Rate (SQ = ",round(mean(PH_forecasted_df$Inflation[66:78]),1),"%):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgUR_ses",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Cash Remittances (SQ = ",round(sum(PH_forecasted_df$`Cash Remittances`[66:78])/1000000,2)," USD Million):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgGDP_ses",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("GDP Annual Growth Rate (SQ = ",round(mean(PH_gdp_toDisplay$GDP),1),"%):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgFCPI_ses",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Food Inflation (SQ = ",round(mean(PH_forecasted_df$`Food Inflation`[66:78]),1),"%):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgCS_ses",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Consumer Spending (SQ = ",round(mean(PH_forecasted_df$`Consumer spending`[66:78]),0)," PHP Million):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4, sliderTextInput(inputId = "fmcgCCI_ses",
                                            label = div(style='color: black; font-size: 1.8rem; font-family: Roboto Condensed;', HTML(paste0("Consumer Confidence Index (SQ = ",round(mean(PH_forecasted_df$CCI[66:78]),1),"):"))),
                                            choices = c("Reduced","Slightly Reduced","Status Quo","Slightly Increased","Increased"),
                                            selected = "Status Quo",
                                            hide_min_max = T,
                                            width="95%")
                  ),
                  column(4,actionBttn("reset_ses","Reset Controllers to Status Quo",style="jelly",size="s"))
)

frow9 <- fluidRow(column(2,
                         radioButtons("switchview", div(style='color: black; font-size: 1.5rem; font-weight: bold; font-family: Roboto Condensed;', HTML("Select View")),
                                      c("Show Plots" = "p",
                                        "Show Table" = "t")
                         )
),
conditionalPanel(condition="input.switchview=='p'",
                 column(10,plotlyOutput("macro.plot"))
),
conditionalPanel(condition="input.switchview=='t'",
                 column(10,tableOutput("macro.table"))
)
)

ui <- navbarPage(title = div(img(src='WP PRIMARY_GRADIENT WHTE.png', style="margin-top: -57px; padding-right:2px;padding-bottom:0px", height = 135, width = 180)),
  #title = div(img(src='KANTAR_Small_Logo_White_RGB.png', style="margin-top: -2px; padding-right:2px;padding-bottom:15px", height = 40, width = 150)),
                 windowTitle="Market Outlook Simulator",
                 theme=shinytheme("yeti"),
                 tabPanel("Sector Comparison",
                          #HTML('<meta name="viewport" content="width=1024">'),
                          frow1,
                          frow2
                 ),
                 tabPanel("SES Comparison",
                          frow3,
                          frow4
                 ),
                 tabPanel("Macro Factors",
                          frow9)
)

############################################################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    selected.sector <- reactive({
        
        return(input$sector)
        
    })
    
    df.selected <- reactive({
      
      forecasted_df <- PH_forecasted_df
      input.values <- PH.input.values

      out <- fmcg_forecast(forecasted_df, input.values, input, country, sector, 65, "sector")
        
      return(out)
        
    })
    
    tp1.selected <- reactive({
      
      return(input$tp1)
      
    })
    
    output$plot1 <- renderPlot({
        
        sector <- selected.sector()
        df.selected <- df.selected()
        tp1 <- tp1.selected()
        
        df.selected <- select(df.selected, 1:3, which(names(df.selected)==sector))
        
        if(tp1=="quarter"){
          
          df.quarterly <- df.selected%>%
            select(year, qtr, 4) %>%
            group_by(year, qtr) %>%
            summarise_all(sum)
          
          df.plot <- data.frame(Year=c(rep(2022,4),rep(2023,4),rep(2024,4),rep(2025,4),rep(2026,4)),
                                Quarter=rep(c("Q1","Q2","Q3","Q4"),5),
                                #Quarter=c("Q4", rep(c("Q1","Q2","Q3","Q4"),3)),
                                Growth.Rate=c(((as.numeric(unlist(df.quarterly[5,3]))/as.numeric(unlist(df.quarterly[1,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[6,3]))/as.numeric(unlist(df.quarterly[2,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[7,3]))/as.numeric(unlist(df.quarterly[3,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[8,3]))/as.numeric(unlist(df.quarterly[4,3])))-1)*100, #2022
                                              ((as.numeric(unlist(df.quarterly[9,3]))/as.numeric(unlist(df.quarterly[5,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[10,3]))/as.numeric(unlist(df.quarterly[6,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[11,3]))/as.numeric(unlist(df.quarterly[7,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[12,3]))/as.numeric(unlist(df.quarterly[8,3])))-1)*100, #2023
                                              ((as.numeric(unlist(df.quarterly[13,3]))/as.numeric(unlist(df.quarterly[9,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[14,3]))/as.numeric(unlist(df.quarterly[10,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[15,3]))/as.numeric(unlist(df.quarterly[11,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[16,3]))/as.numeric(unlist(df.quarterly[12,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[17,3]))/as.numeric(unlist(df.quarterly[13,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[18,3]))/as.numeric(unlist(df.quarterly[14,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[19,3]))/as.numeric(unlist(df.quarterly[15,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[20,3]))/as.numeric(unlist(df.quarterly[16,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[21,3]))/as.numeric(unlist(df.quarterly[17,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[22,3]))/as.numeric(unlist(df.quarterly[18,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[23,3]))/as.numeric(unlist(df.quarterly[19,3])))-1)*100,
                                              ((as.numeric(unlist(df.quarterly[24,3]))/as.numeric(unlist(df.quarterly[20,3])))-1)*100
                                )
          )
          
          df.plot$Year <- factor(df.plot$Year, levels=c(2022,2023,2024,2025,2026))
          df.plot$Quarter <- factor(df.plot$Quarter, levels=c("Q1","Q2","Q3","Q4"))
          df.plot$col <- factor(c(rep("lightgreen",16),rep("lightblue",4)),
                                levels=c("lightgreen","lightblue")
          )
          
          ggplot(df.plot, aes(x = Quarter, y = Growth.Rate, fill = col)) +
            geom_col() +
            geom_text(aes(label = paste0(round(Growth.Rate,1),"%")), color = "black", size = 5.5, fontface="bold", position = position_stack(vjust = 0.5)) +
            facet_wrap(. ~ Year, scales = "free_x", strip.position = "bottom", ncol=5) +
            labs(x="", y="",title=paste0("Philippines (DUMMY) ",sector), subtitle="YoY Growth Rates") +
            theme_minimal()+
            scale_fill_manual(
              values = c("lightgreen", "lightblue"),
              labels = c("Actuals", "Forecasted")
            ) +
            theme(axis.text.x=element_text(size=12),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold", size=28),
                  plot.subtitle = element_text(hjust=0.5, size=12),
                  legend.title = element_blank(),
                  legend.position = c(0.95, 1.15),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.spacing = unit(0.5, "lines"),
                  strip.text.x = element_text(size = 18, face = "bold"),
                  strip.placement = "outside")
          
        }else if(tp1=="half"){
          
          df.half <- df.selected%>%
            select(year, half, 4) %>%
            group_by(year, half) %>%
            summarise_all(sum)
          
          df.plot <- data.frame(Year=c(rep(2022,2),rep(2023,2),rep(2024,2),rep(2025,2),rep(2026,2)),
                                Half=rep(c("H1","H2"),5),
                                Growth.Rate=c(((as.numeric(unlist(df.half[3,3]))/as.numeric(unlist(df.half[1,3])))-1)*100,
                                              ((as.numeric(unlist(df.half[4,3]))/as.numeric(unlist(df.half[2,3])))-1)*100,
                                              ((as.numeric(unlist(df.half[5,3]))/as.numeric(unlist(df.half[3,3])))-1)*100,
                                              ((as.numeric(unlist(df.half[6,3]))/as.numeric(unlist(df.half[4,3])))-1)*100, #2019
                                              ((as.numeric(unlist(df.half[7,3]))/as.numeric(unlist(df.half[5,3])))-1)*100,
                                              ((as.numeric(unlist(df.half[8,3]))/as.numeric(unlist(df.half[6,3])))-1)*100,
                                              ((as.numeric(unlist(df.half[9,3]))/as.numeric(unlist(df.half[7,3])))-1)*100,
                                              ((as.numeric(unlist(df.half[10,3]))/as.numeric(unlist(df.half[8,3])))-1)*100,
                                              ((as.numeric(unlist(df.half[11,3]))/as.numeric(unlist(df.half[9,3])))-1)*100,
                                              ((as.numeric(unlist(df.half[12,3]))/as.numeric(unlist(df.half[10,3])))-1)*100
                                )
          )
          
          df.plot$Year <- factor(df.plot$Year, levels=c(2022,2023,2024,2025,2026))
          df.plot$Half <- factor(df.plot$Half, levels=c("H1","H2"))
          df.plot$col <- factor(c(rep("lightgreen",8),rep("lightblue",2)),
                                levels=c("lightgreen","lightblue")
          )
          
          ggplot(df.plot, aes(x = Half, y = Growth.Rate, fill = col)) +
            geom_col() +
            geom_text(aes(label = paste0(round(Growth.Rate,1),"%")), color = "black", size = 5.5, fontface="bold", position = position_stack(vjust = 0.5)) +
            facet_wrap(. ~ Year, scales = "free_x", strip.position = "bottom", ncol=5) +
            labs(x="", y="",title=paste0("Philippines (DUMMY) ",sector), subtitle="YoY Growth Rates") +
            theme_minimal()+
            scale_fill_manual(
              values = c("lightgreen", "lightblue"),
              labels = c("Actuals", "Forecasted")
            ) +
            theme(axis.text.x=element_text(size=12),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold", size=28),
                  plot.subtitle = element_text(hjust=0.5, size=12),
                  legend.title = element_blank(),
                  legend.position = c(0.95, 1.15),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.spacing = unit(0.5, "lines"),
                  strip.text.x = element_text(size = 18, face = "bold"),
                  strip.placement = "outside")
          
        }
        
    }) 
    
    # output$plot1_quarter <- renderPlot({
    #   
    #   sector <- selected.sector()
    #   df.selected <- df.selected()
    #   
    #   df.selected <- select(df.selected, 1:3, which(names(df.selected)==sector))
    #   
    #   df.quarterly <- df.selected%>%
    #     select(year, qtr, 4) %>%
    #     group_by(year, qtr) %>%
    #     summarise_all(sum)
    #   
    #   df.plot <- data.frame(Year=c(rep(2019,4),rep(2020,4),rep(2021,4)),
    #                         Quarter=rep(c("Q1","Q2","Q3","Q4"),3),
    #                         Growth.Rate=c(((as.numeric(unlist(df.quarterly[5,3]))/as.numeric(unlist(df.quarterly[1,3])))-1)*100,
    #                                       ((as.numeric(unlist(df.quarterly[6,3]))/as.numeric(unlist(df.quarterly[2,3])))-1)*100,
    #                                       ((as.numeric(unlist(df.quarterly[7,3]))/as.numeric(unlist(df.quarterly[3,3])))-1)*100,
    #                                       ((as.numeric(unlist(df.quarterly[8,3]))/as.numeric(unlist(df.quarterly[4,3])))-1)*100, #2019
    #                                       ((as.numeric(unlist(df.quarterly[9,3]))/as.numeric(unlist(df.quarterly[5,3])))-1)*100,
    #                                       ((as.numeric(unlist(df.quarterly[10,3]))/as.numeric(unlist(df.quarterly[6,3])))-1)*100,
    #                                       ((as.numeric(unlist(df.quarterly[11,3]))/as.numeric(unlist(df.quarterly[7,3])))-1)*100,
    #                                       ((as.numeric(unlist(df.quarterly[12,3]))/as.numeric(unlist(df.quarterly[8,3])))-1)*100, #2020
    #                                       ((as.numeric(unlist(df.quarterly[13,3]))/as.numeric(unlist(df.quarterly[9,3])))-1)*100,
    #                                       ((as.numeric(unlist(df.quarterly[14,3]))/as.numeric(unlist(df.quarterly[10,3])))-1)*100,
    #                                       ((as.numeric(unlist(df.quarterly[15,3]))/as.numeric(unlist(df.quarterly[11,3])))-1)*100,
    #                                       ((as.numeric(unlist(df.quarterly[16,3]))/as.numeric(unlist(df.quarterly[12,3])))-1)*100
    #                         )
    #   )
    #   
    #   df.plot$Year <- factor(df.plot$Year, levels=c(2019,2020,2021))
    #   df.plot$Quarter <- factor(df.plot$Quarter, levels=c("Q1","Q2","Q3","Q4"))
    #   df.plot$col <- factor(c(rep("lightgreen",9),rep("lightblue",3)),
    #                         levels=c("lightgreen","lightblue")
    #   )
    #   
    #   ggplot(df.plot, aes(x = Year, y = Growth.Rate, fill = col)) +
    #     geom_col() +
    #     geom_text(aes(label = paste0(round(Growth.Rate,1),"%")), color = "black", size = 5.5, fontface="bold", position = position_stack(vjust = 0.5)) +
    #     facet_wrap(. ~ Quarter, scales = "free_x", strip.position = "bottom", ncol=4) +
    #     labs(x="", y="",title=paste0("Philippines ",sector), subtitle="YoY Growth Rates") +
    #     theme_minimal()+
    #     scale_fill_manual(
    #       values = c("lightgreen", "lightblue"),
    #       labels = c("Actuals", "Forecasted")
    #     ) +
    #     theme(axis.text.x=element_text(size=12),
    #           axis.text.y=element_blank(),
    #           axis.ticks.y=element_blank(),
    #           plot.title = element_text(hjust=0.5, face="bold", size=28),
    #           plot.subtitle = element_text(hjust=0.5, size=12),
    #           legend.title = element_blank(),
    #           legend.position = c(0.95, 1.15),
    #           panel.grid.major = element_blank(),
    #           panel.grid.minor = element_blank(),
    #           panel.spacing = unit(0.5, "lines"),
    #           strip.text.x = element_text(size = 18, face = "bold"),
    #           strip.placement = "outside")
    #   
    # }) 
    
    output$growth.text.1 <- renderUI({
        
        sector <- selected.sector()
        df.selected <- df.selected()
        
        sector.2022 <- df.selected %>%
          filter(year==2026) %>%
          select(sector) %>%
          summarise_all(sum)
        
        total.fmcg.2022 <- df.selected %>%
          filter(year==2026) %>%
          select(`IN-HOME FMCG`) %>%
          summarise_all(sum)
        
        weight.2022 <- paste0(round((sector.2022/total.fmcg.2022)*100,1),"%")
        
        df.selected <- select(df.selected, 1:3, which(names(df.selected)==sector))
        
        df.year <- df.selected %>%
          select(-c(2,3)) %>%
          group_by(year) %>%
          summarise_all(sum)
        
        growth.rate <- ((as.numeric(unlist(df.year[6,2]))/as.numeric(unlist(df.year[5,2])))-1)*100
        
        if(growth.rate<0){
          growth.rate.text <- paste0(round(growth.rate,1),"%")
        }else{
          growth.rate.text <- paste0("+",round(growth.rate,1),"%")
        }
        
        HTML(paste0(tags$p(style="color: black; font-size: 40px; text-align: center; font-weight: bold;",
                           HTML(paste0("<br/>",growth.rate.text))
        ),
        tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;",
               HTML("_____________")
        ),
        tags$p(style="color: black; font-size: 16px; text-align: center;",
               HTML(paste0("FY 2026 <br/>", sector,"<br/> YoY Growth Rate"))
        ),
        tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;",
               HTML("_____________")
        ),
        tags$p(style="color: black; font-size: 16px; text-align: center;",
               HTML(paste0(weight.2022, " of <br/> IN-HOME FMCG Value"))
        )
        )
        )
        
    })
    
    output$growth.text.2 <- renderUI({
      
      sector <- selected.sector()
      df.selected <- df.selected()
      
      sector.2022 <- df.selected %>%
        filter(year==2026) %>%
        select(sector) %>%
        summarise_all(sum)
      
      total.fmcg.2022 <- df.selected %>%
        filter(year==2026) %>%
        select(`IN-HOME FMCG`) %>%
        summarise_all(sum)
      
      weight.2022 <- paste0(round((sector.2022/total.fmcg.2022)*100,1),"%")
      
      df.selected <- select(df.selected, 1:3, which(names(df.selected)==sector))
      
      df.year <- df.selected %>%
        select(-c(2,3)) %>%
        group_by(year) %>%
        summarise_all(sum)
      
      growth.rate <- ((as.numeric(unlist(df.year[6,2]))/as.numeric(unlist(df.year[5,2])))-1)*100
      
      if(growth.rate<0){
        growth.rate.text <- paste0(round(growth.rate,1),"%")
      }else{
        growth.rate.text <- paste0("+",round(growth.rate,1),"%")
      }
      
      HTML(paste0(tags$p(style="color: black; font-size: 40px; text-align: center; font-weight: bold;",
                         HTML(paste0("<br/>",growth.rate.text))
      ),
      tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;",
             HTML("_____________")
      ),
      tags$p(style="color: black; font-size: 16px; text-align: center;",
             HTML(paste0("FY 2026 <br/>", sector,"<br/> YoY Growth Rate"))
      ),
      tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;",
             HTML("_____________")
      ),
      tags$p(style="color: black; font-size: 16px; text-align: center;",
             HTML(paste0(weight.2022, " of <br/> IN-HOME FMCG Value"))
      )
      )
      )
      
    })
    
    output$plot1_sector <- renderPlot({
      
      df.selected <- df.selected()
      
      df.sector <- df.selected %>%
        select(year, c(4:9)) %>%
        group_by(year) %>%
        summarise_all(sum) %>%
        melt(id.vars="year") 
      
      df.plot <- data.frame(Year=c(rep(22,6),rep(23,6),rep(24,6),rep(25,6),rep(26,6)),
                            Sector=rep(c("BEVERAGES","DAIRY","PACKAGED GROCERY","PERSONAL CARE","HOME CARE","IN-HOME FMCG"),5),
                            Growth.Rate=c(((as.numeric(unlist(df.sector[2,3]))/as.numeric(unlist(df.sector[1,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[8,3]))/as.numeric(unlist(df.sector[7,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[14,3]))/as.numeric(unlist(df.sector[13,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[20,3]))/as.numeric(unlist(df.sector[19,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[26,3]))/as.numeric(unlist(df.sector[25,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[32,3]))/as.numeric(unlist(df.sector[31,3])))-1)*100, #2023 GR
                                          
                                          ((as.numeric(unlist(df.sector[3,3]))/as.numeric(unlist(df.sector[2,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[9,3]))/as.numeric(unlist(df.sector[8,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[15,3]))/as.numeric(unlist(df.sector[14,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[21,3]))/as.numeric(unlist(df.sector[20,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[27,3]))/as.numeric(unlist(df.sector[26,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[33,3]))/as.numeric(unlist(df.sector[32,3])))-1)*100,
                                          
                                          ((as.numeric(unlist(df.sector[4,3]))/as.numeric(unlist(df.sector[3,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[10,3]))/as.numeric(unlist(df.sector[9,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[16,3]))/as.numeric(unlist(df.sector[15,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[22,3]))/as.numeric(unlist(df.sector[21,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[28,3]))/as.numeric(unlist(df.sector[27,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[34,3]))/as.numeric(unlist(df.sector[33,3])))-1)*100,
                                          
                                          ((as.numeric(unlist(df.sector[5,3]))/as.numeric(unlist(df.sector[4,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[11,3]))/as.numeric(unlist(df.sector[10,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[17,3]))/as.numeric(unlist(df.sector[16,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[23,3]))/as.numeric(unlist(df.sector[22,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[29,3]))/as.numeric(unlist(df.sector[28,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[35,3]))/as.numeric(unlist(df.sector[34,3])))-1)*100,
                                          
                                          ((as.numeric(unlist(df.sector[6,3]))/as.numeric(unlist(df.sector[5,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[12,3]))/as.numeric(unlist(df.sector[11,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[18,3]))/as.numeric(unlist(df.sector[17,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[24,3]))/as.numeric(unlist(df.sector[23,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[30,3]))/as.numeric(unlist(df.sector[29,3])))-1)*100,
                                          ((as.numeric(unlist(df.sector[36,3]))/as.numeric(unlist(df.sector[35,3])))-1)*100
                            )
      )
      
      df.plot$Year <- factor(df.plot$Year, levels=c(22,23,24,25,26))
      df.plot$Sector <- factor(df.plot$Sector, levels=c("BEVERAGES","DAIRY","PACKAGED GROCERY","PERSONAL CARE","HOME CARE","IN-HOME FMCG"))
      df.plot$col <- factor(c(rep("grey",24),"#ff4c4c","#ff7f50","lightblue","yellow","#4ca64c","#b266b2"),
                            levels=c("grey","#ff4c4c","#ff7f50","lightblue","yellow","#4ca64c","#b266b2"))
      
      ggplot(df.plot, aes(x = Year, y = Growth.Rate, fill = col)) +
        geom_col() +
        geom_text(aes(label = paste0(round(Growth.Rate,1),"%")), color = "black", size = 5.5, fontface="bold", position = position_stack(vjust = 0.5)) +
        facet_wrap(. ~ Sector, scales = "free_x", strip.position = "top", ncol=6) +
        labs(x="", y="",title="Philippines (DUMMY)", subtitle="FY Growth Rates") +
        theme_minimal()+
        theme(axis.text.x=element_text(size=10),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.title = element_text(hjust=0.5, face="bold", size=28),
              plot.subtitle = element_text(hjust=0.5, size=12),
              legend.title = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(2, "lines"),
              strip.text.x = element_text(size = 12, face = "bold"),
              strip.placement = "outside")+
        scale_fill_identity()
      
    }) 
    
    output$sector.contri.text <- renderUI({
      
      df.selected <- df.selected()
      
      df.sector <- df.selected %>%
        select(year, c(4:9)) %>%
        group_by(year) %>%
        summarise_all(sum) %>%
        melt(id.vars="year") %>%
        filter(year==2026)
      
      HTML(paste0(tags$p(style="color: black; font-size: 30px; text-align: center; font-weight: bold;", 
                         HTML("<br/> Sector <br/> Value <br/> Contribution")
      ),
      tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;", 
             HTML("_____________")
      ),
      tags$p(style="color: black; font-size: 12px; text-align: center;", 
             HTML(paste0("BEVERAGES: ", paste0(round(df.sector$value[1]/df.sector$value[6]*100,1),"%")))
      ),
      tags$p(style="color: black; font-size: 12px; text-align: center;", 
             HTML(paste0("DAIRY: ", paste0(round(df.sector$value[2]/df.sector$value[6]*100,1),"%")))
      ),
      tags$p(style="color: black; font-size: 12px; text-align: center;", 
             HTML(paste0("PACKAGED GROCERY: ", paste0(round(df.sector$value[3]/df.sector$value[6]*100,1),"%")))
      ),
      tags$p(style="color: black; font-size: 12px; text-align: center;", 
             HTML(paste0("PERSONAL CARE: ", paste0(round(df.sector$value[4]/df.sector$value[6]*100,1),"%")))
      ),
      tags$p(style="color: black; font-size: 12px; text-align: center;", 
             HTML(paste0("HOME CARE: ", paste0(round(df.sector$value[5]/df.sector$value[6]*100,1),"%")))
      )
      )
      )
      
    })
    
    
    observeEvent(input$reset, updateSliderTextInput(session, "fmcgCCI", selected="Status Quo"))
    observeEvent(input$reset, updateSliderTextInput(session, "fmcgUR", selected="Status Quo"))
    observeEvent(input$reset, updateSliderTextInput(session, "fmcgCS", selected="Status Quo"))
    observeEvent(input$reset, updateSliderTextInput(session, "fmcgGDP", selected="Status Quo"))
    observeEvent(input$reset, updateSliderTextInput(session, "fmcgIR", selected="Status Quo"))
    observeEvent(input$reset, updateSliderTextInput(session, "fmcgFCPI", selected="Status Quo"))
    
    observeEvent(input$reset_ses, updateSliderTextInput(session, "fmcgCCI_ses", selected="Status Quo"))
    observeEvent(input$reset_ses, updateSliderTextInput(session, "fmcgUR_ses", selected="Status Quo"))
    observeEvent(input$reset_ses, updateSliderTextInput(session, "fmcgCS_ses", selected="Status Quo"))
    observeEvent(input$reset_ses, updateSliderTextInput(session, "fmcgGDP_ses", selected="Status Quo"))
    observeEvent(input$reset_ses, updateSliderTextInput(session, "fmcgIR_ses", selected="Status Quo"))
    observeEvent(input$reset_ses, updateSliderTextInput(session, "fmcgFCPI_ses", selected="Status Quo"))
    
######################################################################### SES ###############################################################################
    
    selected.ses <- reactive({
      
      return(input$ses)
      
    })
    
    tp2.selected <- reactive({
      
      return(input$tp2)
      
    })
    
  df.selected.ses <- reactive({
    
      sector <- selected.ses()

      forecasted_df <- PH_forecasted_df
      input.values <- PH.input.values
      
      out.sector <- fmcg_forecast(PH_forecasted_df, PH.input.values, input, "Philippines", "IN-HOME FMCG", 65, "ses.sector")
      sector.inhome.fmcg <- select(out.sector, `IN-HOME FMCG`)
      
      out.ses <- fmcg_forecast(forecasted_df, input.values, input, "Philippines", sector, 65, "ses")
      
      out <- out.ses[,1:3]
      out$`SES HIGH` <- (out.ses$`SES HIGH`/out.ses$`IN-HOME FMCG`)*as.numeric(unlist(sector.inhome.fmcg))
      out$`SES MEDIUM` <- (out.ses$`SES MEDIUM`/out.ses$`IN-HOME FMCG`)*as.numeric(unlist(sector.inhome.fmcg))
      out$`SES LOW` <- (out.ses$`SES LOW`/out.ses$`IN-HOME FMCG`)*as.numeric(unlist(sector.inhome.fmcg))
      out$`IN-HOME FMCG` <- as.numeric(unlist(sector.inhome.fmcg))
      out[1:65,4:6] <- out.ses[1:65,4:6]
      
      return(out)
      
    })  
  
  output$plot2 <- renderPlot({
    
    sector <- selected.ses()
    df.selected <- df.selected.ses()
    tp2 <- tp2.selected()
    
    df.selected <- select(df.selected, 1:3, which(names(df.selected)==sector))
    
    if(tp2=="quarter"){
      
      df.quarterly <- df.selected%>%
        select(year, qtr, 4) %>%
        group_by(year, qtr) %>%
        summarise_all(sum)
      
      df.plot <- data.frame(Year=c(rep(2022,4),rep(2023,4),rep(2024,4),rep(2025,4),rep(2026,4)),
                            Quarter=rep(c("Q1","Q2","Q3","Q4"),5),
                            #Quarter=c("Q4", rep(c("Q1","Q2","Q3","Q4"),3)),
                            Growth.Rate=c(((as.numeric(unlist(df.quarterly[5,3]))/as.numeric(unlist(df.quarterly[1,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[6,3]))/as.numeric(unlist(df.quarterly[2,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[7,3]))/as.numeric(unlist(df.quarterly[3,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[8,3]))/as.numeric(unlist(df.quarterly[4,3])))-1)*100, #2022
                                          ((as.numeric(unlist(df.quarterly[9,3]))/as.numeric(unlist(df.quarterly[5,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[10,3]))/as.numeric(unlist(df.quarterly[6,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[11,3]))/as.numeric(unlist(df.quarterly[7,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[12,3]))/as.numeric(unlist(df.quarterly[8,3])))-1)*100, #2023
                                          ((as.numeric(unlist(df.quarterly[13,3]))/as.numeric(unlist(df.quarterly[9,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[14,3]))/as.numeric(unlist(df.quarterly[10,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[15,3]))/as.numeric(unlist(df.quarterly[11,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[16,3]))/as.numeric(unlist(df.quarterly[12,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[17,3]))/as.numeric(unlist(df.quarterly[13,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[18,3]))/as.numeric(unlist(df.quarterly[14,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[19,3]))/as.numeric(unlist(df.quarterly[15,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[20,3]))/as.numeric(unlist(df.quarterly[16,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[21,3]))/as.numeric(unlist(df.quarterly[17,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[22,3]))/as.numeric(unlist(df.quarterly[18,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[23,3]))/as.numeric(unlist(df.quarterly[19,3])))-1)*100,
                                          ((as.numeric(unlist(df.quarterly[24,3]))/as.numeric(unlist(df.quarterly[20,3])))-1)*100
                            )
      )
      
      df.plot$Year <- factor(df.plot$Year, levels=c(2022,2023,2024,2025,2026))
      df.plot$Quarter <- factor(df.plot$Quarter, levels=c("Q1","Q2","Q3","Q4"))
      df.plot$col <- factor(c(rep("lightgreen",16),rep("lightblue",4)),
                            levels=c("lightgreen","lightblue")
      )
      
      ggplot(df.plot, aes(x = Quarter, y = Growth.Rate, fill = col)) +
        geom_col() +
        geom_text(aes(label = paste0(round(Growth.Rate,1),"%")), color = "black", size = 5.5, fontface="bold", position = position_stack(vjust = 0.5)) +
        facet_wrap(. ~ Year, scales = "free_x", strip.position = "bottom", ncol=5) +
        labs(x="", y="",title=paste0("Philippines (DUMMY) ",sector), subtitle="YoY Growth Rates") +
        theme_minimal()+
        scale_fill_manual(
          values = c("lightgreen", "lightblue"),
          labels = c("Actuals", "Forecasted")
        ) +
        theme(axis.text.x=element_text(size=12),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.title = element_text(hjust=0.5, face="bold", size=28),
              plot.subtitle = element_text(hjust=0.5, size=12),
              legend.title = element_blank(),
              legend.position = c(0.95, 1.15),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(0.5, "lines"),
              strip.text.x = element_text(size = 18, face = "bold"),
              strip.placement = "outside")
      
    }else if(tp2=="half"){
      
      df.half <- df.selected%>%
        select(year, half, 4) %>%
        group_by(year, half) %>%
        summarise_all(sum)
      
      df.plot <- data.frame(Year=c(rep(2022,2),rep(2023,2),rep(2024,2),rep(2025,2),rep(2026,2)),
                            Half=rep(c("H1","H2"),5),
                            Growth.Rate=c(((as.numeric(unlist(df.half[3,3]))/as.numeric(unlist(df.half[1,3])))-1)*100,
                                          ((as.numeric(unlist(df.half[4,3]))/as.numeric(unlist(df.half[2,3])))-1)*100,
                                          ((as.numeric(unlist(df.half[5,3]))/as.numeric(unlist(df.half[3,3])))-1)*100,
                                          ((as.numeric(unlist(df.half[6,3]))/as.numeric(unlist(df.half[4,3])))-1)*100, #2019
                                          ((as.numeric(unlist(df.half[7,3]))/as.numeric(unlist(df.half[5,3])))-1)*100,
                                          ((as.numeric(unlist(df.half[8,3]))/as.numeric(unlist(df.half[6,3])))-1)*100,
                                          ((as.numeric(unlist(df.half[9,3]))/as.numeric(unlist(df.half[7,3])))-1)*100,
                                          ((as.numeric(unlist(df.half[10,3]))/as.numeric(unlist(df.half[8,3])))-1)*100,
                                          ((as.numeric(unlist(df.half[11,3]))/as.numeric(unlist(df.half[9,3])))-1)*100,
                                          ((as.numeric(unlist(df.half[12,3]))/as.numeric(unlist(df.half[10,3])))-1)*100
                            )
      )
      
      df.plot$Year <- factor(df.plot$Year, levels=c(2022,2023,2024,2025,2026))
      df.plot$Half <- factor(df.plot$Half, levels=c("H1","H2"))
      df.plot$col <- factor(c(rep("lightgreen",8),rep("lightblue",2)),
                            levels=c("lightgreen","lightblue")
      )
      
      ggplot(df.plot, aes(x = Half, y = Growth.Rate, fill = col)) +
        geom_col() +
        geom_text(aes(label = paste0(round(Growth.Rate,1),"%")), color = "black", size = 5.5, fontface="bold", position = position_stack(vjust = 0.5)) +
        facet_wrap(. ~ Year, scales = "free_x", strip.position = "bottom", ncol=5) +
        labs(x="", y="",title=paste0("Philippines (DUMMY) ",sector), subtitle="YoY Growth Rates") +
        theme_minimal()+
        scale_fill_manual(
          values = c("lightgreen", "lightblue"),
          labels = c("Actuals", "Forecasted")
        ) +
        theme(axis.text.x=element_text(size=12),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              plot.title = element_text(hjust=0.5, face="bold", size=28),
              plot.subtitle = element_text(hjust=0.5, size=12),
              legend.title = element_blank(),
              legend.position = c(0.95, 1.15),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(0.5, "lines"),
              strip.text.x = element_text(size = 18, face = "bold"),
              strip.placement = "outside")
      
    }
    
  }) 
  
  # output$plot2_quarter <- renderPlot({
  #   
  #   sector <- selected.ses()
  #   df.selected <- df.selected.ses()
  #   
  #   df.selected <- select(df.selected, 1:3, which(names(df.selected)==sector))
  #   
  #   df.quarterly <- df.selected%>%
  #     select(year, qtr, 4) %>%
  #     group_by(year, qtr) %>%
  #     summarise_all(sum)
  #   
  #   df.plot <- data.frame(Year=c(rep(2019,4),rep(2020,4),rep(2021,4)),
  #                         Quarter=rep(c("Q1","Q2","Q3","Q4"),3),
  #                         Growth.Rate=c(((as.numeric(unlist(df.quarterly[5,3]))/as.numeric(unlist(df.quarterly[1,3])))-1)*100,
  #                                       ((as.numeric(unlist(df.quarterly[6,3]))/as.numeric(unlist(df.quarterly[2,3])))-1)*100,
  #                                       ((as.numeric(unlist(df.quarterly[7,3]))/as.numeric(unlist(df.quarterly[3,3])))-1)*100,
  #                                       ((as.numeric(unlist(df.quarterly[8,3]))/as.numeric(unlist(df.quarterly[4,3])))-1)*100, #2019
  #                                       ((as.numeric(unlist(df.quarterly[9,3]))/as.numeric(unlist(df.quarterly[5,3])))-1)*100,
  #                                       ((as.numeric(unlist(df.quarterly[10,3]))/as.numeric(unlist(df.quarterly[6,3])))-1)*100,
  #                                       ((as.numeric(unlist(df.quarterly[11,3]))/as.numeric(unlist(df.quarterly[7,3])))-1)*100,
  #                                       ((as.numeric(unlist(df.quarterly[12,3]))/as.numeric(unlist(df.quarterly[8,3])))-1)*100, #2020
  #                                       ((as.numeric(unlist(df.quarterly[13,3]))/as.numeric(unlist(df.quarterly[9,3])))-1)*100,
  #                                       ((as.numeric(unlist(df.quarterly[14,3]))/as.numeric(unlist(df.quarterly[10,3])))-1)*100,
  #                                       ((as.numeric(unlist(df.quarterly[15,3]))/as.numeric(unlist(df.quarterly[11,3])))-1)*100,
  #                                       ((as.numeric(unlist(df.quarterly[16,3]))/as.numeric(unlist(df.quarterly[12,3])))-1)*100
  #                         )
  #   )
  #   
  #   df.plot$Year <- factor(df.plot$Year, levels=c(2019,2020,2021))
  #   df.plot$Quarter <- factor(df.plot$Quarter, levels=c("Q1","Q2","Q3","Q4"))
  #   df.plot$col <- factor(c(rep("lightgreen",9),rep("lightblue",3)),
  #                         levels=c("lightgreen","lightblue")
  #   )
  #   
  #   ggplot(df.plot, aes(x = Year, y = Growth.Rate, fill = col)) +
  #     geom_col() +
  #     geom_text(aes(label = paste0(round(Growth.Rate,1),"%")), color = "black", size = 5.5, fontface="bold", position = position_stack(vjust = 0.5)) +
  #     facet_wrap(. ~ Quarter, scales = "free_x", strip.position = "bottom", ncol=4) +
  #     labs(x="", y="",title=paste0("Philippines ",sector), subtitle="YoY Growth Rates") +
  #     theme_minimal()+
  #     scale_fill_manual(
  #       values = c("lightgreen", "lightblue"),
  #       labels = c("Actuals", "Forecasted")
  #     ) +
  #     theme(axis.text.x=element_text(size=12),
  #           axis.text.y=element_blank(),
  #           axis.ticks.y=element_blank(),
  #           plot.title = element_text(hjust=0.5, face="bold", size=28),
  #           plot.subtitle = element_text(hjust=0.5, size=12),
  #           legend.title = element_blank(),
  #           legend.position = c(0.95, 1.15),
  #           panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.spacing = unit(0.5, "lines"),
  #           strip.text.x = element_text(size = 18, face = "bold"),
  #           strip.placement = "outside")
  #   
  # }) 
  
  output$growth.text.ses.1 <- renderUI({
    
    sector <- selected.ses()
    df.selected <- df.selected.ses()
    
    sector.2022 <- df.selected %>%
      filter(year==2026) %>%
      select(sector) %>%
      summarise_all(sum)
    
    total.fmcg.2022 <- df.selected %>%
      filter(year==2026) %>%
      select(`IN-HOME FMCG`) %>%
      summarise_all(sum)
    
    weight.2022 <- paste0(round((sector.2022/total.fmcg.2022)*100,1),"%")
    
    df.selected <- select(df.selected, 1:3, which(names(df.selected)==sector))
    
    df.year <- df.selected %>%
      select(-c(2,3)) %>%
      group_by(year) %>%
      summarise_all(sum)
    
    growth.rate <- ((as.numeric(unlist(df.year[6,2]))/as.numeric(unlist(df.year[5,2])))-1)*100
    
    if(growth.rate<0){
      growth.rate.text <- paste0(round(growth.rate,1),"%")
    }else{
      growth.rate.text <- paste0("+",round(growth.rate,1),"%")
    }
    
    HTML(paste0(tags$p(style="color: black; font-size: 40px; text-align: center; font-weight: bold;", 
                       HTML(paste0("<br/>",growth.rate.text))
    ),
    tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;", 
           HTML("_____________")
    ),
    tags$p(style="color: black; font-size: 16px; text-align: center;", 
           HTML(paste0("FY 2026 <br/>", sector,"<br/> YoY Growth Rate"))
    ),
    tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;", 
           HTML("_____________")
    ),
    tags$p(style="color: black; font-size: 16px; text-align: center;", 
           HTML(paste0(weight.2022, " of <br/> IN-HOME FMCG Value"))
    )
    )
    )
    
  })
  
  output$growth.text.ses.2 <- renderUI({

    sector <- selected.ses()
    df.selected <- df.selected.ses()
    
    sector.2022 <- df.selected %>%
      filter(year==2026) %>%
      select(sector) %>%
      summarise_all(sum)
    
    total.fmcg.2022 <- df.selected %>%
      filter(year==2026) %>%
      select(`IN-HOME FMCG`) %>%
      summarise_all(sum)
    
    weight.2022 <- paste0(round((sector.2022/total.fmcg.2022)*100,1),"%")
    
    df.selected <- select(df.selected, 1:3, which(names(df.selected)==sector))
    
    df.year <- df.selected %>%
      select(-c(2,3)) %>%
      group_by(year) %>%
      summarise_all(sum)
    
    growth.rate <- ((as.numeric(unlist(df.year[6,2]))/as.numeric(unlist(df.year[5,2])))-1)*100
    
    if(growth.rate<0){
      growth.rate.text <- paste0(round(growth.rate,1),"%")
    }else{
      growth.rate.text <- paste0("+",round(growth.rate,1),"%")
    }
    
    HTML(paste0(tags$p(style="color: black; font-size: 40px; text-align: center; font-weight: bold;", 
                       HTML(paste0("<br/>",growth.rate.text))
    ),
    tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;", 
           HTML("_____________")
    ),
    tags$p(style="color: black; font-size: 16px; text-align: center;", 
           HTML(paste0("FY 2026 <br/>", sector,"<br/> YoY Growth Rate"))
    ),
    tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;", 
           HTML("_____________")
    ),
    tags$p(style="color: black; font-size: 16px; text-align: center;", 
           HTML(paste0(weight.2022, " of <br/> IN-HOME FMCG Value"))
    )
    )
    )
    
  })
  
  output$plot2_ses <- renderPlot({
    
    df.selected <- df.selected.ses()
    
    df.ses <- df.selected %>%
      select(year, c(4:7)) %>%
      group_by(year) %>%
      summarise_all(sum) %>%
      melt(id.vars="year") 
    
    df.plot <- data.frame(Year=c(rep(22,3),rep(23,3),rep(24,3),rep(25,3),rep(26,3)),
                          SES=rep(c("SES HIGH","SES MEDIUM","SES LOW"),5),
                          Growth.Rate=c(((as.numeric(unlist(df.ses[2,3]))/as.numeric(unlist(df.ses[1,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[8,3]))/as.numeric(unlist(df.ses[7,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[14,3]))/as.numeric(unlist(df.ses[13,3])))-1)*100,
                                        
                                        ((as.numeric(unlist(df.ses[3,3]))/as.numeric(unlist(df.ses[2,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[9,3]))/as.numeric(unlist(df.ses[8,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[15,3]))/as.numeric(unlist(df.ses[14,3])))-1)*100,
                                        
                                        ((as.numeric(unlist(df.ses[4,3]))/as.numeric(unlist(df.ses[3,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[10,3]))/as.numeric(unlist(df.ses[9,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[16,3]))/as.numeric(unlist(df.ses[15,3])))-1)*100,
                                        
                                        ((as.numeric(unlist(df.ses[5,3]))/as.numeric(unlist(df.ses[4,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[11,3]))/as.numeric(unlist(df.ses[10,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[17,3]))/as.numeric(unlist(df.ses[16,3])))-1)*100,
                                        
                                        ((as.numeric(unlist(df.ses[6,3]))/as.numeric(unlist(df.ses[5,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[12,3]))/as.numeric(unlist(df.ses[11,3])))-1)*100,
                                        ((as.numeric(unlist(df.ses[18,3]))/as.numeric(unlist(df.ses[17,3])))-1)*100
                          )
    )
    
    df.plot$Year <- factor(df.plot$Year, levels=c(22,23,24,25,26))
    df.plot$SES <- factor(df.plot$SES, levels=c("SES HIGH","SES MEDIUM","SES LOW"))
    df.plot$col <- factor(c(rep("grey",12),"#4ca64c","lightblue","yellow"), levels=c("grey","#4ca64c","lightblue","yellow"))
    
    ggplot(df.plot, aes(x = Year, y = Growth.Rate, fill = col)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Growth.Rate,1),"%")), color = "black", size = 5.5, fontface="bold", position = position_stack(vjust = 0.5)) +
      facet_wrap(. ~ SES, scales = "free_x", strip.position = "top", ncol=3) +
      labs(x="", y="",title="Philippines (DUMMY)", subtitle="FY Growth Rates \n") +
      theme_minimal()+
      theme(axis.text.x=element_text(size=10),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.title = element_text(hjust=0.5, face="bold", size=32),
            plot.subtitle = element_text(hjust=0.5, size=16),
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(2, "lines"),
            strip.text.x = element_text(size = 16, face = "bold"),
            strip.placement = "outside")+
      scale_fill_identity()
    
  })
    
  output$ses.contri.text <- renderUI({
    
    df.selected <- df.selected.ses()
    
    df.ses <- df.selected %>%
      select(year, c(4:6)) %>%
      group_by(year) %>%
      summarise_all(sum) %>%
      melt(id.vars="year") %>%
      filter(year==2026)
    
    HTML(paste0(tags$p(style="color: black; font-size: 30px; text-align: center; font-weight: bold;", 
                       HTML("<br/> SES Value <br/> Contribution")
                       ),
                tags$p(style="color: black; font-size: 20px; text-align: center; font-weight: bold;", 
                       HTML("_____________")
                       ),
                tags$p(style="color: black; font-size: 16px; text-align: center;", 
                       HTML(paste0("SES HIGH: ", paste0(round(df.ses$value[1]/sum(df.ses$value)*100,1),"%")))
                       ),
                tags$p(style="color: black; font-size: 16px; text-align: center;", 
                       HTML(paste0("SES MEDIUM: ", paste0(round(df.ses$value[2]/sum(df.ses$value)*100,1),"%")))
                       ),
                tags$p(style="color: black; font-size: 16px; text-align: center;", 
                       HTML(paste0("SES LOW: ", paste0(round(df.ses$value[3]/sum(df.ses$value)*100,1),"%")))
                       )
                )
    )
    
  })
    
#################################################################### Macro Tables ########################################################################

  
  output$macro.table <- renderTable({
    
      df <- select(PH_forecasted_df, 1:11)
      df <- df[66:78,c(1,5,6:11)]
      df$`GDP growth rate` <- PH_gdp_toDisplay$GDP
      df$Country <- "Philippines"
      df <- df[,-c(1,2)]
      names(df)[4] <- "GDP Annual growth rate"
      df <- melt(df, id.vars="Country")
      df$Period <- rep(c("2026_01","2026_02","2026_03","2026_04","2026_05","2026_06","2026_07","2026_08","2026_09","2026_10","2026_11","2026_12","2026_13"),6)
      df <- df[,c(4,2,3)]
      names(df) <- c("Period","Macroeconomics Factor","Value")
      df$Sources <- c(rep("Bangko Sentral ng Pilipinas",13),rep("Bangko Sentral ng Pilipinas",13),rep("National Statistics Office of Philippines",13),rep("National Statistics Office of Philippines",13),rep("Philippine Statistics Authority",13),rep("Philippine Statistics Authority",13))
    
    df
    
    
  })
  
  output$macro.plot <- renderPlotly({
    
    df <- PH_forecasted_df
    
    df.plot <- select(df, 1:11)
    df.plot<- df.plot[66:78,c(1,5,6:11)]
    df.plot$`GDP growth rate` <- PH_gdp_toDisplay$GDP
    df.plot$Period <- c("26_P01","26_P02","26_P03","26_P04","26_P05","26_P06","26_P07","26_P08","26_P09","26_P10","26_P11","26_P12","26_P13")
    df.plot<- df.plot[,-c(1,2)]
    names(df.plot)[4] <- "GDP Annual growth rate"
    df.plot <- melt(df.plot, id.vars="Period")
    df.plot$Period <- factor(df.plot$Period, levels=c("26_P01","26_P02","26_P03","26_P04","26_P05","26_P06","26_P07","26_P08","26_P09","26_P10","26_P11","26_P12","26_P13"))
    
    ggplotly(
      p = ggplot(data=df.plot, aes(x=Period,y=value, group=1)) + 
             geom_line() +
             facet_wrap(~variable, ncol=2, scales = "free")+
             ggtitle("Philippines Macroeconomics Factors") +
             xlab("")+
             ylab("")+
             theme_minimal() +
             theme(plot.title = element_text(hjust = 0.5, size = 24),
                   axis.text.x = element_text(size=6)),
      height = 550
    )
    
    
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
