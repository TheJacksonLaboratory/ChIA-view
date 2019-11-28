## app.R ##
if (!require("shiny")) install.packages("shiny")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("dplyr")) install.packages("dplyr")
if (!require("pheatmap")) install.packages("pheatmap")
if (!require("bedr")) install.packages("bedr")
if (!require("ggforce")) install.packages("ggforce")
#if (!require("rstudioapi")) install.packages("rstudioapi")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("cowplot")) install.packages("cowplot")
if (!require("grid")) install.packages("grid")
if (!require("sitools")) install.packages("sitools")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyr")) install.packages("tidyr")
# Define server logic required to draw a histogram
if (!require("colourpicker")) install.packages("colourpicker")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("leaflet")) install.packages("leaflet")
if (!require("rgdal")) install.packages("rgdal")
if (!require("sp")) install.packages("sp")
if (!require("xtable")) install.packages("xtable")
if (!require("plotly")) install.packages("plotly")
if (!require("data.table")) install.packages("data.table")
if (!require("gggenes")) install.packages("gggenes")
#if (!require("")) install.packages("")
#if (!require("")) install.packages("")
options(shiny.maxRequestSize = 300*1024^2)


FLIST=list.dirs(dirname(rstudioapi::getActiveDocumentContext()$path),full.names = FALSE)

#FLIST<-FLIST0[!(row.names(FLIST0) %in% c("figures","gene_anno")),]
ui <- dashboardPage(skin = "yellow",
                    #ERROR: 'arg' should be one of “blue”, “black”, “purple”, “green”, “red”, “yellow”
  dashboardHeader(title = "ChIA-view v1.0"),

  dashboardSidebar(

    sidebarMenu(
      conditionalPanel(condition="input.conditionedPanels==1",
                       
                       fluidRow( title="REFR",  column(12, submitButton(text="Update!",icon("refresh"),width='100%'))),
                        menuItem(HTML('<FONT color="#ff9933"><h3>Data Upload</h3></FONT>'), 
                             
                           fluidRow( title="MAIN",
                                # column(12, submitButton(text="Update!",icon("refresh"),width='100%')),
                                
                              #  column(12, textInput("FIN", label = HTML('<FONT color="yellow"><em><h4>input file</em></h4></FONT>'),value ="Demo_chiadrop")),
                                column(12, selectInput("FIN", label = HTML('<FONT color="yellow"><em><h4>input file</em></h4></FONT>'), 
                                                   #    choices = list("Demo_chiadrop", "Demo_p2chiadrop", "GAM001", "SPRITE" ), 
                                                   choices = FLIST,
                                                       selected = "F.Demo_CDP")),
                                 column(12, selectInput("FGENE", label = ("select genome"), 
                                                       choices = list("dm3", "dm6", "hg19", "hg38", "mm9" ,"mm10" ), 
                                                       selected = "dm3")),
        
                                 column(12, HTML('<FONT color="yellow"><em><h4>fragment number/GEM</em></h4></FONT>'), align="left",offset=1),
                                 column(6,numericInput("FN_BE",label = "≥",  value = 3)),
                                 column(6,numericInput("FN_SE",label = "<",  value = 1000)),
                                column(12, HTML('<FONT color="yellow"><em><h4>Promoter number/GEM</em></h4></FONT>'), align="left",offset=1),
                                column(6,numericInput("PNL",label = "≥",  value = 1)),
                                column(6,numericInput("PNR",label = "<",  value = 1000)),
                                 #    column(6,numericInput("FN_BE", label = ("minFN"), value = 5)),
                                # column(6,numericInput("FN_SE", label = ("maxFN"), value = 999)),
                                 column(12,textInput("REGION_INPUT", label =  HTML('<FONT color="yellow"><em><h4>Region</em></h4></FONT>'),
                                                     value = "chrX:2612016-3228318")),
                                # column(12, sliderInput("slider2", label =  HTML('<FONT color="yellow"><em></em><h4>Region Adjust %</em></h4></FONT>'), 
                                #                        min = -100, max = 200, value = c(0,100))),
                                column(6, sliderInput("slider2L", label =  "left %", 
                                                       min = -100, max = 49, value = 0)),
                                column(6, sliderInput("slider2R", label = "right %", 
                                                       min = -49, max = 100, value = 0)),
                                
                                column(12, HTML('<FONT color="yellow"><em><h4>plot size</em></h4></FONT>'), align="left",offset=1),
                                column(6,numericInput("WIDTH", label ="width", value = 1000,step=50,width = '100%')),
                                column(6,numericInput("HEIGHT", label = "height",value = 600,step=50,width = '100%')),

                                column(12, numericInput("RESOLUTION", label = ("picture_resolution"), value = 600,step=100))
                                
                       )#fluidRow( title="linear",
                        ),# menuItem("main",
                       menuItem(HTML('<FONT color="#ff9933"><h3>Track Selection</h3></FONT>'), tabName = "dashboard", 
                                
                                fluidRow( title="track",
                                          # column(12, submitButton(text="Update!",icon("refresh"),width='100%')),
                                          column(12,prettyCheckbox("GENE_VIEW", label = "gene track", value = TRUE)),
                                          column(12,prettyRadioButtons("GENE_STYLE", label=HTML('<FONT color="yellow"><em><h4>gene track style</em></h4></FONT>'),
                                                                       inline = TRUE, 
                                                                       choices = list("compressed","extracted"),
                                                                       selected="compressed")),
                                          
                                          column(12,prettyCheckbox("PE_VIEW", label = "Promoter track", value = FALSE)),
                                        #  column(12,prettyCheckbox("P_SELECT", label = "Filterd by Promoter", value = FALSE),shape = "curve"),
                                          column(12,prettyRadioButtons("P_SELECT", label=HTML('<FONT color="yellow"><em><h4>GEMs with Promoter?</em></h4></FONT>'),
                                                                       inline = TRUE, 
                                                                       choices = list("YES","NO"),
                                                                       selected="NO")),
                                          column(12,prettyCheckbox("COLOR_VIEW", label = "fragment track colorful", value = TRUE)),
                                          
                                          column(12,prettyCheckbox("LIB_VIEW", label = "fragment track lib", value = FALSE)),
                                          column(12,prettyCheckbox("CLUSTER_VIEW", label = "cluster view", value = TRUE)),
                                          column(12,prettyCheckbox("LINEAR_INFO", label = "data information", value = TRUE))
                                )#fluidRow
                       ),#menuItem

                       menuItem(HTML('<FONT color="#ff9933"><h3>Fragment View</h3></FONT>'), tabName = "dashboard", 
                                fluidRow( title="Linear view",
                             #  column(12, submitButton(text="Update!",icon("refresh"),width='100%')),
                                column(12, HTML('<FONT color="yellow"><em><h4>fragment view color</em></h4></FONT>'),  align="left",offset=1),
                                column(6,colourpicker::colourInput("CCOLOR4", "Fragment", "darkred")),
                                column(6,colourpicker::colourInput("CCOLOR6", "Promoter", "red")),
                                column(6,colourpicker::colourInput("CCOLOR3", "Line", "gray")),
                                column(6,colourpicker::colourInput("CCOLOR5", "Background", "white")),
                      
                                
                                column(12, HTML('<FONT color="yellow"><em><h4>fragment view elements</em></h4></FONT>'), align="left",offset=1),
                                column(6,numericInput("FSIZE", label = ("Frag size"), value = 0.5,step=0.1) ),
                                column(6,numericInput("LSIZE", label = ("line size"), value = 0.05,step=0.01)),
                           #  column(12,numericInput("LIB_SIZE", label = ("legend text size"), value = 2,step=0.5)),
                               # column(12,prettyCheckbox("P_SELECT", label = "Promoter only", value = TRUE)),
                                column(12,radioButtons("FRAGTYPE", label="Fragment shape",inline = TRUE,
                                                       choices = list("exact"="butt","round","square"),selected="round"))

                              )#fluidRow( title="linear",
                       ),# menuItem("Linear view",
            
                       menuItem(HTML('<FONT color="#ff9933"><h3>Cluster View</h3></FONT>'), tabName = "dashboard", 
                               fluidRow( title="Cluster view",
                                                #  column(12, submitButton(text="Update!",icon("refresh"),width='100%')),
                               column(12, HTML('<FONT color="yellow"><em><h4>Cluster view elements</em></h4></FONT>'), align="left",offset=1),
                               column(6,colourpicker::colourInput("CCOLOR2", "Cluster", "darkred")),
                               column(6, colourpicker::colourInput("CCOLOR1", "Background", "#EFFCED")),   
                               column(12,  numericInput("BN", label = ("bin#"), value = 100,step=10)),
                               column(12,  numericInput("CUTROW", label = ("row_cluster#"), value = 1,step=1)),
                               
                               column(12, selectInput("HCM", label = ("hclustering method"), 
                                             choices = list("ward.D", "ward.D2", "single", "complete", "average" ,
                                                            "mcquitty" , "median" ,"centroid" ), 
                                             selected = "ward.D2")),
                                column(12, selectInput("DIST", label = ("distance function"), 
                                             choices = list("euclidean", "maximum",
                                                            "manhattan", "canberra", "binary" , "minkowski"), 
                                             selected = "euclidean"))
                                 
                       )#fluidRow
                       )#menuItem

          
          
      ),#con1
      conditionalPanel(condition="input.conditionedPanels==2",
                       fluidRow( title="REFR",  column(12, submitButton(text="Update!",icon("refresh"),width='100%'))),
        menuItem(HTML('<FONT color="#00ffff"><h3>Extrusion Data</h3></FONT>'), tabName = "dashboard", 
             fluidRow( title="EV_MAIN",
                     #  column(12, textInput("EV_FIN", label = HTML('<FONT color="yellow"><em><h4>input file</em></h4></FONT>'),value ="Demo_p2chiadrop")),
                      # column(12, selectInput("EV_FIN", label = HTML('<FONT color="yellow"><em><h4>input file</em></h4></FONT>'), 
                      #                        #    choices = list("Demo_chiadrop", "Demo_p2chiadrop", "GAM001", "SPRITE" ), 
                      #                        choices = FLIST,
                      #                        selected = "F.Demo_P2CDP")),
                       column(12, selectInput("EV_FIN", label = HTML('<FONT color="yellow"><em><h4>input file</em></h4></FONT>'), 
                                              choices = FLIST,
                                              selected = "F.Demo_P2CDP")),
                      # 
                       column(12, selectInput("EV_FGENE", label = ("select genome"), 
                                              choices = list("dm3", "dm6", "hg19", "hg38", "mm9" ,"mm10" ), 
                                              selected = "dm3")),
                       # column(12,prettyRadioButtons("EV_INPUT_TYPE", label = HTML('<FONT color="yellow"><em><h4>input data select</em></h4></FONT>'),inline = TRUE,   fill = TRUE,shape="curve", choices = list("demo","input"),selected="input")),
                        #column(12,conditionalPanel("input.EV_INPUT_TYPE=='input'",
                       #                 fileInput("EV_FIN", label = HTML('<FONT color="yellow"><em><h4>input file</em></h4></FONT>')),
                       #                 fileInput("EV_FGENE", label = HTML('<FONT color="yellow"><em><h4>gene list</em></h4></FONT>')))),
                        column(12,selectInput("EV_GENE_INPUT",  label = HTML('<FONT color="yellow"><em><h4>gene select</em></h4></FONT>'), 
                                                             choices = "TEST_GENE,68812,+,chrX", selectize = truelength( "TEST_GENE,68812,+,chrX"), selected = "TEST_GENE,68812,+,chrX" )),
                      # TEST_FGN=data.frame("GC"="chrX","GS"=1614039,"GE"=3682851,"GNM"="TEST_GENE","GTP"="TEST_ISO","GST"="+","GLEN"=68812)
                     #  column(6, sliderInput("GENELENL", label =  HTML('<FONT color="yellow"><em></em><h4>Gene length ≥ </em></h4></FONT>'), min =1000,  max = 10000, value = 5000)),
                      # column(6, sliderInput("GENELENR", label =  HTML('<FONT color="yellow"><em></em><h4>Gene length ≥ </em></h4></FONT>'), min =1000,  max = 3784166, value =3784166 )),
                      column(12, sliderInput("GENELEN", label =  HTML('<FONT color="yellow"><em></em><h4>Gene length ≥ </em></h4></FONT>'), min =100,  max = 50000, value =50000,step=500 )),
                      
                     # column(12, sliderInput("GENELEN", label =  HTML('<FONT color="yellow"><em></em><h4>Gene length ≥ </em></h4></FONT>'), min =0,  max = 3784166, value = c(10000,3784166))),
                      column(12, HTML('<FONT color="yellow"><em><h4>fragment number/GEM</h4></em></FONT>'), align="left",offset=1),
                      
                            column(6,numericInput("EV_EFN_BE",label = "min",  value = 2)),
                        column(6,numericInput("EV_EFN_SE",label = "max",  value = 1000)),
                        column(12, HTML('<FONT color="yellow"><em><h4>TSS extension (bp)</h4></em></FONT>'), align="left",offset=1),
                        column(6,numericInput("EV_TSS_EXT_FRONT", label = ("front"), value = 2500,step=100)),
                        column(6,numericInput("EV_TSS_EXT_REAR", label = ("rear"), value = 2500,step=100)),
                     column(12, HTML('<FONT color="yellow"><em><h4>plot size</h4></em></FONT>'), align="left",offset=1),
                     column(6,numericInput("EV_WIDTH", label ="width", value = 800,step=50,width = '100%')),
                     column(6,numericInput("EV_HEIGHT", label = "height",value = 1000,step=50,width = '100%')),
                     column(12,sliderInput("EV_VIEW", label = HTML('<FONT color="yellow"><em><h4>plot extension before TSS</em></h4></FONT>'),  min = 0, max = 1, value = 0,step=0.1) ),
                     
                        column(12,prettyCheckbox("EX_INFO", label = "show extrusion info", value = TRUE))
               )#fluidRow
             
        ),#menuItem             
        menuItem(HTML('<FONT color="#00ffff"><h3>Extrusion Plot</h3></FONT>'), tabName = "dashboard",                             
         fluidRow( title="extrusion plot",
                 #  fluidRow( title="REFR",  column(12, submitButton(text="Update!",icon("refresh"),width='100%'))),
                   
                   
                 column(12,radioButtons("EV_FRAGTYPE", label="Fragment shape",inline = TRUE,
                                        choices = list("exact"="butt","round","square"),selected="square")),
                  column(12, HTML('<FONT color="yellow"><em><h4>elements size</h4></em></FONT>'), align="left",offset=1),
                  column(6,numericInput("EV_FRAG_SIZE", label = ("Fragment"), value = 0.3,step=0.05) ),
                  column(6,numericInput("EV_LINE_SIZE", label = ("Line"), value = 0.2,step=0.05)),
                  column(12, HTML('<FONT color="yellow"><em><h4>color</em></h4></FONT>'),  align="left",offset=1),
                  column(12,colourpicker::colourInput("EV_P_COLOR", "Promoter", "green")),
                  column(12,colourpicker::colourInput("EV_E_COLOR", "nonPromoter", "darkgreen")),
                  column(12,colourpicker::colourInput("EV_LINE_COLOR", "Line", "gray")),
                  column(12,colourpicker::colourInput("EV_PBACK_COLOR", "Promoter background", "yellow")),
                  column(12,numericInput("EV_RESOLUTION", label = HTML('<FONT color="yellow"><em><h4>plot resolution</em></h4></FONT>'),value = 600,step=100))
       )#fluidRow
        )#menuItem    
    ),##con2
    fluidRow( title="REFR",  column(12, submitButton(text="Update!",icon("refresh"),width='100%')))
    )#sidebarMenu(
),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabsetPanel(id="conditionedPanels",
               

      tabPanel("Fragment view", value=1, 
               fluidRow(
                 column(2, uiOutput("main_plot.ui")),
                 conditionalPanel("input.GENE_VIEW",
                                  column(12,imageOutput("GENE", height = "100%", width = "200%"))),
                 conditionalPanel("input.PE_VIEW",
                                  column(12,imageOutput("LV_MONO", height = "100%", width = "100%"))),
               conditionalPanel("input.COLOR_VIEW",
                                           column(12,imageOutput("LV_COLOR", height = "100%", width = "100%"))),
               

              conditionalPanel("input.LIB_VIEW",
                                           column(12,imageOutput("LV_LIB", height = "100%", width = "100%"))),
               
             conditionalPanel("input.CLUSTER_VIEW",
                                            column(12,imageOutput("CV", height = "100%", width = "100%"))),
        
             conditionalPanel("input.LINEAR_INFO",
               column(12, box("input file: ", verbatimTextOutput("INPUT_FILE")),box("gene file",verbatimTextOutput("GENELIST"))), 
               column(12, box("input data: ",verbatimTextOutput("P2")), box("gene list",verbatimTextOutput("GENE_TABLE"))), 
               column(12, box( "statistics: ",verbatimTextOutput("P3")),box("FN/GEM: ",tableOutput("P4"))))
                 
               )##fluidRow(
      ), # tabPanel("Linear",
      tabPanel("Gene body view", value=2,
             #  box("FIN name",verbatimTextOutput("EV_INPUT_FILE")),
               column(2, uiOutput("main_plot_ev.ui")),
              # column(10, submitButton("Update!",icon("refresh"))),
               column(12,box(verbatimTextOutput("ERGN"))),
               column(12,  imageOutput("PLOT_EVGENE", height = "10%", width = "100%")),
               column(12,  imageOutput("PLOT_EXTRUSION", height = "100%", width = "100%")),
            conditionalPanel("input.EX_INFO",
                column(12,  box(verbatimTextOutput("THIS_E_GENE")), box( "statistics: ",verbatimTextOutput("E_STA"))),        
               # column(12," E_WINPLOT",verbatimTextOutput("E_WINPLOT")),
                column(12, box("extrusion input file",verbatimTextOutput("EV_INPUT_FILE")),box("extrusion gene list",verbatimTextOutput("EV_GENELIST"))),
                column(12, box("input data: ",verbatimTextOutput("E_FIN")), box("gene list",verbatimTextOutput("E_GENELIST")))
               
             )

           
      )#Extrusion
  
    )# tabsetPanel

  )# dashboardBody

)# dashboardPage


Server<-function(input, output,session) {

  FINOUT =reactive({ 
    req(input$REGION_INPUT)
    RGN=input$REGION_INPUT
    RC=unlist(strsplit(RGN, "[:-]"))[[1]]
    
      req(input$FIN)
      output$INPUT_FILE <- renderText({paste0(input$FIN,"/",RC,".SUBRDS")})
      readRDS(paste0(input$FIN,"/",RC,".SUBRDS"))

  })#reactive({ 
  
  
  
  FGENELIST =reactive({ 

      req(input$FGENE)
        NAME=paste0(input$FGENE,".LONGEST_ISOFORM")
        read.table(NAME,sep="|")

  })#reactive({ 
  
  ##################
  #TEST_FGN=data.frame("GC"="chrX","GS"=3614039,"GE"=3682851,"GNM"="TEST_GENE","GTP"="TEST_ISO","GST"="+","GLEN"=68812)
  #TEST_GENE,68478,+,chr2L
  #GENENAME=unlist(strsplit(input$EV_GENE_INPUT, "[,]"))[[1]]
  
  
  FINOUT_EV =reactive({ 
    req(input$EV_GENE_INPUT)
    RC=unlist(strsplit(input$EV_GENE_INPUT, "[,]"))[[4]]
   
      req(input$EV_FIN)
      output$EV_INPUT_FILE <- renderText({paste0(input$EV_FIN,"/",RC,".SUBRDS")})
      readRDS(paste0(input$EV_FIN,"/",RC,".SUBRDS"))

  })#reactive({ 
  
  
  FGENELIST_EV =reactive({ 

      req(input$EV_FGENE)
      NAME=paste0(input$EV_FGENE,".LONGEST_ISOFORM")
      print(NAME)
      read.table(NAME,sep="|")
  })#reactive({ 
  ############################
  plotW <- reactive({  
    req(input$WIDTH); 
    as.numeric(input$WIDTH) 
  })
  plotH <- reactive({  
    req(input$HEIGHT); 
    as.numeric(input$HEIGHT) 
  })
  plotR <- reactive({  
    req(input$RESOLUTION); 
    as.numeric(input$RESOLUTION) 
  })
  

  
  output$main_plot <- renderPlot({
    
    
    
   
    FIN<-FINOUT()
    print ("After readRDS ~~~~~~~~~~~~~~~")
    #colnames(FIN)<-c("freg_chrom", "freg_start", "freg_end" ,"frag_num","gem_id")
    
    colnames(FIN)[1] <- "freg_chrom"
    colnames(FIN)[2] <- "freg_start"
    colnames(FIN)[3] <- "freg_end" 
    colnames(FIN)[4] <- "frag_num"
    colnames(FIN)[5] <- "gem_id"
    if(ncol(FIN)>5){
      colnames(FIN)[6] <- "PETYPE"
    }
    NAME="IN01[1,1]"
    
    print (head(FIN))
    
    output$P2 <- renderPrint({ head(FIN)})
    ## 1. set parameter
    ##1.1 REGION selection
    # RGN="chr2R:2169000-2517000"
    
    #RGN=input$REGION_INPUT
    RGN=input$REGION_INPUT

    RC=unlist(strsplit(RGN, "[:-]"))[[1]]

    RS=as.integer(gsub(",", "",unlist(strsplit(RGN, "[:-]"))[[2]]))
    RE=as.integer(gsub(",", "",unlist(strsplit(RGN, "[:-]"))[[3]]))
    RLEN=RE-RS
 #   RSADJ=as.integer(input$slider2[1])
#    READJ=as.integer(input$slider2[2])
    
 #   if (RSADJ<=0) { 
#      RS=round(RS+RLEN*RSADJ/100)
#    }else{
#      RS=round(RS-RLEN*RSADJ/100)
#    }
#    
#    if (READJ >= 100){
#     RE=round(RE+RLEN*(READJ-100)/100)
#    }else  {
#      RE=round(RE-RLEN*(100-READJ)/100)
#    }             
    RSADJ=as.integer(input$slider2L)
    NEWRS=round(RS+RLEN*RSADJ/100)
    READJ=as.integer(input$slider2R)                    
    NEWRE=round(RE+RLEN*READJ/100)
    
    RS=ifelse(NEWRS==0,0,RS)
    RE=ifelse(NEWRE==0,0,RE)
    
    
    NEWRLEN=NEWRE-NEWRS
    RS=ifelse(NEWRLEN<0,RS,NEWRS)
    RE=ifelse(NEWRLEN<0,RE,NEWRE)
    print("//////////////////")
    print (RS)
    print(RE)
    #  output$P3 <- renderPrint({ paste0(RC,":",RS,"-",RE)})
    
    #updateSliderInput(session,"slider2",value=c(RS,RE),min = ifelse((RS-RLEN)<0,0,(RS-RLEN)), max =ifelse((RE+RLEN)<0,0,(RE+RLEN)))
    # updateSelectInput(session,"ZOOM",value=0)
   # updateSliderInput(session,"ZOOM",value=0)
  # updateSliderInput(session,"MOVE",value=0)
    # RC;RS;RE
    # RC=unlist(strsplit(RGN, "[:-]"))[[1]]
    #  RS=as.integer(unlist(strsplit(RGN, "[:-]"))[[2]])
    #  RE=as.integer(unlist(strsplit(RGN, "[:-]"))[[3]])
   # RLEN=RE-RS
    # RC;RS;RE;RLEN
    ## 1.2 bin size
    #
    #BINNUM=100
    BINNUM=as.integer(input$BN)
    BINSIZE=round(RLEN/BINNUM)  
    
    # BINNUM;BINSIZE
    BIN_COOR_LIST<-seq(0,BINNUM,1)*BINSIZE+RS
    # BIN_COOR_LIST
    LOCAL_FN_BE=as.numeric(input$FN_BE)
    LOCAL_FN_SE=as.numeric(input$FN_SE)
    ##1.4 filter by local FN (local fregment number)
    #LOCAL_FN_REGION<-c(2,18) ## set region    mmLCFN ≤ LCFN ≤ MMLCFN
    #mmLCFN<-LOCAL_FN_REGION[1]
    #MMLCFN<-LOCAL_FN_REGION[2]
    mmLCFN=LOCAL_FN_BE;
    MMLCFN=LOCAL_FN_SE
    ##1.5 select hcluster method #"ward.D", "ward.D2", "single", "complete", "average" (= UPGMA),
    #"mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
    #METHOD_HC_ROW = "complete" #default
    METHOD_HC_ROW=input$HCM
    ##1.6 he distance measure to be used. This must be one of "euclidean", "maximum",
    #"manhattan", "canberra", "binary" or "minkowski". Any unambiguous substring can be given.
    # METHOD_DIST_ROW="euclidean" #default
    METHOD_DIST_ROW=input$DIST
    ## 1.7 plot freg size
    
    ##1.10 ALL INFO

    #################################
    # 2. data filtering
    # head(FIN);nrow(FIN)
    #"freg_chrom, freg_start, freg_end, frag_num, gem_id"
    
    P_ONLY=input$P_SELECT
   if (P_ONLY=="YES") {
      FLCL<-FIN%>%filter(freg_chrom==RC, freg_start>=RS, freg_end<=RE)
      FP<-FLCL%>%filter(PETYPE=="P")
      PGEM<-data.frame("gem_id"=FP$gem_id)
      PNLGL<-data.frame(table(PGEM$gem_id))
      FILTER_PNLGL<-PNLGL%>%filter(Freq>=input$PNL,Freq<input$PNR)
      FILTER_PNLGL_GEMID<-data.frame("gem_id"=FILTER_PNLGL$Var1)
      print ("XXXXXXXXXXXXXXXXX")
       print(head(PNLGL))
       print(head(FILTER_PNLGL))
        print ("XXXXXXXXXXXXXXXXX")
      AIN0<-merge(FLCL,FILTER_PNLGL_GEMID,by="gem_id")
      print(head( AIN0))

    }else{
      AIN0<-FIN%>%filter(freg_chrom==RC, freg_start>=RS, freg_end<=RE)
    }
   
   
    AIN0_FREQ<-data.frame(table(AIN0$gem_id))

    AIN0_FREQ_FILTER<-AIN0_FREQ%>%filter(Freq>=mmLCFN, Freq<=MMLCFN)  ## local FN filtering
    NDATA=nrow(AIN0_FREQ_FILTER)
    while (NDATA<2){
        mmLCFN=mmLCFN-1
      AIN0<-FIN%>%
        filter(freg_chrom==RC, freg_start>=RS, freg_end<=RE)
      AIN0_FREQ<-data.frame(table(AIN0$gem_id))
      AIN0_FREQ_FILTER<-AIN0_FREQ%>%filter(Freq>=mmLCFN, Freq<=MMLCFN)  ## local FN filtering
      NDATA=nrow(AIN0_FREQ_FILTER)
 
    }
    
    print ("416~~~~~~~~~~~~~~")
    print (paste("FIN",nrow(FIN)))
    print (paste("AIN0_FREQ",nrow(AIN0_FREQ)))
    print (head(AIN0_FREQ))
    print ("417~~~~~~~~~~~~~~")
    
    print ("336")
    print(mmLCFN)
    print(NDATA)
    print(head(AIN0_FREQ_FILTER))
    print("---------") 
    updateNumericInput(session,"FN_BE",value=mmLCFN)
    updateTextInput(session,"REGION_INPUT",value=paste0(RC,":",RS,"-",RE))
    updateSliderInput(session,"slider2L",   min = -100,  max = 49, value = 0)
    updateSliderInput(session,"slider2R",   min = -49,  max = 100, value = 0)
    INFO=paste("REGION",RC,RS,RE,
               "\nRGN_LEN",RLEN,
               "\nBINSIZE:",BINSIZE,
               "\nBIN_NUM",BINNUM,
               "\nLOCAL_FN",mmLCFN,MMLCFN,
               "\nMETHOD_HC_ROW",METHOD_HC_ROW,
               "\nMETHOD_DIST_ROW",METHOD_DIST_ROW,
               sep=",")
    # INFO
    
    
    #head(AIN0_FREQ_FILTER)
    colnames(AIN0_FREQ_FILTER)<-c("gem_id","LCFN")
    AIN<-merge(AIN0,AIN0_FREQ_FILTER,by="gem_id")
    print( head(AIN0))
    # head(AIN);nrow(AIN)
    
    AIN2<-AIN%>%separate(gem_id,c("lib","sub_group","GEMNUMID","GEMSEQ","READHEEAD","SOURCE","GEMFN","THISFN"),"-",extra = "drop")
    # head(AIN2)
    LOCAL_UNIQ_GEM<-unique(AIN2$GEMNUMID)
    LOCAL_UNIQ_GEM_NUM<-length(LOCAL_UNIQ_GEM)
    # LOCAL_UNIQ_GEM_NUM
    ###################################
    # 3. make matrix
    ## 3.1 mk 0 matrix
    
    length(BIN_COOR_LIST)
    #(LOCAL_UNIQ_GEM)
    DFM<-data.frame(matrix(0,nrow=LOCAL_UNIQ_GEM_NUM,ncol=BINNUM+1))
    #DFM
    colnames(DFM)<-BIN_COOR_LIST
    rownames(DFM)<-LOCAL_UNIQ_GEM
    
    
    #head(AIN);nrow(AIN);
    # head(AIN2);nrow(AIN2)
    ## mark each frag with rowname(GEMNUMID) and colname(BIN_POS_FNMID)
    AIN3<-AIN2%>%mutate(BIN_POS_FNMID=RS+floor(((freg_end-freg_start)/2+freg_start-RS)/BINSIZE)*BINSIZE)
    # head(AIN3)
    #tail(AIN3)
    #AIN3[100165042,2169000]
    
    ## fill fregment in to matrix
    for (i in 1:nrow(AIN3)){
      #for (i in 1:3){
      ROWNAM=as.character(AIN3$GEMNUMID[i])
      COLNAM=as.character(AIN3$BIN_POS_FNMID[i])
      #print(ROWNAM)
      # print(COLNAM)
      # print (DFM[ROWNAM,COLNAM])
      DFM[ROWNAM,COLNAM]=1
      #  print (DFM[ROWNAM,COLNAM])
      
    }
    print ("544")
    print ( colSums(DFM))
    print ("546")
    ROWNAM=AIN3$GEMNUMID[1]
    #ROWNAM
    CUTR=input$CUTROW
    CC1=input$CCOLOR1
    CC2=input$CCOLOR2
    #library(pheatmap)
    res<-pheatmap(DFM,legend = FALSE,
                  cluster_rows=TRUE,
                  cluster_cols=FALSE,
                  boder=FALSE,
                  border_color =NA,
                  cutree_rows = CUTR,
                  treeheight_row = 2.5, 
                  treeheight_col = 0,
                  clustering_method =METHOD_HC_ROW,
                  clustering_distance_rows = METHOD_DIST_ROW,
                  color =colorRampPalette(c(CC1,CC2))(2),
                  annotation_colors=CC1,
                  show_rownames=FALSE,show_colnames = FALSE,
                  silent = TRUE)
    
    
    
    DF<-data.frame("A","B",stringsAsFactors = FALSE)
    for (i in 1:length(res$tree_row$order)){
      ROW_CLUSTER_ORDER=i
      ROW_CLUSTER_NAME=res$tree_row$labels[res$tree_row$order[i]]
      DF[ROW_CLUSTER_ORDER,] <- list(ROW_CLUSTER_ORDER,ROW_CLUSTER_NAME)
    }
    # head(DF)
    colnames(DF)<-c("related_plot_line_num","GEMNUMID")
    #  head(AIN2)
    
    print("422")
    MIN<-merge(AIN2,DF,by = "GEMNUMID")
    WIN01=MIN
    # str(WIN01)
    WIN01<-transform(WIN01, related_plot_line_num = as.numeric(related_plot_line_num))
    YBREAK=20 ## howmany scales in yaxis
    TEXTSIZE=16 ## text size in plot
    LH=as.numeric(input$LSIZE)
    FH=as.numeric(input$FSIZE)
    
    PCOLOR1=input$CCOLOR3 
    PCOLOR2=input$CCOLOR4
    PCOLOR3=input$CCOLOR5
    
    # head(WIN01)
    YMIN=-max(as.integer(WIN01$related_plot_line_num))
    YMAX=0
    FTP=input$FRAGTYPE
    
    ###
    FGN=FGENELIST()
    output$GENE_TABLE <- renderPrint({ head(FGN)})
    #FGN=readRDS(GN)
    head(FGN)
    colnames(FGN)<-c("GC","GS","GE","GNM","GTP","GST","GLEN")
    SFGN<-FGN%>%filter(GC==RC,GS>=RS,GE<=RE)
    ###
    EG=0
    SFGN$direction <- ifelse(SFGN$GST == "+", 1, -1)
    SFGN$GEMNUMID <- SFGN$GNM
    if (input$GENE_STYLE=="compressed"){
      GH=200
      PLOT_GENE<-ggplot(SFGN,aes(xmin=GS,xmax=GE,y=1,fill=GNM,label=GNM,forward = direction))+
        geom_vline(xintercept = RS,color="gray",size=0.1)+
        geom_vline(xintercept = RE,color="gray",size=0.1)+
        geom_gene_arrow(arrowhead_width = grid::unit(1, "mm"),
                        arrowhead_height = grid::unit(1, "mm"),
                        arrow_body_height = grid::unit(0.7, "mm"),
                        size=0.1)+
        geom_gene_label(align = "center",reflow = TRUE,grow = TRUE,
                        min.size = 0.5,
                        fontface = "italic",
                        height = grid::unit(2, "mm")
        ) +
        # scale_fill_manual(values=c("+"="green","-"="#66ccff"))+
        scale_x_continuous(limits=c(RS-EG,RE+EG),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
        theme_bw()+
        theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              panel.background = element_rect(fill=PCOLOR3),panel.border = element_rect(size=0),
              plot.title = element_blank(),axis.title = element_blank(),axis.text = element_blank(),
              axis.ticks = element_blank())
    }else{
      GH=plotH()
      PLOT_GENE<-ggplot(SFGN,aes(xmin=GS,xmax=GE,y=GNM,fill=GNM,label=GNM,forward = direction))+
        geom_vline(xintercept = RS,color="gray",size=0.1)+
        geom_vline(xintercept = RE,color="gray",size=0.1)+
        geom_gene_arrow(arrowhead_width = grid::unit(1, "mm"),
                        arrowhead_height = grid::unit(1, "mm"),
                        arrow_body_height = grid::unit(0.7, "mm"),
                        size=0.1)+
        geom_gene_label(align = "center",reflow = TRUE,grow = TRUE,
                        min.size = 0.5,
                        fontface = "italic",
                        height = grid::unit(2, "mm")
        ) +
        # scale_fill_manual(values=c("+"="green","-"="#66ccff"))+
        scale_x_continuous(limits=c(RS-EG,RE+EG),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
        theme_bw()+
        theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              panel.background = element_rect(fill=PCOLOR3),panel.border = element_rect(size=0),
              plot.title = element_blank(),axis.title = element_blank(),axis.text = element_blank(),
              axis.ticks = element_blank())
    }
    
    print (head(WIN01))
    PIN03<-ggplot(WIN01,aes(group=GEMNUMID,fill=GEMNUMID,color=GEMNUMID))+
      geom_vline(xintercept = RS,color="gray",size=0.1)+
      geom_vline(xintercept = RE,color="gray",size=0.1)+
      theme_bw()+
      theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.background = element_rect(fill=PCOLOR3),panel.border = element_rect(size=0),
            plot.title = element_blank(),axis.title = element_blank(),axis.text = element_blank(),
            axis.ticks = element_blank())+
      scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
      geom_line(aes(x=freg_start,y=-related_plot_line_num),size=LH,color=PCOLOR1)+
      geom_segment(aes(x=freg_start,xend=freg_end,y=-related_plot_line_num,yend=-related_plot_line_num),
                   size=FH,lineend = FTP,color=PCOLOR2)
    if(ncol(FIN)>5){PIN03<-PIN03+ geom_segment(data=filter(WIN01,PETYPE=="P"),aes(x=freg_start,xend=freg_end,y=-related_plot_line_num,yend=-related_plot_line_num),
                   size=FH,lineend = FTP,color=input$CCOLOR6)}
    PIN03COLOR<-ggplot(WIN01,aes(group=GEMNUMID,fill=GEMNUMID,color=GEMNUMID))+
      geom_vline(xintercept = RS,color="gray",size=0.1)+
      geom_vline(xintercept = RE,color="gray",size=0.1)+
      theme_bw()+
      theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.background = element_rect(fill=PCOLOR3),panel.border = element_rect(size=0),
            plot.title = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
      scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
      geom_line(aes(x=freg_start,y=-related_plot_line_num),size=LH)+
      geom_segment(aes(x=freg_start,xend=freg_end,y=-related_plot_line_num,yend=-related_plot_line_num),
                   size=FH,lineend = FTP)
    PIN03LIB<-ggplot(WIN01,aes(group=GEMNUMID,fill=lib,color=lib))+
      geom_vline(xintercept = RS,color="gray",size=0.1)+
      geom_vline(xintercept = RE,color="gray",size=0.1)+
      theme_bw()+
      theme(legend.position="bottom",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            legend.text=element_text(size=2),legend.title = element_text(colour="blue", size=2, face="bold"),legend.key.size = unit(0.1, 'lines'),
            panel.background = element_rect(fill=PCOLOR3),panel.border = element_rect(size=0),
            plot.title = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
      scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
      geom_line(aes(x=freg_start,y=-related_plot_line_num),size=LH)+
      geom_segment(aes(x=freg_start,xend=freg_end,y=-related_plot_line_num,yend=-related_plot_line_num),
                   size=FH,lineend = FTP)+
      guides(fill=guide_legend(ncol=12))
    
    
    TIME<-format(Sys.time(), "%Y%m%d.%H%M%S")
    
    
    
    
    DFINFO=data.frame(t(data.frame(
      "bin number"=BINNUM,
      "bin size"=BINSIZE,
      "region"=paste0(RC,":",RS,"-",RE),
      "FN/GEM"=paste0(mmLCFN,"~",MMLCFN),
      "hclustering method"=METHOD_HC_ROW,
      "distace function"=METHOD_DIST_ROW,
      "GEM count"=-YMIN,
      "Fragment count"=nrow(WIN01)
    )))
    
    colnames(DFINFO)<-c("values")
    output$P3 <- renderPrint({DFINFO}) 
    
    INFO_WIN01<-data.frame((table(unique(select(WIN01,GEMSEQ,LCFN))$LCFN)))
    colnames(INFO_WIN01)<-c("FNperGEM","GEM_count")
    rownames(INFO_WIN01)<-INFO_WIN01$FNperGEM
    #  output$P4 <- renderPrint({INFO_WIN01}) 
    output$P4 <- renderTable({xtable(INFO_WIN01)})
    output$P5 <- renderPrint({ head(WIN01)})
    
    
    # Plot the data ####
    output$LV_COLOR <- renderImage({
      outfile_LV_COLOR <- tempfile(fileext = '.png')
      png(outfile_LV_COLOR, 
          width = plotW(), 
          height = plotH(),
          res = plotR())
      grid.arrange(PIN03COLOR)
      dev.off()
      list(src = outfile_LV_COLOR,
           contentType = 'image/png',
           width = plotW(),
           height =plotH(),
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    output$LV_MONO <- renderImage({
      outfile_LV_MONO <- tempfile(fileext = '.png')
      png(outfile_LV_MONO, 
          width = plotW(), 
          height = plotH(),
          res = plotR())
      grid.arrange(PIN03)
      dev.off()
      list(src = outfile_LV_MONO,
           contentType = 'image/png',
           width = plotW(),
           height =plotH(),
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    output$LV_LIB <- renderImage({
      outfile_LV_LIB <- tempfile(fileext = '.png')
      png(outfile_LV_LIB, 
          width = plotW(), 
          height = plotH()*1.2,
          res = plotR())
      grid.arrange(PIN03LIB)
      dev.off()
      list(src = outfile_LV_LIB,
           contentType = 'image/png',
           width = plotW(),
           height =plotH()*1.2,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    output$CV <- renderImage({
      outfile_CV <- tempfile(fileext = '.png')
      png(outfile_CV, 
          width = plotW(), 
          height = plotH(),
          res = plotR())
      grid.arrange(res[[4]])
      dev.off()
      list(src = outfile_CV,
           contentType = 'image/png',
           width = plotW(),
           height =plotH(),
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    output$GENE <- renderImage({
      outfile_GENE <- tempfile(fileext = '.png')
      png(outfile_GENE, 
          width = plotW(), 
          height = GH,
          res = plotR())
      grid.arrange(PLOT_GENE)
      dev.off()
      list(src = outfile_GENE,
           contentType = 'image/png',
           width = plotW(),
           height =GH,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    
    
    
  })## main_plot##########################################
  
  output$main_plot.ui <- renderUI({ 
    plotOutput("main_plot", width = 5,height=5)
  })
  
  


  
  
  EV_plotW <- reactive({  
    req(input$EV_WIDTH); 
    as.numeric(input$EV_WIDTH) 
  })
  EV_plotH <- reactive({  
    req(input$EV_HEIGHT); 
    as.numeric(input$EV_HEIGHT) 
  })
  EV_plotR <- reactive({  
    req(input$EV_RESOLUTION); 
    as.numeric(input$EV_RESOLUTION) 
  })
  
  output$main_plot_ev <- renderPlot({

    print ("After readRDS ~~~~~~~~~~~~~~~")
    FIN<-FINOUT_EV()

    
    
    
     #colnames(FIN)<-c("freg_chrom", "freg_start", "freg_end" ,"frag_num","gem_id")

      colnames(FIN)[1] <- "freg_chrom"
      colnames(FIN)[2] <- "freg_start"
      colnames(FIN)[3] <- "freg_end" 
      colnames(FIN)[4] <- "frag_num"
      colnames(FIN)[5] <- "gem_id"
    if (ncol(FIN)>=6){
      colnames(FIN)[6] <- "PETYPE"
    }


  
  ##########~~~~~~~~~~~~~~
  
   
    
    EIN=FIN[,1:5]
    head(EIN)
    output$E_FIN <- renderPrint({ head(EIN)})
    GENENAME=unlist(strsplit(input$EV_GENE_INPUT, "[,]"))[[1]]
    
    #GENENAME="luna"
    #GENENAME="roX2"
    #GENENAME="lilli"
    #GN<-"UCSC_Known_Genes_dm3.txt.LONGESTISO.sC.bed6.rds"
    FGN=FGENELIST_EV()
   # output$E_GENELIST <- renderPrint({ head(FGN)})
    
    #FGN=readRDS(GN)
    head(FGN)
    colnames(FGN)<-c("GC","GS","GE","GNM","GTP","GST","GLEN")
    print(head(FGN))
    TEST_FGN=data.frame("GC"="chrX","GS"=3614039,"GE"=3682851,"GNM"="TEST_GENE","GTP"="TEST_ISO","GST"="+","GLEN"=68812)
    FGN<-rbind(TEST_FGN,FGN)
    print(head(FGN))
    GENEINFO<-FGN%>%
      mutate(INDEX=paste(GNM,GLEN,GST,GC,sep=","))%>%
      filter(as.integer(GLEN)>as.integer(input$GENELEN))%>%
      arrange(GNM)
    filter(FGN,GNM==GENENAME)
    THISGENE<-filter(FGN,GNM==GENENAME)
    THISPTN<-filter(GENEINFO,GNM==GENENAME)
   # updateSelectInput(session, "EV_GENE_INPUT",choices = GENEINFO$INDEX,selected =THISPTN$INDEX)
    FILTERED_GENEINFO_NOORDER<-GENEINFO%>%  filter(as.integer(GLEN)>=as.integer(input$GENELEN))
    
    FILTERED_GENEINFO<-arrange(FILTERED_GENEINFO_NOORDER,GNM)

    print("9999999999999999")
    print(head(FILTERED_GENEINFO))
   # FILTERED_GENEINFO<-GENEINFO
   # FILTERED_GENEINFO<-GENEINFO
    FILTERED_GENEINFO<-FILTERED_GENEINFO[order(FILTERED_GENEINFO$GLEN,decreasing = TRUE),]
    output$E_GENELIST <- renderPrint({ head(FILTERED_GENEINFO)})
    updateSelectInput(session, "EV_GENE_INPUT",choices = GENEINFO$INDEX,selected =THISPTN$INDEX)

    
    
    # grep("roX",FGN$GNM)

    head(THISGENE)
    GLEN=THISGENE$GE-THISGENE$GS
    PST=levels(droplevels(THISGENE$GST))
    ERC=levels(droplevels(THISGENE$GC))
    #TE=5000
    TEF=input$EV_TSS_EXT_FRONT
    TER=input$EV_TSS_EXT_REAR
    PC=ERC
    EG=as.numeric(input$EV_VIEW)
    #EG=1
    if (THISGENE$GST=="+"){
      TSS=THISGENE$GS
      PS=TSS-TEF
      PE=TSS+TER
      ERS=PS-EG*GLEN
      ERE=THISGENE$GE
    }else{  ##if (THISGENE$GST=="-")
      TSS=THISGENE$GE
      PS=TSS-TER
      PE=TSS+TEF
      ERS=THISGENE$GS
      ERE=PE+EG*GLEN
    }
    
    
    PC;PS;PE;PST
    ERC;ERS;ERE;PST
    
    
    
    
    
 #   library(dplyr)
    
    
    ERC;ERS;ERE
    rm(RIN)
    RIN<-EIN%>%filter(freg_chrom==ERC,freg_start>=ERS,freg_end<=ERE)
    nrow(RIN)
    head(RIN)
    RIN<-RIN%>%add_rownames()
    head(RIN)
    ######## get local FN
    LOCAL_FN_LIST<-data.frame(table(RIN$gem_id))
    head(LOCAL_FN_LIST)
    #LOCAL_FN_LIST<-data.frame(LOCAL_FN_LIST=summary(RIN$gem_id))
    colnames(LOCAL_FN_LIST)<-c("gem_id","local_fn")
    head(filter(LOCAL_FN_LIST,local_fn>1))
    ##########
    LRIN<-merge(RIN,LOCAL_FN_LIST,by="gem_id")
    head(LRIN)
    nrow(LRIN)
    #LFN=2
   # library(data.table)
    RIN_AF<-LRIN%>%filter(local_fn>=input$EV_EFN_BE,local_fn<=input$EV_EFN_SE)
    RINP<-RIN_AF%>%filter(!(freg_start > PE | freg_end  < PS))## promoter ovlp
    RINP<-RINP%>%mutate(PETYPE="P")
    nrow(RINP)
    nrow(RIN_AF)
    head(RINP)
    
    
    
    
    ### get uniq GEM ID of the gene
    
    UGEM_RINP<-data.frame(UGEM=unique(RINP$gem_id),occupied=GENENAME)
    head(UGEM_RINP)
    nrow(UGEM_RINP)
    #### get all fragmen wthi these UNIQ GEM ID
    #RIN[grep("SHG0030N-101426823-FEA.3.2",RIN$gem_id),]
    head(RIN)
    MIN01<-merge(RIN,UGEM_RINP,by.x="gem_id",by.y="UGEM",drop=TRUE)
    nrow(MIN01) ## total local FN
    head(MIN01)
    head(merge(MIN01,RINP,by="rowname",all=TRUE))
    head(merge(MIN01,RINP,by="rowname",all.y=TRUE,incomparables = "PP"))
    
    MIN02<-merge(MIN01,RINP,by="rowname",all=TRUE)
    
    head(MIN02)
    rm(MIN03)
    MIN03<-MIN02%>%mutate(NEWTYPE=ifelse(is.na(PETYPE),"E","P"))%>%
      select(-gem_id.y, -freg_chrom.y, -freg_start.y, -freg_end.y,-frag_num.y,  -PETYPE, -local_fn)
    head(MIN03)
    colnames(MIN03)<-c("rowname","gem_id", "freg_chrom", "freg_start", "freg_end", "PE_num", "occupied", "PETYPE")
    head(MIN03)
    unique(MIN03$PETYPE)
    MIN<-MIN03
    #### get GEM COV of EACH
    EACH_GEM_LEND<-aggregate(MIN$freg_start,by=list(MIN$gem_id),min)
    colnames(EACH_GEM_LEND)<-c("gem_id","EACH_GEM_LEND")
    head(EACH_GEM_LEND)
    EACH_GEM_REND<-aggregate(MIN$freg_end,by=list(MIN$gem_id),max)
    colnames(EACH_GEM_REND)<-c("gem_id","EACH_GEM_REND")
    head(EACH_GEM_REND)
    MGD_EACH_GEM_ENDS<-merge(EACH_GEM_LEND,EACH_GEM_REND,by="gem_id")
    MGD_EACH_GEM_LEN<-MGD_EACH_GEM_ENDS%>%mutate(EACH_GEM_COV=EACH_GEM_REND-EACH_GEM_LEND)
    head(MGD_EACH_GEM_LEN)
    #  SORT_MGD_EACH_GEM_LEN<-MGD_EACH_GEM_LEN[order(MGD_EACH_GEM_LEN$EACH_GEM_COV,decreasing = TRUE),]
    if(PST=="+"){
      SORT_MGD_EACH_GEM_LEN<-MGD_EACH_GEM_LEN[order(MGD_EACH_GEM_LEN$EACH_GEM_REND,decreasing = TRUE),]
    }else if (PST=="-"){
      SORT_MGD_EACH_GEM_LEN<-MGD_EACH_GEM_LEN[order(MGD_EACH_GEM_LEN$EACH_GEM_LEND,decreasing = FALSE),]
    }
    head(SORT_MGD_EACH_GEM_LEN)
    SORT_MGD_EACH_GEM_LEN$YPOS=1:nrow(SORT_MGD_EACH_GEM_LEN)
    ##################################
    WINE<-merge(MIN,SORT_MGD_EACH_GEM_LEN)
    head(WINE)
    
    library(ggplot2)
    #####################
    #select Y range
    YMIN=0
    YMAX=max(WINE$YPOS)
    #YMAX=20
    ##############
    WINPLOT<-WINE%>%
      filter(YPOS>=YMIN, YPOS<=YMAX)
    ERS;ERE
    head(WINPLOT)
    THISGENE$direction <- ifelse(THISGENE$GST == "+", 1, -1)
    EV_FTP=input$EV_FRAGTYPE
    PLOT_STEP<-ggplot(WINPLOT)+
      geom_rect(aes(xmin=PS,xmax=PE,ymin=YMIN,ymax=YMAX),size=0,fill=input$EV_PBACK_COLOR,alpha=0.5)+
      geom_vline(xintercept = ERS,color="gray",size=0.02)+
      geom_vline(xintercept = TSS,color="gray",size=0.02)+
      geom_vline(xintercept = ERE,color="gray",size=0.02)+

        geom_line(aes(x=freg_start,y=YPOS,group=gem_id),color=input$EV_LINE_COLOR,size=input$EV_LINE_SIZE)+
        geom_segment(aes(x=freg_start,xend=freg_end,y=YPOS,yend=YPOS,group=gem_id,color=PETYPE),lineend=EV_FTP,size=input$EV_FRAG_SIZE)+
        scale_color_manual(values=c("P"=input$EV_P_COLOR,"E"=input$EV_E_COLOR))+
        scale_x_continuous(breaks=c(ERS,TSS,ERE), label=scales::comma)+
        theme_void()+
        coord_cartesian(xlim=c(ERS,ERE))+
  
        theme(legend.position = "none")



     PLOT_GENE<-ggplot(THISGENE,aes(x=GS,xend=GE,y=1,yend=1,fill=GST,label=GNM,forward = direction,color=GST))+
       geom_vline(xintercept = ERS,color="gray",size=0.02)+
       geom_vline(xintercept = TSS,color="gray",size=0.02)+
       geom_vline(xintercept = ERE,color="gray",size=0.02)+
       geom_segment(size=0.1,arrow = arrow( length=unit(seq(1,.5,length=.5),"mm"),ends=ifelse(THISGENE$GST=="+","last","first")))+
       geom_point(aes(x=ifelse(THISGENE$GST=="+",GS,GE)),size=0.8,shape="|")+
       geom_text(aes(x=GS+(GE-GS)/2,y=1,label=GNM),inherit.aes = TRUE,color="black",fontface = "italic",size=1)+
       scale_color_manual(values=c("+"="green","-"="#66ccff"))+
       scale_fill_manual(values=c("+"="green","-"="#66ccff"))+
       scale_x_continuous(breaks=c(ERS,TSS,ERE), label=scales::comma)+
       coord_cartesian(xlim=c(ERS,ERE))+
       theme_void()+
       theme(legend.position = "none")

    output$PLOT_EXTRUSION <- renderImage({
      outfile4 <- tempfile(fileext = '.png')
      # Generate the PNG
      png(outfile4, 
          width = EV_plotW(), 
          height = EV_plotH(),
          res = EV_plotR())
      grid.arrange(PLOT_STEP)
     # grid.arrange(arrangeGrob(PLOT_STEP,PLOT_GENE,ncol=1,heights=c(9,1)))
      dev.off()
      list(src = outfile4,
           contentType = 'image/png',
           width = EV_plotW(),
           height =EV_plotH(),
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    output$PLOT_EVGENE <- renderImage({
      outfile5 <- tempfile(fileext = '.png')
      # Generate the PNG
      png(outfile5, 
          width = EV_plotW(), 
          height = 60,
          res = EV_plotR())
      grid.arrange(PLOT_GENE)
      dev.off()
      list(src = outfile5,
           contentType = 'image/png',
           width = EV_plotW(),
           height =60,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    EV_DFINFO=data.frame(t(data.frame(
      "GEM count"=YMAX,
      "Fragment count"=nrow(WINPLOT),
      "Promoter fragment number"=nrow(filter(WINPLOT,PETYPE=="P" )),
      "NonPromoter fragment number"=nrow(filter(WINPLOT,PETYPE=="E" )),
      "Left NonPromoter fragment number"=nrow(filter(WINPLOT,PETYPE=="E",freg_end < TSS )),
      "Right NonPromoter fragment number"=nrow(filter(WINPLOT,PETYPE=="E",freg_start > TSS ))
    )))

    colnames(EV_DFINFO)<-c("values")

     output$E_WINPLOT <- renderPrint({ head(WINPLOT)})
      output$THIS_E_GENE <- renderPrint({ head(THISGENE)})
      output$E_STA <- renderPrint({ EV_DFINFO  })
      
      output$ERGN <- renderText({ paste0(PC,":",ERS,"-",ERE)})
      
  })## main_plot_ev
  output$main_plot_ev.ui <- renderUI({ 
     plotOutput("main_plot_ev", width = 5,height=5)
  })##main_plot_ev.ui 

  
  ###########~~~~~~~~~~~~~~~

}

shinyApp(ui, Server)
