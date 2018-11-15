shinyServer(function(input, output){
  source(file.path("enrich_dataset.R"),  local = TRUE)$value

  options(shiny.maxRequestSize=30*1024^2)
  #############################################################################################
  ##########          Chargement du dataset                      #############################
  #############################################################################################

  #This function is repsonsible for loading in the selected file
  filedata <- reactive({



    infile <- input$datafile
    if (is.null(infile) & input$Demo==0) {
      # User has not uploaded a file yet
      return(NULL)
    }
    else if (!is.null(infile)){
    read.csv(infile$datapath, na.strings = ".")

    }

    else {infile <- input$datafile

    read.csv("www/DataSet_type.csv", na.strings = ".")}
  })



  #############################################################################################
  ##########          Informations Globales                      #############################
  #############################################################################################

  #The following set of functions populate the column selectors
  output$Variables <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)

    items=names(df)
    names(items)=items
    selectInput("varlist", "Variables list:",items,multiple=F)

  })

  #time variables
  output$TimeVar <- renderUI({

    selectInput("TimeVar", "Choose Time Variable:",input$Time_related$Right,multiple=F)

  })

  #Continuous covariates
  output$ContinuousVar <- renderUI({
    # df <-filedata()
    # if (is.null(df)) return(NULL)
    # test <- df %>% summarise_each(funs(n_distinct))
    # items=names(as.data.frame(test)[,test>10])
    # names(items)=items
    selectInput("ContinuousVar", "Variables list:",c("",input$mychooser$Left),multiple=F,selected = "NULL")

  })

  output$Groupby <- renderUI({
    # df <-filedata()
    # if (is.null(df)) return(NULL)
    # test <- df %>% summarise_each(funs(n_distinct))
    # items=names(as.data.frame(test)[,test<=10])
    # names(items)=items
    selectInput("bylist", "By:",input$mychooser$Right,multiple=F,selected = "NULL")


  })

  output$Subjects <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)

    if (input$DoseList=="No variable selected"){df}
    else {

      ff <- as.formula(paste("~", input$Dosefilter, "==", input$DoseList))
      df<- df %>% filter_(ff)
    }
    subjid=levels(as.factor(df$ID))
   #subjid= unique(df[[ID]])

    selectInput("IDlist", " ID selection :",subjid,multiple=TRUE, selected = "NULL")

  })

  output$MeasurementCMT <- renderUI({
    req(df) <-filedata()
    if (is.null(df)) return(NULL)
    listCMT=levels(as.factor(df$CMT))
    selectInput("CMTlist", " CMT for drug measurment:",listCMT,multiple=FALSE )

  })

  #Je mets les infos globales dans les valueBOX
  output$check_box <- renderInfoBox({

    infileraw <- input$datafile
    if (is.null(infileraw) & input$Demo==0) {
      textfile="Welcome in PMxplore !!! Select your dataset !"
      colorcheck="purple"
      iconcheck="hand-o-up"
      labelcheck=""

    }
    else if (!is.null(infileraw)){
      L=readLines(infileraw$datapath)
      L1=readLines(infileraw$datapath,n=1)

      numfields <-count.fields(textConnection(L),sep=",")
      numfields1 <-count.fields(textConnection(L1),sep=",")
      x=numfields[numfields!=numfields1]

      if (length(x)==0){textfile=" OK"
                        colorcheck="green"
                        iconcheck="check-square"
                        labelcheck="Check number of rows and columns"}
                  else {textfile="Error in Dataset, inconsistent number of rows or cols"
                        colorcheck="red"
                        iconcheck="thumbs-down"
                        labelcheck="Check number of rows and columns"}
    }

    else if(input$Demo!=0){
          textfile= "OK"
          colorcheck="green"
          iconcheck='check-square'
          labelcheck="Check number of rows and columns"
      }


    infoBox(title=labelcheck, textfile, color = colorcheck, icon = icon(iconcheck))
  })



  output$subjects_box <- renderInfoBox({
    df <-filedata()
    #if (is.null(df)) return("NULL")
    dataset<- as_tibble(req(df))
    IDnew <-as.factor(dataset$ID)
    number_of_subjects=nlevels(IDnew)

    infoBox("Number of subjects:", number_of_subjects,  icon = icon("users"))
  })

  output$observations_box <- renderInfoBox({
    df <-filedata()
    #if (is.null(df)) return(NULL)
    dataset<- as_tibble(req(df))
    number_of_observations=nrow(dataset)
    infoBox("Number of observations:", number_of_observations,icon = icon("line-chart"))
  })

  output$compartments_box <- renderInfoBox({
    df <-filedata()
    #if (is.null(df)) return(NULL)
    dataset<- as_tibble(req(df))

    number_of_compartments=nlevels(as.factor(dataset$CMT))

    infoBox("Number of compartments:", number_of_compartments, icon = icon("sitemap"))
  })

  output$doses_box <- renderInfoBox({
    df <-filedata()
    #if (is.null(df)) return(NULL)
    df<- as_tibble(req(df))

    df2<- df %>% select(input$Dose_related$Right) %>% select(1) %>% setNames("DOSE")%>% mutate(DOSE=as.factor(DOSE))
    paste(levels(df2$DOSE), collapse=", ")

    list_of_doses=paste(levels(df2$DOSE),collapse=", ")


    infoBox("list of doses:",list_of_doses, icon = icon("arrow-circle-down"))
  })



  #############################################################################################
  ##########          Dataset View                                #############################
  #############################################################################################

  #This previews the CSV data file
   output$filetable <- DT::renderDataTable({filedata()},
     options = list(pageLength = 20, lengthMenu=c(10,20,40,100),dom = 'lftip', paging=TRUE, scrollX = TRUE,  filter='top',
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#4682B4', 'color': '#fff'});",

                      "}")
     ))
   #)

    #)


   #############################################################################################
   ##########          Continuous Covariates                      #############################
   #############################################################################################

    # box plot with plotly:
    output$fileboxplot <-  renderPlotly({


      df <-if (input$covariates_values_type==0)
                                {tbl_df(filedata())}
        else if (input$covariates_values_type==1)
                                {tbl_df(filedata()%>% dplyr::arrange(ID,TIME,AMT)  %>%  group_by(ID) %>% slice(1L))}


       # g1 <<- ggplot(df, aes_string(x=input$bylist,y=input$varlist,fill=input$bylist, group=input$bylist))+
       #   geom_boxplot()+
       #   stat_summary(fun.y=mean,geom="point",shape=5, size=2)

      # mp <- aes(x=as.factor(SEX), fill=as.factor(SEX))

       mp<- aes()

       mp$y <- if(input$Plot_type == 0 ){as.name(req(input$ContinuousVar))}
          else if (input$Plot_type == 1){as.name(req(input$ContinuousVar))}
          else if (input$Plot_type == 2){NULL}
          else if (input$Plot_type == 3){NULL}

        mp$x <- if(input$Plot_type == 0 ){""}
            else if (input$Plot_type == 1){""}
            else if (input$Plot_type == 2){as.name(req(input$ContinuousVar))}
            else if (input$Plot_type == 3){as.name(req(input$ContinuousVar))}

        mp$fill <- if (input$Split==T){colx<- input$bylist
                                        df <- df %>% dplyr::mutate(my_val=.[[(colx)]])
                                        df$my_val<-as.factor(df$my_val)
                                        as.name('my_val')
                                        }
               else if (input$Split==F) {NULL }


        #mp$group <- as.name(req(input$bylist))

        t <- if(input$Plot_type == 0){geom_boxplot()} else if (input$Plot_type == 1){geom_violin()}
              else if (input$Plot_type == 2){geom_histogram(bins = input$BinsNumber)} else if (input$Plot_type == 3){geom_density()}


        b<-as.formula(paste0("~",req(input$bylist)))
        f<- if (input$Split==T){facet_wrap(b) } else if (input$Split==F){NULL}
        col<- req(input$bylist)


       g <- ggplot(df, mp)+t+f+labs(fill=input$bylist)+theme(legend.background = element_rect(fill="lightblue",size=0.5, linetype="solid"))


      ggplotly(g
              )

    })

   #gestion des statistiques descriptives
  tabstats <- reactiveValues(stats=NULL, stats2=NULL, statcat=NULL)

    output$stat_table <- renderTable({

      df <-if (input$covariates_values_type==0)
      {tbl_df(filedata())}
      else if (input$covariates_values_type==1)
      {tbl_df(filedata()%>% dplyr::arrange(ID,TIME,AMT)  %>%  group_by(ID) %>% slice(1L))}

        s<- (req(input$ContinuousVar))

        #as.table(summary(as.name(req(input$ContinuousVar))))
        #stats <- df %>% as.table(summary("CLCR"))
        tabstats$stats <- df %>%
          select(one_of(s)) %>%
          summarise_all(
            funs(Mean=mean(.,na.rm=T), Median=median(.,na.rm=T),
                 Min=min(.,na.rm=T),Max=max(.,na.rm=T),SD=sd(.,na.rm=T),
                 N_obs=length, "Q2.5%" = quantile(., 0.025,na.rm=T),"Q5%" = quantile(., 0.05,na.rm=T),
                               "Q10%" = quantile(., 0.1,na.rm=T),"Q25%" = quantile(., 0.25,na.rm=T),
                               "Q75%" = quantile(., 0.75,na.rm=T),"Q90%" = quantile(., 0.9,na.rm=T),
                               "Q95%" = quantile(., 0.95,na.rm=T),"Q97.5%" = quantile(., 0.975,na.rm=T)
                 ))


    })

    output$stat_table2 <- renderTable({

      df <-if (input$covariates_values_type==0)
      {tbl_df(filedata())}
      else if (input$covariates_values_type==1)
      {tbl_df(filedata()%>% dplyr::arrange(ID,TIME,AMT)  %>%  group_by(ID) %>% slice(1L))}

      if (input$Split==T){
        s<- (req(input$ContinuousVar))
      s2<- (req(input$bylist))
      df_<- df %>% group_by_(.dots=s2)

      tabstats$stats2<-df_ %>% select(one_of(s))%>% summarise_all(funs(Mean=mean(.,na.rm=T), Median=median(.,na.rm=T),
                                                                                              Min=min(.,na.rm=T),Max=max(.,na.rm=T),SD=sd(.,na.rm=T),
                                                                                              N_obs=length,"Q2.5%" = quantile(., 0.025,na.rm=T),"Q5%" = quantile(., 0.05,na.rm=T),
                                                               "Q10%" = quantile(., 0.1,na.rm=T),"Q25%" = quantile(., 0.25,na.rm=T),
                                                               "Q75%" = quantile(., 0.75,na.rm=T),"Q90%" = quantile(., 0.9,na.rm=T),
                                                               "Q95%" = quantile(., 0.95,na.rm=T),"Q97.5%" = quantile(., 0.975,na.rm=T)))

      } else if (input$Split==F){NULL}
      #stats2<-stats2[,c(1,2,4,6,8,10,12)]


    })

    ### Téléchargement de la table de stats en sortie:
    output$download_stats <-  downloadHandler(
      filename = function() {
        paste("Stat-on-",input$datafile,".xlsx", sep="")
      },
      content = function(file) {
        sht<-paste0("by",input$bylist)
         xlsx::write.xlsx(tabstats$stats, file, sheetName="global",row.names=FALSE, col.names=TRUE,append=FALSE, showNA=TRUE)
         if (!is.null(tabstats$stats2)){xlsx::write.xlsx(as.data.frame(tabstats$stats2), file, sheetName=sht,row.names=FALSE, col.names=TRUE,append=TRUE, showNA=TRUE)
      }},contentType ="application/vnd.ms-excel"

    )

    # je cache le bouton quand rien à télécharger:
    observe({
      if (is.null(tabstats$stats)) {
        shinyjs::hide(id="download_stats", anim=FALSE, animType ="slide")
      }  else {
        shinyjs::show(id="download_stats", anim=FALSE, animType ="slide")
      }
    })
    #gestion du bouton split:

    observe({
      if (input$Split==F) {
        shinyjs::hide(id="Groupby", anim=FALSE, animType ="slide")
      }  else {
        shinyjs::show(id="Groupby", anim=FALSE, animType ="slide")
      }
    })

    #############################################################################################
    ##########          Dependent Variable                           #############################
    #############################################################################################

    #Filtre sur CMT
    output$Filter_CMT <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)

      CMTlev=levels(as.factor(df$CMT))

      selectInput("CMTlist","Filter on CMT:",choices = c("No filter",CMTlev) ,multiple=F, selected = "No filter")

    })

    #Filtre sur autres (DVID,YTYPE,...)
    output$Other_filter <- renderUI({

      selectInput("Other_filter", "Other filter",choices = c("No filter",input$mychooser$Right),multiple=F,selected = "No filter")

    })

    #Valeur du filtre
    output$filt_Value <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)

      Valuelev<-if (input$Other_filter=="No Filter"){"No variable selected"}
      else {
        unique(df[[input$Other_filter]])
            }

      selectInput("filt_Value","Value =",choices = Valuelev ,multiple=F)


    })



    #Filtre sur DOSE pour SPLIT graphique
    output$Split_dose <- renderUI({


      selectInput("SplitList","Split by dose:",choices = c("No split",input$Dose_related$Right) ,multiple=F, selected = "No split")

    })

    #ajout d'un autre split:
    output$Split_other <- renderUI({
      df <-tbl_df(filedata())
      if (is.null(df)) return(NULL)
      othersplit <- df %>% select(one_of(input$mychooser$Right),-one_of(input$Dose_related$Right)) %>% names()
      selectInput("Split_other","Other split:",choices = c("No split",othersplit) ,multiple=F, selected = "No split")

    })

    ###Gestion des sélecteurs à cacher/montrer:
    shinyjs::onclick("toggle_others_options",
                     shinyjs::toggle(id = "others_options", anim = TRUE ,animType ="fade"))
    shinyjs::hide(id="others_options",time=0.5)


    #Filtre sur DOSE pour FILTRE OPTION
    output$Filter_dose <- renderUI({


      selectInput("Dosefilter","Filter by dose:",choices = c("No Filter",input$Dose_related$Right) ,multiple=F, selected = "No split" )

    })

    #Liste des levels de DOSE pour FILTRE OPTION    à modifier ça ne marche pas
    output$Level_dose <- renderUI({
      df <-tbl_df(filedata())
      if (is.null(df)) return(NULL)


      Doselev<-if (input$Dosefilter=="No Filter"){"No variable selected"}
                                  else {

                                    unique(df[[input$Dosefilter]])

                                    }

      selectInput("DoseList","Dose =",choices = Doselev ,multiple=F)

    })

  #selection de la dependant variable  , si pas de DV
    #
    # output$DV_choice <- renderUI({
    #   selectInput("DV_choice", "Dependant Variable", choices=c("DV",input$mychooser$Left),multiple=F,selected = "DV")
    # })

   #Onglet concernant DV en foction du temps

      output$DV_graph <- renderPlotly({
   # output$DV_graph <- renderPlot({
      df <- tbl_df(filedata())

      # if (exists("DV",where=df)==F){df<- df %>% mutate(DV=CONC)} else {df}

      df<-df %>% dplyr::mutate(LOG=log10(DV))
      df <- df %>% dplyr::group_by(ID)



      # df<- dplyr::mutate(df,LOG=log10(DV))
      mdvp<-aes()
      mdvp$x<-as.name(req(input$TimeVar))
      mdvp$group<-as.name("ID")

      if (input$Ytype==0)
                          {mdvp$y<-as.name('DV')}
      else if (input$Ytype==1)
                          {df<-df %>% dplyr::mutate(LOG=log10(DV))
                            mdvp$y<-as.name('LOG')
                          }

      if (input$CMTlist=="No filter"){df}
                                else {df<- df %>% filter(CMT==input$CMTlist)}

      if (input$Other_filter=="No filter"){df}
                                else {
                                      ffv<-as.formula(paste0("~", input$Other_filter, "== '", input$filt_Value,"'"))
                                      df<- df %>% filter_(ffv)
                                      }
      if (input$DoseList=="No variable selected"){df}
                                else {

                                  ff <- as.formula(paste("~", input$Dosefilter, "==", input$DoseList))
                                  df<- df %>% filter_(ff)
                                  }


      if (input$SplitList=="No split" & input$Split_other=="No split"){Fac<-NULL}
       else if (input$SplitList!="No split" & input$Split_other=="No split"){S1<-as.formula(paste0("~",req(input$SplitList)))

                                              if (input$FreeScale==T){Fac<-facet_wrap(S1,scales="free")}
                                                                else {Fac<-facet_grid(S1)}
                                                                              }
      else if (input$SplitList=="No split" & input$Split_other!="No split"){S1<-as.formula(paste0("~",req(input$Split_other)))

                                              if (input$FreeScale==T){Fac<-facet_wrap(S1,scales="free")} #voir pourquoi le scales marche pas avec facet grid
                                              else {Fac<-facet_grid(S1)}
      }
      else {S1 <-as.formula(paste0("~",req(input$SplitList),"~",req(input$Split_other)))
                                              if (input$FreeScale==T){Fac<-facet_wrap(S1,scales="free")}
                                              else {Fac<-facet_grid(S1)}

      }

      if (input$Alldata == 1)
      {
        subj_id <- req(input$IDlist)
        df <- df %>% filter(ID %in% subj_id)
      }
      else if (input$Alldata == 0)
      {}

      if (input$Colors==F){mdvp2=NULL}
      else {mdvp2<-aes(color=ID)}



      df$ID<-as.factor(df$ID)
      dv_plot <- df  %>% ungroup() %>%
        ggplot(mdvp) + geom_point(mdvp2) + geom_line(mdvp2)+Fac



        req(ggplotly(dv_plot))
       # dv_plot
    })

    #Stats sur DV
    output$stat_dv <- renderTable({

      if (input$Alldata == 0)
                              {NULL}
      else if (input$Alldata == 1)
                              {

      df <-tbl_df(filedata())
      if (exists("DV",where=df)==F){df<- df %>% mutate(DV=CONC)} else {df}
      if (input$CMTlist=="No filter"){df}
      else {df<- df %>% filter(CMT==input$CMTlist)}

      if (input$Other_filter=="No filter"){df}
      else {
        ffv<-as.formula(paste0("~", input$Other_filter, "== '", input$filt_Value,"'"))
        df<- df %>% filter_(ffv)
      }

      if (input$Ytype==0)
      {statON<-as.name('DV')}
      else if (input$Ytype==1)
      {df<-df %>% dplyr::mutate(LOG=log10(DV))
      statON<-as.name('LOG')
      }


      df <-df %>% dplyr::group_by(ID)

      statsDV <- df %>%
        filter(ID %in% input$IDlist) %>%
        select_(statON) %>%
        summarise_all(
          funs(Mean=mean(.,na.rm=T), Median=median(.,na.rm=T),
               Min=min(.,na.rm=T),Max=max(.,na.rm=T),SD=sd(.,na.rm=T),
               N_obs=length))
                                }

    })



    #############################################################################################
    ##########     Covariables Catégoriques                         #############################
    #############################################################################################

    #selection de la variable à étudier :

    output$CategoricalVar <- renderUI({

      selectInput("CatVarList", "Variables list:",input$mychooser$Right,multiple=F)

    })

    #selection variable de split:
    output$SplitCatVar <- renderUI({

      selectInput("SplitCatVar", "Split 1 :",choices=c("No split",input$mychooser$Right),multiple=F, selected="No split")

    })

    #selection variable de split 2eme niveau:
    output$Split2CatVar <- renderUI({

      selectInput("Split2CatVar", "Split 2 :",choices=c("No split",input$mychooser$Right),multiple=F, selected="No split")

    })
    #Sortie graphique :

    output$CatPlot <-  renderPlotly({




      df <-if (input$covariates_values_type_cat==0)
      {tbl_df(filedata())}
      else if (input$covariates_values_type_cat==1)
      {tbl_df(filedata()%>% dplyr::arrange(ID,TIME,AMT)  %>%  group_by(ID) %>% slice(1L))}

        test<- if (paste0(unique(df[input$CatVarList]))=="NA"){NULL}
         else {input$CatVarList}
        col<-req(test)
      col<- input$CatVarList
       df <- df %>% dplyr::mutate(my_val=.[[(col)]])
       df$my_val<-as.factor(df$my_val)

       v<-aes()
       v$x<-as.name('my_val')
       v$fill<-as.name('my_val')


       f<- if (input$SplitCatVar=="No split" & input$Split2CatVar=="No split")
              {NULL}
          else if (input$SplitCatVar!="No split" & input$Split2CatVar=="No split")
              {facetting <-as.formula(paste0("~",req(input$SplitCatVar)))
                facet_grid(facetting,labeller = label_both) }
        else if (input$SplitCatVar!="No split" & input$Split2CatVar!="No split")
             {facetting <-as.formula(paste0("~",req(input$SplitCatVar),"~",req(input$Split2CatVar)))
               facet_grid(facetting,labeller = label_both) }

      cp <-df  %>% ggplot(v)+geom_bar(stat = "count")+xlab(col)+scale_fill_discrete(name = col)+f


       ggplotly(cp
       )

    })

    # Descriptive Statistics on Categorical variates

    output$stat_cat_table <- renderTable({

      df <-if (input$covariates_values_type_cat==0)
      {tbl_df(filedata())}
      else if (input$covariates_values_type_cat==1)
      {tbl_df(filedata()%>% dplyr::arrange(ID,TIME,AMT)  %>%  group_by(ID) %>% slice(1L))}

      Globrow<-nrow(df)
      col<- input$CatVarList

      df <- df %>% dplyr::mutate(Tested_Var_value=.[[(col)]])
      df$Tested_Var_value<-as.factor(df$Tested_Var_value)

      tabstats$statcat<- if (input$SplitCatVar=="No split" & input$Split2CatVar=="No split")
                                  {df %>% group_by(Tested_Var_value) %>%
                                          summarise(n=n()) %>%
                                          mutate(
                                          freq=paste0(round(100 * n/Globrow, 1), " %"))%>%
                                          setNames(c(input$CatVarList,"N","Freq"))}
                else if (input$SplitCatVar!="No split" & input$Split2CatVar=="No split")
                                  {col2<- input$SplitCatVar
                                   df<- df %>% dplyr::mutate(Split1_value=.[[(col2)]])
                                   df$Split1_value<-as.factor(df$Split1_value)
                                        df %>% group_by(Tested_Var_value,Split1_value) %>%
                                          summarise(n=n()) %>%
                                          mutate(rel.freq=paste0(round(100 * n/sum(n), 1), " %"),
                                          freq=paste0(round(100 * n/Globrow, 1), " %"))%>%
                                          setNames(c(input$CatVarList, input$SplitCatVar,"N", "Relative Freq", "Freq"))}
                else if (input$SplitCatVar!="No split" & input$Split2CatVar!="No split")
                                  {col2<- input$SplitCatVar
                                   col3<- input$Split2CatVar
                                  df<- df %>% dplyr::mutate(Split1_value=.[[(col2)]],Split2_value=.[[(col3)]])
                                  df$Split1_value<-as.factor(df$Split1_value)
                                  df$Split2_value<-as.factor(df$Split2_value)
                                    df %>% group_by(Tested_Var_value,Split1_value,Split2_value) %>%
                                          summarise(n=n()) %>%
                                          mutate(rel.freq=paste0(round(100 * n/sum(n), 1), " %"),
                                          freq=paste0(round(100 * n/Globrow, 1), " %"))} %>%
                                          setNames(c(input$CatVarList, input$SplitCatVar, input$Split2CatVar,"N", "Relative Freq", "Freq"))

    })

    ### Téléchargement de la table de stats en sortie:
    output$download_stats_cat <-  downloadHandler(
      filename = function() {
        paste("Stat-on-CATvars",input$datafile,".xlsx", sep="")
      },
      content = function(file) {

        xlsx::write.xlsx(as.data.frame(tabstats$statcat), file, sheetName="freq",row.names=FALSE, col.names=TRUE,append=FALSE, showNA=TRUE)
        },contentType ="application/vnd.ms-excel"

    )

    #############################################################################################
    ##########                Manage Covariable                    #############################
    #############################################################################################
    output$ConTab <- renderTable({

      df <-tbl_df(filedata())
      test <- df  %>% summarise_each(funs(n_distinct))
      Continuous=c(names(as.data.frame(test)[,test>10]),NULL)


      ConTab<- as.data.frame(Continuous)
      })

    output$CatTab <- renderTable({

      df <-tbl_df(filedata())
      test <- df  %>% summarise_each(funs(n_distinct))
      Categorical=c(names(as.data.frame(test)[,test<=10]),NULL)


      CatTab<- as.data.frame(Categorical)
    })

    output$choices <- renderUI({
      if (is.null(data())){
        return(NULL)
      }else{
        df <-tbl_df(filedata())
        test <- df %>% select(-one_of("TAD","TOD","TSLD","NTIME","TIML","TIM1","ID","L2","DEL","DEL1","DEL2","DEL3","DV","TIME","AMT","RATE","SS","II","ADDL","CMT","PCMT","CALL","CONT","DATE","DAT1","DAT2","DAT3","L1","PRED","RES","WRES"))%>%
                      summarise_each(funs(n_distinct))
        chooserInput("mychooser", "Continuous", "Categorical",
                     names(as.data.frame(test)[,test>10]), names(as.data.frame(test)[,test<=10]), size = 20, multiple = TRUE
        )

      }
    })

    output$Time_related <- renderUI({
      if (is.null(data())){
        return(NULL)
      }else{
        df <-tbl_df(filedata())
        time_rel <- df %>% select(one_of("TIME","TAD","TOD","TSLD","NTIME","TIML","TIM1")) %>% names()
        No_time_rel <- df %>% select(-one_of("BA","GFR","ALB","ALP","AST","DOSE","FDOSE", "NDOSE","RACE","RAC1","RAC2","RAC3","RAC4","ASIAN","CLCR","BMI","WT","HT","AGE","SEX","TIME","TAD","TOD","TSLD","NTIME","TIML","TIM1","ID","L2","DEL","DEL1","DEL2","DEL3","DV","MDV","EVID","AMT","RATE","CMT","PCMT","CALL","CONT","L1","PRED","RES","WRES")) %>% names()

        chooserInput("Time_related", "NoTRV", "TRV",
                     No_time_rel, time_rel, size = 20, multiple = TRUE
        )

      }
    })


    output$Dose_related <- renderUI({
      if (is.null(data())){
        return(NULL)
      }else{
        df <-tbl_df(filedata())
        VarDose <- df %>% select(one_of("DGRP","DADM"),contains("DOSE")) %>% names()
        NoVarDose <- df %>% select(-contains("DOSE"),-one_of("BA","GFR","ALB","ALP","AST","RACE","RAC1","RAC2","RAC3","RAC4","ASIAN","CLCR","BMI","WT","HT","AGE","SEX","TAD","TOD","TSLD","NTIME","TIML","TIM1","ID","L2","DEL","DEL1","DEL2","DEL3","DV","MDV","TIME","EVID","PRED","RES","WRES")) %>% names()

        chooserInput("Dose_related", "NoDRV", "DRV",
                     NoVarDose, VarDose, size = 20, multiple = TRUE
        )

      }
    })

    output$modalite_tab <- DT::renderDataTable({
      if (is.null(data())){
        return(NULL)
      }else{
      df <-tbl_df(filedata())
      mod <- df  %>% summarise_all(funs(n_distinct)) %>% gather() %>% rename(Var_Name=key, modalities_number=value)
      mod
    }}
    ,
                        options = list(pageLength = 10, dom = 'ltip', paging=TRUE, scrollX = TRUE,  filter='top',
                                       initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color': 'LightSkyBlue', 'color': 'black'});",
                                         "$(this.api().table().body()).css({'color': 'black'});",
                                         "}")
                        ))

    output$COT_varlist <- renderPrint({
      if (is.null(data())){
        return(NULL)
      }else{
        df <-tbl_df(filedata())
      countmod <- df %>% dplyr::group_by(ID) %>% summarise_all(funs(n_distinct)) %>% select(-one_of("ID","DV","MDV","AMT","DEL","DEL1","DEL2","DEL3","EVID","CMT")) %>%
        map(~sum(.)/nlevels(as.factor(df$ID))) %>%
        keep(function(x) x!=1) %>%  names()
      countmod
    }})

    count_mbi<- eventReactive(input$COT_details, { if (is.null(data())){
      return(NULL)
    }else{
      df <-tbl_df(filedata())
      countmodbyID <- df %>% dplyr::group_by(ID) %>% summarise_all(funs(n_distinct)) %>% select(-one_of("DV","MDV","AMT","DEL","DEL1","DEL2","DEL3","EVID","CMT")) %>%
        map(~sum(.)/nlevels(as.factor(df$ID))) %>%
        keep(function(x) x!=1)
      countmodbyID2 <- df %>% dplyr::group_by(ID) %>% summarise_all(funs(n_distinct)) %>% select(names(countmodbyID ))
      countmodbyID2
      }
    })

    output$count_mbi <- DT::renderDataTable(count_mbi(),

      options = list(pageLength = 8, dom = 'ltip', paging=TRUE, scrollX = TRUE,  filter='top',
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#56739A', 'color': 'black'});",
                       "$(this.api().table().body()).css({'color': 'black'});",
                       "}")
      ))


    # option permettant de charger les valeurs sans à avoir à ouvrir l'onglet: (de base c'est onglet dépendant)
    outputOptions(output, "choices", suspendWhenHidden = FALSE)
    outputOptions(output, "Time_related", suspendWhenHidden = FALSE)
    outputOptions(output, "Dose_related", suspendWhenHidden = FALSE)
    outputOptions(output, "Other_filter", suspendWhenHidden = FALSE)
    outputOptions(output, "Split_other", suspendWhenHidden = FALSE)
    outputOptions(output, "Groupby", suspendWhenHidden = FALSE)

    ####### PARTIE ENRICHISSEMENT DU DATASET  #########


    ##### valeurs réactives: le compteur et la table
    values<-reactiveValues(n_row=1,
                           Enrich_Table=data_frame(START = 0,END = 1, STEP=0.5, CMT=1),
                           rich_df=NULL)





    ### Ajouter des lignes
    observeEvent(input$Addrow, {
      values$n_row=values$n_row+1

      values$Enrich_Table <- isolate ({


        Enrich_Table = hot_to_r(input$hot)
        Enrich_Table <- Enrich_Table %>% add_row( START = 0,END = 1, STEP=0.5, CMT=1)
      })

    })
    ### Supprimer des lignes
    observeEvent(input$Delrow, {
      values$n_row=values$n_row-1

      values$Enrich_Table <- isolate ({


        Enrich_Table = hot_to_r(input$hot)
        Enrich_Table <- Enrich_Table[-(nrow(Enrich_Table)),]
      })
    })


    #### ma table
    output$hot <- renderRHandsontable({

         rhandsontable(values$Enrich_Table, useTypes = T, selectCallback = TRUE, contextMenu=FALSE)
    })

    output$table <- renderTable({values$Enrich_Table})

    ### Mise à jour de la table en appuyant sur run
    observeEvent(input$Run, {
      values$Enrich_Table <- isolate ({Enrich_Table = hot_to_r(input$hot)})

    })

    ### Selection des variables à remplir:

    output$fillcol<-renderUI({

      if (input$ColOption==T){
       df <-filedata()

       df <- as_tibble(req(df))
       tofilllist <- df %>% names()%>% setdiff(c("AMT", "DV", "RATE", "SS", "II", "ADDL"))

      checkboxGroupInput("fillcol", label=NULL,choices=tofilllist,selected=tofilllist,inline = T)
      } else {NULL}

    })

    ### définition du dataset enrichi
    observeEvent(input$Run==T,{
      df <-tbl_df(filedata())
      pr           <-  isolate({values$Enrich_Table}) %>% rowwise() %>% do(res=list(liste = list(times= seq(.$START,.$END,by= .$STEP), cmt = .$CMT))) %>% as.list()%>% unlist(.,recursive=FALSE) %>% unlist(.,recursive=FALSE)
      isolate({
        fill_columns <-if (input$ColOption==T){input$fillcol} else {df %>% names()%>% setdiff(c("AMT", "DV", "RATE", "SS", "II", "ADDL"))}
      })
      withProgress({
        values$rich_df<- df %>% enrich_dataset(periods=pr , columns_to_fill = fill_columns)
        setProgress(value = 1, message = "Done !")
      }, value = 0.5, message = "Creating new dataset...")


    })

    ### Enrichissement du dataset:

    output$richdata <- DT::renderDataTable({



        values$rich_df

      },
      options = list(pageLength = 20,lengthMenu=c(20,40,100), dom = 'lftip', paging=TRUE, scrollX = TRUE,  filter='top',
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#4682B4', 'color': '#fff'});",
                       "}")
      ))

    ### Téléchargement de la table en sortie:
    output$download1 <-  downloadHandler(
      filename = function() {
        paste("Enriched",input$datafile, sep="")
      },
      content = function(file) {

        write_csv(req(values$rich_df), file, na=".",
                  col_names=TRUE,append=FALSE)
                    }

      )
    ###gestion des boutons à cacher

    observe({
      if (values$n_row<=1) {
        shinyjs::disable("Delrow")
      }  else {
         shinyjs::enable("Delrow")
       }
    })
    observe({
      if (input$Run==0) {
        shinyjs::hide(id="download1", anim=FALSE, animType ="slide")
      }  else {
        shinyjs::show(id="download1", anim=FALSE, animType ="slide")
      }
    })





})
