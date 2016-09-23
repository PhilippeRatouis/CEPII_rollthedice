#Place 1 Run once
source("SourceCode.R")
library(Rgraphviz)
library(igraph)
library(expm)
library(Matrix)

shinyServer(
  function(input,output){
    #Place 2 Run once for every visitor of the app
    GROWTH <- sfc.model("GROWTH.txt",modelName="GROWTH_MODEL")
    dataGROWTH <- simulate(GROWTH)
    
    output$equations <- renderTable({
      temp<-as.data.frame(GROWTH$equations[,c(1,2)])
      colnames(temp)<-c("Variable","Equation")
      temp
      #matrix(c(getwd(),list.dirs(getwd())),ncol=1)
    })
    
    output$dag <- renderPlot({
      plot_graph_hierarchy(graph=generate.DAG.collaspe( adjacency = t(GROWTH$matrix) )$orginal_graph,main="GROWTH")
    })
    
    output$plot <- renderPlot({
      run=FALSE
      if(input$npl!=as.numeric(GROWTH$variables[19,2])){
        GROWTH<-sfc.editVar(GROWTH,var="npl",init=input$npl)
        run=TRUE
      }
      if(input$psiD!=as.numeric(GROWTH$variables[18,2])){
        GROWTH<-sfc.editVar(GROWTH,var="psiD",init=input$psiD)
        run=TRUE
      }
      if(input$gr0!=as.numeric(GROWTH$variables[5,2])){
        GROWTH<-sfc.editVar(GROWTH,var="gr0",init=input$gr0)
        run=TRUE
      }
      if(input$Nfe!=as.numeric(GROWTH$variables[62,2])){
        GROWTH<-sfc.editVar(GROWTH,var="Nfe",init=input$Nfe)
        run=TRUE
      }
      if(input$gr_pr!=as.numeric(GROWTH$variables[3,2])){
        GROWTH<-sfc.editVar(GROWTH,var="gr_pr",init=input$gr_pr)
        run=TRUE
      }
      if(input$gr_g!=as.numeric(GROWTH$variables[51,2])){
        GROWTH<-sfc.editVar(GROWTH,var="gr_g",init=input$gr_g)
        run=TRUE
      }
      if(run)
        dataGROWTH<-simulate(GROWTH)
      #PLace 3, runs every time somtheing changes in the widgets
      myData<-as.data.frame(dataGROWTH$baseline)
      varnames<-input$checkGroup
      matplot(myData[c(varnames)],type="l",xlab="",ylab="",lwd=2,lty=1)
      legend("topleft",col=1:length(varnames),bty='n',lwd=2,lty=1,legend=varnames)
    })
    
    output$plotscen <- renderPlot({
      run=FALSE
      if(input$npl!=as.numeric(GROWTH$variables[19,2])){
        GROWTH<-sfc.editVar(GROWTH,var="npl",init=input$npl)
        run=TRUE
      }
      if(input$psiD!=as.numeric(GROWTH$variables[18,2])){
        GROWTH<-sfc.editVar(GROWTH,var="psiD",init=input$psiD)
        run=TRUE
      }
      if(input$gr0!=as.numeric(GROWTH$variables[5,2])){
        GROWTH<-sfc.editVar(GROWTH,var="gr0",init=input$gr0)
        run=TRUE
      }
      if(input$Nfe!=as.numeric(GROWTH$variables[62,2])){
        GROWTH<-sfc.editVar(GROWTH,var="Nfe",init=input$Nfe)
        run=TRUE
      }
      if(input$gr_pr!=as.numeric(GROWTH$variables[3,2])){
        GROWTH<-sfc.editVar(GROWTH,var="gr_pr",init=input$gr_pr)
        run=TRUE
      }
      if(input$gr_g!=as.numeric(GROWTH$variables[51,2])){
        GROWTH<-sfc.editVar(GROWTH,var="gr_g",init=input$gr_g)
        run=TRUE
      }
      vars<-c()
      values<-c()
      if(input$psiD_scen!=input$psiD){
        vars<-c(vars,"psiD")
        values<-c(values,input$psiD_scen)
      }
      if(input$npl_scen!=input$npl){
        vars<-c(vars,"npl")
        values<-c(values,input$npl_scen)
      }
      if(input$gr0_scen!=input$gr0){
        vars<-c(vars,"gr0")
        values<-c(values,input$gr0_scen)
      }
      if(input$Nfe_scen!=input$Nfe){
        vars<-c(vars,"Nfe")
        values<-c(values,input$Nfe_scen)
      }
      if(input$gr_pr_scen!=input$gr_pr){
        vars<-c(vars,"gr_pr")
        values<-c(values,input$gr_pr_scen)
      }
      if(input$gr_g_scen!=input$gr_g){
        vars<-c(vars,"gr_g")
        values<-c(values,input$gr_g_scen)
      }
      if(length(vars)>0){
        GROWTH<-sfc.addScenario(model=GROWTH,vars=list(vars),values=list(values),inits=input$init,ends=input$end)
        dataGROWTH<-simulate(GROWTH)
        scen<-as.data.frame(dataGROWTH$scenario_1)
        myData<-as.data.frame(dataGROWTH$baseline)
      }else{
        if(run)
          dataGROWTH<-simulate(GROWTH)
        scen<-as.data.frame(dataGROWTH$baseline)
        myData<-as.data.frame(dataGROWTH$baseline)
      }
      #PLace 3, runs every time somtheing changes in the widgets
      varnames<-input$checkGroup_scen
      matplot(rownames(scen),scen[c(varnames)]/myData[c(varnames)],type="l",xlab="",ylab="",lwd=2,lty=1)
      legend("topleft",col=1:length(varnames),bty='n',lwd=2,lty=1,legend=varnames)
    })
  }
)