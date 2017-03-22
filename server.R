library(shiny)

shinyServer(function(input, output) {

  #### set up inital table for viewing ####
    output$contents <-renderTable({  
      if(input$example.data == TRUE){
        dat = read.csv("PCB-01-TRB-050.txt",sep="",header=F)
        colnames(dat)<-c("B","M")
      }
      else({
       inFile <-input$file1
      if(is.null(inFile))
        return(NULL)
      dat = read.csv(inFile$datapath,header=input$header,sep=input$sep,quote=input$quote)
      if(input$header == FALSE){
      colnames(dat)<-c("B","M")
      }
      })
      dat
      },digits=c(2,2,7))
######### plots on first page #############
  output$plot1 <- renderPlot({
    if(input$example.data == TRUE){
      dat = read.csv("PCB-01-TRB-050.txt",sep="",header=F)
      colnames(dat)<-c("B","M")
    }
    else({
      inFile <-input$file1
      if(is.null(inFile))
        return(NULL)
      dat = read.csv(inFile$datapath,header=input$header,sep=input$sep,quote=input$quote)
      if(input$header == FALSE){
        colnames(dat)<-c("Field","Magnetization")
      }
    })
    inScale <- input$scale
    sf <- input$smooth.factor
    dat[,3] <- log(dat[,1],10)
    dat[,4] <- smooth.spline(log(dat[,1],10),dat[,2],spar=sf)$y
    dat[,5] <- abs(predict(smooth.spline(log(dat[,1],10),dat[,2],spar=sf),deriv=1)$y)
    dat[,6] <- smooth.spline(dat[,1],dat[,2],spar=sf)$y
    dat[,7] <- abs(predict(smooth.spline(dat[,1],dat[,2],spar=sf),deriv=1)$y)
    dat[,8] <- abs(predict(smooth.spline(log(dat[,1],10),dat[,2],spar=0),deriv=1)$y)
    dat[,9] <- abs(predict(smooth.spline(dat[,1],dat[,2],spar=0),deriv=1)$y)
    
    
    if(input$scale == "log"){
      if(input$plot.type == "magnetization data"){
      plot(dat[,3],dat[,2] ,xlim=c(min(dat[,3]),max(dat[,3])),ylim=c(0,max(dat[,2])),
           xlab = paste("log B (",input$B.units,")"), ylab = paste("M (",input$M.units,")"),col="dark grey",pch=19)
      lines(dat[,3],dat[,4],col="black",type="l",lwd=2)
      legend(2.8,-1,c("data","spline"),pch=c(20,NA),lty=c(NA,1),xpd=TRUE)
      } 
      else({
      plot(dat[,3],dat[,8],col="dark grey",type="p",pch=19,
           xlim=c(min(dat[,3]),max(dat[,3])),ylim=c(0,max(dat[,8])),
           xlab = paste("log B (",input$B.units,")"), ylab = "dM/dlog(B)")
      points(dat[,3],dat[,5],col="black",type="l",lwd=2)
      legend(2.8,-1,c("data","spline"),pch=c(20,NA),lty=c(NA,1),xpd=TRUE)
      })
    }
    else({
      if(input$plot.type == "magnetization data"){
      plot(dat[,1],dat[,2] ,xlim=c(min(dat[,1]),max(dat[,1])),ylim=c(0,max(dat[,2])),col="dark grey",pch=19,
           xlab = paste("B (",input$B.units,")"), ylab = paste("M (",input$M.units,")"))
      lines(dat[,1],dat[,4],col="black",type="l",lwd=2)
      legend(2.8,-1,c("data","spline"),pch=c(20,NA),lty=c(NA,1),xpd=TRUE)
      }
      else({
      plot(dat[,1],dat[,9],col="dark grey",type="p",pch=19,
           xlim=c(min(dat[,1]),max(dat[,1])),ylim=c(0,max(dat[,9])),
           xlab = paste("B (",input$B.units,")"), ylab = "dM/dB")
      points(dat[,1],dat[,7],col="black",type="l",lwd=2)
      legend(2.8,-1,c("data","spline"),pch=c(20,NA),lty=c(NA,1),xpd=TRUE)
      })
    })
    })
########################################################
########## selecting components ########################
  output$ui.comp1 <- renderUI({
    conditionalPanel(
      condition = "input.comp1 == true",
      h4("Component 1"),
      if(input$scale == "log"){
      sliderInput("B1","Mean Coercivity:",min = 0, max = input$max.scale.log, value = 0, step = 0.01)
      }
      else({
      sliderInput("B1","Mean Coercivity:", min = 0, max = input$max.scale, value = 0, step = 0.01)
      }),
      if(input$scale=="log"){
      sliderInput("DP1","Dispersion:",min = 0, max = 2.0, value = 0, step = 0.01) 
      }
      else({
      sliderInput("DP1","Dispersion:", min = 0, max = 10^(2.0), value = 0, step = 0.01)
      }),
      sliderInput("P1","Relative Proportion:", min = 0, max = 1.0, value = 0, step = 0.01),
      sliderInput("S1","Skewness:",min=0, max = 2, value = 1, step = 0.05)
    )
  })
  output$ui.comp2 <- renderUI({
    conditionalPanel(
      condition = "input.comp2 == true",
      h4("Component 2"),
      if(input$scale == "log"){
        sliderInput("B2","Mean Coercivity:",min = 0, max = input$max.scale.log, value = 0, step = 0.01)
      }
      else({
        sliderInput("B2","Mean Coercivity:", min = 0, max = input$max.scale, value = 0, step = 0.01)
      }),
      if(input$scale=="log"){
        sliderInput("DP2","Dispersion:",min = 0, max = 2.0, value = 0, step = 0.01) 
      }
      else({
        sliderInput("DP2","Dispersion:", min = 0, max = 10^(2.0), value = 0, step = 0.01)
      }),
      sliderInput("P2","Relative Proportion:", min = 0, max = 1.0, value = 0, step = 0.01),
      sliderInput("S2","Skewness:",min=0, max = 2, value = 1, step = 0.05)
    )
  })

  output$ui.comp3 <- renderUI({
    conditionalPanel(
      condition = "input.comp3 == true",
      h4("Component 3"),
      if(input$scale == "log"){
        sliderInput("B3","Mean Coercivity:",min = 0, max = input$max.scale.log, value = 0, step = 0.01)
      }
      else({
        sliderInput("B3","Mean Coercivity:", min = 0, max = input$max.scale, value = 0, step = 0.01)
      }),
      if(input$scale=="log"){
        sliderInput("DP3","Dispersion:",min = 0, max = 2.0, value = 0, step = 0.01) 
      }
      else({
        sliderInput("DP3","Dispersion:", min = 0, max = 10^(2.0), value = 0, step = 0.01)
      }),
      sliderInput("P3","Relative Proportion:", min = 0, max = 1.0, value = 0, step = 0.01),
      sliderInput("S3","Skewness:",min=0, max = 2, value = 1, step = 0.05)
    )
  })
output$ui.comp4 <- renderUI({
  conditionalPanel(
    condition = "input.comp4 == true",
    h4("Component 4"),
    if(input$scale == "log"){
      sliderInput("B4","Mean Coercivity:",min = 0, max = input$max.scale.log, value = 0, step = 0.01)
    }
    else({
      sliderInput("B4","Mean Coercivity:", min = 0, max = input$max.scale, value = 0, step = 0.01)
    }),
    if(input$scale=="log"){
      sliderInput("DP4","Dispersion:",min = 0, max = 2.0, value = 0, step = 0.01) 
    }
    else({
      sliderInput("DP4","Dispersion:", min = 0, max = 10^(2.0), value = 0, step = 0.01)
    }),
    sliderInput("P4","Relative Proportion:", min = 0, max = 1.0, value = 0, step = 0.01),
    sliderInput("S4","Skewness:",min=0, max = 2, value = 1, step = 0.05)
  )
})
output$ui.comp5 <- renderUI({
  conditionalPanel(
    condition = "input.comp5 == true",
    h4("Component 5"),
    if(input$scale == "log"){
      sliderInput("B5","Mean Coercivity:",min = 0, max = input$max.scale.log, value = 0, step = 0.01)
    }
    else({
      sliderInput("B5","Mean Coercivity:", min = 0, max = input$max.scale, value = 0, step = 0.01)
    }),
    if(input$scale=="log"){
      sliderInput("DP5","Dispersion:",min = 0, max = 2.0, value = 0, step = 0.01) 
    }
    else({
      sliderInput("DP5","Dispersion:", min = 0, max = 10^(2.0), value = 0, step = 0.01)
    }),
    sliderInput("P5","Relative Proportion:", min = 0, max = 1.0, value = 0, step = 0.01),
    sliderInput("S5","Skewness:",min=0, max = 2, value = 1, step = 0.05)
  )
})
output$ui.comp6 <- renderUI({
  conditionalPanel(
    condition = "input.comp6 == true",
    h4("Component 6"),
    if(input$scale == "log"){
      sliderInput("B6","Mean Coercivity:",min = 0, max = input$max.scale.log, value = 0, step = 0.01)
    }
    else({
      sliderInput("B6","Mean Coercivity:", min = 0, max = input$max.scale, value = 0, step = 0.01)
    }),
    if(input$scale=="log"){
      sliderInput("DP6","Dispersion:",min = 0, max = 2.0, value = 0, step = 0.01) 
    }
    else({
      sliderInput("DP6","Dispersion:", min = 0, max = 10^(2.0), value = 0, step = 0.01)
    }),
    sliderInput("P6","Relative Proportion:", min = 0, max = 1.0, value = 0, step = 0.01),
    sliderInput("S6","Skewness:",min=0, max = 2, value = 1, step = 0.05)
  )
})

#############################################################################
    output$plot2 <-renderPlot({
      if(input$example.data == TRUE){
        dat = read.csv("PCB-01-TRB-050.txt",sep="",header=F)
        colnames(dat)<-c("B","M")
      }
      else({
        inFile <-input$file1
        if(is.null(inFile))
          return(NULL)
        dat = read.csv(inFile$datapath,header=input$header,sep=input$sep,quote=input$quote)
        if(input$header == FALSE){
          colnames(dat)<-c("Field","Magnetization")
        }
      })
      inScale <- input$scale
      sf <- input$smooth.factor
      dat[,3] <- log(dat[,1],10)
      dat[,4] <- smooth.spline(log(dat[,1],10),dat[,2],spar=sf)$y
      dat[,5] <- abs(predict(smooth.spline(log(dat[,1],10),dat[,2],spar=sf),deriv=1)$y)
      dat[,6] <- smooth.spline(dat[,1],dat[,2],spar=sf)$y
      dat[,7] <- abs(predict(smooth.spline(dat[,1],dat[,2],spar=sf),deriv=1)$y)
      dat[,8] <- abs(predict(smooth.spline(log(dat[,1],10),dat[,2],spar=0),deriv=1)$y)
      dat[,9] <- abs(predict(smooth.spline(dat[,1],dat[,2],spar=0),deriv=1)$y)
    
      if(is.numeric(input$plot_click$x)== "TRUE"){
        if(input$plot_click$x > max(dat[,3])/2){
          dat = subset(dat,dat[,3] < input$plot_click$x)
        }
        else(dat = subset(dat,dat[,3] > input$plot_click$x))
      }
      
      if(input$scale == "log"){
        M.sm = dat[,4]
        G = dat[,8]
        #G.sm = abs(predict(smooth.spline(log(dat[,1],10),dat[,2],spar=sf),seq(min(dat[,3]),max(dat[,3]),0.01),deriv=1)$y)
        B = dat[,3]
        B.d = seq(min(dat[,3]),max(dat[,3]),0.01)
        G.sm = approx(x=dat[,3], y=dat[,5], xout = B.d, method = "linear")$y
      }
      else({
        M.sm = dat[,6]
        G = dat[,9]
        #G.sm = abs(predict(smooth.spline(dat[,1],dat[,2],spar=sf),seq(min(dat[,1]),max(dat[,1]),1.0),deriv=1)$y)
        B = dat[,1]
        B.d = seq(min(dat[,1]),max(dat[,1]),1.0)
        G.sm = approx(x=dat[,1], y=dat[,7], xout = B.d, method = "linear")$y
      })

##################### components for fitting ####################################
if(input$comp1 == TRUE){
  B1 = input$B1
  DP1 = input$DP1
  P1 = input$P1
  S1 = input$S1
}
else({
  B1 = NULL 
  DP1 = NULL 
  P1 = NULL  
  S1 = NULL
})
if(input$comp2 == TRUE){
  B2 = input$B2
  DP2 = input$DP2
  P2 = input$P2
  S2 = input$S2
}
else({
  B2 = NULL 
  DP2 = NULL 
  P2 = NULL 
  S2 = NULL
})
if(input$comp3 == TRUE){
  B3 = input$B3
  DP3 = input$DP3
  P3 = input$P3
  S3 = input$S3
}
else({
  B3 = NULL 
  DP3 = NULL 
  P3 = NULL 
  S3 = NULL
})
if(input$comp4 == TRUE){
  B4 = input$B4
  DP4 = input$DP4
  P4 = input$P4
  S4 = input$S4
}
else({
  B4 = NULL 
  DP4 = NULL 
  P4 = NULL 
  S4 = NULL
})
if(input$comp5 == TRUE){
  B5 = input$B5
  DP5 = input$DP5
  P5 = input$P5
  S5 = input$S5
}
else({
  B5 = NULL 
  DP5 = NULL 
  P5 = NULL 
  S5 = NULL
})
if(input$comp6 == TRUE){
  B6 = input$B6
  DP6 = input$DP6
  P6 = input$P6
  S6 = input$S6
}
else({
  B6 = NULL 
  DP6 = NULL 
  P6 = NULL 
  S6 = NULL
})


     Pi = c(B1,DP1,P1,
            B2,DP2,P2,
            B3,DP3,P3,
            B4,DP4,P4,
            B5,DP5,P5,
            B6,DP6,P6)
     n.comp = length(Pi)/3
     S = c(S1,S2,S3,S4,S5,S6)
     C = matrix(nrow=length(B.d),ncol=n.comp)   
     library('fGarch')
     if(n.comp == 1){
       C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],S[1])/max(dsnorm(B.d,Pi[1],Pi[2],S[1]))))
       G.model = C[,1] 
     }
     if(n.comp == 2){
       C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],S[1])/max(dsnorm(B.d,Pi[1],Pi[2],S[1]))))
       C[,2] = Pi[6]*(max(G.sm)*(dsnorm(B.d,Pi[4],Pi[5],S[2])/max(dsnorm(B.d,Pi[4],Pi[5],S[2]))))
       G.model = C[,1]+C[,2] 
     }
      if(n.comp == 3){
        C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],S[1])/max(dsnorm(B.d,Pi[1],Pi[2],S[1]))))
        C[,2] = Pi[6]*(max(G.sm)*(dsnorm(B.d,Pi[4],Pi[5],S[2])/max(dsnorm(B.d,Pi[4],Pi[5],S[2]))))
        C[,3] = Pi[9]*(max(G.sm)*(dsnorm(B.d,Pi[7],Pi[8],S[3])/max(dsnorm(B.d,Pi[7],Pi[8],S[3]))))
        G.model = C[,1]+C[,2]+C[,3] 
      }
      if(n.comp == 4){
        C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],S[1])/max(dsnorm(B.d,Pi[1],Pi[2],S[1]))))
        C[,2] = Pi[6]*(max(G.sm)*(dsnorm(B.d,Pi[4],Pi[5],S[2])/max(dsnorm(B.d,Pi[4],Pi[5],S[2]))))
        C[,3] = Pi[9]*(max(G.sm)*(dsnorm(B.d,Pi[7],Pi[8],S[3])/max(dsnorm(B.d,Pi[7],Pi[8],S[3]))))
        C[,4] = Pi[12]*(max(G.sm)*(dsnorm(B.d,Pi[10],Pi[11],S[4])/max(dsnorm(B.d,Pi[10],Pi[11],S[4]))))
        G.model = C[,1]+C[,2]+C[,3]+C[,4] 
      }
     if(n.comp == 5){
        C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],S[1])/max(dsnorm(B.d,Pi[1],Pi[2],S[1]))))
        C[,2] = Pi[6]*(max(G.sm)*(dsnorm(B.d,Pi[4],Pi[5],S[2])/max(dsnorm(B.d,Pi[4],Pi[5],S[2]))))
        C[,3] = Pi[9]*(max(G.sm)*(dsnorm(B.d,Pi[7],Pi[8],S[3])/max(dsnorm(B.d,Pi[7],Pi[8],S[3]))))
        C[,4] = Pi[12]*(max(G.sm)*(dsnorm(B.d,Pi[10],Pi[11],S[4])/max(dsnorm(B.d,Pi[10],Pi[11],S[4]))))
        C[,5] = Pi[15]*(max(G.sm)*(dsnorm(B.d,Pi[13],Pi[14],S[5])/max(dsnorm(B.d,Pi[13],Pi[14],S[5]))))
        G.model = C[,1]+C[,2]+C[,3]+C[,4]+C[,5] 
      }
      if(n.comp == 6){
        C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],S[1])/max(dsnorm(B.d,Pi[1],Pi[2],S[1]))))
        C[,2] = Pi[6]*(max(G.sm)*(dsnorm(B.d,Pi[4],Pi[5],S[2])/max(dsnorm(B.d,Pi[4],Pi[5],S[2]))))
        C[,3] = Pi[9]*(max(G.sm)*(dsnorm(B.d,Pi[7],Pi[8],S[3])/max(dsnorm(B.d,Pi[7],Pi[8],S[3]))))
        C[,4] = Pi[12]*(max(G.sm)*(dsnorm(B.d,Pi[10],Pi[11],S[4])/max(dsnorm(B.d,Pi[10],Pi[11],S[4]))))
        C[,5] = Pi[15]*(max(G.sm)*(dsnorm(B.d,Pi[13],Pi[14],S[5])/max(dsnorm(B.d,Pi[13],Pi[14],S[5]))))
        C[,6] = Pi[18]*(max(G.sm)*(dsnorm(B.d,Pi[16],Pi[17],S[6])/max(dsnorm(B.d,Pi[16],Pi[17],S[6]))))
        G.model = C[,1]+C[,2]+C[,3]+C[,4]+C[,5]+C[,6] 
      }
   #### plot 2 #####
  plot(B,G,col="dark grey",type="p",pch=19,
   ylim=c(0,max(G)),
     xlab = 
       if(input$scale == "log"){paste("log B (",input$B.units,")")} 
     else({paste("B (",input$B.units,")")}), 
     ylab = if(input$scale == "log"){
       c("dM/dlog(B)")
     }
     else({c("dM/dB")}))
  points(B.d,G.sm,col="black",type="l",lwd=3)
    colors = c("blue","purple","green","dark red","dark green","magenta")
    for(i in 1:ncol(C)){
       points(B.d,C[,i],col=colors[i],type="l",lwd=1)
     }
   points(B.d,G.model,col="orange",type="l",lwd=5)

output$RSS.plot2 <- renderPrint({
  RSS.plot2 <- sum(((G.model-G.sm)^2))
  RSS.plot2
})
})
output$common_components <- renderTable({
  x = read.csv("component.summary.csv",header=TRUE)
  colnames(x)<-c("Component","Mean Coercivity","Dispersion")
  x
})
#######################################################################################
    output$plot3<- renderPlot({
      if(input$example.data == TRUE){
        dat = read.csv("PCB-01-TRB-050.txt",sep="",header=F)
        colnames(dat)<-c("B","M")
      }
      else({
        inFile <-input$file1
        if(is.null(inFile))
          return(NULL)
        dat = read.csv(inFile$datapath,header=input$header,sep=input$sep,quote=input$quote)
        if(input$header == FALSE){
          colnames(dat)<-c("Field","Magnetization")
        }
      })
      
        inScale <- input$scale
        sf <- input$smooth.factor
        dat[,3] <- log(dat[,1],10)
        dat[,4] <- smooth.spline(log(dat[,1],10),dat[,2],spar=sf)$y
        dat[,5] <- abs(predict(smooth.spline(log(dat[,1],10),dat[,2],spar=sf),deriv=1)$y)
        dat[,6] <- smooth.spline(dat[,1],dat[,2],spar=sf)$y
        dat[,7] <- abs(predict(smooth.spline(dat[,1],dat[,2],spar=sf),deriv=1)$y)
        dat[,8] <- abs(predict(smooth.spline(log(dat[,1],10),dat[,2],spar=0),deriv=1)$y)
        dat[,9] <- abs(predict(smooth.spline(dat[,1],dat[,2],spar=0),deriv=1)$y)
        
        if(is.numeric(input$plot_click$x)== "TRUE"){
          if(input$plot_click$x > max(dat[,3])/2){
          dat = subset(dat,dat[,3] < input$plot_click$x)
          }
          else(dat = subset(dat,dat[,3] > input$plot_click$x))
        }
        
        if(input$scale == "log"){
          M.sm = dat[,4]
          G = dat[,8]
          #G.sm = abs(predict(smooth.spline(log(dat[,1],10),dat[,2],spar=sf),seq(min(dat[,3]),max(dat[,3]),0.01),deriv=1)$y)
          B = dat[,3]
          B.d = seq(min(dat[,3]),max(dat[,3]),0.01)
          G.sm = approx(x=dat[,3], y=dat[,5], xout = B.d, method = "linear")$y
        }
        else({
          M.sm = dat[,6]
          G = dat[,9]
          #G.sm = abs(predict(smooth.spline(dat[,1],dat[,2],spar=sf),seq(min(dat[,1]),max(dat[,1]),1.0),deriv=1)$y)
          B = dat[,1]
          B.d = seq(min(dat[,1]),max(dat[,1]),1.0)
          G.sm = approx(x=dat[,1], y=dat[,7], xout = B.d, method = "linear")$y
        })
      ##################### components for fitting ####################################
      if(input$comp1 == TRUE){
        B1 = input$B1
        DP1 = input$DP1
        P1 = input$P1
        S1 = input$S1
      }
      else({
        B1 = NULL 
        DP1 = NULL 
        P1 = NULL  
        S1 = NULL
      })
      if(input$comp2 == TRUE){
        B2 = input$B2
        DP2 = input$DP2
        P2 = input$P2
        S2 = input$S2
      }
      else({
        B2 = NULL 
        DP2 = NULL 
        P2 = NULL 
        S2 = NULL
      })
      if(input$comp3 == TRUE){
        B3 = input$B3
        DP3 = input$DP3
        P3 = input$P3
        S3 = input$S3
      }
      else({
        B3 = NULL 
        DP3 = NULL 
        P3 = NULL 
        S3 = NULL
      })
      if(input$comp4 == TRUE){
        B4 = input$B4
        DP4 = input$DP4
        P4 = input$P4
        S4 = input$S4
      }
      else({
        B4 = NULL 
        DP4 = NULL 
        P4 = NULL 
        S4 = NULL
      })
      if(input$comp5 == TRUE){
        B5 = input$B5
        DP5 = input$DP5
        P5 = input$P5
        S5 = input$S5
      }
      else({
        B5 = NULL 
        DP5 = NULL 
        P5 = NULL 
        S5 = NULL
      })
      if(input$comp6 == TRUE){
        B6 = input$B6
        DP6 = input$DP6
        P6 = input$P6
        S6 = input$S6
      }
      else({
        B6 = NULL 
        DP6 = NULL 
        P6 = NULL 
        S6 = NULL
      })
      
      Pi = c(B1,DP1,P1,S1,
             B2,DP2,P2,S2,
             B3,DP3,P3,S3,
             B4,DP4,P4,S4,
             B5,DP5,P5,S5,
             B6,DP6,P6,S6)
      n.comp = length(Pi)/4
    #  if(input$Pi.reset){
     #   P.temp = reactive({Pf})
    #    n.comp.temp = length(P.temp())/3
    #  }
    #  if(input$reset){
     #   Pi = P.temp()
      #  n.comp = n.comp.temp
    #  }
    #  if(input$newmodel){
     #   Pi = c(B1,DP1,P1,
      #         B2,DP2,P2,
       #        B3,DP3,P3,
        #       B4,DP4,P4,
         #      B5,DP5,P5,
          #     B6,DP6,P6)
      #  n.comp = length(Pi)/3
    #  }      
      C = matrix(nrow=length(B.d),ncol=n.comp)   
    if(n.comp == 1){
      C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))
      G.model = C[,1] 
    }
    if(n.comp == 2){
      C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))
      C[,2] = Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))
      G.model = C[,1]+C[,2] 
    }
    if(n.comp == 3){
      C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))
      C[,2] = Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))
      C[,3] = Pi[11]*(max(G.sm)*(dsnorm(B.d,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.d,Pi[9],Pi[10],Pi[12]))))
      G.model = C[,1]+C[,2]+C[,3] 
    }
    if(n.comp == 4){
      C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))
      C[,2] = Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))
      C[,3] = Pi[11]*(max(G.sm)*(dsnorm(B.d,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.d,Pi[9],Pi[10],Pi[12]))))
      C[,4] = Pi[15]*(max(G.sm)*(dsnorm(B.d,Pi[13],Pi[14],Pi[16])/max(dsnorm(B.d,Pi[13],Pi[14],Pi[16]))))
      G.model = C[,1]+C[,2]+C[,3]+C[,4] 
    }
    if(n.comp == 5){
      C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))
      C[,2] = Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))
      C[,3] = Pi[11]*(max(G.sm)*(dsnorm(B.d,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.d,Pi[9],Pi[10],Pi[12]))))
      C[,4] = Pi[15]*(max(G.sm)*(dsnorm(B.d,Pi[13],Pi[14],Pi[16])/max(dsnorm(B.d,Pi[13],Pi[14],Pi[16]))))
      C[,5] = Pi[19]*(max(G.sm)*(dsnorm(B.d,Pi[17],Pi[18],Pi[20])/max(dsnorm(B.d,Pi[17],Pi[18],Pi[20]))))
      G.model = C[,1]+C[,2]+C[,3]+C[,4]+C[,5] 
    }
    if(n.comp == 6){
      C[,1] = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))
      C[,2] = Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))
      C[,3] = Pi[11]*(max(G.sm)*(dsnorm(B.d,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.d,Pi[9],Pi[10],Pi[12]))))
      C[,4] = Pi[15]*(max(G.sm)*(dsnorm(B.d,Pi[13],Pi[14],Pi[16])/max(dsnorm(B.d,Pi[13],Pi[14],Pi[16]))))
      C[,5] = Pi[19]*(max(G.sm)*(dsnorm(B.d,Pi[17],Pi[18],Pi[20])/max(dsnorm(B.d,Pi[17],Pi[18],Pi[20]))))
      C[,6] = Pi[23]*(max(G.sm)*(dsnorm(B.d,Pi[21],Pi[22],Pi[24])/max(dsnorm(B.d,Pi[21],Pi[22],Pi[24]))))
      G.model = C[,1]+C[,2]+C[,3]+C[,4]+C[,5]+C[,6] 
    }
        ##### set up optimization function #####
      if(n.comp == 1){
        fitG <- function(x,y,Pi){
          f=function(Pi){
            tot.n = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))
            sum(((tot.n-G.sm)^2))
          }
          optim(Pi,f)
        } 
      } 
      if(n.comp == 2){
        fitG <- function(x,y,Pi){
          f=function(Pi){
            tot.n = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))
            sum(((tot.n-G.sm)^2))
          }
          optim(Pi,f)
        } 
      } 
      if(n.comp == 3){
        fitG <- function(x,y,Pi){
          f=function(Pi){
            tot.n = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))+Pi[11]*(max(G.sm)*(dsnorm(B.d,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.d,Pi[9],Pi[10],Pi[12]))))
            sum(((tot.n-G.sm)^2))
          }
          optim(Pi,f)
        } 
      } 
      if(n.comp == 4){
        fitG <- function(x,y,Pi){
          f=function(Pi){
            tot.n = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))+Pi[11]*(max(G.sm)*(dsnorm(B.d,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.d,Pi[9],Pi[10],Pi[12]))))+Pi[15]*(max(G.sm)*(dsnorm(B.d,Pi[13],Pi[14],Pi[16])/max(dsnorm(B.d,Pi[13],Pi[14],Pi[16]))))
            sum(((tot.n-G.sm)^2))
          }
          optim(Pi,f)
        } 
      } 
      if(n.comp == 5){
        fitG <- function(x,y,Pi){
          f=function(Pi){
            tot.n = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))+Pi[11]*(max(G.sm)*(dsnorm(B.d,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.d,Pi[9],Pi[10],Pi[12]))))+Pi[15]*(max(G.sm)*(dsnorm(B.d,Pi[13],Pi[14],Pi[16])/max(dsnorm(B.d,Pi[13],Pi[14],Pi[16]))))+Pi[19]*(max(G.sm)*(dsnorm(B.d,Pi[17],Pi[18],Pi[20])/max(dsnorm(B.d,Pi[17],Pi[18],Pi[20]))))
            sum(((tot.n-G.sm)^2))
          }
          optim(Pi,f)
        } 
      } 
      if(n.comp == 6){
        fitG <- function(x,y,Pi){
          f=function(Pi){
            tot.n = Pi[3]*(max(G.sm)*(dsnorm(B.d,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.d,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(G.sm)*(dsnorm(B.d,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.d,Pi[5],Pi[6],Pi[8]))))+Pi[11]*(max(G.sm)*(dsnorm(B.d,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.d,Pi[9],Pi[10],Pi[12]))))+Pi[15]*(max(G.sm)*(dsnorm(B.d,Pi[13],Pi[14],Pi[16])/max(dsnorm(B.d,Pi[13],Pi[14],Pi[16]))))+Pi[19]*(max(G.sm)*(dsnorm(B.d,Pi[17],Pi[18],Pi[20])/max(dsnorm(B.d,Pi[17],Pi[18],Pi[20]))))+Pi[23]*(max(G.sm)*(dsnorm(B.d,Pi[21],Pi[22],Pi[24])/max(dsnorm(B.d,Pi[21],Pi[22],Pi[24]))))
            sum(((tot.n-G.sm)^2))
          }
          optim(Pi,f)
        } 
      } 
        fit.test = fitG(x=B.d,y=G.sm,Pi) #run function 
        
        Pf = fit.test$par
        #### calculate final distributions #####
      C.op = matrix(nrow=length(B.d),ncol=n.comp)   
      if(n.comp == 1){
        C.op[,1] = Pf[3]*(max(G.sm)*(dsnorm(B.d,Pf[1],Pf[2],Pf[4])/max(dsnorm(B.d,Pf[1],Pf[2],Pf[4]))))
        G.model.op = C.op[,1] 
      }
      if(n.comp == 2){
        C.op[,1] = Pf[3]*(max(G.sm)*(dsnorm(B.d,Pf[1],Pf[2],Pf[4])/max(dsnorm(B.d,Pf[1],Pf[2],Pf[4]))))
        C.op[,2] = Pf[7]*(max(G.sm)*(dsnorm(B.d,Pf[5],Pf[6],Pf[8])/max(dsnorm(B.d,Pf[5],Pf[6],Pf[8]))))
        G.model.op = C.op[,1]+C.op[,2] 
      }
      if(n.comp == 3){
        C.op[,1] = Pf[3]*(max(G.sm)*(dsnorm(B.d,Pf[1],Pf[2],Pf[4])/max(dsnorm(B.d,Pf[1],Pf[2],Pf[4]))))
        C.op[,2] = Pf[7]*(max(G.sm)*(dsnorm(B.d,Pf[5],Pf[6],Pf[8])/max(dsnorm(B.d,Pf[5],Pf[6],Pf[8]))))
        C.op[,3] = Pf[11]*(max(G.sm)*(dsnorm(B.d,Pf[9],Pf[10],Pf[12])/max(dsnorm(B.d,Pf[9],Pf[10],Pf[12]))))
        G.model.op = C.op[,1]+C.op[,2]+C.op[,3] 
      }
      if(n.comp == 4){
        C.op[,1] = Pf[3]*(max(G.sm)*(dsnorm(B.d,Pf[1],Pf[2],Pf[4])/max(dsnorm(B.d,Pf[1],Pf[2],Pf[4]))))
        C.op[,2] = Pf[7]*(max(G.sm)*(dsnorm(B.d,Pf[5],Pf[6],Pf[8])/max(dsnorm(B.d,Pf[5],Pf[6],Pf[8]))))
        C.op[,3] = Pf[11]*(max(G.sm)*(dsnorm(B.d,Pf[9],Pf[10],Pf[12])/max(dsnorm(B.d,Pf[9],Pf[10],Pf[12]))))
        C.op[,4] = Pf[15]*(max(G.sm)*(dsnorm(B.d,Pf[13],Pf[14],Pf[16])/max(dsnorm(B.d,Pf[13],Pf[14],Pf[16]))))
        G.model.op = C.op[,1]+C.op[,2]+C.op[,3]+C.op[,4] 
      }
      if(n.comp == 5){
        C.op[,1] = Pf[3]*(max(G.sm)*(dsnorm(B.d,Pf[1],Pf[2],Pf[4])/max(dsnorm(B.d,Pf[1],Pf[2],Pf[4]))))
        C.op[,2] = Pf[7]*(max(G.sm)*(dsnorm(B.d,Pf[5],Pf[6],Pf[8])/max(dsnorm(B.d,Pf[5],Pf[6],Pf[8]))))
        C.op[,3] = Pf[11]*(max(G.sm)*(dsnorm(B.d,Pf[9],Pf[10],Pf[12])/max(dsnorm(B.d,Pf[9],Pf[10],Pf[12]))))
        C.op[,4] = Pf[15]*(max(G.sm)*(dsnorm(B.d,Pf[13],Pf[14],Pf[16])/max(dsnorm(B.d,Pf[13],Pf[14],Pf[16]))))
        C.op[,5] = Pf[19]*(max(G.sm)*(dsnorm(B.d,Pf[17],Pf[18],Pf[20])/max(dsnorm(B.d,Pf[17],Pf[18],Pf[20]))))
        G.model.op = C.op[,1]+C.op[,2]+C.op[,3]+C.op[,4]+C.op[,5] 
      }
      if(n.comp == 6){
        C.op[,1] = Pf[3]*(max(G.sm)*(dsnorm(B.d,Pf[1],Pf[2],Pf[4])/max(dsnorm(B.d,Pf[1],Pf[2],Pf[4]))))
        C.op[,2] = Pf[7]*(max(G.sm)*(dsnorm(B.d,Pf[5],Pf[6],Pf[8])/max(dsnorm(B.d,Pf[5],Pf[6],Pf[8]))))
        C.op[,3] = Pf[11]*(max(G.sm)*(dsnorm(B.d,Pf[9],Pf[10],Pf[12])/max(dsnorm(B.d,Pf[9],Pf[10],Pf[12]))))
        C.op[,4] = Pf[15]*(max(G.sm)*(dsnorm(B.d,Pf[13],Pf[14],Pf[16])/max(dsnorm(B.d,Pf[13],Pf[14],Pf[16]))))
        C.op[,5] = Pf[19]*(max(G.sm)*(dsnorm(B.d,Pf[17],Pf[18],Pf[20])/max(dsnorm(B.d,Pf[17],Pf[18],Pf[20]))))
        C.op[,6] = Pf[23]*(max(G.sm)*(dsnorm(B.d,Pf[21],Pf[22],Pf[24])/max(dsnorm(B.d,Pf[21],Pf[22],Pf[24]))))
        G.model.op = C.op[,1]+C.op[,2]+C.op[,3]+C.op[,4]+C.op[,5]+C.op[,6] 
      }
        
        ###### calculate true contributions for each component ####### 
        library(MESS)
        mod.a = auc(B.d,G.model.op, from = min(B.d), to = max(B.d),type="spline")
        dat.a = auc(B.d,G.sm, from = min(B.d), to = max(B.d), type="spline")
        C.op.a = matrix(nrow=n.comp,ncol=1)  
      for (i in 1:n.comp){
          C.op.a[i,] = auc(B.d,C.op[,i],from = min(B.d), to = max(B.d),type = "spline")
        }
        TC = matrix(nrow=n.comp,ncol=1)
      for (i in 1:n.comp){
        TC[i,] = C.op.a[i,]/mod.a
      }
        ####### plots ######
        plot(B,G,col="dark grey",type="p",pch=20,
             ylim=c(0,max(G)),
             xlab = 
               if(input$scale == "log"){paste("log B (",input$B.units,")")} 
             else({paste("B (",input$B.units,")")}), 
             ylab = if(input$scale == "log"){
               c("dM/dlog(B)")
             }
             else({c("dM/dB")}))
        points(B.d,G.sm,col="black",type="l",lwd=3)
      colors = c("blue","purple","green","dark red","dark green","magenta")
      for(i in 1:ncol(C.op)){
        points(B.d,C.op[,i],col=colors[i],type="l",lwd=1)
      }
        points(B.d,G.model.op,col="orange",lwd=5,type="l")
       
      output$results <- renderPrint({
          final.parameters<- matrix(nrow=5,ncol=n.comp)
            if(n.comp == 1){
            final.parameters[,1] = c(Pf[1:4],TC[1,])
            }
          if(n.comp == 2){
            final.parameters[,1] = c(Pf[1:4],TC[1,])
            final.parameters[,2] = c(Pf[5:8],TC[2,])
          }
          if(n.comp == 3){
            final.parameters[,1] = c(Pf[1:4],TC[1,])
            final.parameters[,2] = c(Pf[5:8],TC[2,])
            final.parameters[,3] = c(Pf[9:12],TC[3,])
          }
          if(n.comp == 4){
            final.parameters[,1] = c(Pf[1:4],TC[1,])
            final.parameters[,2] = c(Pf[5:8],TC[2,])
            final.parameters[,3] = c(Pf[9:12],TC[3,])
            final.parameters[,4] = c(Pf[13:16],TC[4,])
          }
          if(n.comp == 5){
            final.parameters[,1] = c(Pf[1:4],TC[1,])
            final.parameters[,2] = c(Pf[5:8],TC[2,])
            final.parameters[,3] = c(Pf[9:12],TC[3,])
            final.parameters[,4] = c(Pf[13:16],TC[4,])
            final.parameters[,5] = c(Pf[17:20],TC[5,])
          }
          if(n.comp == 6){
            final.parameters[,1] = c(Pf[1:4],TC[1,])
            final.parameters[,2] = c(Pf[5:8],TC[2,])
            final.parameters[,3] = c(Pf[9:12],TC[3,])
            final.parameters[,4] = c(Pf[13:16],TC[4,])
            final.parameters[,5] = c(Pf[17:20],TC[5,])
            final.parameters[,6] = c(Pf[21:24],TC[6,])
          }
          col.labels = c("component 1","component 2", "component 3", "component 4", "component 5", "component 6")
          colnames(final.parameters) = col.labels[1:n.comp]
          rownames(final.parameters) = c("Bh","DP","P","S","TC")
          final.parameters
      })
      output$RSS.plot3 <- renderPrint({
        RSS.plot3 <- fit.test$value
        RSS.plot3
      })
    
    #### F-test ###### 
    if(input$ftest == TRUE) {
      n = length(B.d)
      n.comp.2 = input$n.comp2
      RSS <- fit.test$value
      RSS.2 = as.numeric(input$RSS.op1)
      if(n.comp > n.comp.2){
        f.val = ((RSS.2 - RSS)/((n-n.comp.2)-(n-n.comp)))/(RSS/(n-n.comp))
        p.val = pf(f.val,((n-n.comp.2)-(n-n.comp)),(n-n.comp),lower.tail=FALSE)
      }
      else({
        f.val = ((RSS - RSS.2)/((n-n.comp)-(n-n.comp.2)))/(RSS.2/(n-n.comp.2))
        p.val = pf(f.val,((n-n.comp)-(n-n.comp.2)),(n-n.comp.2),lower.tail=FALSE)
      })
      output$f.val <- renderPrint({
        f.val
      })
      output$p.val <- renderPrint({
        p.val 
      })
    }

 output$plot4<-renderPlot({

##############################################
#### begin code for resampling ###############
s = list () #to hold all the resampled subsets of data for spline fitting
sp = list() #to hold all the spline fits
z = list () #to hold derivatives 

n = input$n.resample

if(input$scale == "log"){
B.r = seq(min(dat[,3]),max(dat[,3]),by=0.01)
}
else({
B.r = seq(min(dat[,1]),max(dat[,1]),by=1.0)
})

p.r = input$p.r

for (i in 1:n){
  s[i] = list(dat[sample(1:nrow(dat),round(p.r*nrow(dat)),replace=FALSE),]) #subset of data for spline fitting
  
  if(input$scale =="log"){
  sp[i] = list(smooth.spline(x=log(unlist(s[[i]][1]),10),y=unlist(s[[i]][2]),spar=sf))
  }
  else({
    sp[i] = list(smooth.spline(x= unlist(s[[i]][1]), y= unlist(s[[i]][2]),spar=sf))
  })
}
for (i in 1:n){
  if(input$scale == "log"){
  z[i] = list(approx(x = dat[,3], y = (abs((predict(sp[[i]],dat[,3],deriv=1)$y))), xout = B.r, method = "linear")$y)
  }
  else({
    z[i] = list(approx(x = dat[,1], y = (abs((predict(sp[[i]],dat[,1],deriv=1)$y))), xout = B.r, method = "linear")$y)
  })
}

#calculate median, mean, and quartiles for the y-axis coercivity resample data 
H = matrix(unlist(z),nrow=length(B.r))
H.u = apply(H,1,quantile,probs=0.975)
H.l = apply(H,1,quantile,probs=0.025)
H_mean = apply(H,1,mean)

#fit.p = list()
Pi = Pf

P.r = matrix(nrow=length(Pi),ncol=n)
for (i in 1:n){
  for(j in 1:(4*n.comp)){ 
  P.r[j,i] = rnorm(1,Pi[j],Pi*0.02)
}
}

if(n.comp == 1){
  l = c(1:3)
}
if(n.comp == 2){
  l = c(1:3,5:7)
}
if(n.comp == 3){
  l = c(1:3,5:7,9:11)
}
if(n.comp == 4){
  l = c(1:3,5:7,9:11,13:15)
}
if(n.comp == 5){
  l = c(1:3,5:7,9:11,13:15,17:19)
}
if(n.comp == 6){
  l = c(1:3,5:7,9:11,13:15,17:19,21:23)
}
for (i in l){
  P.r[i,] = abs(P.r[i,])
}

#function again 
##### set up optimization function #####
if(n.comp == 1){
  fitR <- function(x,y,Pi){
    f=function(Pi){
      tot.n = Pi[3]*(max(y)*(dsnorm(B.r,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.r,Pi[1],Pi[2],Pi[4]))))
      sum(((tot.n-y)^2))
    }
    optim(Pi,f)
  } 
} 
if(n.comp == 2){
  fitR <- function(x,y,Pi){
    f=function(Pi){
      tot.n = Pi[3]*(max(y)*(dsnorm(B.r,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.r,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(H[,i])*(dsnorm(B.r,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.r,Pi[5],Pi[6],Pi[8]))))
      sum(((tot.n-y)^2))
    }
    optim(Pi,f)
  } 
} 
if(n.comp == 3){
  fitR <- function(x,y,Pi){
    f=function(Pi){
      tot.n = Pi[3]*(max(y)*(dsnorm(B.r,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.r,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(H[,i])*(dsnorm(B.r,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.r,Pi[5],Pi[6],Pi[8]))))+Pi[11]*(max(H[,i])*(dsnorm(B.r,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.r,Pi[9],Pi[10],Pi[12]))))
      sum(((tot.n-y)^2))
    }
    optim(Pi,f)
  } 
} 
if(n.comp == 4){
  fitR <- function(x,y,Pi){
    f=function(Pi){
      tot.n = Pi[3]*(max(y)*(dsnorm(B.r,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.r,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(H[,i])*(dsnorm(B.r,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.r,Pi[5],Pi[6],Pi[8]))))+Pi[11]*(max(H[,i])*(dsnorm(B.r,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.r,Pi[9],Pi[10],Pi[12]))))+Pi[15]*(max(H[,i])*(dsnorm(B.r,Pi[13],Pi[14],Pi[16])/max(dsnorm(B.r,Pi[13],Pi[14],Pi[16]))))
      sum(((tot.n-y)^2))
    }
    optim(Pi,f)
  } 
} 
if(n.comp == 5){
  fitR <- function(x,y,Pi){
    f=function(Pi){
      tot.n = Pi[3]*(max(y)*(dsnorm(B.r,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.r,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(H[,i])*(dsnorm(B.r,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.r,Pi[5],Pi[6],Pi[8]))))+Pi[11]*(max(H[,i])*(dsnorm(B.r,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.r,Pi[9],Pi[10],Pi[12]))))+Pi[15]*(max(H[,i])*(dsnorm(B.r,Pi[13],Pi[14],Pi[16])/max(dsnorm(B.r,Pi[13],Pi[14],Pi[16]))))+Pi[19]*(max(H[,i])*(dsnorm(B.r,Pi[17],Pi[18],Pi[20])/max(dsnorm(B.r,Pi[17],Pi[18],Pi[20]))))
      sum(((tot.n-y)^2))
    }
    optim(Pi,f)
  } 
} 
if(n.comp == 6){
  fitR <- function(x,y,Pi){
    f=function(Pi){
      tot.n = Pi[3]*(max(y)*(dsnorm(B.r,Pi[1],Pi[2],Pi[4])/max(dsnorm(B.r,Pi[1],Pi[2],Pi[4]))))+Pi[7]*(max(H[,i])*(dsnorm(B.r,Pi[5],Pi[6],Pi[8])/max(dsnorm(B.r,Pi[5],Pi[6],Pi[8]))))+Pi[11]*(max(H[,i])*(dsnorm(B.r,Pi[9],Pi[10],Pi[12])/max(dsnorm(B.r,Pi[9],Pi[10],Pf[12]))))+Pi[15]*(max(H[,i])*(dsnorm(B.r,Pi[13],Pi[14],Pi[16])/max(dsnorm(B.r,Pi[13],Pi[14],Pi[16]))))+Pi[19]*(max(H[,i])*(dsnorm(B.r,Pi[17],Pi[18],Pi[20])/max(dsnorm(B.r,Pi[17],Pi[18],Pi[20]))))+Pi[23]*(max(H[,i])*(dsnorm(B.r,Pi[21],Pi[22],Pi[24])/max(dsnorm(B.r,Pi[21],Pi[22],Pi[24]))))
      sum(((tot.n-y)^2))
    }
    optim(Pi,f)
  } 
} 

fit.p = matrix(nrow = n.comp*4,ncol= n)

if(input$calc.error == TRUE) {
  withProgress(message="Perfoming calculations, please be patient",value=0,{
for(i in 1:n){
  fit.p[,i] = fitR(x=B.r,y=H[,i],Pi=P.r[,i])$par
  incProgress(1/n,detail=paste("resample number",i))
    }
  })

p = fit.p
co.1.r = matrix(nrow=length(B.r),ncol=n)
co.2.r = matrix(nrow=length(B.r),ncol=n)
co.3.r = matrix(nrow=length(B.r),ncol=n)
co.4.r = matrix(nrow=length(B.r),ncol=n)
co.5.r = matrix(nrow=length(B.r),ncol=n)
co.6.r = matrix(nrow=length(B.r),ncol=n)
D = matrix(nrow=length(B.r),ncol=n)

for (i in 1:n){
if(n.comp==1){
  co.1.r[,i] = p[3,i]*(max(H[,i])*(dsnorm(B.r,p[1,i],p[2,i],p[4,i])/max(dsnorm(B.r,p[1,i],p[2,i],p[4,i]))))  
  D[,i]=co.1.r[,i]
}
if(n.comp==2){
  co.1.r[,i] = p[3,i]*(max(H[,i])*(dsnorm(B.r,p[1,i],p[2,i],p[4,i])/max(dsnorm(B.r,p[1,i],p[2,i],p[4,i])))) 
  co.2.r[,i] = p[7,i]*(max(H[,i])*(dsnorm(B.r,p[5,i],p[6,i],p[8,i])/max(dsnorm(B.r,p[5,i],p[6,i],p[8,i]))))
  D[,i]=co.1.r[,i]+co.2.r[,i]
}
if(n.comp==3){
  co.1.r[,i] = p[3,i]*(max(H[,i])*(dsnorm(B.r,p[1,i],p[2,i],p[4,i])/max(dsnorm(B.r,p[1,i],p[2,i],p[4,i]))))
  co.2.r[,i] = p[7,i]*(max(H[,i])*(dsnorm(B.r,p[5,i],p[6,i],p[8,i])/max(dsnorm(B.r,p[5,i],p[6,i],p[8,i]))))
  co.3.r[,i] = p[11,i]*(max(H[,i])*(dsnorm(B.r,p[9,i],p[10,i],p[12,i])/max(dsnorm(B.r,p[9,i],p[10,i],p[12,i]))))
  D[,i]=co.1.r[,i]+co.2.r[,i]+co.3.r[,i]
}
if(n.comp==4){
  co.1.r[,i] = p[3,i]*(max(H[,i])*(dsnorm(B.r,p[1,i],p[2,i],p[4,i])/max(dsnorm(B.r,p[1,i],p[2,i],p[4,i]))))
  co.2.r[,i] = p[7,i]*(max(H[,i])*(dsnorm(B.r,p[5,i],p[6,i],p[8,i])/max(dsnorm(B.r,p[5,i],p[6,i],p[8,i]))))
  co.3.r[,i] = p[11,i]*(max(H[,i])*(dsnorm(B.r,p[9,i],p[10,i],p[12,i])/max(dsnorm(B.r,p[9,i],p[10,i],p[12,i]))))
  co.4.r[,i] = p[15,i]*(max(H[,i])*(dsnorm(B.r,p[13,i],p[14,i],p[16,i])/max(dsnorm(B.r,p[13,i],p[14,i],p[16,i]))))
  D[,i]=co.1.r[,i]+co.2.r[,i]+co.3.r[,i]+co.4.r[,i]
}
if(n.comp==5){
  co.1.r[,i] = p[3,i]*(max(H[,i])*(dsnorm(B.r,p[1,i],p[2,i],p[4,i])/max(dsnorm(B.r,p[1,i],p[2,i],p[4,i]))))
  co.2.r[,i] = p[7,i]*(max(H[,i])*(dsnorm(B.r,p[5,i],p[6,i],p[8,i])/max(dsnorm(B.r,p[5,i],p[6,i],p[8,i]))))
  co.3.r[,i] = p[11,i]*(max(H[,i])*(dsnorm(B.r,p[9,i],p[10,i],p[12,i])/max(dsnorm(B.r,p[9,i],p[10,i],p[12,i]))))
  co.4.r[,i] = p[15,i]*(max(H[,i])*(dsnorm(B.r,p[13,i],p[14,i],p[16,i])/max(dsnorm(B.r,p[13,i],p[14,i],p[16,i]))))
  co.5.r[,i] = p[19,i]*(max(H[,i])*(dsnorm(B.r,p[17,i],p[18,i],p[20,i])/max(dsnorm(B.r,p[17,i],p[18,i],p[20,i]))))
  D[,i]=co.1.r[,i]+co.2.r[,i]+co.3.r[,i]+co.4.r[,i]+co.5.r[,i]
}
if(n.comp==6){
  co.1.r[,i] = p[3,i]*(max(H[,i])*(dsnorm(B.r,p[1,i],p[2,i],p[4,i])/max(dsnorm(B.r,p[1,i],p[2,i],p[4,i]))))
  co.2.r[,i] = p[7,i]*(max(H[,i])*(dsnorm(B.r,p[5,i],p[6,i],p[8,i])/max(dsnorm(B.r,p[5,i],p[6,i],p[8,i]))))
  co.3.r[,i] = p[11,i]*(max(H[,i])*(dsnorm(B.r,p[9,i],p[10,i],p[12,i])/max(dsnorm(B.r,p[9,i],p[10,i],p[12,i]))))
  co.4.r[,i] = p[15,i]*(max(H[,i])*(dsnorm(B.r,p[13,i],p[14,i],p[16,i])/max(dsnorm(B.r,p[13,i],p[14,i],p[16,i]))))
  co.5.r[,i] = p[19,i]*(max(H[,i])*(dsnorm(B.r,p[17,i],p[18,i],p[20,i])/max(dsnorm(B.r,p[17,i],p[18,i],p[20,i]))))
  co.6.r[,i] = p[23,i]*(max(H[,i])*(dsnorm(B.r,p[21,i],p[22,i],d[24,i])/max(dsnorm(B.r,p[21,i],p[22,i],p[24,i]))))
  D[,i]=co.1.r[,i]+co.2.r[,i]+co.3.r[,i]+co.4.r[,i]+co.5.r[,i]+co.6.r[,i]
} 
} #closes for loop 

D_mean = apply(D,1,mean,na.rm=T)
D_sd = apply(D,1,sd,na.rm=T)
#D_lower = D_mean - D_sd
#D_upper = D_mean + D_sd
D_upper = apply(D,1,quantile,probs=0.975,na.rm=T)
D_lower = apply(D,1,quantile,probs=0.025,na.rm=T)

p_mean = apply(p,1,mean,na.rm=T)
p_sd = apply(p,1,sd,na.rm=T)
p_upper = p_mean + p_sd
p_lower = p_mean - p_sd

#### using the parameters to define the error envelopes is causing errors 
#### instead, the approach below will calculate the 95% confidence interval of individual component resamples

if(n.comp == 1){
co.1.mean = apply(co.1.r,1,mean,na.rm=T)
co.1.upper = apply(co.1.r,1,quantile,probs=0.975,na.rm=T)
co.1.lower = apply(co.1.r,1,quantile,probs=0.025,na.rm=T)
}
if(n.comp == 2){
  co.1.mean = apply(co.1.r,1,mean,na.rm=T)
  co.1.upper = apply(co.1.r,1,quantile,probs=0.975,na.rm=T)
  co.1.lower = apply(co.1.r,1,quantile,probs=0.025,na.rm=T)
  co.2.mean = apply(co.2.r,1,mean,na.rm=T)
  co.2.upper = apply(co.2.r,1,quantile,probs=0.975,na.rm=T)
  co.2.lower = apply(co.2.r,1,quantile,probs=0.025,na.rm=T)
}
if(n.comp == 3){
  co.1.mean = apply(co.1.r,1,mean,na.rm=T)
  co.1.upper = apply(co.1.r,1,quantile,probs=0.975,na.rm=T)
  co.1.lower = apply(co.1.r,1,quantile,probs=0.025,na.rm=T)
  co.2.mean = apply(co.2.r,1,mean,na.rm=T)
  co.2.upper = apply(co.2.r,1,quantile,probs=0.975,na.rm=T)
  co.2.lower = apply(co.2.r,1,quantile,probs=0.025,na.rm=T)
  co.3.mean = apply(co.3.r,1,mean,na.rm=T)
  co.3.upper = apply(co.3.r,1,quantile,probs=0.975,na.rm=T)
  co.3.lower = apply(co.3.r,1,quantile,probs=0.025,na.rm=T)
}
if(n.comp == 4){
  co.1.mean = apply(co.1.r,1,mean,na.rm=T)
  co.1.upper = apply(co.1.r,1,quantile,probs=0.975,na.rm=T)
  co.1.lower = apply(co.1.r,1,quantile,probs=0.025,na.rm=T)
  co.2.mean = apply(co.2.r,1,mean,na.rm=T)
  co.2.upper = apply(co.2.r,1,quantile,probs=0.975,na.rm=T)
  co.2.lower = apply(co.2.r,1,quantile,probs=0.025,na.rm=T)
  co.3.mean = apply(co.3.r,1,mean,na.rm=T)
  co.3.upper = apply(co.3.r,1,quantile,probs=0.975,na.rm=T)
  co.3.lower = apply(co.3.r,1,quantile,probs=0.025,na.rm=T)
  co.4.mean = apply(co.4.r,1,mean,na.rm=T)
  co.4.upper = apply(co.4.r,1,quantile,probs=0.975,na.rm=T)
  co.4.lower = apply(co.4.r,1,quantile,probs=0.025,na.rm=T)
}
if(n.comp == 5){
  co.1.mean = apply(co.1.r,1,mean,na.rm=T)
  co.1.upper = apply(co.1.r,1,quantile,probs=0.975,na.rm=T)
  co.1.lower = apply(co.1.r,1,quantile,probs=0.025,na.rm=T)
  co.2.mean = apply(co.2.r,1,mean,na.rm=T)
  co.2.upper = apply(co.2.r,1,quantile,probs=0.975,na.rm=T)
  co.2.lower = apply(co.2.r,1,quantile,probs=0.025,na.rm=T)
  co.3.mean = apply(co.3.r,1,mean,na.rm=T)
  co.3.upper = apply(co.3.r,1,quantile,probs=0.975,na.rm=T)
  co.3.lower = apply(co.3.r,1,quantile,probs=0.025,na.rm=T)
  co.4.mean = apply(co.4.r,1,mean,na.rm=T)
  co.4.upper = apply(co.4.r,1,quantile,probs=0.975,na.rm=T)
  co.4.lower = apply(co.4.r,1,quantile,probs=0.025,na.rm=T)
  co.5.mean = apply(co.5.r,1,mean,na.rm=T)
  co.5.upper = apply(co.5.r,1,quantile,probs=0.975,na.rm=T)
  co.5.lower = apply(co.5.r,1,quantile,probs=0.025,na.rm=T)
}
if(n.comp == 6){
  co.1.mean = apply(co.1.r,1,mean,na.rm=T)
  co.1.upper = apply(co.1.r,1,quantile,probs=0.975,na.rm=T)
  co.1.lower = apply(co.1.r,1,quantile,probs=0.025,na.rm=T)
  co.2.mean = apply(co.2.r,1,mean,na.rm=T)
  co.2.upper = apply(co.2.r,1,quantile,probs=0.975,na.rm=T)
  co.2.lower = apply(co.2.r,1,quantile,probs=0.025,na.rm=T)
  co.3.mean = apply(co.3.r,1,mean,na.rm=T)
  co.3.upper = apply(co.3.r,1,quantile,probs=0.975,na.rm=T)
  co.3.lower = apply(co.3.r,1,quantile,probs=0.025,na.rm=T)
  co.4.mean = apply(co.4.r,1,mean,na.rm=T)
  co.4.upper = apply(co.4.r,1,quantile,probs=0.975,na.rm=T)
  co.4.lower = apply(co.4.r,1,quantile,probs=0.025,na.rm=T)
  co.5.mean = apply(co.5.r,1,mean,na.rm=T)
  co.5.upper = apply(co.5.r,1,quantile,probs=0.975,na.rm=T)
  co.5.lower = apply(co.5.r,1,quantile,probs=0.025,na.rm=T)
  co.6.mean = apply(co.6.r,1,mean,na.rm=T)
  co.6.upper = apply(co.6.r,1,quantile,probs=0.975,na.rm=T)
  co.6.lower = apply(co.6.r,1,quantile,probs=0.025,na.rm=T)
}


p.final = cbind(p_mean,p_sd)


library(MESS)
if(n.comp == 1){
  C1.op.m = co.1.mean
  C1.op.l = co.1.lower 
  C1.op.u = co.1.upper
  G.model.op.m = D_mean
  G.model.op.l = D_lower
  G.model.op.u = D_upper
  mod.a.m = auc(B.r,G.model.op.m,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.l = auc(B.r,G.model.op.l,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.u = auc(B.r,G.model.op.u,from = min(B.r),to = max(B.r),type = "spline")
  dat.a.m = auc(B.r,H_mean,from = min(B.r), to = max(B.r),type="spline")
  dat.a.l = auc(B.r,H.l,from = min(B.r), to = max(B.r),type="spline")
  dat.a.u = auc(B.r,H.u,from = min(B.r), to = max(B.r),type="spline")
  ## comp 1 ## 
  C1.op.m.a = auc(B.r,C1.op.m,from = min(B.r), to = max(B.r),type="spline")
  C1.op.l.a = auc(B.r,C1.op.l,from = min(B.r), to = max(B.r),type="spline")
  C1.op.u.a = auc(B.r,C1.op.u,from = min(B.r), to = max(B.r),type="spline")
  ## % contribution ## 
  TC = matrix(nrow = n.comp,ncol=3)
  TC[1,1] <- C1.op.m.a/mod.a.m
  TC[1,2] <- C1.op.l.a/mod.a.l #l
  TC[1,3] <- C1.op.u.a/mod.a.u #u
}
if(n.comp == 2){
  C1.op.m = co.1.mean
  C1.op.l = co.1.lower 
  C1.op.u = co.1.upper
  C2.op.m = co.2.mean
  C2.op.l = co.2.lower 
  C2.op.u = co.2.upper
  G.model.op.m = D_mean
  G.model.op.l = D_lower
  G.model.op.u = D_upper
  mod.a.m = auc(B.r,G.model.op.m,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.l = auc(B.r,G.model.op.l,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.u = auc(B.r,G.model.op.u,from = min(B.r),to = max(B.r),type = "spline")
  dat.a.m = auc(B.r,H_mean,from = min(B.r), to = max(B.r),type="spline")
  dat.a.l = auc(B.r,H.l,from = min(B.r), to = max(B.r),type="spline")
  dat.a.u = auc(B.r,H.u,from = min(B.r), to = max(B.r),type="spline")
  ## comp 1 ## 
  C1.op.m.a = auc(B.r,C1.op.m,from = min(B.r), to = max(B.r),type="spline")
  C1.op.l.a = auc(B.r,C1.op.l,from = min(B.r), to = max(B.r),type="spline")
  C1.op.u.a = auc(B.r,C1.op.u,from = min(B.r), to = max(B.r),type="spline")
  C2.op.m.a = auc(B.r,C2.op.m,from = min(B.r), to = max(B.r),type="spline")
  C2.op.l.a = auc(B.r,C2.op.l,from = min(B.r), to = max(B.r),type="spline")
  C2.op.u.a = auc(B.r,C2.op.u,from = min(B.r), to = max(B.r),type="spline")
  ## % contribution ## 
  TC = matrix(nrow = n.comp,ncol=3)
  TC[1,1] <- C1.op.m.a/mod.a.m
  TC[1,2] <- C1.op.l.a/mod.a.l #l
  TC[1,3] <- C1.op.u.a/mod.a.u #u
  TC[2,1] <- C2.op.m.a/mod.a.m
  TC[2,2] <- C2.op.l.a/mod.a.l #l
  TC[2,3] <- C2.op.u.a/mod.a.u #u
}
if(n.comp == 3){
  C1.op.m = co.1.mean
  C1.op.l = co.1.lower 
  C1.op.u = co.1.upper
  C2.op.m = co.2.mean
  C2.op.l = co.2.lower 
  C2.op.u = co.2.upper
  C3.op.m = co.3.mean
  C3.op.l = co.3.lower 
  C3.op.u = co.3.upper
  G.model.op.m = D_mean
  G.model.op.l = D_lower
  G.model.op.u = D_upper
  mod.a.m = auc(B.r,G.model.op.m,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.l = auc(B.r,G.model.op.l,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.u = auc(B.r,G.model.op.u,from = min(B.r),to = max(B.r),type = "spline")
  dat.a.m = auc(B.r,H_mean,from = min(B.r), to = max(B.r),type="spline")
  dat.a.l = auc(B.r,H.l,from = min(B.r), to = max(B.r),type="spline")
  dat.a.u = auc(B.r,H.u,from = min(B.r), to = max(B.r),type="spline")
  ## comp 1 ## 
  C1.op.m.a = auc(B.r,C1.op.m,from = min(B.r), to = max(B.r),type="spline")
  C1.op.l.a = auc(B.r,C1.op.l,from = min(B.r), to = max(B.r),type="spline")
  C1.op.u.a = auc(B.r,C1.op.u,from = min(B.r), to = max(B.r),type="spline")
  C2.op.m.a = auc(B.r,C2.op.m,from = min(B.r), to = max(B.r),type="spline")
  C2.op.l.a = auc(B.r,C2.op.l,from = min(B.r), to = max(B.r),type="spline")
  C2.op.u.a = auc(B.r,C2.op.u,from = min(B.r), to = max(B.r),type="spline")
  C3.op.m.a = auc(B.r,C3.op.m,from = min(B.r), to = max(B.r),type="spline")
  C3.op.l.a = auc(B.r,C3.op.l,from = min(B.r), to = max(B.r),type="spline")
  C3.op.u.a = auc(B.r,C3.op.u,from = min(B.r), to = max(B.r),type="spline")
  ## % contribution ## 
  TC = matrix(nrow = n.comp,ncol=3)
  TC[1,1] <- C1.op.m.a/mod.a.m
  TC[1,2] <- C1.op.l.a/mod.a.l #l
  TC[1,3] <- C1.op.u.a/mod.a.u #u
  TC[2,1] <- C2.op.m.a/mod.a.m
  TC[2,2] <- C2.op.l.a/mod.a.l #l
  TC[2,3] <- C2.op.u.a/mod.a.u #u
  TC[3,1] <- C3.op.m.a/mod.a.m
  TC[3,2] <- C3.op.l.a/mod.a.l #l
  TC[3,3] <- C3.op.u.a/mod.a.u #u
}
if(n.comp == 4){
  C1.op.m = co.1.mean
  C1.op.l = co.1.lower 
  C1.op.u = co.1.upper
  C2.op.m = co.2.mean
  C2.op.l = co.2.lower 
  C2.op.u = co.2.upper
  C3.op.m = co.3.mean
  C3.op.l = co.3.lower 
  C3.op.u = co.3.upper
  C4.op.m = co.4.mean
  C4.op.l = co.4.lower 
  C4.op.u = co.4.upper
  G.model.op.m = D_mean
  G.model.op.l = D_lower
  G.model.op.u = D_upper
  mod.a.m = auc(B.r,G.model.op.m,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.l = auc(B.r,G.model.op.l,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.u = auc(B.r,G.model.op.u,from = min(B.r),to = max(B.r),type = "spline")
  dat.a.m = auc(B.r,H_mean,from = min(B.r), to = max(B.r),type="spline")
  dat.a.l = auc(B.r,H.l,from = min(B.r), to = max(B.r),type="spline")
  dat.a.u = auc(B.r,H.u,from = min(B.r), to = max(B.r),type="spline")
  ## comp 1 ## 
  C1.op.m.a = auc(B.r,C1.op.m,from = min(B.r), to = max(B.r),type="spline")
  C1.op.l.a = auc(B.r,C1.op.l,from = min(B.r), to = max(B.r),type="spline")
  C1.op.u.a = auc(B.r,C1.op.u,from = min(B.r), to = max(B.r),type="spline")
  C2.op.m.a = auc(B.r,C2.op.m,from = min(B.r), to = max(B.r),type="spline")
  C2.op.l.a = auc(B.r,C2.op.l,from = min(B.r), to = max(B.r),type="spline")
  C2.op.u.a = auc(B.r,C2.op.u,from = min(B.r), to = max(B.r),type="spline")
  C3.op.m.a = auc(B.r,C3.op.m,from = min(B.r), to = max(B.r),type="spline")
  C3.op.l.a = auc(B.r,C3.op.l,from = min(B.r), to = max(B.r),type="spline")
  C3.op.u.a = auc(B.r,C3.op.u,from = min(B.r), to = max(B.r),type="spline")
  C4.op.m.a = auc(B.r,C4.op.m,from = min(B.r), to = max(B.r),type="spline")
  C4.op.l.a = auc(B.r,C4.op.l,from = min(B.r), to = max(B.r),type="spline")
  C4.op.u.a = auc(B.r,C4.op.u,from = min(B.r), to = max(B.r),type="spline")
  ## % contribution ## 
  TC = matrix(nrow = n.comp,ncol=3)
  TC[1,1] <- C1.op.m.a/mod.a.m
  TC[1,2] <- C1.op.l.a/mod.a.l #l
  TC[1,3] <- C1.op.u.a/mod.a.u #u
  TC[2,1] <- C2.op.m.a/mod.a.m
  TC[2,2] <- C2.op.l.a/mod.a.l #l
  TC[2,3] <- C2.op.u.a/mod.a.u #u
  TC[3,1] <- C3.op.m.a/mod.a.m
  TC[3,2] <- C3.op.l.a/mod.a.l #l
  TC[3,3] <- C3.op.u.a/mod.a.u #u
  TC[4,1] <- C3.op.m.a/mod.a.m
  TC[4,2] <- C3.op.l.a/mod.a.l #l
  TC[4,3] <- C3.op.u.a/mod.a.u #u
}
if(n.comp == 5){
  C1.op.m = co.1.mean
  C1.op.l = co.1.lower 
  C1.op.u = co.1.upper
  C2.op.m = co.2.mean
  C2.op.l = co.2.lower 
  C2.op.u = co.2.upper
  C3.op.m = co.3.mean
  C3.op.l = co.3.lower 
  C3.op.u = co.3.upper
  C4.op.m = co.4.mean
  C4.op.l = co.4.lower 
  C4.op.u = co.4.upper
  C5.op.m = co.5.mean
  C5.op.l = co.5.lower 
  C5.op.u = co.5.upper
  G.model.op.m = D_mean
  G.model.op.l = D_lower
  G.model.op.u = D_upper
  mod.a.m = auc(B.r,G.model.op.m,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.l = auc(B.r,G.model.op.l,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.u = auc(B.r,G.model.op.u,from = min(B.r),to = max(B.r),type = "spline")
  dat.a.m = auc(B.r,H_mean,from = min(B.r), to = max(B.r),type="spline")
  dat.a.l = auc(B.r,H.l,from = min(B.r), to = max(B.r),type="spline")
  dat.a.u = auc(B.r,H.u,from = min(B.r), to = max(B.r),type="spline")
  ## comp 1 ## 
  C1.op.m.a = auc(B.r,C1.op.m,from = min(B.r), to = max(B.r),type="spline")
  C1.op.l.a = auc(B.r,C1.op.l,from = min(B.r), to = max(B.r),type="spline")
  C1.op.u.a = auc(B.r,C1.op.u,from = min(B.r), to = max(B.r),type="spline")
  C2.op.m.a = auc(B.r,C2.op.m,from = min(B.r), to = max(B.r),type="spline")
  C2.op.l.a = auc(B.r,C2.op.l,from = min(B.r), to = max(B.r),type="spline")
  C2.op.u.a = auc(B.r,C2.op.u,from = min(B.r), to = max(B.r),type="spline")
  C3.op.m.a = auc(B.r,C3.op.m,from = min(B.r), to = max(B.r),type="spline")
  C3.op.l.a = auc(B.r,C3.op.l,from = min(B.r), to = max(B.r),type="spline")
  C3.op.u.a = auc(B.r,C3.op.u,from = min(B.r), to = max(B.r),type="spline")
  C4.op.m.a = auc(B.r,C4.op.m,from = min(B.r), to = max(B.r),type="spline")
  C4.op.l.a = auc(B.r,C4.op.l,from = min(B.r), to = max(B.r),type="spline")
  C4.op.u.a = auc(B.r,C4.op.u,from = min(B.r), to = max(B.r),type="spline")
  C5.op.m.a = auc(B.r,C5.op.m,from = min(B.r), to = max(B.r),type="spline")
  C5.op.l.a = auc(B.r,C5.op.l,from = min(B.r), to = max(B.r),type="spline")
  C5.op.u.a = auc(B.r,C5.op.u,from = min(B.r), to = max(B.r),type="spline")
  ## % contribution ## 
  TC = matrix(nrow = n.comp,ncol=3)
  TC[1,1] <- C1.op.m.a/mod.a.m
  TC[1,2] <- C1.op.l.a/mod.a.l #l
  TC[1,3] <- C1.op.u.a/mod.a.u #u
  TC[2,1] <- C2.op.m.a/mod.a.m
  TC[2,2] <- C2.op.l.a/mod.a.l #l
  TC[2,3] <- C2.op.u.a/mod.a.u #u
  TC[3,1] <- C3.op.m.a/mod.a.m
  TC[3,2] <- C3.op.l.a/mod.a.l #l
  TC[3,3] <- C3.op.u.a/mod.a.u #u
  TC[4,1] <- C3.op.m.a/mod.a.m
  TC[4,2] <- C3.op.l.a/mod.a.l #l
  TC[4,3] <- C3.op.u.a/mod.a.u #u
  TC[5,1] <- C3.op.m.a/mod.a.m
  TC[5,2] <- C3.op.l.a/mod.a.l #l
  TC[5,3] <- C3.op.u.a/mod.a.u #u
}
if(n.comp == 6){
  C1.op.m = co.1.mean
  C1.op.l = co.1.lower 
  C1.op.u = co.1.upper
  C2.op.m = co.2.mean
  C2.op.l = co.2.lower 
  C2.op.u = co.2.upper
  C3.op.m = co.3.mean
  C3.op.l = co.3.lower 
  C3.op.u = co.3.upper
  C4.op.m = co.4.mean
  C4.op.l = co.4.lower 
  C4.op.u = co.4.upper
  C5.op.m = co.5.mean
  C5.op.l = co.5.lower 
  C5.op.u = co.5.upper
  C6.op.m = co.6.mean
  C6.op.l = co.6.lower 
  C6.op.u = co.6.upper
  G.model.op.m = D_mean
  G.model.op.l = D_lower
  G.model.op.u = D_upper
  mod.a.m = auc(B.r,G.model.op.m,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.l = auc(B.r,G.model.op.l,from = min(B.r),to = max(B.r),type = "spline")
  mod.a.u = auc(B.r,G.model.op.u,from = min(B.r),to = max(B.r),type = "spline")
  dat.a.m = auc(B.r,H_mean,from = min(B.r), to = max(B.r),type="spline")
  dat.a.l = auc(B.r,H.l,from = min(B.r), to = max(B.r),type="spline")
  dat.a.u = auc(B.r,H.u,from = min(B.r), to = max(B.r),type="spline")
  ## comp 1 ## 
  C1.op.m.a = auc(B.r,C1.op.m,from = min(B.r), to = max(B.r),type="spline")
  C1.op.l.a = auc(B.r,C1.op.l,from = min(B.r), to = max(B.r),type="spline")
  C1.op.u.a = auc(B.r,C1.op.u,from = min(B.r), to = max(B.r),type="spline")
  C2.op.m.a = auc(B.r,C2.op.m,from = min(B.r), to = max(B.r),type="spline")
  C2.op.l.a = auc(B.r,C2.op.l,from = min(B.r), to = max(B.r),type="spline")
  C2.op.u.a = auc(B.r,C2.op.u,from = min(B.r), to = max(B.r),type="spline")
  C3.op.m.a = auc(B.r,C3.op.m,from = min(B.r), to = max(B.r),type="spline")
  C3.op.l.a = auc(B.r,C3.op.l,from = min(B.r), to = max(B.r),type="spline")
  C3.op.u.a = auc(B.r,C3.op.u,from = min(B.r), to = max(B.r),type="spline")
  C4.op.m.a = auc(B.r,C4.op.m,from = min(B.r), to = max(B.r),type="spline")
  C4.op.l.a = auc(B.r,C4.op.l,from = min(B.r), to = max(B.r),type="spline")
  C4.op.u.a = auc(B.r,C4.op.u,from = min(B.r), to = max(B.r),type="spline")
  C5.op.m.a = auc(B.r,C5.op.m,from = min(B.r), to = max(B.r),type="spline")
  C5.op.l.a = auc(B.r,C5.op.l,from = min(B.r), to = max(B.r),type="spline")
  C5.op.u.a = auc(B.r,C5.op.u,from = min(B.r), to = max(B.r),type="spline")
  C6.op.m.a = auc(B.r,C6.op.m,from = min(B.r), to = max(B.r),type="spline")
  C6.op.l.a = auc(B.r,C6.op.l,from = min(B.r), to = max(B.r),type="spline")
  C6.op.u.a = auc(B.r,C6.op.u,from = min(B.r), to = max(B.r),type="spline")
  ## % contribution ## 
  TC = matrix(nrow = n.comp,ncol=3)
  TC[1,1] <- C1.op.m.a/mod.a.m
  TC[1,2] <- C1.op.l.a/mod.a.l #l
  TC[1,3] <- C1.op.u.a/mod.a.u #u
  TC[2,1] <- C2.op.m.a/mod.a.m
  TC[2,2] <- C2.op.l.a/mod.a.l #l
  TC[2,3] <- C2.op.u.a/mod.a.u #u
  TC[3,1] <- C3.op.m.a/mod.a.m
  TC[3,2] <- C3.op.l.a/mod.a.l #l
  TC[3,3] <- C3.op.u.a/mod.a.u #u
  TC[4,1] <- C3.op.m.a/mod.a.m
  TC[4,2] <- C3.op.l.a/mod.a.l #l
  TC[4,3] <- C3.op.u.a/mod.a.u #u
  TC[5,1] <- C3.op.m.a/mod.a.m
  TC[5,2] <- C3.op.l.a/mod.a.l #l
  TC[5,3] <- C3.op.u.a/mod.a.u #u
  TC[6,1] <- C3.op.m.a/mod.a.m
  TC[6,2] <- C3.op.l.a/mod.a.l #l
  TC[6,3] <- C3.op.u.a/mod.a.u #u
}

##### actual plot ###### 
error.plot = function(){
plot(B.r,H.u,type="n",col="black",lwd=2,
     xlim=c(0,max(B.r)),ylim=c(0,max(max(H.u)+(0.05*max(H.u)),max(G.model.op.u))),
     axes = F,
     xlab = 
       if(input$scale == "log"){paste("log B (",input$B.units,")")} 
     else({paste("B (",input$B.units,")")}), 
     ylab = if(input$scale == "log"){
       c("dM/dlog(B)")
     }
     else({c("dM/dB")})) 
  
rect(0,max(max(H.u)+(0.05*max(H.u)),max(G.model.op.u)),max(B.r),0)
axis(1,pos = 0)
axis(2,pos = 0)

points(B,G,type="p",pch=19,col="grey")

lines(B.r,H.l,type="n",col="black",lwd=2) 
polygon(c(B.r,rev(B.r)),c(H.l,rev(H.u)),col="grey")
lines(B.r,H_mean,type="n",col="black",pch=20)

if(n.comp == 1){
  ##comp 1 ##
lines(B.r,C1.op.m,col="blue",lwd=2)
lines(B.r,C1.op.u,col="blue",lwd=2,type="n")
lines(B.r,C1.op.l,col="blue",lwd=2,type="n")
polygon(c(B.r,rev(B.r)),c(C1.op.l,rev(C1.op.u)),col="blue")
}
if(n.comp == 2){
  ##comp 1 ##
  lines(B.r,C1.op.m,col="blue",lwd=2)
  lines(B.r,C1.op.u,col="blue",lwd=2,type="n")
  lines(B.r,C1.op.l,col="blue",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C1.op.l,rev(C1.op.u)),col="blue")
  ##comp 2 ##
  lines(B.r,C2.op.m,col="purple",lwd=2)
  lines(B.r,C2.op.u,col="purple",lwd=2,type="n")
  lines(B.r,C2.op.l,col="purple",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C2.op.l,rev(C2.op.u)),col="purple")
}
if(n.comp == 3){
  ##comp 1 ##
  lines(B.r,C1.op.m,col="blue",lwd=2)
  lines(B.r,C1.op.u,col="blue",lwd=2,type="n")
  lines(B.r,C1.op.l,col="blue",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C1.op.l,rev(C1.op.u)),col="blue")
  ##comp 2 ##
  lines(B.r,C2.op.m,col="purple",lwd=2)
  lines(B.r,C2.op.u,col="purple",lwd=2,type="n")
  lines(B.r,C2.op.l,col="purple",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C2.op.l,rev(C2.op.u)),col="purple")
  ##comp 3 ##
  lines(B.r,C3.op.m,col="green",lwd=2)
  lines(B.r,C3.op.u,col="green",lwd=2,type="n")
  lines(B.r,C3.op.l,col="green",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C3.op.l,rev(C3.op.u)),col="green")
}
if(n.comp == 4){
  ##comp 1 ##
  lines(B.r,C1.op.m,col="blue",lwd=2)
  lines(B.r,C1.op.u,col="blue",lwd=2,type="n")
  lines(B.r,C1.op.l,col="blue",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C1.op.l,rev(C1.op.u)),col="blue")
  ##comp 2 ##
  lines(B.r,C2.op.m,col="purple",lwd=2)
  lines(B.r,C2.op.u,col="purple",lwd=2,type="n")
  lines(B.r,C2.op.l,col="purple",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C2.op.l,rev(C2.op.u)),col="purple")
  ##comp 3 ##
  lines(B.r,C3.op.m,col="green",lwd=2)
  lines(B.r,C3.op.u,col="green",lwd=2,type="n")
  lines(B.r,C3.op.l,col="green",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C3.op.l,rev(C3.op.u)),col="green")
  ##comp 4 ##
  lines(B.r,C4.op.m,col="dark red",lwd=2)
  lines(B.r,C4.op.u,col="dark red",lwd=2,type="n")
  lines(B.r,C4.op.l,col="dark red",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C4.op.l,rev(C4.op.u)),col="dark red")
}
if(n.comp == 5){
  ##comp 1 ##
  lines(B.r,C1.op.m,col="blue",lwd=2)
  lines(B.r,C1.op.u,col="blue",lwd=2,type="n")
  lines(B.r,C1.op.l,col="blue",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C1.op.l,rev(C1.op.u)),col="blue")
  ##comp 2 ##
  lines(B.r,C2.op.m,col="purple",lwd=2)
  lines(B.r,C2.op.u,col="purple",lwd=2,type="n")
  lines(B.r,C2.op.l,col="purple",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C2.op.l,rev(C2.op.u)),col="purple")
  ##comp 3 ##
  lines(B.r,C3.op.m,col="green",lwd=2)
  lines(B.r,C3.op.u,col="green",lwd=2,type="n")
  lines(B.r,C3.op.l,col="green",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C3.op.l,rev(C3.op.u)),col="green")
  ##comp 4 ##
  lines(B.r,C4.op.m,col="dark red",lwd=2)
  lines(B.r,C4.op.u,col="dark red",lwd=2,type="n")
  lines(B.r,C4.op.l,col="dark red",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C4.op.l,rev(C4.op.u)),col="dark red")
  ##comp 5 ##
  lines(B.r,C5.op.m,col="dark green",lwd=2)
  lines(B.r,C5.op.u,col="dark green",lwd=2,type="n")
  lines(B.r,C5.op.l,col="dark green",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C5.op.l,rev(C5.op.u)),col="dark green")
}
if(n.comp == 6){
  ##comp 1 ##
  lines(B.r,C1.op.m,col="blue",lwd=2)
  lines(B.r,C1.op.u,col="blue",lwd=2,type="n")
  lines(B.r,C1.op.l,col="blue",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C1.op.l,rev(C1.op.u)),col="blue")
  ##comp 2 ##
  lines(B.r,C2.op.m,col="purple",lwd=2)
  lines(B.r,C2.op.u,col="purple",lwd=2,type="n")
  lines(B.r,C2.op.l,col="purple",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C2.op.l,rev(C2.op.u)),col="purple")
  ##comp 3 ##
  lines(B.r,C3.op.m,col="green",lwd=2)
  lines(B.r,C3.op.u,col="green",lwd=2,type="n")
  lines(B.r,C3.op.l,col="green",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C3.op.l,rev(C3.op.u)),col="green")
  ##comp 4 ##
  lines(B.r,C4.op.m,col="dark red",lwd=2)
  lines(B.r,C4.op.u,col="dark red",lwd=2,type="n")
  lines(B.r,C4.op.l,col="dark red",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C4.op.l,rev(C4.op.u)),col="dark red")
  ##comp 5 ##
  lines(B.r,C5.op.m,col="dark green",lwd=2)
  lines(B.r,C5.op.u,col="dark green",lwd=2,type="n")
  lines(B.r,C5.op.l,col="dark green",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C5.op.l,rev(C5.op.u)),col="dark green")
  ##comp 6 ##
  lines(B.r,C6.op.m,col="magenta",lwd=2)
  lines(B.r,C6.op.u,col="magenta",lwd=2,type="n")
  lines(B.r,C6.op.l,col="magenta",lwd=2,type="n")
  polygon(c(B.r,rev(B.r)),c(C6.op.l,rev(C6.op.u)),col="magenta")
}
lines(B.r,G.model.op.m,type="l",col="orange",lwd=2)
lines(B.r,G.model.op.u,type="n")
lines(B.r,G.model.op.l,type="n")
polygon(c(B.r,rev(B.r)),c(G.model.op.l,rev(G.model.op.u)),col="orange")

# in p.final column 1 is mean value and column 2 is sd
  } #closes function for error.plot

## print plot on webpage ## 
output$plot4 <- renderPlot({
  print(error.plot())
})

####
output$final.results <- renderPrint({
  final.results = matrix(nrow=10,ncol=n.comp)
  if(n.comp ==1){
    final.results[,1]<- c(p.final[1,1],p.final[1,2],p.final[2,1],p.final[2,2],p.final[3,1],p.final[3,2],p.final[4,1],p.final[4,2],TC[1,1],(abs(TC[1,1]-TC[1,2])+abs(TC[1,1]-TC[1,3])/2))                         
  }
  if(n.comp ==2){
    final.results[,1]<- c(p.final[1,1],p.final[1,2],p.final[2,1],p.final[2,2],p.final[3,1],p.final[3,2],p.final[4,1],p.final[4,2],TC[1,1],(abs(TC[1,1]-TC[1,2])+abs(TC[1,1]-TC[1,3])/2))
    final.results[,2]<- c(p.final[5,1],p.final[5,2],p.final[6,1],p.final[6,2],p.final[7,1],p.final[7,2],p.final[8,1],p.final[8,2],TC[2,1],(abs(TC[2,1]-TC[2,2])+abs(TC[2,1]-TC[2,3])/2))
  }
  if(n.comp ==3){
    final.results[,1]<- c(p.final[1,1],p.final[1,2],p.final[2,1],p.final[2,2],p.final[3,1],p.final[3,2],p.final[4,1],p.final[4,2],TC[1,1],(abs(TC[1,1]-TC[1,2])+abs(TC[1,1]-TC[1,3])/2))
    final.results[,2]<- c(p.final[5,1],p.final[5,2],p.final[6,1],p.final[6,2],p.final[7,1],p.final[7,2],p.final[8,1],p.final[8,2],TC[2,1],(abs(TC[2,1]-TC[2,2])+abs(TC[2,1]-TC[2,3])/2))
    final.results[,3]<- c(p.final[9,1],p.final[9,2],p.final[10,1],p.final[10,2],p.final[11,1],p.final[11,2],p.final[12,1],p.final[12,2],TC[3,1],(abs(TC[3,1]-TC[3,2])+abs(TC[3,1]-TC[3,3])/2))
  }
  if(n.comp ==4){
    final.results[,1]<- c(p.final[1,1],p.final[1,2],p.final[2,1],p.final[2,2],p.final[3,1],p.final[3,2],p.final[4,1],p.final[4,2],TC[1,1],(abs(TC[1,1]-TC[1,2])+abs(TC[1,1]-TC[1,3])/2))
    final.results[,2]<- c(p.final[5,1],p.final[5,2],p.final[6,1],p.final[6,2],p.final[7,1],p.final[7,2],p.final[8,1],p.final[8,2],TC[2,1],(abs(TC[2,1]-TC[2,2])+abs(TC[2,1]-TC[2,3])/2))
    final.results[,3]<- c(p.final[9,1],p.final[9,2],p.final[10,1],p.final[10,2],p.final[11,1],p.final[11,2],p.final[12,1],p.final[12,2],TC[3,1],(abs(TC[3,1]-TC[3,2])+abs(TC[3,1]-TC[3,3])/2))
    final.results[,4]<- c(p.final[13,1],p.final[13,2],p.final[14,1],p.final[14,2],p.final[15,1],p.final[15,2],p.final[16,1],p.final[16,2],TC[4,1],(abs(TC[4,1]-TC[4,2])+abs(TC[4,1]-TC[4,3])/2))
  }
  if(n.comp ==5){
    final.results[,1]<- c(p.final[1,1],p.final[1,2],p.final[2,1],p.final[2,2],p.final[3,1],p.final[3,2],p.final[4,1],p.final[4,2],TC[1,1],(abs(TC[1,1]-TC[1,2])+abs(TC[1,1]-TC[1,3])/2))
    final.results[,2]<- c(p.final[5,1],p.final[5,2],p.final[6,1],p.final[6,2],p.final[7,1],p.final[7,2],p.final[8,1],p.final[8,2],TC[2,1],(abs(TC[2,1]-TC[2,2])+abs(TC[2,1]-TC[2,3])/2))
    final.results[,3]<- c(p.final[9,1],p.final[9,2],p.final[10,1],p.final[10,2],p.final[11,1],p.final[11,2],p.final[12,1],p.final[12,2],TC[3,1],(abs(TC[3,1]-TC[3,2])+abs(TC[3,1]-TC[3,3])/2))
    final.results[,4]<- c(p.final[13,1],p.final[13,2],p.final[14,1],p.final[14,2],p.final[15,1],p.final[15,2],p.final[16,1],p.final[16,2],TC[4,1],(abs(TC[4,1]-TC[4,2])+abs(TC[4,1]-TC[4,3])/2))
    final.results[,5]<- c(p.final[17,1],p.final[17,2],p.final[18,1],p.final[18,2],p.final[19,1],p.final[19,2],p.final[20,1],p.final[20,2],TC[5,1],(abs(TC[5,1]-TC[5,2])+abs(TC[5,1]-TC[5,3])/2))
  }
  if(n.comp ==6){
    final.results[,1]<- c(p.final[1,1],p.final[1,2],p.final[2,1],p.final[2,2],p.final[3,1],p.final[3,2],p.final[4,1],p.final[4,2],TC[1,1],(abs(TC[1,1]-TC[1,2])+abs(TC[1,1]-TC[1,3])/2))
    final.results[,2]<- c(p.final[5,1],p.final[5,2],p.final[6,1],p.final[6,2],p.final[7,1],p.final[7,2],p.final[8,1],p.final[8,2],TC[2,1],(abs(TC[2,1]-TC[2,2])+abs(TC[2,1]-TC[2,3])/2))
    final.results[,3]<- c(p.final[9,1],p.final[9,2],p.final[10,1],p.final[10,2],p.final[11,1],p.final[11,2],p.final[12,1],p.final[12,2],TC[3,1],(abs(TC[3,1]-TC[3,2])+abs(TC[3,1]-TC[3,3])/2))
    final.results[,4]<- c(p.final[13,1],p.final[13,2],p.final[14,1],p.final[14,2],p.final[15,1],p.final[15,2],p.final[16,1],p.final[16,2],TC[4,1],(abs(TC[4,1]-TC[4,2])+abs(TC[4,1]-TC[4,3])/2))
    final.results[,5]<- c(p.final[17,1],p.final[17,2],p.final[18,1],p.final[18,2],p.final[19,1],p.final[19,2],p.final[20,1],p.final[20,2],TC[5,1],(abs(TC[5,1]-TC[5,2])+abs(TC[5,1]-TC[5,3])/2))
    final.results[,6]<- c(p.final[21,1],p.final[21,2],p.final[22,1],p.final[22,2],p.final[23,1],p.final[23,2],p.final[24,1],p.final[24,2],TC[6,1],(abs(TC[6,1]-TC[6,2])+abs(TC[6,1]-TC[6,3])/2))
  }
  rownames(final.results)= c("Bh","Bh.sd","DP","DP.sd","P","P.sd","S","S.sd","TC.mean","TC.sd")
  col.labels = c("component 1","component 2", "component 3", "component 4", "component 5", "component 6")
  colnames(final.results) = col.labels[1:n.comp]
 
  if(input$scale == "log"){
  extra.results = final.results
  extra.results[1:4,] <- 10^(extra.results[1:4,])
  output$extra.results <- renderPrint({
    extra.results
  })
 }
 
  output$export.data <- downloadHandler(
    filename = function(){
      paste(input$file.name,".csv")
    },
    content = function(file) {
      write.csv(final.results,file)
    }
  )
  output$export.extra.data <- downloadHandler(
    filename = function(){
      paste(input$file.name,".csv")
    },
    content = function(file) {
      write.csv(extra.results,file)
    }
  )
  final.results
  })

## export plot for saving ## 
output$export.png <- downloadHandler(
  filename = function(){
    paste(input$file.name,".png")
  },
  content = function(file){
    png(file)
    error.plot()
    dev.off()
  }
)
output$export.eps <- downloadHandler(
  filename = function(){
    paste(input$file.name,".eps")
  },
  content = function(file){
    setEPS(file)
    postscript(file)
    error.plot()
    dev.off()
  }
)

##############

} # this closes the if(calc.error == TRUE) statement 
    })
  })
#output$video <- renderText({
  #return('<iframe width="200" height="100" src="https://www.youtube.com/watch?v=ImaYMoTi2g8" frameborder="0" allowfullscreen></iframe>')
  #return(paste('<iframe style="height:400px; width:80%" src="',"https://www.youtube.com/watch?v=ImaYMoTi2g8", '"></iframe>', sep = ""))
#})

output$pdfviewer <- renderText({
  return(paste('<iframe style="height:800px; width:100%" src="',"maxunmix_user_manual.pdf", '"></iframe>', sep = ""))
})
})

