rankheatplot<-function(data,format="percentage", lab.plot="default.titles", color="color", title.name=NULL, cex=0.65, legend.treatment="FALSE", pos.treatment.label=c(-0.25,-0.4), pos.outcome.label=c(0.01,-0.4), asterisk="FALSE",show.numbers="TRUE") {
  mydata<-data
  nt<-dim(mydata)[1]			#number of treatments
  no<-dim(mydata)[2]-1 			#number of outcomes
  treatments<-mydata[,1]
  outcomes<-c(names(c(mydata[c(1:no+1)])))
  options(warn=-1)
  ########convert rank into percentage
  if(format=="rank"){
    mydata1<-matrix(nrow=nt, ncol=no+1)
    lengthoftreatment<-as.numeric(colSums(!is.na(mydata)))
    mydata1[,1]<-mydata[,1]
    for(j in 2:(no+1)){
      for(i in 1:nt){
        mydata1[i,j]<-(1-mydata[i,j]/lengthoftreatment[j])*100
      }
    }
    mydata<-mydata1
  }
  #################create the size of the circle########################################
  par(mar = c(10, 8, 2,0))
  
  if(lab.plot== "numbers"){
    factors = 1:nt
  }
  if(lab.plot== "vector"){
    factors = vector
  }
  if(lab.plot=="default.titles"){
    factors = mydata[,1]
  }
  circos.par(points.overflow.warning = FALSE)
  circos.initialize(factors = factors, xlim=c(0, 10))
  circos.trackPlotRegion(factors = factors, ylim = c(0, 100), track.height = 0.05, bg.border = NA, panel.fun = function(x, y) {circos.text(5, 100, facing = "bending", cex = cex, get.cell.meta.data("sector.index"))})
  
  if(no==1|no==2){
    for (i in 1 : no){
      circos.trackPlotRegion(factors, ylim = c(0,100),track.height = 0.3)
    }
  }
  if(no==3|no==4){
    for (i in 1 : no){
      circos.trackPlotRegion(factors, ylim = c(0,100),track.height = 0.17)
    }
  }
  if(no>=5 & no<7){
    for (i in 1 : no){
      circos.trackPlotRegion(factors, ylim = c(0,100),track.height = 0.11)
    }
  }
  if(no==7){
    for (i in 1 : no){
      circos.trackPlotRegion(factors, ylim = c(0,100),track.height = 0.09)
    }
  }
  if(no==8 | no==9){
    for (i in 1 : no){
      circos.trackPlotRegion(factors, ylim = c(0,100),track.height = 0.07)
    }
  }
  if(no>=10 & no<=13){
    for (i in 1 : no){
      circos.trackPlotRegion(factors, ylim = c(0,100),track.height = 0.05)
    }
  }
  
  ###############colors / grey / color.blind#############
  
  mydata2<-mydata[,c(1:no+1)]
  mydata2<-data.matrix(mydata2)
  if(color == "grey"){
    asterisk="TRUE"
    mycolors<-c("#000000","#080808","#101010","#181818","#282828","#303030", "#383838", "#404040","#484848", "#505050", "#585858", "#606060", "#686868", "#707070","#787878","#808080", "#888888", "#909090","#989898","#A0A0A0","#A8A8A8", "#B0B0B0","#B8B8B8", "#C0C0C0", "#C8C8C8", "#D0D0D0", "#D8D8D8","#E0E0E0","#E8E8E8","#F0F0F0","#F8F8F8","#FFFFFF")
    for(i in 1:nt){
      for(j in 1:no){
        start = get.cell.meta.data("cell.start.degree", factors[i], j+1)
        end = get.cell.meta.data("cell.end.degree", factors[i], j+1)
        top = get.cell.meta.data("cell.top.radius", factors[i], j+1)
        bottom = get.cell.meta.data("cell.bottom.radius", factors[i], j+1)
        
        if(is.na(mydata2[i,j])=="TRUE"){		#white
          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom, border = "black", col = "white")
          if(asterisk=="TRUE"){
            circos.text(5, 50, sector.index = factors[i], facing = "downward",track.index= j+1,labels="*",cex=1)		
            # circos.text(5, 50, sector.index = factors[i],facing = "downward", track.index= j+1,labels=mydata2[i,j],cex=0.5)
          }
        }else{	
          if(as.numeric(mydata2[i,j])<3.1){				#red
            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#000000")	
          }
          else{
            if(as.numeric(mydata2[i,j])>=3.1 & as.numeric(mydata2[i,j])<6.2){  
              draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#080808")	
            }
            else{
              if(as.numeric(mydata2[i,j])>=6.2 & as.numeric(mydata2[i,j])<9.3){   
                draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#101010")	
              }
              else{
                if(as.numeric(mydata2[i,j])>=9.3 & as.numeric(mydata2[i,j])<12.4){   
                  draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#181818")	
                }
                else{
                  if(as.numeric(mydata2[i,j])>=12.4 & as.numeric(mydata2[i,j])<15.5){   
                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#282828")	
                  }
                  else{
                    if(as.numeric(mydata2[i,j])>=15.5 & as.numeric(mydata2[i,j])<18.6){   
                      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#303030")	
                    }
                    else{
                      if(as.numeric(mydata2[i,j])>=18.6 & as.numeric(mydata2[i,j])<21.7){   
                        draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#383838")	
                      }
                      else{
                        if(as.numeric(mydata2[i,j])>=21.7 & as.numeric(mydata2[i,j])<24.8){   
                          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#404040")	
                        }
                        else{
                          if(as.numeric(mydata2[i,j])>=24.8 & as.numeric(mydata2[i,j])<27.9){   
                            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#484848")	
                          }
                          else{
                            if(as.numeric(mydata2[i,j])>=27.9 & as.numeric(mydata2[i,j])<31){   
                              draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#505050")	
                            }
                            else{
                              if(as.numeric(mydata2[i,j])>=31 & as.numeric(mydata2[i,j])<34.1){   
                                draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#585858")	
                              }
                              else{
                                if(as.numeric(mydata2[i,j])>=34.1 & as.numeric(mydata2[i,j])<37.2){   
                                  draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#606060")	
                                }
                                else{
                                  if(as.numeric(mydata2[i,j])>=37.2 & as.numeric(mydata2[i,j])<40.3){   
                                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#686868")	
                                  }
                                  else{
                                    if(as.numeric(mydata2[i,j])>=40.3 & as.numeric(mydata2[i,j])<43.4){   
                                      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#707070")	
                                    }
                                    else{
                                      if(as.numeric(mydata2[i,j])>=43.4 & as.numeric(mydata2[i,j])<46.5){   
                                        draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#787878")	
                                      }
                                      else{
                                        if(as.numeric(mydata2[i,j])>=46.5 & as.numeric(mydata2[i,j])<49.6){   
                                          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#808080")	
                                        }
                                        else{
                                          if(as.numeric(mydata2[i,j])>=49.6 & as.numeric(mydata2[i,j])<52.7){   
                                            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#888888")	
                                          }
                                          else{
                                            if(as.numeric(mydata2[i,j])>=52.7 & as.numeric(mydata2[i,j])<55.8){   
                                              draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#909090")	
                                            }
                                            else{
                                              if(as.numeric(mydata2[i,j])>=55.8 & as.numeric(mydata2[i,j])<58.9){   
                                                draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#989898")	
                                              }
                                              else{
                                                if(as.numeric(mydata2[i,j])>=58.9 & as.numeric(mydata2[i,j])<62){   
                                                  draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#A0A0A0")	
                                                }
                                                else{
                                                  if(as.numeric(mydata2[i,j])>=62 & as.numeric(mydata2[i,j])<65.1){   
                                                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#A8A8A8")	
                                                  }
                                                  else{
                                                    if(as.numeric(mydata2[i,j])>=65.1 & as.numeric(mydata2[i,j])<68.2){   
                                                      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#B0B0B0")	
                                                    }
                                                    else{
                                                      if(as.numeric(mydata2[i,j])>=68.2 & as.numeric(mydata2[i,j])<71.3){   
                                                        draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#B8B8B8")	
                                                      }
                                                      else{
                                                        if(as.numeric(mydata2[i,j])>=71.3 & as.numeric(mydata2[i,j])<74.4){   
                                                          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#C0C0C0")	
                                                        }
                                                        else{
                                                          if(as.numeric(mydata2[i,j])>=74.4 & as.numeric(mydata2[i,j])<77.5){   
                                                            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#C8C8C8")	
                                                          }
                                                          else{
                                                            if(as.numeric(mydata2[i,j])>=77.5 & as.numeric(mydata2[i,j])<80.6){   
                                                              draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#D0D0D0")	
                                                            }
                                                            else{
                                                              if(as.numeric(mydata2[i,j])>=80.6 & as.numeric(mydata2[i,j])<83.7){   
                                                                draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#D8D8D8")	
                                                              }
                                                              else{
                                                                if(as.numeric(mydata2[i,j])>=83.7 & as.numeric(mydata2[i,j])<86.8){   
                                                                  draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#E0E0E0")	
                                                                }
                                                                else{
                                                                  if(as.numeric(mydata2[i,j])>=86.8 & as.numeric(mydata2[i,j])<89.9){   
                                                                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#E8E8E8")	
                                                                  }
                                                                  else{
                                                                    if(as.numeric(mydata2[i,j])>=89.9 & as.numeric(mydata2[i,j])<93){   
                                                                      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#F0F0F0")	
                                                                    }
                                                                    else{
                                                                      if(as.numeric(mydata2[i,j])>=93 & as.numeric(mydata2[i,j])<96.1){   
                                                                        draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#F8F8F8")	
                                                                      }
                                                                      else{
                                                                        if(as.numeric(mydata2[i,j])>=96.1){   
                                                                          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FFFFFF")	
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        if(show.numbers=="TRUE"){
          if(as.numeric(mydata2[i,j])<=60 && is.na(mydata2[i,j])!="TRUE" ){
            circos.text(5, 50, sector.index = factors[i],facing = "downward", track.index= j+1,col = "white",labels=mydata2[i,j],cex=0.5)
          }else{
            if(as.numeric(mydata2[i,j])>60 && is.na(mydata2[i,j])!="TRUE" ){
              circos.text(5, 50, sector.index = factors[i],facing = "downward", track.index= j+1,col = "black",labels=mydata2[i,j],cex=0.5)
            }
          }
        }
      }
    }
  }
  if(color == "color"){
    mycolors<-c("#FF0000","#FF1000","#FF2000","#FF3000","#FF4000","#FF5000","#FF6000","#FF7000","#FF8000","#FF9000","#FFA000","#FFB000","#FFC000","#FFD000","#FFE000","#FFF000","#FFFF00","#F0FF00","#E0FF00","#D0FF00","#C0FF00","#B0FF00","#A0FF00","#90FF00","#80FF00","#70FF00","#60FF00","#50FF00","#40FF00","#30FF00","#10ff10","#1BF110")
    
    for(i in 1:nt){
      for(j in 1:no){
        start = get.cell.meta.data("cell.start.degree", factors[i], j+1)
        end = get.cell.meta.data("cell.end.degree", factors[i], j+1)
        top = get.cell.meta.data("cell.top.radius", factors[i], j+1)
        bottom = get.cell.meta.data("cell.bottom.radius", factors[i], j+1)
      
        if(is.na(mydata2[i,j])=="TRUE"){		#white
          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom, border = "black", col = "white")
          if(asterisk=="TRUE"){
            circos.text(5, 50, sector.index = factors[i], facing = "downward",track.index= j+1,labels="*",cex=1)		
          }
        }else{	
          if(as.numeric(mydata2[i,j])<3.1){				
            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF0000")	
          }
          else{
            if(as.numeric(mydata2[i,j])>=3.1 & as.numeric(mydata2[i,j])<6.2){  
              draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF1000")	
            }
            else{
              if(as.numeric(mydata2[i,j])>=6.2 & as.numeric(mydata2[i,j])<9.3){   
                draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF2000")	
              }
              else{
                if(as.numeric(mydata2[i,j])>=9.3 & as.numeric(mydata2[i,j])<12.4){   
                  draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF3000")	
                }
                else{
                  if(as.numeric(mydata2[i,j])>=12.4 & as.numeric(mydata2[i,j])<15.5){   
                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF4000")	
                  }
                  else{
                    if(as.numeric(mydata2[i,j])>=15.5 & as.numeric(mydata2[i,j])<18.6){   
                      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF5000")	
                    }
                    else{
                      if(as.numeric(mydata2[i,j])>=18.6 & as.numeric(mydata2[i,j])<21.7){   
                        draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF6000")	
                      }
                      else{
                        if(as.numeric(mydata2[i,j])>=21.7 & as.numeric(mydata2[i,j])<24.8){   
                          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF7000")	
                        }
                        else{
                          if(as.numeric(mydata2[i,j])>=24.8 & as.numeric(mydata2[i,j])<27.9){   
                            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF8000")	
                          }
                          else{
                            if(as.numeric(mydata2[i,j])>=27.9 & as.numeric(mydata2[i,j])<31){   
                              draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FF9000")	
                            }
                            else{
                              if(as.numeric(mydata2[i,j])>=31 & as.numeric(mydata2[i,j])<34.1){   
                                draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FFA000")	
                              }
                              else{
                                if(as.numeric(mydata2[i,j])>=34.1 & as.numeric(mydata2[i,j])<37.2){   
                                  draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FFB000")	
                                }
                                else{
                                  if(as.numeric(mydata2[i,j])>=37.2 & as.numeric(mydata2[i,j])<40.3){   
                                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FFC000")	
                                  }
                                  else{
                                    if(as.numeric(mydata2[i,j])>=40.3 & as.numeric(mydata2[i,j])<43.4){   
                                      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FFD000")	
                                    }
                                    else{
                                      if(as.numeric(mydata2[i,j])>=43.4 & as.numeric(mydata2[i,j])<46.5){   
                                        draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FFE000")	
                                      }
                                      else{
                                        if(as.numeric(mydata2[i,j])>=46.5 & as.numeric(mydata2[i,j])<49.6){   
                                          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FFF000")	
                                        }
                                        else{
                                          if(as.numeric(mydata2[i,j])>=49.6 & as.numeric(mydata2[i,j])<52.7){   
                                            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#FFFF00")	
                                          }
                                          else{
                                            if(as.numeric(mydata2[i,j])>=52.7 & as.numeric(mydata2[i,j])<55.8){   
                                              draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#F0FF00")	
                                            }
                                            else{
                                              if(as.numeric(mydata2[i,j])>=55.8 & as.numeric(mydata2[i,j])<58.9){   
                                                draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#E0FF00")	
                                              }
                                              else{
                                                if(as.numeric(mydata2[i,j])>=58.9 & as.numeric(mydata2[i,j])<62){   
                                                  draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#D0FF00")	
                                                }
                                                else{
                                                  if(as.numeric(mydata2[i,j])>=62 & as.numeric(mydata2[i,j])<65.1){   
                                                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#C0FF00")	
                                                  }
                                                  else{
                                                    if(as.numeric(mydata2[i,j])>=65.1 & as.numeric(mydata2[i,j])<68.2){   
                                                      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#B0FF00")	
                                                    }
                                                    else{
                                                      if(as.numeric(mydata2[i,j])>=68.2 & as.numeric(mydata2[i,j])<71.3){   
                                                        draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#A0FF00")	
                                                      }
                                                      else{
                                                        if(as.numeric(mydata2[i,j])>=71.3 & as.numeric(mydata2[i,j])<74.4){   
                                                          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#90FF00")	
                                                        }
                                                        else{
                                                          if(as.numeric(mydata2[i,j])>=74.4 & as.numeric(mydata2[i,j])<77.5){   
                                                            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#80FF00")	
                                                          }
                                                          else{
                                                            if(as.numeric(mydata2[i,j])>=77.5 & as.numeric(mydata2[i,j])<80.6){   
                                                              draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#70FF00")	
                                                            }
                                                            else{
                                                              if(as.numeric(mydata2[i,j])>=80.6 & as.numeric(mydata2[i,j])<83.7){   
                                                                draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#60FF00")	
                                                              }
                                                              else{
                                                                if(as.numeric(mydata2[i,j])>=83.7 & as.numeric(mydata2[i,j])<86.8){   
                                                                  draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#50FF00")	
                                                                }
                                                                else{
                                                                  if(as.numeric(mydata2[i,j])>=86.8 & as.numeric(mydata2[i,j])<89.9){   
                                                                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#40FF00")	
                                                                  }
                                                                  else{
                                                                    if(as.numeric(mydata2[i,j])>=89.9 & as.numeric(mydata2[i,j])<93){   
                                                                      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#30FF00")	
                                                                    }
                                                                    else{
                                                                      if(as.numeric(mydata2[i,j])>=93 & as.numeric(mydata2[i,j])<96.1){   
                                                                        draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#20FF00")	
                                                                      }
                                                                      else{
                                                                        if(as.numeric(mydata2[i,j])>=96.1){   
                                                                          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#10FF00")	
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        if(show.numbers=="TRUE"){
          circos.text(5, 50, sector.index = factors[i],facing = "downward", track.index= j+1,labels=mydata2[i,j],cex=0.5)
        }
      }
    }
  }
  if(color == "color.blind"){
    mycolors<-c("#D73027", "#F46D43", "#FDAE61", "#FEE08B","#FFFF99","#A6D96A", "#66BD63", "#1A9850")
    for(i in 1:nt){
      for(j in 1:no){
        start = get.cell.meta.data("cell.start.degree", factors[i], j+1)
        end = get.cell.meta.data("cell.end.degree", factors[i], j+1)
        top = get.cell.meta.data("cell.top.radius", factors[i], j+1)
        bottom = get.cell.meta.data("cell.bottom.radius", factors[i], j+1)
        
        polar2cart<-function(x,y,dist,bearing,as.deg=FALSE){ 
          ## Translate Polar coordinates into Cartesian coordinates
          ## based on starting location, distance, and bearing
          ## as.deg indicates if the bearing is in degrees (T) or radians (F)
          
          if(as.deg){
            ##if bearing is in degrees, convert to radians
            bearing=bearing*pi/180
          }
          
          newx<-x+dist*sin(bearing)  ##X
          newy<-y+dist*cos(bearing)  ##Y
          return(list("x"=newx,"y"=newy))
        }
        topstart<-polar2cart(0,0,top,start,TRUE)
        bottomstart<-polar2cart(0,0,bottom,start,TRUE)
        topend<-polar2cart(0,0,top,end,TRUE)
        bottomend<-polar2cart(0,0,bottom,end,TRUE)
        
        ########################################################	
        
        if(is.na(mydata2[i,j])=="TRUE"){		#white
          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col = "white")
          if(asterisk=="TRUE"){
            circos.text(5, 9, sector.index = factors[i], facing = "inside",track.index= j+1,labels="*",cex=1)			
          }
        }
        else{	
          if(as.numeric(mydata2[i,j])<12.5){		
            draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#D73027")
            polygon(y=c(topstart$x,bottomstart$x,topend$x,bottomend$x),x=c(topstart$y,bottomstart$y,topend$y,bottomend$y),density = 50)
          }
          else{
            if(as.numeric(mydata2[i,j])>=12.5 & as.numeric(mydata2[i,j])<25){   
              draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col ="#F46D43")
              polygon(y=c(topstart$x,bottomstart$x,topend$x,bottomend$x),x=c(topstart$y,bottomstart$y,topend$y,bottomend$y))  
            }
            else{
              if(as.numeric(mydata2[i,j])>=25 & as.numeric(mydata2[i,j])<37.5){	
                draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col =  "#FDAE61" )		
                polygon(y=c(topstart$x,bottomstart$x,topend$x,bottomend$x),x=c(topstart$y,bottomstart$y,topend$y,bottomend$y),lwd=2,lty="dotted")
              }
              else{
                if(as.numeric(mydata2[i,j])>=37.5 & as.numeric(mydata2[i,j])<50){ 	
                  draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col = "#FEE08B")	
                  polygon(y=c(topstart$x,topend$x,bottomend$x,bottomstart$x,topstart$x),x=c(topstart$y,topend$y,bottomend$y,bottomstart$y,topstart$y),density = 40,lty="dotted") 						
                }
                else{
                  if(as.numeric(mydata2[i,j])>=50 & as.numeric(mydata2[i,j])<62.5){	
                    draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col = "#FFFF99")	
                    polygon(y=c(topstart$x,topend$x,bottomend$x,bottomstart$x,topstart$x),x=c(topstart$y,topend$y,bottomend$y,bottomstart$y,topstart$y),density = 10) 	
                  }	
                  else{
                    if(as.numeric(mydata2[i,j])>=62.5 & as.numeric(mydata2[i,j])<75){  
                      draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col = "#A6D96A")	
                      polygon(y=c(topstart$x,topend$x,bottomend$x,bottomstart$x,topstart$x),x=c(topstart$y,topend$y,bottomend$y,bottomstart$y,topstart$y),density = 50)
                    }
                    else{
                      if(as.numeric(mydata2[i,j])>=75 & as.numeric(mydata2[i,j])<87.5){	
                        draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col = "#66BD63")	
                        polygon(y=c(topstart$x,topend$x,bottomend$x,bottomstart$x,topstart$x),x=c(topstart$y,topend$y,bottomend$y,bottomstart$y,topstart$y),density = 10,lwd=2)
                      }
                      else{
                        if(as.numeric(mydata2[i,j])>=87.5 & as.numeric(mydata2[i,j])<100){ 	
                          draw.sector(start.degree = start, end.degree = end, rou1 = top, rou2=bottom,border = "black", col = "#1A9850")	
                          polygon(y=c(topstart$x,topend$x,bottomend$x,bottomstart$x,topstart$x),x=c(topstart$y,topend$y,bottomend$y,bottomstart$y,topstart$y),density = 30,lwd=2)																									
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  ############create the vector for labels########################
  names.treatments<-matrix(nrow=nt,ncol=1)
  for(i in 1:nt){
    names.treatments[i]<-paste(factors[i] , ":", treatments[i])
  }
  names.outcomes<-matrix(nrow=no,ncol=1)
  letters<-letters[1:no]
  for(i in 1:no){
    names.outcomes[i]<-paste(letters[i] , ":", outcomes[i])
  }
  if(asterisk==TRUE){
    names.outcomes[no+1]<-paste("** White sectors including a ë*í  refer to treatments")
    names.outcomes[no+2]<-paste("without data on the outcome within the circle **")
  }
  if(asterisk==FALSE){
    names.outcomes[no+1]<-paste("** White sectors refer to treatments")
    names.outcomes[no+2]<-paste("without data on the outcome within the circle **")
  }
  ################################################################
  ###############legends in the plot##################
  par(xpd=TRUE)
  title(main=title.name)
  legend("bottomright", inset=pos.outcome.label,legend=names.outcomes ,title="Circles from outside in refer to :", names.outcomes, cex=cex)
  if(legend.treatment=="TRUE"){
    legend("bottomright",inset=pos.treatment.label,title="Treatment", legend=names.treatments, cex=cex )
  }
  
  ############scale############################
  if(color=="color.blind"){		
    par(new=TRUE, plt=c(0,1,0,1), mar = c(9, 15, 17,5), usr=c(0,1,0,1))
    breaks <- as.matrix(c(12.5,12.5,12.5,12.5,12.5,12.5,12.5,12.5))
    barplot(breaks,horiz = TRUE, col=mycolors ,beside = FALSE, xlim = c(0, 100), ylim = c(0.5,10))
    polygon(y=c(0.2,0.2,1.2,1.2),x=c(12.5,0,12.5,0),density = 50)
    polygon(y=c(0.2,0.2,1.2,1.2),x=c(25,12.5,25,12.5))    	
    polygon(y=c(0.2,0.2,1.15,1.15),x=c(37.5,25,37.5,25),lwd=2,lty="dotted")
    polygon(y=c(0.2,1.2,1.2,0.2,0.2),x=c(50,50,37.5,37.5,50),density = 40,lty="dotted") 
    polygon(y=c(0.2,1.2,1.2,0.2,0.2),x=c(62.5,62.5,50,50,62.5),density = 10)
    polygon(y=c(0.2,1.2,1.2,0.2,0.2),x=c(75,75,62.5,62.5,75),density = 50)
    polygon(y=c(0.2,1.15,1.15,0.2,0.2),x=c(87.5,87.5,75,75,87.5),density = 10,lwd=2)
    polygon(y=c(0.2,1.15,1.15,0.2,0.2),x=c(100,100,87.5,87.5,100),density = 30,lwd=2)
  }
  if(color=="color"|color=="grey"){
    brks<-c(0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,100)
    image.plot(breaks=brks ,legend.mar = 9,legend.shrink= 0.5,legend.only=TRUE, zlim=c(0,100),col=mycolors,horizontal=TRUE)
  }
  
}