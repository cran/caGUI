##################################f################
##                                              ##
## caGUI: A tcl/tk Interface to the ca package  ##
## Written by Angelos Markos; Mar 2010	        ##
##               amarkos@uom.gr                 ##
##################################################

"caGUI" <- function()
{
  require(tcltk2) || stop("Package tcltk2 is required")
  require(ca) || stop("Package ca is required")
  tclRequire("BWidget")

  #Load ca data sets
	data(smoke)
	data(author)
	data(wg93)

#
# Main dialog window with title
#
  env<-environment()
  tt <- tktoplevel()

#  asw <- round(as.numeric(tkwinfo("screenwidth",tt)))
#  ash <- round(as.numeric(tkwinfo("screenheight",tt)))
#  if (asw/ash <= 1080/720) {gsw <- min(1080, round(asw * 0.9))
#        gsh <- min(720, round(ash/1080 * 720 * 0.9))
#    }
#    else {
#        gsw <- min(1080, round(ash/720 * 1080 * 0.9))
#        gsh <- min(720, round(ash * 0.9))
#    }
  
#  tkwm.geometry(tt, paste("345x470", "+", round(asw/2 - 
#                345/2, 0), "+", round(ash/2 - 
#                470/2, 0), sep = ""))
        
  tkwm.title(tt,"caGUI")
  tkwm.resizable(tt, 0, 0)
#  tkgrab.set(tt)  
  Rico <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin",
      "R.exe"), res = "R")
  tk2ico.set(tt, Rico)
  tk2ico.destroy(Rico)
  rm(Rico)
  tkwm.deiconify(tt)

	frame1 <- tkframe(tt, relief="groove", borderwidth=1, background="darkgreen")

	for (i in 1:length(.libPaths())) {
		icnfnameR <- file.path(paste(.libPaths()[i],"/caGUI/statcount.gif",sep=""))
		if (file.exists(icnfnameR)) {
			icn <- tkimage.create("photo", file=icnfnameR)
			Rlabel <- tklabel(frame1, image=icn, background="darkgreen")
			oldopt <- options("htmlhelp")$htmlhelp
			tkbind(Rlabel, "<Button-1>", function() help.start())
			options("htmlhelp" = oldopt)
      tit <- tklabel(frame1,text="caGUI - A GUI for the ca package", font=tkfont.create(size=13,weight="bold"), foreground="white", background="darkgreen")
  		tkgrid(Rlabel, tit, padx=5, sticky = "w")
 		}
 #	else if 
 #		{
 #     tit <- tklabel(frame1,text="caGUI - A GUI for the ca package", font=tkfont.create(size=13,weight="bold"), foreground="white", background="darkgreen")
 # 		tkgrid(tit, padx=5, sticky = "w")
 #		}
  }
	tkpack(frame1, expand="TRUE",fill="x")

     
# FONTS

#  fontHeading <- tkfont.create(family="times",size=100,weight="bold")        # tkgrid(tklabel(tt,text="A Nice Big Font for the Heading",font=fontHeading))
#  rb <- tclVar("local")

  
# Variables for text fields - ca
  dsvar <- tclVar()
  outputvar <- tclVar()
  nfvar <- tclVar()
  plot3dvar <- tclVar("0")
  rowsupvar <- tclVar()
  rowsupvarindex <- tclVar()
  colsupvar <- tclVar()
  colsupvarindex <- tclVar()
  rowsubvar <- tclVar()
  rowsubvarindex <- tclVar()
  colsubvar <- tclVar()
  colsubvarindex <- tclVar()
  otherVariable<-tclVar()

  lbl <- tclVar("both")
  contrib <- tclVar("none")
  rowmasslog <- FALSE
  colmasslog <- FALSE
  lbllog <- "2"
  rowmass <- tclVar("0")
  colmass <- tclVar("0")
  rowarrow <- tclVar("0")
  colarrow <- tclVar("0")
  rowarrowlog <- FALSE
  colarrowlog <- FALSE
  rbValue <- tclVar("local")
  rb2Value <- tclVar("whitespace")
  rb3Value <- tclVar("period")
  map <- tclVar("symmetric")
  sumvar <- tclVar("full")
  rowwhat <- tclVar("all")
  colwhat <- tclVar("all")
  Axe1<-tclVar("1")
  Axe2<-tclVar("2")
  Axe3<-tclVar("3")
  Axe1num<- 1
  Axe2num<- 2
  Axe3num<- 3
  rpchchoice <- tclVar("16")
  cpchchoice <- tclVar("17")
  srpchchoice <- tclVar("1")
  scpchchoice <- tclVar("24")
  rpchchoicenum <- 16
  cpchchoicenum <- 17
  srpchchoicenum <- 1
  scpchchoicenum <- 24
  Rcol.row<-Rcol.row.tmp<-"black"
  Ccol.col<-Ccol.col.tmp<-"red"
  sfvar <- tclVar("0.00002")
  sfvarnum <- "0.00002"
  
# Variables for text fields - mjca
  dsvar2 <- tclVar()
  outputvar2 <- tclVar()
  nfvar2 <- tclVar()
  sumvar2 <- tclVar("full")
  rowsupvar2 <- tclVar()
  rowsupvar2 <- tclVar()
  rowsupvarindex2 <- tclVar()
  colsupvar2 <- tclVar()
  colsupvarindex2 <- tclVar()
  rowsubvar2 <- tclVar()
  rowsubvarindex2 <- tclVar()
  colsubvar2 <- tclVar()
  colsubvarindex2 <- tclVar()
  sepvar <- tclVar("")
	
  lbl2 <- tclVar("both")
  contrib2 <- tclVar("none")
  rowmasslog2 <- FALSE
  colmasslog2 <- FALSE       
  centroidlog <- FALSE
  centroid <- tclVar("0")
  lbllog2 <- "2"
  rowmass2 <- tclVar("0")
  colmass2 <- tclVar("0")
  rowarrow2 <- tclVar("0")
  colarrow2 <- tclVar("0")
  rowarrowlog2 <- FALSE
  colarrowlog2 <- FALSE
  map2 <- tclVar("symmetric")
  lambda <- tclVar("Indicator")
  rowwhat2 <- tclVar("none")
  colwhat2 <- tclVar("all")
  Axe11<-tclVar("1")
  Axe21<-tclVar("2")
  Axe1num1<- 1
  Axe2num2<- 2
  rpchchoice2 <- tclVar("16")
  cpchchoice2 <- tclVar("17")
  srpchchoice2 <- tclVar("1")
  scpchchoice2 <- tclVar("24")
  rpchchoicenum2 <- 16
  cpchchoicenum2 <- 17
  srpchchoicenum2 <- 1
  scpchchoicenum2 <- 24
  Rcol.row2<-Rcol.row.tmp2<-"black"
  Ccol.col2<-Ccol.col.tmp2<-"red"

 # Tabs  
  nb <- tk2notebook(tt, tabs = c("Simple CA", "Multiple and Joint CA"))
  tkpack(nb, fill = "both", expand = 1)
  tb1 <- tk2notetab(nb, "Simple CA")
  tb2 <- tk2notetab(nb, "Multiple and Joint CA")
  tk2notetab.select(nb, "Simple CA")
  tk2notetab.text(nb) 
     
#### Simple Correspondence Analysis #### 
                           
# Title
#
  TFrame <- tkframe(tb1,relief="groove")

  # I/O, Supplementary, Subset
  IOFrame <- tkframe(tb1, borderwidth=0,relief="groove")
  
  tkgrid(tklabel(IOFrame,text="- Input & Output -", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")), columnspan=5)
  ds.entry <- tkentry(IOFrame, textvariable=dsvar)
  output.entry <- tkentry(IOFrame, textvariable=outputvar)
  tkinsert(output.entry, "end", "res")
  dfnr.label <- tklabel(IOFrame)
  dfnc.label <- tklabel(IOFrame)
	
  choosedf.but <- tkbutton(IOFrame, text="Select", command=function() SelectDataSet(ds.entry, dfnr.label, dfnc.label))
  choosedfHlp.but <- tkbutton(IOFrame,text="?",command=function() tkmessageBox(title="Help",message="Read a data set from command line.",icon="info",type="ok"))
	readds.but <- tkbutton(IOFrame, text="Read", command=function() ReadDataSet(ds.entry, dfnr.label, dfnc.label))
  readdfHlp.but <- tkbutton(IOFrame,text="?",command=function() tkmessageBox(title="Help",message="Read a data set from a file.",icon="info",type="ok"))
  tkgrid(tklabel(IOFrame,text="Input data set : "), ds.entry, choosedf.but, choosedfHlp.but, readds.but, readdfHlp.but, dfnr.label, dfnc.label, sticky="w")
  tkgrid(tklabel(IOFrame,text="Output object name : "), output.entry, sticky="w")

  tkgrid(tklabel(IOFrame,text="    ")) # Blank line

  tkgrid(tklabel(IOFrame,text="- Supplementary -", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")), columnspan=5)
  
  rowsup.entry <- tkentry(IOFrame, textvariable=rowsupvar)
  rowsup.label <- tklabel(IOFrame, width=4)
  ChooseRowSup.but <- tkbutton(IOFrame, text="Select", command=function() OnRowSup(ds.entry,ChooseRowSup.but,rowsupvar,rowsupvarindex))
  RowSupHlp.but <- tkbutton(IOFrame,text="?",command=function() tkmessageBox(title="Help",message="Indices of supplementary rows.",icon="info",type="ok"))
  tkgrid(tklabel(IOFrame,text="Supplementary Rows : "), rowsup.entry, ChooseRowSup.but,RowSupHlp.but, rowsup.label, sticky="w")
	
  colsup.entry <- tkentry(IOFrame, textvariable=colsupvar)
  colsup.label <- tklabel(IOFrame, width=4)
  ChooseColSup.but <- tkbutton(IOFrame, text="Select", command=function() OnColSup(ds.entry,ChooseColSup.but,colsupvar,colsupvarindex))
  ColSupHlp.but <- tkbutton(IOFrame,text="?",command=function() tkmessageBox(title="Help",message="Indices of supplementary columns.",icon="info",type="ok"))
  tkgrid(tklabel(IOFrame,text="Supplementary Columns : "), colsup.entry, ChooseColSup.but, ColSupHlp.but,colsup.label,sticky="w")
 
  tkgrid(tklabel(IOFrame,text="    ")) # Blank line
 
  tkgrid(tklabel(IOFrame,text="- Subsets -", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")), columnspan=5)
  
  rowsub.entry <- tkentry(IOFrame, textvariable=rowsubvar)
  rowsub.label <- tklabel(IOFrame, width=4)
  ChooseRowSub.but <- tkbutton(IOFrame, text="Select", command=function() OnRowSub(ds.entry,ChooseRowSub.but,rowsubvar,rowsubvarindex))
  RowSubHlp.but <- tkbutton(IOFrame,text="?",command=function() tkmessageBox(title="Help",message="Row indices of subset.",icon="info",type="ok"))
  tkgrid(tklabel(IOFrame,text="Subset Rows : "), rowsub.entry, ChooseRowSub.but,RowSubHlp.but, rowsub.label, sticky="w")
	
  colsub.entry <- tkentry(IOFrame, textvariable=colsubvar)
  colsub.label <- tklabel(IOFrame, width=4)
  ChooseColSub.but <- tkbutton(IOFrame, text="Select", command=function() OnColSub(ds.entry,ChooseColSub.but,colsubvar,colsubvarindex))
  ColSubHlp.but <- tkbutton(IOFrame,text="?",command=function() tkmessageBox(title="Help",message="Column indices of subset.",icon="info",type="ok"))
	
  tkgrid(tklabel(IOFrame,text="Subset Columns : "), colsub.entry, ChooseColSub.but, ColSubHlp.but,colsub.label,sticky="w")

  tkgrid(IOFrame)  
  tkgrid(tklabel(tb1,text="    ")) # Blank line
 
  #NoFrame <- tkframe(tb1, relief="groove", borderwidth=2)  
# Main Options
  
  NAFrame <- tkframe(tb1, relief="groove", borderwidth=0)
  tkgrid(tklabel(NAFrame,text="        - Options -        ", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")), columnspan=4, sticky="we")
  nf.spin <- tkwidget(NAFrame, "SpinBox", textvariable = nfvar, editable = FALSE,  width="5", values = c("all", 1:1000),justify="right")
  DimHlp.but <- tkbutton(NAFrame, text="?", command=function() tkmessageBox(title="Help",message="Number of Dimensions to be included in the output; if empty, the maximum possible dimensions are included.",icon="info",type="ok"))
  tkgrid(tklabel(NAFrame,text="Number of Dimensions : "), columnspan=2, row=1, column=0,sticky="w")
  tkgrid(nf.spin, row=1, column=2,sticky="w")
  tkgrid(DimHlp.but, row=1, column=3,sticky="w")
  
  Axe.label<-tklabel(NAFrame,text="Plot Dimensions : ")
  Axe1.spin <-tkwidget(NAFrame, "SpinBox", textvariable = Axe1, editable = FALSE,  width="5", values = c(1:1000),justify="right")
  Axe2.spin <-tkwidget(NAFrame, "SpinBox", textvariable = Axe2, editable = FALSE,  width="5", values = c(1:1000),justify="right")
  Axe3.spin <-tkwidget(NAFrame, "SpinBox", textvariable = Axe3, editable = FALSE,  width="5", values = c(1:1000),justify="right")
 
  AxesHlp.but <- tkbutton(NAFrame, text="?", command=function() tkmessageBox(title="Help",message="Indicates the dimensions to plot on horizontal and vertical axes respectively.",icon="info",type="ok"))
  tkgrid(Axe.label, columnspan=2, row=2, column=0,sticky="w")
  tkgrid(Axe1.spin, row=2, column=2,sticky="w")
  tkgrid(Axe2.spin, row=2, column=3,sticky="w")
  tkgrid(AxesHlp.but, row=2, column=4,sticky="w")

  plot3d.check <- tkcheckbutton(NAFrame)
  tkconfigure(plot3d.check,variable=plot3dvar)
  ZAxisHlp.but <- tkbutton(NAFrame, text="?", command=function() tkmessageBox(title="Help",message="Indicates the dimension to plot on the depth axis (3D must must be checked).",icon="info",type="ok"))
  Td <- tklabel(NAFrame,text="Plot 3D :")
  T3d <- tklabel(NAFrame,text="3rd Axis:")

  tkgrid(Td,sticky="e",column=0,row=3)
  tkgrid(plot3d.check,sticky="w",column=1,row=3)
  tkgrid(T3d,sticky="w",column=2,row=3) 
  tkgrid(Axe3.spin,sticky="w",column=3,row=3)
  tkgrid(ZAxisHlp.but,sticky="w",column=4,row=3)
#  tkgrid(tklabel(NAFrame, text = "     "))
#  tkgrid(plot3dFrame)
  tkgrid(tklabel(NAFrame, text = "     "))
                                                                   
#  summary.check <- tkcheckbutton(NAFrame)
#  tkconfigure(summary.check,variable=sumvar)
# #tkgrid(tklabel(NAFrame, text="Show Summary : "), summary.check, sticky="w")
#  tkgrid(tklabel(NAFrame,text="    ")) # Blank line

  sumtypes <- c("full","brief","none")
  summary.cb <- tkwidget(NAFrame, "ComboBox", editable = FALSE, values = sumtypes) 
  tkconfigure(summary.cb, textvariable = sumvar, width="5")
#  sumHlp.but <- tkbutton(NAFrame, text="?", command=function() tkmessageBox(title="Help",message="Output type.",icon="info",type="ok"))
 # tkgrid()
  tkplace(tklabel(NAFrame, text="Numeric Output : "), relx = 0.25, rely = 0.81, anchor = "sw")
  tkplace(summary.cb, relx = 0.75, rely = 0.81, anchor = "sw")
  
  tkgrid(tklabel(NAFrame, text = "     "))
  tkgrid(tklabel(NAFrame, text = "     "))
  Options.but <- tkbutton(NAFrame, text="Graphical Options", command=function() OnPlotCA())
  tkplace(Options.but, relx = 0.35, rely = 1.00, anchor = "sw")
 
  tkgrid(NAFrame)
  
#  tkgrid(tklabel(tb1,text="    ")) # Blank line


#### Multiple Correspondence Analysis #### 
     
# Title
#
TFrame2 <- tkframe(tb2, relief="groove")

# I/O, Supplementary, Subset
IOFrame2 <- tkframe(tb2, relief="groove", borderwidth=0)
tkgrid(tklabel(IOFrame2,text="- Input & Output -", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")), columnspan=5)
ds.entry2 <- tkentry(IOFrame2, textvariable=dsvar2)
output.entry2 <- tkentry(IOFrame2, textvariable=outputvar2)
tkinsert(output.entry2, "end", "res")
dfnr.label2 <- tklabel(IOFrame2, width=4)
dfnc.label2 <- tklabel(IOFrame2, width=4)
choosedf.but2 <- tkbutton(IOFrame2, text="Select", command=function() SelectDataSet(ds.entry2, dfnr.label2, dfnc.label2))
choosedfHlp.but2 <- tkbutton(IOFrame2,text="?",command=function() tkmessageBox(title="Help",message="The input data set.",icon="info",type="ok"))
readds.but2 <- tkbutton(IOFrame2, text="Read", command=function() ReadDataSet(ds.entry2, dfnr.label2, dfnc.label2))
readdfHlp.but2 <- tkbutton(IOFrame2,text="?",command=function() tkmessageBox(title="Help",message="Read a data set from a file.",icon="info",type="ok"))

tkgrid(tklabel(IOFrame2,text="Input data set : "), ds.entry2, choosedf.but2, choosedfHlp.but2, readds.but2, readdfHlp.but2,dfnr.label2, dfnc.label2, sticky="w")
	
tkgrid(tklabel(IOFrame2,text="Output object name : "), output.entry2, sticky="w")

tkgrid(tklabel(IOFrame2,text="    ")) # Blank line
   
tkgrid(tklabel(IOFrame2,text="- Supplementary -", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")),columnspan=5)
  
colsup.entry2 <- tkentry(IOFrame2, textvariable=colsupvar2)
colsup.label2 <- tklabel(IOFrame2, width=4)
ChooseColSup.but2 <- tkbutton(IOFrame2, text="Select", command=function() OnColSup(ds.entry2,ChooseColSup.but2,colsupvar2,colsupvarindex2))
ColSupHlp.but2 <- tkbutton(IOFrame2,text="?",command=function() tkmessageBox(title="Help",message="Indices of supplementary columns.",icon="info",type="ok"))
  
tkgrid(tklabel(IOFrame2,text="Supplementary Columns : "), colsup.entry2, ChooseColSup.but2, ColSupHlp.but2,colsup.label2,sticky="w")
  
tkgrid(tklabel(IOFrame2,text="    ")) # Blank line

tkgrid(tklabel(IOFrame2,text="- Subsets -", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")), columnspan=5)
  
colsub.entry2 <- tkentry(IOFrame2, textvariable=colsubvar2)
colsub.label2 <- tklabel(IOFrame2, width=4)
ChooseColSub.but2 <- tkbutton(IOFrame2, text="Select", command=function() OnColSub(ds.entry2,ChooseColSub.but2,colsubvar2,colsubvarindex2))
ColSubHlp.but2 <- tkbutton(IOFrame2,text="?",command=function() tkmessageBox(title="Help",message="Column indices of subset.",icon="info",type="ok"))

tkgrid(tklabel(IOFrame2,text="Subset Columns : "), colsub.entry2, ChooseColSub.but2, ColSubHlp.but2,colsub.label2,sticky="w")
  
tkgrid(IOFrame2)

tkgrid(tklabel(tb2,text="    ")) # Blank line
tkgrid(tklabel(tb2,text="    ")) # Blank line  
# Main Options
NAFrame2 <- tkframe(tb2, relief="groove", borderwidth=0)
tkgrid(tklabel(NAFrame2,text="        - Options -        ", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")), columnspan=4, sticky="we")

#MFrame <- tkframe(NAFrame2, relief="groove")
# Combobox Lambda
items <- c("Indicator", "Burt", "Adjusted", "JCA")
cb <- tkwidget(NAFrame2, "ComboBox", editable = FALSE, values = items) 
tkconfigure(cb, textvariable = lambda, width=10,justify="center")
  
ComboHlp.but <- tkbutton(NAFrame2, text="?", command=function() tkmessageBox(title="Help",message="Scaling method. Using JCA results in a joint correspondence analysis using iterative adjusment of the Burt matrix in the solution space.",icon="info",type="ok"))

tkgrid(tklabel(NAFrame2,text="Scaling method : "), row=1, column=0,sticky="w") 
tkgrid(cb, row=1, columnspan=2,column=1,sticky="w")
tkgrid(ComboHlp.but,row=1,column=2,sticky="e",pady=7) 
#tkgrid(tklabel(NAFrame2,text="   ")) # Blank line
 
nf.spin2 <- tkwidget(NAFrame2, "SpinBox", textvariable = nfvar2, editable = FALSE,  width="5", values = c("all", 1:1000),justify="right")
DimHlp.but2 <- tkbutton(NAFrame2, text="?", command=function() tkmessageBox(title="Help",message="Number of dimensions to be included in the output; if empty, the maximum possible dimensions are included.",icon="info",type="ok"))

tkgrid(tklabel(NAFrame2,text="Number of Dimensions : "), row=2, column=0,sticky="w")
tkgrid(nf.spin2, row=2, column=1,sticky="w")
tkgrid(DimHlp.but2, row=2, column=2,sticky="w",pady=7)
 
    
Axe.label2<-tklabel(NAFrame2,text="Plot Dimensions : ")
Axe1.spin2 <-tkwidget(NAFrame2, "SpinBox", textvariable = Axe11, editable = FALSE,  width="5", values = c(1:1000),justify="right")
Axe2.spin2 <-tkwidget(NAFrame2, "SpinBox", textvariable = Axe21, editable = FALSE,  width="5", values = c(1:1000),justify="right")
AxesHlp.but2 <- tkbutton(NAFrame2, text="?", command=function() tkmessageBox(title="Help",message="Indicates the dimensions to plot on horizontal and vertical axes respectively.",icon="info",type="ok"))  

tkgrid(Axe.label2,Axe1.spin2, Axe2.spin2, AxesHlp.but2,sticky="w")
  

 # sep.entry <- tkentry(NAFrame2, textvariable = sepvar, width="5", state="normal")
 # SepHlp.but <- tkbutton(NAFrame2, text="?", command=function() tkmessageBox(title="Help",message="Seperator used for combining variable and category names.",icon="info",type="ok"))
 # tkgrid(tklabel(NAFrame2,text="Separator : "), sep.entry, SepHlp.but, sticky="w")
 # tclvalue(sep.entry) <- ""

#  summary.check2 <- tkcheckbutton(NAFrame2)
#  tkconfigure(summary.check2,variable=sumvar2)
#  SumHlp.but2 <- tkbutton(NAFrame2, text="?", command=function() tkmessageBox(title="Help",message="Specifies if a summary of the results will be produced.",icon="info",type="ok"))
#  tkgrid(tklabel(NAFrame2, text="Show Summary : "), summary.check2, sticky="w")


  sumtypes2 <- c("full","brief","none")
  summary.cb2 <- tkwidget(NAFrame2, "ComboBox", editable = FALSE, values = sumtypes2) 
  tkconfigure(summary.cb2, textvariable = sumvar2, width="5")
 # sumHlp2.but <- tkbutton(NAFrame2, text="?", command=function() tkmessageBox(title="Help",message="Output type.",icon="info",type="ok"))
  
  tkgrid(tklabel(NAFrame2, text="Numeric Output : "), summary.cb2, sticky="w",pady=8)

  Options.but2 <- tkbutton(NAFrame2, text="Graphical Options", command=function() OnPlotMJCA())
 # tkgrid(Options.but2, sticky="we")
 tkplace(Options.but2, relx = 0.35, rely = 1.00, anchor = "sw")
 tkgrid(tklabel(NAFrame2,text="   ")) # Blank line
 
  tkgrid(NAFrame2)  
                                                

################################
# Function to build the Graphical display options Window (CA)
################################
  OnPlotCA<-function()
    {	
   	  if (tclvalue(tkget(ds.entry))!='')  {
        obj<-(tclvalue(tkget(ds.entry)))
     } else {
         tkmessageBox(title="Error",message="Please select a data set.",icon="error",type="ok")
         tkfocus(tt)
         return(0)
      }
       
      PlotCAWin<-tktoplevel()
      tkwm.title(PlotCAWin, "Graphical Options")
      tkwm.resizable(PlotCAWin, 0, 0)
  #    tkwm.geometry(PlotCAWin, "-100+50")
       onPlotCAOK<-function()
      {
       assign("map", map, envir=env)
       assign("rowwhat", rowwhat, envir=env)
       assign("colwhat", colwhat, envir=env)
       assign("contrib", contrib, envir=env)
       assign("sumvar", sumvar, envir=env)
   

       assign("Rcol.row", Rcol.row.tmp, envir=env)
       assign("Ccol.col", Ccol.col.tmp, envir=env)
     
       if(tclvalue(rowmass)=="1") assign("rowmasslog", TRUE, envir=env)
       else assign("rowmasslog", FALSE, envir=env)
      
       if(tclvalue(colmass)=="1") assign("colmasslog", TRUE, envir=env)
       else assign("colmasslog", FALSE, envir=env)
            
       if(tclvalue(rowarrow)=="1") assign("rowarrowlog", TRUE, envir=env)
       else assign("rowarrowlog", FALSE, envir=env)
      
       if(tclvalue(colarrow)=="1") assign("colarrowlog", TRUE, envir=env)
       else assign("colarrowlog", FALSE, envir=env)
                 
       if(tclvalue(lbl)=="symbols only") assign("lbllog", 0, envir=env)
       else if(tclvalue(lbl)=="labels only") assign("lbllog", 1, envir=env)
       else assign("lbllog", 2, envir=env)
  
       tkdestroy(PlotCAWin)
       tkfocus(tt)
      }
      
      PlotCAFrame<-tkframe(PlotCAWin, borderwidth=2,relief="groove")
     
     	tkgrid(tklabel(PlotCAFrame,text="        - Specify Options -        ", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")), columnspan=5)
           
      # Combobox map
      map.label<-tklabel(PlotCAFrame,text="Map Type : ")
      maptypes <- c("symmetric","rowprincipal","colprincipal","symbiplot","rowgab","colgab","rowgreen","colgreen" )
      map.cb <- tkwidget(PlotCAFrame, "ComboBox", editable = FALSE, values = maptypes) 
      tkconfigure(map.cb, textvariable = map, width=12)
      CbMapHlp.but <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies the map type.",icon="info",type="ok"))
      tkgrid(map.label,map.cb,CbMapHlp.but,sticky='w')
    
      # Combobox contrib
      contrib.label<-tklabel(PlotCAFrame,text="Contributions : ")
      corctr <- c("none","absolute","relative")
      contrib.cb <- tkwidget(PlotCAFrame, "ComboBox", editable = FALSE, values = corctr)
      tkconfigure(contrib.cb, textvariable = contrib, width=12)
      CbContribHlp.but <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies if contributions (relative or absolute) should be represented by different colour intensities.",icon="info",type="ok"))
      tkgrid(contrib.label,contrib.cb,CbContribHlp.but,sticky='w')
      
      # Combobox Labels
      lab.label<-tklabel(PlotCAFrame,text="Labels : ")
      labcon <- c("symbols only","labels only","both")
      lab.cb <- tkwidget(PlotCAFrame, "ComboBox", editable = FALSE, values = labcon)
      tkconfigure(lab.cb, textvariable = lbl, width=12)
      CbLabHlp.but <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies if the plot should contain symbols only, labels only or both symbols and labels. The third option results in the symbols being plotted at the coordinates and the labels with an offset.",icon="info",type="ok"))
      tkgrid(lab.label,lab.cb,CbLabHlp.but,sticky='w')
  
      tkgrid(tklabel(PlotCAFrame, text=" "))
      tkgrid(tklabel(PlotCAFrame, text="  "),tklabel(PlotCAFrame,text="Rows"),tklabel(PlotCAFrame, text="Columns"),sticky="w")
      
      # Combobox what
      rowwhattypes <- colwhattypes <- c("all","active","passive","none")
      rowwhat.cb <- tkwidget(PlotCAFrame, "ComboBox", editable = FALSE, values = rowwhattypes) 
      colwhat.cb <- tkwidget(PlotCAFrame, "ComboBox", editable = FALSE, values = colwhattypes) 
      tkconfigure(rowwhat.cb, textvariable = rowwhat, width="9")
      tkconfigure(colwhat.cb, textvariable = colwhat, width="9")
      CbWhatHlp.but <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies contents of the plot (rows and/or columns).",icon="info",type="ok"))
      tkgrid(tklabel(PlotCAFrame, text="Points : "), rowwhat.cb, colwhat.cb, CbWhatHlp.but, sticky="w")
     
      rspinbox <- tkwidget(PlotCAFrame, "SpinBox", textvariable = rpchchoice, editable = FALSE, values = c(" ", "NA", 0:25),justify="right",width = 10)
      cspinbox <- tkwidget(PlotCAFrame, "SpinBox", textvariable = cpchchoice, editable = FALSE, values = c(" ", "NA", 0:25),justify="right",width = 10)
      srspinbox <- tkwidget(PlotCAFrame, "SpinBox", textvariable = srpchchoice, editable = FALSE, values = c(" ", "NA", 0:25),justify="right",width = 10)
      scspinbox <- tkwidget(PlotCAFrame, "SpinBox", textvariable = scpchchoice, editable = FALSE, values = c(" ", "NA", 0:25),justify="right",width = 10)
      apchHlp.but <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Symbol of active points. Some values: open circle=21, open diamond=23, open square=22, open triangle=24, solid circle=19, solid diamond=18, solid square=15, solid triangle=17, plus (+)=3, cross (x)=4, dot (.)= .",icon="info",type="ok"))
      spchHlp.but <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Symbol of supplementaty points.Some values: open circle=21, open diamond=23, open square=22, open triangle=24, solid circle=19, solid diamond=18, solid square=15, solid triangle=17, plus (+)=3, cross (x)=4, dot (.)= .",icon="info",type="ok"))
      tkgrid(tklabel(PlotCAFrame,text="Symbols of active : "),rspinbox,cspinbox,apchHlp.but, sticky="w")
      tkgrid(tklabel(PlotCAFrame,text="Symbols of supplementary : "),srspinbox,scspinbox,spchHlp.but, sticky="w")
                                                 
      rowmass.check <- tkcheckbutton(PlotCAFrame)
      colmass.check <- tkcheckbutton(PlotCAFrame)
      tkconfigure(rowmass.check,variable=rowmass)
      tkconfigure(colmass.check,variable=colmass)
      ChMassHlp.but <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Indicates if mass should be indicated with the radius of the points.",icon="info",type="ok"))
      tkgrid(tklabel(PlotCAFrame, text="Masses : "), rowmass.check, colmass.check,ChMassHlp.but ,sticky="w")
 
      rowarrow.check <- tkcheckbutton(PlotCAFrame)
      colarrow.check <- tkcheckbutton(PlotCAFrame)
      tkconfigure(rowarrow.check,variable=rowarrow)
      tkconfigure(colarrow.check,variable=colarrow)
      ChArrowHlp.but <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies if the plot should contain points (unchecked) or arrows (checked).",icon="info",type="ok"))
      tkgrid(tklabel(PlotCAFrame, text="Arrows : "),rowarrow.check,colarrow.check,ChArrowHlp.but,sticky="w")
   
      tkgrid(tklabel(PlotCAFrame, text = " "))
 
  
      Rcol.row.value <- Rcol.row
    
      canvas.row <- tkcanvas(PlotCAFrame,width="80",height="25",bg=Rcol.row.value)
      ChangeColor.row <- function()
      {
        Rcol.row.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.row.value,title="Select color"))
        if (nchar(Rcol.row.value)>0)
        {
          tkconfigure(canvas.row,bg=Rcol.row.value)
          assign("Rcol.row.tmp", Rcol.row.value, envir=env)
        }
      }
      ChangeColor.row.button <- tkbutton(PlotCAFrame,text="Change Color",command=ChangeColor.row)
      ColRowHlpBtn <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies the colours of row point symbols, by default black for rows.",icon="info",type="ok"))
      tkgrid(tklabel(PlotCAFrame, text="Color of row symbols : "),canvas.row,ChangeColor.row.button,ColRowHlpBtn, sticky="w")
  
      Ccol.col.value <- Ccol.col
      canvas.col <- tkcanvas(PlotCAFrame,width="80",height="25",bg=Ccol.col.value)
      ChangeColor.col <- function()
      {
        Ccol.col.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Ccol.col.value,title="Select color"))
        if (nchar(Ccol.col.value)>0)
        {
          tkconfigure(canvas.col,bg=Ccol.col.value)
          assign("Ccol.col.tmp", Ccol.col.value, envir=env)
        }
      }
 
      ChangeColor.col.button <- tkbutton(PlotCAFrame,text="Change Color",command=ChangeColor.col)
      ColColHlpBtn <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies the colours of column point symbols, by default red for columns.",icon="info",type="ok"))
      tkgrid(tklabel(PlotCAFrame, text="Color of column symbols : "),canvas.col,ChangeColor.col.button, ColColHlpBtn, sticky="w")
     
   
      tkgrid(tklabel(PlotCAFrame, text = " "))
 
      sfHlp.but <- tkbutton(PlotCAFrame, text="?", command=function() tkmessageBox(title="Help",message="A scaling factor for the volume of the 3d primitives.",icon="info",type="ok"))
    	sf.entry <- tkentry(PlotCAFrame, textvariable=sfvar,width="12",justify = "right")
      tkgrid(tklabel(PlotCAFrame, text="Scaling factor (for 3D) : "),sf.entry,sfHlp.but,sticky="w")
   
   
      tkgrid(tklabel(PlotCAFrame, text = " "))

      OKFrame <- tkframe(PlotCAWin, relief="groove")
	    PlotCAOK.but<-tkbutton(OKFrame,text="OK",width=16,command=function() onPlotCAOK())
   
      tkgrid(PlotCAFrame)
      tkgrid(tklabel(OKFrame, text = " "))
      tkgrid(PlotCAOK.but)
       
      tkgrid(OKFrame)
  }
 
 ################################
# Function to build the Graphical display options Window (MJCA)
################################ 
  OnPlotMJCA<-function()
    {	
       if (tclvalue(tkget(ds.entry2))!='')  {
         obj2<-(tclvalue(tkget(ds.entry2)))
     } else {
         tkmessageBox(title="Error",message="Please select a data set.",icon="error",type="ok")
       tkfocus(tt)
       return(0)
      }
      PlotMJCAWin<-tktoplevel()
      tkwm.title(PlotMJCAWin, "Graphical Options")
      tkwm.resizable(PlotMJCAWin, 0, 0)
  
    #  tkwm.geometry(PlotMJCAWin, "-100+50")
       onPlotMJCAOK<-function()
      {
       assign("map2", map2, envir=env)
       assign("contrib2", contrib2, envir=env)
       assign("rowwhat2", rowwhat2, envir=env)
       assign("colwhat2", colwhat2, envir=env)
       assign("sumvar2", sumvar2, envir=env)
   
     
       assign("Rcol.row2", Rcol.row.tmp2, envir=env)
       assign("Ccol.col2", Ccol.col.tmp2, envir=env)
     
       if(tclvalue(rowmass2)=="1") assign("rowmasslog2", TRUE, envir=env)
       else assign("rowmasslog2", FALSE, envir=env)
      
       if(tclvalue(colmass2)=="1") assign("colmasslog2", TRUE, envir=env)
       else assign("colmasslog2", FALSE, envir=env)
       
       if(tclvalue(centroid)=="1") assign("centroidlog", TRUE, envir=env)
       else assign("centroidlog", FALSE, envir=env) 
            
       if(tclvalue(rowarrow2)=="1") assign("rowarrowlog2", TRUE, envir=env)
       else assign("rowarrowlog", FALSE, envir=env)
      
       if(tclvalue(colarrow2)=="1") assign("colarrowlog2", TRUE, envir=env)
       else assign("colarrowlog2", FALSE, envir=env)
                 
       if(tclvalue(lbl2)=="symbols only") assign("lbllog2", 0, envir=env)
       else if(tclvalue(lbl2)=="labels only") assign("lbllog2", 1, envir=env)
       else assign("lbllog2", 2, envir=env)
  
       tkdestroy(PlotMJCAWin)
       tkfocus(tt)
      }
     
      PlotMJCAFrame<-tkframe(PlotMJCAWin, borderwidth=2,relief="groove")
     
     	tkgrid(tklabel(PlotMJCAFrame,text="        - Specify Options -        ", foreground="darkgreen",font=tkfont.create(size=9,weight="bold")), columnspan=5)

      # Combobox map
      map.label2<-tklabel(PlotMJCAFrame,text="Map Type : ")
      maptypes2 <- c("symmetric","rowprincipal","colprincipal","symbiplot","rowgab","colgab","rowgreen","colgreen" )
      map.cb2 <- tkwidget(PlotMJCAFrame, "ComboBox", editable = FALSE, values = maptypes2) 
      tkconfigure(map.cb2, textvariable = map2, width=12)
      CbMapHlp.but2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies the map type.",icon="info",type="ok"))
      tkgrid(map.label2,map.cb2,CbMapHlp.but2,sticky='w')
    
      # Combobox contrib
      contrib.label2<-tklabel(PlotMJCAFrame,text="Contributions : ")
      corctr2 <- c("none","absolute","relative")
      contrib.cb2 <- tkwidget(PlotMJCAFrame, "ComboBox", editable = FALSE, values = corctr2)
      tkconfigure(contrib.cb2, textvariable = contrib2, width=12)
      CbContribHlp.but2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies if contributions (relative or absolute) should be represented by different colour intensities.",icon="info",type="ok"))
      tkgrid(contrib.label2,contrib.cb2,CbContribHlp.but2,sticky='w')
      
      # Combobox Labels
      lab.label2<-tklabel(PlotMJCAFrame,text="Labels : ")
      labcon2 <- c("symbols only","labels only","both")
      lab.cb2 <- tkwidget(PlotMJCAFrame, "ComboBox", editable = FALSE, values = labcon2)
      tkconfigure(lab.cb2, textvariable = lbl2, width=12)
      CbLabHlp.but2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies if the plot should contain symbols only, labels only or both symbols and labels. The third option results in the symbols being plotted at the coordinates and the labels with an offset.",icon="info",type="ok"))
      tkgrid(lab.label2,lab.cb2,CbLabHlp.but2,sticky='w')
  
      tkgrid(tklabel(PlotMJCAFrame, text=" "))
      tkgrid(tklabel(PlotMJCAFrame, text="  "),tklabel(PlotMJCAFrame,text="Rows"),tklabel(PlotMJCAFrame, text="Columns"),sticky="w")
      
      # Combobox what
      rowwhattypes2 <- colwhattypes2 <- c("all","active","passive","none")
      rowwhat.cb2 <- tkwidget(PlotMJCAFrame, "ComboBox", editable = FALSE, values = rowwhattypes2) 
      colwhat.cb2 <- tkwidget(PlotMJCAFrame, "ComboBox", editable = FALSE, values = colwhattypes2) 
      tkconfigure(rowwhat.cb2, textvariable = rowwhat2, width="9")
      tkconfigure(colwhat.cb2, textvariable = colwhat2, width="9")
      CbWhatHlp.but2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies contents of the plot (rows and/or columns).",icon="info",type="ok"))
      tkgrid(tklabel(PlotMJCAFrame, text="Points : "), rowwhat.cb2, colwhat.cb2, CbWhatHlp.but2, sticky="w")
      
      rspinbox2 <- tkwidget(PlotMJCAFrame, "SpinBox", textvariable = rpchchoice2, editable = FALSE, values = c(" ", "NA", 0:25),justify="right",width = 10)
      cspinbox2 <- tkwidget(PlotMJCAFrame, "SpinBox", textvariable = cpchchoice2, editable = FALSE, values = c(" ", "NA", 0:25),justify="right",width = 10)
      srspinbox2 <- tkwidget(PlotMJCAFrame, "SpinBox", textvariable = srpchchoice2, editable = FALSE, values = c(" ", "NA", 0:25),justify="right",width = 10)
      scspinbox2 <- tkwidget(PlotMJCAFrame, "SpinBox", textvariable = scpchchoice2, editable = FALSE, values = c(" ", "NA", 0:25),justify="right",width = 10)
      apchHlp.but2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Symbol of active points. Some values: open circle=21, open diamond=23, open square=22, open triangle=24, solid circle=19, solid diamond=18, solid square=15, solid triangle=17, plus (+)=3, cross (x)=4, dot (.)= .",icon="info",type="ok"))
      spchHlp.but2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Symbol of supplementaty points.Some values: open circle=21, open diamond=23, open square=22, open triangle=24, solid circle=19, solid diamond=18, solid square=15, solid triangle=17, plus (+)=3, cross (x)=4, dot (.)= .",icon="info",type="ok"))
      tkgrid(tklabel(PlotMJCAFrame,text="Symbols of active : "),rspinbox2,cspinbox2,apchHlp.but2, sticky="w")
      tkgrid(tklabel(PlotMJCAFrame,text="Symbols of supplementary : "),srspinbox2,scspinbox2,spchHlp.but2, sticky="w")
      
      rowmass.check2 <- tkcheckbutton(PlotMJCAFrame)
      colmass.check2 <- tkcheckbutton(PlotMJCAFrame)
      tkconfigure(rowmass.check2,variable=rowmass2)
      tkconfigure(colmass.check2,variable=colmass2)
      ChMassHlp.but2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Indicates if mass should be indicated with the radius of the points.",icon="info",type="ok"))
  
      tkgrid(tklabel(PlotMJCAFrame, text="Masses : "), rowmass.check2, colmass.check2, ChMassHlp.but2, sticky="w")
      rowarrow.check2 <- tkcheckbutton(PlotMJCAFrame)
      colarrow.check2 <- tkcheckbutton(PlotMJCAFrame)
      tkconfigure(rowarrow.check2,variable=rowarrow2)
      tkconfigure(colarrow.check2,variable=colarrow2)
      ChArrowHlp.but2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies if the plot should contain points (unchecked) or arrows (checked).",icon="info",type="ok"))
      tkgrid(tklabel(PlotMJCAFrame, text="Arrows : ") , rowarrow.check2, colarrow.check2, ChArrowHlp.but2,sticky="w")

    # TODO:
    #  centroid.check <- tkcheckbutton(PlotMJCAFrame)
    #  tkconfigure(centroid.check,variable=centroid)
    #  centroid.lbl <- tklabel(PlotMJCAFrame, text="Centroids : ")
    #  tkgrid(centroid.lbl, centroid.check,sticky="w")
         
      tkgrid(tklabel(PlotMJCAFrame, text = " "))
  
      Rcol.row.value2 <- Rcol.row2
      canvas.row2 <- tkcanvas(PlotMJCAFrame,width="80",height="25",bg=Rcol.row.value2)
      ChangeColor.row2 <- function()
      {
        Rcol.row.value2<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.row.value2,title="Select color"))
        if (nchar(Rcol.row.value2)>0)
        {
          tkconfigure(canvas.row2,bg=Rcol.row.value2)
          assign("Rcol.row.tmp2", Rcol.row.value2, envir=env)
        }
      }
      ChangeColor.row.button2 <- tkbutton(PlotMJCAFrame,text="Change Color",command=ChangeColor.row2)
      ColRowHlpBtn2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies the colours of row point symbols, by default black for rows.",icon="info",type="ok"))
      tkgrid(tklabel(PlotMJCAFrame, text="Color of row symbols : "),canvas.row2,ChangeColor.row.button2,ColRowHlpBtn2, sticky="w")
  
      Ccol.col.value2 <- Ccol.col2
      canvas.col2 <- tkcanvas(PlotMJCAFrame,width="80",height="25",bg=Ccol.col.value2)
      ChangeColor.col2 <- function()
      {
        Ccol.col.value2<-tclvalue(tcl("tk_chooseColor",initialcolor=Ccol.col.value2,title="Select color"))
        if (nchar(Ccol.col.value2)>0)
        {
          tkconfigure(canvas.col2,bg=Ccol.col.value2)
          assign("Ccol.col.tmp2", Ccol.col.value2, envir=env)
        }
      }
 
      ChangeColor.col.button2 <- tkbutton(PlotMJCAFrame,text="Change Color",command=ChangeColor.col2)
      ColColHlpBtn2 <- tkbutton(PlotMJCAFrame, text="?", command=function() tkmessageBox(title="Help",message="Specifies the colours of column point symbols, by default red for columns.",icon="info",type="ok"))
      tkgrid(tklabel(PlotMJCAFrame, text="Color of column symbols : "),canvas.col2,ChangeColor.col.button2, ColColHlpBtn2, sticky="w")
  
      OKFrame2 <- tkframe(PlotMJCAWin, relief="groove")
  	
      PlotMJCAOK.but2<-tkbutton(OKFrame2,text="OK",width=16,command=function() onPlotMJCAOK())
   
      tkgrid(PlotMJCAFrame)
      tkgrid(tklabel(OKFrame2, text = " "))
      tkgrid(PlotMJCAOK.but2)
      tkgrid(OKFrame2)
    }

################################
# Function to build the command line from dialog widgets - MJCA
################################
  "build" <- function()
	{
	#
	# Check that the data set field is not empty and get its name
	#
 
		if (tclvalue(dsvar) != "") {
			obj  <- parse(text=tclvalue(dsvar))[[1]]
		} else {
			return(0)
		}
		
		if (tclvalue(nfvar) != "all") {
		  nf <- parse(text=tclvalue(nfvar))[[1]]
		} else {
      nf <- NA
    }
    
   	#Supplementary       
    if (tclvalue(rowsupvarindex) != "") {
	    rsupvindex <- parse(text=paste('c(',paste(unlist(strsplit(tclvalue(rowsupvarindex),split=" ")),collapse=",",sep=""),')',sep=""))[[1]]
		} else {
      rsupvindex <- NA
    }
    
    if (tclvalue(colsupvarindex) != "") {
      csupvindex <- parse(text=paste('c(',paste(unlist(strsplit(tclvalue(colsupvarindex),split=" ")),collapse=",",sep=""),')',sep=""))[[1]]
		} else {
      csupvindex <- NA
    }
    
    #Subset   	
  	if (tclvalue(rowsubvarindex) != "") {
     rsubvindex <- parse(text=paste('c(',paste(unlist(strsplit(tclvalue(rowsubvarindex),split=" ")),collapse=",",sep=""),')',sep=""))[[1]]
		#  rsubvindex <- parse(text=tclvalue(rowsubvarindex))[[1]]
		} else {
      rsubvindex <- NA
    }
    
    if (tclvalue(colsubvarindex) != "") {
      csubvindex <- parse(text=paste('c(',paste(unlist(strsplit(tclvalue(colsubvarindex),split=" ")),collapse=",",sep=""),')',sep=""))[[1]]
		} else {
      csubvindex <- NA
    }

   substitute(ca(obj = obj,nd = nf,suprow=rsupvindex,supcol=csupvindex,subsetrow=rsubvindex,subsetcol=csubvindex)) 
   }


################################
# Function to build the command line from dialog widgets - MJCA
################################
	"buildmjca" <- function()
	{
	#
	# Check that the data set field is not empty and get its name
	#
		if (tclvalue(dsvar2) != "") {
			obj2  <- parse(text=tclvalue(dsvar2))[[1]]
		} else {
      tkfocus(tt)
			return(0)
		}
		
		if (tclvalue(nfvar2) != "all") {
		  nf2 <- parse(text=tclvalue(nfvar2))[[1]]
		} else {
      nf2 <- NA
    }
 
    sepvar <- tclvalue(sepvar)
    	  
    # Supplementary
    if (tclvalue(colsupvarindex2) != "") {
	     csupvindex2 <- parse(text=paste('c(',paste(unlist(strsplit(tclvalue(colsupvarindex2),split=" ")),collapse=",",sep=""),')',sep=""))[[1]]
		} else {
      csupvindex2 <- NA
    }
    
    #Subset
  	if (tclvalue(colsubvarindex2) != "") {
       csubvindex2 <- parse(text=paste('c(',paste(unlist(strsplit(tclvalue(colsubvarindex2),split=" ")),collapse=",",sep=""),')',sep=""))[[1]]
		} else {
      csubvindex2 <- NA
    }
    
    lambda <- tclvalue(lambda)
                                       #subsetcol=csubvindex2 0.3.2  ps=sepvar
    substitute(mjca(obj = obj2,nd = nf2,lambda=lambda,supcol=csupvindex2)) 
	}
	
################################
# Function to reset all dialog elements to default values
################################
	"reset" <- function()
	{
	if (tk2notetab.text(nb) == "Simple CA") 
  {
    rbValue <- tclVar("local")
    rb2Value <- tclVar("whitespace")
    rb3Value <- tclVar("period")    
    tclvalue(dsvar)<-""
	  tclvalue(rowsupvar) <- ""
	  tclvalue(rowsupvarindex) <- ""
    tclvalue(colsupvar) <- ""
    tclvalue(colsupvarindex) <- ""
    tclvalue(rowsubvar) <- ""
    tclvalue(rowsubvarindex) <- ""
    tclvalue(colsubvar) <- "" 
    tclvalue(colsubvarindex) <- ""
		tclvalue(nfvar) <- "all"
	  rowVariable <- tclVar("0")
    colVariable <- tclVar("1")
    tkconfigure(dfnr.label, text="")
		tkconfigure(dfnc.label, text="")
	  tkconfigure(ChooseRowSup.but, fg="black",text="Select")
	  tkconfigure(ChooseColSup.but, fg="black",text="Select")
	  tkconfigure(ChooseRowSub.but, fg="black",text="Select")
	  tkconfigure(ChooseColSub.but, fg="black",text="Select")
    tclvalue(map) <- "symmetric"
    tclvalue(rowwhat) <- "all"
    tclvalue(colwhat) <- "all"
    tclvalue(sumvar) <- "full"
    tclvalue(contrib) <- "none"      
    tclvalue(lbl) <- "2"
    tclvalue(Axe1) <- "1"
    tclvalue(Axe2) <- "2"
    tclvalue(Axe3) <- "3"
    tclvalue(rpchchoice) <- "16"
    tclvalue(cpchchoice) <- "17"
    tclvalue(srpchchoice) <- "1"
    tclvalue(scpchchoice) <- "24"
    tclvalue(rowmass) <- "0"
    tclvalue(colmass) <- "0"
    tclvalue(rowarrow) <- "0"
    tclvalue(colarrow) <- "0"
    tclvalue(plot3dvar) <- "0"
    Rcol.row<-Rcol.row.tmp<-"black"
    Ccol.col<-Ccol.col.tmp<-"red"
    tclvalue(sfvar) <- "0.00002"
    tkfocus(tt)
  }
  ### MJCA
else if (tk2notetab.text(nb) == "Multiple and Joint CA")
{
  	tclvalue(dsvar2)<-""
	  tclvalue(rowsupvar2) <- ""
	  tclvalue(rowsupvarindex2) <- ""
    tclvalue(colsupvar2) <- ""
    tclvalue(colsupvarindex2) <- ""
    tclvalue(rowsubvar2) <- ""
    tclvalue(rowsubvarindex2) <- ""
    tclvalue(colsubvar2) <- "" 
    tclvalue(colsubvarindex2) <- ""
		tclvalue(nfvar2) <- "all"  
    tkconfigure(dfnr.label2, text="")
		tkconfigure(dfnc.label2, text="")
	  tkconfigure(ChooseColSup.but2, fg="black",text="Select")
	  tkconfigure(ChooseColSub.but2, fg="black",text="Select")
    tclvalue(lambda)<- "Indicator"
#    tkconfigure(cb, textvariable = lambda)
    tclvalue(map2) <- "symmetric"
    tclvalue(contrib2) <- "none"      
    tclvalue(lbl2) <- "2"
    tclvalue(Axe11) <- "1"
    tclvalue(Axe21) <- "2"
    tclvalue(rowmass2) <- "0"
    tclvalue(colmass2) <- "0"    
    tclvalue(rowarrow2) <- "0"
    tclvalue(colarrow2) <- "0"
    tclvalue(centroid) <- "0"
    tclvalue(sepvar) <- ""
    tclvalue(sumvar2) <- "full"
    tclvalue(rpchchoice2) <- "16"
    tclvalue(cpchchoice2) <- "17"
    tclvalue(srpchchoice2) <- "1"
    tclvalue(scpchchoice2) <- "24"
    Rcol.row2<-Rcol.row.tmp2<-"black"
    Ccol.col2<-Ccol.col.tmp2<-"red"
    tclvalue(rowwhat2) <- "none"
    tclvalue(colwhat2) <- "all"
    tkfocus(tt)
}    
  }


################################
# Function to reset all dialog elements to default values
################################
	
################################
# Function to launch computations
################################
 	"execca" <- function()
{
## Simple CA
if (tk2notetab.text(nb) == "Simple CA") 
{
		  if (tclvalue(tkget(ds.entry))!='')  {
        obj<-(tclvalue(tkget(ds.entry)))
     } else {
         tkmessageBox(title="Error",message="Please select a data set.",icon="error",type="ok")
         tkfocus(tt)
         return(0)
      }
  #
	# Check that the analysis name is not empty and get it
	#
    assign("Axe1num",as.integer(tclvalue(Axe1)),envir=env)
    assign("Axe2num",as.integer(tclvalue(Axe2)),envir=env)
    assign("Axe3num",as.integer(tclvalue(Axe3)),envir=env)

    assign("sfvarnum",as.numeric(tclvalue(sfvar)),envir=env)

    assign("rpchchoicenum",as.numeric(tclvalue(rpchchoice)),envir=env)
    assign("cpchchoicenum",as.numeric(tclvalue(cpchchoice)),envir=env)
    assign("srpchchoicenum",as.numeric(tclvalue(srpchchoice)),envir=env)
    assign("scpchchoicenum",as.numeric(tclvalue(scpchchoice)),envir=env)

    if (tclvalue(outputvar) == "") tkinsert(output.entry, "end", "res")
		outputname <- parse(text=paste("\"",tclvalue(outputvar)[[1]],"\"",sep=""))

	# Build and display the command line so that the user can check it
		cmd <- build()

		if (cmd == 0) {
       tkfocus(tt)
       return(0)
       }

	 # Echo the command line to the console
 		pr1 <- substr(options("prompt")$prompt, 1,2)
  	cat(eval(outputname), " <- ", deparse(cmd, width = 256), "\n", pr1, sep="")
	#
	# Execute ca command
	#
		resca <- eval.parent(cmd)
    assign(eval(outputname), resca, pos=1)
    eval(outputname)
    tkfocus(tt)
		if (tclvalue(sumvar) == "full") {
	    	cmd3 <- substitute(summary(cmd))
        pr3 <- substr(options("prompt")$prompt, 1,2)
      	cat(deparse(cmd3, width = 256), "\n", pr3, sep="")
        print(eval.parent(cmd3))
        tkfocus(tt)
 		 }
	  else if (tclvalue(sumvar) == "brief") {
	      print(eval.parent(cmd))
	      tkfocus(tt)
	  }
 
    map <- tclvalue(map)
    rowwhat <- tclvalue(rowwhat)
    colwhat <- tclvalue(colwhat)
    sumvar <- tclvalue(sumvar)
    contrib <- tclvalue(contrib)
  
if (tclvalue(plot3dvar) == "0") {
    cmd2 <- substitute(plot(cmd,dim=c(Axe1num,Axe2num),pch=c(rpchchoicenum,srpchchoicenum,cpchchoicenum,scpchchoicenum),map=map,what=c(rowwhat,colwhat),contrib=contrib,labels=lbllog,mass=c(rowmasslog,colmasslog),arrows=c(rowarrowlog,colarrowlog),col=c(Rcol.row.tmp,Ccol.col.tmp)))
} 
else {
    cmd2 <- substitute(plot3d.ca(cmd,dim=c(Axe1num,Axe2num,Axe3num),pch=c(rpchchoicenum,srpchchoicenum,cpchchoicenum,scpchchoicenum),map=map,what=c(rowwhat,colwhat),contrib=contrib,labels=lbllog,mass=c(rowmasslog,colmasslog),arrows=c(rowarrowlog,colarrowlog),col=c(Rcol.row.tmp,Ccol.col.tmp),labcol=c(Rcol.row.tmp,Ccol.col.tmp), sf=sfvarnum))
}  
 	 # Echo the command line to the console
 		pr2 <- substr(options("prompt")$prompt, 1,2)
  	cat(deparse(cmd2, width = 256), "\n", pr2, sep="")
	#
	# Execute plot command
	#
		eval.parent(cmd2)
		tkfocus(tt)
  
}

### MJCA
else if (tk2notetab.text(nb) == "Multiple and Joint CA")
{
 		  if (tclvalue(tkget(ds.entry2))!='')  {
        obj<-(tclvalue(tkget(ds.entry2)))
     } else {
         tkmessageBox(title="Error",message="Please select a data set.",icon="error",type="ok")
         tkfocus(tt)
         return(0)
      }
  #
	# Check that the analysis name is not empty and get it
	#
		if (tclvalue(outputvar2) == "") tkinsert(output.entry2, "end", "res")
		outputname2 <- parse(text=paste("\"",tclvalue(outputvar2)[[1]],"\"",sep=""))
	
    assign("rpchchoicenum2",as.numeric(tclvalue(rpchchoice2)),envir=env)
    assign("cpchchoicenum2",as.numeric(tclvalue(cpchchoice2)),envir=env)
    assign("srpchchoicenum2",as.numeric(tclvalue(srpchchoice2)),envir=env)
    assign("scpchchoicenum2",as.numeric(tclvalue(scpchchoice2)),envir=env)
	
	
    assign("Axe1num1",as.integer(tclvalue(Axe11)),envir=env)
    assign("Axe2num2",as.integer(tclvalue(Axe21)),envir=env)

  #
	# Build and display the command line so that the user can check it
	#
		cmdmjca <- buildmjca()
		if (cmdmjca == 0)
    {
     tkfocus(tt)
     return(0)
    }       
  # Echo the command line to the console
   pr2 <- substr(options("prompt")$prompt, 1,2)
	 cat(eval(outputname2), " <- ", deparse(cmdmjca, width = 256), "\n", pr2, sep="")
	#
	# Execute the command
	#
		resmjca <- eval.parent(cmdmjca)
		assign(eval(outputname2), resmjca, pos=1)
		eval(outputname2)

		if (tclvalue(sumvar2) == "full") {
	    	cmdmjca3 <- substitute(summary(cmdmjca))
        pr3 <- substr(options("prompt")$prompt, 1,2)
      	cat(deparse(cmdmjca3, width = 256), "\n", pr3, sep="")
        print(eval.parent(cmdmjca3))
 		 }
    else if (tclvalue(sumvar2) == "brief") {
	      print(eval.parent(cmdmjca))
	  }
    
    map2 <- tclvalue(map2)
    rowwhat2 <- tclvalue(rowwhat2)
    colwhat2 <- tclvalue(colwhat2)
    contrib2 <- tclvalue(contrib2)
    sumvar2 <- tclvalue(sumvar2)
    cmdmjca2 <- substitute(plot(cmdmjca,dim=c(Axe1num1,Axe2num2),pch=c(rpchchoicenum2,srpchchoicenum2,cpchchoicenum2,scpchchoicenum2),map=map2,centroids=cetroidlog,what=c(rowwhat2,colwhat2),contrib=contrib2,labels=lbllog2,mass=c(rowmasslog2,colmasslog2),arrows=c(rowarrowlog2,colarrowlog2),col=c(Rcol.row.tmp2,Ccol.col.tmp2)))
  
  	 # Echo the command line to the console
 		pr2 <- substr(options("prompt")$prompt, 1,2)
  	cat(deparse(cmdmjca2, width = 256), "\n", pr2, sep="")
	#
	# Execute plot command
	#
		eval.parent(cmdmjca2)
    tkfocus(tt)
}

	}
	
#
# Reset and Submit buttons
#
 # ntab <- as.numeric(tcl(nb, "index", "end"))
 
  RCSFrame <- tkframe(tt, relief="groove", borderwidth=2, background="white")
	reset.but <- tkbutton(RCSFrame, text="Reset", command=reset,foreground="white", background="darkgreen",font=tkfont.create(size=9,weight="bold"))
	submit.but <- tkbutton(RCSFrame, text="Run", default="active", command=function() execca(),foreground="white", background="darkgreen",font=tkfont.create(size=9,weight="bold"))
	cancel.but <- tkbutton(RCSFrame, text="Exit", command=function() tkdestroy(tt),foreground="white", background="darkgreen",font=tkfont.create(size=9,weight="bold"))
  tkpack(cancel.but, submit.but, reset.but, side="left", expand="TRUE", fill="x")  
  tkpack(RCSFrame, expand="TRUE", fill="x")
                                 
#	RCSFrame2 <- tkframe(tt, relief="groove",borderwidth=2, background="white")
#	reset.but2 <- tkbutton(RCSFrame2, text="Reset", command=resetmjca)
#	cancel.but2 <- tkbutton(RCSFrame2, text="Exit", command=function() tkdestroy(tt))
#	submit.but2 <- tkbutton(RCSFrame2, text="Run", default="active", command=function() execmjca())
#	tkgrid(cancel.but2, submit.but2, reset.but2, ipadx=20)	
#	tkgrid(RCSFrame2)
   
# If window is closed by user, terminate the dialog
#	tkbind(tt, "<KeyPress-Return>", function() execca())
	tkbind(tt, "<KeyPress-Escape>", function() tkdestroy(tt))
  tkfocus(tt)

"OnColSub" <- function(df.entry,ChooseColSub.but,colsubvar,colsubvarindex)
{
      env<-environment()
      if (tclvalue(tkget(df.entry))!='')
        obj<-(tclvalue(tkget(df.entry)))
      else
      {
        tkmessageBox(title="Error",message="Please select a data set.",icon="error",type="ok")
        tkfocus(tt)
        return(0)
      }

      vars<-colnames(get(obj))
      sublcol<-NULL
      sublcolindex<-NULL

      ColSubWin<-tktoplevel()
      tkwm.title(ColSubWin,"Select subset columns")
      #cr?ation de la fonction DOK.funct
      OK.fun<-function() 
      {
        vsup.select<-listvar.nom[as.numeric(tkcurselection(listvar))+1]
        vsup.select.index <- as.numeric(tkcurselection(listvar))+1 
        if(length(vsup.select)==0)
        {
          assign("sublcol", NULL, envir=env)
          assign("sublcolindex", NULL, envir=env)
    #      tclvalue(.DilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(ChooseColSub.but, fg="black",text="Select")
          tclvalue(colsubvar)<-""
          tclvalue(colsubvarindex)<-""
          tkdestroy(ColSubWin)
          tkfocus(tt)
          return(0)
        }
        assign("sublcol", vsup.select, envir=env)
        assign("sublcolindex", vsup.select.index, envir=env)
        tclvalue(colsubvar)<-sublcol
        tclvalue(colsubvarindex)<-sublcolindex
   #     tclvalue(.DilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(ChooseColSub.but, fg="blue",text="Modify")
        tkdestroy(ColSubWin)

      }

      # cr?ation et mise en page de la fenetre Dillu
      listvar<-tklistbox(ColSubWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrvar,...)) # Liste vide
      scrvar <-tkscrollbar(ColSubWin,repeatinterval=5,command=function(...)tkyview(listvar,...))
      listvar.nom<-NULL
      indice<-0


      for (i in (1:ncol(get(obj)))) {
 #         if (is.numeric(get(obj)[,i])) {
            tkinsert(listvar,"end",vars[i]) # On renseigne la liste
            listvar.nom<-c(listvar.nom,vars[i])
            if(vars[i] %in% sublcol) tkselection.set(listvar, indice)
            indice<-indice+1
#          }
      }

      OK.but<-tkbutton(ColSubWin, text="OK", width=16,command=OK.fun)

      tkgrid(tklabel(ColSubWin, text=""))
      tkgrid(tklabel(ColSubWin, text = "Select subset column(s)", fg = "darkgreen",font=tkfont.create(size=9,weight="bold")), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listvar, scrvar, sticky = "nw")
      tkgrid.configure(scrvar, sticky = "ens", columnspan=1)
      tkgrid.configure(listvar, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(ColSubWin, text=""))
      tkgrid(OK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(ColSubWin, text=""))
      tkgrid.columnconfigure(ColSubWin,0, minsize=25)
      tkgrid.columnconfigure(ColSubWin,2, minsize=25)

  }
  
"OnColSup" <- function(df.entry,ChooseColSup.but,colsupvar,colsupvarindex)
{
      env<-environment()
      if (tclvalue(tkget(df.entry))!='')
        obj<-(tclvalue(tkget(df.entry)))
      else
      {
        tkmessageBox(title="Error",message="Please select a data set.",icon="error",type="ok")
        tkfocus(tt)
        return(0)
      }

      vars<-colnames(get(obj))
      suplcol<-NULL
      suplcolindex<-NULL
      
      ColSupWin<-tktoplevel()
      tkwm.title(ColSupWin,"Select supplementary columns")
      #cr?ation de la fonction DOK.funct
      OK.fun<-function()
      {
        vsup.select<-listvar.nom[as.numeric(tkcurselection(listvar))+1]
        vsup.select.index <- as.numeric(tkcurselection(listvar))+1 
        if(length(vsup.select)==0)
        {
          assign("suplcol", NULL, envir=env)
          assign("suplcolindex", NULL, envir=env)
    #      tclvalue(.DilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(ChooseColSup.but, fg="black",text="Select")
          tclvalue(colsupvar)<-""
          tclvalue(colsupvarindex)<-""
          tkdestroy(ColSupWin)
          tkfocus(tt)
          return(0)
        }
        assign("suplcol", vsup.select, envir=env)
        assign("suplcolindex", vsup.select.index, envir=env)
        tclvalue(colsupvar)<-suplcol
        tclvalue(colsupvarindex) <- suplcolindex
   #     tclvalue(.DilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(ChooseColSup.but, fg="blue",text="Modify")
        tkdestroy(ColSupWin)

      }

      listvar<-tklistbox(ColSupWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrvar,...)) # Liste vide
      scrvar <-tkscrollbar(ColSupWin,repeatinterval=5,command=function(...)tkyview(listvar,...))
      listvar.nom<-NULL
      indice<-0
      
      for (i in (1:ncol(get(obj)))) {
   #       if (is.numeric(get(obj)[,i])) {
            tkinsert(listvar,"end",vars[i]) # On renseigne la liste
            listvar.nom<-c(listvar.nom,vars[i])
            if(vars[i] %in% suplcol) tkselection.set(listvar, indice)
            indice<-indice+1
  #        }
      }
   
      OK.but<-tkbutton(ColSupWin, text="OK", width=16,command=OK.fun)

      tkgrid(tklabel(ColSupWin, text=""))
      tkgrid(tklabel(ColSupWin, text = "Select supplementary column(s)", fg = "darkgreen",font=tkfont.create(size=9,weight="bold")), column=1, columnspan = 1, sticky = "ew")
      tkgrid(listvar, scrvar, sticky = "nw")
      tkgrid.configure(scrvar, sticky = "ens", columnspan=1)
      tkgrid.configure(listvar, sticky = "ew", column=1, columnspan=1)
      tkgrid(tklabel(ColSupWin, text=""))
      tkgrid(OK.but, column=1,columnspan=1, sticky="ew")
      tkgrid(tklabel(ColSupWin, text=""))
      tkgrid.columnconfigure(ColSupWin,0, minsize=25)
      tkgrid.columnconfigure(ColSupWin,2, minsize=25)

  }
"OnRowSub" <- function(df.entry,ChooseRowSub.but,rowsubvar,rowsubvarindex)
{
      env<-environment()
      if (tclvalue(tkget(df.entry))!='')
        obj<-(tclvalue(tkget(df.entry)))
      else
      {
         tkmessageBox(title="Error",message="Please select a data set.",icon="error",type="ok")
         tkfocus(tt)
         return(0)
      }

      rows<-rownames(get(obj))
      sublrow<-NULL
      sublrowindex<-NULL

      RowSubWin<-tktoplevel()
      tkwm.title(RowSubWin,"Select subset rows")

      OK.fun<-function()
      {
        Ligne.select <- rows[as.numeric(tkcurselection(listLigne))+1]
        Ligne.select.index <- as.numeric(tkcurselection(listLigne))+1 
        if(length(Ligne.select)==0)
        {
          assign("sublrow", NULL, envir=env)
          assign("sublrowindex", NULL, envir=env)
        #  tclvalue(.LilluLabel)<-paste(firstLabel, "", sep=" ")
          tkconfigure(ChooseRowSub.but, fg="black",text="Select")
           tclvalue(rowsubvar)<-""
           tclvalue(rowsubvarindex)<-""
          tkdestroy(RowSubWin)
          tkfocus(tt)
          return()
        }
        assign("sublrow", Ligne.select, envir=env)
        assign("sublrowindex", Ligne.select.index, envir=env)
        tclvalue(rowsubvar) <- sublrow
        tclvalue(rowsubvarindex) <- sublrowindex

        #as.character(vnr[numi]))
#        tclvalue(.LilluLabel)<-paste(label, ": OK", sep=" ")
       # tclvalue(.LilluLabel)<-paste(label, "", sep=" ")
        tkconfigure(ChooseRowSub.but, fg="blue",text="Modify")
        tkdestroy(RowSubWin)

      }
      listLigne<-tklistbox(RowSubWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrLigne,...)) # Liste vide
      scrLigne <-tkscrollbar(RowSubWin,repeatinterval=5,command=function(...)tkyview(listLigne,...))
      indice<-0

      for (i in (1:nrow(get(obj))))
      {
  #     if (is.numeric(get(obj)[,i])) {
          tkinsert(listLigne,"end",rows[i]) # On renseigne la liste
          if(rows[i] %in% sublrow) tkselection.set(listLigne, indice)
          indice<-indice+1
 #       }
      }
        OK.but<-tkbutton(RowSubWin, text="OK", width=16,command=OK.fun)

        tkgrid(tklabel(RowSubWin, text=""))
        tkgrid(tklabel(RowSubWin, text = "Select subset row(s)", fg = "darkgrenn",font=tkfont.create(size=9,weight="bold")), column=1, columnspan = 1, sticky = "ew")
        tkgrid(listLigne, scrLigne, sticky = "nw")
        tkgrid.configure(scrLigne, sticky = "ens", columnspan=1)
        tkgrid.configure(listLigne, sticky = "ew", column=1, columnspan=1)
        tkgrid(tklabel(RowSubWin, text=""))
        tkgrid(OK.but, column=1,columnspan=1, sticky="ew")
        tkgrid(tklabel(RowSubWin, text=""))
        tkgrid.columnconfigure(RowSubWin,0, minsize=25)
        tkgrid.columnconfigure(RowSubWin,2, minsize=25)

  }
"OnRowSup" <- function(df.entry,ChooseRowSup.but,rowsupvar,rowsupvarindex)
{
      env<-environment()
      if (tclvalue(tkget(df.entry))!='')
        obj<-(tclvalue(tkget(df.entry)))
      else
      {
         tkmessageBox(title="Error",message="Please select a data set.",icon="error",type="ok")
         tkfocus(tt)
         return(0)
      }
    
      rows<-rownames(get(obj))
      suplrow<-NULL
      suplrowindex<-NULL
      
         
      RowSupWin<-tktoplevel()
      tkwm.title(RowSupWin,"Select supplementary rows")

      #cr?ation de la fonction LOK.funct
      OK.fun<-function()
      {
        Ligne.select <- rows[as.numeric(tkcurselection(listLigne))+1]
        Ligne.select.index<-as.numeric(tkcurselection(listLigne))+1
     #   Ligne.select.index <- as.numeric(tkcurselection(listLigne))+1 
        if(length(Ligne.select)==0)
        {
          assign("suplrow", NULL, envir=env)
          assign("suplrowindex", NULL, envir=env)
          tkconfigure(ChooseRowSup.but, fg="black",text="Select")
          tclvalue(rowsupvar)<-""
          tclvalue(rowsupvarindex)<-""
          tkdestroy(RowSupWin)
          tkfocus(tt)
          return()
        }
        assign("suplrow", Ligne.select, envir=env)
        assign("suplrowindex", Ligne.select.index, envir=env)
        tclvalue(rowsupvar)<-suplrow
        tclvalue(rowsupvarindex)<- suplrowindex
        tkconfigure(ChooseRowSup.but, fg="blue",text="Modify")
        tkdestroy(RowSupWin)
      
      }
      listLigne<-tklistbox(RowSupWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrLigne,...)) # Liste vide
      scrLigne <-tkscrollbar(RowSupWin,repeatinterval=5,command=function(...)tkyview(listLigne,...))
      indice<-0
      listfact.nom<-NULL
      for (i in (1:nrow(get(obj))))
      {
  #     if (is.numeric(get(obj)[,i])) {
          tkinsert(listLigne,"end",rows[i])
          listfact.nom <- c(listfact.nom,rows[i])
          if(rows[i] %in% suplrow) tkselection.set(listLigne, indice)
          indice<-indice+1
  #      }
      }

        OK.but<-tkbutton(RowSupWin, text="OK", width=16,command=OK.fun)

        tkgrid(tklabel(RowSupWin, text=""))
        tkgrid(tklabel(RowSupWin, text = "Select supplementary row(s)", fg = "darkgreen",font=tkfont.create(size=9,weight="bold")), column=1, columnspan = 1, sticky = "ew")
        tkgrid(listLigne, scrLigne, sticky = "nw")
        tkgrid.configure(scrLigne, sticky = "ens", columnspan=1)
        tkgrid.configure(listLigne, sticky = "ew", column=1, columnspan=1)
        tkgrid(tklabel(RowSupWin, text=""))
        tkgrid(OK.but, column=1,columnspan=1, sticky="ew")
        tkgrid(tklabel(RowSupWin, text=""))
        tkgrid.columnconfigure(RowSupWin,0, minsize=25)
        tkgrid.columnconfigure(RowSupWin,2, minsize=25)
        
  }

"is.valid.name" <- function(x) 
{
    length(x) == 1 && is.character(x) && x == make.names(x)
}

"listDataSets" <- function(envir = .GlobalEnv, ...) 
{
    Vars <- ls(envir = envir, all.names = TRUE)
    if (length(Vars) == 0) 
        return(Vars)
    names(which(sapply(Vars, function(.x) is.data.frame(get(.x, 
        envir = envir)))))
}

  
"ReadDataSet" <- function(ds.entry, dfnr.label, dfnc.label) {
  
   	vnr <- NULL
  	vnc <- NULL
  
    top<-tktoplevel(borderwidth = 10)
    tkwm.title(top, "Read Text Data From File, Clipboard, or URL")
    tkwm.resizable(top, 0, 0)

    optionsFrame <- tkframe(top)
    dsname <- tclVar("data")
    entryDsname <- ttkentry(optionsFrame, width="20", textvariable=dsname)

    rowVariable <- tclVar("0")
    colVariable <- tclVar("1")
    
    rowCheckBox <- tkcheckbutton(optionsFrame, variable=rowVariable)
    colCheckBox <- tkcheckbutton(optionsFrame, variable=colVariable)

    missingVariable <- tclVar("NA")
    missingEntry <- ttkentry(optionsFrame, width="8", textvariable=missingVariable)
                                              
    onOK <- function(){
        tkdestroy(top)
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
         tkmessageBox(title="Error",message="You must enter a name for the data set.",icon="error",type="ok")
         tkfocus(tt)
         return()
         }
        if (!is.valid.name(dsnameValue)){
         tkmessageBox(title="Error",message=paste('"', dsnameValue, '" ', "is not a valid name.",sep=" "),icon="error",type="ok")
           tkfocus(tt)
         return()
         }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, "Data set"))){
                ReadDataSet(ds.entry, dfnr.label, dfnc.label)
               tkfocus(tt) 
               return()
            }
            }
                 
 
      ext <- 0
      location <- tclvalue(rbValue)
      file <- if (location == "clipboard") "clipboard" 
			else if (location == "local") tclvalue(tkgetOpenFile(filetypes=
            	'{"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}} {"MS Excel file" {*.xls ".XLS" "*.xlsx" ".XLSX"}} {"All Files" {"*"}}'))	 
      else {
                subdialog <- tktoplevel(borderwidth=10)
                tkwm.title(subdialog, "Internet URL")
        		onOKsub <- function(){
        		    tkdestroy(subdialog)
      			    tkfocus(tt)
		    		}
	
      			urlFrame <- tkframe(subdialog)
    				urlVar <- tclVar("")
    				
          	urlabel <- tklabel(subdialog,text="Specify address:")
            url <- ttkentry(urlFrame, font="logFont", width="30", textvariable=urlVar)
	    			urlXscroll <- ttkscrollbar(urlFrame,
						orient="horizontal", command=function(...) tkxview(url, ...))
    				tkconfigure(url, xscrollcommand=function(...) tkset(urlXscroll, ...))
    		#		subOKCancelHelp()
    				tkgrid(url, sticky="w")
	   		 	  tkgrid(urlXscroll, sticky="ew")
    				tkgrid(urlFrame, sticky="nw")
	   		#	  tkgrid(subButtonsFrame, sticky="w")
    	   	 subOKbut <- tkbutton(subdialog, text = "OK",  command = onOKsub, width = 12)	
	     		 tkgrid(subOKbut, sticky="we")
   			   tkfocus(subdialog)
           tkwait.window(subdialog)
    				tclvalue(urlVar)
				}
        if (file == "") {
  #          if ("grab.focus") tkgrab.release(top)
            tkdestroy(top)
            return()
            }

    #TODO
    if (location == "local") {
       sop <- match(".", rev(strsplit(file, NULL)[[1]]))[1]
       ext <- tolower(substring(file, nchar(file) - sop + 2, nchar(file)))
     }
     if(ext == "xls" || ext == "xlsx"){
        if(!require(RODBC))
        stop("This function requires the RODBC package.\n")
       # close all databases in case of error
       on.exit(odbcCloseAll())
       channel <- switch(EXPR = ext, xls = odbcConnectExcel(file),xlsx = odbcConnectExcel2007(file))
       tabdat <- sqlTables(channel)
       names(tabdat) <- tolower(names(tabdat))
       tabname <- tabdat$table_name
       tabdat <- ifelse(tabdat$table_type =="TABLE",
       substring(tabname, 2, nchar(tabname) - 2),
       substring(tabname, 1, nchar(tabname) - 1))
    # if there are several tables
        if(length(tabdat)>1)
            fil <- tk_select.list(sort(tabdat),
            title = "Select")
          else
            fil <- tabdat
          if(fil == ""){
            tkmessageBox(title="Error",message="No table selected.",icon="error",type="ok")
            tkfocus(tt)
            return()
            }
          if(ext == "xls" || ext == "xlsx")
            fil <- paste("[", fil, "$]", sep = "")
          
    # Retrieve the data
    dat <- sqlQuery(channel = channel, query = paste("select * from", fil))
    names(dat)<- trim.blanks(names(dat))
    dat <- trim.col.na(dat)
    odbcCloseAll()
    assign(dsnameValue, as.data.frame(dat), envir = .GlobalEnv)
    command <- paste("sqlQuery(channel = ",channel,", select * from ", fil,")",
            sep = "")

    logger(paste(dsnameValue, " <- ", command, sep = ""))
    activeDataSet(dsnameValue)    
    
    tkdelete(ds.entry, 0, "end")
    tkinsert(ds.entry, "end", dsnameValue)
  
   }
   else {
        colhead <- tclvalue(colVariable)# == "1"
        assign("rowhead",as.integer(tclvalue(rowVariable)),envir=env)
        if (rowhead == 0) rowhead=NULL
     #   tclvalue(rowVariable) <- "1"
        rb1 <- tclvalue(rb2Value)
        del <- if (rb1 == "whitespace") ""
            else if (rb1 == "commas") ","
            else if (rb1 == "tabs") "\\t"
            else tclvalue(otherVariable)
    
        miss <- tclvalue(missingVariable)
        dec <- if (tclvalue(rb3Value) == "period") "." else ","

    command <- paste('read.table("', file,'", header=', colhead, ',row.names=', rowhead, ',sep="', del, '", na.strings="', miss, '", dec="', dec, '", strip.white=TRUE)', sep="")
    logger(paste(dsnameValue, " <- ", command, sep=""))
		result <- justDoIt(command)
		if (class(result)[1] !=  "try-error"){
        	assign(dsnameValue, result, envir=.GlobalEnv)
        	activeDataSet(dsnameValue)
			}
        
    tkdelete(ds.entry, 0, "end")
      tkinsert(ds.entry, "end", dsnameValue)
   }
        tkfocus(tt)
        }

 #    buttonsFrame <- tkframe(top)
      OKbutton <- tkbutton(optionsFrame, text = "OK", 
      width = "12", command = onOK, 
      default = "active", borderwidth = 3)

      onCancel <- function() {
        tkdestroy(top)
     #  tkfocus(tt())
      }

    cancelButton <- tkbutton(optionsFrame, text = "Cancel", width = "12", command = onCancel, borderwidth = 3)
 
  #  tkgrid(buttonsFrame, OKbutton, cancelButton)
  
    tkgrid(tklabel(optionsFrame, text="Enter name for data set:"), entryDsname, sticky="w")
    tkgrid(tklabel(optionsFrame,text="    ")) # Blank line
    
    tkgrid(tklabel(optionsFrame, text="Row labels in first column:"), rowCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Column labels in first row:"), colCheckBox, sticky="w")
 #   tkgrid(tklabel(optionsFrame,text="    ")) # Blank line
  
##    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Read data from clipboard:")), clipboardCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text="Missing data indicator:"), missingEntry, sticky="w",pady=4)
#  tkgrid(tklabel(optionsFrame,text="    ")) # Blank line

  tkgrid(tklabel(optionsFrame,text="Location of Data File", foreground="darkgreen"),sticky="w",pady=4)
  lfs <- tkradiobutton(optionsFrame)
  clp <- tkradiobutton(optionsFrame)
  iurl <- tkradiobutton(optionsFrame)

  tkconfigure(lfs,variable=rbValue,value="local")
  tkconfigure(clp,variable=rbValue,value="clipboard")
  tkconfigure(iurl,variable=rbValue,value="url")
  tkgrid(tklabel(optionsFrame,text="Local file system"),lfs,sticky="w")
  tkgrid(tklabel(optionsFrame,text="Clipboard"),clp,sticky="w")
  tkgrid(tklabel(optionsFrame,text="Internet URL"),iurl,sticky="w")

  delimiterFrame<-tkframe(optionsFrame)
 
  tkgrid(tklabel(delimiterFrame,text="Field Separator", foreground="darkgreen"),sticky="w",pady=4)

  wspace <- tkradiobutton(delimiterFrame)
  commas <- tkradiobutton(delimiterFrame)
  tabs <- tkradiobutton(delimiterFrame)
  other <- tkradiobutton(delimiterFrame)

  otherEntry <- ttkentry(delimiterFrame, width="4", textvariable=otherVariable)  
                              
  tkconfigure(wspace,variable=rb2Value,value="whitespace")
  tkconfigure(commas,variable=rb2Value,value="commas")
  tkconfigure(tabs,variable=rb2Value,value="tabs")
  tkconfigure(other,variable=rb2Value,value="other")
  tkgrid(tklabel(delimiterFrame,text="White space"),wspace,sticky="w")
  tkgrid(tklabel(delimiterFrame,text="Comma"),commas,sticky="w")
  tkgrid(tklabel(delimiterFrame,text="Tab"),tabs,sticky="w")
  tkgrid(tklabel(delimiterFrame,text="Other"),other,tklabel(optionsFrame,text="Specify :"),otherEntry,sticky="w")
            

  tkgrid(tklabel(delimiterFrame,text="    ")) # Blank line

  tkgrid(tklabel(optionsFrame,text="Decimal-point character", foreground="darkgreen"),sticky="w",pady=4)

  decperiod <- tkradiobutton(optionsFrame)
  deccomma <- tkradiobutton(optionsFrame)

  tkconfigure(decperiod,variable=rb3Value,value="period")
  tkconfigure(deccomma,variable=rb3Value,value="comma")
  tkgrid(tklabel(optionsFrame,text="Period [.]"),decperiod,sticky="w")
  tkgrid(tklabel(optionsFrame,text="Comma [,]"),deccomma,sticky="w")

  tkgrid(delimiterFrame, sticky="w", columnspan=2)

  tkgrid(OKbutton, cancelButton)
  tkgrid(optionsFrame, sticky="w")
  
 
#
# Put the row and column numbers of the dataframe in the corresponding labels
#
#	tkconfigure(dfnr.label, text=as.character(vnr))
#	tkconfigure(dfnc.label, text=as.character(vnc))

 }

"logger" <- function (command) {
#	lines <- strsplit(command, "\n")[[1]]
#		for (line in seq(along=lines)) {
#			prompt <- ifelse (line==1, paste("\n", "prefixes"[1], sep=""), paste("\n", "prefixes"[2], sep=""))
#			cat(paste(prompt, lines[line]))  ##rmh
#		cat("\n")                          ##rmh
#	}
	cat(command)
  command
}

"justDoIt" <- function(command) {
	capture.output(result <- try(eval(parse(text=command), envir=.GlobalEnv), silent=TRUE))
	if (class(result)[1] ==  "try-error"){
		return(result)
	}
	result
}

"trim.blanks"<-function (text) 
{
  gsub("^ *", "", gsub(" *$", "", text))
}

"checkReplace"<- function (name, type ="Variable") 
{

 tkmessageBox(title="Error",message = sprintf("%s %s already exists.\nOverwrite %s?",type, name, tolower(type)), icon = "warning", type = "yesno", 
 default = "no")
}

 
"activeDataSet" <- function (dsname) 
{
    if (!is.data.frame(ds <- get(dsname, envir = .GlobalEnv))) {
        command <- paste(dsname, " <- as.data.frame(", dsname, 
            ")", sep = "")
        justDoIt(command)
        logger(command)
    }
    varnames <- names(get(dsname, envir = .GlobalEnv))
    newnames <- make.names(varnames)
    badnames <- varnames != newnames
    if (any(badnames)) {
        command <- paste("names(", dsname, ") <- make.names(names(", 
            dsname, "))", sep = "")
    	logger(command)
     	result <- try(parse(text=paste(command)), silent=TRUE)
    }
    dsname
}


"trim.col.na" <- function(dat){
# Remove variables with only missing values (occurs sometimes with modified Excel file)
    colsup <- NULL
    for (i in 1:ncol(dat))
    {
    if (length(dat[is.na(dat[,i])==T,i]) ==length(dat[,i]))
     colsup <- c(colsup,i)
    }
    if (length(colsup) > 0)
     dat <- dat[,-colsup]
    dat
    }


 
################################
# Function to choose the dataframe : builds a listbox containing the data sets
# that are in the global environment and allows the user to choose one
################################
"SelectDataSet" <- function(ds.entry, dfnr.label, dfnc.label)
{
	tf <- tktoplevel()
	tkwm.title(tf,"Select data set")
  
  done <- tclVar(0)

	vnr <- NULL
	vnc <- NULL
	numi <- 1

	listbox.df <- tklistbox(tf)
	scr.df <- tkscrollbar(tf, repeatinterval=5, command=function(...)tkyview(listbox.df,...))
	tkconfigure(listbox.df, yscrollcommand=function(...)tkset(scr.df,...))
	frame1 <- tkframe(tf, relief="groove", borderwidth=2)
#	cancel.but <- tkbutton(frame1, text="Cancel", command=function()tkdestroy(tf))
	submit.but <- tkbutton(frame1, text="OK", width=12, default="active", command=function()tclvalue(done)<-1)
  tkpack(submit.but, side="left")
	tkpack(frame1, side="bottom")
	tkpack(listbox.df, side="left", fill="both", expand=TRUE)
	tkpack(scr.df, side="right", fill="y")

	obj <- ls(globalenv())
#
# For all objects in the global environment, check to see if it is a dataframe
# or a list. If it is a data frame, insert it in the listbox, and if it is a list,
# check its elements.
#
	flb <- function(x1) {
		xobj <- get(x1, envir=globalenv())
		if (is.matrix(xobj)) {
	   	xobj <- try(eval(parse(text="as.data.frame(xobj)")), silent=TRUE)
		 } 
    if (is.data.frame(xobj)) {
			tkinsert(listbox.df , "end", x1)
			cbind(nrow(xobj),ncol(xobj))
		} else if (is.list(xobj)) {
			if (length(names(xobj)) != 0) {
				fn1 <- function(x) {
					sobjn <- paste(x1,"$",x,sep="")
					sobj <- try(eval(parse(text=sobjn)), silent=TRUE)
					if (is.data.frame(sobj)) {
						tkinsert(listbox.df , "end", sobjn)
					}
				}
				sapply(names(xobj), fn1)
				fn2 <- function(x) {
					sobjn <- paste(x1,"$",x,sep="")
					sobj <- try(eval(parse(text=sobjn)), silent=TRUE)
					if (is.data.frame(sobj)) {
						cbind(nrow(sobj), ncol(sobj))
					}
				}
				res <- sapply(names(xobj), fn2)
        tkfocus(tt)
				return(res)
			}
		}
	}
	v <- unlist(lapply(obj, flb))
	if (length(v) > 0) {
		vnr <- v[seq(from=1,to=length(v),by=2)]
		vnc <- v[seq(from=2,to=length(v),by=2)]
	}

	tkbind(listbox.df , "<Double-ButtonPress-1>", function() tclvalue(done)<-1)
	tkbind(tf, "<Destroy>", function() tclvalue(done)<-2)
	tkbind(tf, "<KeyPress-Return>", function() tclvalue(done)<-1)
	tkbind(tf, "<KeyPress-Escape>", function() tkdestroy(tf))

	tkwait.variable(done)
	if(tclvalue(done)=="2") return(0)
#
# Get the number of the element choosed by the user
#
	numc <- tclvalue(tkcurselection(listbox.df))
	numi <- as.integer(numc)+1

	if(numc == "") {
		tkdestroy(tf)
    tkfocus(tt)
		return(0)
	}

	choix <- tclvalue(tkget(listbox.df, numc))

#
# Put the name of the object in the dataframe text entry
#
	tkdelete(ds.entry, 0, "end")
	tkinsert(ds.entry, "end", choix)
#
# Put the row and column numbers of the dataframe in the corresponding labels
#
	tkconfigure(dfnr.label, text=as.character(vnr[numi]))
	tkconfigure(dfnc.label, text=as.character(vnc[numi]))
  tkfocus(tt)
	tkdestroy(tf)
}
    tkfocus(tt)
}

