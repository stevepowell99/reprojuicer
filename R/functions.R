#' Provides an easy xls workbook for reproducibly organising a dataset - reordering, renaming, recoding, providing longer and shorter labels, switching easily between labels in multiple languages, calculating new variables, grouping into blocks.
#'
#'
#' Also provides a function filter_keep_label which allows you to subset a data frame and still keep the labels and other attributes
#'
#' Main intro at the moment is the readme: \link{https://github.com/stevepowell99/reprojuicer}
#' Note you can use raw_data$myvar in formula to refer to version of an earlier variable which has already been changed by a formula.
#' Use label2 for another language, create more if you want
#' @export
#' @import XLConnect
#' @name reprojuice
#'
#'
reprojuice=function(dataname="data",type="",method="quicker",onlyLoad=F,spreadsheet_path=""){
  #/usr/bin/libreoffice
  if(onlyLoad){
    assign(paste0(dataname,".r"),readRDS(paste0(dataname,".r")),envir=.GlobalEnv)##reassign the new tidy colnames


    if(file.exists(paste0(dataname,".b")))assign(paste0(dataname,".b"),readRDS(paste0(dataname,".b")),envir=.GlobalEnv)

    if(file.exists(paste0(dataname,".s")))assign(paste0(dataname,".s"),readRDS(paste0(dataname,".s")),envir=.GlobalEnv)



    #     specials=readRDS("specials")
  } else {
    labnames=xc("varnames ncol level n1 newvarnames label label2 formula recode setlevout block")
    propfilename=paste0("prop.",dataname,".xls")


    ## tidy colnames
    raw_data=get(dataname,envir=.GlobalEnv)
    colnames(raw_data)=make.names(colnames(raw_data),unique=T)# have to do this because the xlconnect and csv calls wil do it
    assign(dataname,raw_data,envir=.GlobalEnv)##reassign the new tidy colnames


    ## decide is case new file or existing
    if(!file.exists(propfilename)) {
      current_data=raw_data
      case="newfile"

      oo=t(data.frame(make_fresh_propsheet(current_data)))
      new_propsheet=propsheet=data.frame(oo)


    } else {
      case="oldfile"
      wb <- XLConnect::loadWorkbook(propfilename)
      sheets <<- XLConnect::getSheets(wb)
      #
      #read in worksheet and create list current_data to receive the this's from raw_data
      propsheet= XLConnect::readWorksheetFromFile(propfilename,sheet="props",rownames=NULL,colTypes="character",startCol=1)
      propsheet=propsheet[!is.na(propsheet[,1]),]
      rownames(propsheet)=make.names(propsheet[,1],unique=T)
      colnames(propsheet)[1]="varnames"
      # browser()
      propsheet<<-propsheet
      message("reading in file")
      current_data=as.list(rep(times=nrow(propsheet),99))

      ## now the loop should replace but it adds!!!
      # browser()
      # browser()

      names(current_data)=propsheet$varnames
      for(n in rownames(propsheet)){
        if(n %in% colnames(raw_data))current_data[[n]]=raw_data[,n]
      }


      for (varname in rownames(propsheet)){
        if(varname %in% colnames(raw_data)) current_var=current_data[[varname]]#raw_data[,varname] else current_var=rep(NA,nrow(raw_data))

        if(length(current_var)!=nrow(raw_data)) {stop(paste0("Looks like you have a data frame within your data frame?",varname));}


        if(!xmb(propsheet[varname,"formula"])) {
          #
          current_var=with(current_data,eval(parse(text=propsheet[varname,"formula"])))
        }
        var_is_new=!(varname %in% rownames(propsheet))
        if(var_is_new) warning(paste0("Adding new rows to props file: ",varname))


        # loop through the only two cases which change data - recode and setlevout
        for(current_property in colnames(propsheet)){

          ##### RECODE

          ## a recode has been ordered! so handle the recodes
          if(current_property=="recode" & !xmb(propsheet[varname,current_property]) ){
            tabname=propsheet[varname,current_property]
            propsheet$recode[is.na(propsheet$recode)]=""
            myblock<<-propsheet[propsheet$recode==tabname,]

            issingle=dim(myblock)[1]==1
            myvars=rownames(propsheet)[propsheet$recode==tabname]
            if(issingle) concord=data.frame("output"=names(table(current_var)),"input"=names(table(current_var))) else {
              mydata=raw_data[,myvars]
              #             bropropsheeter()
              blockvalues=do.call(what="c",as.list(mydata))
              mytable=table(blockvalues)
              concord=data.frame("output"=names(mytable),"input"=names(mytable))
            }
            if(tabname %in% sheets) {  #the worksheet already exists, only need to add any new data it found
              mySheet= XLConnect::readWorksheetFromFile(propfilename,sheet=tabname,startCol=1,startRow=1,rownames=NULL,colTypes="character")#fixme if it is empty it will crash
              #             myrecodes=data.frame(mySheet[,1:ncol(mySheet)])

              concord=mySheet # temporarily switched off adding new data
              # if data is to be con or int but has blanks, these shd be NA. str or factor cd be NA or blank, cater for both. xlconnect imports blank as NA so have to code NA into blank for non-numeric and "NA" into NA
              mySheet[is.na(mySheet)]=""
              mySheet[mySheet=="NA"]=NA #amusing switch because xlconnect imports empty string as NA
              if(propsheet[varname,"setlevout"] %in% xc("con int")){
                current_var[current_var==""]=NA
              } #


              ins=mySheet[,2:ncol(mySheet)]
              outs=mySheet[,1]
              #
              if(method=="slow" ){
                current_var=sapply(current_var,function(v){
                  instemp=ifelse(v=="" & !is.null(dim(ins)),ins[,1],ins)# ugly but otherwise when you have multiple input specs the trailing blanks get counted. This means that if you want a blank in the input spec it has to be in first column.
                  if(sum(instemp==v,na.rm=T)>1) stop(paste0("you have this twice in your input specification: ",v))
                  if(!is.na(v)) whichrow=which(rowSums(data.frame(ins)==v,na.rm=T)>0) else whichrow=which(rowSums(is.na(data.frame(ins)),na.rm=T)>0) #because if v is NA we have to treat it specially
                  o=outs[whichrow]
                  ifelse(length(o)==0,v,o)
                })} else {
                  for(v in unlist(ins)){
                    if(!is.na(v)) whichrow=which(rowSums(data.frame(ins)==v,na.rm=T)>0) else whichrow=which(rowSums(is.na(data.frame(ins)),na.rm=T)>0) #because if v is NA we have to treat it specially
                    o=outs[min(whichrow)]
                    if(!is.na(v)){
                      if(length(o)!=0 )  if(!is.na(v)) {current_var[current_var==v]=o}
                    } else current_var[is.na(current_var)]=o
                  }
                }


              if(propsheet[varname,"setlevout"] %in% xc("ord nom")){
                current_var=factor(current_var,ordered=ifelse(propsheet[varname,"setlevout"]=="ord",T,F),levels=(mySheet[,1]))
              }
            }
            #
            if(myblock[1,1]==varname){
              XLConnect::writeWorksheetToFile(propfilename,concord,tabname,styleAction=XLC$"STYLE_ACTION.NONE")##fixme so only does itonce for multiples
            }

          }

          ##### SETLEVout
          # of course you could do this with the formulae too

          if(current_property=="setlevout"){

            if(xmb(propsheet[varname,current_property]))next()
            if(propsheet[varname,current_property]=="str")current_var=as.character(current_var)
            if(propsheet[varname,current_property]=="nom")current_var=(factor(current_var,ordered=F)) #not sure what happens if there was already a recode above
            if(propsheet[varname,current_property]=="ord" & xmb(propsheet[varname,"recode"])){current_var=(factor(current_var,ordered=T))}# if you provided a recode, this has already been done.
            if(propsheet[varname,current_property]=="int")current_var=as.integer(current_var)
            if(propsheet[varname,current_property]=="con")current_var=as.numeric(current_var)
          }
        }  # END OF CURRENT_PROPERTY LOOP; STILL IN VARNAME LOOP






        # browser()

        ## SO NOW WE LOAD MAYBE TRANSFORMED CURRENTVAR INTO L
        current_data[[varname]]=current_var
        current_data<<-current_data


        ## GIVE THIS VAR IN L THE NAME WHICH WAS SET IN VARNAMES
        names(current_data)[ncol(current_data)]=propsheet[varname,"varnames"]
      }
      ## END OF PROPSHEET ROW LOOP

      ## ADD IN ANY NEW VARS DEFINED IN PROPSHEET
      current_data<<-current_data
      current_data=data.frame(current_data)

      rownames(current_data)=rownames(raw_data)

      orphans=data.frame(raw_data[,!(colnames(raw_data)  %in% colnames(current_data))])
      colnames(orphans)=colnames(raw_data)[which(!(colnames(raw_data)  %in% colnames(current_data)))]
      current_data=data.frame(current_data,orphans) # to add in any new rows in data raw_data which were not in propsheet



      for(j in 1:ncol(current_data)){
        i=colnames(current_data)[j]
        store=attributes(current_data[,i])
        attributes(current_data[,i])=propsheet[i,]
        attr(current_data[,i],"ncol")=j
        attr(current_data[,i],"varnames")=i #to make sure the varnames are always correct
        propsheet[i,"varnames"]=i
        propsheet[i,"ncol"]=j
        if(attr(current_data[,i],"setlevout")%in% xc("ord nom")) attr(current_data[,i],"levels")=store$levels
        if(attr(current_data[,i],"setlevout")%in% xc("ord nom")) attr(current_data[,i],"class")=store$class
      }

      new_propsheet=data.frame(propsheet)
    }


    ##   END OF NEWFILE / OLDFILE



    #     specials=setdiff(colnames(propsheet),labnames)
    #     nspecials=specials[str_sub(specials,1,1)!="."]
    #     attr(current_data,"specials")=specials
    #
    #
    #     attr(current_data,"specials")=specials

    ### PRETTY CERTAIN THIS WAS ONLY TO DO WITH CLUSTERS
    lold=current_data
    if(F&case!="newfile"){  ####### note ffffffffff
      for(j in 1:ncol(current_data)){
        i=make.names(colnames(current_data))[j]
        if(!(i %in% colnames(lold) )){
          #
          attr(current_data[,i],"ncol")=j
          attr(current_data[,i],"varnames")=i #to make sure the varnames are always correct

          propsheet[i,"varnames"]=propsheet[i,"label"]=propsheet[i,"shortlabs"]=i
          propsheet[i,"ncol"]=j
          propsheet[i,"setlevout"]=attr(current_data[,i],"setlevout")


        } #it must have come from cluster addition and wasn't already covered
      }
      colnames(current_data)=ifelse(is.na(propsheet[,"newvarnames"]),colnames(current_data),propsheet[,"newvarnames"])

      new_propsheet=data.frame(propsheet)
      kill<<-new_propsheet
    }
    XLConnect::writeWorksheetToFile(propfilename,new_propsheet,"props",styleAction=XLConnect::XLC$"STYLE_ACTION.NONE")
    # system(paste0("wmctrl -c ",propfilename," - LibreOffice Calc"), wait=FALSE)

    wb <- XLConnect::loadWorkbook(propfilename, create = TRUE)

    # Create a worksheet
    XLConnect::createSheet(wb, name = "props")

    # Create a custom anonymous cell style
    cs <- XLConnect::createCellStyle(wb)

    # Specify the fill background color for the cell style created above
    # setFillBackgroundColor(cs, color = XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")

    # Specify the fill foreground color
    XLConnect::setFillForegroundColor(cs, color = XLConnect::XLC$"COLOR.LIGHT_CORNFLOWER_BLUE")

    # Specify the fill pattern
    XLConnect:: setFillPattern(cs, fill = XLConnect::XLC$"FILL.SOLID_FOREGROUND")

    # Set the cell style created above for the top left cell (A1) in the
    # 'cellstyles' worksheet
    XLConnect:: setCellStyle(wb, formula=paste0("props!$A$1:$F$",1+nrow(propsheet)), cellstyle = cs)
    XLConnect::setCellStyle(wb, formula=paste0("props!$A$1:$",LETTERS[1+ncol(propsheet)],"$1"), cellstyle = cs)

    # Save the workbook
    XLConnect::saveWorkbook(wb)


    if(spreadsheet_path!="")browseURL(paste0(propfilename),browser=spreadsheet_path)



    attr(current_data,"labels")=new_propsheet
    assign(paste0(dataname,".r"),current_data,envir=.GlobalEnv)# update raw_data adding new cols if necessary

    #     if(assigndata.r)assign(paste0("data.r"),current_data,envir=.GlobalEnv)# update raw_data adding new cols if necessary
    #     #   stop("assinged data")

    saveRDS(current_data,paste0(dataname,".r"))


    b<<-findr(equ="list",att="block",dat=current_data,datafilename=paste0(dataname,".r"))


    saveRDS(b,paste0(dataname,".b"))#
    assign(paste0(dataname,".b"),b,envir=.GlobalEnv) #more general version when dealing with several datasets

    # assign(paste0("specials"),specials,envir=.GlobalEnv)

    #     saveRDS(specials,"specials")
  }

}

#' helps you find a fresh block
#' @export
#'
findr=function(att="block",dat,equ="x",datafilename="",...){
  if(equ=="list") {
    l=unlist(unique(sapply(dat,attr,att)))
    if(!is.null(l))    {l=na.omit(l)
    # browser()
    d=lapply(l,function(x) findr_inner(att=att,dat=dat,equ=x,datafilename=datafilename,...))
    names(d)=l} else d=NULL


  } else d=findr_inner(att,dat,equ,datafilename=datafilename,...)

  d
} # Note the dataset it returns also has a label attribute which tries to extract an appropriate label from the attribute

findr_inner=function(att="block",dat,equ="x",datafilename=""){

  x=equ==sapply(dat,attr,att)
  x[is.na(x)]=F
  #         browser()
  d=data.frame(dat[,x])


  if(equ=="x") eqx="" else eqx=equ
  # browser()


  if(nrow(d)==0 | ncol(d)==0) warning(cat("findr returned an empty block: ",att,equ))
  if(datafilename!="") d=data.frame(lapply(d,function(x){
    # browser()
    attr(x,"datafilename")=datafilename
    x
  }))
  attr(d,"label")=paste(att,eqx,sep=":") # dont put whitespace cos of pandoc
  attr(d,"datafilename")=datafilename
  attr(d,"blocktitle")=unique(na.omit(sapply(d,attr,"blocktitle"))) #could put in a warning if there are tow

  d
}


xc=function(str,sepp=" ")stringr::str_split(str,sepp)[[1]]

xmb=function(x,y="") if(!xexists(x))T else{ if(length(x)==0) TRUE  else if(class(x)=="data.frame") FALSE else if(is.na(x)) TRUE else if(is.null(x)) TRUE  else if(x==y) TRUE else FALSE}
#true if missing or null or y, otherwise false. NOTE IT GIVES F IF IT IS ANY DATA FRAME, EVEN AN EMPTY ONE

xexists=function(x)class(try(class(x),silent=T))!="try-error"



thisfindr=function(att,equ="x",dat=l,wstemp=ws){
  #
  if(!(att %in% colnames(wstemp)))stop(P("There is something wrong with the attribute name for the thisfindr function: ",att))
  wss=wstemp[,att]==equ
  wss[is.na(wss)]=FALSE
  data.frame(dat)[,wss] #has to be -1 because l still has a leading stupid empty vector which is cut off at the end
}



classer=function(x){
  y=class(x)[1]
  s=switch(EXPR=y,"integer"="con","factor"="nom","character"="str","numeric"="con","ordered"="ord","logical"="log","labelled"="nom")
  if(is.numeric(x))s="con"
  att=attr(x,"setlevout")
  if(!is.null(att)) if(!(att %in% xc("con nom str ord int"))) s=att #you can't force a variable to be one of these types if it is not
  s
}



make_fresh_propsheet=function(df,labnames=xc("varnames ncol level n1 newvarnames label label2 formula recode setlevout block")){ #gets stuff from the data only
  #   df=get(nam,envir=.GlobalEnv)
  #   df<<-df
  colnames(df)=make.names(colnames(df),unique=T)
  sapply(colnames(df),function(xx){
    x=df[,xx]
    t=table(x)
    tt=names(t[rev(order(t))])
    sapply(labnames,function(y){

      if(y=="varnames") xx else {
        if(y=="ncol") min(which(xx==colnames(df))) else {
          if(y=="label") ifelse(is.null(attr(x,"label")),xx,attr(x,"label")) else {
            if(y=="label2") ifelse(is.null(attr(x,"label")),xx,attr(x,"label")) else {
              if(y=="level") classer(x) else {
                if(y=="setlevout") classer(x) else {
                  if(y=="nt") length(unique(x)) else {
                    if(y=="n1") paste(tt[1:min(10,length(tt))],collapse=";;") else {
                      if(y=="n2") "" else {
                        if(y=="n3") "" else {
                          a=attr(x,y)
                          if(is.null(a)) '' else a
                        }}}}}}}}}}
    })
  })

}#it gets the possibly properties





#' Simple wrapper for ggplot
#' So you can print the labels instead of the varnames
#' Set a label:
#' \code{attr(mtcars$cyl,"label")="cylinder"}
#' Or, because reprojuicer imports Hmisc, more conveniently:
#' \code{label(mtcars$cyl)="cylinder"}
#' \code{library(ggplot2);ggplotl(data=mtcars,aes(cyl,disp))+geom_point()}
#'
#' @param labs If you want to use attributes other than \code{label} for your labels, give it a list like \code{labs=list(x="otherattribute",y="yetanotherattribute"))}. By default, the attribute \code{label} is used.
#' @return various things
#' @export
#'
#' @examples
#' attr(mtcars$disp,"labelb")="čćđž"
#' attr(mtcars$disp,"label")="Displacement"
#' library(reprojuicer)
#' ggplotl(data=mtcars,aes(disp))+geom_density()
#' ggplotl(data=mtcars,aes(disp),labs=list(x="labelb"))+geom_density()
#' ggplotl(data=mtcars,aes(gear,cyl,colour=disp),labs=list(colour="labelb"))+geom_point()
#'
#'
ggplotl=function(...,labs=list()){
  plot=ggplot2::ggplot(...)
  dat=plot$data
  for(m in names(plot$mapping)){
    char=paste0(plot$mapping[m])
    l=labs[[m]]
    lab=ifelse(is.null(l),"label",l)
    ml=attr(dat[,char],lab)
    plot$labels[m]=ml
  }

  plot
}



#' Filter a dataset, keeping attributes - fix me what if attr assignment fails
#' @param df A data frame you want to split
#' @param split a vector which will filter the data frame. NA values replaced as FALSE
#' @param addlabel A note you should add to the label attribute so that the filtering is explained in graphs etc
#' @return The filtered data frame, usually with fewer rows. But the attributes of the variables are retained where possible.
#' @export
#'
filter_keep_label=function(df,split,addlabel=" - subset"){
  split[is.na(split)]=F
  if(length(dim(df))>0){

  dfn=df[split,]
  for(n in colnames(df)){
    attributes(dfn[,n])=attributes(df[,n])
    attr(dfn[,n],"label")=paste0(attr(dfn[,n],"label"),addlabel)

  }
  } else {
  dfn=df[split]
    attributes(dfn)=attributes(df)
    attr(dfn,"label")=paste0(attr(dfn,"label"),addlabel)
  }
  dfn
}

