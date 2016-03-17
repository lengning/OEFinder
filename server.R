library(shiny)
library(shinyFiles)
library(gdata)

FindOEfun<-
function (Data, Group = NULL, Poly = 2, Nchunk = 8, Sigcut = 0.01, MeanLOD=1,
    Plot = FALSE, NumPlot = NULL, numNullgenes=10000, numPermu=10,
	Seed=1, mfrow=c(5,4))
{
    set.seed(Seed)
    
    # rescale here
    DataIn <- Data
	if(min(DataIn)<0)stop("Error: exist values < 0!")
    DataIn1 <- DataIn[which(rowMeans(DataIn)> MeanLOD & apply(DataIn,1,sd)>0),]
    cat(paste("\n Removed genes with mean smaller or equal to", MeanLOD, "\n"))
    DataMean <- apply(DataIn1,1,mean)
    DataSD <- apply(DataIn1,1,sd)
    Data <- (DataIn1-DataMean)/DataSD
    	
    
    Ncol <- ncol(Data)
    if (is.null(Group)) {
        Col <- rainbow(Nchunk)
        V_B <- rep(1:Nchunk, each = ceiling(Ncol/Nchunk))[1:Ncol]
        ColV_B <- rep(Col, each = ceiling(Ncol/Nchunk))[1:Ncol]
        Group <- as.factor(V_B)
    }
    else {
        if (!is.factor(Group))
        Group <- factor(Group)
        Nchunk <- nlevels(Group)
        Col <- rainbow(Nchunk)
        names(Col) <- levels(Group)
        V_B <- Group
        ColV_B <- Col[Group]
    }
    ## aggregate statistics on original data
    DataUse <- Data
    Datasort <- DataUse[, order(Group)]
    Fac <- as.numeric(factor(Group[order(Group)]))
    Aggr <- t(sapply(1:nrow(Datasort), function(i)PolyEach(Datasort[i, ], Fac , Poly)))
    rownames(Aggr) <- rownames(Datasort)
    colnames(Aggr) <- c("p2nd","coef2nd","coef1","r2","adjr2", "beta",
	"onesidep","ssall","ssr","ssreg","F","fpval","aggrstat")

		#browser()
    ## Permute data
    if(numNullgenes/numPermu > nrow(Data)) numPermu <- ceiling(numNullgenes/nrow(Data))
    set.seed(Seed)
    PermList <- sapply(1:numPermu,function(i)Datasort[,sample(1:Ncol,Ncol)],simplify=F)
    PermuMatAll <- do.call(rbind, PermList)
    set.seed(Seed)
    PermuMat <- PermuMatAll[sample(1:nrow(PermuMatAll), numNullgenes),]
    PermuAgg <- sapply(1:nrow(PermuMat), function(i)PolyEach(PermuMat[i, ], Fac , Poly)["aggrstat"])


	# empirical p value    
	AggrV <- Aggr[,"aggrstat"]
	Aggrp <- sapply(1:length(AggrV), function(i) sum(PermuAgg >= AggrV[i])/length(PermuAgg))
	names(Aggrp)=names(AggrV)
	AggrpS <- sort(Aggrp)
	Sig <- AggrpS[which(AggrpS<=Sigcut)]
        Allp <- Aggrp 
	Allpsort <- AggrpS  

	# Adj
	Po <- poly(Fac, Poly)
	coe2 <- Aggr[,"coef2nd"]
	Toadj <- outer(coe2, Po[,2])
	coe1 <- Aggr[,"coef1"]
	Toadj1 <- outer(coe1, Po[,1])
	rownames(Toadj) <- rownames(Datasort)
	if(length(Sig)>0){
	Dataadj <- Datasort
	#Dataadj[names(Sig),] <-  Dataadj[names(Sig),]-Toadj[names(Sig),] 
	Datarm <- Datasort[setdiff(rownames(Datasort),names(Sig)),]
}
	else{
	Dataadj <- Datasort
	Datarm <- Datasort
}

	# original scale
	DataadjTran <- Dataadj*DataSD + DataMean
	DataadjTran[names(Sig),] <-   outer(DataMean[names(Sig)], rep(1,Ncol))
	DatarmTran <- Datarm * DataSD[setdiff(rownames(Datasort),names(Sig))] + DataMean[setdiff(rownames(Datasort),names(Sig))]
        

	if(Plot==TRUE){
                if(is.null(NumPlot))NumPlot=length(Sig)
		if(NumPlot>0){
                par(mfrow=mfrow)
                for(i in 1:NumPlot){
		tmpname=names(Allpsort)[i]	
                plot(1:Ncol,
                      DataIn[tmpname, order(Group)],
                       main=paste0(
                        tmpname," pval ",round(Allpsort[tmpname],4)),
                         ylab="Normalized Expression",xlab="cell", col=ColV_B)
        }
}}

	Out=list(Sig=Sig, Allp=Allp, Allpsort=Allpsort, AdjustedData=DataadjTran,
	CleanedData=DatarmTran, Statistics=Aggr, nullVector=PermuAgg)
}



PolyEach <- function(X, V, Poly){
        Po <- poly(V, Poly)
        aa <- lm(X ~ Po)
        bb <- summary(aa)
        coe <- coefficients(bb)
        # one side p
        onesd <- pt(coe[3,3],aa$df.residual,lower.tail=FALSE)
        # SS
        ssall <- sum((X-mean(X))^2)
        ssr <- sum(aa$residuals^2)
        ssreg <- ssall-ssr
        fstat <- bb$fstatistic
        fp <- pf(fstat[1],fstat[2],fstat[3],lower.tail=FALSE)
        out <- c(coe[3, 4],coe[3,1], coe[2,1],
                bb$r.squared, bb$adj.r.squared,
                coe[3,3],onesd, ssall, ssr, ssreg,
                bb$fstatistic[1],fp, -log(onesd)-log(fp))
        names(out) <- c("p2nd","coef2nd","coef1","r2","adjr2", "beta",
  "onesidep","ssall","ssr","ssreg","F","fpval","aggrstat")
  out
}






# Define server logic for slider examples
shinyServer(function(input, output, session) {
    volumes <- c('home'="~")
    shinyDirChoose(input, 'Outdir', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$Dir <- renderPrint({parseDirPath(volumes, input$Outdir)})	
	# Group V
	In <- reactive({
	the.file <- input$filename$name

    print(input$Outdir)
    outdir <- paste0("~/",input$Outdir[[1]][[2]],"/")
    message("output folder")
    print(outdir)
	
	Sep=strsplit(the.file,split="\\.")[[1]]
  	if(Sep[length(Sep)]%in%c("xls"))a1=read.xls(input$filename$datapath,stringsAsFactors=F,header=TRUE, row.names=1)
  	if(Sep[length(Sep)]=="csv")a1=read.csv(input$filename$datapath,stringsAsFactors=F,header=TRUE, row.names=1)
  	if(Sep[length(Sep)]%in%c("txt","tab"))a1=read.table(input$filename$datapath,stringsAsFactors=F,header=TRUE, row.names=1)
  	Data=data.matrix(a1)

	Group.file <- input$GroupVector$name
  	GroupB <- ifelse(is.null(Group.file),FALSE,TRUE)
  	#GroupB=FALSE
	GroupV <- NULL
  	
	if(GroupB==TRUE){
    Group.Sep=strsplit(Group.file,split="\\.")[[1]]
    if(Group.Sep[length(Group.Sep)]%in%c("xls"))
    GroupVIn=read.xls(input$GroupVector$datapath,stringsAsFactors=F,header=F)
    if(Group.Sep[length(Group.Sep)]=="csv")
    GroupVIn=read.csv(input$GroupVector$datapath,stringsAsFactors=F,header=F)
    if(Group.Sep[length(Group.Sep)]%in%c("txt","tab"))
    GroupVIn=read.table(input$GroupVector$datapath,stringsAsFactors=F,header=F, sep="\t")
    GroupV=GroupVIn[[1]]
    }
    # Compose data frame
		#input$filename$name
		List <- list(
		Input=the.file,
		GroupFile=Group.file,
		withGroup=GroupB, GroupV=GroupV,
		Num=input$GroupNum, 
		NormTF=ifelse(input$Norm_buttons=="1",TRUE,FALSE),
		RMTF=ifelse(input$RM_buttons=="1",TRUE,FALSE), 
		FDR=input$TgtFDR, 
		LODNum=input$LOD, NumPermu=input$PermIn,
		Dir=outdir, 
		exExpF = paste0(outdir,input$exNormFileName,".csv"),
		exOEF = paste0(outdir,input$exListFileName,".csv"),		
		exPVF = paste0(outdir,input$exPVFileName,".csv"),
		PlotTF = ifelse(input$Plot_buttons=="1",TRUE,FALSE), 
		PlotF = paste0(outdir,input$exPlotFileName,".pdf"),
		PlotN = input$PlotNum
)
		# normalization and LOD
		DataUse0=Data
  	if(List$NormTF){
    	library(EBSeq)
			Sizes <- MedianNorm(DataUse0)
			if(is.na(Sizes)){
			Sizes <- MedianNorm(DataUse0, alternative=T)
			print("alternative normalization is applied - all genes have at least one zeros")
			}
    	DataUse0=GetNormalizedMat(Data,Sizes)
 	 	}
  	DataUse=DataUse0[which(rowMeans(DataUse0)>List$LODNum),]
		# main function
  	if(List$PlotTF)pdf(List$PlotF, height=15,width=15)
					PN <- NULL
					if(List$PlotN!="")PN <- as.numeric(List$PlotN) 
					
  		Res <- FindOEfun(Data=DataUse,
					Group = List$GroupV , Poly = 2, Nchunk = List$Num, 
					Sigcut = List$FDR, MeanLOD = List$LODNum,
          Plot = List$PlotTF, NumPlot = PN, 
					numNullgenes = List$NumPermu,
          numPermu = ceiling(List$NumPermu/(nrow(DataUse))*5),
          Seed=1, mfrow=c(5,4))
  	if(List$PlotTF)dev.off()

	# write out
	Sig=matrix(Res$Sig,ncol=1)
     rownames(Sig)=names(Res$Sig)
     colnames(Sig)="p-value"

     Allp=matrix(Res$Allpsort,ncol=1)
     rownames(Allp)=names(Res$Allpsort)
     colnames(Allp)="p-value"
     write.csv(Sig,file=List$exOEF)
 
 	#cat(paste0("\n\nNum OE genes: ", length(Res$Sig)))
	 write.csv(Allp,file=List$exPVF)
  	 if(List$RMTF)write.csv(Res$CleanedData, file=List$exExpF)
  	 else write.csv(Res$AdjustedData, file=List$exExpF)

	List=c(List, list(Sig=Sig, DataSig=DataUse[rownames(Sig),]))	
}) 

  Act <- eventReactive(input$Submit,{
		      In()})
	# Show the values using an HTML table
  output$print0 <- renderText({
		tmp <- Act()
		str(tmp)
		paste("output directory:", tmp$Dir,
		"; # OE genes:", length(tmp$Sig))
  })

	output$tab <- renderDataTable({
		tmp <- Act()$Sig
		t1 <- cbind(rownames(tmp),tmp)
		colnames(t1) <- c("gene", "pvalue")
		print("done")
		t1
		},options = list(lengthManu = c(4,4), pageLength = 20))

#	output$done <- renderText({"Done"})
})
