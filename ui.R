library(shiny)
library(shinyFiles)
library(gdata)
options(shiny.maxRequestSize=500*1024^2) 
# Define UI for slider demo application
shinyUI(pageWithSidebar(

  #  Application title
  headerPanel("Detect OE genes"),

  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(width=9,
    # file
    
		fileInput("filename", label = "File input (support .csv, .xls, .txt, .tab)"),

		# grouping vector
		fileInput("GroupVector", label = "Grouping vector \n file name (if not specified, adjacent cells 
		will be grouped together; support .csv, .xls, .txt, .tab)"),
	
		column(3,
		# Group num
		numericInput("GroupNum",
		label = "Number of groups (it will be ignored if grouping vector is provided)",
						        value = 8),


		# p value
		numericInput("TgtFDR",
		label = "p value cutoff",
						        value = 0.01),

		# Normalization
		radioButtons("Norm_buttons",
		    label = "Normalization needed?",
				 choices = list("Yes" = 1,
								   "No" = 2),
						         selected = 1)),								
		column(3,
		# Remove or impute
		radioButtons("RM_buttons",
		    label = "Remove OE genes or impute?",
				 choices = list("Remove" = 1,
								   "Impute" = 2),
						         selected = 1),								
		
		# LOD
		numericInput("LOD",
		label = "Lower limit of detection (mean)",
						        value = 1),

		# Num permutation
		numericInput("PermIn",
		label = "Number of permutations",
						        value = 10000)),
		column(3,
		# output dir
	  shinyDirButton('Outdir', 'Output folder select', 'Please select a folder'),

	# export normalzied matrix
	textInput("exNormFileName", 
	label = "Export file name - expression matrix", 
		        value = "normalized"),
		
 	# export OE gene list
	textInput("exListFileName", 
	label = "Export file name - OE gene list", 
		        value = "OEgenes"),
	# export OE pval
	textInput("exPVFileName", 
	label = "Export file name - p values", 
		        value = "pval")),

	column(3,
		# Remove or impute
		radioButtons("Plot_buttons",
		    label = "Plot OE genes?",
				 choices = list("Yes" = 1,
								   "No" = 2),
						         selected = 1),								
		# num genes to plot
		textInput("PlotNum", 
		label = "Number of genes to plot (if not specified, all OE genes will be plotted)", 
		        value = ""),
		# plot name
		textInput("exPlotFileName", 
		label = "Export file name for the plots?", 
		        value = "Plots")),

		actionButton("Submit","Submit for processing")
),

	
						

  # Show a table summarizing the values entered
  mainPanel(
    h4(textOutput("print0")),
		#tableOutput("values")
		dataTableOutput("tab")
  )
))
