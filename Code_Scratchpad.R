##############################
# NETWORK GRAPH FACILITATOR FOR GTD-STYLE (GETTING-THINGS-DONE-STYLE) NOTES
# Jacob Levernier
# AdUnumDatum.org
# January 2015
# Distributed under the GPLv2 License
# (If you would like to redistribute the code under other license terms, please contact the author)
##############################

setwd("~/Desktop/Note-Taking_Network_Grapher/")


paste("Working from directory '", getwd(), "'...") # This will get the directory from which RScript is being called.

# Following http://stackoverflow.com/a/4574903, read in arguments passed through a bash call to this script (using, in bash, 'Rscript /path/to/this/Script.R')
args <- commandArgs(TRUE)
# As an example, print(args[1]) Print the first argument passed to the script. 'args[1]' is equivalent to '$1' in a bash script.





# PLAN FOR MULTIPLE-FILES:
# Scan each file given in $args, and record the source file for each in the metadata list below (do an apply() loop over args(), then concat all lines into one big object, with the metadata list recording the source for each line). This can then be built into the edge list created below.

# Find metadata for the file that's been fenced off at the top of the file with a leading and trailing '---' on its own line (like this (without the comment delimeters):
#---
#Title: Test
#Author: Test
#Year: 2015
#---

file_yaml_metadata_fence_lines <- grep('^---', file1.text, perl=TRUE)[1:2]

# If we've found two '---' strings, we'll assume that they're metadata fences. In that case, we'll parse the metadata and add it to the metadata object. Otherwise, we'll skip this part.
if(!any(is.na(file_yaml_metadata_fence_lines))){
	yaml_metadata_for_file.unparsed <- file1.text[(file_yaml_metadata_fence_lines[1]+1):(file_yaml_metadata_fence_lines[2]-1)]
	
	# Remove those lines from the originally-read lines from the file:
	file1.text <- file1.text[-((file_yaml_metadata_fence_lines[1]):(file_yaml_metadata_fence_lines[2]))]
	
	# Split each metadata string by the first instance of ": ". This approach comes from http://stackoverflow.com/a/26247455
	yaml_metadata_for_file.parsed <- regmatches(yaml_metadata_for_file.unparsed, regexpr(": ", yaml_metadata_for_file.unparsed), invert = TRUE)
	
	lapply(
		yaml_metadata_for_file.parsed, 
		function(metadata_line) {
			file1.meta_information$metadata_line[[2]] <- metadata_line[[2]]
	})
	
}

tester <- list()
tester[5] <- "Gp"

# A DEMONSTRATION OF GETTING apply() TO WRITE VARIABLES OUTSIDE OF ITSELF, BY GOING OUTSIDE OF ITS NORMAL SCOPE:
# This is per http://stackoverflow.com/a/2657002. Both it and http://stackoverflow.com/a/2657002 recommend using a for loop when variable writing is the goal.
sapply(1:3, 
	function(number){
		tester[number] <<- number
		tester2 <<- 2
})







data_file_to_start <- 
	#args[1]
	"./todo.txt"
# Following http://stackoverflow.com/a/6603126, read in the file as a list:
file1.text <- scan(data_file_to_start, what="list", sep="\n", blank.lines.skip=TRUE) # Note that blank.lines.skip=TRUE will skip all blank lines in the file.

# Create a blank list to fill in:
file1.meta_information <- list()

file1.meta_information$number_of_leading_tabs <- attr(regexpr("^(\t*)", file1.text), "match.length") #The regex here is based on http://stackoverflow.com/a/3916879. We're here getting the number of leading tabs on each line, so that we can collapse each line's tabs without losing hierarchy information.

# Strip leading tabs off of each line:
file1.stripped_text <- sub("^\t*", "", file1.text)

file1.meta_information$is_part_of_unordered_list <- grepl("^\\*", file1.stripped_text) # Note whether each line is part of an unordered list (starting with '*')

file1.meta_information$is_part_of_ordered_list <-	grepl("^[0-9]\\.", file1.stripped_text) # Note whether each line is part of an ordered list (starting with, e.g. '1.')

# Strip out leading * or [0.-9.] list markers:
file1.stripped_text <- sub("^\\*\\s?", "", file1.stripped_text)
file1.stripped_text <- sub("^[0-9]\\.\\s", "", file1.stripped_text)


# Check for the presence of several special character combinations:
# --> blah blah <-- Nota Bene (a note of extra importance)
# {{ blah blah }} Note to self / Original Idea
# L> Explicit link to previous line.

file1.meta_information$contains_nota_bene_note <-	grepl("-->.*<--", file1.stripped_text)
file1.meta_information$contains_note_to_self <-	grepl("\\{\\{.*\\}\\}", file1.stripped_text)
file1.meta_information$contains_explicit_link_to_previous_line <-	grepl("L>", file1.stripped_text)

View(file1.meta_information)




###################
# FURTHER PLANNING
# * JUST CREATE AN EDGE LIST, AND THEN CONVERT IT FROM "LONG"/"TALL" FORM TO "WIDE" FORM (I.E., AN ADJACENCY MATRIX) IN ONE STEP:
###################

# Following http://stackoverflow.com/a/25487162, use igraph to get an adjacency matrix from our edge list:
library(igraph)

adjacency_matrix <- as.matrix(
	get.adjacency(
		graph.edgelist(as.matrix(edge_list), directed=FALSE)
	)
)







# Bash should already have done this:
# grep --perl-regexp --only-matching --no-filename "\+\w*" ~/Desktop/Note-Taking_Network_Grapher/todo.txt | sort | uniq > /tmp/note_taking_graph_helper_unique_tags.txt
master_tag_list <- system(paste('grep --perl-regexp --only-matching --no-filename "\\+\\w*" ', data_file_to_start, ' | sort | uniq'), intern=TRUE) # This could be refactored into R code (rather than bash calls) later.

#master_tag_list_file <- "/tmp/note_taking_graph_helper_unique_tags.txt"
# Read the master tag list into a list:
#master_tag_list <- scan(master_tag_list_file, what="list", sep="\n")

node_text_dataframe <- as.data.frame(file1.text, stringsAsFactors = FALSE)
# View(node_text_dataframe)
# str(node_text_dataframe)

# THIS WORKS FOR ARBITRARILY RESHAPING TEXT INTO BLOCKS, WITH \n SEPARATORS. This follows http://jeromyanglim.tumblr.com/post/33554853812/how-to-automatically-break-a-caption-over-multiple
node_text_dataframe[["hard_wrapped"]] <- as.character(lapply(node_text_dataframe[['file1.text']], 
	 function(x){
	 	paste(
	 		strwrap(x, width=20, simplify=TRUE)
	 		,
	 		collapse = "\n"
	 	)
	 }
))
# View(node_text_dataframe)
# str(node_text_dataframe)

# Clear memory from possible past runs of this script:
#rm(edge_list)
#rm(binary_association_matrix)

edge_list <- data.frame(
	Source=character(), # Just create an empt dataframe for now, following http://stackoverflow.com/a/10689206
	Target=character(), 
	stringsAsFactors=FALSE
)

# Start a new dataframe. We'll fill it in below.
binary_association_matrix <- data.frame(
	Text=node_text_dataframe[["hard_wrapped"]]
)

# Create a logical vector for each line of the original file, vs. each tag from the master list. Ultimately, this will give us a filled-out dataframe showing which tags each line of original text has.
for(j in 1:length(master_tag_list)){
	tag <- master_tag_list[j]

	for(i in 1:length(node_text_dataframe[["hard_wrapped"]])){
		text <- node_text_dataframe[["hard_wrapped"]][i]
		
		if(grepl(tag, text)){
			row_to_append <- cbind(text, tag)
			edge_list <- rbind(edge_list, row_to_append)
		}
	}

	binary_association_matrix[[tag]] <- grepl(tag, node_text_dataframe[["hard_wrapped"]])*1 # Following http://r.789695.n4.nabble.com/Changing-a-logical-matrix-into-a-numeric-matrix-td3206797.html, multiplying by 1 here turns a logical vector (e.g., 'TRUE', 'TRUE', 'FALSE', etc.) into a numerical one (e.g., 1, 1, 0, etc.)
}

# View(edge_list)
# View(binary_association_matrix)


# Make a network graph using the Edge List:

library('qgraph')
library('methods') # Per http://t4007.science-graph-igraph-general.sciencetalk.info/could-not-find-function-is-in-graph-adjacency-t4007.html, if this script is being called from RScript, this needs to be explicitly called. Calling it solves an error: 'could not find function "is"'.








# WORKING EXAMPLE CODE MODIFIED FROM http://sachaepskamp.com/qgraph/examples
# TO GET AN ADJACENCY MATRIX WORKING, THE BIG THING IS TO GET THE COLUMN NAMES SET.

set.seed(1)
adj = matrix(sample(0:1, 10^2, TRUE, prob = c(0.8, 0.2)), nrow = 10, ncol = 10)

rownames(adj) <- NULL #c('a','b','c','d','e','f','g','h','i','j') # ROWNAMES works fine, but doesn't seem to influence the graph one way or another.

colnames(adj) <- c('a','b','c','d','e','f','g','h','i','j')

qgraph(adj)
title("Unweighted and directed graphs", line = 2.5)




########################
# UPDATE: VUE IS GOOD WITH EDGE LISTS (and adjacency matrices, although for adjacency matrices, it prints all 0- or NA-relationship links (so you have to search for '0' and delete them). 
#
# TO USE AN EDGE LIST WITH VIEW: Have three columns: one for source, one for target, and one for relationship (this column can be blank, but should be there). Then, in VUE, go to Windows -> Datasets, and click "+" to import a dataset. **Set "Import as Matrix Data" to TRUE.** Then say that the dataset is "Tall" ("Wide" would be for an adjacency matrix, or correlation matrix, etc.). Select the source, target, and relationship columns. Then you're good to go!!!
########################











# To enable plotting when called from RScript, per http://stackoverflow.com/a/3302401
X11(
	width=11,
	height=8.5
	)
graph <- qgraph(
 	edge_list,
	esize=5,
	gray=TRUE,
	label.scale=TRUE,
	curve=1,
	curveAll=TRUE,
	directed=FALSE,
	layout='spring', # Can also be 'groups' or 'circular',
	shape="circle",
	border.width=.5,
	labels=TRUE
 )

# For non-RScript work, playwith() allows resizing plots dynamically. It doesn't seem to allow zooming with qgraph output, but the window itself can be resized, which is a nice feature.
#library('playwith')
#playwith(plot(graph))

# To stop plots from terminating when the script finishes after being called from RScript, per http://stackoverflow.com/a/3302401
message("Press Return To Continue. Press Y/y and then Return to save a PDF.")

#invisible(
user_typed_response <- readLines("stdin", n=1)
#)

if(user_typed_response == 'Y' || user_typed_response == 'y'){
	# This follows the advice of http://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html
	
	plot_title <- paste("Map of '", data_file_to_start, "'")
	pdf_map_output_filename <- "Network_Map.pdf"
	
	#png(file="animals45.png",width=1200,height=800,res=300)
	pdf(
		file=pdf_map_output_filename, 
		width=11, 
		height=8.5,
		title=plot_title # This is the title within the title.
	)
	par(oma=c(0,0,0,0)) # From http://stackoverflow.com/a/13631358. '?par' states that oma is 'a vector of the form c(bottom, left, top, right) giving the size of the outer margins in lines of text.' We're here adding space for a title.
	plot(graph)
	title(
		main=NULL, 
		sub=plot_title,
		xlab=NULL,
		ylab=NULL
	)
	dev.off()
	
	message("File saved to '", getwd(), "/", pdf_map_output_filename,"'")
}


# To stop plots from terminating when the script finishes after being called from RScript, per http://stackoverflow.com/a/3302401
message("Press Y/y and then Return to save CSV output. Otherwise, just press Return.")

#invisible(
user_typed_response <- readLines("stdin", n=1)
#)

if(user_typed_response == 'Y' || user_typed_response == 'y'){
	write.csv(edge_list, file="Edge_List.csv", row.names=FALSE, eol="\n", quote=TRUE)
	
	write.csv(binary_association_matrix, file="Binary_Association_Matrix.csv", row.names=FALSE, eol="\n", quote=TRUE)
}


