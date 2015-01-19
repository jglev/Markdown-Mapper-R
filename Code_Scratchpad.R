##############################
# NETWORK GRAPH FACILITATOR FOR GTD-STYLE (GETTING-THINGS-DONE-STYLE) NOTES
# Jacob Levernier
# AdUnumDatum.org
# January 2015
# Distributed under the GPLv2 License
# (If you would like to redistribute the code under other license terms, please contact the author)
##############################

#setwd("~/Desktop/Note-Taking_Network_Grapher/")
paste("Working from directory '", getwd(), "'...") # This will get the directory from which RScript is being called.

# Following http://stackoverflow.com/a/4574903, read in arguments passed through a bash call to this script (using, in bash, 'Rscript /path/to/this/Script.R')
args <- commandArgs(TRUE)
# As an example, print(args[1]) Print the first argument passed to the script. 'args[1]' is equivalent to '$1' in a bash script.


edge_list <- data.frame(
	Source=character(), # Just create an empt dataframe for now, following http://stackoverflow.com/a/10689206
	Target=character(), 
	stringsAsFactors=FALSE
)

####################################
# PLAN FOR MULTIPLE-FILES: Scan each file given in $args, and add to the edge list as we process each
####################################

# For testing
#args <- c("./todo.txt", "done.txt")

for(data_file_to_start in args){
	print(paste("Processing file '", data_file_to_start, "'..."))
	
	#data_file_to_start <- 
		#args[1]
	#	"./todo.txt"
	# Following http://stackoverflow.com/a/6603126, read in the file as a list:
	file.text <- scan(data_file_to_start, what="list", sep="\n", blank.lines.skip=TRUE) # Note that blank.lines.skip=TRUE will skip all blank lines in the file.
	
	# Create a blank list to fill in:
	file.meta_information <- list()
	
	# THE ABOVE SYSTEM CALL IS NOW PORTED INTO STRAIGHT R:
	# See http://www.regular-expressions.info/rlanguage.html
	# An example: 
	# regmatches('This is a test tester!', gregexpr('test\\w*','This is a test tester!'))
	
	# Find metadata for the file that's been fenced off at the top of the file with a leading and trailing '---' on its own line (like this (without the comment delimeters):
	#---
	#Title: Test
	#Author: Test
	#Year: 2015
	#---
	
	# Remove YAML-metadata-related variables from previous loops, and then go forward.
	file_yaml_metadata_fence_lines <- NULL
	yaml_metadata_for_file.unparsed <- NULL
	yaml_metadata_for_file.parsed <- NULL
	
	file_yaml_metadata_fence_lines <- grep('^---', file.text, perl=TRUE)[1:2]
	
	# If we've found two '---' strings, we'll assume that they're metadata fences. In that case, we'll parse the metadata and add it to the metadata object. Otherwise, we'll skip this part.
	if(!any(is.na(file_yaml_metadata_fence_lines))){
		yaml_metadata_for_file.unparsed <- file.text[(file_yaml_metadata_fence_lines[1]+1):(file_yaml_metadata_fence_lines[2]-1)]
		
		# Remove those lines from the originally-read lines from the file:
		file.text <- file.text[-((file_yaml_metadata_fence_lines[1]):(file_yaml_metadata_fence_lines[2]))]
		
		# Split each metadata string by the first instance of ": ". This approach comes from http://stackoverflow.com/a/26247455
		yaml_metadata_for_file.parsed <- regmatches(yaml_metadata_for_file.unparsed, regexpr(": ", yaml_metadata_for_file.unparsed), invert = TRUE)
		
		# Start filling in the metadata table for each line of the parsed file.
		for(metadata_line in yaml_metadata_for_file.parsed) {
			file.meta_information[[metadata_line[[1]]]] <- rep(metadata_line[[2]], length(file.text))
		}
	} # End of YAML loop
	
	# Further fill in the metadata table:
	file.meta_information$file_name <- data_file_to_start
	
	file.meta_information$number_of_leading_tabs <- attr(regexpr("^(\t*)", file.text), "match.length") #The regex here is based on http://stackoverflow.com/a/3916879. We're here getting the number of leading tabs on each line, so that we can collapse each line's tabs without losing hierarchy information.
	
	# Strip leading tabs off of each line:
	file.stripped_text <- sub("^\t*", "", file.text)
	
	file.meta_information$is_part_of_unordered_list <- grepl("^\\*", file.stripped_text) # Note whether each line is part of an unordered list (starting with '*')
	
	file.meta_information$is_part_of_ordered_list <- grepl("^[0-9]\\.", file.stripped_text) # Note whether each line is part of an ordered list (starting with, e.g. '1.')
	
	# Strip out leading * or [0.-9.] list markers:
	file.stripped_text <- sub("^\\*\\s?", "", file.stripped_text)
	file.stripped_text <- sub("^[0-9]\\.\\s", "", file.stripped_text)
	
	
	# Check for the presence of several special character combinations:
	# --> blah blah <-- Nota Bene (a note of extra importance)
	# {{ blah blah }} Note to self / Original Idea
	# L> Explicit link to previous line.
	
	file.meta_information$contains_nota_bene_note <-	grepl("-->.*<--", file.stripped_text)
	file.meta_information$contains_note_to_self <-	grepl("\\{\\{.*\\}\\}", file.stripped_text)
	file.meta_information$contains_explicit_link_to_previous_line <-	grepl("L>", file.stripped_text)
	
	#View(file.meta_information)
	
	
	# Perform the grep, returning all values (one vector per row):
	tag_list_by_row <- regmatches(
		file.text, 
		gregexpr('\\+\\w*',file.text)
	)
	
	# Collapse the rows into a single vector:
	master_tag_list <- unlist(tag_list_by_row)
	
	# Get unique values from the single vector:
	master_tag_list <- unique(master_tag_list)
	
	
	# Bash should already have done this:
	# grep --perl-regexp --only-matching --no-filename "\+\w*" ~/Desktop/Note-Taking_Network_Grapher/todo.txt | sort | uniq > /tmp/note_taking_graph_helper_unique_tags.txt
	#master_tag_list <- system(paste('grep --perl-regexp --only-matching --no-filename "\\+\\w*" ', data_file_to_start, ' | sort | uniq'), intern=TRUE) # This could be refactored into R code (rather than bash calls) later.
	
	#master_tag_list_file <- "/tmp/note_taking_graph_helper_unique_tags.txt"
	# Read the master tag list into a list:
	#master_tag_list <- scan(master_tag_list_file, what="list", sep="\n")
	
	node_text_dataframe <- as.data.frame(file.text, stringsAsFactors = FALSE)
	# View(node_text_dataframe)
	# str(node_text_dataframe)
	
	# THIS WORKS FOR ARBITRARILY RESHAPING TEXT INTO BLOCKS, WITH \n SEPARATORS. This follows http://jeromyanglim.tumblr.com/post/33554853812/how-to-automatically-break-a-caption-over-multiple
	file.hard_wrapped_text <- as.character(lapply(file.stripped_text, 
		function(x){
			paste(
				strwrap(x, width=20, simplify=TRUE)
				,
				collapse = "\n"
			)
		}
	))
	
	
	# Add the actual hard-wrapped text to the meta information object, so that we know which line we're talking about. This will also make the hard-wrapped text the primary key on which to join the meta information and edge list in programs like Visual Understanding Environment.
	# NOTE WELL that file.meta_information should be ordered such that the 'hard_wrapped_text' column DOES NOT first. This will make programs like Visual Understanding Environment parse it better.
	file.meta_information$hard_wrapped_text <- file.hard_wrapped_text
	
	# If we DID want this column to come first in the dataframe, we could use this:
	#file.meta_information <- data.frame(
	#	hard_wrapped_text = file.hard_wrapped_text, 
	#	file.meta_information
	#)
	
	
	
	# Clear memory from possible past runs of this script:
	#rm(edge_list)
	#rm(binary_association_matrix)
	
	# Loop through the text and make an edge list from it:
	for(line_number in 1:length(file.hard_wrapped_text)){
		# print(paste("Processing line number", line_number, "...")) # Good for debugging.
		
		text_line <- file.hard_wrapped_text[line_number]
		
		#######################################
		# If the line was indented, find the next-highest line that's one tab less-indented.
		#######################################
	
		number_of_leading_tabs_for_this_line <- as.data.frame(file.meta_information)[line_number,"number_of_leading_tabs"]
		
		if(number_of_leading_tabs_for_this_line > 0){
			possible_parent_line_numbers <- which(file.meta_information$number_of_leading_tabs[1:line_number] == (number_of_leading_tabs_for_this_line - 1)) # I could use which.max() here, but it doesn't do a good job when there are no matches.
			
			# If the search above for other line numbers yielded a match (i.e., likely_parent_line_number is not "integer(0)" (which is how R shows no match here)), add it to the edge list (in the format Source, Relationship, Target):
			if(length(possible_parent_line_numbers) != 0) {
				likely_parent_line_number <- max(possible_parent_line_numbers)
				edge_list <- rbind(edge_list, data.frame(
					Source = file.hard_wrapped_text[likely_parent_line_number],
					Relationship = "Parent",
					Target = text_line
				))
			}
		}
		
		#######################################
		# If line contains an explicit character combination link to the previous line, add that link to the edge list
		#######################################
		
		if(file.meta_information$contains_explicit_link_to_previous_line[line_number] == TRUE && line_number > 1){ # Line number > 1 is here to avoid errors associated with looking up the (non-existant) previous line '0'.
			edge_list <- rbind(edge_list, data.frame(
				Source = file.hard_wrapped_text[(line_number-1)],
				Relationship = "",
				Target = text_line
			))
		}
	
		
		#######################################
		# If line is part of an ordered list (1., 2., 3., etc.), if the next line up with the same indentation is also part of an ordered list, make an edge between them.
		#######################################
		
		if(file.meta_information$is_part_of_ordered_list[line_number] == TRUE && line_number > 1){
				possible_higher_list_item_line_numbers <- which(
					file.meta_information$number_of_leading_tabs[1:(line_number-1)] == number_of_leading_tabs_for_this_line
					& file.meta_information$is_part_of_ordered_list[1:(line_number-1)] == TRUE
				) # I could use which.max() here, but it doesn't do a good job when there are no matches. Also, '&' is used here because it is vectorized, whereas '&&' is not, per http://stackoverflow.com/a/15141015
				
				# If the search above for other line numbers yielded a match (i.e., likely_parent_line_number is not "integer(0)" (which is how R shows no match here)), add it to the edge list (in the format Source, Relationship, Target):
				if(length(possible_higher_list_item_line_numbers) != 0) {
					likely_parent_list_item_line_number <- max(possible_higher_list_item_line_numbers)
					
					# Check if there are non-ordered-list items in between this possible parent line and the current text_line at the same indentation level. If there are, then these two lines are probably not part of the same list. If there aren't, then we'll make an edge between the two lines:
					are_there_intervening_lines_that_arent_part_of_list <- (length(which(
						file.meta_information$number_of_leading_tabs[likely_parent_list_item_line_number:line_number] == number_of_leading_tabs_for_this_line
						& file.meta_information$is_part_of_ordered_list[likely_parent_list_item_line_number:line_number] == FALSE
					)) != 0)
					
					if(are_there_intervening_lines_that_arent_part_of_list == FALSE){
						edge_list <- rbind(edge_list, data.frame(
							Source = file.hard_wrapped_text[likely_parent_list_item_line_number],
							Relationship = "List Item",
							Target = text_line
						))
					}
				}
		} # End of if statement for ordered list.
		
		
		
	
		#######################################
		# If line contains tags, make edge list entries for each
		#######################################
		
		if(length(tag_list_by_row[line_number][[1]]) > 0){ # [[1]] here is because each row of tag_list_by_row is itself a list.
			for(tag in tag_list_by_row[line_number][[1]]){
				edge_list <- rbind(edge_list, data.frame(
					Source = file.hard_wrapped_text[line_number],
					Relationship = "Tag",
					Target = tag
				))
			}
		}
		
	} # End of loop through text.
		
	
	edge_list_merged_with_metadata <- merge(edge_list, file.meta_information, by.x="Source", by.y="hard_wrapped_text")
	#View(edge_list_merged_with_metadata)	
	
	# Because programs like Visual Understanding Environment don't seem to be able to import matrix-data (like our edge list) AND join those to other datasets consistently, we'll add an edge for each of the YAML metadata pieces, as well as filename:
	#as.data.frame(yaml_metadata_for_file.parsed)[1,][1]
	#file_name
	if(length(yaml_metadata_for_file.parsed) > 0){
		# print(paste("The parsed YAML Metadata for the file is as follows:", yaml_metadata_for_file.parsed)) # Good for debugging.
		
		for(metadata_line in yaml_metadata_for_file.parsed) {
			yaml_title <- metadata_line[[1]]
			edge_list <- rbind(
				edge_list,
				data.frame(
					Source = file.meta_information$hard_wrapped_text,
					Relationship = yaml_title,
					Target = file.meta_information[[yaml_title]]
				)
			)
		}
	}
	
	edge_list <- rbind(
		edge_list,
		data.frame(
			Source = file.meta_information$hard_wrapped_text,
			Relationship = "File",
			Target = file.meta_information$file_name
		)
	)

} # End of 'for(data_file_to_start in args)' loop.

#View(edge_list)


write.csv(edge_list, file="Edge_List.csv", row.names=FALSE, na="")
write.csv(file.meta_information, file="Meta_Information.csv", row.names=FALSE, na="")







	
	plot_title <- paste("Map of '", data_file_to_start, "'")
	pdf_map_output_filename <- "Network_Map.pdf"
	


# Make a network graph using the Edge List:

library('qgraph')
library('methods') # Per http://t4007.science-graph-igraph-general.sciencetalk.info/could-not-find-function-is-in-graph-adjacency-t4007.html, if this script is being called from RScript, this needs to be explicitly called. Calling it solves an error: 'could not find function "is"'.

	
	
	
	
	
	
	

	########################
	# UPDATE: VUE IS GOOD WITH EDGE LISTS (and adjacency matrices, although for adjacency matrices, it prints all 0- or NA-relationship links (so you have to search for '0' and delete them). 
	#
	# TO USE AN EDGE LIST WITH VUE: Have three columns: one for source, one for target, and one for relationship (this column can be blank, but should be there). Then, in VUE, go to Windows -> Datasets, and click "+" to import a dataset. **Set "Import as Matrix Data" to TRUE.** Then say that the dataset is "Tall" ("Wide" would be for an adjacency matrix, or correlation matrix, etc.). Select the source, target, and relationship columns. Then you're good to go!!!
	########################
	
	


message("Generating quick-view network graph...")

# To enable plotting when called from RScript, per http://stackoverflow.com/a/3302401
X11(
	width=11,
	height=8.5
)
graph <- qgraph(
	edge_list[c("Source", "Target")],
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
#dev.off()
	
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
message("Press Y/y and then Return to save Edge List CSV output. Otherwise, just press Return.")

#invisible(
user_typed_response <- readLines("stdin", n=1)
#)

if(user_typed_response == 'Y' || user_typed_response == 'y'){
	edge_list_filename <- "Edge_List.csv"
	write.csv(edge_list, file=edge_list_filename, row.names=FALSE, eol="\n", quote=TRUE)
	message("File saved to '", getwd(), "/", edge_list_filename,"'")
	
	message("
If you would like to use this edge list in Visual Understanding Environment (VUE), do the following: 
1. In VUE, go to Windows -> Datasets, and click '+' to import a dataset. 
2. **Set 'Import as Matrix Data' to TRUE.** 
3. Select that the dataset is 'Tall' ('Wide' would be for an adjacency matrix, or correlation matrix, etc.).
4. Select the source, target, and relationship columns. Then you're good to go!
	")
}


message("Press Y/y and then Return to save Adjacency Matrix CSV output. Otherwise, just press Return.")

#invisible(
user_typed_response <- readLines("stdin", n=1)
#)

if(user_typed_response == 'Y' || user_typed_response == 'y'){
	
	###################
	# We've already created an edge list, and can at this point convert it to an adjacency matrix (i.e., going from "long"/"tall" format to "wide" format) in one step.
	###################
	
	# Following http://stackoverflow.com/a/25487162, use igraph to get an adjacency matrix from our edge list:
	library(igraph)
	
	adjacency_matrix <- as.matrix(
		get.adjacency(
			graph.edgelist(
				as.matrix(edge_list[c("Source", "Target")]),
				directed=FALSE)
		)
	)
	adjacency_matrix_filename <- "Adjacency_Matrix.csv"	
	write.csv(adjacency_matrix, file=adjacency_matrix_filename, row.names=FALSE, eol="\n", quote=TRUE)
	message("File saved to '", getwd(), "/", adjacency_matrix_filename,"'")

}

