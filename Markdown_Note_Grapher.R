##############################
# NETWORK GRAPH FACILITATOR FOR GTD-STYLE (GETTING-THINGS-DONE-STYLE) NOTES
# Jacob Levernier
# AdUnumDatum.org
# January 2015
# Distributed under the GPLv2 License
# (If you would like to redistribute the code under other license terms, please contact the author)
##############################


# NOTE WELL: Although this script gives limitd information on the tags ('+tag') used in files passed to it, I recommend this bash function for getting tag counts:

# function search-file-for-tags() { # This lets you search a file to see all unique tags (starting, e.g., with '@' or '+') in a given file.
# 	# grep -Po "$1\w*" $2 | sort | uniq -c | sort -r # $1 here is the tag leader (e.g., \+ or \@); $2 is the filename.
# 	grep --perl-regexp --only-matching --no-filename "$1\w*" $2 | sort | uniq --count | sort --numeric-sort --reverse --field-separator=" " --key=1 # This will correctly count and sort (by count) across multiple files in a directory, not printing filenames.
# }

# You can then call this function with, e.g.,
# search-file-for-tags "\+" "/path/to/file"
# or
# cat file1 file2 | search-file-for-tags "\+"


# Define a function that installs a package if it can't be found:
checkPackage <- function(packageName, verbose = FALSE){
	if(verbose == TRUE){
		print(paste("Attempting to load package '", packageName, "'...", sep=""))
	}
	if(suppressMessages(!require(packageName, character.only = TRUE))){
		print(paste("The package '", packageName, "' wasn't found, so we'll try to install it now..."), sep="")
		
		# If the package is installed successfully, load it. Otherwise, give an error.
		install.packages(packageName, repos = "http://cran.r-project.org")
		if(verbose == TRUE){
			print("Package installed, so we'll try to load it now...")	
			require(packageName, character.only = TRUE)
		} else { # If verbose is NOT set to TRUE:
			suppressMessages(require(packageName, character.only = TRUE))
		}
	}else{
		if(verbose == TRUE){
			print("Package loaded successfully.")	
		}
	}
}

# Load the argparse package, which allows for argument parsing and automatic help documentation generation.
checkPackage('argparse', verbose = FALSE)



##################
# Set our command-line options:
##################

# This follows the argparse vignette at http://cran.r-project.org/web/packages/argparse/vignettes/argparse.pdf, which points to https://docs.python.org/2/library/argparse.html, the documentation for the python package for which this R library is a wrapper.

# A note on the argument options below: Per https://docs.python.org/2/library/argparse.html#action (the help documentation on which this R wrapper is based), action="store_const" is for flags -- it just stores a None value) (you can also use store_true or store_false to store "TRUE" and "FALSE", respectively) action="store" stores the value of the argument.

parser <- ArgumentParser(
	# Per https://docs.python.org/2/library/argparse.html#action, `prog=''` sets the name that's displayed in the auto-generated help documentation (if the user uses `--help`). Similarly with `description=''`:
	prog='Markdown Network Grapher',
	formatter_class = 'argparse.RawTextHelpFormatter', # This allows for linebreaks in the description below for the auto-generated help documentation. See http://quabr.com/27150625/r-argparse-line-breaks-in-description, which notes that line breaks must be escaped.
	description='An edge-list and adjacency-matrix creator for notes made in markdown.\\n\\n\\
	Explanation: blah blah blah'
)

default_tag_delimiter.string <- '\\+\\{.*?\\}'
default_tag_delimiter.explanation <- "TEST EXPLANATION" # This needs to be filled in with the explanation that's further down in the script.

parser$add_argument("-t", "--tag-delimiter", 
	action="store", 
	type="character", 
	default=default_tag_delimiter.string,
	help=paste(
		"A regular expression for tags. Defaults to '", 
		default_tag_delimiter.string, 
		"'", 
		if(default_tag_delimiter.explanation != ""){
			paste(
				" (",
				default_tag_delimiter.explanation,
				")",
				sep=""
			)
		},
		".",
		sep=""
	)
)

parser$add_argument(
	"files_to_parse", # Because it lacks a '-' flag, this will be interpreted as a positional argument.
	metavar="File to parse", # What will be displayed in the help documentation.
	nargs='+', # Gather as many filenames as are listed into a big list, and create an error message if there isn't at least one filename given (see https://docs.python.org/2/library/argparse.html#nargs)
	help="A list of plain-text files to parse."
) 

parser$add_argument(
	"-e",
	"--edge-list-name",
	metavar="Name for edge list", # What will be displayed in the help documentation.
	action="store", 
	type="character", 
	default="",
	help="Filename for CSV edge list to be saved. If this is not set, an edge list will not be created."
) 

parser$add_argument(
	"-a",
	"--adjacency-matrix-name",
	metavar="Name for adjacency matrix", # What will be displayed in the help documentation.
	action="store", 
	type="character", 
	default="",
	help="Filename for CSV adjacency matrix to be saved. If this is not set, an adjacency matrix will not be created."
) 

parser$add_argument(
	"-m",
	"--disable-master-tag-list",
	action="store_true", 
	default=FALSE,
	help="If this flag is set, a list of all of the unique tags used in the files will NOT be printed."
)

parser$add_argument(
	"-v",
	"--verbose",
	action="store_true", 
	default=FALSE,
	help="If this flag is set, package loading messages and progress messages will be printed. (If packages need to be installed, the messages indicating that will always be printed, regardless of whether this flag is set)."
)

parser$add_argument(
	"-q",
	"--disable-quick-view-graph",
	action="store_true", 
	default=FALSE,
	help="If this flag is set, a quick-view graph will NOT be drawn and presented."
)

parser$add_argument(
	"--quick-view-graph-name",
	metavar="Name for quick-view graph PDF file",
	action="store", 
	default="",
	help="Filename for quick-view graph to be saved (as a PDF). If this is not set, the quick-view graph will not be created."
)

# Read all of the arguments passed into this script:
args <- parser$parse_args()
# These can now be read with, e.g., `print(args$quick_view_graph_name)` or `print(args$files_to_parse)`

##################
# END OF command-line options section
##################


#setwd("~/Desktop/Note-Taking_Network_Grapher/")
#paste("Working from directory '", getwd(), "'...") # This will get the directory from which RScript is being called.

# Following http://stackoverflow.com/a/4574903, read in arguments passed through a bash call to this script (using, in bash, 'Rscript /path/to/this/Script.R')
# args <- commandArgs(TRUE)
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

# Create a blank object for the master list of all tags used across files. We'll fill this in below.
master_tag_list <- NULL

for(data_file_to_parse in args$files_to_parse){
	if(args$verbose == TRUE){
		print(paste("Processing file '", data_file_to_parse, "'..."))
	}
	
	#data_file_to_parse <- 
		#args[1]
	#	"./todo.txt"
	# Following http://stackoverflow.com/a/6603126, read in the file as a list:
	file.text <- scan(data_file_to_parse, what="list", sep="\n", blank.lines.skip=TRUE, quiet=!args$verbose) # Note that blank.lines.skip=TRUE will skip all blank lines in the file. !args$verbose is used here so that if verbose == TRUE, quiet will == FALSE, and vice versa.
	
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
	file.meta_information$file_name <- data_file_to_parse
	
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
	# Note: This regex allows for two types of tags: '+tag', and '+{tag phrase that includes spaces}'. The latter is included because it allows +{phrases to be tagged} (which allows easier searching across text files)
	tag_list_by_row <- regmatches(
		file.text, 
		gregexpr(paste('(?<!\\\\)', args$tag_delimiter, sep=""),file.text, perl=TRUE)
			# Let's document this regular expression:
				# 'perl = TRUE' is set because, following https://stackoverflow.com/questions/8834872/r-regular-expression-lookbehind#comment11030798_8834872, negative lookbehinds are only enabled in Perl regular expressions in R.
				# Note that lots of things are double-escaped (because R requires them to be. Hence, '\\w' is just '\w' (i.e., a word character), and '\\\\' is just '\\', i.e., an escaped '\'.
				# '(?<!\\\\)' ---> "Make sure that whatever is after this section (i.e., '+') is NOT preceeded by a '\'." (this is a 'negative lookbehind'). This allows the user to escape the string.
				# '\\+\\{.*?\\}' ---> "Give me every occurance of '+' followed immediately by '{}', with whatever you find between them ('{.*?\\}*').
		#\\+\\w* # Original Regex, for doing +word style tags. This interfered with writing equations (e.g., 2+2).
	)
	
	# We are tolower()-ing tags to make them more connectable across files (since, e.g., +Tag and +tag would otherwise be counted as two separate tags).
	tag_list_by_row <- lapply(tag_list_by_row, tolower)
	
	# Collapse the rows into a single vector, building up over loops through files:
	master_tag_list_for_this_file <- c(unlist(tag_list_by_row))
	
	# Get unique values from the single vector:
	master_tag_list <- c(master_tag_list, master_tag_list_for_this_file)
	
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
		# If line contains a "nota-bene" character combination, add that to the edge list
		#######################################
		
		if(file.meta_information$contains_nota_bene_note[line_number] == TRUE){
		  edge_list <- rbind(edge_list, data.frame(
		    Source = file.hard_wrapped_text[(line_number)],
		    Relationship = "Contains Type",
		    Target = "Nota Bene"
		  ))
		}
    
    
		#######################################
		# If line contains an "original thought" character combination, add that to the edge list
		#######################################
		
		if(file.meta_information$contains_note_to_self[line_number] == TRUE){
		  edge_list <- rbind(edge_list, data.frame(
		    Source = file.hard_wrapped_text[(line_number)],
		    Relationship = "Contains Type",
		    Target = "Original Thought"
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
							Relationship = "Ordered List Item",
							Target = text_line
						))
					}
				}
		} # End of if statement for ordered list.
    
    
		#######################################
		# If line is part of an UNordered list (with '*' bullet points), if the next line up with the same indentation is also part of an unordered list, make an edge between them. This code is redundant with the code above, and so could likely be refactored in the future.
		#######################################
		
		if(file.meta_information$is_part_of_unordered_list[line_number] == TRUE && line_number > 1){
		  possible_higher_list_item_line_numbers <- which(
		    file.meta_information$number_of_leading_tabs[1:(line_number-1)] == number_of_leading_tabs_for_this_line
		    & file.meta_information$is_part_of_unordered_list[1:(line_number-1)] == TRUE
		  ) # I could use which.max() here, but it doesn't do a good job when there are no matches. Also, '&' is used here because it is vectorized, whereas '&&' is not, per http://stackoverflow.com/a/15141015
		  
		  # If the search above for other line numbers yielded a match (i.e., likely_parent_line_number is not "integer(0)" (which is how R shows no match here)), add it to the edge list (in the format Source, Relationship, Target):
		  if(length(possible_higher_list_item_line_numbers) != 0) {
		    likely_parent_list_item_line_number <- max(possible_higher_list_item_line_numbers)
		    
		    # Check if there are non-ordered-list items in between this possible parent line and the current text_line at the same indentation level. If there are, then these two lines are probably not part of the same list. If there aren't, then we'll make an edge between the two lines:
		    are_there_intervening_lines_that_arent_part_of_list <- (length(which(
		      file.meta_information$number_of_leading_tabs[likely_parent_list_item_line_number:line_number] == number_of_leading_tabs_for_this_line
		      & file.meta_information$is_part_of_unordered_list[likely_parent_list_item_line_number:line_number] == FALSE
		    )) != 0)
		    
		    if(are_there_intervening_lines_that_arent_part_of_list == FALSE){
		      edge_list <- rbind(edge_list, data.frame(
		        Source = file.hard_wrapped_text[likely_parent_list_item_line_number],
		        Relationship = "Unordered List Item",
		        Target = text_line
		      ))
		    }
		  }
		} # End of if statement for UNordered list.
		
		
		
	
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

} # End of 'for(data_file_to_parse in args)' loop.

#View(edge_list)


#write.csv(edge_list, file="Edge_List.csv", row.names=FALSE, na="")
#write.csv(file.meta_information, file="Meta_Information.csv", row.names=FALSE, na="")

	
	
	
	

	########################
	# UPDATE: VUE IS GOOD WITH EDGE LISTS (and adjacency matrices, although for adjacency matrices, it prints all 0- or NA-relationship links (so you have to search for '0' and delete them). 
	#
	# TO USE AN EDGE LIST WITH VUE: Have three columns: one for source, one for target, and one for relationship (this column can be blank, but should be there). Then, in VUE, go to Windows -> Datasets, and click "+" to import a dataset. **Set "Import as Matrix Data" to TRUE.** Then say that the dataset is "Tall" ("Wide" would be for an adjacency matrix, or correlation matrix, etc.). Select the source, target, and relationship columns. Then you're good to go!!!
	########################
	

if(args$disable_master_tag_list != TRUE){
	message("The master list of all tags ('+tag') used in the given files is as follows:")
	print(as.matrix(sort(table(master_tag_list), decreasing = TRUE)))
}

if(args$disable_quick_view_graph != TRUE || args$quick_view_graph_name != ""){
	checkPackage('qgraph', verbose = args$verbose)
	checkPackage('methods', verbose = args$verbose) # Per http://t4007.science-graph-igraph-general.sciencetalk.info/could-not-find-function-is-in-graph-adjacency-t4007.html, if this script is being called from RScript, this needs to be explicitly called. Calling it solves an error: 'could not find function "is"'.
	
	pdf(file = NULL) # This isn't here because we're writing a pdf or anything else at this point. Rather, its here following so that R has an open display driver when it generates the graph below. Lacking this, a (blank) file called 'Rplots.pdf' is automatically created in the working directory. This happens as a result of running this script from Rscript, and happens even if 'DoNotPlot = TRUE' is set in qgraph().
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
		labels=TRUE,
		DoNotPlot=TRUE
	)
}

if(args$disable_quick_view_graph != TRUE){
	message("Generating quick-view network graph...")
	
	# To enable plotting when called from RScript, per http://stackoverflow.com/a/3302401
	X11(
		width=11,
		height=8.5
	)
	plot(graph)
	#dev.off()
	
	# For non-RScript work, playwith() allows resizing plots dynamically. It doesn't seem to allow zooming with qgraph output, but the window itself can be resized, which is a nice feature.
	#library('playwith')
	#playwith(plot(graph))

	# To stop plots from terminating when the script finishes after being called from RScript, per http://stackoverflow.com/a/3302401
	message("Press Return To Continue.")
	user_typed_response <- readLines("stdin", n=1)

} # End of if() statement for plotting quick-view graph.


if(args$quick_view_graph_name != ""){ # If we've been given anything here, we'll take it as a filepath, and save a PDF to it.
	# This follows the advice of http://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html
	
	plot_title <- paste("Map of [", paste(args$files_to_parse, collapse = ", "), "]")
	pdf_map_output_filename <- args$quick_view_graph_name
	
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
} # End of 'if(args$quick_view_graph_name != "")' statement


if(args$edge_list_name != ""){ # If we've been given anything here, we'll take it as a filepath, and save the edge list to it.
	edge_list_filename <- args$edge_list_name
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


if(args$adjacency_matrix_name != ""){ # If we've been given anything here, we'll take it as a filepath, and save the adjecency matrix list to it.
	
	###################
	# We've already created an edge list, and can at this point convert it to an adjacency matrix (i.e., going from "long"/"tall" format to "wide" format) in one step.
	###################
	
	# Following http://stackoverflow.com/a/25487162, use igraph to get an adjacency matrix from our edge list:
	checkPackage('igraph', verbose = args$verbose)
	
	adjacency_matrix <- as.matrix(
		get.adjacency(
			graph.edgelist(
				as.matrix(edge_list[c("Source", "Target")]),
				directed=FALSE)
		)
	)
	adjacency_matrix_filename <- args$adjacency_matrix_name
	write.csv(adjacency_matrix, file=adjacency_matrix_filename, row.names=TRUE, eol="\n", quote=TRUE)
	message("File saved to '", getwd(), "/", adjacency_matrix_filename,"'")

}

