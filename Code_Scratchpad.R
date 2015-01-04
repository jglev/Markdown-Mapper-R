setwd("~/Desktop/Note-Taking_Network_Grapher/")

data_file_to_start <- "./done2.txt"
# Following http://stackoverflow.com/a/6603126, read in the file as a list:
file1.text <- scan(data_file_to_start, what="list", sep="\n")

# Bash should already have done this:
# grep --perl-regexp --only-matching --no-filename "\+\w*" ~/Desktop/Note-Taking_Network_Grapher/todo.txt | sort | uniq > /tmp/note_taking_graph_helper_unique_tags.txt

master_tag_list_file <- "/tmp/note_taking_graph_helper_unique_tags.txt"
# Read the master tag list into a list:
master_tag_list <- scan(master_tag_list_file, what="list", sep="\n")

node_text_dataframe <- as.data.frame(file1.text, stringsAsFactors = FALSE)
node_text_dataframe
str(node_text_dataframe)

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
View(node_text_dataframe)
str(node_text_dataframe)


rm(edge_list)
edge_list <- data.frame(
	Source=as.Date(character()), # Just create an empt dataframe for now, following http://stackoverflow.com/a/10689206
	Target=character(), 
	stringsAsFactors=FALSE
) 

# Create a logical vector for each line of the original file, vs. each tag from the master list. Ultimately, this will give us a filled-out dataframe showing which tags each line of original text has.
for(i in 1:length(node_text_dataframe[["hard_wrapped"]])){
	text <- node_text_dataframe[["hard_wrapped"]][i]
	for(j in 1:length(master_tag_list)){
		tag <- master_tag_list[[j]]
		if(grepl(tag, text)){
			row_to_append <- cbind(text, tag)
			edge_list <- rbind(edge_list, row_to_append)
		}
		
		binary_association_matrix[[master_tag_list[i]]] <- 
			grepl(master_tag_list[[i]], file1.text)*1 # Following http://r.789695.n4.nabble.com/Changing-a-logical-matrix-into-a-numeric-matrix-td3206797.html, multiplying by 1 here turns a logical vector (e.g., 'TRUE', 'TRUE', 'FALSE', etc.) into a numerical one (e.g., 1, 1, 0, etc.)
	}
}

View(edge_list)
View(binary_association_matrix)

write.csv(edge_list, file="Edge_List.csv", row.names=FALSE, eol="\n", quote=TRUE)
write.csv(binary_association_matrix, file="Binary_Association_Matrix.csv", row.names=FALSE, eol="\n", quote=TRUE)

# Make a network graph using the Edge List:

library('qgraph')

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

