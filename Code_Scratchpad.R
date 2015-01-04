library(qgraph)

# This is following (loosely) http://www.r-bloggers.com/qgraph-version-1-1-0-and-how-to-simply-make-a-gui-using-rpanel/

data(big5)
data(big5groups)
qgraph(cor(big5),groups=big5groups,gui=TRUE)



Edges <- data.frame(
    from = rep(1:5,each=5),
		    to = rep(1:5,times=5),
				    thickness = abs(rnorm(25)))

Edges <- subset(Edges,from!=to)

qgraph(Edges,esize=5,gray=TRUE)

# Following http://stackoverflow.com/a/20255453

node.data <- data.frame(
    one=c("This is a test","And another test","Yet a third test"),
    two=c("And another test","Yet a third test","This is a test")
  )

# Testing hard-wrapping lines, following http://stat.ethz.ch/R-manual/R-patched/library/base/html/strwrap.html

x <- "This General Public License does not permit incorporating your program into proprietary programs."

# THIS WORKS FOR ARBITRARILY RESHAPING TEXT INTO BLOCKS, WITH \n SEPARATORS. This follows http://jeromyanglim.tumblr.com/post/33554853812/how-to-automatically-break-a-caption-over-multiple
node.data_hard_wrapped <- lapply(node.data[['one']], 
	function(x){
		paste(
			strwrap(x, width=5, simplify=TRUE)
			,
			collapse = "\n"
			)
		}
)





library(igraph)

g <- graph.data.frame(
  node.data_hard_wrapped,
  directed=FALSE # make this true if your network has a direction in the links
)
plot(g)






setwd("~/Desktop/Note-Taking_Network_Grapher/")

data_file_to_start <- "./todo.txt"
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

rm(dataframe_to_use)
dataframe_to_use <- data.frame(
	Text=as.Date(character()), # Just create an empt dataframe for now, following http://stackoverflow.com/a/10689206
	Tag=character(), 
	stringsAsFactors=FALSE
) 

# Create a logical vector for each line of the original file, vs. each tag from the master list. Ultimately, this will give us a filled-out dataframe showing which tags each line of original text has.
for(i in length(node_text_dataframe[['file1.text']])){
	text <- node_text_dataframe[['file1.text']][i]
	for(j in 1:length(master_tag_list)){
		if(grepl(master_tag_list[[j]], text)){
			row_to_append <- c(text, master_tag_list[[j]])
			dataframe_to_use <- rbind(dataframe_to_use, row_to_append)
			"Yes"
			#dataframe_to_use[[master_tag_list[i]]] <- 
			#grepl(master_tag_list[[i]], file1.text)*1 # Following http://r.789695.n4.nabble.com/Changing-a-logical-matrix-into-a-numeric-matrix-td3206797.html, multiplying by 1 here turns a logical vector (e.g., 'TRUE', 'TRUE', 'FALSE', etc.) into a numerical one (e.g., 1, 1, 0, etc.)
			#grep(master_tag_list[[i]], file1.text)
		}
	}
}

dataframe_to_use

#library('igraph')
#install.packages('InteractiveIGraph')
#library('InteractiveIGraph')

# Loosely using the tutorial at http://duomenuanalize.lt/sna-interactive-graphs-working-directly-r-graphic-device
#gOrg <- graph.famous("Heawood")
#plot(gOrg)
#X11(type="Xlib") # From http://stackoverflow.com/a/16673763, for Linux support.
#g = InteractiveIGraph(gOrg)
# THE ABOVE (FROM INTERACTIVEIGRAPH) ISN'T VERY COMPELLING

# Following http://www.r-bloggers.com/simple-network-diagrams-in-r/
#library(qgraph) 
# NOT WORKING:
# qgraph(dataframe_to_use, groups = factor(colnames(dataframe_to_use)[-1]))

write.csv(dataframe_to_use, file="Binary_Matrix.csv", row.names=FALSE)



Edges <- data.frame(
	from = rep(1:5,each=5),
	to = rep(1:5,times=5)
	#,
	#thickness = abs(rnorm(25))
	)

Edges <- subset(Edges,from!=to)

library('qgraph')
qgraph(Edges,esize=5,gray=TRUE)

qgraph(
	cbind(dataframe_to_use[c('file1.text')]
	,
	c('+1',NA,NA,'+1'))
	,esize=5,gray=TRUE)
