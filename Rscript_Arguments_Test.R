print("Starting test now...")

# Define a function that installs a package if it can't be found:
checkPackage <- function(packageName){
	print(paste("Attempting to load package '", packageName, "'...", sep=""))
	if(!require(packageName, character.only = TRUE)){
		print("The package wasn't found, so we'll try to install it now...")
		
		# If the package is installed successfully, load it. Otherwise, give an error.
		if(install.packages(packageName, repos = "http://cran.r-project.org")){
			print("Package installed, so we'll try to install it now...")	
			require(packageName, character.only = TRUE)
		}else{
			print(paste("ERROR: We couldn't install the package '", packageName, "' successfully. Exiting the script so that you can figure out what went wrong...", sep=""))
		}
	}else{
		print("Package loaded successfully.")	
	}
}


checkPackage('argparse')

# This follows the argparse vignette at http://cran.r-project.org/web/packages/argparse/vignettes/argparse.pdf, which points to https://docs.python.org/2/library/argparse.html, the documentation for the python package for which this R library is a wrapper.

# A note on the argument options below: Per https://docs.python.org/2/library/argparse.html#action (the help documentation on which this R wrapper is based), action="store_const" is for flags -- it just stores a None value) (you can also use store_true or store_false to store "TRUE" and "FALSE", respectively) action="store" stores the value of the argument.

parser <- ArgumentParser(
	# Per https://docs.python.org/2/library/argparse.html#action, `prog=''` sets the name that's displayed in the auto-generated help documentation (if the user uses `--help`). Similarly with `description=''`:
	prog='Markdown Network Grapher',
	formatter_class = 'argparse.RawTextHelpFormatter', # This allows for linebreaks in the description below for the auto-generated help documentation. See http://quabr.com/27150625/r-argparse-line-breaks-in-description, which notes that line breaks must be escaped.
	description='An edge-list and adjacency-matrix creator for notes made in markdown.\\n\\n\\
Explanation: blah blah blah'
)

default_tag_delimiter.string <- "THIS IS A TEST"
default_tag_delimiter.explanation <- "TEST EXPLANATION"

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
	"--edge-list",
	metavar="Name for edge list", # What will be displayed in the help documentation.
	action="store", 
	type="character", 
	default="",
	help="Filename for CSV edge list to be saved. If this is not set, an edge list will not be created."
) 

parser$add_argument(
	"-a",
	"--adjacency-matrix",
	metavar="Name for adjacency matrix", # What will be displayed in the help documentation.
	action="store", 
	type="character", 
	default="",
	help="Filename for CSV adjacency matrix to be saved. If this is not set, an adjacency matrix will not be created."
) 

parser$add_argument(
	"-m",
	"--show-master-tag-list",
	action="store_true", 
	default=FALSE,
	help="If this flag is set, a list of all of the unique tags used in the files will be printed."
)

parser$add_argument(
	"-q",
	"--show-quick-view-graph",
	action="store_true", 
	default=FALSE,
	help="If this flag is set, a quick-view graph will be drawn and presented."
)

parser$add_argument(
	"--save-quick-view-graph",
	action="store", 
	default="",
	help="Filename for quick-view graph to be saved (as a PDF). If this is not set, the quick-view graph will not be created."
)




parser$add_argument(
	"-f",
	"--change-phrase-from",
	metavar="Phrase to target for replacing", # What will be displayed in the help documentation.
	action="append", # This argument can be specified multiple times, and will be saved into a list.
	type="character", 
	help="To be used to create a dictionary for turning one phrase into another. Needs to be paired with '--change-phrase-to'. Can be used multiple times for multiple phrases (in that case, the elements in the 'from' and 'to' lists are matched up in the order they were created. This is especially useful for consolidating similar tags. NOTE: This flag DOES NOT change the original input file at all."
) 


parser$add_argument(
	"-i",
	"--change-phrase-into",
	metavar="Phrase with which to replace another phrase", # What will be displayed in the help documentation.
	action="append", # This argument can be specified multiple times, and will be saved into a list.
	type="character", 
	help="To be used to create a dictionary for turning one phrase into another. Needs to be paired with '--change-phrase-from'. Can be used multiple times for multiple phrases (in that case, the elements in the 'from' and 'to' lists are matched up in the order they were created. This is especially useful for consolidating similar tags. NOTE: This flag DOES NOT change the original input file at all."
) 


args <- parser$parse_args()

print(length(args$change_phrase_from))
print(length(args$change_phrase_into))

if(length(args$change_phrase_from) != length(args$change_phrase_into)){ # If we've been given a dictionary of replacement terms to use, but the 'from' and 'to' columns don't match up, throw an error:
	stop("ERROR: The '--change-phrase-from' and '--change-phrase-into' lists don't match up -- they are not the same length (this could be because you forgot to pair them up for every phrase you want to replace, if it's more than 1). Exiting so that you can figure out what went wrong.")
}

# If we want to replace these as-is, we can do an apply loop with them combined using cbind as soon as we read in each file. The code to do so without treating these as regular expressions is `gsub("{test", "REPLACED", "This is a {test etc. {", fixed=TRUE)`.




#print(args$dictionary_term)


print("Files to parse are: ")
print(args$files_to_parse)

print("Ending test.")
