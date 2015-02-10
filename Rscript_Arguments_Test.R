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
	"--adjacency-matrix",
	metavar="Name for adjacency matrix", # What will be displayed in the help documentation.
	action="store", 
	type="character", 
	default="",
	help="Filename for CSV adjacency matrix to be saved. If this is not set, an adjacency matrix will not be created."
) 

parser$add_argument(
	"--save-quick-view-graph",
	metavar="Name for quick-view graph PDF file"
	action="store", 
	default="",
	help="Filename for quick-view graph to be saved (as a PDF). If this is not set, the quick-view graph will not be created."
)


args <- parser$parse_args()




print(args$save_quick_view_graph)


print("Files to parse are: ")
print(args$files_to_parse)

print("Ending test.")
