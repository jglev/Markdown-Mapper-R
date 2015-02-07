print("Starting test now...")

library('argparse')

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
	action="store", 
	type="character", 
	default="",
	help="Filename for edge list to be saved. If this is not set, an edge list will not be created."
) 

parser$add_argument(
	"-a",
	"--adjacency-matrix",
	action="store", 
	type="character", 
	default="",
	help="Filename for adjacency matrix to be saved. If this is not set, an adjacency matrix will not be created."
) 

parser$add_argument(
	"-q",
	"--show-quick-view-graph",
	action="store_true", 
	default="",
	help="If this flag is set, a quick-view graph will be drawn and presented."
)

parser$add_argument(
	"--save-quick-view-graph",
	action="store_true", 
	default=FALSE,
	help="If this flag is set, a quick-view graph will be saved as a PDF file."
)


args <- parser$parse_args()

print(args$save_quick_view_graph)

if(args$tester != ""){
	print("TEST IS SET")
	print(paste("TEST IS: ", args$test))
} else {
	print("NOT SET")	
}

print("Files to parse are: ")
print(args$files_to_parse)

print("Ending test.")