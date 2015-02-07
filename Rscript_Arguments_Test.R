print("Starting test now...")

library('argparse')

# This follows the argparse vignette at http://cran.r-project.org/web/packages/argparse/vignettes/argparse.pdf

parser <- ArgumentParser(
	# Per https://docs.python.org/2/library/argparse.html#action, `prog=''` sets the name that's displayed in the auto-generated help documentation (if the user uses `--help`). Similarly with `description=''`:
	prog='Markdown Network Grapher',
	formatter_class = 'argparse.RawTextHelpFormatter', # This allows for linebreaks in the description below for the auto-generated help documentation. See http://quabr.com/27150625/r-argparse-line-breaks-in-description, which notes that line breaks must be escaped.
	description='An edge-list and adjacency-matrix creator for notes made in markdown.\\n\\
	Usage: blah blah blah'
)

parser$add_argument("-a", "--tester", 
	action="store", 
	type="character", 
	default="", 
	help="This is a test module."
)

parser$add_argument("-t", "--tag-delimiter", 
	action="store", 
	type="character", 
	default="", 
	help="A regular expression for tags. Defaults to '+{tag goes in here}'."
)

parser$add_argument(
	"files_to_parse", 
	metavar="File to parse", # What will be displayed in the help documentation.
	nargs='+', # Gather as many filenames as are listed into a big list, and create an error message if there isn't at least one filename given (see https://docs.python.org/2/library/argparse.html#nargs)
	help="A list of plain-text files to parse."
) 
# Because it lacks a '-' flag, this will be interpreted as a positional argument.



# Per https://docs.python.org/2/library/argparse.html#action (the help documentation on which this R wrapper is based), action="store_const" is for flags -- it just stores a None value) (you can also use store_true or store_false to store "TRUE" and "FALSE", respectively).

args <- parser$parse_args()

if(args$tester != ""){
	print("TEST IS SET")
	print(paste("TEST IS: ", args$test))
} else {
	print("NOT SET")	
}

print("Files to parse are: ")
print(args$files_to_parse)

print("Ending test.")