print("Starting test now...")

library('argparse')

# This follows the argparse vignette at http://cran.r-project.org/web/packages/argparse/vignettes/argparse.pdf

parser <- ArgumentParser()

parser$add_argument("-t", "--tester", action="store", type="character", default="", help="This is a test module.")

# Per https://docs.python.org/2/library/argparse.html#action (the help documentation on which this R wrapper is based), action="store_const" is for flags -- it just stores a None value) (you can also use store_true or store_false to store "TRUE" and "FALSE", respectively).

args <- parser$parse_args()

if(args$tester != ""){
	print("TEST IS SET")
	print(paste("TEST IS: ", args$test))
} else {
	print("NOT SET")	
}


print("Ending test.")