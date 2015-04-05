file.text <- scan("~/Desktop/Note-Taking_Network_Grapher/example2.txt", what = "list", sep = "\n", blank.lines.skip = TRUE)

file.text
file.text[1]
#do.call("rbind", file.text[28:29])
paste(file.text[28:29], collapse = "\n")

#file_singleline_marker_locations <- grep('(```.*```|<code>.*</code>)', file.text, perl=TRUE)[1:2]

vector_of_singleline_markers <- c("</?code>", "</?blockquote>","```")

# Create a blank variable, which we'll fill in below:
linesToRemove <- NULL
file.text2 <- file.text

for(marker in vector_of_singleline_markers){
	print(paste("Marker is",marker))
	matchingLines <- grep(marker, file.text, perl=TRUE)
	print("Matching lines are")
	print(matchingLines)
	if(length(matchingLines) > 1){ # If we have more than 1 lines that match the pattern
		numberOfMatchingPairsOfLines <- floor(length(matchingLines)/2)
		print(paste("There are", numberOfMatchingPairsOfLines, "pairs of lines for this search parameter."))
		# Combine pairs of lines, going through one pair at a time:
		for(i in numberOfMatchingPairsOfLines){
			# First, replace the first line of text with all of the combined text. Later, we'll remove the other original lines (after we've done this for all pairs of matching lines -- so that index numbers aren't messed up as we go):
			file.text2[matchingLines[1*i]] <- paste(file.text[matchingLines[1*i]:matchingLines[2*i]], collapse = "\n") # Combine everything between the two line numbers.
			linesToRemove <- c(linesToRemove, ((matchingLines[1*i]+1):matchingLines[2*i]))
			print("Lines to remove are")
			print(linesToRemove)
		}
	}
}

linesToRemove

# Now that we've looped through consolidating text, remove the original subsequent lines of text:

# First, consolidate the pairs of line numbers, such that if there are line number pairs that fit within other pairs, we just end up using the first set of pairs.

# Second, actually remove the lines:
file.text2 <- file.text2[-linesToRemove]

file.text2


#listOfLineNumbers <- grep('(</?code>|</?blockquote>|```)', file.text, perl=TRUE)

#listOfLineNumbers

#file.text[listOfLineNumbers]

# Conceptually following https://stat.ethz.ch/pipermail/r-help/2012-December/342539.html , which states:
# "findInterval returns a numeric vector indicating which bin(s) the argument vector element(s) fall(s) into. Items below the lower bound get a zero which means if the result is used as an index the there will be no item chosen for that value. Items above the maximal boundary get a value of n+1 where n is the number of bins."
col1 <- c(1,4,7,8,13)
col2 <- c(2,6,10,9,16)

findInterval(col2,col1,col2)

