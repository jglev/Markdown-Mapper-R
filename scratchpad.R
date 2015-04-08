file.text <- scan("~/Desktop/Remote_MountPoints/Home_Computer_Home_Directory/Desktop/Note-Taking_Network_Grapher/example2.txt", what = "list", sep = "\n", blank.lines.skip = TRUE)
#file.text <- scan("~/Desktop/Note-Taking_Network_Grapher/example2.txt", what = "list", sep = "\n", blank.lines.skip = TRUE)

file.text
file.text[1]
#do.call("rbind", file.text[28:29])
#paste(file.text[1:3], collapse = "\n")

#file_singleline_marker_locations <- grep('(```.*```|<code>.*</code>)', file.text, perl=TRUE)[1:2]

vector_of_singleline_start_markers <- c("<code>", "<blockquote>","```")

vector_of_singleline_end_markers <- c("</code>", "</blockquote>","```")

if(length(vector_of_singleline_start_markers) != length(vector_of_singleline_start_markers)){print("Not the same length")}


# Create a blank variable, which we'll fill in below:
linesToRemove <- NULL
file.text2 <- file.text

# We already know that both vectors are the same length, so we can just use the length of one of them here in calculating an index over which to iterate:
for(markerNumber in 1:length(vector_of_singleline_start_markers)){
	print(paste("Marker is",vector_of_singleline_start_markers[markerNumber],vector_of_singleline_end_markers[markerNumber]))
	
	# Only match lines that have an odd number of matches for each vector. This way, even if the start and end markers are identical, we control for there being a start and end marker on the same line. So we only want lines where there's a single start/end marker, or both a start and end marker, followed by another start marker, etc.:
	matchingStartLines <- which(sapply(regmatches(file.text2, gregexpr(vector_of_singleline_start_markers[markerNumber], file.text2)), length) %% 2 == 1) # % is modulo (i.e., remainder)
	#grep(vector_of_singleline_start_markers[markerNumber], file.text2, perl=TRUE)
	matchingEndLines <- grep(vector_of_singleline_end_markers[markerNumber], file.text2, perl=TRUE)
	
	# If the start and end markers are identical, the vectors of start- and end-lines will also be identical, which isn't right. In that case, we'll reconstruct the vectors by alternating between start and close.
	# NOTE WELL: This assumes that There won't be any lines that actually do have the multiline markers on a single line (e.g., '```Test```').
	if(vector_of_singleline_start_markers[markerNumber] == vector_of_singleline_end_markers[markerNumber]){
		#sapply(regmatches(file.text, gregexpr('```',file.text)), length)
		# Check if number is odd: `if (x %% 2) { # odd number }`
		matchingStartLines <- matchingStartLines[seq(1,length(matchingEndLines),2)] # Get the odd elements.
		matchingEndLines <- matchingEndLines[seq(2,length(matchingEndLines),2)] # Get the even elements.
	}
	
	print("Matching lines are")
	print(matchingStartLines)
	print(matchingEndLines)
	
	if(length(matchingStartLines) != length(matchingEndLines)){
		print("Start and end lines aren't matched.")	
	}else{
		
		minLengthOfMatchingLines <- min(length(matchingStartLines), length(matchingEndLines))
		
		if(length(minLengthOfMatchingLines) >= 1 ){ # If we have at least 1 line pair that match the pattern
			#numberOfMatchingPairsOfLines <- floor(length(matchingLines)/2)
			#print(paste("There are", numberOfMatchingPairsOfLines, "pairs of lines for this search parameter."))
			# Combine pairs of lines, going through one pair at a time:
			linesToRemove <- NULL # Wipe this from the last iteration. We'll fill it in again below.
			
			for(i in 1:minLengthOfMatchingLines){
				print(i)
				# First, replace the first line of text with all of the combined text. Later, we'll remove the other original lines (after we've done this for all pairs of matching lines -- so that index numbers aren't messed up as we go):
				
				# Make sure that the markers aren't on the same line -- if they are, we don't need to do anything further with them. Also, if the end line is on a higher line than the start line, there's probably something wrong, so we won't do anything with it, ether:
				if(matchingStartLines[i] == matchingEndLines[i] || matchingStartLines[i] > matchingEndLines[i]){
					print(paste("Start (", matchingStartLines[i], ") and end (", matchingEndLines[i], ") lines do not warrant further action. Passing over them..."), sep = "")
				} else { # If we SHOULD do something about the lines...
					file.text2[matchingStartLines[i]] <- paste(file.text2[matchingStartLines[i]:matchingEndLines[i]], collapse = "\n") # Combine everything between the two line numbers.
					linesToRemove <- c(linesToRemove,(matchingStartLines[i]+1):matchingEndLines[i])
					print("Lines to remove are")
					print(linesToRemove)
				}
			}
			
			# Remove the lines, if there are any to remove:
			if(length(linesToRemove > 0)){
				file.text2 <- file.text2[-linesToRemove]
			}
			
			print("File.text2 is now")
			print(file.text2)
		}
	}
}
# Now that we've looped through consolidating text, remove the original subsequent lines of text:

# First, consolidate the pairs of line numbers, such that if there are line number pairs that fit within other pairs, we just end up using the first set of pairs.

# Second, actually remove the lines:


file.text2


#listOfLineNumbers <- grep('(</?code>|</?blockquote>|```)', file.text, perl=TRUE)

#listOfLineNumbers

#file.text[listOfLineNumbers]

