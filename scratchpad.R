#file.text <- scan("~/Desktop/Remote_MountPoints/Home_Computer_Home_Directory/Desktop/Note-Taking_Network_Grapher/example2.txt", what = "list", sep = "\n", blank.lines.skip = TRUE)
file.text <- scan("~/Desktop/Note-Taking_Network_Grapher/example2.txt", what = "list", sep = "\n", blank.lines.skip = TRUE)

file.text
file.text[1]
#do.call("rbind", file.text[28:29])
#paste(file.text[1:3], collapse = "\n")

#file_singleline_marker_locations <- grep('(```.*```|<code>.*</code>)', file.text, perl=TRUE)[1:2]

args$single_line_beginning_marker <- c("<code>", "<blockquote>","```")

args$single_line_closing_marker <- c("</code>", "</blockquote>","```")

if(length(args$single_line_beginning_marker) != length(args$single_line_beginning_marker)){print("Not the same length")}


full_code_block_marker_dictionary_to_use <- data.frame("StartingMarker" = args$single_line_beginning_marker, "EndingMarker" = args$single_line_closing_marker)

# Create a blank variable, which we'll fill in below:
file.text <- file.text

# We already know that both vectors are the same length, so we can just use the length of one of them here in calculating an index over which to iterate:
for(markerNumber in 1:length(args$single_line_beginning_marker)){
	if(args$verbose == TRUE){
		print(paste("Processing marker '",args$single_line_beginning_marker[markerNumber], "' ... '",args$single_line_closing_marker[markerNumber],"'...", sep = ""))
	}

	# Only match lines that have an odd number of matches for each vector. This way, even if the start and end markers are identical, we control for there being a start and end marker on the same line. So we only want lines where there's a single start/end marker, or both a start and end marker, followed by another start marker, etc.:
	matchingStartLines <- which(sapply(regmatches(file.text, gregexpr(args$single_line_beginning_marker[markerNumber], file.text)), length) %% 2 == 1) # % is modulo (i.e., remainder)
	#grep(args$single_line_beginning_marker[markerNumber], file.text, perl=TRUE)
	matchingEndLines <- which(sapply(regmatches(file.text, gregexpr(args$single_line_closing_marker[markerNumber], file.text)), length) %% 2 == 1)
	
	# If the start and end markers are identical, the vectors of start- and end-lines will also be identical, which isn't right. In that case, we'll reconstruct the vectors by alternating between start and close.
	if(args$single_line_beginning_marker[markerNumber] == args$single_line_closing_marker[markerNumber]){
		#sapply(regmatches(file.text, gregexpr('```',file.text)), length)
		# Check if number is odd: `if (x %% 2) { # odd number }`
		matchingStartLines <- matchingStartLines[seq(1,length(matchingEndLines),2)] # Get the odd elements.
		matchingEndLines <- matchingEndLines[seq(2,length(matchingEndLines),2)] # Get the even elements.
	}
	
	if(args$verbose == TRUE){
		print("The lines matching the Start marker are as follows:")
		print(matchingStartLines)
		print("The lines matching the Closing marker are as follows:")
		print(matchingEndLines)
	}
	
	#if(length(matchingStartLines) != length(matchingEndLines)){
	#	print("Start and end lines aren't matched.")	
	#}else{
		
		minLengthOfMatchingLines <- min(length(matchingStartLines), length(matchingEndLines))
		
		if(length(minLengthOfMatchingLines) >= 1 ){ # If we have at least 1 line pair that match the pattern
			# Combine pairs of lines, going through one pair at a time:
			linesToRemove <- NULL # Wipe this from the last iteration. We'll fill it in again below.
			
			for(i in 1:minLengthOfMatchingLines){
				# First, replace the first line of text with all of the combined text. Later, we'll remove the other original lines (after we've done this for all pairs of matching lines -- so that index numbers aren't messed up as we go):
				
				# Make sure that the markers aren't on the same line -- if they are, we don't need to do anything further with them. Also, if the end line is on a higher line than the start line, there's probably something wrong, so we won't do anything with it, ether:
				if(matchingStartLines[i] == matchingEndLines[i] || matchingStartLines[i] > matchingEndLines[i]){
					if(args$verbose == TRUE){
						print(paste("Start (", matchingStartLines[i], ") and end (", matchingEndLines[i], ") lines do not warrant further action. Passing over them..."), sep = "")
					}
				} else { # If we SHOULD do something about the lines...
					
					# Now that we've looped through consolidating text, remove the original subsequent lines of text:
					
					file.text[matchingStartLines[i]] <- paste(file.text[matchingStartLines[i]:matchingEndLines[i]], collapse = "\n") # Combine everything between the two line numbers.
					linesToRemove <- c(linesToRemove,(matchingStartLines[i]+1):matchingEndLines[i])
					if(args$verbose == TRUE){
						print("Removing the following now-vestigial lines:")
						print(linesToRemove)
					}
				}
			}
			
			# Remove the lines, if there are any to remove:
			if(length(linesToRemove > 0)){
				file.text <- file.text[-linesToRemove]
			}
			
			# The lines below are very nice for debugging, but would probably be overkill in normal output, even if the user did ask for verbosity. Thus, I'm commenting them out for now.
			#if(args$verbose == TRUE){	
			#	print("The current processed state of the input text is now as follows:e file is now as follows:")
			#	print(file.text)
			#}
		}
	#} # End of "If length of start and end lines is equal" block.
}


file.text


