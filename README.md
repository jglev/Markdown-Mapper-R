# Markdown Mapper

Generate concept maps from plaintext notes.

A full explanation is available at [AD UNUM DATUM](http://adunumdatum.org/introducing-markdown-mapper.html "Ad Unum Datum: 'Introducing Markdown Mapper').

Distributed under the GPLv2 License (If you would like to redistribute the code under other license terms, please contact the author).

## Requirements

1. Download and install [R](http://cran.cnr.berkeley.edu/ "R Download page")
1. Download and install [Python v2.7+](https://www.python.org/ "Python"), including the `argparse` and `json` packages (which I think come installed by default in many Python distributions). (Note that Python may already be installed on your system, if you use Linux or Mac OSX).
1. **Download the Markdown Mapper script, "Markdown_Note_Grapher.R," from this repository.**
1. Open a terminal. Run the script with `Rscript /path/to/Markdown_Note_Grapher.R path/to/the_text_file_you_want_to_map.txt` or `R CMD BATCH /path/to/Markdown_Note_Grapher.R path/to/the_text_file_you_want_to_map.txt`. You could also create a shortcut to this command using, e.g., a `bash` ['Alias' file](https://en.wikipedia.org/wiki/Alias_%28Unix_shell%29 "Bash Alias explanation"). I added a line to my `.alias` file so that I can summon Markdown Mapper with the command `concept-map`.

## How to use it

Currently, Markdown Mapper is written in R, and designed to be run with the `Rscript` command (which is installed as part of R, and allows R scripts to be run from the command line). Because it is just an R script, it should work across all platforms. Do note, however, that it currently has only been tested on Linux, and does use an `X11` window to generate a pop-up quick-view graph.

The script takes plain-text notes, either from one or more files or from stdin, and turn them into a network graph. The script can export a quick-view network graph, or an [edge list](https://en.wikipedia.org/wiki/Adjacency_list) or [adjacency matrix](https://en.wikipedia.org/wiki/Adjacency_matrix) to import into other mapping or network analysis programs, such as [Gephi](https://gephi.github.io/ "Gephi") or [Visual Understanding Environment (VUE)](https://vue.tufts.edu/ "VUE"). Each line/paragraph of text is treated as a node in the network. Indentations (either with tabs or four spaces) in Markdown-style bullet (*) and numbered (1.) lists are used to determine relationships between lines, as are several special character combinations:
	--> Notes of extra importance <--
	{{ Note to self / Original Idea }}
	^^^ Explicit link to previous line.
	You can also tag (e.g., hashtag) any words or phrases. The default tag delimiters are +{tag} and @{tag}. Tags can contain spaces, +{like this}.

When using Markdown Mapper, you can list as many text files as you want. By default, the script will output the master tag list after looking across all of the files given to it, then will print a quick-view network graph to allow the user to see, at a glance, the network structure of the notes (This graph can be saved as a PDF with the `--quick-view-graph-name` flag).

**For full documentation, run the script with the `--help` flag.**
