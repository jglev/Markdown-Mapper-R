# Markdown Mapper

Generate concept maps from plaintext notes.

A full explanation is available at [AD UNUM DATUM](http://adunumdatum.org/introducing-markdown-mapper.html "Ad Unum Datum: 'Introducing Markdown Mapper'").

Distributed under the GPLv2 License (If you would like to redistribute the code under other license terms, please contact the author).

Current version: 0.1.2

## Recent Changes

* Code blocks and blockquotes are now supported with the `--single-line-beginning-marker` and `--single-line-closing-marker` flags, such that all lines within the text block are treated as a single node in the network.
* Debugging output now indicates if there are no edges to graph in the network. 

## Requirements

1. Download and install [R](http://cran.cnr.berkeley.edu/ "R Download page"). Due to library dependencies, you'll need R version ≥ 3.1.0.
	* If you are installing R for the first time, it may be necessary on some systems to run `R` once from the command line and install a package (any package) manually, using, e.g., `install.packages('argparse')`. This will allow R to prompt you if it needs to create a new directory for installing packages in the future — lacking this one-time prompt, the Markdown Mapper script might fail with an error message.
1. Download and install [Python v2.7+](https://www.python.org/ "Python") (according to the [R `argparse` GitHub page](https://github.com/trevorld/argparse "GitHub: argparse for R"), Python v3.2+ does work, as well), including the `argparse` and `json` packages (which I think come installed by default in many Python distributions). Note that Python may already be installed on your system, if you use Linux or Mac OSX.
1. **Download the Markdown Mapper script, "Markdown_Note_Grapher.R," from this repository.**
1. Open a terminal. Run the script with `Rscript /path/to/Markdown_Note_Grapher.R path/to/the_text_file_you_want_to_map.txt`. You could also create a shortcut to this command using, e.g., a `bash` ['Alias' file](https://en.wikipedia.org/wiki/Alias_%28Unix_shell%29 "Bash Alias explanation"). I added a line to my `.alias` file so that I can summon Markdown Mapper with the command `concept-map`.
	* Windows users may need to find the Rscript.exe executable in Program Files, since it apparently is not added automatically to the command prompt's [path](https://en.wikipedia.org/wiki/PATH_%28variable%29 "Command Line 'Path' explanation"). Alternatively, Windows users can install R in [Cygwin](https://en.wikipedia.org/wiki/Cygwin "Cygwin"), which provides a more Unix-like command prompt.
	* This script has only been tested on Linux systems; the pop-up quick-view graph functionality may not work on Windows, since it uses an `X11()` window.

The first time you run the script, it will likely install several R packages. Some of these can take quite a while to install. After that first time, however, the script runs much more quickly.

## How to use it

Currently, Markdown Mapper is written in R, and designed to be run with the `Rscript` command (which is installed as part of R, and allows R scripts to be run from the command line). Because it is just an R script, it should work across all platforms. Do note, however, that it currently has only been tested on Linux, and does use an `X11` window to generate a pop-up quick-view graph.

The script takes plain-text notes, either from one or more files or from stdin, and turn them into a network graph. The script can export a quick-view network graph, or an [edge list](https://en.wikipedia.org/wiki/Adjacency_list) or [adjacency matrix](https://en.wikipedia.org/wiki/Adjacency_matrix) to import into other mapping or network analysis programs, such as [Gephi](https://gephi.github.io/ "Gephi") or [Visual Understanding Environment (VUE)](https://vue.tufts.edu/ "VUE"). Each line/paragraph of text is treated as a node in the network. Indentations (either with tabs or four spaces) in Markdown-style bullet (*) and numbered (1.) lists are used to determine relationships between lines, as are several special character combinations:  
	--> Notes of extra importance <--  
	{{ Note to self / Original Idea }}  
	^^^ Explicit link to previous line.  
	You can also tag (e.g., hashtag) any words or phrases. The default tag delimiters are +{tag} and @{tag}. Tags can contain spaces, +{like this}.

When using Markdown Mapper, you can list as many text files as you want. By default, the script will output the master tag list after looking across all of the files given to it, then will print a quick-view network graph to allow the user to see, at a glance, the network structure of the notes (This graph can be saved as a PDF with the `--quick-view-graph-name` flag).

**For full documentation, run the script with the `--help` flag.**
