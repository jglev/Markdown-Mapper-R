library(qgraph)

# This is following (loosely) http://www.r-bloggers.com/qgraph-version-1-1-0-and-how-to-simply-make-a-gui-using-rpanel/

data(big5)
data(big5groups)
qgraph(cor(big5),groups=big5groups,gui=TRUE)



Edges <- data.frame(
    from = rep(1:5,each=5),
		    to = rep(1:5,times=5),
				    thickness = abs(rnorm(25)))

Edges <- subset(Edges,from!=to)

qgraph(Edges,esize=5,gray=TRUE)

# Following http://stackoverflow.com/a/20255453

node.data <- data.frame(
    one=c("This is a test","And another test","Yet a third test"),
    two=c("And another test","Yet a third test","This is a test")
  )

# Testing hard-wrapping lines, following http://stat.ethz.ch/R-manual/R-patched/library/base/html/strwrap.html

x <- "This General Public License does not permit incorporating your program into proprietary programs."

# THIS WORKS FOR ARBITRARILY RESHAPING TEXT INTO BLOCKS, WITH \n SEPARATORS. This follows http://jeromyanglim.tumblr.com/post/33554853812/how-to-automatically-break-a-caption-over-multiple
node.data_hard_wrapped <- lapply(node.data[['one']], 
	function(x){
		paste(
			strwrap(x, width=5, simplify=TRUE)
			,
			collapse = "\n"
			)
		}
)

# See also the much clearer strwrap tutorial at http://www.endmemo.com/program/R/strwrap.php




library(igraph)

g <- graph.data.frame(
  node.data_hard_wrapped,
  directed=FALSE # make this true if your network has a direction in the links
)
plot(g)







