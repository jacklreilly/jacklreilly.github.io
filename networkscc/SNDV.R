#Title: SNDV.R
#Author: Jack Reilly
#Last Revision: February 13, 2023
#Purpose: Illustrate the supreme court agreement network; show different ways of illustrating networks
#Reference: Adapted from:
#Ognyanova, K. (2021) Network visualization with R. Retrieved from www.kateto.net/network-visualization

#Load the igraph library and set the working directory 
library("igraph")
setwd("your/working/directory")

#Load the node and edge data
nodes <- read.csv("supreme_nodes.csv", header=T, as.is=T)
links <- read.csv("supreme_edgelist.csv", header=T, as.is=T)

# Examine the data:
head(nodes)
head(links)

# Converting the data to an igraph object:
# The graph_from_data_frame() function takes two data frames: 'd' and 'vertices'.
# 'd' identifies the edgelist. It should start with two columns 
# containing the source and target node IDs for each network tie.
# 'vertices' identifies the node list. It should start with a column of node IDs.
# Any additional columns in either data frame are interpreted as attributes.
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 

#Can use igraph to convert to adjacency matrix
adjacency<-as_adjacency_matrix(net, attr="weight")

# Examine the resulting object:
class(net)
net 

# We can access the nodes, edges, and their attributes:
E(net)
V(net)
E(net)$weight
V(net)$seniority

# Or find specific nodes and edges by attribute:
# (that returns objects of type vertex sequence / edge sequence)
V(net)[president=="Obama"]
E(net)[weight>.9]

# You can also access elements of the network matrix directly
# using standard R matrix syntax
net[1,]
net[5,7]

# First attempt to plot the graph:
plot.igraph(net) # It . . . could be improved!

# If you want to lock layout across all networks, run this line 
# And then add "layout=lw" to each graph command
#lw <- layout_with_fr(net, weights=E(net)$weight)

#####
##1##  Add names to nodes
#####

#If we just add labels, things don't look so great still
#We must do more
plot.igraph(net, 
            vertex.label=V(net)$justice,
            vertex.label.family="Helvetica")

#Adding abbreviations rather than names
#Still not great, must do more with font and node size
plot.igraph(net, 
            vertex.label=V(net)$abbrev,
            vertex.label.family="Helvetica")

#If we're going to go with abbreviations, this is more aesthetic 
plot.igraph(net, 
            vertex.label=V(net)$abbrev, 
            vertex.label.family="Helvetica", 
            vertex.label.font=2, 
            vertex.label.color="black", 
            vertex.size=20)

# Let's and reduce the arrow size and remove the labels:
plot.igraph(net, 
            vertex.label=V(net)$abbrev,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=20)

#We might prefer names, though, for interpretatiblity 
plot.igraph(net, 
            vertex.label=V(net)$justice,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=48)


#####
##2##  Color by party
#####

## Generate colors based on party:
V(net)$color <- ifelse(V(net)$party=="Republican","coral1","cadetblue3")

plot.igraph(net, 
            vertex.label=V(net)$justice,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=48)

#####
##3##  Scale by centrality
#####

#Scale nodes by eigenvector centrality
V(net)$eigen<-evcent(net)$vector

plot.igraph(net, 
            vertex.label=V(net)$justice,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=V(net)$eigen*50)

#While we're at it, let's make our chief justice more apparent
V(net)$width<-ifelse(V(net)$type=="Chief",2,1)

plot.igraph(net, 
            vertex.label=V(net)$justice,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=V(net)$eigen*50,
            vertex.frame.width=V(net)$width)

#####
##4##  Edges
#####

#Add edge weights, multiplying the weight to scale it reasonably
plot.igraph(net, 
            edge.width=E(net)$weight*5,
            vertex.label=V(net)$justice,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=V(net)$eigen*50,
            vertex.frame.width=V(net)$width)

######
##E1##  Extras 1: Illustrating changing network layouts
######

par(mfrow=c(2,5), mar=c(1,1,1,1))
for (i in 1:10) {
  plot.igraph(net, 
              edge.width=E(net)$weight*2.5,
              vertex.label=V(net)$abbrev,
              vertex.label.family="Helvetica",
              vertex.label.font=2,
              vertex.label.color="black",
              vertex.size=V(net)$eigen*50,
              vertex.frame.width=V(net)$width) }

#We can choose which layout to use, we don't have to use the default
par(mfrow=c(1,1))
plot.igraph(net, 
            edge.width=E(net)$weight*10,
            vertex.label=V(net)$justice,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=V(net)$eigen*50,
            vertex.frame.width=V(net)$width,
            layout=layout_in_circle)

#We can set and store a particular layout to use
lw <- layout_with_fr(net, weights=E(net)$weight)

#And look - the layouts don't change anymore
par(mfrow=c(2,5), mar=c(1,1,1,1))
for (i in 1:10) {
  plot.igraph(net, 
              edge.width=E(net)$weight*2.5,
              vertex.label=V(net)$abbrev,
              vertex.label.family="Helvetica",
              vertex.label.font=2,
              vertex.label.color="black",
              vertex.size=V(net)$eigen*50,
              vertex.frame.width=V(net)$width,
              layout=lw) }

par(mfrow=c(1,1))
plot.igraph(net, 
            edge.width=E(net)$weight*5,
            vertex.label=V(net)$justice,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=V(net)$eigen*50,
            vertex.frame.width=V(net)$width,
            layout=lw)

#The popular spring layouts (even one that considers weights, like FR) still change run to run
#It can be nice to just run the layout a couple times until you find one you like, then store that layout to the drive
#and can save and reload that layout from disk if we wish across R sessions if we really like it
#the following two lines (when uncommented) write the layout to disk and then reload it
#write.csv(nice_layout, "layout.csv")
#lw <- as.matrix(read.csv("layout.csv"))

######
##E2##  Types of layouts
######

# The following illustrates all the kinds of layouts you can choose for our supreme court network
#Some look better than others

#First, grep all the kinds of layouts from the igraph package
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

#Graph them all
par(mfrow=c(3,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.width=E(net)$weight*2.5, 
       layout=l,vertex.label=NA, 
       vertex.frame.width=0,
 main=layout) }

######
##E3##  Highlighting particular nodes
######

par(mfrow=c(1,1))

#Mark nodes - just the basic command, without all our prior graphical niceties
plot(net, mark.groups=list(c(7,8,9), c(5,6)), 
     mark.col=c("#FFE4E1","#C5E5E7"), mark.border=NA)

#Mark Trump, Obama, and Bush Jr judges with red, blue, and yellow
#Justices are picked out by their order in the list

plot.igraph(net, mark.groups=list(c(7,8,9), c(5,6), c(1,4)), 
            mark.col=c("#FFE4E1","#C5E5E7", "#ECD89A"), mark.border=NA,
            edge.width=E(net)$weight*5,
            vertex.label=V(net)$justice,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=V(net)$eigen*50,
            vertex.frame.width=V(net)$width)

######
##E4##  More than one way to illustrate network information
######

#Choose color palette
palf <- colorRampPalette(c("white", "darkgreen")) 

#Draw a heatmap of agreement
heatmap(netm[,9:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )
