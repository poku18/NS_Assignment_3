library(igraph)

advice <- matrix(scan("Advice.txt"),ncol=71,nrow=71,byrow=T) 
nodes <- read.csv("Lawyers.csv", header=T) 
head(nodes)
#Creating a graph object from the adjacency matrix
graph <- graph_from_adjacency_matrix(advice, mode = "directed")

#Question 1: Lawyer groups
table(nodes$Status) #36 Partners and 35 Associates
table(nodes$Gender) # 53 Men and 18 Women
table(nodes$Practice) #41 Litigation and 30 Corporate

#Question 2: Information about the vertices may be added to a graph object as a vertex attribute. 
V(graph)$status <- nodes$Status 
V(graph)$practice <- nodes$Practice 

#Plot showing status groups
plot(graph, vertex.size=5, edge.arrow.size=0.5, 
     vertex.color=V(graph)$status) 
#Change arrow size, vertex color and layout
plot(graph, vertex.size=5, edge.arrow.size=0.18, 
     vertex.color=ifelse(V(graph)$status==1, "lightblue", "salmon"), layout=layout_nicely(graph)) 

#Plot showing practice types
plot(graph, vertex.size=5, edge.arrow.size=0.5, 
     vertex.color=V(graph)$practice) 
#Change arrow size, vertex color and layout
plot(graph, vertex.size=5, edge.arrow.size=0.18, 
vertex.color=ifelse(V(graph)$practice==1, "yellow", "violet"), layout=layout_with_lgl(graph)) 

#Question 3: Compute the in-degrees and out-degrees of the vertices.
# Make histograms of the two vectors to see the variation

in_deg <- degree(graph, mode = "in")
in_degree_hist = hist(in_deg,breaks = 50)

out_deg <- degree(graph, mode = "out")
out_degree_hist = hist(out_deg,breaks=30)


#Question 4. Create a scatterplot between these two types of degrees and 
# explain what this plot tells you. Compute the correlation between the in degree and out 
# degree and use its value to explain the pattern that you see in the scatterplot.

plot(in_deg, out_deg) 

#Question 5. Attach the two degree vectors as the last two columns.
nodes[,10] <- in_deg
nodes[,11] <- out_deg
colnames(nodes)[10:11] <-c("indegree","outdegree")
print(head(nodes))

# 5 (a) Draw scatterplots of each degree type against the seniority
plot(nodes$Seniority, nodes$indegree) 
plot(nodes$Seniority, nodes$outdegree)


