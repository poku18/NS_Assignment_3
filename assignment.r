library(igraph)
options(digits=3)

advice <- matrix(scan("Advice.txt"),ncol=71,nrow=71,byrow=T) 
nodes <- read.csv("Lawyers.csv", header=T) 
head(nodes)
#Creating a graph object from the adjacency matrix
graph <- graph_from_adjacency_matrix(advice, mode = "directed")

#Question 1: Lawyer groups
table(nodes$Status) #36 Partners and 35 Associates
table(nodes$Gender) # 53 Men and 18 Women
table(nodes$Practice) #41 Litigation and 30 Corporate

#Information about the vertices may be added to a graph object as a vertex attribute. 
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
in_degree_hist = hist(in_deg,breaks = 30, labels=TRUE,col="pink")

out_deg <- degree(graph, mode = "out")
out_degree_hist = hist(out_deg,breaks=30, labels=TRUE,col="skyblue")


#Question 4. Create a scatterplot between these two types of degrees and 
# explain what this plot tells you. Compute the correlation between the in degree and out 
# degree and use its value to explain the pattern that you see in the scatterplot.

plot(in_deg, out_deg) 
cor(in_deg, out_deg)  #Correlation: 0.14

#Question 5. Attach the two degree vectors as the last two columns.
nodes[,10] <- in_deg
nodes[,11] <- out_deg
colnames(nodes)[10:11] <-c("indegree","outdegree")
print(head(nodes,5))

  #5(a) Draw scatterplots of each degree type against the seniority
plot(nodes$Seniority, nodes$indegree) 
plot(nodes$Seniority, nodes$outdegree)

cor(nodes$Seniority, nodes$indegree) #Correlation: -0.632
cor(nodes$Seniority, nodes$outdegree) #Correlation: 0.137

  #5(b) Draw side-by-side boxplots of the degrees against the status. 
aggregate(indegree ~ Status, data = nodes, mean) # Status 1: 17.69, 2: 7.29
aggregate(outdegree ~ Status, data = nodes, mean) #Status 1: 12.6, 2: 12.5

cor(nodes$Status,nodes$indegree)
cor(nodes$Status,nodes$outdegree)
plot(factor(nodes$Status), nodes$outdegree,boxwex = 0.3,col = "orange",
        border = "brown",
        horizontal = TRUE)
plot(factor(nodes$Status), nodes$indegree, boxwex = 0.3,col = "orange",
        border = "brown",
        horizontal = TRUE)

  #5(b) Draw side-by-side boxplots of the degrees against the practice categories
aggregate(indegree ~ Practice, data = nodes, mean) # Practice 1: 13.3, 2: 11.6
aggregate(outdegree ~ Practice, data = nodes, mean) # Practice 1: 12.8, 2: 12.2

plot(factor(nodes$Practice), nodes$outdegree, col = "orange",boxwex = 0.3,border = "brown",horizontal = TRUE) 

plot(factor(nodes$Practice), nodes$indegree, col = "orange",boxwex = 0.3,border = "brown",horizontal = TRUE, breaks=80) 

# Q6: Compute the eigenvector centrality scores for the vertices and store the values in an igraph object called eigen.cent
eigen.cent = eigen_centrality(graph)
nodes[,12] = eigen.cent$vector
colnames(nodes)[12] <-c("EigenValue")

# 6(a) 
print(head(nodes,5))

# 6(b)
print(nodes[,1][which(nodes$EigenValue > .8)])
print(nodes[,1][which(nodes$EigenValue ==1)])

# 6(c)
plot(nodes$Seniority, nodes$EigenValue)
cor(nodes$Seniority, nodes$EigenValue) #Correlation: -0.451

# 6(d)
plot(factor(nodes$Status), nodes$EigenValue, col = "orange",boxwex = 0.3,
     border = "brown", horizontal=TRUE)
aggregate(EigenValue ~ Status, data = nodes, mean) 

# 6(e)
plot(factor(nodes$Practice), nodes$EigenValue, col = "orange",boxwex = 0.3,
     border = "brown", horizontal=TRUE)
aggregate(EigenValue ~ Practice, data = nodes, mean)










