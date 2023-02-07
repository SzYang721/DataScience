# The function ug() from gRbase creates an undirected graph.
library(gRbase)
g1=ug(~a:b:e+a:c:e+c:d:e+c:g+d:f)
class(g1)
as(g1, "matrix")
library(Rgraphviz)
plot(g1)

# Graphs can also be defined using adjacency matrices. 
m=matrix(c(0,1,1,0,1,1,0,0,1,1,1,0,0,1,1,0,1,1,0,1,1,1,1,0), nrow = 5)
rownames(m)=colnames(m)=c("a", "b", "c", "d", "e")
m
as(m, "graphNEL")

#Graphs can be altered using addEdge() and removeEdge()
g1a=addEdge("a", "d", g1)
g1b=removeEdge("c", "d", g1)
par(mfrow=c(1, 3))
plot(g1, main="g1")
plot(g1a, main="g1a")
plot(g1b, main="g1b")

# We can create subgraph via subGraph() function
g1c=subGraph(c("b", "c", "d", "e"), g1)
par(mfrow=c(1, 3))
plot(g1, main="g1")
plot(g1c, main = "g1c")



# Some important concepts in graphical modelling: complete, clique, separate
# A subset of vertices is complete if all pairs of vertices in this subset are connected by an edge. A graph is complete if the set of vertices is complete. 
# A clique is a maximal complete subset, that is a complete subset which is not contained in a larger complete subset. 
library(RBGL)
is.complete(g1, set=c("a", "b", "e"))
is.complete(g1)
str(maxClique(g1))

# A subset S (vertices subsets) is said to separate subsets A and B if every path between a vertex in A and a vertex in B contains a vertex from S. 
g2=ug(~a:b:e+a:c:e+b:d:e+c:d:e)
plot(g2)
separates("a", "d", c("b", "c", "e"), g2)

# Factorization and dependence graph
# The dependence graph for a probability p(x)=p_AB(x_AB)p_BCD(x_BCD)p_CE(x_CE)p_DE(x_DE)
plot((g3=ug(~A:B+B:C:D+C:E+D:E)))

# Global Markov Property:(D, E) independent of A | conditional B
plot(g3)
separates(c("D", "E"), "A", "B", g3)





