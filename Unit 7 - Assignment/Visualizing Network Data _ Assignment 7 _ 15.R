# VISUALIZING NETWORK DATA

# Problem 1

edges = read.csv("edges.csv")
str(edges)
mean(edges)
users = read.csv("users.csv")
str(users)
146*2/59

table(users$locale)
table(users$locale, users$school)

table(users$gender, users$school)

# Problem 2

install.packages("igraph")
library(igraph)

?graph.data.frame()
g = graph.data.frame(edges, FALSE, users)

plot(g, vertex.size=5, vertex.label=NA)

degree(g)
table(degree(g))  # or
table(degree(g) >= 10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

summary(V(g)$size)

# Problem 3

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$school = "black"
V(g)$school[V(g)$gender == "A"] = "red"
V(g)$school[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$locale = "black"
V(g)$locale[V(g)$gender == "A"] = "red"
V(g)$locale[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

# Problem 4

?igraph.plotting
mark.border=rainbow(length(mark.groups), alpha=1)
plot(g, vertex.size=5, vertex.label=NA, edge.width = 2)
