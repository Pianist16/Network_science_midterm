setwd("C:/Users/User/Desktop/Чина/CEU/2nd Year/1.Fall2015/Statistical Methods/midterm")

#### 1 ####

library(igraph)
m1 <- read.graph("marvel.net", format="pajek") #complete initial bipartite graph
V(m1)$name=V(m1)$id # adding name attribute
m2 <- bipartite.projection(m1, multiplicity = TRUE) # to keep weights normal in projected graphs
m3 <- m2$proj1 # projection of network on the character set of nodes
vcount(m3)
ecount(m3)

### vertex centrality
degree(m3)
mean(degree(m3)) # average degree = 51.88622
min(degree(m3)) # 0
which.min(degree(m3)) # Berserker II
max(degree(m3)) # 1933
which.max(degree(m3)) # Captain America
betweenness(m3, v=V(m3), directed=F)
max(betweenness(m3)) # 889404.7
which.max(betweenness(m3)) # Havok/Alex Summers
min(betweenness(m3)) # 0
which.min(betweenness(m3)) # Abominatrix

### degree distribution
plot(degree.distribution(m3), xlab="degree", ylab="frequency", main = "Degree Distribution")
hist(degree.distribution(m3), col="blue",
     xlab = "Degree", ylab = "Frequency",
     main = "Degree Distribution")

dd <- degree.distribution(m3)
d <- 1:(max(degree(m3))-1)
ind <- (dd!= 0)
plot(d[ind], dd[ind],log = "xy",col="blue",
     xlab = c("log-degree"), ylab = c("log-frequency"),
     main = "Degree Distribution")

### clustering coefficient
transitivity(m3)

### edge centrality
eb <- edge.betweenness(m3, e=E(m3), directed = F)
E(m3)[order(eb, decreasing=T)[1:3]]
max(eb) # 49684.43
which.max(eb)
  
### density
n <- vcount(m3)
l <- ecount(m3)
density <- l/(n*(n-1)/2)
graph.density(m3) # ~0.008

### components
is.connected(m3)
cl.m3 <- components(m3)
cl.m3
average.path.length(m3)

#### 2 ####

ff.graph <- induced.subgraph(m1, vids=unlist(neighborhood(m1,order=1,nodes=(grep("^FF [0-9]|^FF [0-9]+/", V(m1)$id)))))
ux.graph <- induced.subgraph(m1, vids=unlist(neighborhood(m1,order=1,nodes=(grep("^UX [0-9]|^UX [0-9]+/", V(m1)$id)))))
asm.graph <- induced.subgraph(m1, vids=unlist(neighborhood(m1,order=1,nodes=(grep("^ASM [0-9]|^ASM [0-9]+/", V(m1)$id)))))

#### 3 ####

ff.1 <- induced.subgraph(graph=ff.graph,vids=unlist(neighborhood(ff.graph,order=1,nodes=grep("^FF 1$|^FF 1/[1-9]$",V(ff.graph)$id))))
ff.3 <- induced.subgraph(graph=ff.graph,vids=unlist(neighborhood(ff.graph,order=1,nodes=grep("^FF [1-3]$|^FF [1-3]/[1-9]$",V(ff.graph)$id))))
ff.10 <-induced.subgraph(graph=ff.graph,vids=unlist(neighborhood(ff.graph,order=1,nodes=grep("^FF [1-9]$|^FF [1-9]/[1-9]$|^FF 10$|^FF 10/[1-9]$",V(ff.graph)$id))))
ff.30 <- induced.subgraph(graph=ff.graph,vids=unlist(neighborhood(ff.graph,order=1,nodes=grep("^FF [1-9]$|^FF [1-9]/[1-9]$|^FF [1-2][0-9]$|^FF [1-2][0-9]/[1-9]$|^FF 30$|^FF 30/[1-9]$",V(ff.graph)$id))))
ff.100 <- induced.subgraph(graph=ff.graph,vids=unlist(neighborhood(ff.graph,order=1,nodes=grep("^FF [1-9]$|^FF [1-9]/[1-9]$|^FF [1-9][0-9]$|^FF [1-9][0-9]/[1-9]$|^FF 100$|^FF 100/[1-9]$",V(ff.graph)$id))))
ff.300 <- induced.subgraph(graph=ff.graph,vids=unlist(neighborhood(ff.graph,order=1,nodes=grep("^FF [1-9]$|^FF [1-9][0-9]$|^FF [1-2][0-9][0-9]$|^FF 300$|^FF [1-9]/[1-9]$|^FF [1-9][0-9]/[1-9]$|^FF [1-2][0-9][0-9]/[1-9]$|^FF 300/[1-9]$",V(ff.graph)$id))))

ux.1 <- induced.subgraph(graph=ux.graph,vids=unlist(neighborhood(ux.graph,order=1,nodes=grep("^UX 1$|^UX 1/[1-9]$",V(ux.graph)$id))))
ux.3 <- induced.subgraph(graph=ux.graph,vids=unlist(neighborhood(ux.graph,order=1,nodes=grep("^UX [1-3]$|^UX [1-3]/[1-9]$",V(ux.graph)$id))))
ux.10 <-induced.subgraph(graph=ux.graph,vids=unlist(neighborhood(ux.graph,order=1,nodes=grep("^UX [1-9]$|^UX [1-9]/[1-9]$|^UX 10$|^UX 10/[1-9]$",V(ux.graph)$id))))
ux.30 <- induced.subgraph(graph=ux.graph,vids=unlist(neighborhood(ux.graph,order=1,nodes=grep("^UX [1-9]$|^UX [1-9]/[1-9]$|^UX [1-2][0-9]$|^UX [1-2][0-9]/[1-9]$|^UX 30$|^UX 30/[1-9]$",V(ux.graph)$id))))
ux.100 <- induced.subgraph(graph=ux.graph,vids=unlist(neighborhood(ux.graph,order=1,nodes=grep("^UX [1-9]$|^UX [1-9]/[1-9]$|^UX [1-9][0-9]$|^UX [1-9][0-9]/[1-9]$|^UX 100$|^UX 100/[1-9]$",V(ux.graph)$id))))
ux.300 <- induced.subgraph(graph=ux.graph,vids=unlist(neighborhood(ux.graph,order=1,nodes=grep("^UX [1-9]$|^UX [1-9][0-9]$|^UX [1-2][0-9][0-9]$|^UX 300$|^UX [1-9]/[1-9]$|^UX [1-9][0-9]/[1-9]$|^UX [1-2][0-9][0-9]/[1-9]$|^UX 300/[1-9]$",V(ux.graph)$id))))

asm.1 <- induced.subgraph(graph=asm.graph,vids=unlist(neighborhood(asm.graph,order=1,nodes=grep("^ASM 1$|^ASM 1/[1-9]$",V(asm.graph)$id))))
asm.3 <- induced.subgraph(graph=asm.graph,vids=unlist(neighborhood(asm.graph,order=1,nodes=grep("^ASM [1-3]$|^ASM [1-3]/[1-9]$",V(asm.graph)$id))))
asm.10 <-induced.subgraph(graph=asm.graph,vids=unlist(neighborhood(asm.graph,order=1,nodes=grep("^ASM [1-9]$|^ASM [1-9]/[1-9]$|^ASM 10$|^ASM 10/[1-9]$",V(asm.graph)$id))))
asm.30 <- induced.subgraph(graph=asm.graph,vids=unlist(neighborhood(asm.graph,order=1,nodes=grep("^ASM [1-9]$|^ASM [1-9]/[1-9]$|^ASM [1-2][0-9]$|^ASM [1-2][0-9]/[1-9]$|^ASM 30$|^ASM 30/[1-9]$",V(asm.graph)$id))))
asm.100 <- induced.subgraph(graph=asm.graph,vids=unlist(neighborhood(asm.graph,order=1,nodes=grep("^ASM [1-9]$|^ASM [1-9]/[1-9]$|^ASM [1-9][0-9]$|^ASM [1-9][0-9]/[1-9]$|^ASM 100$|^ASM 100/[1-9]$",V(asm.graph)$id))))
asm.300 <- induced.subgraph(graph=asm.graph,vids=unlist(neighborhood(asm.graph,order=1,nodes=grep("^ASM [1-9]$|^ASM [1-9][0-9]$|^ASM [1-2][0-9][0-9]$|^ASM 300$|^ASM [1-9]/[1-9]$|^ASM [1-9][0-9]/[1-9]$|^ASM [1-2][0-9][0-9]/[1-9]$|^ASM 300/[1-9]$",V(asm.graph)$id))))

#### 4 ####

ff.1.proj <- bipartite.projection(ff.1, multiplicity = TRUE)
ff.3.proj <- bipartite.projection(ff.3, multiplicity = TRUE)
ff.10.proj <- bipartite.projection(ff.10, multiplicity = TRUE)
ff.30.proj <- bipartite.projection(ff.30, multiplicity = TRUE)
ff.100.proj <- bipartite.projection(ff.100, multiplicity = TRUE)
ff.300.proj <- bipartite.projection(ff.300, multiplicity = TRUE)

ff.1.proj.char <- ff.1.proj$proj1
ff.3.proj.char <- ff.3.proj$proj1
ff.10.proj.char <- ff.10.proj$proj1
ff.30.proj.char <- ff.30.proj$proj1
ff.100.proj.char <- ff.100.proj$proj1
ff.300.proj.char <- ff.300.proj$proj1

ux.1.proj <- bipartite.projection(ux.1, multiplicity = TRUE)
ux.3.proj <- bipartite.projection(ux.3, multiplicity = TRUE)
ux.10.proj <- bipartite.projection(ux.10, multiplicity = TRUE)
ux.30.proj <- bipartite.projection(ux.30, multiplicity = TRUE)
ux.100.proj <- bipartite.projection(ux.100, multiplicity = TRUE)
ux.300.proj <- bipartite.projection(ux.300, multiplicity = TRUE)

ux.1.proj.char <- ux.1.proj$proj1
ux.3.proj.char <- ux.3.proj$proj1
ux.10.proj.char <- ux.10.proj$proj1
ux.30.proj.char <- ux.30.proj$proj1
ux.100.proj.char <- ux.100.proj$proj1
ux.300.proj.char <- ux.300.proj$proj1

asm.1.proj <- bipartite.projection(asm.1, multiplicity = TRUE)
asm.3.proj <- bipartite.projection(asm.3, multiplicity = TRUE)
asm.10.proj <- bipartite.projection(asm.10, multiplicity = TRUE)
asm.30.proj <- bipartite.projection(asm.30, multiplicity = TRUE)
asm.100.proj <- bipartite.projection(asm.100, multiplicity = TRUE)
asm.300.proj <- bipartite.projection(asm.300, multiplicity = TRUE)

asm.1.proj.char <- asm.1.proj$proj1
asm.3.proj.char <- asm.3.proj$proj1
asm.10.proj.char <- asm.10.proj$proj1
asm.30.proj.char <- asm.30.proj$proj1
asm.100.proj.char <- asm.100.proj$proj1
asm.300.proj.char <- asm.300.proj$proj1

mean(degree(ff.1.proj.char)) # 4
mean(degree(ff.3.proj.char)) # 4.(6)
mean(degree(ff.10.proj.char)) # 8
mean(degree(ff.30.proj.char)) # 12.64
mean(degree(ff.100.proj.char)) # 19.32727
mean(degree(ff.300.proj.char)) # 24.27615

mean(degree(ux.1.proj.char)) # 6
mean(degree(ux.3.proj.char)) # 8
mean(degree(ux.10.proj.char)) # 14.96552
mean(degree(ux.30.proj.char)) # 17.73134
mean(degree(ux.100.proj.char)) # 23.98649
mean(degree(ux.300.proj.char)) # 47.02845

mean(degree(asm.1.proj.char)) # 5
mean(degree(asm.3.proj.char)) # 6.75
mean(degree(asm.10.proj.char)) # 10.83871
mean(degree(asm.30.proj.char)) # 19.48276
mean(degree(asm.100.proj.char)) # 22.96774
mean(degree(asm.300.proj.char)) # 25.51128

degree(ff.300.proj.char)[order(degree(ff.300.proj.char), decreasing=TRUE)[1:5]]
##HUMAN TORCH/JOHNNY S MR. FANTASTIC/REED R INVISIBLE WOMAN/SUE  THING/BENJAMIN J. GR RICHARDS, FRANKLIN B 
##       237                  236                  231                  224                  131 


degree(ff.1.proj.char,"HUMAN TORCH/JOHNNY S") # 4
degree(ff.3.proj.char,"HUMAN TORCH/JOHNNY S") # 5
degree(ff.10.proj.char,"HUMAN TORCH/JOHNNY S") # 13
degree(ff.30.proj.char,"HUMAN TORCH/JOHNNY S") # 49
degree(ff.100.proj.char,"HUMAN TORCH/JOHNNY S") # 109
degree(ff.300.proj.char,"HUMAN TORCH/JOHNNY S") # 237

degree(ff.1.proj.char,"MR. FANTASTIC/REED R") # 4
degree(ff.3.proj.char,"MR. FANTASTIC/REED R") # 5
degree(ff.10.proj.char,"MR. FANTASTIC/REED R") # 13
degree(ff.30.proj.char,"MR. FANTASTIC/REED R") # 49
degree(ff.100.proj.char,"MR. FANTASTIC/REED R") # 109
degree(ff.300.proj.char,"MR. FANTASTIC/REED R") # 236

degree(ff.1.proj.char,"INVISIBLE WOMAN/SUE ") # 4
degree(ff.3.proj.char,"INVISIBLE WOMAN/SUE ") # 5
degree(ff.10.proj.char,"INVISIBLE WOMAN/SUE ") # 13
degree(ff.30.proj.char,"INVISIBLE WOMAN/SUE ") # 49
degree(ff.100.proj.char,"INVISIBLE WOMAN/SUE ") # 108
degree(ff.300.proj.char,"INVISIBLE WOMAN/SUE ") # 231

degree(ff.1.proj.char,"THING/BENJAMIN J. GR") # 4
degree(ff.3.proj.char,"THING/BENJAMIN J. GR") # 5
degree(ff.10.proj.char,"THING/BENJAMIN J. GR") # 13
degree(ff.30.proj.char,"THING/BENJAMIN J. GR") # 49
degree(ff.100.proj.char,"THING/BENJAMIN J. GR") # 109
degree(ff.300.proj.char,"THING/BENJAMIN J. GR") # 224

degree(ff.1.proj.char,"RICHARDS, FRANKLIN B") # is not in first issues
degree(ff.3.proj.char,"RICHARDS, FRANKLIN B") # is not in these issues
degree(ff.10.proj.char,"RICHARDS, FRANKLIN B") # is not in these issues
degree(ff.30.proj.char,"RICHARDS, FRANKLIN B") # is not in these issues
degree(ff.100.proj.char,"RICHARDS, FRANKLIN B") # 25
degree(ff.300.proj.char,"RICHARDS, FRANKLIN B") # 131

degree(ux.300.proj.char)[order(degree(ux.300.proj.char), decreasing=TRUE)[1:5]]
##CYCLOPS/SCOTT SUMMER STORM/ORORO MUNROE S     WOLVERINE/LOGAN  COLOSSUS II/PETER RA PROFESSOR X/CHARLES  
##       361                  358                     355               352                  330 


degree(ux.1.proj.char,"CYCLOPS/SCOTT SUMMER") # 6
degree(ux.3.proj.char,"CYCLOPS/SCOTT SUMMER") # 9
degree(ux.10.proj.char,"CYCLOPS/SCOTT SUMMER") # 28
degree(ux.30.proj.char,"CYCLOPS/SCOTT SUMMER") # 66
degree(ux.100.proj.char,"CYCLOPS/SCOTT SUMMER") # 146
degree(ux.300.proj.char,"CYCLOPS/SCOTT SUMMER") # 361

degree(ux.1.proj.char,"STORM/ORORO MUNROE S") # is not in the first issues
degree(ux.3.proj.char,"STORM/ORORO MUNROE S") # is not in these issues
degree(ux.10.proj.char,"STORM/ORORO MUNROE S") # is not in these issues
degree(ux.30.proj.char,"STORM/ORORO MUNROE S") # is not in these issues
degree(ux.100.proj.char,"STORM/ORORO MUNROE S") # 51
degree(ux.300.proj.char,"STORM/ORORO MUNROE S") # 358

degree(ux.1.proj.char,"WOLVERINE/LOGAN ") # is not in the first issues
degree(ux.3.proj.char,"WOLVERINE/LOGAN ") # is not in these issues
degree(ux.10.proj.char,"WOLVERINE/LOGAN ") # is not in these issues
degree(ux.30.proj.char,"WOLVERINE/LOGAN ") # is not in these issues
degree(ux.100.proj.char,"WOLVERINE/LOGAN ") # 51
degree(ux.300.proj.char,"WOLVERINE/LOGAN ") # 355

degree(ux.1.proj.char,"COLOSSUS II/PETER RA") # is not in the first issues
degree(ux.3.proj.char,"COLOSSUS II/PETER RA") # is not in these issues
degree(ux.10.proj.char,"COLOSSUS II/PETER RA") # is not in these issues
degree(ux.30.proj.char,"COLOSSUS II/PETER RA") # is not in these issues
degree(ux.100.proj.char,"COLOSSUS II/PETER RA") # 51
degree(ux.300.proj.char,"COLOSSUS II/PETER RA") # 352

degree(ux.1.proj.char,"PROFESSOR X/CHARLES ") # 6
degree(ux.3.proj.char,"PROFESSOR X/CHARLES ") # 9
degree(ux.10.proj.char,"PROFESSOR X/CHARLES ") # 28
degree(ux.30.proj.char,"PROFESSOR X/CHARLES ") # 66
degree(ux.100.proj.char,"PROFESSOR X/CHARLES ") # 121
degree(ux.300.proj.char,"PROFESSOR X/CHARLES ") # 330

degree(asm.300.proj.char)[order(degree(asm.300.proj.char), decreasing=TRUE)[1:5]]
##SPIDER-MAN/PETER PAR    JAMESON, J. JONAH          PARKER, MAY       ROBERTSON, JOE   LEEDS, BETTY BRANT 
##        265                  197                  167                  167                  163


degree(asm.1.proj.char,"SPIDER-MAN/PETER PAR") # 9
degree(asm.3.proj.char,"SPIDER-MAN/PETER PAR") # 15
degree(asm.10.proj.char,"SPIDER-MAN/PETER PAR") # 30
degree(asm.30.proj.char,"SPIDER-MAN/PETER PAR") # 57
degree(asm.100.proj.char,"SPIDER-MAN/PETER PAR") # 92
degree(asm.300.proj.char,"SPIDER-MAN/PETER PAR") # 265

degree(asm.1.proj.char,"JAMESON, J. JONAH") # 5
degree(asm.3.proj.char,"JAMESON, J. JONAH") # 12
degree(asm.10.proj.char,"JAMESON, J. JONAH") # 28
degree(asm.30.proj.char,"JAMESON, J. JONAH") # 56
degree(asm.100.proj.char,"JAMESON, J. JONAH") # 90
degree(asm.300.proj.char,"JAMESON, J. JONAH") # 197

degree(asm.1.proj.char,"PARKER, MAY") # 5
degree(asm.3.proj.char,"PARKER, MAY") # 8
degree(asm.10.proj.char,"PARKER, MAY") # 25
degree(asm.30.proj.char,"PARKER, MAY") # 55
degree(asm.100.proj.char,"PARKER, MAY") # 75
degree(asm.300.proj.char,"PARKER, MAY") # 167

degree(asm.1.proj.char,"ROBERTSON, JOE") # is not in the first issues
degree(asm.3.proj.char,"ROBERTSON, JOE") # is not in these issues
degree(ams.10.proj.char,"ROBERTSON, JOE") # is not in these issues
degree(asm.30.proj.char,"ROBERTSON, JOE") # is not in these issues
degree(asm.100.proj.char,"ROBERTSON, JOE") # 47
degree(asm.300.proj.char,"ROBERTSON, JOE") # 167

degree(asm.1.proj.char,"LEEDS, BETTY BRANT") # is not in the first issues
degree(asm.3.proj.char,"LEEDS, BETTY BRANT") # is not in these issues
degree(asm.10.proj.char,"LEEDS, BETTY BRANT") # 21
degree(asm.30.proj.char,"LEEDS, BETTY BRANT") # 52
degree(asm.100.proj.char,"LEEDS, BETTY BRANT") # 72
degree(asm.300.proj.char,"LEEDS, BETTY BRANT") # 163

#### 5 ####

diameter(ff.1.proj.char) # 1
diameter(ff.3.proj.char) # 2
diameter(ff.10.proj.char) # 3
diameter(ff.30.proj.char) # 5
diameter(ff.100.proj.char) # 5
diameter(ff.300.proj.char) # 6

diameter(ux.1.proj.char) # 1
diameter(ux.3.proj.char) # 2
diameter(ux.10.proj.char) # 3
diameter(ux.30.proj.char) # 5
diameter(ux.100.proj.char) # 5
diameter(ux.300.proj.char) # 5

diameter(asm.1.proj.char) # 2
diameter(asm.3.proj.char) # 3
diameter(asm.10.proj.char) # 3
diameter(asm.30.proj.char) # 3
diameter(asm.100.proj.char) # 4
diameter(asm.300.proj.char) # 4

#### 6 ####

### k-core maximal FF

core.ff.1 <- coreness(ff.1.proj.char)
core.ff.3 <- coreness(ff.3.proj.char)
core.ff.10 <- coreness(ff.10.proj.char)
core.ff.30 <- coreness(ff.30.proj.char)
core.ff.100 <- coreness(ff.100.proj.char)
core.ff.300 <- coreness(ff.300.proj.char)

max.core.ff.1 <- max(coreness(ff.1.proj.char)) # 4 this is k in k-core
max.core.ff.3 <- max(coreness(ff.3.proj.char)) # 4
max.core.ff.10 <- max(coreness(ff.10.proj.char)) # 7
max.core.ff.30 <- max(coreness(ff.30.proj.char)) # 12
max.core.ff.100 <- max(coreness(ff.100.proj.char)) # 21
max.core.ff.300 <- max(coreness(ff.300.proj.char)) # 23

vertices.max.core.ff.1 <- which(core.ff.1 == max.core.ff.1)
kcore.ff.1 <- induced.subgraph(graph=ff.1.proj.char, vids=vertices.max.core.ff.1)
vertices.max.core.ff.3 <- which(core.ff.3 == max.core.ff.3)
kcore.ff.3 <- induced.subgraph(graph=ff.3.proj.char, vids=vertices.max.core.ff.3)
vertices.max.core.ff.10 <- which(core.ff.10 == max.core.ff.10)
kcore.ff.10 <- induced.subgraph(graph=ff.10.proj.char, vids=vertices.max.core.ff.10)
vertices.max.core.ff.30 <- which(core.ff.30 == max.core.ff.30)
kcore.ff.30 <- induced.subgraph(graph=ff.30.proj.char, vids=vertices.max.core.ff.30)
vertices.max.core.ff.100 <- which(core.ff.100 == max.core.ff.100)
kcore.ff.100 <- induced.subgraph(graph=ff.100.proj.char, vids=vertices.max.core.ff.100)
vertices.max.core.ff.300 <- which(core.ff.300 == max.core.ff.300)
kcore.ff.300 <- induced.subgraph(graph=ff.300.proj.char, vids=vertices.max.core.ff.300)

vcount(kcore.ff.1) # 5
vcount(kcore.ff.3) # 6
vcount(kcore.ff.10) # 8
vcount(kcore.ff.30) # 13
vcount(kcore.ff.100) # 22
vcount(kcore.ff.300) # 35

v.ff.1 <- vertex_attr(kcore.ff.1)$name # composition of the k-core
v.ff.3 <- vertex_attr(kcore.ff.3)$name
v.ff.10 <- vertex_attr(kcore.ff.10)$name
v.ff.30 <- vertex_attr(kcore.ff.30)$name
v.ff.100 <- vertex_attr(kcore.ff.100)$name
v.ff.300 <- vertex_attr(kcore.ff.300)$name

### k-core maximal UX

core.ux.1 <- coreness(ux.1.proj.char)
core.ux.3 <- coreness(ux.3.proj.char)
core.ux.10 <- coreness(ux.10.proj.char)
core.ux.30 <- coreness(ux.30.proj.char)
core.ux.100 <- coreness(ux.100.proj.char)
core.ux.300 <- coreness(ux.300.proj.char)

max.core.ux.1 <- max(coreness(ux.1.proj.char)) # 6 this is k in k-core
max.core.ux.3 <- max(coreness(ux.3.proj.char)) # 7
max.core.ux.10 <- max(coreness(ux.10.proj.char)) # 13
max.core.ux.30 <- max(coreness(ux.30.proj.char)) # 14
max.core.ux.100 <- max(coreness(ux.100.proj.char)) # 24
max.core.ux.300 <- max(coreness(ux.300.proj.char)) # 40

vertices.max.core.ux.1 <- which(core.ux.1 == max.core.ux.1)
kcore.ux.1 <- induced.subgraph(graph=ux.1.proj.char, vids=vertices.max.core.ux.1)
vertices.max.core.ux.3 <- which(core.ux.3 == max.core.ux.3)
kcore.ux.3 <- induced.subgraph(graph=ux.3.proj.char, vids=vertices.max.core.ux.3)
vertices.max.core.ux.10 <- which(core.ux.10 == max.core.ux.10)
kcore.ux.10 <- induced.subgraph(graph=ux.10.proj.char, vids=vertices.max.core.ux.10)
vertices.max.core.ux.30 <- which(core.ux.30 == max.core.ux.30)
kcore.ux.30 <- induced.subgraph(graph=ux.30.proj.char, vids=vertices.max.core.ux.30)
vertices.max.core.ux.100 <- which(core.ux.100 == max.core.ux.100)
kcore.ux.100 <- induced.subgraph(graph=ux.100.proj.char, vids=vertices.max.core.ux.100)
vertices.max.core.ux.300 <- which(core.ux.300 == max.core.ux.300)
kcore.ux.300 <- induced.subgraph(graph=ux.300.proj.char, vids=vertices.max.core.ux.300)

vcount(kcore.ux.1) # 7
vcount(kcore.ux.3) # 8
vcount(kcore.ux.10) # 14
vcount(kcore.ux.30) # 15
vcount(kcore.ux.100) # 42
vcount(kcore.ux.300) # 118

v.ux.1 <- vertex_attr(kcore.ux.1)$name # composition of the k-core
v.ix.3 <- vertex_attr(kcore.ux.3)$name
v.ux.10 <- vertex_attr(kcore.ux.10)$name
v.ux.30 <- vertex_attr(kcore.ux.30)$name
v.ux.100 <- vertex_attr(kcore.ux.100)$name
v.ux.300 <- vertex_attr(kcore.ux.300)$name

### k-core maximal ASM

core.asm.1 <- coreness(asm.1.proj.char)
core.asm.3 <- coreness(asm.3.proj.char)
core.asm.10 <- coreness(asm.10.proj.char)
core.asm.30 <- coreness(asm.30.proj.char)
core.asm.100 <- coreness(asm.100.proj.char)
core.asm.300 <- coreness(asm.300.proj.char)

max.core.asm.1 <- max(coreness(asm.1.proj.char)) # 5 this is k in k-core
max.core.asm.3 <- max(coreness(asm.3.proj.char)) # 6
max.core.asm.10 <- max(coreness(asm.10.proj.char)) # 9
max.core.asm.30 <- max(coreness(asm.30.proj.char)) # 23
max.core.asm.100 <- max(coreness(asm.100.proj.char)) # 23
max.core.asm.300 <- max(coreness(asm.300.proj.char)) # 24

vertices.max.core.asm.1 <- which(core.asm.1 == max.core.asm.1)
kcore.asm.1 <- induced.subgraph(graph=asm.1.proj.char, vids=vertices.max.core.asm.1)
vertices.max.core.asm.3 <- which(core.asm.3 == max.core.asm.3)
kcore.asm.3 <- induced.subgraph(graph=asm.3.proj.char, vids=vertices.max.core.asm.3)
vertices.max.core.asm.10 <- which(core.asm.10 == max.core.asm.10)
kcore.asm.10 <- induced.subgraph(graph=asm.10.proj.char, vids=vertices.max.core.asm.10)
vertices.max.core.asm.30 <- which(core.asm.30 == max.core.asm.30)
kcore.asm.30 <- induced.subgraph(graph=asm.30.proj.char, vids=vertices.max.core.asm.30)
vertices.max.core.asm.100 <- which(core.asm.100 == max.core.asm.100)
kcore.asm.100 <- induced.subgraph(graph=asm.100.proj.char, vids=vertices.max.core.asm.100)
vertices.max.core.asm.300 <- which(core.asm.300 == max.core.asm.300)
kcore.asm.300 <- induced.subgraph(graph=asm.300.proj.char, vids=vertices.max.core.asm.300)

vcount(kcore.asm.1) # 6
vcount(kcore.asm.3) # 7
vcount(kcore.asm.10) # 15
vcount(kcore.asm.30) # 24
vcount(kcore.asm.100) # 24
vcount(kcore.asm.300) # 54

v.asm.1 <- vertex_attr(kcore.asm.1)$name # composition of the k-core
v.asm.3 <- vertex_attr(kcore.asm.3)$name
v.asm.10 <- vertex_attr(kcore.asm.10)$name
v.asm.30 <- vertex_attr(kcore.asm.30)$name
v.asm.100 <- vertex_attr(kcore.asm.100)$name
v.asm.300 <- vertex_attr(kcore.asm.300)$name

### plotting k as a function of number of issues

k.asm <- c(5,6,9,23,23,24)
k.ux <- c(6,7,13,14,24,40)
k.ff <- c(4,4,7,12,21,23)
issues <- c(1,3,10,30,100,300)

plot(issues, k.ff,type = "l", xlab = "issue", ylab = "k", main = "k against issues - FF")
plot(issues, k.ux,type = "l", xlab = "issue", ylab = "k", main = "k against issues - UX")
plot(issues, k.asm,type = "l", xlab = "issue", ylab = "k", main = "k against issues - ASM")

### plotting degrees as a function of the group of issues for wolverine, cyclops, joe robinson,spider-man, franklin b and johnny s

d.wolverine <- c(0,0,0,0,51, 355)
d.cyclops <- c(6,9,28,66,146,361)
d.joerob <- c(0,0,0,0,47,167)
d.spidman <- c(9,15,30,57,92,265)
d.franklin <- c(0,0,0,0,25,131)
d.johnny <- c(4,5,13,49,109,237)

plot(issues, d.wolverine, type = "l", xlab = "issue", ylab = "degree", main = "degree against issues - Wolverine")
plot(issues, d.cyclops, type = "l", xlab = "issue", ylab = "degree", main = "degree against issues - Cyclops")
plot(issues, d.joerob, type = "l", xlab = "issue", ylab = "degree", main = "degree against issues - Robby")
plot(issues, d.spidman, type = "l", xlab = "issue", ylab = "degree", main = "degree against issues - Spider-Man")
plot(issues, d.franklin, type = "l", xlab = "issue", ylab = "degree", main = "degree against issues - Franklin B")
plot(issues, d.johnny, type = "l", xlab = "issue", ylab = "degree", main = "degree against issues - Johnny S")

### plotting dynamics of the average degree

deg.ff <- c(4, 4.67, 8, 12.64, 19.33, 24.28)
deg.ux <- c(6, 8, 14.97, 17.73, 23.97, 47.03)
deg.asm <- c(5, 6.75, 10.84, 19.48, 22.97, 25.51)

plot(issues,deg.ff, type = "l", xlab = "issue", ylab = "av.degree", main = "av.degree against issues - FF")
plot(issues,deg.ux, type = "l", xlab = "issue", ylab = "av.degree", main = "av.degree against issues - UX")
plot(issues,deg.asm, type = "l", xlab = "issue", ylab = "av.degree", main = "av.degree against issues - ASM")

### plotting evolution of the size of maximal k-core over time

size.k.ff <- c(5,6,8,13,22,35)
size.k.ux <- c(7,8,14,15,42,118)
size.k.asm <- c(6,7,15,24,24,54)

plot(issues, size.k.ff,type = "l",xlab = "issue",ylab = "number of vertices in max k-core",main = "vertices in max k-core against issues - FF")
plot(issues, size.k.ux,type = "l",xlab = "issue",ylab = "number of vertices in max k-core",main = "vertices in max k-core against issues - UX")
plot(issues, size.k.asm,type = "l",xlab = "issue",ylab = "number of vertices in max k-core",main = "vertices in max k-core against issues - ASM")

