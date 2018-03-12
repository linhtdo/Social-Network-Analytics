library(Hmisc)			
library(data.table)		
library(aplpack)		
library(psych)			
library(ggplot2)

Edf = read.csv('Edges.csv')
View(edges)
Vdf = read.csv('Vertices.csv')
View(vertices)

barplot(table(Vdf$Verified),main = 'Verified Users by Twitter?')


# Extracting the year the user join Twitter
Vdf$Joined.Twitter.Date..UTC.=strptime(Vdf$Joined.Twitter.Date..UTC.,'%m/%d/%Y')
Vdf$Joined.Twitter.Date..UTC.

Vdf$YearJoined = as.numeric(format(as.Date(Vdf$Joined.Twitter.Date..UTC.),'%Y'))
barplot(table(Vdf$YearJoined), ylim = c(0,400), main = 'Number of new users per year')

# Cleaning data: Converting language column to lowercase then plot
Vdf$Language = tolower(Vdf$Language)
lang = table(Vdf$Language)
barplot(lang, ylim = c(0,800),main = 'Number of Users by Languages')

barplot(aggregate(Vdf$Followed, by = list(Vdf$Language),FUN = 'mean')$x, main = 'Average Number of Followers by Languages')

barplot(aggregate(Vdf$Followed, by = list(Vdf$Verified),FUN = 'mean')$x, main = 'Average Number of Followers by Types of Users
        Unverified vs. Verified',
        xlab = 'Unverified                                                     Verified')

#create graph from edge list, specifying graph as directed
library(igraph)

EdgesGraph=graph_from_data_frame(Edf, directed=T)
V(EdgesGraph)
#get a better list
cbind(V(EdgesGraph))
#get the list of edges
E(EdgesGraph)			    
#specify vertices DataFrame as source for attributes
EdgesGraph=graph_from_data_frame(Edf, directed=T, vertices=Vdf)
plot(EdgesGraph)

#re-open edges list, ensuring text is not converted to factors
Edf=read.csv('Edges.csv',header=T, stringsAsFactors = F)
#Edf=rbind(Edf,c('PG','PG'))   #add a self-tie/loop edge
#Edf=rbind(Edf,c('LS','LS'))   #add another loop edge

EdgesGraph=graph_from_data_frame(Edf, directed=T, vertices=Vdf)
plot(EdgesGraph)
plot(simplify(EdgesGraph))
EG = simplify(EdgesGraph)

plot(EG, layout=layout.davidson.harel(EG), vertex.shape='sphere', 
     vertex.color='white', 
     vertex.label.color='black', vertex.label.cex=0.5, vertex.label=V(EG)$Name)

plot(EG, layout=layout.fruchterman.reingold(EG), vertex.shape='sphere', 
     vertex.color='white', 
     vertex.label.color='black', vertex.label.cex=0.75, vertex.label=V(EG)$Name)

tkplot(EG, layout=layout.davidson.harel(EG), vertex.shape='sphere', 
       vertex.color='white', 
       vertex.label.color='black', vertex.label.cex=0.5, vertex.label=V(EG)$Name)

plot(EG, layout=layout.drl(EG), vertex.shape='sphere', 
     vertex.color=ifelse(V(EG)$Verified==FALSE, 'lightblue','darkred'), 
     vertex.label.color=ifelse(V(EG)$Verified==FALSE, 'black','white'),
     vertex.label.cex=0.5, vertex.label=V(EG)$Name)

plot(EG, layout=layout.drl(EG), vertex.shape='sphere', 
     vertex.color=ifelse(V(EG)$YearJoined==2017, 'lightblue',ifelse(V(EG)$YearJoined==2018,'lightpink','darkred')), 
     vertex.label.color=ifelse(V(EG)$YearJoined==2017, 'black',ifelse(V(EG)$YearJoined==2018,'gray','white')),
     vertex.label.cex=0.5, vertex.label=V(EG)$Name)

min(Vdf$Followed) #0
max(Vdf$Followed) #261846
mean(Vdf$Followed) #2342.893

plot(EG, layout=layout.drl(EG), vertex.shape='sphere', 
     vertex.color=ifelse(V(EG)$Followed > 2343, 'darkred','lightpink'), 
     vertex.label.color=ifelse(V(EG)$Followed > 2343, 'white','black'),
     vertex.label.cex=0.5, vertex.label=V(EG)$Name)

table(Vdf$Time.Zone)

vcount(EG) # gives us vertices
ecount(EG) # gives us the edges

graph.density(EG)
diameter(EG)
farthest.nodes(EG)
farthest.nodes(EG)$vertices
farthest.nodes(EG)$distance

degree(EG)
closeness(EG)
betweenness(EG)

EgoMetrics = cbind(degree(EG))
colnames(EgoMetrics) ='Degree'             

EgoMetrics = cbind(EgoMetrics,Closeness =closeness(EG))
EgoMetrics = cbind(EgoMetrics, Betweenness = betweenness(EG))
EgoMetrics = cbind(EgoMetrics, EVCentrality = evcent(EG)$vector)

View(EgoMetrics)
pairs(EgoMetrics)

evcent(EG)
evcent(EG)$vector

#use metrics in plot
plot(EG, layout=layout.drl(EG), vertex.shape='sphere', 
     vertex.color=ifelse(V(EG)$Verified==FALSE, 'lightblue','darkred'), 
     vertex.label.color=ifelse(V(EG)$Party==FALSE, 'black','white'),
     vertex.size=sqrt(evcent(EG)$vector)*25,
     vertex.label.cex=0.5, vertex.label=V(EG)$Name)

#clique=fully-connected component
cliques(EG, min=5, max=5)		  #identifies cliques of specified size - do not execute; takes too long
clique.number(EG)			  #identifies the number of distinct cliques
largest.cliques(EG)

# Assortaivity
assortativity(EG, Vdf$Verified, directed=T) # 0.06692097
assortativity(EG, Vdf$YearJoined, directed=T) # 0.1474663
assortativity(EG, Vdf$Followed, directed=T) # 0.05008563
assortativity(EG, Vdf$Language, directed=T) # 0.08634998



assortativity_nominal(EG,Vdf$Language,directed=T) # 0.09800004

assortativity_nominal(EG,Vdf$YearJoined,directed=T) # 0.009459744

#does degree centrality account for ties
assortativity_degree(EG, directed=T) # -0.1551914

cluster_walktrap(EG)			  #detect fault-lines with walktrap algorithm
cluster_edge_betweenness(EG)
#cluster_fast_greedy(EG)		  #detect fault-lines with the fast-greedy algorithm
cluster_walktrap(EG, steps=200)	  #increase "steps" in random walk algorithm to obtain better partition

w=cluster_walktrap(EG, steps = 200)		  #save the cluster solution

plot(w, EG)				  #plot the partition

o=cluster_edge_betweenness(EG)			  #try another algorithm
plot(o, EG)
plot(o, EG, margin=-.25,		  #plot partition; Adjusting margin to zoom in (smaller), or zoom out (greater)
     edge.color='darkred',vertex.label=V(EG)$Name,
     vertex.size=evcent(EG)$vector*5, vertex.shape='sphere',
     vertex.label.family='sans',vertex.label.font=2,
     vertex.label.cex=.7,vertex.label.dist=.25)

#save the partition to a data frame
EGPartitions=data.frame(cbind(o$names, o$membership))
colnames(EGPartitions)=c('Names','Group')
View(EGPartitions)

avgfol = mean(Vdf$Followed) 
popular = count(Vdf$Followed >avgfol)
popular

median(Vdf$Followed)

#partitioning/community detection/subgroups
#walktrap.community
#fastgreedy.community