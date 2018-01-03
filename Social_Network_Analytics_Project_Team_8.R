library(splitstackshape)
library(igraph)
library(data.table)
library(ggplot2)

setwd("C:/Users/47599/Desktop/Social Network Analytics/project/data")

# Clean Data --------------------------------------------------------------

# Create edgelists for co-membership network

data_group<-read.csv("SDFB_groups.csv")
# Remove Square Brackets
data_group$Members.List..Name.with.SDFB.Person.ID.<-substr(
  data_group$Members.List..Name.with.SDFB.Person.ID., 2, 
  nchar(as.character(data_group$Members.List..Name.with.SDFB.Person.ID.))-1)
data_group<-data_group[-which(data_group$Members.List..Name.with.SDFB.Person.ID.==""),]
data_group<-data_group[complete.cases(data_group),]

# Split Member List
members <- cSplit(data_group, "Members.List..Name.with.SDFB.Person.ID.", ",")
members<-members[,-c("SDFB.Group.ID","Name", "Description", "Start.Year.Type","Start.Year","End.Year.Type","End.Year")]
members<-as.data.frame(sapply(members, function(x) substr(x, 2, nchar(as.character(x))-8)))
members<-members[-which(is.na(members$Members.List..Name.with.SDFB.Person.ID._02)==TRUE),]
possible_pairs<-lapply(seq_len(nrow(members)), function(i) t(members[i,]))
for(i in seq_along(possible_pairs)){
  possible_pairs[[i]] = possible_pairs[[i]][!is.na(possible_pairs[[i]])]
}

edges <- lapply(seq_along(possible_pairs), function(i) t(combn(possible_pairs[[i]], 2)))
edges_cogroup<- data.frame(do.call(rbind, edges))
write.csv(edges_cogroup, "edges_cogroup.csv")


# People Information Data
data_people_1<-read.csv("SDFB_people.csv")
data_people_all<-data_people_1[,c("SDFB.Person.ID","Display.Name","Gender","Prefix","Historical.Significance"
                            ,"Extant.Birth.Year","Group.List")]
write.csv(data_people_all, "data_people_all.csv")
data_people_2<-read.csv("data_people_withgroups.csv")
data_people_grouped<-data_people_2[,c("Display.Name","Index","Extant.Birth.Year")]
write.csv(data_people, "data_people_grouped.csv")

#Relationship Data Set
data_1<-read.csv("SDFB_relationships_100000000_100020000.csv")
data_2<-read.csv("SDFB_relationships_100020001_100040000.csv")
data_3<-read.csv("SDFB_relationships_100040001_100060000.csv")
data_4<-read.csv("SDFB_relationships_100060001_100080000.csv")
data_5<-read.csv("SDFB_relationships_100080001_100100000.csv")
data_6<-read.csv("SDFB_relationships_100100001_100120000.csv")
data_7<-read.csv("SDFB_relationships_100120001_100140000.csv")
data_8<-read.csv("SDFB_relationships_100140001_100160000.csv")
data_9<-read.csv("SDFB_relationships_100160001_100180000.csv")
data_10<-read.csv("SDFB_relationships_greater_than_100180000.csv")


data_relationships<-rbind(data_1, data_2, data_3, data_4, data_5, data_6, data_6, data_7, data_8, data_9, data_10)
data_relationships<-data_relationships[,c("Person.1.ID","Person.2.ID",
                                          "Start.Year","End.Year","Maximum.Confidence")]
edge_relationships<-data_relationships[complete.cases(data_relationships),]
merge_rl_1<-merge(edge_relationships, data_people_all, by.x="Person.1.ID", by.y="SDFB.Person.ID")
merge_rl_2<-merge(merge_rl_1, data_people_all, by.x="Person.2.ID", by.y="SDFB.Person.ID")
edges_relationship<-merge_rl_2[,c("Display.Name.x","Display.Name.y", "Start.Year","End.Year","Maximum.Confidence")]
write.csv(edges_relationship, "edges_relationship.csv")

edges_relationship_grouped<-edges_relationship[which(edges_relationship[,1]%in%
                                                      data_people_grouped$Display.Name |edges_relationship[,2]%in%
                                                       data_people_grouped$Display.Name),]
write.csv(edges_relationship_grouped, "edges_relationship_grouped.csv")
#export the combined data to a single csv file is to ease the teamwork 



# All relationships network -------------

node<-read.csv("Clean Data/data_people_all.csv")
nodes<-node[unique(sort(node$Display.Name)),]
edges<-read.csv("Clean Data/edges_relationship.csv")

# Group the maximum confidence into 4 groups to show different degree for relationship
rl_degree<-function(x){
  if (x<=20){y<-1} else {
      if (x<=40) {y<-2} else {
          if (x<=60) {y<-3} else {y<-4}}} 
  return(y)
  }
edges$degree<-lapply(edges$Maximum.Confidence, rl_degree)

# Create network object and set attribute for vertices and edges
graph<-graph.data.frame(edges[,c("Display.Name.x", "Display.Name.y")], directed=FALSE, 
                             vertices=unique(node$Display.Name))
V(graph)$gender<-as.character(nodes$Gender)
V(graph)$birthyear<-as.numeric(as.character(nodes$Extant.Birth.Year))
E(graph)$degree<-edges$degree
V(graph)$nobility<-as.character(nodes$Prefix)


# 1) Plot the network, color edge by degree
V(graph)$size<-0.01
V(graph)[2113]$size<-10
plot.igraph(graph,layout=layout.fruchterman.reingold,edge.size=10,vertex.label=NA,
            edge.curved=TRUE)
hist(as.numeric(E(graph)$degree))

# Remove all the isolated node
# Random Sample 
V(graph)$degree<-degree(graph)
graph<-delete_vertices(graph, which(V(graph)$degreec==0))
index<-sample(length(V(graph)),100)
index<-append(index,which(V(graph)$name == "Francis Bacon"))
graph_c<-induced_subgraph(graph,index)
V(graph_c)$size<-0.01
V(graph_c)[V(graph_c)$name=="Francis Bacon"]$size<-10
plot.igraph(simplify(graph_c),layout=layout.fruchterman.reingold,edge.size=10,vertex.label=NA,
            edge.curved=TRUE)
plot(delete.vertices(simplify(graph_c), degree(graph_c)==0))
# 2) How many steps for each person to reach Francis.Bacon
which(V(graph)$name == "Francis Bacon")
# Node 2113 is Francis Bacon
distances<-as.matrix(distances(graph, to=2113, mode="all", weights=NA))
write.csv(distances,"all_network_steps_to_Francis_Bacon.csv")
hist(distances)
max(distances)
mean(distances)
length(which(distances=="Inf"))
# Infinite indicates there are 2497 people Francis Bacon can not connect to them
# Lets remove those vertices Francis Bacon cannot connect, try to find out the max shortest path to Mr.Bacon
distances_c<-distances[(distances!="Inf"),]
max(distances_c)
# max shortest path to Mr.Bacon is 5 which means in this network people can reach to Bacon within 6 people
mean(distances_c)
# average shortest path to Mr.Bacon is 2.34

# 3) Who is most central?
V(graph)$degreec<-centr_degree(graph)$res
V(graph)$betweeness<-centr_betw(graph)$res
V(graph)$closeness<-centr_clo(graph)$res
V(graph)$eigenc<-centr_eigen(graph)$vector
V(graph)$coreness<-coreness(graph)

centrality<-list(name=unlist(V(graph)$name),birthyear=unlist(V(graph)$birthyear),gender=unlist(V(graph)$gender),
                 degree=unlist(V(graph)$degreec),nobility=unlist(V(graph)$nobility),
                               between=unlist(V(graph)$betweeness),close=unlist(V(graph)$closeness),
                               eigenvector=unlist(V(graph)$eigenc), core=unlist(V(graph)$coreness))
centrality<-as.data.frame(centrality, stringsAsFactors=FALSE)
write.csv(centrality,"all_network_centrality.csv")
centrality[centrality$degree==max(centrality$degree),]
# name degree             between close eigenvector
# 2296 King  Charles II   2556   7943929 0.0004001898 

centrality[centrality$between==max(centrality$between),]
# name degree             between close eigenvector
# 2296 King  Charles II   2556   7943929 0.0004001898 

centrality[centrality$close==max(centrality$close),]
# name degree             between close eigenvector
# 2296 King  Charles II   2556   7943929 0.0004001898 

centrality[centrality$eigenvector==max(centrality$eigenvector),]
# name degree             between close eigenvector
# 2296 King  Charles II   2556   7943929 0.0004001898 

hist(centrality$degree[centrality$degree!=0], nclass=500)
hist(centrality$between[centrality$between!=0], nclass=500)


# Delete vertices whose degree centrality is 0
graph_c<-delete_vertices(graph, which(V(graph)$degreec==0))
hist(as.numeric(V(graph_c)$degreec),nclass=100)
# 5 Most central people
V(graph)[which(V(graph)$degreec%in%tail(sort(unlist(V(graph)$degreec)),5))]
# King Henry VII,King James II and VII,King James I and VI,King Charles I,King Charles II
# Steps to reach these people
distance_top1<-as.matrix(distances(graph, to=match("King Charles II",V(graph)$name), mode="all",weights=NA))
mean(distance_top1[(distance_top1!="Inf"),])
# 1.99
max(distance_top1[(distance_top1!="Inf"),])
# 5
distance_top2<-as.matrix(distances(graph, to=match("King Charles I",V(graph)$name), mode="all",weights=NA))
mean(distance_top2[(distance_top1!="Inf"),])
# 2.046
max(distance_top2[(distance_top1!="Inf"),])
# 5
distance_top3<-as.matrix(distances(graph, to=match("King James I and VI",V(graph)$name), mode="all",weights=NA))
mean(distance_top3[(distance_top1!="Inf"),])
# 2.10
max(distance_top3[(distance_top1!="Inf"),])
# 5
distance_top4<-as.matrix(distances(graph, to=match("King James II and VII",V(graph)$name), mode="all",weights=NA))
mean(distance_top4[(distance_top1!="Inf"),])
# 2.14
max(distance_top4[(distance_top1!="Inf"),])
# 5
distance_top5<-as.matrix(distances(graph, to=match("King Henry VII",V(graph)$name), mode="all",weights=NA))
mean(distance_top5[(distance_top1!="Inf"),])
# 2.75
max(distance_top5[(distance_top1!="Inf"),])
# 6

# How about Francis Bacon
V(graph)$degreec[2113]
# Francis Bacon ranked as 42nd in terms of degree centrality but the shortest path is not ranked as 42nd

# Plot most 200 central people
top_100_centr<-tail(sort(centrality$degree),20)
top_100_centr<-append(top_100_centr,V(graph)[V(graph)$name=="Francis Bacon"]$degreec)
graph_c100<-induced_subgraph(graph,which(V(graph)$degreec%in%top_100_centr))


V(graph_c100)$size<-5
V(graph_c100)[V(graph_c100)$name=="Francis Bacon"]$size<-10
V(graph_c100)[V(graph_c100)$name=="Francis Bacon"]$size<-10
E(graph_c100)$color[E(graph_c100)$degree == 4] <- 'dodgerblue4'
E(graph_c100)$color[E(graph_c100)$degree == 3] <- 'steelblue2'
E(graph_c100)$color[E(graph_c100)$degree == 2] <- 'grey70'
E(graph_c100)$color[E(graph_c100)$degree == 1] <- 'white'
plot(graph_c100)

# 4) Does the network/relationships change over time
time_all<-aggregate(centrality[,3:7], list(centrality$birthyear), mean)
write.csv(time_all,"all_network_centrality_year.csv")
# plot centrality against people's birth year
# plot centrality against people's birth year

ggplot(data=time_all,aes(x=Group.1, y=degree))+geom_col()+theme_bw()
ggplot(data=time_all,aes(x=Group.1, y=between))+geom_col()+theme_bw()
ggplot(data=time_all,aes(x=Group.1, y=close))+geom_col()+theme_bw()
ggplot(data=time_all,aes(x=Group.1, y=eigenvector))+geom_col()+theme_bw()
ggplot(data=time_all,aes(x=Group.1, y=core))+geom_col()+theme_bw()

# 5) Explore the relationship between gender, birth year, nobility and centrality
centrality$gender[centrality$gender=="male"]<-1
centrality$gender<-as.character(centrality$gender)
centrality$gender[centrality$gender=="female"]<-2
# gender: male=1, female=2
# extract the prefix
for (i in 1:nrow(centrality)){
  centrality$prefix[i]<-strsplit(as.character(centrality$name[i]), split=" ")[[1]][1]
}
centrality$nobility[centrality$prefix%in%unique(centrality$nobility)]<-1
centrality$nobility[centrality$nobility!=""]<-1
centrality$nobility[centrality$nobility==""]<-0
# nobility: nobel=1, non-nobel=0
centrality$gender<-as.factor(centrality$gender)
centrality$nobility<-as.factor(centrality$nobility)
l1<-lm(degree~nobility+between+birthyear+gender,data=centrality)
summary(l1)
#Call:
#lm(formula = degree ~ nobility + between + birthyear + gender, 
     data = centrality)

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -1204.62   -18.32   -11.19     4.39   531.74 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.834e+01  7.546e+00  -2.431   0.0151 *  
#  nobility1    5.981e+00  7.847e-01   7.622 2.67e-14 ***
#  between      4.694e-04  2.863e-06 163.952  < 2e-16 ***
#  birthyear    2.589e-02  4.731e-03   5.473 4.49e-08 ***
#  gender2     -5.317e+00  8.613e-01  -6.174 6.86e-10 ***
#  genderother -2.574e+01  2.743e+01  -0.939   0.3479    

# Grouped Network ---------------------------------------------------------
node_g<-read.csv("Clean Data/data_people_withgroups.csv")
node_g$Display.Name<-as.character(node_g$Display.Name)
edges_g<-read.csv("Clean Data/edges_relationship_grouped.csv")
edges_g$degree<-lapply(edges_g$Maximum.Confidence, rl_degree)
graph_g<-graph_from_data_frame(edges_g[,c("Display.Name.x","Display.Name.y")], directed=FALSE)
graph_g<-induced_subgraph(graph_g,which(V(graph_g)$name%in%unique(node_g$Display.Name)))
# Match Group Index for vertices
node_gs<-as.data.frame(V(graph_g)$name)
for (i in 1:nrow(node_gs)){
  node_gs$group[i]<-node_g$Index[which(node_g$Display.Name==node_gs$`V(graph_g)$name`[i])]
}
V(graph_g)$group<-as.numeric(as.character(node_gs$group))

# 1) Overall look of this grouped Network

graph_g<-delete_vertices(simplify(graph_g),centr_degree(graph_g)$res==0)
V(graph_g)$color[V(graph_g)$group == 1]<-"blue"
V(graph_g)$size<-0.1
V(graph_g)[V(graph_g)$name=="Francis Bacon"]$size<-10
E(graph_g)$color[E(graph_g)$degree == 4] <- 'dodgerblue4'
E(graph_g)$color[E(graph_g)$degree == 3] <- 'steelblue2'
E(graph_g)$color[E(graph_g)$degree == 2] <- 'grey70' 
E(graph_g)$color[E(graph_g)$degree == 1] <- 'white'
plot.igraph(graph_g, vertex.label=NA)

# highlight Francis Bacon
V(graph_g)[which(V(graph_g)$name=="Francis Bacon")]$size<-10

# Group nodes by occupation
merge_g<-merge(edges_g,node_g[,c("Display.Name","Index")],by.x="Display.Name.x",by.y="Display.Name")
merge_g<-merge(merge_g,node_g[,c("Display.Name","Index")],by.x="Display.Name.y",by.y="Display.Name")
group_g_edge_list<-merge_g[,c("Index.x","Index.y","Start.Year","degree")]
group_g_edge<-graph_from_data_frame(group_g_edge_list[,1:2],directed=FALSE)
E(group_g_edge)$degree<-group_g_edge_list$degree
E(group_g_edge)$year<-group_g_edge_list$Start.Year
E(group_g_edge)$color[E(group_g_edge)$degree == 4] <- 'dodgerblue4'
E(group_g_edge)$color[E(group_g_edge)$degree == 3] <- 'steelblue2'
E(group_g_edge)$color[E(group_g_edge)$degree == 2] <- 'grey70'
E(group_g_edge)$color[E(group_g_edge)$degree == 1] <- 'white'
E(group_g_edge)$size<-0.5
V(group_g_edge)$color<-V(group_g_edge)$name
plot(simplify(group_g_edge, remove.multiple=FALSE, remove.loops=TRUE))

# 2) How many steps to reach Francis Bacon for each group
distance_1<-as.data.frame(distances(graph_g,to=match("Francis Bacon",V(graph_g)$name), mode="all",weights=NA))
distance_1$name<-rownames(distance_1)
distances<-distance_1[which(distance_1$`Francis Bacon`!="Inf"),]
mean(distances$`Francis Bacon`)
distances<-merge(distances,node_gs,by.x="name",by.y="V(graph_g)$name")
distances_g<-aggregate(distances$`Francis Bacon`, list(distances$group), mean)
write.csv(distances_g,"group_network_steps_to_Francis_Bacon.csv")
plot(distances$group,mean(distances$`Francis Bacon`))
plot(aggregate(distances$`Francis Bacon`, list(distances$group), mean))
#gardeners have the smallest shortest path
#Group.1        x
#1       0 0.000000
#2       1 2.895833
#3       2 2.731481
#4       3 2.822330
#5       4 2.787234
#6       5 2.500000
#7       6 2.825175
#8       7 2.811966
#9       8 2.894410

# 3) Which group is more central
V(graph_g)$degreec<-centr_degree(graph_g)$res
V(graph_g)$betweeness<-centr_betw(graph_g)$res
V(graph_g)$closeness<-centr_clo(graph_g)$res
V(graph_g)$eigenc<-centr_eigen(graph_g)$vector
degree_group<-data.frame(x1=unlist(V(graph_g)$name),degree=unlist(V(graph_g)$degreec),betw=unlist(V(graph_g)$betweeness),
                         close=unlist(V(graph_g)$closeness),eigenc=unlist(V(graph_g)$eigenc),
                         x3=unlist(V(graph_g)$group))
g_centr<-aggregate(degree_group[,2:5], list(degree_group$x3), mean)
write.csv(g_centr,"group_network_centrality.csv")
#Group.1         x
#1       0 68.000000
#2       1  7.868056
#3       2  9.027778
#4       3  8.327553
#5       4  8.698630
#6       5  5.666667
#7       6 12.097902
#8       7 12.017094
#9       8  7.740741
# military people and historians have higher degree centrality
plot(aggregate(degree_group$x2, list(degree_group$x3), mean))

# 4) Does it change over time?
aggregate(distances$`Francis Bacon`, list(distances$group), mean)

# Affiliation Matrix ------------------------------------------------------
edges_a<-read.csv("Clean Data/edges_cogroup.csv")
edges_a<-edges_a[,c("X1","X2")]
graph_a<-graph_from_edgelist(as.matrix(edges_a),directed=FALSE)

# 1) Overall look of this network
V(graph_a)$size<-0.1
V(graph_a)[V(graph_a)$name=="Francis Bacon"]$size<-10
plot(simplify(graph_a),vertex.label=NA)


# 2) Who is central in this network
V(graph_a)$degreec<-centr_degree(graph_a)$res
V(graph_a)$betweeness<-centr_betw(graph_a)$res
V(graph_a)$closeness<-centr_clo(graph_a)$res
V(graph_a)$eigenc<-centr_eigen(graph_a)$vector
degree_a<-data.frame(x1=unlist(V(graph_a)$name),degree=unlist(V(graph_a)$degreec),betw=unlist(V(graph_a)$betweeness),
                         close=unlist(V(graph_a)$closeness),eigenc=unlist(V(graph_a)$eigenc))
hist(degree_a$x2)
V(graph_a)[which(V(graph_a)$degreec%in%tail(sort(unlist(V(graph_a)$degreec)),5))]
#Henry Grey,Charles Montagu,Paolo Antonio Rolli,John Arbuthnot,Charles Cadogan,James Hamilton,Thomas Coke
#Charles Lennox,Richard Boyle
# Almost all of these high degree centrality people belongs to group: Royal Academy of Music
# Francis Bacon is not the central as well

merge_a<-merge(degree_a,data_people_all[,c("Display.Name","Group.List")],by.x="x1",by.y="Display.Name")
write.csv(merge_a, "cogroup_network_centrality.csv")
# 3) Jaccard Similarity
adj<-as_adjacency_matrix(graph_a,type="both",names=TRUE)
adj<-as.matrix(adj)
jac_sim<-function(i,j){
  a<-adj[,i]%*%adj[,j]
  b<-sum(adj[,i])
  c<-sum(adj[,j])
  jac_sim<-a/(b+c-a)
  return(jac_sim)
}
for (i in 1:nrow(edges_a)){
  edges_a$jac[i]<-jac_sim(edges_a[i,1],edges_a[i,2])
}

