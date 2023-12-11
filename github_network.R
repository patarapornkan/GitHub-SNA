library(igraph)
library(igraphdata)
library(rjson)
library(ggplot2)

edges = as.data.frame(read.table(file='Desktop/Analysis_of_social_networks/Project/dataset/git_web_ml/musae_git_edges.csv', sep=',', header=T))
users = as.data.frame(read.table(file='Desktop/Analysis_of_social_networks/Project/dataset/git_web_ml/musae_git_target.csv', sep=',', header=T))
edges = as.matrix(edges)
features = as.data.frame(read.table(file='Desktop/Analysis_of_social_networks/Project/dataset/git_web_ml/features.csv', sep=',', header=T))

# id starts from 0 but r indexing starts from 1 so will shift all ids up by 1
users$id = users$id + 1
edges = edges + 1
features$id = features$id + 1 
features = features[, 2:length(features)]

# create a graph
gh.graph = graph_from_edgelist(edges, directed = FALSE)

# set attributes to nodes
gh.graph = set_vertex_attr(gh.graph, name='id', index=V(gh.graph), users$id)
# gh.graph = set_vertex_attr(gh.graph, name='name', index=V(gh.graph), users$name)
gh.graph = set_vertex_attr(gh.graph, name='ml_target', index=V(gh.graph), users$ml_target)

# exploratory data analysis 

# components
decomp = components(gh.graph)
# $no = 1 , which means all users are connected in this github network

# degree distribution
gh.degree = degree(gh.graph)
summary(gh.degree)
hist(gh.degree)
hist(gh.degree[gh.degree < 100], xlab='Degree', ylab='Frequency')
#follows free-scale network -> preferential attachment
users$degree = gh.degree
users.ml_dev = users[users$ml_target==1,]
users.web_dev = users[users$ml_target==0,]
ml_deg_hist = hist(users.ml_dev[users.ml_dev$degree < 100,]$degree)
web_deg_hist = hist(users.web_dev[users.web_dev$degree < 100,]$degree)

plot(web_deg_hist, col=rgb(1,0,0,1/4))
plot(ml_deg_hist, col=rgb(0,0,1,1/4), add=T)

V(gh.graph)$color[V(gh.graph)$ml_target == 0] = "azure3"
V(gh.graph)$color[V(gh.graph)$ml_target == 1] = "black"

users$type[users$ml_target==1] = 'ML_dev'
users$type[users$ml_target==0] = 'web_dev'
users$color[users$ml_target==1] = 'black'
users$color[users$ml_target==0] = 'azure'
ggplot(users[users$degree < 100,], aes(x=degree,fill=type)) +
  geom_histogram(binwidth = 10) +
  scale_fill_grey(start=0.1, end=0.5)
p = p + scale_color_manual(values=c("#FFFFFF", "000000"))


#prune based on degree
gh.graph$degree = users$degree
gh.graph.pruned = induced_subgraph(gh.graph, V(gh.graph)[gh.graph$degree > 50])
layout = layout_with_kk(gh.graph.pruned)
plot(gh.graph.pruned,vertex.color=gh.graph.pruned$color, vertex.label=NA, layout=layout, vertex.size=2)

pruned_df = as_long_data_frame(gh.graph.pruned)
jaccard <- function(M, user1, user2) {
  sums = rowSums(M[,c(user1, user2)])
  
  similarity = length(sums[sums==2])
  total = length(sums[sums==1]) + similarity
  
  similarity/total
}


gh.eigencentrality = eigen_centrality(gh.graph, directed=FALSE)$vector
hist(gh.eigencentrality)
#same trend with degree. influential users are few

#communities
gh.mod = fastgreedy.community(gh.graph)
gh.mod_membership = gh.mod$membership
sizes(gh.mod)
users$com_no = gh.mod_membership

gh.communities = aggregate(users$ml_target, by=list(users$com_no), FUN=mean)
gh.communities$size = sizes(gh.mod)
names(gh.communities) = c("com_no", "prop_ml", "size")

gh.graph$community = gh.mod_membership
gh.graph.sam_com = induced_subgraph(gh.graph, V(gh.graph)[gh.graph$community == 1])
layout = layout_with_kk(gh.graph.sam_com)
plot(gh.graph.sam_com, vertex.color=gh.graph.sam_com$color, vertex.label=NA, layout=layout, vertex.size=3)
# samp_v = sample(V(gh.graph.sam_com), 400)
# gh.graph.final_viz = induced_subgraph(gh.graph.sam_com, V(gh.graph.sam_com)[samp_v])
# layout = layout_with_kk(gh.graph.final_viz)
# decomp = components(gh.graph.final_viz)
# largest_comp = which.max(decomp$csize)
# largest_ids = V(gh.graph.final_viz)[decomp$membership == largest_comp]
# gh.graph.final = induced_subgraph(gh.graph.final_viz, V(gh.graph.final_viz)[largest_ids])
# plot(gh.graph.final, vertex.color=gh.graph.final$color, vertex.label=NA, layout=layout, vertex.size=3)

# investigate homophily
test_df = as.data.frame(edges)
test_df = merge(test_df, users[, c("id", 'ml_target')], by.x='id_1', by.y='id', all.x = TRUE)
colnames(test_df)[3] <- "ml_target_1"
test_df = merge(test_df, users[, c("id", 'ml_target')], by.x='id_2', by.y='id', all.x = TRUE)
colnames(test_df)[4] <- "ml_target_2"

corr_df = as.data.frame(unique(cbind(test_df$id_1, test_df$ml_target_1)))
colnames(corr_df) = c("id_1", 'ml_target_1')
user.alter_ml_target_mean = aggregate(test_df$ml_target_2, list(test_df[, c("id_1")]), FUN=mean)
colnames(user.alter_ml_target_mean) = c("id_1", "ml_target_2")
corr_df = merge(corr_df, user.alter_ml_target_mean, by='id_1', all.x=TRUE)
cor(corr_df$ml_target_1, corr_df$ml_target_2, method=c("pearson"))

logistic_reg = glm(corr_df$ml_target_2 ~ corr_df$ml_target_1, family='binomial')
summary(logistic_reg)

target_colloration = lm(ml_target_2 ~ ml_target_1, corr_df)
summary(target_colloration)


