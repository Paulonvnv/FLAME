#### FUNCION 1 ####

get_network_clusters = 
  function(
    pairwise_relatedness,
    variable = 'rhat',
    threshold = 1,
    cols = c("sample_i", "sample_j"),
    rhat_formula = "timedist >= 16 & timedist <= 28 & geodist <= 750",
    metadata,
    sample_id = 'sample_id'){
    
    library(igraph)
    if(!require(GGally)){
      install.packages("GGally")
      library(GGally)
    }else{library(GGally)}
    
    if(!require(network)){
      install.packages("network")
      library(network)
    }else{library(network)}
    
    if(!require(sna)){
      install.packages("sna")
      library(sna)
    }else{library(sna)}
    
    if(!require(ggnet)){
      devtools::install_github("briatte/ggnet")
      library(ggnet)
    }else{library(ggnet)}
    
    library(S4Vectors)
    
    library(parallel)
    library(doMC)
    
    registerDoMC(detectCores()-1)
    
    
    pairwise_relatedness_l = 
      pairwise_relatedness[pairwise_relatedness[[cols[1]]] %in% metadata[[sample_id]] &
                             pairwise_relatedness[[cols[2]]] %in% metadata[[sample_id]]
                           ,
      ]
    
    print('Defining edges ...')
    
    edges_start = Sys.time()
    
    if(!(variable %in% colnames(pairwise_relatedness_l))){
      
      conditions = unlist(strsplit(rhat_formula, '( & | \\| )'))
      
      rhat_formula_c = rhat_formula
      
      for(condition in conditions){
        
        temp_var = str_extract(condition, '([A-z]|\\.|_)+')
        temp_rep = gsub(temp_var, paste0("pairwise_relatedness_l[['", temp_var, "']]"), condition)
        
        rhat_formula_c = gsub(condition, temp_rep, rhat_formula_c)
        
      }
      
      pairwise_relatedness_l[[variable]] = 0
      
      pairwise_relatedness_l[eval(parse(text = rhat_formula_c)),
      ][[variable]] = 1
      
    }
    
    selected_edges = pairwise_relatedness_l[pairwise_relatedness_l[[variable]] >= threshold,][,cols]
    
    edges = foreach(edge = 1:nrow(selected_edges), .combine = 'c') %dopar% {
      
      unlist(selected_edges[edge,])
    }
    
    sample_list = unique(unlist(pairwise_relatedness_l[,cols]))
    
    isolates = sample_list[!(sample_list %in% unique(edges))]
    
    edges_end = Sys.time()
    print('Identification of samples conected by edges and singletons took:')
    print(edges_end - edges_start)
    
    
    print('Creating network object ...')
    
    network_start = Sys.time()
    network_object = make_graph(edges = edges, isolates = isolates, directed=T)
    network_end = Sys.time()
    
    print('Generiation of the network object took: ')
    print(network_end - network_start)
    
    
    print('Identifying clusters in network ...')
    
    clusters_start = Sys.time()
    clusters = NULL
    for(node in 1:length(network_object)){
      
      # Set of samples linked with the node
      set_of_samples = c(names(network_object[[node]]), names(network_object[[node]][[1]]))
      
      proportion_of_samples_in_the_cluster = sapply(clusters, function(cluster){
        sum(set_of_samples %in% cluster)/length(set_of_samples)
      })
      
      cluster = which(proportion_of_samples_in_the_cluster > 0 & proportion_of_samples_in_the_cluster < 1 )
      
      if(length(cluster) == 1){
        
        clusters[[paste0('Cluster_', cluster)]] =
          c(clusters[[paste0('Cluster_', cluster)]],
            set_of_samples[!(set_of_samples %in% clusters[[paste0('Cluster_', cluster)]])]
          )
        
      }else if(length(cluster) > 1){
        # Combine clusters
        merged_cluster = unlist(clusters[cluster])
        
        names(merged_cluster) = NULL
        
        merged_cluster = c(merged_cluster,
                           set_of_samples[!(set_of_samples %in% merged_cluster)]
        )
        
        clusters[[cluster[1]]] = merged_cluster
        
        clusters[cluster[-1]] = NULL
        
        names(clusters) = paste0('Cluster_', 1:length(clusters))
        
        
      }else if(length(cluster) == 0 & isEmpty(proportion_of_samples_in_the_cluster)){
        cluster = length(clusters) + 1
        clusters[[paste0('Cluster_', cluster)]]  = set_of_samples
      }else if(length(cluster) == 0 & sum(proportion_of_samples_in_the_cluster == 1) == 0){
        cluster = length(clusters) + 1
        clusters[[paste0('Cluster_', cluster)]]  = set_of_samples
        
      }
      
    }
    
    
    cluster_df = NULL
    
    for(cluster in names(clusters)){
      
      cluster_name = cluster
      
      if(length(clusters[[cluster]]) == 1){
        cluster_name = gsub('Cluster', 'Singleton', cluster_name)
      }else if(length(clusters[[cluster]]) == 2){
        #cluster_name = gsub('Cluster', 'Doubleton', cluster_name)
      }else if(length(clusters[[cluster]]) == 3){
        #cluster_name = gsub('Cluster', 'Tripleton', cluster_name)
      }
      
      
      cluster_df = rbind(cluster_df, data.frame(Cluster = cluster_name, Sample_id = clusters[[cluster]]))
    }
    
    
    clusters_end = Sys.time()
    
    print('Identification of clusters took:')
    
    print(clusters_end - clusters_start)
    
    return(list(network_object = network_object,
                clusters = cluster_df))
    
  }


####Funcion 2####
long2wide_relatedness =function(pairwise_relatedness, metadata, id, id_i, id_j, var){
  library(svMisc)
  pairwise_relatedness_l = pairwise_relatedness
  
  pairwise_relatedness_l$id_i = pairwise_relatedness_l[[id_i]]
  pairwise_relatedness_l$id_j = pairwise_relatedness_l[[id_j]]
  
  pairwise_relatedness_l %<>% filter(id_i %in% metadata[[id]],
                                     id_j %in% metadata[[id]])
  
  pairwise_relatedness_matrix = matrix(data = NA,
                                       ncol = nrow(metadata),
                                       nrow = nrow(metadata),
                                       dimnames = list(metadata[[id]],
                                                       metadata[[id]]))
  
  for(pair in 1:nrow(pairwise_relatedness_l)){
    
    pairwise_relatedness_matrix[pairwise_relatedness_l[pair,][[id_i]],
                                pairwise_relatedness_l[pair,][[id_j]]] = 
      pairwise_relatedness_l[pair,][[var]]
    
    pairwise_relatedness_matrix[pairwise_relatedness_l[pair,][[id_j]],
                                pairwise_relatedness_l[pair,][[id_i]]] = 
      0
    #pairwise_relatedness_l[pair,][[var]]
    
    progress(round(100*pair/nrow(pairwise_relatedness_l)))
  }
  
  return(pairwise_relatedness_matrix)
  
}

 ####funcion 3####
plot_ggnetwork = function(pairwise_relatedness_matrix = NULL,
                          metadata,
                          id,
                          color_by = NULL,
                          palette = 'Paired', 
                          shape_by = NULL,
                          shape_levels = NULL,
                          alpha_by = NULL,
                          alpha = NULL,
                          labels_by = NULL,
                          labels2paste = NULL,
                          x_displacement = 0,
                          y_displacement = 0,
                          vertex.size = 4,
                          directed = T,
                          arrow.size = 12,
                          mode = 'fruchtermanreingold'){
  
  
  library(igraph)
  if(!require(GGally)){
    install.packages("GGally")
    library(GGally)
  }else{library(GGally)}
  
  if(!require(network)){
    install.packages("network")
    library(network)
  }else{library(network)}
  
  if(!require(sna)){
    install.packages("sna")
    library(sna)
  }else{library(sna)}
  
  if(!require(ggnet)){
    devtools::install_github("briatte/ggnet")
    library(ggnet)
  }else{library(ggnet)}
  
  library(RColorBrewer)
  
  
  if(!is.null(color_by)){
    if(sum(is.na(metadata[[color_by]])) > 0){
      metadata[is.na(metadata[[color_by]]),][[color_by]] = 'missing data'
    }
  }
  
  if(!is.null(shape_by)){
    if(sum(is.na(metadata[[shape_by]])) > 0){
      metadata[is.na(metadata[[shape_by]]),][[shape_by]] = 'missing data'
    }
  }
  
  
  if(!is.null(alpha_by)){
    if(sum(is.na(metadata[[alpha_by]])) > 0){
      metadata[is.na(metadata[[alpha_by]]),][[alpha_by]] = 'missing data'
    }
  }
  
  
  
  relatedness_network = network(pairwise_relatedness_matrix, directed = directed)
  
  network.vertex.names(relatedness_network) = metadata[[id]]
  
  
  if(!is.null(color_by)){
    relatedness_network %v% color_by = as.character(metadata[[color_by]])
  }else{
    color_by = 'gray75'
  }
  
  if(!is.null(shape_by)){
    
    if(!is.null(shape_levels)){
      relatedness_network %v% shape_by = factor(metadata[[shape_by]], levels = shape_levels)
    }else{
      relatedness_network %v% shape_by = as.character(metadata[[shape_by]])  
    }
    
  }else{
    shape_by = 16
  }
  
  if(!is.null(alpha_by)){
    relatedness_network %v% alpha_by = as.character(metadata[[alpha_by]])
  }else if(!is.null(alpha)){
    alpha_by = alpha
  }else{
    alpha_by = 1
  }
  
  if('AUTO' %in% toupper(palette)){
    
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    
    palette = col_vector[1:length(unique(metadata[[color_by]]))]
    
  }
  
  if(length(palette) > 1){
    
    plot_network = ggnet2(relatedness_network,
                          size = vertex.size,
                          color = color_by,
                          #color.palette = color_palette,
                          shape = shape_by,
                          #palette = 'Paired',
                          #color.label = TRUE,
                          arrow.size = arrow.size,
                          alpha = alpha_by,
                          mode = mode)
    
    
    plot_network = plot_network +
      scale_color_manual(values = palette)
    
  }else{
    
    plot_network = ggnet2(relatedness_network,
                          size = vertex.size,
                          color = color_by,
                          #color.palette = color_palette,
                          shape = shape_by,
                          palette = palette,
                          #color.label = TRUE,
                          arrow.size = arrow.size,
                          alpha = alpha_by,
                          mode = mode)
    
  }
  
  # Adding Labels
  
  if(!is.null(labels_by) & !is.null(labels2paste)){
    
    xcoord = NULL
    
    ycoord = NULL
    
    for(label2paste in labels2paste){
      
      xcoord = c(xcoord,
                 mean(plot_network@data[['x']][
                   plot_network@data[['label']] %in% 
                     metadata[[id]][metadata[[labels_by]] == label2paste]])
      )
      
      ycoord = c(ycoord,
                 mean(plot_network@data[['y']][
                   plot_network@data[['label']] %in% 
                     metadata[[id]][metadata[[labels_by]] == label2paste]])
      )
      
    }
    
    plot_network = 
      plot_network +
      annotate(geom = 'text',
               x = xcoord + x_displacement,
               y = ycoord + y_displacement,
               label = labels2paste,
               fontface = 'bold'
      )
    
    
  }
  
  return(list(network_object = relatedness_network,
              plot_network = plot_network))
  
}
