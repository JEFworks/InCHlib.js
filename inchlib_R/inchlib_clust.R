library(rjson)

## Modifying inchlib_clust.py for R
## Use rjson to write in JSON format

get.attr <- function(hr, attribute) {

    # Convert to dendrogram
    hr.d <- as.dendrogram(hr)

    attr.children.global <<- c()

    # Recursive function to go through tree
    attr.children <- function(hr.d) {
        a <- attr(hr.d, attribute)
        if(is.null(a)) { a <- NA }
        attr.children.global <<- c(attr.children.global, a)
        if(length(hr.d)>1) {
            for(i in 1:length(hr.d)) {
                attr.children(hr.d[[i]])
            }
        }
    } 
    attr.children(hr.d)

    return(attr.children.global)
}

get.names <- function(hr) {

    # Convert to dendrogram
    hr.d <- as.dendrogram(hr)
    
    # Get all node names
    names.global <<- c()
    #i <<- 1
    names <- function(hr.d) {
        a <- attr(hr.d, "label") 
        #if(is.null(a)) { a <- paste('Parent', i) }
        # Create unique identifier by collapsing all attributes for internal nodes
        if(is.null(a)) { a <- paste(attributes(hr.d), collapse='.') }
        names.global <<- c(names.global, a)
        #i <<- i + 1
        if(length(hr.d)>1) {
            for(i in 1:length(hr.d)) {
                names(hr.d[[i]])
            }
        }
    }
    names(hr.d)
    names.global
        
    return(names.global)
}

get.children <- function(hr) {

    # Convert to dendrogram
    hr.d <- as.dendrogram(hr)

    # Get children relationship
    children.global <<- c()
    #Recursive function to go through tree
    children <- function(hr.d) {
        if(attr(hr.d, "height")>0) {
            lc <- attr(hr.d[[1]], "label")
            if(is.null(lc)) { lc <- paste(attributes(hr.d[[1]]), collapse='.') }
            rc <- attr(hr.d[[2]], "label")
            if(is.null(rc)) { rc <- paste(attributes(hr.d[[2]]), collapse='.') }
            children.global <<- c(children.global, paste(lc, rc, sep=';'))
            for(i in 1:length(hr.d)) {
                children(hr.d[[i]])
            }
        }
        # Is leaf, no more children
        else {
            children.global <<- c(children.global, "NA;NA")
        }
    }
    children(hr.d)
    children.global.df <- do.call(rbind, lapply(children.global, function(x) strsplit(x, ';')[[1]]))
    colnames(children.global.df) <- c('left.child', 'right.child')

    return(children.global.df)
}

dend.to.table <- function(hc) {
    hc.dend <- cbind('row.name'=get.names(hc), get.children(hc), 'height'=get.attr(hc, 'height'), 'num.children'=get.attr(hc, 'member'), 'is.leaf'= get.attr(hc, 'leaf'))
    rownames(hc.dend) <- hc.dend[,'row.name']
    parent.id <- rep(NA, nrow(hc.dend)); names(parent.id) <- hc.dend[,'row.name'] 
    parent.id[hc.dend[,'left.child'][hc.dend[,'left.child']!='NA']] <- hc.dend[hc.dend[,'left.child']!='NA', 'row.name']
    parent.id[hc.dend[,'right.child'][hc.dend[,'right.child']!='NA']] <- hc.dend[hc.dend[,'right.child']!='NA', 'row.name']
    hc.dend <- cbind(hc.dend, parent.id)
    return(data.frame(hc.dend, stringsAsFactors=FALSE))
}

#"node_id": {
#           "count": 2,    //number of items (leafs) which lie in the dendrogram hierarchy below the given node
#            "distance": 3.32,    //distance from the zero base of the dendrogram, given by the distance measure used for clustering
#            "parent": "node_1",    //the ID of a parent node
#            "left_child": "leaf_1",   //ID of a left child
#            "right_child": "leaf_2"   //ID of a right child
#    }, 
json.node <- function(str) {
    node <- list(
        "count" = as.numeric(str$num.children),
        "distance" = as.numeric(str$height),
        "left_child" = str$left.child,
        "right_child" = str$right.child
    )
        
    if(!is.na(str$parent.id)) {
        node[["parent"]] <- str$parent.id
    }

    return(node) 
}

#"leaf_id": {
#        "count": 1,    //number of items (leafs) which lie in the dendrogram hierarchy below the given node
#            "distance": 0,    //distance from the zero base of the dendrogram, given by the distance measure used for clustering
#            "features": [1.4, 3.5, 5.1],    //values of individual features defining a data item which is represented by the heatmap row
#            "parent": "node_1",    //the ID of a parent node
#            "objects": ["object_id"]    //list of IDs of objects (data points) represented by a given row
#    },
json.row.leaf <- function(str, dat) {
    leaf <- 
        list(
            "count" = 1,
            "distance" = 0,
            "features" = as.matrix(dat),
            "objects" = list(str$row.name),
            "parent" = str$parent.id
        )
    
    return(leaf)
}
# For column dendrograms
json.col.leaf <- function(str) {
    leaf <- 
        list(
            "count" = 1,
            "distance" = 0,
            "parent" = str$parent.id
        )
    
    return(leaf)    
}

row.dend.to.json <- function(dend, data) {

    nodes <- which(is.na(dend[, 'is.leaf']))
    leaves <- which(dend[, 'is.leaf']==TRUE)

    nodes.json <- lapply(nodes, function(k) {
        str <- dend[k,]
        json.node(str)
    }); names(nodes.json) <- dend[nodes ,'row.name']

    leaves.json <- lapply(leaves, function(k) {
        str <- dend[k,]
        row.name <- dend[k, 'row.name']
        dat <- data[row.name,]
        json.row.leaf(str, dat)
    }); names(leaves.json) <- dend[leaves, 'row.name']
    
    all.json <- c(nodes.json, leaves.json)
        
    final.json <- 
        list(
            "feature_names" = colnames(data),
            "nodes" = all.json
        )
    
    return(final.json)
}

col.dend.to.json <- function(dend) {

    nodes <- which(is.na(dend[, 'is.leaf']))
    leaves <- which(dend[, 'is.leaf']==TRUE)

    nodes.json <- lapply(nodes, function(k) {
        str <- dend[k,]
        json.node(str)
    }); names(nodes.json) <- dend[nodes, 'row.name']

    leaves.json <- lapply(leaves, function(k) {
        str <- dend[k,]
        json.col.leaf(str)
    }); names(leaves.json) <- dend[leaves, 'row.name']
    
    all.json <- c(nodes.json, leaves.json)

    final.json <- 
        list(
            "feature_names" = colnames(data),
            "nodes" = all.json
        )
        
    return(final.json)
}

# Given numeric dataframe with data rownames as colnames
# convert to metadata inchlib json format
# to be displayed as row colors in the heatmap
# Ex:
# > rowCol
#        Alabama Alaska Arizona Arkansas
# rowCol    13.2     10     8.1      8.8  
rowCol.to.json <- function(rowCol) {

    nodes.list <- lapply(1:ncol(rowCol), function(i) {
        list(as.matrix(rowCol[,i]))
    }) 
    names(nodes.list) <- colnames(rowCol)
   
    metadata.json <- 
        list(
            "nodes" = nodes.list            
        )
    if(!is.null(rownames(rowCol))) {
        metadata.json[["feature_names"]] <- list(rownames(rowCol))
    }
    else {
        metadata.json[["feature_names"]] <- list("row label")
    }
    return(metadata.json)
}

# Given numeric dataframe
# convert to column metadata inchlib json format
# to be displayed as column colors in the heatmap
# Ex.
#> princomp(x = USArrests)$scores[1:2,]
# PC1 -64.80216 11.44801 -2.494933  2.407901
# PC2  -92.82745 17.98294 20.126575 -4.094047  
# "column_metadata" = 
colCol.to.json <- function(colCol) {
    col.metadata.json <- 
        list(
            "features"=list(colCol)
        )
    if(!is.null(rownames(colCol))) {
        col.metadata.json[["feature_names"]] <- list(rownames(colCol))
    }
    else {
        col.metadata.json[["feature_names"]] <- list("column label")
    }
    return(col.metadata.json)
}

