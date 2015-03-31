source('inchlib_clust.R')

# Sample data
data <- data.matrix(mtcars[1:20,])

# Goal is to recreate this heatmap via InCHlib
map <- heatmap(data,
    keep.dendro = TRUE,
    col = colorRampPalette(c("white", "red"))(100),
    scale = "column"
)
hc <- map$Rowv
vc <- map$Colv
order <- rev(map$colInd)

# Draw row dendrogram with data
row.dend <- dend.to.table(hc)        
row.dend.json <- row.dend.to.json(row.dend, data[,order])

# Draw column dendrogram
col.dend <- dend.to.table(vc)
col.dend.json <- col.dend.to.json(col.dend)

# Uncomment to add row side colors
#rowCol <- t(abs(prcomp(x = data)$x[,1]))
#rowCol.json <- rowCol.to.json(rowCol)

# Uncomment to add column side colors
#colCol <- t(prcomp(x=t(data))$x[,1])
#colCol.json <- colCol.to.json(colCol)

# Final json 
json <- toJSON(
     list(
	"data" = row.dend.json,
        "column_dendrogram" = col.dend.json
        #"metadata" = rowCol.json,
        #"column_metadata" = colCol.json
     )
)
# Write to file
cat(json,file="example.json")

# Compare, need to reorder for better comparison
heatmap(data,
    Rowv = hc, Colv = rev(vc),
    col = colorRampPalette(c("white", "red"))(100),
    scale = "column",
    revC = T
)

