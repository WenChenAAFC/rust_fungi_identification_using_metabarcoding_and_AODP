# An R function that wraps DNA sequences in HTML spans with nucleotide-specific colors, 
# and then prints them using the htmltools::HTML() function 
# so Quarto renders it with full color in HTML output.

color_dna <- function(dna_stringset, n = 5) {
  library(Biostrings)
  library(htmltools)
  
  # Define colors for each base
  base_colors <- c(
    A = "#A0FFA0", # green
    C = "#A0C0FF", # blue
    G = "#FFD080", # orange
    T = "#FFB0B0", # pink
    N = "#CCCCCC"  # gray for unknown
  )
  
  out <- character()
  for (i in seq_len(min(n, length(dna_stringset)))) {
    seq <- as.character(dna_stringset[i])
    width <- width(dna_stringset[i])
    name <- names(dna_stringset)[i]
    
    # Color each character
    color_seq <- paste0(
      vapply(strsplit(seq, "")[[1]], function(base) {
        col <- base_colors[base]
        if (is.na(col)) col <- "#EEEEEE"
        sprintf('<span style="background-color:%s">%s</span>', col, base)
      }, character(1)),
      collapse = ""
    )
    
    out <- c(out, sprintf(
      "<pre><b>[%d]</b> %d bp  %s  <code>%s</code></pre>",
      i, width, name, color_seq
    ))
  }
  
  HTML(paste(out, collapse = "\n"))
}
