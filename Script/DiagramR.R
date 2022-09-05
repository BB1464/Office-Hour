library(DiagrammeR)


data <- list(a=1000, b=800, c=600, d=400)


DiagrammeR::grViz("digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = rectangle, width = 4, fillcolor = Biege]
a [label = '@@1']
b [label = '@@2']
c [label = '@@3']
d [label = '@@4']

a -> c b ->  c -> d

}

[1]: paste0('Raw Data (n = ', data$a, ')')
[2]: paste0('Remove Errors (n = ', data$b, ')')
[3]: paste0('Identify Potential Customers (n = ', data$c, ')')
[4]: paste0('Select Top Priorities (n = ', data$d, ')')
")







data <- list(a=1000, b=1000, c=1000)


DiagrammeR::grViz("digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = rectangle, width = 4, fillcolor = Biege]
a [label = '@@1']
b [label = '@@2']
c [label = '@@3']


a -> b ->  c

}

[1]: paste0('Excel')
[2]: paste0('SPSS or Genstart')
[3]: paste0('Microsoft Word')
")
