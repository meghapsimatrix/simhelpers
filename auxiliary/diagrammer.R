library(DiagrammeR)

grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']

      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3 -> tab4 -> tab5;
      }

      [1]: 'Experimental Design'
      [2]: 'Data Generating Model'
      [3]: 'Estimation Methods'
      [4]: 'Performance Summaries'
      [5]: 'Results'
      ")
