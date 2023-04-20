KruskalPostHoc <- function(grupo, variable, direccion, nombre) {
  # Realizar la prueba de Kruskal-Wallis
  kruskal <- kruskal.test(grupo ~ variable)
  print(kruskal)
  
  # Realizar el análisis post hoc 
  wilcox <- pairwise.wilcox.test(variable, grupo, p.adjust.method = "bonferroni")
  print(wilcox)
  capture.output(nombre, "Kruskal-Wallis rank sum test",kruskal,"Pairwise comparisons using Wilcoxon rank sum test with continuity correction ",wilcox, file = direccion)
}

