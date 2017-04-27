##Teste instâncias Algoritmo Dijkstra

# Test Set 1

ist.1.1 <- read.delim("inst_v100_s1.dat")
ist.2.1 <- read.delim("inst_v200_s1.dat")
ist.3.1 <- read.delim("inst_v300_s1.dat")
ist.4.1 <- read.delim("inst_v400_s1.dat")
ist.5.1 <- read.delim("inst_v500_s1.dat")
ist.6.1 <- read.delim("inst_v600_s1.dat")
ist.7.1 <- read.delim("inst_v700_s1.dat")
ist.8.1 <- read.delim("inst_v800_s1.dat")
ist.9.1 <- read.delim("inst_v900_s1.dat")
ist.10.1 <- read.delim("inst_v1000_s1.dat")

# Transformação em grafo e Lista de adjacência

library('igraph')


grafo.1.1 <- graph.data.frame(ist.1.1)
list.1.1 <- get.adjedgelist(grafo.1.1)

grafo.2.1 <- graph.data.frame(ist.2.1)
list.2.1 <- get.adjedgelist(grafo.2.1)

grafo.3.1 <- graph.data.frame(ist.3.1)
list.3.1 <- get.adjedgelist(grafo.3.1)

grafo.4.1 <- graph.data.frame(ist.4.1)
list.4.1 <- get.adjedgelist(grafo.4.1)

grafo.5.1 <- graph.data.frame(ist.5.1)
list.5.1 <- get.adjedgelist(grafo.5.1)

grafo.6.1 <- graph.data.frame(ist.6.1)
list.6.1 <- get.adjedgelist(grafo.6.1)

grafo.7.1 <- graph.data.frame(ist.7.1)
list.7.1 <- get.adjedgelist(grafo.7.1)

grafo.8.1 <- graph.data.frame(ist.8.1)
list.8.1 <- get.adjedgelist(grafo.8.1)

grafo.9.1 <- graph.data.frame(ist.9.1)
list.9.1 <- get.adjedgelist(grafo.9.1)

grafo.10.1 <- graph.data.frame(ist.10.1)
list.10.1 <- get.adjedgelist(grafo.10.1) 

# Para salvar as listas de adjacencias

saveRDS(list.1.1, file = 'list.1.1.rds')
saveRDS(list.2.1, file = 'list.2.1.rds')
saveRDS(list.3.1, file = 'list.3.1.rds')
saveRDS(list.4.1, file = 'list.4.1.rds')
saveRDS(list.5.1, file = 'list.5.1.rds')
saveRDS(list.6.1, file = 'list.6.1.rds')
saveRDS(list.7.1, file = 'list.7.1.rds')
saveRDS(list.8.1, file = 'list.8.1.rds')
saveRDS(list.9.1, file = 'list.9.1.rds')
saveRDS(list.10.1, file = 'list.10.1.rds')


library('data.tree')



# Test Set 2

ist.1.2 <- read.delim("inst_v100_s2.dat")
ist.2.2 <- read.delim("inst_v200_s2.dat")
ist.3.2 <- read.delim("inst_v300_s2.dat")
ist.4.2 <- read.delim("inst_v400_s2.dat")
ist.5.2 <- read.delim("inst_v500_s2.dat")
ist.6.2 <- read.delim("inst_v600_s2.dat")
ist.7.2 <- read.delim("inst_v700_s2.dat")
ist.8.2 <- read.delim("inst_v800_s2.dat")
ist.9.2 <- read.delim("inst_v900_s2.dat")
ist.10.2 <- read.delim("inst_v1000_s2.dat")




# Definição dos parâmetros de input para a função e transformação em heap binário

dfinst.10.1 <- readRDS(file = './listas_frequencia/list.10.1.rds')

n = length(dfinst.10.1) 
tree.10.1 <- make_tree(n, children = 2)

co <- layout.reingold.tilford(tree.10.1, params=list(root=1)) 
plot(tree.10.1, layout=co)

graphdj.10.1 <- graph.adjacency(dfinst.10.1, weighted=TRUE)

dfinst.2.1 <- readRDS(file = './listas_frequencia/list.2.1.rds')

n2 = length(dfinst.2.1)
tree.2.1 <- make_tree(n2, children = 2)

# Número de nodes:
n = length(tree.2.1[,1])

# Fonte do node:
v = 1

# Destino do node:
dest = n

# Custo:
cost = tree.2.1

is.vector(list(tree.2.1))

## Algoritmo Dijkstra
dijkstra = function(n,v,cost,dest){
  # criação de variáveis para guardar os dados
  dest = numeric(n)
  flag = numeric(n)
  prev = numeric(n)
  
  # para cada node da rede, medir a distância começando por v até i 
  for(i in 1:n){
    prev[i] = -1
    dest[i] = cost[v,i]
  }
  # para iniciar o contador que vai marcar o número de passos na rede
  count = 2
  
  # até chegar ao node de destino n
  while(count <= n){
    min = 9
    
    # fazer um loop para cada node
    for(w in 1:n){
      # checar se o caminho é menor do que o que já existe e se for, substituir o caminho mínimo anterior
      if(dest[w] < min && !flag[w]){
        min = dest[w]
        u = w
      }
    }
    # para indicar que a máquina tem que ir para o lugar especificado anteriormente
    flag[u] = 1
    count = count + 1
    
    # Para fazer novamente um loop para cada node, mas lembrando onde já passamos. 
    # Aqui repete o processo de checar o caminho mais curto, atualizar as distâncias e manter um track dos nodes visitados.
    for(w in 1:n){
      if((dest[u]+cost[u,w] < dest[w]) && !flag[w]){
        dest[w]=dest[u]+cost[u,w]
        prev[w]=u
      }
    }
  }
  return(prev)
}


## Criação de uma função que retorne o caminho.

save_path = function(f,x){
  path = x
  while(f[x] != -1){
    path = c(path, f[x])
    x = f[x]
    save_path(f,x)
  }
  path = c(path, 1)
  return(path)
}

## Rodar o algoritmo Dijkstra no grafo criado.

prev2 <- dijkstra(n,v,cost,dest)
path2 <- save_path(prev2, dest) 

prev10 <- dijkstra(n,v,cost,dest)
path10 <- save_path(prev10, dest)

prev = dijkstra(n,v,cost,dest)
path = save_path(prev,dest)

## Para mostrar path
path
