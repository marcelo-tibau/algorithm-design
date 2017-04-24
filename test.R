### Implementação do Algoritmo Dijkstra, que mede o caminho mais curto.
# Essa implementação segue as especificações dadas no livro "Algorithm Design" de Jon Kleinberg e Eva Tardos.


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


# Criação de uma lista de adjacência com dados:

edgesdf <- data.frame(nodea=c(1,2,4,2,1), nodeb=c(1,2,3,4,5))

adjalist <- by(edgesdf, edgesdf$nodea, function(x) x$nodeb)

for(i in as.character(unique(edgesdf$nodea))) {
  cat(i, '->', adjalist[[i]], '\n')
}


myadj <- stack(adjalist) 


# criação do grafo com base na lista de adjacência usando a biblioteca 'igraph' em R.
library(igraph)

grafo <- graph.data.frame(myadj)

plot(grafo)

graphdj <- grafo



# Criação de uma matriz de adjacência com dados:
datadij <- as.matrix(read.table(text=
                                  "node X1 X2 X3 X4 X5 X6
                            1  0  3  7  4 NA NA
                            2  3  0  2 NA NA  9
                            3  7  2  0  1  3  6
                            4  4 NA  1  0  3 NA
                            5 NA NA  3  3  0  3
                            6 NA  9  6 NA  3  0", header=T))

# Preparação dos dados para criar uma função para grafo: definir NA como zero para indicar que não há limite direto.
newdatadij <- datadij[,1]
datadij <- datadij[, -1]
colnames(datadij) <- rownames(datadij) <- newdatadij
datadij[is.na(datadij)] <- 0


# criação do grafo com base na matriz de adjacência usando a biblioteca 'igraph' em R.
library(igraph)

graphdj <- graph.adjacency(datadij, weighted=TRUE)

plot(graphdj)

# Definição dos parâmetros de input para a função:

# Número de nodes:
n = length(graphdj[,1])

# Fonte do node:
v = 1

# Destino do node:
dest = n

# Grafo:
cost = graphdj


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
  count = 10
  
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

prev = dijkstra(n,v,cost,dest)
path = save_path(prev,dest)

## Para mostrar path
path

### Comparação com a função já construída da biblioteca 'igraph'

dij.paths <- shortest.paths(graphdj, algorithm = "dijkstra")

