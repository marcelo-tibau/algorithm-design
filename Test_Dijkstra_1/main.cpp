// Implementação do algoritmo Dijkstra para lista de adjacência, usando C

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
 
int main()
{
   FILE *inst_1_1;
   char str[60];
   
   // Abrindo as instâncias//
   inst_1_1 = fopen("inst_v100_s1.dat" , "r");
      if( fgets (str, 60, inst_1_1)!=NULL ) 
   {
      //acessando o conteúdo stdout//
      puts(str);
   }
   fclose(inst_1_1);
   
   return(0);
}
 
// Estrutura para representar o nó na lista de adjacência
struct AdjListNode
{
    int dest;
    int weight;
    struct AdjListNode* next;
};

// Estrutura para representar a lista de adjacência
struct AdjList
{
    struct AdjListNode *head;
};
 
// Estrutura para representar o grafo, com tamanho de arestas V
struct Graph
{
    int V;
    struct AdjList* array;
};

 // Função para criar um novo nó na lista de adjacência
 struct AdjListNode* newAdjListNode(int dest, int weight)
 {
     struct AdjListNode* newNode=
            (struct AdjListNode*) malloc(sizeof(struct AdjListNode));
            newNode->dest = dest;
            newNode->weight = weight;
            newNode->next = NULL;
            return newNode;
 }

// Função para criar grafo com vertices V
struct Graph* createGraph(int V)
{
    struct Graph* graph = (struct Graph*) malloc(sizeof(struct Graph));
    graph->V = V;
    
    // Criar aresta de lista de adjacências de tamanho V
    graph->array = (struct AdjList*) malloc(V * sizeof(struct AdjList));
    
    // Inicializar lista de adjacência vazia
    for (int i = 0; i < V; ++i)
        graph->array[i].head = NULL;
        
    return graph;
}  
 
// Inclusão de vertice no grafo levando a inclusão de nó na lista de adjacência
void addEdge(struct Graph* graph, int src, int dest, int weight)
{
    struct AdjListNode* newNode = newAdjListNode(dest, weight);
    newNode->next = graph->array[src].head;
    graph->array[src].head = newNode;
    
    // Inclusão de vertice da fonte para o destino
    newNode = newAdjListNode(src, weight);
    newNode->next = graph->array[dest].head;
    graph->array[dest].head = newNode;
}

 // Estrutura para representar o nó do heap binário
 struct MinHeapNode
 {
     int v;
     int dist;
 };
 
 
// Estrutura para representar o heap binário 
struct MinHeap
{
    int size;
    int capacity;
    int*pos;
    struct MinHeapNode **array;
};

// Função para criar o nó do heap binário
struct MinHeapNode* newMinHeapNode(int v, int dist)
{
    struct MinHeapNode* minHeapNode =
    (struct MinHeapNode*) malloc(sizeof(struct MinHeapNode));
    minHeapNode->v = v;
    minHeapNode->dist = dist;
    return minHeapNode;
}

// Função para criar o heap binário
struct MinHeap* createMinHeap(int capacity)
{
    struct MinHeap* minHeap =
    (struct MinHeap*) malloc(sizeof(struct MinHeap));
    minHeap->pos = (int *)malloc(capacity * sizeof(int));
    minHeap->size = 0;
    minHeap->capacity = capacity;
    minHeap->array = (struct MinHeapNode**) malloc(capacity * sizeof(struct MinHeapNode*));
    return minHeap;
    
}
 
// Função para fazer o min heapify
void swapMinHeapNode(struct MinHeapNode** a, struct MinHeapNode** b)
{
    struct MinHeapNode* t = *a;
    *a = *b;
    *b = t;
}
 
// Função para fazer o update dos nós
 
void minHeapify(struct MinHeap* minHeap, int idx)
{
    int smallest, left, right;
    smallest = idx;
    left = 2 * idx + 1;
    right = 2 * idx + 2;
    
    if (left < minHeap->size &&
    minHeap->array[right]->dist < minHeap->array[smallest]->dist)
        smallest = left;
        
    if (right < minHeap->size &&
    minHeap->array[right]->dist < minHeap->array[smallest]->dist)
        smallest = right;
        
    if (smallest != idx)
    {
        // Nós a serem trocados no min heap
        MinHeapNode * smallestNode = minHeap->array[smallest];
        MinHeapNode* idxNode = minHeap->array[idx];
        
        // Troca de posição
        minHeap->pos[smallestNode->v] = idx;
        minHeap->pos[idxNode->v] = smallest;
        
        // Troca de nós
        swapMinHeapNode(&minHeap->array[smallest], &minHeap->array[idx]);
        
        minHeapify(minHeap, smallest);
        
    }
        
}
 
 
// Função para checar se o min heap está vazio
int isEmpty(struct MinHeap* minHeap)
{
    return minHeap->size == 0;
}
 
// Função para extrair o nó mínimo do heap
struct MinHeapNode* extractMin(struct MinHeap* minHeap)
{
    if (isEmpty(minHeap))
        return NULL;
 
    // Guardar o nó raíz 
    struct MinHeapNode* root = minHeap->array[0];
 
    // Substituir o nó raíz com o último nó 
    struct MinHeapNode* lastNode = minHeap->array[minHeap->size - 1];
    minHeap->array[0] = lastNode;
 
    // Update da posição do último nó
    minHeap->pos[root->v] = minHeap->size-1;
    minHeap->pos[lastNode->v] = 0;
 
    // Reduzir o tamanho do heap e heapify a raiz 
    --minHeap->size;
    minHeapify(minHeap, 0);
 
    return root;
}
 
 
 // Função para checar a distância do vértice e checar a indexação atual do nó
 void decreaseKey(struct MinHeap* minHeap, int v, int dist)
 {
     int i = minHeap->pos[v];
     minHeap->array[i]->dist = dist;
     while (i && minHeap->array[i]->dist < minHeap->array[(i - 1) / 2]->dist)
     {
         // Troca do nó com os dos pais
         minHeap->pos[minHeap->array[i]->v] = (i-1)/2;
         minHeap->pos[minHeap->array[(i-1)/2]->v] = i;
         swapMinHeapNode(&minHeap->array[i], &minHeap->array[(i-1) /2]);
         i = (i - 1) /2;
     }
 }
 
// Função para checar se o vertex 'v' está em min heap

bool isInMinHeap(struct MinHeap *minHeap, int v)
{
    if (minHeap->pos[v] < minHeap->size)
        return true;
        return false;
}
 
// Função para imprimir a solução
void printArr(int dist[], int n)
{
    printf("Distância do vértice da fonte\n");
    for (int i = 0; i < n; ++i)
        printf("%d \t\t %d\n", i, dist[i]);
}
 
// Função Dijkstra para calcular o caminho mínimo da fonte para todos os vértices 
void dijkstra(struct Graph* graph, int src)
{
    int V = graph->V;
    int dist[V];
    
    struct MinHeap* minHeap = createMinHeap(V);
    for (int v = 0; v < V; ++v)
    {
        dist[v] = INT_MAX;
        minHeap->array[v] = newMinHeapNode(v, dist[v]);
        minHeap->pos[v]= v;
    }
    // Transformar o vértice fonte em 0 para extraí-lo primeiro
    minHeap->array[src] = newMinHeapNode(src, dist[src]);
    minHeap->pos[src] = src;
    dist[src] = 0;
    decreaseKey(minHeap, src, dist[src]);
    
    // Inicializar o tamanho do min heap como sendo 2
    minHeap->size = V;
    
    // Incluir todos os nós que não tiveram sua menor distância finalizada
    
    while (!isEmpty(minHeap))
    {
        struct MinHeapNode* minHeapNode = extractMin(minHeap);
        int u = minHeapNode->v;
        
        struct AdjListNode* pCrawl = graph->array[u].head;
        while (pCrawl != NULL)
        {
            int v = pCrawl->dest;
            
            if (isInMinHeap(minHeap, v) && dist[u] != INT_MAX && 
                                           pCrawl->weight + dist[u] < dist[v])
            
            {
                dist[v] = dist[v] + pCrawl->weight;
                decreaseKey(minHeap, v, dist[v]);
            }
            pCrawl = pCrawl->next;
        }
    }
    printArr(dist, V);
 
}
 
 