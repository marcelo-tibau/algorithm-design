#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <time.h>
#include <iostream>
#include <vector>
#define INFINITO 1000000

// Struct para representar um nó na lista de adjacência
struct VerticeListaAdj
{
    int destino;
    int peso;
    struct VerticeListaAdj* prox;
};

// Struct para representar a lista de adjacência
struct ListaAdj
{
    struct VerticeListaAdj *inicio;  // ponteiro para o início da fila
};

// Struct para representar um grafo. Um grafo é um array de listas de adjacência.
// O tamanho do array é V (conjunto de vértices do grafo)
struct Grafo
{
    int V;
    struct ListaAdj* array;
};

// Função que cria um novo nó na lista de adjacência do grafo direcionado
struct VerticeListaAdj* insereVerticeListaAdj(int destino, int peso)
{
    struct VerticeListaAdj* insereVertice = (struct VerticeListaAdj*) malloc(sizeof(struct VerticeListaAdj));
    insereVertice->destino = destino;
    insereVertice->peso = peso;
    insereVertice->prox = NULL;
    return insereVertice;
   
}


// Função que cria um grafo com V vértices
struct Grafo* constroiGrafo(int V)
{
    struct Grafo* grafo = (struct Grafo*) malloc(sizeof(struct Grafo));
    grafo->V = V;

    // Cria um array de listas de adjacência.  O tamanho do array é igual a V
    grafo->array = (struct ListaAdj*) malloc(V * sizeof(struct ListaAdj));

     // Inicializa as listas de adjacência vazias e atribui o valor NULL ao nó inicial
    for (int i = 0; i < V; ++i)
        grafo->array[i].inicio = NULL;

    return grafo;
}


// Manipulação de arquivos
FILE* abreArquivo(char modo, char caminho[30])
{
    FILE *arquivo;
    switch(modo)
    {
        case 'g':
            arquivo = fopen(caminho, "wt");
            break;
        case 'l':
            arquivo = fopen(caminho, "rt");
            break;
        case 'a':
            arquivo = fopen(caminho, "a");
            break;
    }
    if(arquivo == NULL)
	{
        printf("Nao foi possivel abrir o arquivo");
        exit(0);
	}
	return arquivo;
}

void fechaArquivo(FILE* arquivo)
{
    fclose(arquivo);
}



// Inclui uma aresta em no grafo (direcionado)
void insereAresta(struct Grafo* grafo, int origem, int destino, int peso)
{
    // Inclui uma aresta da origem ao destino. Um novo nó é adicionado à lista de adjacência
    // da origem.  O nó é colocado no início da lista
    struct VerticeListaAdj* insereVertice = insereVerticeListaAdj(destino, peso);
    insereVertice->prox = grafo->array[origem].inicio;
    grafo->array[origem].inicio = insereVertice;
    
     // Insere uma aresta de volta para tornar o grafo bi-direcional (ALUE e DMXA)
    insereVertice = insereVerticeListaAdj(origem, peso);
    insereVertice->prox = grafo->array[destino].inicio;
    grafo->array[destino].inicio = insereVertice;
}

// Struct que representa o nó do vetor
struct VerticeVectorMin
{
    int  v;
    int dist;
};

// Struct que representa o vetor
struct VectorMin
{
    int tamanho;      // Número de nós que o vetor possui no momento
    int capacidade;  // Capacidade do vetor
    int *pos;     // Ponteiro necessário para a função diminuichave()
    struct VerticeVectorMin **array;
};

// Função que cria um novo nó no vetor
struct VerticeVectorMin* insereVerticeVectorMin(int v, int dist)
{
    struct VerticeVectorMin* vectorMinVertice =
           (struct VerticeVectorMin*) malloc(sizeof(struct VerticeVectorMin));
    vectorMinVertice->v = v;
    vectorMinVertice->dist = dist;
    return vectorMinVertice;
}

// Função que cria o vetor
struct VectorMin* constroiVectorMin(int capacidade)
{
    struct VectorMin* vectorMin = (struct VectorMin*) malloc(sizeof(struct VectorMin));
    vectorMin->pos = (int *)malloc(capacidade * sizeof(int));
    vectorMin->tamanho = 0;
    vectorMin->capacidade = capacidade;
    vectorMin->array = (struct VerticeVectorMin**) malloc(capacidade * sizeof(struct VerticeVectorMin*));
    return vectorMin;
}


// Função para checar se o vetor está vazio
int ehVazia(struct VectorMin* vectorMin)
{
    return vectorMin->tamanho == 0;
}


// Função para extrair o menor nó do vetor
struct VerticeVectorMin* removeMin(struct VectorMin* vectorMin)
{
    if (ehVazia(vectorMin))
        return NULL;

    // Variáveis para guardar o menor peso e sua posição
    int menorP, posMenorP;

    // Variáveis para percorrer a lista de adjacência
    struct VerticeVectorMin* minElem;
    struct VerticeVectorMin* temp = vectorMin->array[0];//configura o primeiro vértice como o menor peso
    menorP = vectorMin->array[0]->dist;
    posMenorP = 0;

    minElem = temp; // Começa considerando primeiro vértice como sendo o menor

    // Percorre todo o vetor
    for(int i= 1; i < sizeof(vectorMin); i++){
	    	temp = vectorMin->array[i]; // Guarda o vértice atual na variável temp
	    	int pesoAtual = vectorMin->array[i]->dist; // Guarda o peso atual
	    	// Se o peso do menor vetor até o momento for maior que o atual, faz a troca
	    	if(menorP > pesoAtual){
	      		minElem = temp;
	      		menorP = pesoAtual;
	      		posMenorP = i;
	    	}
    }

    printf("posMenorP: %d\n", posMenorP);
    //retira o elemento mínimo e move os próximos para a sua posição
    for (int i = posMenorP; i < sizeof(vectorMin); i++ ){
	  		vectorMin->array[i] = vectorMin->array[i+1];

		}

	// Reduz o tamanho do vetor
    --vectorMin->tamanho;
    printf("Tamanho do vetor: %d\n", vectorMin->tamanho);
    // Retorna o menor elemento
    return minElem;
}

// Função para decrementar o valor da distância de um vértice v.
void diminuiChave(struct VectorMin* vectorMin, int v, int dist)
{
    // Faz a troca do valor do peso no vetor caso o valor seja menor
    for(int i = 0; i < sizeof(vectorMin); i++){
        if (v == vectorMin->array[i]->v){
            if (dist < vectorMin->array[i]->dist){
                vectorMin->array[i]->dist = dist;
            }
        }
    }

}

// Função para checar se uma aresta
// 'v' está no vetor ou não
bool existeNoVectorMin(struct VectorMin *vectorMin, int v)
{
   if (vectorMin->pos[v] <= vectorMin->tamanho)
     return true;
   return false;
}

// Função para imprimir a solução
void imprime(int dist[], int n)
{
    printf("Menor distância do nó a partir da origem\n");
    for (int i = 0; i < n; ++i)
        printf("%d \t\t %d\n", i, dist[i]);
}

// A função principal que calcula as distâncias dos caminhos mais curtos da origem para todos
// os outros nós. É uma função com complexidade O(ELogV)
void dijkstra(struct Grafo* grafo, int origem)
{
    // Variáveis para medir o tempo de execução
    float tempo;
    clock_t t_inicio, t_fim;

    t_inicio = clock(); // Guarda o horário do início da execução


    int V = grafo->V;// Recebe o número de vértices do grafo
    int dist[V];      // Valores das distâncias usadas para escolher a aresta de menor peso

    // Vetor representa o conjunto E (arestas)
    struct VectorMin* vectorMin = constroiVectorMin(V);

    // Inicializa o vetor que guarda os resultados com todos os vértices. atribui o valor da distância de todos os vértices
    for (int v = 0; v < V; ++v)
    {
        dist[v] = INT_MAX;
        vectorMin->array[v] = insereVerticeVectorMin(v, dist[v]);
        vectorMin->pos[v] = v;
    }

    // Faz com que o valor da distância do vértice de origem seja igual a 0 para que seja extraído primeiro
    vectorMin->array[origem] = insereVerticeVectorMin(origem, dist[origem]);
    vectorMin->pos[origem]   = origem;
    dist[origem] = 0;
    diminuiChave(vectorMin, origem, dist[origem]);

    // Inicializa o tamanho do vetor igual a V
    vectorMin->tamanho = V;

    // Vertice laço seguinte, vetor contém todos os nós
    // para os quais o  caminho mais curto ainda não foi finalizado.
    while (!ehVazia(vectorMin))
    {
        // Extrai o vértice com o menor valor de distância
        struct VerticeVectorMin* vectorMinVertice = removeMin(vectorMin);
        int u = vectorMinVertice->v; // Guarda o número do vértice extraído
        
        printf("Elemento: %d - Peso: %d \n", vectorMinVertice->v, vectorMinVertice->dist);

        // Passa por todos os vértices visitados de u (o vértice extraído)
        // e atualiza os valores de suas distâncias
        struct VerticeListaAdj* visitado = grafo->array[u].inicio;
        while (visitado != NULL)
        {
            int v = visitado->destino;

            // Se a menor distância para v não está finalizada, e a distância para v
            // passando por u é menor que sua distância calculada anterior..
            if (existeNoVectorMin(vectorMin, v) && dist[u] != INT_MAX && visitado->peso + dist[u] < dist[v])
            {
                dist[v] = dist[u] + visitado->peso;

                // ..atualiza o valor da distância no vetor também
                diminuiChave(vectorMin, v, dist[v]);
            }
            visitado = visitado->prox;
        }
    }

    t_fim = clock(); // Guarda o horario do fim da execução

    tempo = (float)(t_fim - t_inicio)*1000/CLOCKS_PER_SEC; // Calcula o tempo de execução


    FILE *arquivoSaida;
    arquivoSaida = abreArquivo('a',"C:\\Users\\Marcelo\\Documents\\Work\\new2\\saida\\saida_inst_v100_s1_d3.txt");

    // Imprime o tempo de execução
    fprintf(arquivoSaida, "\nTempo total de execucao: %f milissegundo(s).\n\n", tempo);

    // Imprime as menores distâncias calculadas
    for (int i = 0; i < V; ++i)
        fprintf(arquivoSaida, "%d \t %d\n", i, dist[i]);

    fechaArquivo(arquivoSaida);
}


// Programa main para testar as funções acima
int main()
{

    // testes com arquivos

	FILE *arquivoEntrada;
//	FILE *arquivoSaida;
    char prefixo[30];
    int valor1, valor2, valor3;
	int V;
    struct Grafo* grafo = constroiGrafo(0);

	arquivoEntrada = abreArquivo('l', "C:\\Users\\Marcelo\\Documents\\Work\\new2\\entrada\\test-set1\\test-set1\\inst_v100_s1.dat");


	while(!feof(arquivoEntrada))
	{

        fscanf(arquivoEntrada, "%s %d %d %d" , &prefixo, &valor1, &valor2, &valor3);
        if(strcmp(prefixo, "V") == 0)
        {
            V = valor1;

            printf("Total de vertices do grafo: %d \n\n", V);
            grafo = constroiGrafo(V);
        }
        if(strcmp(prefixo, "E") == 0){
            insereAresta(grafo, valor1, valor2, valor3);
            //insereAresta(grafo, valor2, valor1, valor3);
            //fprintf(arquivoSaida, "%s %d %d %d\n", prefixo, valor1, valor2, valor3);
        }
	}



	fechaArquivo(arquivoEntrada);
//	fechaArquivo(arquivoSaida);


    dijkstra(grafo, 0); // Executa o Dijkstra para o grafo

    return 0;
}
