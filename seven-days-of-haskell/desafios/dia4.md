Até agora, você praticou o consumo de dados de uma API. Isto é, exercitou a maneira de receber os dados JSON de uma chamada HTTP e parseá-los, a fim de transformá-los em objetos.

No desafio de hoje, você vai trabalhar com a saída e gerar uma página HTML a partir da lista de objetos que você já tem no seu código Java.

A ideia será criar uma página HTML onde você possa ver as informações sobre o filme, incluindo o pôster, algo como:

![ea7f1ac2-f61b-49e6-a3ff-e1aad5463677](https://empresas.alura.com.br/hs-fs/hubfs/ea7f1ac2-f61b-49e6-a3ff-e1aad5463677.png?width=576&upscale=true&name=ea7f1ac2-f61b-49e6-a3ff-e1aad5463677.png)

Para tal, vou dar um pequeno passo-a-passo para te auxiliar:

    Crie uma nova classe HTMLGenerator, que irá receber no construtor um Writer (por exemplo, PrintWriter)

    Adicione um método chamado ‘generate’, que irá receber uma List<Movie>. Nesse método, gere todo o HTML a partir da lista, usando as informações do objeto. Você pode usar métodos privados para delegar responsabilidades.
    (Obs: Você deve criar e fechar o Writer no método main)

Nesse momento, você pode estar se perguntando se é uma boa prática gerar um HTML dentro de uma classe Java. Realmente não é, pois existem bibliotecas de mais alto nível que simplificam isso, mas para este desafio de Java e a fim de praticar alguns conceitos sobre Orientação a Objetos, podemos fazer uma exceção!
