Recapitulando, no primeiro dia você fez uma chamada à API do IMDB para buscar um JSON com os top 250 filmes. No segundo dia, você extraiu algumas informações desse JSON, como o título dos filmes e a URL dos pôsteres.

Como resultado, você provavelmente obteve algumas listas no seu código para guardar esses atributos, algo como:

//chamada da API omitida

List<String> titles = parseTitles(moviesArray);
titles.forEach(System.out::println);

List<String> urlImages = parseUrlImages(moviesArray);
urlImages.forEach(System.out::println);

//outras listas para anos e notas

No desafio de hoje, a ideia será modelar, ou pelo menos iniciar uma modelagem melhor do seu código.

Pensando um pouco sobre Orientação a Objetos, uma pergunta simples pode ajudar: no caso do seu projeto, o título e o pôster se referem a que tipo de objeto? A um filme, claro. Mas você ainda não tem nenhuma estrutura que defina o que é um filme.

Qual vai ser a cara disso? Um filme (Movie) deve ter os seguintes atributos:

    título (title)
    URL da imagem do pôster (urlImage)
    nota (rating)
    ano (year)

Em outras palavras, em vez de ter várias listas diferentes, uma para cada atributo do filme, é bem melhor organizar isso em uma única List<Movie>, onde cada filme encapsula os seus próprios dados. Bora implementar essa classe?