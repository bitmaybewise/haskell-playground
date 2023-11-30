Olá, Hercules!

Esse é o seu último dia de #7DaysOfCode com Java! Você avançou muito no seu projeto e, para fechar com chave de ouro, você irá voltar a mexer com o seu modelo.

E você pode me perguntar: mas por quê? Ao usar a API do IMDB, os filmes já vêm com uma ordem predefinida, descendente, baseada na nota dos filmes (rating). Tudo bem até aí, mas e se você quiser ordenar os filmes com base em algum outro critério, talvez pela ordem alfabética do nome ou pelo ano?

Bora praticar então?

No mundo Java, é possível ordenar uma coleção usando o método sort() da classe Collections, cuja base é algo como:

Collections.sort(contentList); //mas qual é o critério de ordenação?

Esse método sabe ordenar listas, desde que você defina uma regra - é aí que vai entrar o seu modelo. A partir dele, você poderá definir essa regra, que também é chamada de “ordem natural”. Ou seja, para ter essa regra no modelo, você deverá implementar um método bem definido pela interface Comparable. Dessa forma, os objetos da classe se tornarão comparáveis.

Bora lá então? Vou te dar novamente um pequeno passo-a-passo para ajudar:

    Implemente a interface Comparable<? extends Content> na classe (ou record) Movie (e também na classe Series, se você a tiver criado).
    Para começar, você pode implementar o método usando a nota (rating) como parâmetro de comparação. Por exemplo:

    public int compareTo(Content outro) {
        return this.rating().compareTo(outro.rating());
    }

    No método main, para ordenar a lista e gerar o HTML, use:

    Collections.sort(contentList);

    O método sort() está sobrecarregado, então você pode passar um Comparator como segundo parâmetro para inverter a lista:

    Collections.sort(contentList, Comparator.reverseOrder());

---

EXERCÍCIO EXTRA

Não pare por aí! Vamos usar este último dia para praticar mais e mais de Java!

    Implemente outras formas de comparação usando um Comparator. Você pode usar o método Comparator.comparing(a, b), que recebe a lista e o método que será utilizado na comparação.

    Se você implementou a API da Marvel, junte as listas das séries e dos filmes em uma só. Para tal, use a API de Streams para fazer um join, algo como:

    List<? extends Content> mixedList = Stream.of(seriesList, moviesList)

.flatMap(Collection::stream)
.collect(Collectors.toList());

    Ao misturar as duas listas, pode fazer sentido indicar na sua página HTML se o elemento da lista é uma série ou filme. Para tal, crie um novo método type() na interface Content. Implemente esse método no modelo (em Series e em Movie) e mostre o resultado no seu HTML.

![movie](https://empresas.alura.com.br/hs-fs/hubfs/d19255d0-5b97-450b-9f5e-54e2d2535855.png?width=564&upscale=true&name=d19255d0-5b97-450b-9f5e-54e2d2535855.png)

Repare acima na palavra “Movie” abaixo do título e acima do pôster, indicando que se trata de um filme (e não de uma série ou história em quadrinhos).

