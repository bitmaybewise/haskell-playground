No último desafio, você encapsulou toda a geração do HTML dos filmes dentro de uma classe. Uma possível implementação poderia funcionar da seguinte forma:

```java
PrintWriter writer = new PrintWriter("content.html");

new HtmlGenerator(writer).generate(movies); //movies é um List<Movie>

writer.close();
```

Repare que os detalhes sobre a geração do HTML não importam para quem lê o código, pois a geração do HTML foi encapsulada, e isso é ótimo. No entanto, você ainda tem provavelmente outros códigos expostos - eu estou me referindo ao código sobre a chamada HTTP da API e também ao outro que faz o parseamento do JSON.

A sua primeira tarefa neste desafio será encapsular a chamada da API dentro de uma nova classe. Você pode chamar essa classe de ImdbApiClient.

Além disso, uma segunda tarefa: o código que faz o parseamento do JSON ainda está “solto”! Para melhorar o encapsulamento e separar todas as responsabilidades em suas devidas classes, crie uma nova classe para fazer o parseamento do JSON, algo como:

String json = //chamada da API

List<Movie> movies = new ImdbMovieJsonParser(json).parse()
   
//gerando HTML
//…

Mãos à obra!