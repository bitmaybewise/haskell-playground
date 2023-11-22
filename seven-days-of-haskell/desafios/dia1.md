Este primeiro dia é super importante! Ao final dele, você terá um novo conhecimento que é essencial para os próximos desafios.

Muita gente na nossa área de tecnologia é fanática por cinema, séries e filmes. Por isso mesmo, o tema geral do nosso desafio vai ter tudo a ver com isso.

Você conhece o IMDB? É provavelmente a plataforma mais famosa que agrupa basicamente todos os filmes, séries, programas de TV, atores, etc., do mundo. Você pode imaginar que o banco de dados deles deve ser colossal!

No desafio de hoje, você vai fazer o seu código Java rodar e consumir a API do IMDB! Seu objetivo será imprimir os resultados de uma busca na linha de comando.

Explicando melhor, você vai usar essa API para pesquisar os top 250 filmes e imprimir o JSON correspondente no console da sua IDE.

Para isso, você pode acessar o webservice ou API da plataforma em:

https://imdb-api.com/api

O site do IMDB tem uma documentação bem legal para você se familiarizar com os parâmetros da chamada e analisar o retorno.

Ahh, também será preciso gerar uma APIKey (uma chave de acesso), registrando o seu email no site. Com a chave em mãos, você poderá buscar filmes, séries, etc., através da API.

Por exemplo, para buscar o nome de um filme, basta usar:

https://imdb-api.com/en/API/Top250Movies/<apiKey>https://imdb-api.com/en/API/Top250Movies/

O resultado da pesquisa será um JSON parecido com esse:

{"items":[{"id":"tt5491994","rank":"1","title":"Planet Earth II","fullTitle":"Planet Earth II (2016)"…

Mas chegou a hora de escrever o código e fazer a coisa funcionar! Vamos lá? Vou passar aqui abaixo uma espécie de passo-a-passo, para você não se perder muito neste primeiro dia. O que você vai precisar fazer:

    Criar uma conta no IMDB para ter a chave de acesso ao serviço (apiKey), mas cuidado, essa chave não deve ser commitada no Github ou em outro repositório!

    Criar o código Java que executará uma requisição HTTP do tipo GET. Você pode usar o pacote java.net.http e as classes HttpRequest, HttpClient e HttpResponse, além da classe URI

    Executar a requisição e pegar a resposta (o JSON)

    Imprimir o corpo da resposta no console


Simples, não? Nesse exercício, eu sugeri usar as classes do pacote java.net.http, mas você pode escolher qualquer outra. Se quiser, você pode inclusive usar outros conectores HTTP.

Para esse início, faça tudo no main mesmo! Posteriormente, vou te guiar na evolução desse código, para que ele seja mais maleável, orientado a objetos, e para te gerar algumas reflexões também.