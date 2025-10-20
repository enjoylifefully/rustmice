% --- 1. DECLARAÇÕES DINÂMICAS ---
% Declaramos que 'mapa' e 'estado_inicial' serão adicionados em tempo de execução.
:- dynamic mapa/2.
:- dynamic estado_inicial/1.

% --- 2. O ESTADO (Será lido de stdin) ---
% (estado_inicial(pos(L, C)) será "asserted"
%  pelo predicado run_solver/0)

% --- 3. REGRAS DE INTERAÇÃO COM O MAPA ---

% tipo_celula(+Posicao, -Tipo)
% Pega o tipo de célula (c, p, o, j) em uma dada posição pos(L, C).
tipo_celula(pos(L, C), Tipo) :-
    mapa(L, LinhaLista),      % Encontra a linha
    nth0(C, LinhaLista, Tipo). % Encontra o item na coluna

% pode_entrar(+TipoDeCelula)
% Define em quais tipos de célula o jogador pode andar.
pode_entrar(c).  % Pode andar no 'chão'
pode_entrar(o).  % Pode andar sobre o 'objetivo'
pode_entrar(j).  % << NOVO: O ponto inicial ('j') também é caminhável.

% estado_vitoria(+Estado)
% O jogo é ganho se o estado atual (posição) for do tipo 'o'.
estado_vitoria(Estado) :-
    tipo_celula(Estado, o).

% --- 4. A LÓGICA DE MOVIMENTO (Transição de Estado) ---
% (Esta seção é IDÊNTICA à sua, não precisa mudar nada)

% Mover para Cima
mover(pos(L, C), cima, NovoEstado) :-
    NovaL is L - 1,
    PosicaoDesejada = pos(NovaL, C),
    (   once( (tipo_celula(PosicaoDesejada, Tipo), pode_entrar(Tipo)) )
    ->  NovoEstado = PosicaoDesejada
    ;   NovoEstado = pos(L, C)
    ).

% Mover para Baixo
mover(pos(L, C), baixo, NovoEstado) :-
    NovaL is L + 1,
    PosicaoDesejada = pos(NovaL, C),
    (   once( (tipo_celula(PosicaoDesejada, Tipo), pode_entrar(Tipo)) )
    ->  NovoEstado = PosicaoDesejada
    ;   NovoEstado = pos(L, C)
    ).

% Mover para Direita
mover(pos(L, C), direita, NovoEstado) :-
    NovaC is C + 1,
    PosicaoDesejada = pos(L, NovaC),
    (   once( (tipo_celula(PosicaoDesejada, Tipo), pode_entrar(Tipo)) )
    ->  NovoEstado = PosicaoDesejada
    ;   NovoEstado = pos(L, C)
    ).

% Mover para Esquerda
mover(pos(L, C), esquerda, NovoEstado) :-
    NovaC is C - 1,
    PosicaoDesejada = pos(L, NovaC),
    (   once( (tipo_celula(PosicaoDesejada, Tipo), pode_entrar(Tipo)) )
    ->  NovoEstado = PosicaoDesejada
    ;   NovoEstado = pos(L, C)
    ).

%----------------------------------------------------------------------------------------%
% --- 5. O SOLVER (A "IA") ---
% (Esta seção é IDÊNTICA à sua)

% Helper para definir as ações possíveis
acao(cima).
acao(baixo).
acao(esquerda).
acao(direita).

% resolver(-Caminho)
resolver(Caminho) :-
    estado_inicial(EstadoInicial),
    resolver_com_visitados(EstadoInicial, [EstadoInicial], Caminho).

% resolver_com_visitados(+EstadoAtual, +Visitados, -Caminho)

% REGRA 1: CASO BASE (Vitória)
resolver_com_visitados(Estado, _Visitados, []) :-
    estado_vitoria(Estado).

% REGRA 2: PASSO RECURSIVO (Movimento)
resolver_com_visitados(EstadoAtual, Visitados, [Acao | RestoDoCaminho]) :-
    acao(Acao),
    mover(EstadoAtual, Acao, ProximoEstado),
    \+ member(ProximoEstado, Visitados),
    resolver_com_visitados(ProximoEstado, [ProximoEstado | Visitados], RestoDoCaminho).

    % --- 6. O PONTO DE ENTRADA (MODIFICADO) ---

    % Predicado para ler todos os termos do stdin e "assertá-los"
    % (ESTA PARTE NÃO MUDA)
    read_map :-
        read(Term),         % Lê um termo
        process_term(Term).

    process_term(end_of_file) :- !. % Para quando a entrada (stdin) fechar
    process_term(Term) :-
        assertz(Term),      % Adiciona o fato
        read_map.           % Continua lendo

    % Predicado principal que o Rust irá chamar.
    % (ESTA PARTE É A QUE MUDA)
    run_solver :-
        read_map,           % 1. Lê o mapa do stdin

        % 2. A MUDANÇA:
        %    Usa findall/3 para coletar TODAS as soluções de 'resolver(Caminho)'
        %    em uma única lista chamada 'TodasSolucoes'.
        findall(Caminho, resolver(Caminho), TodasSolucoes),

        % 3. Verifica o resultado
        (   TodasSolucoes = []  % Se a lista de soluções estiver vazia
        ->
            write('no_solution_found'), % Imprime a falha
            nl
        ;
            % Se houver soluções, imprime a lista inteira de soluções
            write(TodasSolucoes),
            nl
        ),

        halt.               % 4. Termina o processo Prolog

    % Define 'run_solver' como o "main" do programa.
    % (ESTA PARTE NÃO MUDA)
    :- initialization(run_solver, main).
