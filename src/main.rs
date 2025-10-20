use macroquad::prelude::*; // Ainda precisamos disso para is_key_pressed, KeyCode, etc.
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};
use std::thread;

// Declara o módulo 'ui' (procura o arquivo src/ui.rs)
mod ui;
// Importa as funções públicas do nosso módulo ui
use ui::{draw_frame, TILE_SIZE};

// --- Estruturas de Dados (agora públicas) ---

// A grid estática (o que não muda)
pub struct Grid {
    pub cells: Vec<Vec<char>>,
    pub width: usize,
    pub height: usize,
}

// Enum para representar as ações do Prolog
#[derive(Debug, Clone, PartialEq)]
pub enum Action {
    Cima,
    Baixo,
    Esquerda,
    Direita,
}

// Estado dinâmico (o que muda)
pub struct GameState {
    pub player_pos: (usize, usize), // (linha, coluna)
    pub all_paths: Vec<Vec<Action>>, // <-- NOVO: Armazena todas as soluções
    pub current_path_idx: usize, // <-- NOVO: Índice da solução atual (ex: 0, 1, ...)
    pub path_step_idx: usize,    // <-- RENOMEADO: (era path_index) Qual passo do caminho atual
}

// --- Funções de Lógica (Rust <-> Prolog) ---
// (Estas funções permanecem IDÊNTICAS)

fn format_prolog_input(grid_cells: &Vec<Vec<char>>, start_pos: (usize, usize)) -> String {
    let mut prolog_facts = String::new();

    for (r, row) in grid_cells.iter().enumerate() {
        let cells_str: Vec<String> = row.iter().map(|c| c.to_string()).collect();
        let prolog_list = cells_str.join(", ");
        prolog_facts.push_str(&format!("mapa({}, [{}]).\n", r, prolog_list));
    }

    let (r, c) = start_pos;
    prolog_facts.push_str(&format!("estado_inicial(pos({}, {})).\n", r, c));

    prolog_facts
}

fn parse_prolog_output(output: &str) -> Result<Vec<Vec<Action>>, &'static str> {
    let trimmed_output = output.trim();

    if trimmed_output.contains("no_solution_found") || trimmed_output == "[]" {
        return Err("Prolog não encontrou solução");
    }

    // Caso especial: solução é não fazer nada (começa no 'o')
    if trimmed_output == "[[]]" {
        return Ok(vec![vec![]]); // Uma solução de 0 passos
    }

    // Remove o '[' e ']' externos da lista de listas
    // Ex: "[[cima, d], [baixo, e]]" -> "[cima, d], [baixo, e]"
    let list_of_paths_str = trimmed_output
        .trim_start_matches('[')
        .trim_end_matches(']')
        .trim();

    let mut all_paths = Vec::new();

    // Divide as listas individuais.
    // O delimitador é "],", o que nos dá substrings como:
    // 1. "[cima, direita"
    // 2. " [direita, cima]]"
    for path_str in list_of_paths_str.split("],") {
        // Limpa cada substring, removendo '[' ']' e espaços
        let clean_path_str = path_str.trim().trim_start_matches('[').trim_end_matches(']');

        if clean_path_str.is_empty() {
            // Se a string de caminho estiver vazia (ex: caso [[]]), é um caminho vazio
            all_paths.push(Vec::new());
            continue;
        }

        // Agora usamos a lógica de parser ANTIGA em cada substring limpa
        // Ex: "cima, direita"
        let actions: Result<Vec<Action>, _> = clean_path_str
            .split(',')
            .map(|s| {
                let action_str = s.trim();
                match action_str {
                    "cima" => Ok(Action::Cima),
                    "baixo" => Ok(Action::Baixo),
                    "esquerda" => Ok(Action::Esquerda),
                    "direita" => Ok(Action::Direita),
                    _ => Err("Ação desconhecida do Prolog"),
                }
            })
            .collect();

        match actions {
            Ok(path) => all_paths.push(path),
            Err(e) => return Err(e),
        }
    }

    if all_paths.is_empty() {
        Err("Falha ao parsear a saída do Prolog")
    } else {
        Ok(all_paths)
    }
}

fn get_path_from_prolog(
    grid_cells: &Vec<Vec<char>>,
    start_pos: (usize, usize),
) -> Result<Vec<Vec<Action>>, String> { // <-- Assinatura atualizada

    let prolog_input = format_prolog_input(grid_cells, start_pos);
    let prolog_command = "swipl";

    // --- INÍCIO DO CÓDIGO FALTANTE ---
    // (Estas linhas provavelmente foram apagadas)

    let mut child = Command::new(prolog_command)
        .args(["-q", "-s", "solver.pl"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Falha ao iniciar SWI-Prolog: {}. Ele está no PATH?", e))?;

    let mut child_stdin = child.stdin.take().expect("Falha ao pegar stdin");

    thread::spawn(move || {
        child_stdin
            .write_all(prolog_input.as_bytes())
            .expect("Falha ao escrever no stdin do Prolog");
    });

    let stdout = child.stdout.take().expect("Falha ao pegar stdout");

    // ESTA É A LINHA QUE CAUSA O ERRO E0425
    let reader = BufReader::new(stdout);

    // --- FIM DO CÓDIGO FALTANTE ---

    // O código continua daqui
    let line = match reader.lines().next() {
        Some(Ok(line)) => line,
        _ => return Err("Prolog não produziu nenhuma saída.".to_string()),
    };

    let output = child.wait_with_output().expect("Falha ao esperar pelo processo");
    if !output.status.success() {
        let error_msg = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Erro do Prolog: {}", error_msg));
    }

    // Chama o NOVO parser
    parse_prolog_output(&line).map_err(|e| e.to_string())
}

fn find_start_pos_and_clean_grid(cells: &mut Vec<Vec<char>>) -> (usize, usize) {
    for (r, row) in cells.iter_mut().enumerate() {
        for (c, cell) in row.iter_mut().enumerate() {
            if *cell == 'j' {
                let pos = (r, c);
                *cell = 'c'; // Substitui 'j' por 'c' (chão)
                return pos;
            }
        }
    }
    panic!("Erro: Mapa não contém um ponto de início 'j'!");
}

fn build_window_conf(grid: &Grid) -> Conf {
    Conf {
        window_title: "Rust + Prolog Solver".to_string(),
        // Usa o tamanho da grid!
        window_width: (grid.width as f32 * TILE_SIZE) as i32,
        window_height: (grid.height as f32 * TILE_SIZE) as i32,
        ..Default::default()
    }
}

async fn run_game(grid: Grid, all_paths: Vec<Vec<Action>>, start_pos: (usize, usize)) {
    // --- 1. SETUP (Estado inicial modificado) ---
    let mut state = GameState {
        player_pos: start_pos,
        all_paths, // Recebe todas as soluções
        current_path_idx: 0,
        path_step_idx: 0,
    };

    // --- 2. GAME LOOP (Roda continuamente) ---
    loop {
        // --- 2a. Update (Lógica Modificada) ---

        // Pega o caminho ATUAL
        let current_path = state.all_paths.get(state.current_path_idx);

        // Ação: Avançar 1 passo (ESPAÇO)
        if is_key_pressed(KeyCode::Space) {
            if let Some(path) = current_path {
                if state.path_step_idx < path.len() {
                    let action = &path[state.path_step_idx];

                    match action {
                        Action::Cima => state.player_pos.0 -= 1,
                        Action::Baixo => state.player_pos.0 += 1,
                        Action::Esquerda => state.player_pos.1 -= 1,
                        Action::Direita => state.player_pos.1 += 1,
                    }

                    state.path_step_idx += 1;
                }
            }
        }

        // NOVO: Ação: Trocar de Solução (PONTO)
        if is_key_pressed(KeyCode::Period) {
            if !state.all_paths.is_empty() {
                // Avança o índice da solução, com wrap-around
                state.current_path_idx = (state.current_path_idx + 1) % state.all_paths.len();

                // RESETAR o estado
                state.player_pos = start_pos; // Volta ao início
                state.path_step_idx = 0;      // Volta ao passo 0
            }
        }

        // BÔNUS: Ação: Resetar (R)
        if is_key_pressed(KeyCode::R) {
            state.player_pos = start_pos;
            state.path_step_idx = 0;
        }

        // --- 2b. Draw (Gráficos) ---
        // A chamada é a mesma, mas ui.rs vai desenhar coisas novas
        draw_frame(&grid, &state);

        next_frame().await
    }
}

fn main() {
    // --- 1. SETUP (Roda uma vez) ---

    // Grid de exemplo com 2+ soluções
    let mut cells = vec![
        vec!['c', 'c', 'c', 'c', 'c'],
        vec!['c', 'p', 'c', 'p', 'p'],
        vec!['j', 'p', 'p', 'c', 'o'],
        vec!['c', 'c', 'c', 'c', 'c'],
    ];

    let start_pos = find_start_pos_and_clean_grid(&mut cells);

    let grid = Grid {
        width: cells[0].len(),
        height: cells.len(),
        cells,
    };

    // Agora esperamos Vec<Vec<Action>>
    let all_paths = match get_path_from_prolog(&grid.cells, start_pos) {
        Ok(paths) => {
            println!("Prolog encontrou {} caminhos:", paths.len());
            for (i, path) in paths.iter().enumerate() {
                println!("  {}: {:?}", i + 1, path);
            }
            paths
        }
        Err(e) => {
            eprintln!("Erro fatal: {}", e);
            panic!("{}", e);
        }
    };

    // --- 2. CONFIGURAÇÃO DA JANELA ---
    let config = build_window_conf(&grid);

    // --- 3. INICIA O JOGO ---
    // Passa 'all_paths' para o run_game
    macroquad::Window::from_config(config, run_game(grid, all_paths, start_pos));
}
