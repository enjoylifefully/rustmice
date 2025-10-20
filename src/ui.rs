use macroquad::prelude::*;
use crate::{Grid, GameState}; // Importa as structs de main.rs (que é o 'crate')

// Constante de renderização
pub const TILE_SIZE: f32 = 64.0;

// --- Funções de Desenho Privadas (só este módulo vê) ---

// Desenha a grid estática
fn draw_grid(grid: &Grid) {
    for (r, row) in grid.cells.iter().enumerate() {
        for (c, cell) in row.iter().enumerate() {
            let color = match cell {
                'p' => DARKGRAY, // Parede
                'o' => GOLD,     // Objetivo
                'c' => GRAY,     // Chão
                // Note que 'j' não é mais necessário, pois virou 'c'
                _ => BLACK,
            };
            draw_rectangle(
                c as f32 * TILE_SIZE,
                r as f32 * TILE_SIZE,
                TILE_SIZE,
                TILE_SIZE,
                color,
            );
        }
    }
}

// Desenha o jogador
fn draw_player(state: &GameState) {
    draw_rectangle(
        state.player_pos.1 as f32 * TILE_SIZE, // Posição X (coluna)
        state.player_pos.0 as f32 * TILE_SIZE, // Posição Y (linha)
        TILE_SIZE,
        TILE_SIZE,
        BLUE, // Cor do jogador
    );
}

// --- Função de Desenho Pública (o main.rs vai chamar esta) ---

/// Desenha um frame completo do jogo (grid, jogador, e texto de UI)
pub fn draw_frame(grid: &Grid, state: &GameState) {
    clear_background(BLACK);

    draw_grid(grid);
    draw_player(state);

    // --- UI de Texto Modificada ---
    let y_base = grid.height as f32 * TILE_SIZE;
    let font_size = 24.0;

    // Texto de ajuda (Espaço)
    draw_text(
        "ESPAÇO: Próximo passo",
        10.0,
        y_base - 40.0, // Posição Y (mais pra cima)
        font_size,
        WHITE,
    );

    // Texto de ajuda (Ponto e R)
    draw_text(
        "'.': Próx. solução | 'R': Resetar",
        10.0,
        y_base - 10.0, // Posição Y (no fundo)
        font_size,
        WHITE,
    );

    // NOVO: Contador de Solução
    let sol_text = if state.all_paths.is_empty() {
        "Solução: 0 / 0".to_string()
    } else {
        // Mostra índice + 1 (para ser 1-based)
        format!("Solução: {} / {}", state.current_path_idx + 1, state.all_paths.len())
    };

    // Alinha o texto à direita
    let text_dims = measure_text(&sol_text, None, font_size as u16, 1.0);
    let x_pos_sol = (grid.width as f32 * TILE_SIZE) - text_dims.width - 10.0;

    draw_text(
        &sol_text,
        x_pos_sol,     // Canto direito
        y_base - 10.0, // Posição Y (no fundo)
        font_size,
        WHITE,
    );
}
