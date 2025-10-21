import pygame
import os

from prolog import generate_grid

SCREEN_WIDTH = 600
HUD_HEIGHT = 100

GREY = (30, 30, 30)
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
FLOOR_COLOR = (210, 180, 140)
WALL_COLOR = (139, 69, 19)
BOX_COLOR = (160, 82, 45)

SYMBOLS = {
    "floor": " ",
    "wall": "⬛",
    "mouse": "🐭",
    "cheese": "🧀",
    "cat": "🐈",
    "box": "📦",
    "rat": "🐀",
}

ASSETS_PATH = "assets"


class GameUI:
    def __init__(self):
        pygame.init()
        pygame.display.set_caption("RustMice")

        self.start()
        self.reset_state()

    def start(self):
        grid, solutions = generate_grid()

        self.grid = grid
        self.solutions = solutions
        self.side = len(grid)

        self.cell_size = SCREEN_WIDTH // self.side

        screen_height = SCREEN_WIDTH + HUD_HEIGHT
        self.screen = pygame.display.set_mode(
            (SCREEN_WIDTH, screen_height), pygame.NOFRAME
        )

        self.clock = pygame.time.Clock()
        self.font = pygame.font.Font(None, 36)
        self.large_font = pygame.font.Font(None, 90)

        self.mouse_start_pos = self.find_start_pos()
        self.images = self.load_images()

        self.has_solutions = bool(self.solutions)
        self.reset_state()

    def find_start_pos(self):
        for r, row in enumerate(self.grid):
            for c, cell in enumerate(row):
                if cell == "mouse":
                    return (r, c)
        return (0, 0)

    def reset_state(self):
        """Reseta o estado para o início da solução atual."""
        self.current_solution_index = 0
        self.current_step_index = 0
        self.mouse_pos = self.mouse_start_pos
        self.running = True

    def load_images(self):
        images = {}

        sprite_map = {
            "rat": "Rat.png",
            "cheese": "Cheese.png",
            "cat": "Cat.png",
            "box": "Box.png",
            "wall": "Wall.png",
            "floor": "Floor.png",
            "AngryCat": "AngryCat.png",
            "BoxRat": "BoxRat.png",
            "CheeseRat": "CheeseRat.png",
        }

        for key, filename in sprite_map.items():
            file_path = os.path.join(ASSETS_PATH, filename)

            try:
                image = pygame.image.load(file_path)

                scaled_image = pygame.transform.scale(
                    image, (self.cell_size, self.cell_size)
                )

                images[key] = scaled_image.convert_alpha()

            except Exception as e:
                print(f"AVISO: Falha ao carregar sprite: '{file_path}'")
                print(f"Erro: {e}")

        return images

    def draw_grid(self):
        for r, row in enumerate(self.grid):
            for c, cell_type in enumerate(row):
                rect = pygame.Rect(
                    c * self.cell_size,
                    r * self.cell_size,
                    self.cell_size,
                    self.cell_size,
                )

                key_to_draw = None
                color_fallback = None

                if cell_type == "mouse":
                    cell_type = "floor"

                if cell_type == "cat":
                    if self.current_step_index > 0 and self.current_step_index % 3 == 2:
                        key_to_draw = "AngryCat"
                    else:
                        key_to_draw = "cat"

                elif cell_type == "box":
                    key_to_draw = "box"
                    color_fallback = BOX_COLOR

                elif cell_type == "wall":
                    key_to_draw = "wall"
                    color_fallback = WALL_COLOR

                elif cell_type == "cheese":
                    key_to_draw = "cheese"
                    color_fallback = FLOOR_COLOR

                else:
                    key_to_draw = "floor"
                    color_fallback = FLOOR_COLOR

                if key_to_draw in self.images:
                    self.screen.blit(self.images[key_to_draw], rect)
                else:
                    if color_fallback:
                        pygame.draw.rect(self.screen, color_fallback, rect)
                    else:
                        pygame.draw.rect(self.screen, FLOOR_COLOR, rect)

    def draw_mouse(self):
        r, c = self.mouse_pos
        rect = pygame.Rect(
            c * self.cell_size, r * self.cell_size, self.cell_size, self.cell_size
        )

        original_cell_type = self.grid[r][c]

        key_to_draw = "rat"

        if original_cell_type == "box":
            key_to_draw = "BoxRat"
        elif original_cell_type == "cheese":
            key_to_draw = "CheeseRat"

        if key_to_draw in self.images:
            self.screen.blit(self.images[key_to_draw], rect)
        else:
            print(
                f"AVISO: Sprite '{key_to_draw}' não encontrado, usando fallback de emoji."
            )
            emoji_font = pygame.font.Font(None, int(self.cell_size * 0.8))

            emoji_to_use = SYMBOLS["mouse"]
            if key_to_draw == "rat":
                emoji_to_use = SYMBOLS.get("rat", SYMBOLS["mouse"])
            elif key_to_draw == "cheese":
                emoji_to_use = SYMBOLS.get("cheese", SYMBOLS["mouse"])

            img = emoji_font.render(emoji_to_use, True, BLACK)
            img_rect = img.get_rect(center=rect.center)

            if original_cell_type == "box":
                pygame.draw.rect(self.screen, BOX_COLOR, rect)
            elif original_cell_type == "cheese":
                pygame.draw.rect(self.screen, FLOOR_COLOR, rect)
            else:
                pygame.draw.rect(self.screen, FLOOR_COLOR, rect)
            self.screen.blit(img, img_rect)

    def draw_hud(self):
        hud_y_start = SCREEN_WIDTH

        hud_rect = pygame.Rect(0, hud_y_start, SCREEN_WIDTH, HUD_HEIGHT)
        pygame.draw.rect(self.screen, GREY, hud_rect)

        if self.has_solutions:
            sol_text = (
                f"Solução: {self.current_solution_index + 1}/{len(self.solutions)}"
            )

            steps_in_sol = 0
            if self.current_solution_index < len(self.solutions):
                steps_in_sol = len(self.solutions[self.current_solution_index])
            step_text = f"Passo: {self.current_step_index}/{steps_in_sol}"

            sol_render = self.font.render(sol_text, True, WHITE)
            step_render = self.font.render(step_text, True, WHITE)

            self.screen.blit(sol_render, (20, hud_y_start + 20))
            self.screen.blit(step_render, (20, hud_y_start + 55))

            help_text_1 = "ESPAÇO: Próximo Passo"
            help_text_2 = "N: Próxima Solução"
            help_render_1 = self.font.render(help_text_1, True, WHITE)
            help_render_2 = self.font.render(help_text_2, True, WHITE)
            self.screen.blit(
                help_render_1,
                (SCREEN_WIDTH - help_render_1.get_width() - 20, hud_y_start + 20),
            )
            self.screen.blit(
                help_render_2,
                (SCREEN_WIDTH - help_render_2.get_width() - 20, hud_y_start + 55),
            )
        else:
            no_sol_text = "NENHUMA SOLUÇÃO ENCONTRADA"

            no_sol_render = self.font.render(no_sol_text, True, (255, 100, 100))
            no_sol_rect = no_sol_render.get_rect(
                center=(SCREEN_WIDTH // 2, hud_y_start + 45)
            )
            self.screen.blit(no_sol_render, no_sol_rect)

    def handle_events(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                self.running = False
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    self.running = False
                if event.key == pygame.K_m:
                    self.start()

                if self.has_solutions:
                    if event.key == pygame.K_SPACE:
                        self.advance_step()
                    if event.key == pygame.K_n:
                        self.next_solution()

    def advance_step(self):
        if not self.has_solutions:
            return

        current_solution = self.solutions[self.current_solution_index]
        if self.current_step_index < len(current_solution):
            action = current_solution[self.current_step_index]
            r, c = self.mouse_pos
            if action == "up":
                self.mouse_pos = (r - 1, c)
            elif action == "down":
                self.mouse_pos = (r + 1, c)
            elif action == "left":
                self.mouse_pos = (r, c - 1)
            elif action == "right":
                self.mouse_pos = (r, c + 1)

            self.current_step_index += 1  # O Timer avança

    def next_solution(self):
        if not self.has_solutions:
            return

        self.current_solution_index = (self.current_solution_index + 1) % len(
            self.solutions
        )
        self.current_step_index = 0
        self.mouse_pos = self.mouse_start_pos

    def run(self):
        while self.running:
            self.handle_events()

            self.screen.fill(BLACK)
            self.draw_grid()
            self.draw_mouse()
            self.draw_hud()

            pygame.display.flip()
            self.clock.tick(30)

        pygame.quit()


def run():
    game = GameUI()
    game.run()
