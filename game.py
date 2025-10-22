from dataclasses import dataclass
from typing import final
import pygame
import os
import logic
from logic import Cell, Action, Grid

SCREEN_WIDTH = 600
HUD_HEIGHT = 100

GREY = (30, 30, 30)
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
FLOOR_COLOR = (210, 180, 140)
WALL_COLOR = (139, 69, 19)
BOX_COLOR = (160, 82, 45)

ASSETS_PATH = "assets"


@dataclass
class Pos:
    x: int
    y: int


@final
class GameUI:
    def __init__(self):
        pygame.init()
        pygame.display.set_caption("RustMice")

        self.restart()

    def restart(self):
        self.grid = logic.generate_grid()
        self.solution = logic.get_solution(self.grid)
        logic.print_grid(self.grid)
        logic.print_solution(self.solution)
        self.side = len(self.grid)
        self.cell_size = SCREEN_WIDTH // self.side

        screen_height = SCREEN_WIDTH + HUD_HEIGHT

        self.screen = pygame.display.set_mode(
            (SCREEN_WIDTH, screen_height), pygame.NOFRAME
        )
        self.clock = pygame.time.Clock()
        self.font = pygame.font.Font(None, 36)
        self.large_font = pygame.font.Font(None, 90)
        self.images = self.load_images()

        self.step_index = 0
        self.rat_pos = Pos(0, 0)
        self.running = True

    def load_images(self):
        images: dict[str, pygame.Surface] = {}

        sprite_map = {
            "box": "box.png",
            "box_rat": "box_rat.png",
            "cat": "cat.png",
            "cat_angry": "cat_angry.png",
            "cat_attacking": "cat_attacking.png",
            "cheese": "cheese.png",
            "cheese_rat": "cheese_rat.png",
            "floor": "floor.png",
            "rat": "rat.png",
            "water": "water.png",
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
        for y, row in enumerate(self.grid):
            for x, cell in enumerate(row):
                rect = pygame.Rect(
                    x * self.cell_size,
                    y * self.cell_size,
                    self.cell_size,
                    self.cell_size,
                )

                key_to_draw = "floor"

                if (x, y) == (self.rat_pos.x, self.rat_pos.y):
                    match cell:
                        case Cell.BOX:
                            key_to_draw = "box_rat"
                        case Cell.CHEESE:
                            key_to_draw = "cheese_rat"
                        case _:
                            key_to_draw = "rat"
                else:
                    match cell:
                        case Cell.CAT:
                            match self.step_index % 3:
                                case 0:
                                    key_to_draw = "cat"
                                case 1:
                                    key_to_draw = "cat_angry"
                                case 2:
                                    key_to_draw = "cat_attacking"
                                case _:
                                    pass
                        case Cell.BOX:
                            key_to_draw = "box"
                        case Cell.WATER:
                            key_to_draw = "water"
                        case Cell.CHEESE:
                            key_to_draw = "cheese"
                        case _:
                            pass

                self.screen.blit(self.images[key_to_draw], rect)

    def draw_hud(self):
        hud_y_start = SCREEN_WIDTH

        hud_rect = pygame.Rect(0, hud_y_start, SCREEN_WIDTH, HUD_HEIGHT)
        pygame.draw.rect(self.screen, GREY, hud_rect)

        if self.solution is not None:
            step_text = f"passo: {self.step_index}/{len(self.solution)}"

            step_render = self.font.render(step_text, True, WHITE)

            self.screen.blit(step_render, (20, hud_y_start + 55))

            help_text_1 = "espaço: próximo passo"
            help_text_2 = "m: novo mapa"
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
            no_sol_text = "nenhuma solução encontrada"

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
                    self.restart()
                if event.key == pygame.K_SPACE:
                    self.update_rat_pos()

    def update_rat_pos(self):
        if self.solution is None:
            return

        self.step_index = (self.step_index + 1) % (len(self.solution) + 1)

        if self.step_index == 0:
            self.rat_pos = Pos(0, 0)
        else:
            action = self.solution[self.step_index - 1]

            match action:
                case Action.UP:
                    self.rat_pos.y -= 1
                case Action.DOWN:
                    self.rat_pos.y += 1
                case Action.LEFT:
                    self.rat_pos.x -= 1
                case Action.RIGHT:
                    self.rat_pos.x += 1

    def run(self):
        while self.running:
            self.handle_events()

            self.screen.fill(BLACK)
            self.draw_grid()
            self.draw_hud()

            pygame.display.flip()
            self.clock.tick(30)

        pygame.quit()
