import pygame, sys
import pygame.font 

pygame.init()
size = (800, 600)
screen = pygame.display.set_mode(size)
pygame.display.set_caption('Meowcat')

def search_font(name):
    found_font = pygame.font.match_font(name)
    return found_font


def message_display(font, size, color, xy, message):
    font_object = pygame.font.Font(font, size)
    rendered_text = font_object.render(message, True, (color))
    screen.blit(rendered_text, xy)
    pygame.display.update()

art = pygame.image.load('test-art-clipped.png')
screen.blit(art, (350, 70))
pygame.display.flip()
font = search_font('PressStart2P')
message_display(font, 30, (255,255,255), (70,270), 'Adventures of Meowcat!')

while True:
    pass
