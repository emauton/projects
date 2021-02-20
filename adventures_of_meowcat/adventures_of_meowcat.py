'''These are the adventures of Meowcat!'''

import collections
import os
import sys
from dataclasses import dataclass
from typing import List, Any
from termcolor import colored


@dataclass
class Exit:
    desc: str
    room: Any
    tags: List[str]


@dataclass
class Item:
    desc: str
    damage: int
    heal: int
    tags: List[str]

@dataclass
class Mob:
    desc: str
    damage: int
    health: int
    aggro: bool
    tags: List[str]


@dataclass
class Room:
    tag: str
    desc: str
    exits: List[Exit]
    items: List[Item]
    mobs: List[Mob]

    def remove_item(self, item):
        self.items = [i for i in self.items if i != item]

    def remove_mob(self, mob):
        self.mobs = [m for m in self.mobs if m != mob]

    def aggro(self, game):
        for m in self.mobs:
            if m.aggro:
                print(f"{m.desc} is aggravated! It attacks you!")
                game.player.health -= m.damage
                print(f"Your health: {game.player.health}")

    def print(self):
        print()
        print(self.desc)

        if self.exits:
            print()
            for exit in self.exits:
                print(exit.desc, exit.tags)

        if self.items or self.mobs:
            print()
            print('In the room you see:')
            for item in self.items:
                print(item.desc, item.tags)
            print()
            for mob in self.mobs:
                print(mob.desc, mob.tags)
        print()


@dataclass
class Player:
    desc: str
    weapon: Item
    health: int

    def use_item(self, item):
        self.weapon = item


@dataclass
class Game:
    all_rooms: List[Room]
    room: Room
    player: Player

    def go_to_tag(self, tag):
        for r in self.all_rooms:
            if r.tag == tag:
                self.room = r


def initialize_game():
    dummy_room = Room('dummy', 'Room under construction', [], [], [])
    room1 = Room('start', 'You are in a dark room.',
             [],
             [Item('A burning torch', 2, 0, ['torch'])],
             [Mob(colored('A flying turtle!', 'green'), 2, 4, False, ['turtle'])])
    room2 = Room('mirror', 'You are in a large empty room with a golden mirror.',
             [],
             [Item('A mirror', 0, 0, ['mirror'])],
             [])
    room3 = Room('statue', 'You are in a large hallway.',
            [],
            [],
            [Mob(colored('A terrifying gargoyle!', 'cyan'), 2, 4, True, ['gargoyle', 'statue'])])
    room1.exits = [Exit('There is a large pit to the right', room2, ['pit', 'right'])]
    room2.exits = [Exit('Climb out of the pit', room1, ['climb', 'up'])]

    player = Player('You are Meowcat! So stronk.', None, 20)

    return Game([dummy_room, room1, room2, room3], room1, player)


def print_welcome_message():
    print(colored('Greetings, ', 'magenta'),
          colored('Meowcat!', 'magenta', 'on_green'))


def print_goodbye_message():
    print(colored('Goodbye, ', 'magenta'),
          colored('Meowcat!', 'magenta', 'on_green'))


def print_prompt():
    print(colored('What do you want to do? ', 'red', 'on_cyan'))


def get_input():
    print_prompt()
    return input()


class ParseError(Exception):
    pass


def parse_command(command):
    words = command.split()

    if len(words) == 1:
        verb = words[0]
        obj = None
    elif len(words) == 2:
        verb, obj = words
    else:
        raise ParseError

    return verb, obj


def search_and_exec(game, obj, things, fn):
    found = False
    for thing in things:
        if obj in thing.tags:
            fn(game, thing)
            found = True
            break
    if not found:
        print(f"You don't see {obj} here.")


def command_go(game, obj):
    def go_fn(game, exit):
        print(f'You go towards the {obj}.')
        game.room = exit.room

    search_and_exec(game, obj, game.room.exits, go_fn)


def command_take(game, obj):
    def take_fn(game, item):
        print(f'You take and wield the {obj}.')
        game.player.use_item(item)
        game.room.remove_item(item)
    search_and_exec(game, obj, game.room.items, take_fn)


def command_catch(game, obj):
    def catch_fn(game, mob):
        print(f"You catch the {obj}! It bites your finger and escapes!")

    search_and_exec(game, obj, game.room.mobs, catch_fn)


def command_hit(game, obj):
    def hit_fn(game, mob):
        if not game.player.weapon:
            print(f"You've got nothing to hit {obj} with!")
            return
        if not mob.aggro:
            print(f"You've just made an enemy! It hits you back!")
            mob.aggro = True
        mob.health -= game.player.weapon.damage

        if mob.health <= 0:
            print(colored(f"You have eliminated {obj}!", 'red'))
            print(f"Your health: {game.player.health}.")
            game.room.remove_mob(mob)
        else:
            print(f"Your health: {game.player.health}; {obj}'s health: {mob.health}")

    search_and_exec(game, obj, game.room.mobs, hit_fn)


def command_use(game, obj):
    def use_fn(game, item):
        if item.desc == 'A mirror':
            print("You see a", colored("purple mist", 'magenta'), "enclose the room!")
            print("You are teleported!")
            game.go_to_tag('statue')
            return
        print(f"You can't use {obj}.")
    search_and_exec(game, obj, game.room.items, use_fn)


def command_quit(game, obj):
    print_goodbye_message()
    sys.exit()


def process_command(game, verb, obj):
    command_table = {
            'go': command_go,
            'take': command_take,
            'catch': command_catch,
            'hit': command_hit,
            'use': command_use,
            'quit': command_quit
            }

    if verb in command_table:
        command_table[verb](game, obj)
    else:
        print(f"I don't understand {verb}!")


def main():
    global torch

    game = initialize_game()
    os.system('clear')
    print_welcome_message()

    while True:
        if game.player.health <= 0:
            print(colored("You are knocked out!", "red"))
            print("You awake blearily ...")
            game = initialize_game()

        game.room.print()
        game.room.aggro(game)
        command = get_input()

        try:
            verb, obj = parse_command(command)
            process_command(game, verb, obj)
        except Exception:
            print("I don't get it! Use '<verb> <object>' to help me understand.")


main()
