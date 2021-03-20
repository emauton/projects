'''These are the adventures of Meowcat!'''

import collections
import os
import random
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
    damage: int  # Maximum damage a weapon can do
    heal: int
    defense: int
    tags: List[str]
    takeable: bool

@dataclass
class Mob:
    desc: str
    damage: int  # Maximum damage this mob can do
    health: int
    aggro: bool
    tags: List[str]
    drop: List[Item]

def try_hit(a, b):
    '''a is something with a "damage" field
       b is something witha "health" field and maybe armour'''

    defense = 0
    if hasattr('b', 'armour'):
        defense = b.armour.defense

    coin = random.randint(0, 1)

    if coin:
        damage = random.randint(1, a.damage) - defense
        if damage <= 0:
            print(f"{b.desc}'s armour", colored('deflects', 'blue'), "a hit!")
        else:
            print(f"{b.desc} is hit for", colored(f'{damage}', 'red'), "!")
            b.health -= damage
    else:
        print(f"{a.desc} attempts to hit but misses {b.desc}!")

@dataclass
class Room:
    tag: str
    desc: str
    exits: List[Exit]
    items: List[Item]
    mobs: List[Mob]

    def remove_item(self, item):
        self.items = [i for i in self.items if i != item]

    def add_item(self, item):
        self.items.append(item)

    def remove_mob(self, mob):
        self.mobs = [m for m in self.mobs if m != mob]

    def aggro(self, game):
        for m in self.mobs:
            if m.aggro:
                print(f"{m.desc} is aggravated! It attacks you!")
                try_hit(m, game.player)

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
    armour: Item
    health: int
    inventory: List[Item]

    def wield_item(self, item):
        self.weapon = item

    def wear_item(self, item):
        self.armour = item

    def add_inventory(self, item):
        self.inventory.append(item)

    def remove_inventory(self, item):
        self.inventory = [i for i in self.inventory if i != item]


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
             [Item('A burning torch', 2, 0, 0, ['torch'], True)],
             [Mob(colored('A flying turtle!', 'green'), 2, 4, False, ['turtle'], None)])
    room2 = Room('mirror', 'You are in a large empty room with a golden mirror.',
             [],
             [Item('A mirror', 0, 0, 0, ['mirror'], False)],
             [])
    room3 = Room('hallway', 'You are in a large hallway.',
            [],
            [],
            [Mob(colored('A terrifying gargoyle!', 'cyan'), 2, 4, True,
                ['gargoyle', 'statue'],
                [Item('Cat fud', 0, 5, 0, ['food'], True)])])
    room4 = Room('ghost', ('You are in a brightly-lit room, with light pouring in from a hole in the ceiling. '
                           'It is covered in moss.'),
            [],
            [Item('A rusty sword', 3, 0, 0, ['sword'], True)],
            [Mob(colored('A ghoulish ghost!', 'cyan'), 3, 3, True, ['ghost', 'ghoul'], None)])
    room5 = Room('executioner', ('This is a massive room. In the centre there is a chopping block. '
                                 'It is covered in grisly blood!'),
            [],
            [],
            [Mob(colored('A giant axe-wielding rat named THE EXECUTIONER!', 'red'), 4, 10, True, ['executioner', 'rat'],
                 [Item('A bloody axe', 4, 0, 0, ['axe'], True),
                  Item('Chainmail', 0, 0, 2, ['chainmail'], True),
                  Item('Cheese', 0, 5, 0, ['cheese'], True),
                  Item('A cyan portal', 0, 0, 0, ['portal'], False)])])
    room6 = Room('rubble', 'This room has been destroyed! There is rubble everywhere and blood on the walls.',
            [],
            [],
            [Mob(colored('The wretch', 'green'), 3, 5, True, ['wretch', 'zombie'],
                 [Item('Mystery meat', 0, 6, 0, ['meat'], True)])])
    room1.exits = [Exit('There is a large pit to the right', room2, ['pit', 'right'])]
    room2.exits = [Exit('Climb out of the pit', room1, ['climb', 'up'])]
    room3.exits = [Exit('There is a door to the left', room4, ['left']),
                   Exit('There is a broken-open hole in the ceiling', room6, ['hole'])]
    room4.exits = [Exit('There is a large, scary gateway!', room5, ['gateway'])]

    player = Player('Meowcat', None, None, 20, [])

    return Game([dummy_room, room1, room2, room3, room4], room1, player)


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
    line = input()
    line.strip()
    return line


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
    return found


def command_go(game, obj):
    def go_fn(game, exit):
        print(f'You go towards the {obj}.')

        if 'gateway' in exit.tags:
            print(colored('As you pass through the gateway, you feel stronger! You are healed!', 'green'))
            game.player.health = 20
        game.room = exit.room

    if not search_and_exec(game, obj, game.room.exits, go_fn):
        print(f"You don't see {obj} here.")



def command_take(game, obj):
    def take_fn(game, item):
        if not item.takeable:
            print(f"You can't take {obj}!")
            return

        print(f'You take the {obj}.')
        game.player.add_inventory(item)
        game.room.remove_item(item)
    if not search_and_exec(game, obj, game.room.items, take_fn):
        print(f"You don't see {obj} here.")


def command_status(game, obj):
    print(colored(f'Your health: {game.player.health}', 'magenta'))
    print(colored('Your inventory:', 'green'))
    if game.player.inventory:
        for item in game.player.inventory:
            print(f' * {item.desc}')
    else:
            print(" * You're not carrying anything!")
    if game.player.weapon:
        print(colored(f'\nYour weapon: {game.player.weapon.desc}', 'red'))
    if game.player.armour:
        print(colored(f'\nYour armour: {game.player.armour.desc}', 'blue'))


def command_catch(game, obj):
    def catch_fn(game, mob):
        print(f"You catch the {obj}! It bites your finger and escapes!")

    if not search_and_exec(game, obj, game.room.mobs, catch_fn):
        print(f"You don't see {obj} here.")

def command_hit(game, obj):
    def hit_fn(game, mob):
        if not game.player.weapon:
            print(f"You've got nothing to hit {obj} with!")
            return
        if not mob.aggro:
            print(f"You've just made an enemy! It hits you back!")
            mob.aggro = True
        try_hit(game.player.weapon, mob)

        if mob.health <= 0:
            print(colored(f"You have eliminated {obj}!", 'red'))
            print(f"Your health: {game.player.health}.")
            game.room.remove_mob(mob)
            if mob.drop:
                for i in mob.drop:
                    print(f"{obj} drops {i.desc}!")
                    game.room.add_item(i)
        else:
            print(f"Your health: {game.player.health}; {obj}'s health: {mob.health}")

    if not search_and_exec(game, obj, game.room.mobs, hit_fn):
        print(f"You don't see {obj} here.")


def command_use(game, obj):
    def use_fn(game, item):
        if 'mirror' in item.tags:
            print("You see a", colored("purple mist", 'magenta'), "enclose the room!")
            print("You are teleported!")
            game.go_to_tag('hallway')
            return

        if 'portal' in item.tags:
            print("You see a", colored("cyan flash", 'cyan'), "!")
            print("You are teleported!")
            game.go_to_tag('hallway')
            return

        if item.damage:
            weapon = game.player.weapon

            print(f'You wield the {obj}.')
            game.player.wield_item(item)
            game.player.remove_inventory(item)
            game.room.remove_item(item)

            if weapon:
                print(f'You put {weapon.desc} back in your pack.')
                game.player.add_inventory(weapon)
            return

        if item.defense:
            armour = game.player.armour

            print(f'You wear the {obj}.')
            game.player.wear_item(item)
            game.player.remove_inventory(item)
            game.room.remove_item(item)

            if armour:
                print(f'You put {armour.desc} back in your pack.')
                game.player.add_inventory(armour)
            return

        if item.heal:
            print(f'You use the {obj}. It heals you!')
            game.player.health += item.heal
            game.player.remove_inventory(item)
            game.room.remove_item(item)
            return

        print(f"You can't use {obj}.")

    if not search_and_exec(game, obj, game.player.inventory, use_fn):
        if not search_and_exec(game, obj, game.room.items, use_fn):
            print(f"You don't see {obj} here.")


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
            'status': command_status,
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

    last_command = None
    while True:
        if game.player.health <= 0:
            print(colored("You are knocked out!", "red"))
            print("You awake blearily ...")
            game = initialize_game()

        game.room.print()
        game.room.aggro(game)
        command = get_input()
        if not command:
            command = last_command
        else:
            last_command = command

        try:
            verb, obj = parse_command(command)
            process_command(game, verb, obj)
        except Exception:
            print("I don't get it! Use '<verb> <object>' to help me understand.")


main()
