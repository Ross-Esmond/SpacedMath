from sympy import *
from sympy.abc import x
from random import *
from inspect import *
from queue import *

def polynomial(degree, terms):
    assert terms <= (1 + degree)
    poly = randrange(1, 10)*x**degree
    degree -= 1
    terms -= 1
    while terms > 0:
        if degree == 0:
            poly = poly + randrange(1,10)
            terms -= 1
        elif randrange(degree + 1) < terms:
            poly = poly + randrange(1, 10)*x**degree
            terms -= 1
        degree -= 1
    return poly

def slotCount(const):
    if const == None:
        return 1
    count = 1
    for a in const['args']:
        count += slotCount(a)
    return count

def insertArg(const, arg, num = None):
    if num == None:
        num = randrange(0, len(const['args']))
    if const['args'][num] != None:
        insertArg(arg, const['args'][num])
    const['args'][num] = arg

def setSlot(const, slot, add):
    queue = Queue()
    queue.put(const)
    if slot == 0:
        insertArg(add, const)
        return add
    at = 0
    while not queue.empty():
        item = queue.get()
        at += 1
        if item == None:
            continue
        if 'args' in item:
            for a in item['args']:
                queue.put(a)
        if at <= slot and slot < at + len(item['args']):
            insertArg(add, item['args'][slot - at])
            item['args'][slot - at] = add
            return const

def fill(const, allowed):
    if const == None:
        return {
          'name': 'poly',
          'degree': randrange(1, 10) if allowed['power'] else 1,
          'terms': randrange(1, 3) if allowed['addition'] else 1
        }
    elif const['name'] == 'mult' or const['name'] == 'div':
        for ind, arg in enumerate(const['args']):
            if arg != None and (arg['name'] == 'mult' or arg['name'] == 'div'):
                insertArg(const, {
                    'name': 'add',
                    'args': [None, None]
                }, ind)
        if const['name'] == 'mult':
            for ind, arg in enumerate(const['args']):
                if arg == None:
                    const['args'][ind] = {
                        'name': 'poly',
                        'degree': randrange(2, 10),
                        'terms': randrange(2, 3)
                    }
        elif const['name'] == 'div':
            deg = randrange(2, 10)
            if const['args'][0] == None:
                const['args'][0] = {
                    'name': 'poly',
                    'degree': randrange(1, deg) if allowed['power'] else 1,
                    'terms': randrange(1, 2) if allowed['addition'] else 1
                } 
            if const['args'][1] == None:
                const['args'][1] = {
                    'name': 'poly',
                    'degree': randrange(deg, 10) if allowed['power'] else 1,
                    'terms': randrange(2, 3) if allowed['addition'] else 1
                } 
    elif const['name'] == 'add':
        for ind, arg in enumerate(const['args']):
            if arg == None:
                const['args'][ind] = {
                    'name': 'poly',
                    'terms': 1,
                    'degree': randrange(1, 10)
                }
    if 'args' in const:
        for arg in const['args']:
            fill(arg, allowed)

def convert(const):
    if const['name'] == 'mult':
        return convert(const['args'][0])*convert(const['args'][1])
    if const['name'] == 'div':
        return convert(const['args'][0])/convert(const['args'][1])
    if const['name'] == 'add':
        return convert(const['args'][0])+convert(const['args'][1])
    if const['name'] == 'poly':
        return polynomial(const['degree'], const['terms'])

def problem(required, allowed):
    what = None
    for name in required:
        what = setSlot(what, randrange(slotCount(what)), { 'name': name, 'args': [None, None] })
    fill(what, allowed)
    return latex(convert(what))

