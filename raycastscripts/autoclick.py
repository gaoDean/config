#!/usr/bin/env python3

from pynput import keyboard as pykeyboard
from pynput import mouse as pymouse
import threading
import time

keyboard = pykeyboard.Controller()
mouse = pymouse.Controller()

running = False

def press_left_click():
    mouse.press(pymouse.Button.left)
    time.sleep(0.01)
    mouse.release(pymouse.Button.left)

def press_key(key):
    keyboard.press(key)
    time.sleep(0.01)
    keyboard.release(key)

def left_button():
    while running:
        for i in range(1,10):
            press_left_click()
            time.sleep(0.3)
        press_key('t')
        time.sleep(0.5)
        press_left_click()
        time.sleep(0.1)
        press_key(pykeyboard.Key.right)
        time.sleep(0.5)

def right_button():
    while running:
        time.sleep(60*9)
        press_key(pykeyboard.Key.right)
        time.sleep(0.3)
        mouse.press(pymouse.Button.right)
        time.sleep(2)
        mouse.release(pymouse.Button.right)

def on_press(key):
    try:
        global running # to assing value to global variable (instead of local variable)

        if key == pykeyboard.Key.esc:
            return False
        if key.char == 'n':
            if not running:  # to run only one `process`
                running = True
                print("running")
                threading.Thread(target=left_button).start()
                threading.Thread(target=right_button).start()
        if key.char == 'e' and running:
            running = False
            print("not running")
    except AttributeError:
        print('special key {0} pressed'.format(
            key))

print("Ready")
# Collect events until released
with pykeyboard.Listener(on_press=on_press) as listener:listener.join()
