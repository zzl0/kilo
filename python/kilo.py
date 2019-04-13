# -*- coding: utf-8 -*-
import os
import sys
import tty
import copy
from curses.ascii import iscntrl
import termios
import contextlib

### terminal ###

@contextlib.contextmanager
def raw_mode(fd):
    old_attr = termios.tcgetattr(fd)

    new_attr = copy.deepcopy(old_attr)
    new_attr[tty.IFLAG] &= ~(termios.BRKINT | termios.ICRNL | termios.IXON
                             | termios.INPCK | termios.ISTRIP)
    new_attr[tty.OFLAG] &= ~(termios.OPOST)
    new_attr[tty.CFLAG] |= (termios.CS8)
    new_attr[tty.LFLAG] &= ~(termios.ECHO | termios.ICANON | termios.IEXTEN
                             | termios.ISIG)
    # set the number of characters read at a time in non-canonical mode.
    new_attr[tty.CC][termios.VMIN] = 1
    # set timeout for read
    new_attr[tty.CC][termios.VTIME] = 1

    termios.tcsetattr(fd, termios.TCSAFLUSH, new_attr)

    try:
        yield fd
    finally:
        termios.tcsetattr(fd, termios.TCSAFLUSH, old_attr)


def editor_read_key(fd):
    while True:
        c = os.read(fd, 1)
        if c:
            return ord(c)

### output

def editor_refresh_screen():
    # clear the entire screen
    os.write(sys.stdout.fileno(), b'\x1b[2J')
    # reposition the cursor
    os.write(sys.stdout.fileno(), b'\x1b[H')

### input

def editor_process_keypress(fd):
    c = editor_read_key(fd)

    if c == ctrl('q'):
        os.write(sys.stdout.fileno(), b'\x1b[2J')
        os.write(sys.stdout.fileno(), b'\x1b[H')
        sys.exit(0)
    elif iscntrl(c):
        print(f'{c}', end='\r\n')
    else:
        print(f"{c} ('{chr(c)}')", end='\r\n')

### utils

def ctrl(c):
    return ord(c) & 0x1f


def main(in_fd):
    while True:
        editor_refresh_screen()
        editor_process_keypress(in_fd)


if __name__ == '__main__':
    with raw_mode(sys.stdin.fileno()) as in_fd:
        main(in_fd)
