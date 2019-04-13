# -*- coding: utf-8 -*-
import os
import sys
import tty
import copy
import fcntl
import struct
from curses.ascii import iscntrl
from enum import Enum, auto
import termios
import contextlib

KILO_VERSION = '0.0.1'

class Key(Enum):
    ARROW_LEFT = 1000
    ARROW_RIGHT = auto()
    ARROW_UP = auto()
    ARROW_DOWN = auto()

### terminal

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
        yield
    finally:
        termios.tcsetattr(fd, termios.TCSAFLUSH, old_attr)


def get_window_size(fd):
    return struct.unpack('hh', fcntl.ioctl(fd, termios.TIOCGWINSZ, '1234'))


def read_key(fd):
    while True:
        c = os.read(fd, 1)
        if c:
            return ord(c)

### append buffer

class AppendBuffer():
    def __init__(self):
        self.buf = b''

    def append(self, s):
        if isinstance(s, str):
            self.buf += s.encode()
        else:
            self.buf += s

### editor

class Editor():
    def __init__(self):
        self.in_fd = sys.stdin.fileno()
        self.out_fd = sys.stdout.fileno()

        self.cx = 0
        self.cy = 0
        self.screenrows, self.screencols = get_window_size(self.out_fd)

    ## output

    def draw_rows(self, ab):
        for y in range(self.screenrows):
            if y == self.screenrows // 3:
                welcome_msg = f'Kilo editor -- version {KILO_VERSION}'.encode()
                welcome_len = min(len(welcome_msg), self.screencols)
                padding = (self.screencols - welcome_len) // 2
                if padding:
                    ab.append(b'~' + b' ' * padding)
                ab.append(welcome_msg[:welcome_len])
            else:
                ab.append(b'~')
            # erases the part of the line to the right of the cursor
            ab.append(b'\x1b[K')
            if y < self.screenrows - 1:
                ab.append(b'\r\n')

    def refresh_screen(self):
        ab = AppendBuffer()

        # hide cursor
        ab.append(b'\x1b[?25l')
        # reposition the cursor
        ab.append(b'\x1b[H')

        self.draw_rows(ab)

        ab.append(f'\x1b[{self.cy + 1};{self.cx + 1}H')
        # show cursor
        ab.append(b'\x1b[?25h')

        os.write(self.out_fd, ab.buf)

    ## input

    def process_keypress(self):
        c = read_key(self.in_fd)

        if c == ctrl('q'):
            os.write(self.out_fd, b'\x1b[2J')
            os.write(self.out_fd, b'\x1b[H')
            sys.exit(0)
        elif c in map(ord, 'wsad'):
            self.move_cursor(c)

    def move_cursor(self, key):
        if key == ord('a'):
            self.cx -= 1
        elif key == ord('d'):
            self.cx += 1
        elif key == ord('w'):
            self.cy -= 1
        elif key == ord('s'):
            self.cy += 1

    ## run

    def run(self):
        while True:
            self.refresh_screen()
            self.process_keypress()


### utils

def ctrl(c):
    return ord(c) & 0x1f


if __name__ == '__main__':
    with raw_mode(sys.stdin.fileno()):
        editor = Editor()
        editor.run()
