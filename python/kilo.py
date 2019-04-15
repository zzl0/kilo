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

import logging
logging.basicConfig(filename='kilo.log',level=logging.DEBUG)

KILO_VERSION = '0.0.1'

SEQ_2_KEY = {
    b'[1~': Key.HOME,
    b'[4~': Key.END,
    b'[3~': Key.DEL,
    b'[5~': Key.PAGE_UP,
    b'[6~': Key.PAGE_DOWN,
    b'[7~': Key.HOME,
    b'[8~': Key.END,
    b'[A': Key.ARROW_UP,
    b'[B': Key.ARROW_DOWN,
    b'[C': Key.ARROW_RIGHT,
    b'[D': Key.ARROW_LEFT,
    b'[H': Key.HOME,
    b'[F': Key.END,
}


class Key(Enum):
    ESC = 27
    ARROW_LEFT = 1000
    ARROW_RIGHT = auto()
    ARROW_UP = auto()
    ARROW_DOWN = auto()
    DEL = auto()
    HOME = auto()
    END = auto()
    PAGE_UP = auto()
    PAGE_DOWN = auto()

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
    new_attr[tty.CC][termios.VMIN] = 0
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
            break

    d = ord(c)
    if d == Key.ESC.value:
        seq = os.read(fd, 3)
        if len(seq) < 2:
            return Key.ESC
        if seq in SEQ_2_KEY:
            return SEQ_2_KEY[seq]
        return Key.ESC
    else:
        return d

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

class Row():
    KILO_TAB_STOP = 8

    def __init__(self, chars):
        self.chars = chars
        self.render = self.gen_render_chars(chars)

    def gen_render_chars(self, chars):
        return chars.replace('\t', ' ' * self.KILO_TAB_STOP)

    @property
    def size(self):
        return len(self.chars)

    @property
    def rsize(self):
        return len(self.render)


class Editor():
    def __init__(self):
        self.in_fd = sys.stdin.fileno()
        self.out_fd = sys.stdout.fileno()

        self.cx = 0
        self.cy = 0
        self.rx = 0
        self.screenrows, self.screencols = get_window_size(self.out_fd)
        self.rowoff = 0
        self.coloff = 0
        self.numrows = 0
        self.rows = []

    def open(self, filename):
        with open(filename) as f:
            for line in f:
                line = line.rstrip('\r\n')
                self.append_row(line)

    def append_row(self, s):
        self.rows.append(Row(s))
        self.numrows += 1

    ## output

    def scroll(self):
        if self.cy < self.rowoff:
            self.rowoff = self.cy
        elif self.cy >= self.rowoff + self.screenrows:
            self.rowoff = self.cy - self.screenrows + 1

        if self.cx < self.coloff:
            self.coloff = self.cx
        elif self.cx >= self.coloff + self.screencols:
            self.coloff = self.cx - self.screencols + 1

    def draw_rows(self, ab):
        for y in range(self.screenrows):
            filerow = y + self.rowoff
            if filerow >= self.numrows:
                if self.numrows == 0 and y == self.screenrows // 3:
                    welcome_msg = f'Kilo editor -- version {KILO_VERSION}'.encode()
                    welcome_len = min(len(welcome_msg), self.screencols)
                    padding = (self.screencols - welcome_len) // 2
                    if padding:
                        ab.append(b'~' + b' ' * padding)
                    ab.append(welcome_msg[:welcome_len])
                else:
                    ab.append(b'~')
            else:
                ab.append(self.rows[filerow].render[self.coloff:])

            # erases the part of the line to the right of the cursor
            ab.append(b'\x1b[K')
            if y < self.screenrows - 1:
                ab.append(b'\r\n')

    def refresh_screen(self):
        self.scroll()
        ab = AppendBuffer()

        # hide cursor
        ab.append(b'\x1b[?25l')
        # reposition the cursor
        ab.append(b'\x1b[H')

        self.draw_rows(ab)

        ab.append(f'\x1b[{self.cy - self.rowoff + 1};{self.cx - self.coloff + 1}H')
        # show cursor
        ab.append(b'\x1b[?25h')

        os.write(self.out_fd, ab.buf)

    ## input

    def process_keypress(self):
        k = read_key(self.in_fd)

        if k == ctrl('q'):
            os.write(self.out_fd, b'\x1b[2J')
            os.write(self.out_fd, b'\x1b[H')
            sys.exit(0)
        elif k == Key.HOME:
            self.cx = 0
        elif k == Key.END:
            if self.cy < self.numrows:
                self.cx = self.rows[self.cy].size
        elif k in (Key.PAGE_DOWN, Key.PAGE_UP):
            for _ in range(self.screenrows):
                arrow_key = Key.ARROW_UP if k == Key.PAGE_UP else Key.ARROW_DOWN
                self.move_cursor(arrow_key)
        elif k in (Key.ARROW_DOWN, Key.ARROW_LEFT, Key.ARROW_RIGHT, Key.ARROW_UP):
            self.move_cursor(k)

    def move_cursor(self, key):
        if key == Key.ARROW_LEFT:
            if self.cx != 0:
                self.cx -= 1
            elif self.cy > 0:
                self.cy -= 1
                self.cx = self.rows[self.cy].size
        elif key == Key.ARROW_RIGHT:
            try:
                if self.cx < self.rows[self.cy].size:
                    self.cx += 1
                elif self.cx == self.rows[self.cy].size:
                    self.cy += 1
                    self.cx = 0
            except IndexError:
                pass
        elif key == key.ARROW_UP:
            if self.cy != 0:
                self.cy -= 1
        elif key == Key.ARROW_DOWN:
            if self.cy < self.numrows:
                self.cy += 1

        try:
            row = self.rows[self.cy]
            self.cx = min(self.cx, row.size)
        except IndexError:
            self.cx = 0

    ## run

    def run(self, filename):
        self.open(filename)

        while True:
            self.refresh_screen()
            self.process_keypress()


### utils

def ctrl(c):
    return ord(c) & 0x1f


if __name__ == '__main__':
    with raw_mode(sys.stdin.fileno()):
        editor = Editor()
        editor.run(sys.argv[1])
