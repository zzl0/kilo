# -*- coding: utf-8 -*-
import sys
import tty
import curses.ascii
import termios
import contextlib


### terminal ###

@contextlib.contextmanager
def raw_mode():
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    tty.setraw(fd)
    yield None
    termios.tcsetattr(fd, termios.TCSAFLUSH, old_settings)


def main():
    while True:
        c = sys.stdin.read(1)
        if c == 'q':
            return
        elif curses.ascii.iscntrl(c):
            print(f'{ord(c)}')
        else:
            print(f"{ord(c)} ('{c}')", )


if __name__ == '__main__':
    with raw_mode() as _:
        main()
