# -*- coding: utf-8 -*-
import sys
import tty
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
    tty.setraw(sys.stdin)

    while True:
        c = sys.stdin.read(1)
        if c == 'q':
            return


if __name__ == '__main__':
    with raw_mode() as _:
        main()
