# ----------------------------
# Makefile Options
# ----------------------------

NAME = DEMO
ICON = icon.png
DESCRIPTION = "CE NES Test Demo"
COMPRESSED = YES
ARCHIVED = NO

BSSHEAP_LOW = D031F6
BSSHEAP_HIGH = D177B6

CFLAGS = -Wall -Wextra -Oz
CXXFLAGS = -Wall -Wextra -Oz

# ----------------------------

include $(shell cedev-config --makefile)
