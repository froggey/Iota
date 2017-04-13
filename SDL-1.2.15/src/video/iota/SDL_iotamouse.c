/*
    SDL - Simple DirectMedia Layer
    Copyright (C) 1997-2012 Sam Lantinga

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

    Sam Lantinga
    slouken@libsdl.org
*/
#include "SDL_config.h"

#include "SDL_mouse.h"
#include "../SDL_cursor_c.h"
#include "../../events/SDL_events_c.h"

#include "SDL_iotamouse_c.h"

/* The implementation dependent data for the window manager cursor */
struct WMcursor {
	int unused;
};

extern int _iota_grab_input(int mode);
extern void _iota_warp_cursor(int x, int y);
extern void _iota_show_cursor(int toggle);

SDL_GrabMode IOTA_GrabInput(_THIS, SDL_GrabMode mode) {
    int is_grabbed = _iota_grab_input(mode);
    if(is_grabbed) {
        return SDL_GRAB_ON;
    } else {
        return SDL_GRAB_OFF;
    }
}

void IOTA_WarpWMCursor(_THIS, Uint16 x, Uint16 y) {
    _iota_warp_cursor(x, y);
}

void IOTA_CheckMouseMode(_THIS) {
    /* If the mouse is hidden and input is grabbed, we use relative mode */
    if((SDL_cursorstate & CURSOR_VISIBLE)) {
        _iota_show_cursor(1);
    } else {
        _iota_show_cursor(0);
    }
}
