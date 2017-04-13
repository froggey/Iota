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

/* Being a null driver, there's no event stream. We just define stubs for
   most of the API. */

#include "SDL.h"
#include "../../events/SDL_sysevents.h"
#include "../../events/SDL_events_c.h"

#include "SDL_iotavideo.h"
#include "SDL_iotaevents_c.h"

enum iota_event_type {
    ievt_quit = 0,
    ievt_key_down = 1,
    ievt_key_up = 2,
    ievt_mouse_motion = 3,
    ievt_mouse_button_down = 4,
    ievt_mouse_button_up = 5,
    ievt_active_event = 6,
};

struct iota_event {
    enum iota_event_type type;
    int data1;
    int data2;
    int data3;
};

extern int _iota_poll_event(struct iota_event *evt);

void IOTA_PumpEvents(_THIS)
{
    struct iota_event evt;
    while(_iota_poll_event(&evt)) {
        switch(evt.type) {
        case ievt_quit:
            SDL_PrivateQuit();
            break;
        case ievt_key_down: {
            SDL_keysym keysym;
            keysym.scancode = evt.data1;
            keysym.mod = evt.data2;
            keysym.sym = evt.data3;
            keysym.unicode = 0;
            SDL_PrivateKeyboard(SDL_PRESSED, &keysym);
            break;
        }
        case ievt_key_up: {
            SDL_keysym keysym;
            keysym.scancode = evt.data1;
            keysym.mod = evt.data2;
            keysym.sym = evt.data3;
            keysym.unicode = 0;
            SDL_PrivateKeyboard(SDL_RELEASED, &keysym);
            break;
        }
        case ievt_mouse_motion: {
            SDL_PrivateMouseMotion(0, 1, evt.data1, evt.data2);
            break;
        }
        case ievt_mouse_button_down: {
            SDL_PrivateMouseButton(SDL_PRESSED, evt.data1, 0, 0);
            break;
        }
        case ievt_mouse_button_up: {
            SDL_PrivateMouseButton(SDL_RELEASED, evt.data1, 0, 0);
            break;
        }
        case ievt_active_event: {
            SDL_PrivateAppActive(evt.data1, evt.data2);
            break;
        }
        }
    }
}

void IOTA_InitOSKeymap(_THIS)
{
	/* do nothing. */
}

/* end of SDL_iotaevents.c ... */
