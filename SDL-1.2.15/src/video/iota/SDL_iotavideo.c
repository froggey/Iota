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

/* Dummy SDL video driver implementation; this is just enough to make an
 *  SDL-based application THINK it's got a working video driver, for
 *  applications that call SDL_Init(SDL_INIT_VIDEO) when they don't need it,
 *  and also for use as a collection of stubs when porting SDL to a new
 *  platform for which you haven't yet written a valid video driver.
 *
 * This is also a great way to determine bottlenecks: if you think that SDL
 *  is a performance problem for a given platform, enable this driver, and
 *  then see if your application runs faster without video overhead.
 *
 * Initial work by Ryan C. Gordon (icculus@icculus.org). A good portion
 *  of this was cut-and-pasted from Stephane Peter's work in the AAlib
 *  SDL video driver.  Renamed to "DUMMY" by Sam Lantinga.
 */

#include "SDL_video.h"
#include "SDL_mouse.h"
#include "../SDL_sysvideo.h"
#include "../SDL_pixels_c.h"
#include "../../events/SDL_events_c.h"

#include "SDL_iotavideo.h"
#include "SDL_iotaevents_c.h"
#include "SDL_iotamouse_c.h"

#define IOTAVID_DRIVER_NAME "iota"

/* Initialization/Query functions */
static int IOTA_VideoInit(_THIS, SDL_PixelFormat *vformat);
static SDL_Rect **IOTA_ListModes(_THIS, SDL_PixelFormat *format, Uint32 flags);
static SDL_Surface *IOTA_SetVideoMode(_THIS, SDL_Surface *current, int width, int height, int bpp, Uint32 flags);
static int IOTA_SetColors(_THIS, int firstcolor, int ncolors, SDL_Color *colors);
static void IOTA_VideoQuit(_THIS);

/* Hardware surface functions */
static int IOTA_AllocHWSurface(_THIS, SDL_Surface *surface);
static int IOTA_LockHWSurface(_THIS, SDL_Surface *surface);
static void IOTA_UnlockHWSurface(_THIS, SDL_Surface *surface);
static void IOTA_FreeHWSurface(_THIS, SDL_Surface *surface);

/* etc. */
static void IOTA_UpdateRects(_THIS, int numrects, SDL_Rect *rects);

extern void _iota_set_caption(const char *title, const char *icon);

static void IOTA_SetCaption(_THIS, const char *title, const char *icon)
{
    _iota_set_caption(title, icon);
}

/* IOTA driver bootstrap functions */

static int IOTA_Available(void)
{
	return(1);
}

static void IOTA_DeleteDevice(SDL_VideoDevice *device)
{
	SDL_free(device->hidden);
	SDL_free(device);
}

static SDL_VideoDevice *IOTA_CreateDevice(int devindex)
{
	SDL_VideoDevice *device;

	/* Initialize all variables that we clean on shutdown */
	device = (SDL_VideoDevice *)SDL_malloc(sizeof(SDL_VideoDevice));
	if ( device ) {
		SDL_memset(device, 0, (sizeof *device));
		device->hidden = (struct SDL_PrivateVideoData *)
				SDL_malloc((sizeof *device->hidden));
	}
	if ( (device == NULL) || (device->hidden == NULL) ) {
		SDL_OutOfMemory();
		if ( device ) {
			SDL_free(device);
		}
		return(0);
	}
	SDL_memset(device->hidden, 0, (sizeof *device->hidden));

	/* Set the function pointers */
	device->VideoInit = IOTA_VideoInit;
	device->ListModes = IOTA_ListModes;
	device->SetVideoMode = IOTA_SetVideoMode;
	device->CreateYUVOverlay = NULL;
	device->SetColors = IOTA_SetColors;
	device->UpdateRects = IOTA_UpdateRects;
	device->VideoQuit = IOTA_VideoQuit;
	device->AllocHWSurface = IOTA_AllocHWSurface;
	device->CheckHWBlit = NULL;
	device->FillHWRect = NULL;
	device->SetHWColorKey = NULL;
	device->SetHWAlpha = NULL;
	device->LockHWSurface = IOTA_LockHWSurface;
	device->UnlockHWSurface = IOTA_UnlockHWSurface;
	device->FlipHWSurface = NULL;
	device->FreeHWSurface = IOTA_FreeHWSurface;
	device->SetCaption = NULL;
	device->SetIcon = NULL;
	device->IconifyWindow = NULL;
	device->GrabInput = IOTA_GrabInput;
	device->GetWMInfo = NULL;
	device->InitOSKeymap = IOTA_InitOSKeymap;
	device->PumpEvents = IOTA_PumpEvents;
    device->WarpWMCursor = IOTA_WarpWMCursor;
    device->CheckMouseMode = IOTA_CheckMouseMode;
    device->SetCaption = IOTA_SetCaption;

	device->free = IOTA_DeleteDevice;

	return device;
}

VideoBootStrap IOTA_bootstrap = {
	IOTAVID_DRIVER_NAME, "SDL iota video driver",
	IOTA_Available, IOTA_CreateDevice
};

extern int _iota_video_init(void);
extern void _iota_video_quit(void);
extern int _iota_set_video_mode(int width, int height);
extern void _iota_video_update(void *pixbuf);

int IOTA_VideoInit(_THIS, SDL_PixelFormat *vformat)
{
	/*
	fprintf(stderr, "WARNING: You are using the SDL iota video driver!\n");
	*/

	/* Determine the screen depth (use default 8-bit depth) */
	/* we change this during the SDL_SetVideoMode implementation... */
	vformat->BitsPerPixel = 32;
	vformat->BytesPerPixel = 4;

	/* We're done! */
	return _iota_video_init();
}

SDL_Rect **IOTA_ListModes(_THIS, SDL_PixelFormat *format, Uint32 flags)
{
   	 return (SDL_Rect **) -1;
}

SDL_Surface *IOTA_SetVideoMode(_THIS, SDL_Surface *current,
				int width, int height, int bpp, Uint32 flags)
{
	if ( this->hidden->buffer ) {
		SDL_free( this->hidden->buffer );
	}

	this->hidden->buffer = SDL_malloc(width * height * 4);
	if ( ! this->hidden->buffer ) {
		SDL_SetError("Couldn't allocate buffer for requested mode");
		return(NULL);
	}

/* 	printf("Setting mode %dx%d\n", width, height); */

	SDL_memset(this->hidden->buffer, 0, width * height * 4);

    if(_iota_set_video_mode(width, height)) {
		SDL_free(this->hidden->buffer);
		this->hidden->buffer = NULL;
		SDL_SetError("Call to set_video_mode failed");
		return(NULL);
	}

	/* Set up the new mode framebuffer */
	current->flags = flags & SDL_FULLSCREEN;
	this->hidden->w = current->w = width;
	this->hidden->h = current->h = height;
	current->pitch = current->w * 4;
	current->pixels = this->hidden->buffer;

	/* We're done */
	return(current);
}

/* We don't actually allow hardware surfaces other than the main one */
static int IOTA_AllocHWSurface(_THIS, SDL_Surface *surface)
{
	return(-1);
}
static void IOTA_FreeHWSurface(_THIS, SDL_Surface *surface)
{
	return;
}

/* We need to wait for vertical retrace on page flipped displays */
static int IOTA_LockHWSurface(_THIS, SDL_Surface *surface)
{
	return(0);
}

static void IOTA_UnlockHWSurface(_THIS, SDL_Surface *surface)
{
	return;
}

static void IOTA_UpdateRects(_THIS, int numrects, SDL_Rect *rects)
{
	_iota_video_update(this->hidden->buffer);
}

int IOTA_SetColors(_THIS, int firstcolor, int ncolors, SDL_Color *colors)
{
	/* do nothing of note. */
	return(1);
}

/* Note:  If we are terminated, this could be called in the middle of
   another SDL video routine -- notably UpdateRects.
*/
void IOTA_VideoQuit(_THIS)
{
	if (this->screen->pixels != NULL)
	{
		SDL_free(this->screen->pixels);
		this->screen->pixels = NULL;
	}
    _iota_video_quit();
}
