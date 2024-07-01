# HiveMap

HiveMap is a sprite mappings editor for Sega Mega Drive games. It takes a PNG spritesheet as its input and is used to create multipiece sprite mappings. Mappings are then stored in an INI file, which can be fed into HiveMapExport to generate Mega Drive compatible graphics and mappings in whatever format the user specified.

![HiveMap](HiveMap.png)

## Usage

* Load either an INI or PNG file. If it's a fresh PNG, a new INI file will be automatically generated.
* Right click on the PNG workspace to create a new sprite box centered on the pointer. This can be resized and should contain the whole sprite.
* Right click on the sprite box to create a new piece. This should be fully within the sprite box or it will not be included. Use the menu on the right side of the program to resize the piece. It can be 8, 16, 24 or 32 pixels in width/height. Try to use as few pieces as possible per sprite, while balancing the number of 8x8 tiles used by the sprite. _Sonic the Hedgehog_ for example is limited to 23 tiles for the Sonic character sprite.
* When a piece is selected, you can change its palette line and priority with the piece menu.
* The palette is generated automatically when the PNG is first loaded by reading pixels left to right, top to bottom. For this reason it is advisible to manually draw the palette on the top-left of the PNG before loading it in HiveMap. You can change individual colours in the palette later on by clicking on the palette menu while nothing is selected, and then clicking on a pixel in the image with the crosshair.
* Zoom in/out with the mousewheel or the zoom dropdown menu.

## INI format

* `image=` Path to PNG file.
* `palette=` RGB values separated by commas.
* `sprite=` Sprite name, x position, y position, width, height.
* `piece=` Piece x position, y position, palette line/priority bitfield, size.
* `grid=` Grid setting (default 40).

* `mapasmfile=` Path to mappings file.
* `dplcasmfile=` Path to DPLC file.
* `gfxasmfile=` Path to graphics list file.
* `gfxfile=` Path to graphics binary file(s).
* `gfxline=` Repeating line in graphics list file.
* `mapindexhead=` First line of mappings index (usually a label).
* `mapindexline=` Repeating line of mappings index.
* `mapindexfoot=` Last line of mappings index.
* `maphead=` First line of mappings entry (usually a label and header).
* `mapline=` Repeating line of mappings entry (piece).
** `{xpos}` x position.
** `{ypos}` y position.
** `{size}` piece size (0-15).
** `{width}` piece width in tiles (1-4).
** `{height}` piece height in tiles (1-4).
** `{xpos}` x position.
** `{offsetlocal}` graphics offset within current sprite.
** `{offsetglobal}` graphics offset within all graphics.
** `{pal}` palette line (replaced with `pal#str`).
** `{priority}` piece priority (replaced with `histr=` or `lowstr=`).
* `mapfoot=` Last line of mappings entry.
* `dplcindexhead=` First line of DPLC index (usually a label).
* `dplcindexline=` Repeating line of DPLC index.
* `dplcindexfoot=` Last line of DPLC index.
* `dplchead=` First line of DPLC entry (usually a label).
* `dplcline=` Repeating line of DPLC entry.
* `dplcfoot=` Last line of DPLC entry.
* `dplc=` "yes" for standard DPLCs; "inline" for DPLCs inline with mappings.
* `pal1str=` String to insert for palette line 1.
* `pal2str=` String to insert for palette line 2.
* `pal3str=` String to insert for palette line 3.
* `pal4str=` String to insert for palette line 4.
* `histr=` String to insert for high priority piece.
* `lowstr=` String to insert for low priority piece.