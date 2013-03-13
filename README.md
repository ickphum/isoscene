isoscene
========

IsoScene - Isometric drawing program

Features
--------
 * Easily create isometric perspective drawings by painting left, top or right sides of square bricks depicted on a 3-axis grid.
 * Each side maintains individual color selection.
 * Colors can be selected from a 24 bit color wheel, offering RGB or HSV settings.
 * Specify one side's color then generate matching shades for the other sides.
 * Select a color from existing sides or from anywhere on your screen (via the color selection dialog).
 * Zoom and pan around an infinite drawing surface.
 * Paint individual bricks or drag out areas on any axis.
 * Cut, copy and paste selected sides.
 * Selection (and deletion) can affect one side, two sides or three sides.
 * Clipboard retains history of all items copied to it, and these can be selected from a list and pasted again.
 * Clipboard items from other scenes can also be selected from a list and pasted, after which they also appear in the normal clipboard history.
 * Full undo/redo capability.
 * Files are auto-saved periodically and on close with no need for user intervention. 
 * Clipboard list and undo/redo lists are saved in full (so you can always go back from an auto-saved position).
 * Scenes can be exported to PNG image files.
 * Import image files onto any of the 3 planes in the grid.
 * Scene appearance and program preferences are configurable from within the program.
 * Interface is simple; buttons use left click or hold (for extended options), mouse wheel for scrolling and nothing else.

Video script
 * Draw a 4x4 cube. Start first side manually, then switch to area.
 * Darken & shade.
 * Add a 1 brick floor to the cube in the darker shade.
 * Erase 2x2 on roof and paint a depression.
 * Change any side to grey, do shade and add stairs and a 3 brick foundation.
 * Select all bricks, taking care to get all triangles.
 * Copy to clipboard, then clear selection.
 * Sample darker tower color
 * Darken, shade and paint a wall (12 bricks visible on top) to the left. Match the top of the roof depression and the foundation (7 bricks).
 * Paste the tower at the end of the wall.
 * Fix the wall joint; use zoom, move.
 * Switch to area select and cut a 5 brick gate out of the wall (4 either side). Fix the gate interior.
 * Save drawing as Bridge and start a new drawing.
 * Draw grass, banks and a stream; the gap at the top of the bank should match the bridge.
 * Select all and copy.
 * Save as Grass then switch back to Bridge.
 * Open paste dialog, select grass scene and paste under bridge.
 * Switch back to Grass and back again to show changes to Bridge were retained.
 * Demonstrate undo to show persistence over reload.
 * Start new scene, change to red palette.
 * Draw 3x3 cube and save as RedCube
 * Export to 3 pixels/tile, matching tile edge colour.
 * Switch to Bridge and import RedCube.png to left side behind bridge.
 * Draw white background and black frame around cube.
