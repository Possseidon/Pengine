# MCDataPackEditor
A tool for creating and managing datapacks for Minecraft (Java-Edition).

The latest release can be found [here](https://github.com/Possseidon/Pengine/releases/tag/MCDPE-0.1.3).

## General Features
### Workspace
* Upon opening a datapack, a tree of all files will show up on the left.
* Following features are existent:
  * Manually refreshing everything, if something changed. 
  * Adding, removing or renaming folders or files. Removing will give you a recycle bin prompt.
  * Copying file paths or just the namespace path (like you would use them in say, the `/function` command)
  * Opening a tab to edit one or all selected files.
    * Doing this on folders will open all files recusivly.

## Function-Editor
The Function-Editor has various useful features, such as code highlighting, suggestions or a code formatter.

It follows a list of all currently implemented and working features:
### Code Highlighting 
* Not yet customizable from within the editor and the colors are currently mostly randomly generated on startup.
### Suggestions and Auto-Completion (Ctrl-Space)
* Not just for simple command literals, but for a lot of other context dependent things.
### Parameter View (Shift-Ctrl-Space)
* Shows the type of each command parameter and highlights the one, you are currently at.
### Inline Parameter Hint
* Shows the next required parameter as a faint text directly at the end of the command.
### Error Checking
* Errors are constantly checked and displayed at the bottom.
* You can double click one of the errors to jump to the exact position.
* In the code they are underlined with dotted lines and have a small icon on the gutter.
* There are multiple error levels, which are:
  * Hint: Just something small to note
  * Warning: Will run but is probably not making sense/not what you intended
  * Error: Cannot run, but can still be formatted
  * Fatal: Can neither be run nor formatted
### Function Parsing
* The parser supports everything a minecraft function (as of version 1.13.1) allows.
### Formatter
* Format your whole code by simply typing "Ctrl-D" on your keyboard.
* This doesn't just remove spaces, but completly recreates the parsed commands from scratch with a (not yet customizable) formatter.

## LootTable-Editor
WIP

## Recipe-Editor
WIP

## Advancement-Editor
WIP

## Tag-Editor
WIP

## Structure-Viewer
WIP
