Design
===

Layers
---
The functionaly of Mu works in layers. These are:

### Fix layer
Adjusts default buffers to fit screen size.

### Input layer
Blocks execution and receives input from the user, which is 
inserted into the editor state.

### State layer
Changes the state of the editor according to the current state
and user input.

### Display layer
Renders the editor buffers in the terminal
