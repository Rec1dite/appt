A simple telnet server for managing appointments

### Steps:
```bash
# Compile
cabal build
```

```bash
# Run
cabal run
```

```bash
# Connect
telnet localhost 4000
```

### Commands:
```
help         - show this message
search <arg> - search appointments
list         - list all appointments
clear        - clear the screen
add <args>   - add a new appointment
remove <n>   - remove appoint with id n
```