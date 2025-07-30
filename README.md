# Sokoban RL Playground

A Sokoban game environment implemented in OCaml for reinforcement learning experiments, featuring tabular Q-learning and REINFORCE scaffolding with curriculum learning.

## Features

- **Core Sokoban Game Engine**: Complete implementation of Sokoban game mechanics
- **Tabular Q-Learning**: Fully functional tabular Q-learning agent
- **REINFORCE Scaffolding**: Structure for policy gradient methods (requires DL framework integration)
- **Curriculum Learning**: Progressive difficulty system starting from simple corridors to complex puzzles
- **Text-based Visualization**: Simple terminal-based game rendering

## Building and Running

```bash
# Build the project
dune build

# Run the demo
dune exec sokoban-rl-playground
```

## Project Structure

- `lib/sokoban.ml` - Core game logic and state representation
- `lib/visualization.ml` - Terminal-based visualization
- `lib/tabular_rl.ml` - Tabular Q-learning implementation
- `lib/reinforce.ml` - REINFORCE algorithm scaffolding
- `lib/curriculum.ml` - Curriculum learning system
- `bin/main.ml` - Demo program showcasing all features

## Game Symbols

- `#` - Wall
- `@` - Player
- `$` - Box
- `.` - Target
- `*` - Box on target
- `+` - Player on target

## Curriculum Learning Stages

1. **Corridor**: Narrow passages of increasing length
2. **Room**: Square rooms of increasing size
3. **Multi-box**: Levels with multiple boxes
4. **Complex**: Challenging predefined puzzles

## Future Work

To complete the REINFORCE implementation, integrate a deep learning framework such as:
- Owl (OCaml numerical computing library)
- OCaml-TensorFlow bindings
- Custom neural network implementation
