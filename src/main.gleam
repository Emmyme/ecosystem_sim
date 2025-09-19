import creatures
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import input
import types.{type World, Creature, Position}
import world

pub fn main() -> Nil {
  io.println("Ecosystem Simulator")
  io.println("Type 'help' for available commands or 'quit' to exit.")
  io.println("")

  // Create initial world
  let initial_world = world.init_world(10, 10)

  // Print the initial grid
  print_world_grid(initial_world)

  // Start the interactive game loop
  interactive_game_loop(initial_world, 1)
}

// Interactive game loop
fn interactive_game_loop(current_world: World, next_id: Int) -> Nil {
  case input.read_line("\n> ") {
    Ok(user_input) -> {
      let command = parse_command(user_input)
      case command {
        Quit -> {
          io.println("Thanks for playing!")
          Nil
        }
        _ -> {
          let #(updated_world, updated_next_id) =
            execute_command(current_world, command, next_id)
          interactive_game_loop(updated_world, updated_next_id)
        }
      }
    }
    Error(error_msg) -> {
      io.println("Error reading input: " <> error_msg)
      io.println("Exiting...")
      Nil
    }
  }
}

// Handle add creature command
fn handle_add_command(
  world: World,
  name: String,
  creature_type: String,
  custom_energy: Int,
  id: Int,
) -> World {
  case creature_type {
    "herbivore" | "carnivore" | "omnivore" | "plant" -> {
      let #(creature_kind, default_energy) = case creature_type {
        "herbivore" -> #(types.Herbivore, 50)
        "carnivore" -> #(types.Carnivore, 40)
        "omnivore" -> #(types.Omnivore, 45)
        "plant" -> #(types.Plant, 30)
        _ -> #(types.Herbivore, 50)
        // Fallback
      }

      let energy = case custom_energy {
        0 -> default_energy
        _ -> custom_energy
      }

      let new_world =
        world.add_creature_random_position(
          world,
          name,
          creature_kind,
          id,
          energy,
        )
      io.println(
        "Added "
        <> name
        <> " ("
        <> creature_type
        <> ") with "
        <> int.to_string(energy)
        <> " energy!",
      )
      new_world
    }
    _ -> {
      io.println(
        "Invalid creature type! Use: herbivore, carnivore, omnivore, or plant",
      )
      world
    }
  }
}

// Handle feed creature command
fn handle_feed_command(
  world: World,
  creature_name: String,
  energy_amount: Int,
) -> World {
  let feed_amount = case energy_amount {
    0 -> 5
    _ -> energy_amount
  }

  case world.feed_creature_by_name(world, creature_name, feed_amount) {
    Ok(fed_world) -> {
      io.println(
        "Fed "
        <> creature_name
        <> " with "
        <> int.to_string(feed_amount)
        <> " energy!",
      )
      fed_world
    }
    Error(error_msg) -> {
      io.println(" x " <> error_msg)
      world
    }
  }
}

// Display world status
fn print_world_status(world: World) -> Nil {
  case world {
    types.GameWorld(_width, _height, creatures, _turn) -> {
      // List all creatures
      list.each(creatures, fn(creature) {
        case creature {
          Creature(name, kind, id, energy, Position(x, y)) -> {
            let type_emoji = case kind {
              types.Herbivore -> "🐰"
              types.Carnivore -> "🐺"
              types.Omnivore -> "🐻"
              types.Plant -> "🌱"
            }
            io.println(
              "  "
              <> type_emoji
              <> " "
              <> name
              <> " (ID:"
              <> int.to_string(id)
              <> ") Energy:"
              <> int.to_string(energy)
              <> " @("
              <> int.to_string(x)
              <> ","
              <> int.to_string(y)
              <> ")",
            )
          }
        }
      })
    }
  }
}

// Create a grid representation of the world
fn print_world_grid(world: World) -> Nil {
  case world {
    types.GameWorld(width, height, _creatures, _turn) -> {
      list.range(0, height - 1)
      |> list.each(fn(y) {
        list.range(0, width - 1)
        |> list.each(fn(x) {
          let current_pos = Position(x, y)
          let creatures_here = world.creatures_at_position(world, current_pos)

          case creatures_here {
            [] -> io.print(" . ")
            [creature, ..] -> {
              case creature {
                Creature(_, kind, _, _, _) -> {
                  let emoji = case kind {
                    types.Herbivore -> "🐰"
                    types.Carnivore -> "🐺"
                    types.Omnivore -> "🐻"
                    types.Plant -> "🌱"
                  }
                  io.print(" " <> emoji <> " ")
                }
              }
            }
          }
        })
        io.println("")
      })
      io.println("")
    }
  }
}

// Run one simulation turn
fn simulate_turn(world: World) -> World {
  world
  |> world.move_all_creatures_randomly
  |> world.process_interactions
  |> apply_creature_behaviors
  |> apply_energy_consumption
  |> world.remove_starved_creatures
}

// Apply creature behaviors
fn apply_creature_behaviors(world: World) -> World {
  case world {
    types.GameWorld(_, _, creatures, _) -> {
      list.fold(creatures, world, fn(current_world, creature) {
        creatures.creature_behavior(current_world, creature)
      })
    }
  }
}

// Apply energy consumption
fn apply_energy_consumption(world: World) -> World {
  case world {
    types.GameWorld(width, height, creature_list, turn) -> {
      let updated_creatures =
        list.map(creature_list, fn(creature) {
          case creature {
            Creature(name, kind, id, energy, position) -> {
              let consumption = creatures.get_energy_consumption(kind)
              let new_energy = int.max(0, energy - consumption)
              Creature(name, kind, id, new_energy, position)
            }
          }
        })
      types.GameWorld(width, height, updated_creatures, turn)
    }
  }
}

// Run simulation turns
fn simulate_turns(world: World, num_turns: Int) -> World {
  case num_turns <= 0 {
    True -> world
    False -> simulate_turns(simulate_turn(world), num_turns - 1)
  }
}

// Command data type for user input
pub type Command {
  AddCreature(name: String, creature_type: String, energy: Int)
  FeedCreature(name: String, energy: Int)
  Step
  Status
  Run(turns: Int)
  Help
  Quit
  Invalid(message: String)
}

// Parse user input into a command
fn parse_command(input: String) -> Command {
  let parts = string.split(string.trim(input), " ")
  case parts {
    ["add", name, creature_type] -> AddCreature(name, creature_type, 0)
    ["add", name, creature_type, energy_str] ->
      case int.parse(energy_str) {
        Ok(energy) -> AddCreature(name, creature_type, energy)
        Error(_) -> Invalid("Invalid energy amount: " <> energy_str)
      }
    ["feed", name] -> FeedCreature(name, 0)
    ["feed", name, energy_str] ->
      case int.parse(energy_str) {
        Ok(energy) -> FeedCreature(name, energy)
        Error(_) -> Invalid("Invalid energy amount: " <> energy_str)
      }
    ["step"] -> Step
    ["status"] -> Status
    ["run", turns_str] ->
      case int.parse(turns_str) {
        Ok(turns) -> Run(turns)
        Error(_) -> Invalid("Invalid number of turns: " <> turns_str)
      }
    ["help"] -> Help
    ["quit"] | ["exit"] -> Quit
    [] -> Invalid("Empty command")
    _ -> Invalid("Unknown command: " <> input)
  }
}

// Execute a command and return the updated world
fn execute_command(
  world: World,
  command: Command,
  next_id: Int,
) -> #(World, Int) {
  case command {
    AddCreature(name, creature_type, energy) -> {
      let new_world =
        handle_add_command(world, name, creature_type, energy, next_id)
      let simulated_world = simulate_turn(new_world)
      print_world_status(simulated_world)
      print_world_grid(simulated_world)
      #(simulated_world, next_id + 1)
    }
    FeedCreature(name, energy) -> {
      let new_world = handle_feed_command(world, name, energy)
      let simulated_world = simulate_turn(new_world)
      print_world_status(simulated_world)
      print_world_grid(simulated_world)
      #(simulated_world, next_id)
    }
    Step -> {
      let new_world = simulate_turn(world)
      print_world_status(new_world)
      print_world_grid(new_world)
      #(new_world, next_id)
    }
    Status -> {
      print_world_status(world)
      print_world_grid(world)
      #(world, next_id)
    }
    Run(turns) -> {
      let new_world = simulate_turns(world, turns)
      print_world_status(new_world)
      print_world_grid(new_world)
      #(new_world, next_id)
    }
    Help -> {
      print_help()
      #(world, next_id)
    }
    Quit -> {
      io.println("Thanks for playing!")
      #(world, next_id)
    }
    Invalid(message) -> {
      io.println("x " <> message)
      io.println("Type 'help' for available commands.")
      #(world, next_id)
    }
  }
}

fn print_help() -> Nil {
  io.println("\nAvailable Commands:")
  io.println(
    "  add <name> <type> [energy]  - Add a creature (types: herbivore, carnivore, omnivore, plant)",
  )
  io.println(
    "                              - Default energies: herbivore=50, carnivore=40, omnivore=45, plant=30",
  )
  io.println(
    "  feed <name> [energy]        - Feed a creature by name (default: 5 energy)",
  )
  io.println("  step                        - Advance simulation by one turn")
  io.println("  status                      - Show current world status")
  io.println("  run <n>                     - Run simulation for n turns")
  io.println("  help                        - Show this help message")
  io.println("  quit                        - Exit the simulator")
  io.println("")
  io.println(
    "Note: The simulation automatically advances one turn after add/feed commands!",
  )
}
