import gleam/int
import gleam/list
import types.{
  type Creature, type CreatureType, type Position, type World, Creature,
  GameWorld, Position,
}
import world

// Get the movement pattern for a creature type
pub fn get_movement_speed(creature_type: CreatureType) -> Int {
  case creature_type {
    types.Plant -> 0
    types.Herbivore -> 1
    types.Carnivore -> 2
    types.Omnivore -> 1
  }
}

// Get how much energy a creature loses per turn
pub fn get_energy_consumption(creature_type: CreatureType) -> Int {
  case creature_type {
    types.Plant -> 0
    types.Herbivore -> 1
    types.Carnivore -> 2
    types.Omnivore -> 1
  }
}

// Get maximum energy for creature type
pub fn get_max_energy(creature_type: CreatureType) -> Int {
  case creature_type {
    types.Plant -> 100
    types.Herbivore -> 80
    types.Carnivore -> 120
    types.Omnivore -> 100
  }
}

// Check if a creature can hunt another creature
pub fn can_hunt(hunter: Creature, prey: Creature) -> Bool {
  case hunter {
    Creature(_, types.Carnivore, _, hunter_energy, _) -> {
      case prey {
        Creature(_, types.Herbivore, _, _, _) -> hunter_energy > 10
        Creature(_, types.Plant, _, _, _) -> False
        _ -> False
      }
    }
    Creature(_, types.Omnivore, _, omnivore_energy, _) -> {
      case prey {
        Creature(_, types.Plant, _, _, _) -> omnivore_energy > 5
        Creature(_, types.Herbivore, _, _, _) -> omnivore_energy > 15
        _ -> False
      }
    }
    _ -> False
  }
}

// Make a creature behave according to its type
pub fn creature_behavior(world: World, creature: Creature) -> World {
  case creature {
    Creature(_name, types.Plant, id, energy, _position) ->
      plant_behavior(world, id, energy)

    Creature(_name, types.Herbivore, id, _energy, position) ->
      herbivore_behavior(world, id, position)

    Creature(_name, types.Carnivore, id, _energy, position) ->
      carnivore_behavior(world, id, position)

    Creature(_name, types.Omnivore, id, _energy, position) ->
      omnivore_behavior(world, id, position)
  }
}

// Plant behavior: slowly regenerate energy 
fn plant_behavior(world: World, plant_id: Int, current_energy: Int) -> World {
  let max_energy = get_max_energy(types.Plant)
  case current_energy < max_energy {
    True -> {
      let new_energy = int.min(max_energy, current_energy + 2)
      world.update_creature_energy(world, plant_id, new_energy)
    }
    False -> world
  }
}

// Herbivore behavior: look for plants to eat
fn herbivore_behavior(
  world: World,
  herbivore_id: Int,
  position: Position,
) -> World {
  let nearby_plants = find_nearby_food(world, position, types.Plant)
  case nearby_plants {
    [plant, ..] -> {
      case plant {
        Creature(_, _, plant_id, plant_energy, _) -> {
          case plant_energy > 15 {
            True -> {
              let energy_eaten = 5
              let new_world =
                world.update_creature_energy(
                  world,
                  plant_id,
                  plant_energy - energy_eaten,
                )
              let herbivore_result =
                world.find_creature(new_world, herbivore_id)
              case herbivore_result {
                Ok(Creature(_, _, _, herbivore_energy, _)) ->
                  world.update_creature_energy(
                    new_world,
                    herbivore_id,
                    herbivore_energy + energy_eaten,
                  )
                Error(_) -> new_world
              }
            }
            False -> world
          }
        }
      }
    }
    [] -> world
  }
}

// Carnivore behavior: Hunt herbivores
fn carnivore_behavior(
  world: World,
  carnivore_id: Int,
  position: Position,
) -> World {
  let nearby_prey = find_nearby_food(world, position, types.Herbivore)
  case nearby_prey {
    [prey, ..] -> {
      case prey {
        Creature(_, _, prey_id, prey_energy, _) -> {
          case prey_energy < 30 {
            True -> {
              let energy_gained = 15
              let new_world = world.remove_creature(world, prey_id)
              let carnivore_result =
                world.find_creature(new_world, carnivore_id)
              case carnivore_result {
                Ok(Creature(_, _, _, carnivore_energy, _)) ->
                  world.update_creature_energy(
                    new_world,
                    carnivore_id,
                    carnivore_energy + energy_gained,
                  )
                Error(_) -> new_world
              }
            }
            False -> world
          }
        }
      }
    }
    [] -> world
  }
}

// Omnivore behavior: Flexible eating
fn omnivore_behavior(
  world: World,
  omnivore_id: Int,
  position: Position,
) -> World {
  let nearby_plants = find_nearby_food(world, position, types.Plant)
  case nearby_plants {
    [_plant, ..] -> herbivore_behavior(world, omnivore_id, position)
    [] -> {
      let nearby_prey = find_nearby_food(world, position, types.Herbivore)
      case nearby_prey {
        [_prey, ..] -> carnivore_behavior(world, omnivore_id, position)
        [] -> world
      }
    }
  }
}

// Find nearby creatures of a specific type
fn find_nearby_food(
  world: World,
  position: Position,
  food_type: CreatureType,
) -> List(Creature) {
  case world {
    GameWorld(_width, _height, creatures, _) -> {
      let Position(x, y) = position
      list.filter(creatures, fn(creature) {
        case creature {
          Creature(_, kind, _, _, creature_position) -> {
            let Position(cx, cy) = creature_position
            let distance =
              int.absolute_value(x - cx) + int.absolute_value(y - cy)
            kind == food_type && distance <= 2
          }
        }
      })
    }
  }
}
