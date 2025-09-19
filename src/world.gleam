import gleam/int
import gleam/list
import types.{
  type Creature, type Position, type World, Creature, GameWorld, Position,
}
import utils

pub fn init_world(width: Int, height: Int) -> World {
  GameWorld(width: width, height: height, creatures: [], turn: 0)
}

pub fn add_creature(world: World, creature: Creature) -> World {
  case world {
    GameWorld(width, height, creatures, turn) ->
      GameWorld(
        width: width,
        height: height,
        creatures: [creature, ..creatures],
        turn: turn,
      )
  }
}

// Add a creature at a random position
pub fn add_creature_random_position(
  world: World,
  name: String,
  kind: types.CreatureType,
  id: Int,
  energy: Int,
) -> World {
  let random_position = utils.random_position(world)
  let new_creature = Creature(name, kind, id, energy, random_position)
  add_creature(world, new_creature)
}

pub fn move_creature(
  world: World,
  creature_id: Int,
  new_position: Position,
) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let updated_creatures =
        list.map(creatures, fn(creature) {
          case creature {
            Creature(name, kind, id, energy, _position) ->
              case id == creature_id {
                True -> Creature(name, kind, id, energy, new_position)
                False -> creature
              }
          }
        })
      GameWorld(width, height, updated_creatures, turn)
    }
  }
}

// Move a creature to a new position
pub fn move_creature_randomly(world: World, creature_id: Int) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let updated_creatures =
        list.map(creatures, fn(creature) {
          case creature {
            Creature(name, kind, id, energy, position) ->
              case id == creature_id {
                True -> {
                  let Position(x, y) = position

                  let dx = utils.random_in_range(-1, 1)
                  let dy = utils.random_in_range(-1, 1)

                  let new_x = utils.clamp(x + dx, 0, width - 1)
                  let new_y = utils.clamp(y + dy, 0, height - 1)
                  Creature(name, kind, id, energy, Position(new_x, new_y))
                }
                False -> creature
              }
          }
        })
      GameWorld(width, height, updated_creatures, turn)
    }
  }
}

// Move all creatures randomly in one turn
pub fn move_all_creatures_randomly(world: World) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let updated_creatures =
        list.map(creatures, fn(creature) {
          let Creature(name, kind, id, energy, position) = creature
          let Position(x, y) = position

          case kind {
            types.Plant -> creature
            _ -> {
              let dx = int.random(5) - 2

              let dy = int.random(5) - 2

              let new_x = utils.clamp(x + dx, 0, width - 1)
              let new_y = utils.clamp(y + dy, 0, height - 1)
              Creature(name, kind, id, energy, Position(new_x, new_y))
            }
          }
        })
      GameWorld(width, height, updated_creatures, turn + 1)
    }
  }
}

// Find a creature by ID
pub fn find_creature(world: World, creature_id: Int) -> Result(Creature, Nil) {
  case world {
    GameWorld(_, _, creatures, _) ->
      list.find(creatures, fn(creature) {
        let Creature(_, _, id, _, _) = creature
        id == creature_id
      })
  }
}

// Remove a creature by ID
pub fn remove_creature(world: World, creature_id: Int) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let updated_creatures =
        list.filter(creatures, fn(creature) {
          let Creature(_, _, id, _, _) = creature
          id != creature_id
        })
      GameWorld(width, height, updated_creatures, turn)
    }
  }
}

// Update creature energy by ID
pub fn update_creature_energy(
  world: World,
  creature_id: Int,
  new_energy: Int,
) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let updated_creatures =
        list.map(creatures, fn(creature) {
          case creature {
            Creature(name, kind, id, _energy, position) ->
              case id == creature_id {
                True -> Creature(name, kind, id, new_energy, position)
                False -> creature
              }
          }
        })
      GameWorld(width, height, updated_creatures, turn)
    }
  }
}

// Get all creatures at a specific position
pub fn creatures_at_position(
  world: World,
  target_position: Position,
) -> List(Creature) {
  case world {
    GameWorld(_, _, creatures, _) ->
      list.filter(creatures, fn(creature) {
        let Creature(_, _, _, _, position) = creature
        position == target_position
      })
  }
}

// Find creature ID by name
pub fn find_creature_id_by_name(
  world: World,
  creature_name: String,
) -> Result(Int, String) {
  case world {
    GameWorld(_width, _height, creatures, _turn) -> {
      let matching_creature =
        list.find(creatures, fn(creature) {
          case creature {
            Creature(name, _kind, _id, _energy, _position) ->
              name == creature_name
          }
        })
      case matching_creature {
        Ok(Creature(_name, _kind, id, _energy, _position)) -> Ok(id)
        Error(_) ->
          Error("Creature with name '" <> creature_name <> "' not found")
      }
    }
  }
}

// Feed creature by name
pub fn feed_creature_by_name(
  world: World,
  creature_name: String,
  energy_amount: Int,
) -> Result(World, String) {
  case find_creature_id_by_name(world, creature_name) {
    Ok(creature_id) -> Ok(feed_creature(world, creature_id, energy_amount))
    Error(msg) -> Error(msg)
  }
}

pub fn feed_creature(
  world: World,
  creature_id: Int,
  energy_amount: Int,
) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let updated_creatures =
        list.map(creatures, fn(creature) {
          case creature {
            Creature(name, kind, id, energy, position) ->
              case id == creature_id {
                True ->
                  Creature(name, kind, id, energy + energy_amount, position)
                False -> creature
              }
          }
        })
      GameWorld(width, height, updated_creatures, turn)
    }
  }
}

// Decrease energy for all creatures
pub fn decrease_all_energy(world: World, amount: Int) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let updated_creatures =
        list.map(creatures, fn(creature) {
          let Creature(name, kind, id, energy, position) = creature
          let new_energy = int.max(0, energy - amount)
          Creature(name, kind, id, new_energy, position)
        })
      GameWorld(width, height, updated_creatures, turn)
    }
  }
}

// Remove creatures with zero energy
pub fn remove_starved_creatures(world: World) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let surviving_creatures =
        list.filter(creatures, fn(creature) {
          let Creature(_, _, _, energy, _) = creature
          energy > 0
        })
      GameWorld(width, height, surviving_creatures, turn)
    }
  }
}

// Get creatures with low energy
pub fn get_hungry_creatures(
  world: World,
  energy_threshold: Int,
) -> List(Creature) {
  case world {
    GameWorld(_, _, creatures, _) ->
      list.filter(creatures, fn(creature) {
        let Creature(_, _, _, energy, _) = creature
        energy <= energy_threshold
      })
  }
}

// Handle predator-prey interactions and herbivore feeding
pub fn process_interactions(world: World) -> World {
  world
  |> process_carnivore_feeding
  |> process_herbivore_feeding
  |> process_carnivore_fighting
}

// Process carnivores eating herbivores at the same position
fn process_carnivore_feeding(world: World) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let #(updated_creatures, _) =
        list.fold(creatures, #([], []), fn(acc, creature) {
          let #(remaining_creatures, eaten_ids) = acc
          let Creature(name, kind, id, energy, position) = creature

          case kind {
            types.Carnivore -> {
              let prey_at_position =
                list.filter(creatures, fn(other) {
                  let Creature(_, other_kind, other_id, _, other_position) =
                    other
                  other_kind == types.Herbivore
                  && position == other_position
                  && other_id != id
                })

              case prey_at_position {
                [first_prey, ..] -> {
                  let Creature(_, _, prey_id, prey_energy, _) = first_prey
                  let energy_gained = prey_energy / 2
                  let fed_carnivore =
                    Creature(name, kind, id, energy + energy_gained, position)
                  #([fed_carnivore, ..remaining_creatures], [
                    prey_id,
                    ..eaten_ids
                  ])
                }
                [] -> {
                  #([creature, ..remaining_creatures], eaten_ids)
                }
              }
            }
            _ -> {
              case list.contains(eaten_ids, id) {
                True -> #(remaining_creatures, eaten_ids)
                False -> #([creature, ..remaining_creatures], eaten_ids)
              }
            }
          }
        })

      GameWorld(width, height, list.reverse(updated_creatures), turn)
    }
  }
}

// Process herbivores eating plants at the same position  
fn process_herbivore_feeding(world: World) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let #(updated_creatures, _) =
        list.fold(creatures, #([], []), fn(acc, creature) {
          let #(remaining_creatures, eaten_plant_ids) = acc
          let Creature(name, kind, id, energy, position) = creature

          case kind {
            types.Herbivore -> {
              let plants_at_position =
                list.filter(creatures, fn(other) {
                  let Creature(_, other_kind, other_id, _, other_position) =
                    other
                  other_kind == types.Plant
                  && position == other_position
                  && other_id != id
                })

              case plants_at_position {
                [first_plant, ..] -> {
                  let Creature(_, _, plant_id, plant_energy, _) = first_plant
                  let energy_gained = plant_energy / 3
                  let fed_herbivore =
                    Creature(name, kind, id, energy + energy_gained, position)
                  #([fed_herbivore, ..remaining_creatures], [
                    plant_id,
                    ..eaten_plant_ids
                  ])
                }
                [] -> {
                  #([creature, ..remaining_creatures], eaten_plant_ids)
                }
              }
            }
            _ -> {
              case kind == types.Plant && list.contains(eaten_plant_ids, id) {
                True -> #(remaining_creatures, eaten_plant_ids)
                False -> #([creature, ..remaining_creatures], eaten_plant_ids)
              }
            }
          }
        })

      GameWorld(width, height, list.reverse(updated_creatures), turn)
    }
  }
}

// Process carnivore and omnivore fighting at the same position
fn process_carnivore_fighting(world: World) -> World {
  case world {
    GameWorld(width, height, creatures, turn) -> {
      let position_groups = utils.group_creatures_by_position(creatures)

      let updated_creatures =
        list.fold(position_groups, [], fn(acc, group) {
          let #(_position, creatures_at_position) = group

          let fighters =
            list.filter(creatures_at_position, fn(creature) {
              let Creature(_, kind, _, _, _) = creature
              kind == types.Carnivore || kind == types.Omnivore
            })

          let non_fighters =
            list.filter(creatures_at_position, fn(creature) {
              let Creature(_, kind, _, _, _) = creature
              kind == types.Herbivore || kind == types.Plant
            })

          case list.length(fighters) {
            0 | 1 -> list.append(acc, creatures_at_position)
            _ -> {
              let damaged_fighters =
                list.map(fighters, fn(fighter) {
                  let Creature(name, kind, id, energy, pos) = fighter
                  let fight_damage = 10
                  Creature(name, kind, id, energy - fight_damage, pos)
                })
              list.append(acc, list.append(damaged_fighters, non_fighters))
            }
          }
        })

      GameWorld(width, height, updated_creatures, turn)
    }
  }
}
