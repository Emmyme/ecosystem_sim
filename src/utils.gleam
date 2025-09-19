import gleam/int
import gleam/list
import types.{
  type Creature, type Position, type World, Creature, GameWorld, Position,
}

// Helper function to generate random number in a range
pub fn random_in_range(min: Int, max: Int) -> Int {
  case max >= min {
    True -> min + int.random(max - min + 1)
    False -> min
  }
}

// Generate a random position within world boundaries
pub fn random_position(world: World) -> Position {
  case world {
    GameWorld(width, height, _, _) ->
      Position(x: int.random(width), y: int.random(height))
  }
}

// Clamp a value between min and max bounds
pub fn clamp(value: Int, min: Int, max: Int) -> Int {
  int.max(min, int.min(max, value))
}

// Helper function to group creatures by their position
pub fn group_creatures_by_position(
  creatures: List(Creature),
) -> List(#(Position, List(Creature))) {
  // This is a simplified implementation - group creatures sharing the same position
  list.fold(creatures, [], fn(acc, creature) {
    let Creature(_, _, _, _, position) = creature

    // Check if we already have this position in our groups
    case
      list.find(acc, fn(group) {
        let #(pos, _) = group
        pos == position
      })
    {
      Ok(#(_, existing_creatures)) -> {
        // Add to existing group and remove the old entry
        let new_group = #(position, [creature, ..existing_creatures])
        let filtered_acc =
          list.filter(acc, fn(group) {
            let #(pos, _) = group
            pos != position
          })
        [new_group, ..filtered_acc]
      }
      Error(_) -> {
        // Create new group for this position
        [#(position, [creature]), ..acc]
      }
    }
  })
}
