pub type CreatureType {
  Herbivore
  Carnivore
  Omnivore
  Plant
}

pub type Position {
  Position(x: Int, y: Int)
}

pub type Creature {
  Creature(
    name: String,
    kind: CreatureType,
    id: Int,
    energy: Int,
    position: Position
  )
}

pub type World {
  GameWorld(
    width: Int, 
    height: Int,
    creatures: List(Creature),
    turn: Int
  )
}
  