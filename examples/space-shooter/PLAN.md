# Space Shooter Implementation Plan

## Overview

A simple space shooter demonstrating:
- Entity Component System (ECS) using directories
- Vector math with Matrix type (`→V2`, `+`, `-`, `*`)
- Collision detection
- Spawning/despawning entities

## Project Structure

```
examples/space-shooter/
├── project.toml              # Project manifest
├── PLAN.md                   # This file
│
├── init.rpl                  # Initialization (runs once)
├── update.rpl                # Game logic (runs every frame)
├── draw.rpl                  # Rendering (runs every frame)
│
├── lib/                      # Reusable programs
│   ├── entity.rpl            # spawn_entity, despawn_entity
│   ├── helpers.rpl           # get_pos, set_pos, get_vel, etc.
│   ├── collision.rpl         # check_collision, check_collisions
│   └── utils.rpl             # clamp_to_screen, is_offscreen
│
├── systems/                  # Game systems (called from update.rpl)
│   ├── player.rpl            # update_player (input handling)
│   ├── enemies.rpl           # spawn_enemy, update_enemies
│   ├── bullets.rpl           # spawn_bullet, update_bullets
│   └── cleanup.rpl           # cleanup_entities (remove dead/offscreen)
│
└── sprites/                  # PNG assets
    ├── player.png
    ├── enemy.png
    └── bullet.png
```

## Project Manifest (project.toml)

```toml
[project]
name = "space-shooter"
version = "0.1.0"
author = "SR5"

[build]
include = ["**/*.rpl", "**/*.png"]

# Spritesheet definition
[sprites.sheet]
image = "sheet.png"

# Named regions within the spritesheet
[sprites.sheet.regions]
playerShip1_blue = { x = 211, y = 941, w = 99, h = 75 }
playerShip1_red = { x = 224, y = 832, w = 99, h = 75 }
enemyRed1 = { x = 425, y = 384, w = 93, h = 84 }
enemyBlue1 = { x = 425, y = 468, w = 93, h = 84 }
laserRed01 = { x = 858, y = 230, w = 9, h = 54 }
laserBlue01 = { x = 856, y = 421, w = 9, h = 54 }
# ... many more regions

# Animations (sequence of frames from the sheet)
[sprites.sheet.animations.engine_fire]
fps = 20
loop = true
frames = [
  { x = 827, y = 125, w = 16, h = 40 },
  { x = 828, y = 206, w = 14, h = 31 },
  # ... more frames
]
```

**Asset Loading Flow:**

1. `rpl-project` loads `project.toml` and all matching files
2. SR5's `Console::load_project()` receives the loaded project
3. SR5 parses `[sprites.*]` sections from the manifest
4. For each spritesheet (e.g., `[sprites.sheet]`):
   - Load the image file (e.g., `sheet.png`)
   - For each region in `[sprites.sheet.regions]`:
     - Crop the region from the image
     - Store cropped bytes at `_resources/sprites/{sheet}/{region_name}`
   - For each animation in `[sprites.sheet.animations.*]`:
     - Crop each frame
     - Store animation data at `_resources/sprites/{sheet}/animations/{anim_name}/`
5. Game code accesses sprites via path-based RCL

**Implementation Notes:**

- `rpl-project::Manifest` needs to preserve extra TOML sections (or return raw TOML)
- SR5 parses `[sprites.*]` sections to extract spritesheets and regions
- Resources stored under `_resources/sprites/` to avoid collisions with user code
- Cropped regions stored as raw RGBA bytes (ready for SPRLOAD) or PNG bytes (for PNGLOAD)

## Runtime Directory Structure

```
HOME/
├── _resources/
│   └── sprites/
│       └── sheet/                      # From [sprites.sheet]
│           ├── playerShip1_blue: <Bytes>
│           ├── playerShip1_red: <Bytes>
│           ├── enemyRed1: <Bytes>
│           ├── enemyBlue1: <Bytes>
│           ├── laserRed01: <Bytes>
│           ├── laserBlue01: <Bytes>
│           │   ... (all regions)
│           └── animations/
│               └── engine_fire/
│                   ├── fps: 20
│                   ├── loop: 1
│                   └── frames: { <Bytes> <Bytes> ... }
│
├── screen_w: 480
├── screen_h: 320
├── entity_ids: { 0 1 2 ... }
├── next_entity_id: 5
├── player_id: 0
├── score: 100
├── lives: 3
│
└── entities/
    ├── 0/                    # Player
    │   ├── pos: [ 240 280 ]
    │   ├── vel: [ 0 0 ]
    │   ├── sprite: 1         # VRAM sprite ID (after PNGLOAD)
    │   └── type: "player"
    ├── 1/                    # Enemy
    │   ├── pos: [ 100 50 ]
    │   ├── vel: [ 0 2 ]
    │   ├── sprite: 2
    │   └── type: "enemy"
    └── 2/                    # Bullet
        ├── pos: [ 240 270 ]
        ├── vel: [ 0 -8 ]
        ├── sprite: 3
        └── type: "bullet"
```

## Entity System

### Entity List

Track active entity IDs in a list:
```rpl
{ 0 1 2 } 'entity_ids' STO
```

### Component Access (lib/helpers.rpl)

```rpl
@ get_pos: ( id -- pos )
<< →STR "entities" SWAP 2 →LIST "pos" + RCL >>

@ set_pos: ( pos id -- )
<< →STR "entities" SWAP 2 →LIST "pos" + STO >>

@ get_vel: ( id -- vel )
<< →STR "entities" SWAP 2 →LIST "vel" + RCL >>

@ get_type: ( id -- type_str )
<< →STR "entities" SWAP 2 →LIST "type" + RCL >>

@ get_sprite: ( id -- sprite_id )
<< →STR "entities" SWAP 2 →LIST "sprite" + RCL >>
```

### Entity Spawning (lib/entity.rpl)

```rpl
@ spawn_entity: ( pos vel sprite type -- id )
<<
  → pos vel sprite type
  <<
    next_entity_id →STR → id_str
    <<
      @ Create entity directory
      "entities/" id_str + CRDIR

      @ Store components
      pos { "entities" id_str "pos" } STO
      vel { "entities" id_str "vel" } STO
      sprite { "entities" id_str "sprite" } STO
      type { "entities" id_str "type" } STO

      @ Add to entity list
      entity_ids next_entity_id + 'entity_ids' STO

      @ Increment and return ID
      next_entity_id DUP 1 + 'next_entity_id' STO
    >>
  >>
>>
```

## Main Files

### init.rpl

```rpl
<<
  @ Screen setup
  480 'screen_w' STO
  320 'screen_h' STO

  @ Sprite bytes already loaded by SR5 into _resources/sprites/sheet/
  @ Load into VRAM and cache sprite IDs for quick access
  "_resources/sprites/sheet/playerShip1_blue" RCL PNGLOAD DROP DROP 'spr_player' STO
  "_resources/sprites/sheet/enemyRed1" RCL PNGLOAD DROP DROP 'spr_enemy' STO
  "_resources/sprites/sheet/laserBlue01" RCL PNGLOAD DROP DROP 'spr_bullet' STO

  @ Create entities directory
  CRDIR "entities" STO

  @ Initialize state
  { } 'entity_ids' STO
  0 'next_entity_id' STO
  0 'score' STO
  3 'lives' STO
  0 'spawn_timer' STO
  60 'spawn_interval' STO

  @ Spawn player
  screen_w 2 / screen_h 50 - →V2   @ pos
  0 0 →V2                           @ vel
  spr_player "player"               @ sprite type
  spawn_entity 'player_id' STO
>>
```

### update.rpl

```rpl
<<
  @ Spawn enemies on timer
  spawn_timer 1 + 'spawn_timer' STO
  IF spawn_timer spawn_interval >= THEN
    spawn_enemy
    0 'spawn_timer' STO
  END

  @ Run systems
  update_player
  update_bullets
  update_enemies
  check_collisions
  cleanup_entities
>>
```

### draw.rpl

```rpl
<<
  @ Draw all entities
  entity_ids <<
    DUP get_sprite
    SWAP get_pos V→ DROP   @ x y
    SPRDRAW
  >> DOLIST DROP

  @ Draw UI
  @ TODO: score, lives
>>
```

## Systems

### systems/player.rpl

```rpl
@ update_player: ( -- )
<<
  BTNS → btns
  <<
    @ Build velocity from input
    0 0 →V2 → vel
    <<
      IF btns 1024 BAND THEN vel -4 0 →V2 + 'vel' STO END  @ Left
      IF btns 2048 BAND THEN vel 4 0 →V2 + 'vel' STO END   @ Right
      IF btns 256 BAND THEN vel 0 -4 →V2 + 'vel' STO END   @ Up
      IF btns 512 BAND THEN vel 0 4 →V2 + 'vel' STO END    @ Down

      @ Apply velocity and clamp
      player_id get_pos vel + clamp_to_screen player_id set_pos

      @ Fire (A button)
      IF btns 1 BAND THEN
        spawn_bullet
      END
    >>
  >>
>>
```

### systems/bullets.rpl

```rpl
@ spawn_bullet: ( -- )
<<
  player_id get_pos 0 -10 →V2 +   @ pos above player
  0 -8 →V2                         @ vel upward
  spr_bullet "bullet"              @ sprite type
  spawn_entity DROP
>>

@ update_bullets: ( -- )
<<
  entity_ids <<
    DUP get_type "bullet" == IF
      DUP DUP get_pos SWAP get_vel + SWAP set_pos
    END
  >> DOLIST DROP
>>
```

### systems/enemies.rpl

```rpl
@ spawn_enemy: ( -- )
<<
  RAND screen_w * FLOOR 50 MAX screen_w 50 - MIN   @ random x
  20 →V2                                            @ pos at top
  0 RAND 2 * 1 + →V2                               @ vel downward
  spr_enemy "enemy"                                 @ sprite type
  spawn_entity DROP
>>

@ update_enemies: ( -- )
<<
  entity_ids <<
    DUP get_type "enemy" == IF
      DUP DUP get_pos SWAP get_vel + SWAP set_pos
    END
  >> DOLIST DROP
>>
```

## Collision Detection (lib/collision.rpl)

```rpl
@ check_collision: ( id1 id2 -- flag )
<<
  get_pos SWAP get_pos   @ pos2 pos1
  -                       @ diff vector
  ABS                     @ magnitude (distance)
  20 <                    @ collision radius
>>

@ check_collisions: ( -- )
<<
  @ For each bullet vs each enemy
  entity_ids <<
    DUP get_type "bullet" == IF
      → bullet_id <<
        entity_ids <<
          DUP get_type "enemy" == IF
            DUP bullet_id check_collision IF
              DUP mark_dead
              bullet_id mark_dead
              score 10 + 'score' STO
            END
          END
        >> DOLIST DROP
      >>
    END
  >> DOLIST DROP
>>
```

## Cleanup (systems/cleanup.rpl)

```rpl
@ mark_dead: ( id -- )
<<
  →STR "entities" SWAP 2 →LIST "dead" + 1 STO
>>

@ is_dead: ( id -- flag )
<<
  →STR "entities" SWAP 2 →LIST "dead" + RCL 1 ==
>>

@ is_offscreen: ( id -- flag )
<<
  get_pos V→ DROP → x y
  <<
    x 0 < x screen_w > OR
    y 0 < y screen_h > OR
    OR
  >>
>>

@ cleanup_entities: ( -- )
<<
  entity_ids <<
    DUP is_dead NOT
    OVER is_offscreen NOT
    AND
  >> SELECT 'entity_ids' STO

  @ TODO: PURGE dead entity directories
>>
```

## Utilities (lib/utils.rpl)

```rpl
@ clamp_to_screen: ( pos -- pos )
<<
  V→ DROP → x y
  <<
    x 16 MAX screen_w 16 - MIN
    y 16 MAX screen_h 16 - MIN
    →V2
  >>
>>
```

## Milestones

### Milestone 0: SR5 Asset Loading
- [ ] Extend rpl-project Manifest to preserve raw TOML (or extra sections)
- [ ] Create Sr5Manifest struct in rpl-sr5
- [ ] Parse `[sr5.sprites]` section
- [ ] Load PNGs and create sprites during Console::load_project()
- [ ] Store sprite IDs as `spr_{name}` in directory

### Milestone 1: Project Structure
- [ ] Create directory layout
- [ ] Create project.toml
- [ ] Stub out all files

### Milestone 2: Entity System
- [ ] lib/helpers.rpl - component accessors
- [ ] lib/entity.rpl - spawn_entity
- [ ] Convert player to entity

### Milestone 3: Player Movement
- [ ] systems/player.rpl - input handling
- [ ] lib/utils.rpl - clamp_to_screen
- [ ] Player uses vector position/velocity

### Milestone 4: Bullets
- [ ] systems/bullets.rpl - spawn, update
- [ ] Fire on button press

### Milestone 5: Enemies
- [ ] systems/enemies.rpl - spawn, update
- [ ] Spawn timer

### Milestone 6: Collisions & Cleanup
- [ ] lib/collision.rpl - detection
- [ ] systems/cleanup.rpl - remove dead/offscreen
- [ ] Scoring

### Milestone 7: Polish
- [ ] Player-enemy collision (lives)
- [ ] Game over state
- [ ] UI (score, lives display)
