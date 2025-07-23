# Godot.Garnet

Very exprimental library meant to help write Godot games with almost all code
driven completely from the F# side outside of a *single* glue C# script.

Main feature right now is importing scenes into a `Container` from the **[Garnet](https://github.com/bcarruthers/garnet) ECS** with minimal boilerplate and zero per‑frame allocations,
but as I work more with Godot and F# I will most likely add any generic relevant helpers here if deemed necessary.

## Installation

```bash
dotnet add package vmenge.Godot.Garnet
```

## How It Works

Godot's [groups](https://docs.godotengine.org/en/4.4/tutorials/scripting/groups.html) are used to tag nodes that should be converted into entities or `Marker` or `Link` components.
When you bring `Godot.Garnet` into scope, an `ImportScene` extension method is added to `Garnet.Composition.Container`.
`Container.ImportScene(this, sceneRoot)` walks the subtree rooted at sceneRoot and:
- Creates an ECS entity for every node in the `entity` group.
- Adds all children Nodes directly as components to that entity.

Additionally, there are two Attributes added to make one's life easier:
- `[<Marker("markergroupname")>]` will create components for an entity if the Node in the `"entity"` group or any of its children belongs to `markergroupname`.
  ```fs
  // Marker attribute structs should **always** be empty.
  [<Marker("spawnPoint")>]
  type SpawnPoint = struct end
  ```

- `[<Link("linkgroupname")>]` will add the component tagged with the `Link` attribute to its parent entity **instead** of the raw node.
  ```fs
  // Link attribute structs should **always** have a single
  // field for the `Node` it is linking to.
  [<Struct;Link("player")>]
  type Player = {
      Node: CharacterBody2D
  }
  ```

Both `Link` and `Marker` groups can be assigned both to the parent `"entity"` node, or any of its children.

So the following Godot node structure
```
World (Node2D)
├─ Player (CharacterBody2D)   [groups: entity, player]
│  └─ CollisionShape2D        [no groups]
└─ SpawnPoint (Marker2D)      [groups: entity, spawnPoint]
```

Using the `Marker` and `Link` attributes like so
```fs
  [<Marker("spawnPoint")>]
  type SpawnPoint = struct end

  [<Struct;Link("player")>]
  type Player = {
      Node: CharacterBody2D
  }
```

Upon importing the `World` node with `Container.ImportScene`, would result in the following entities with components:
```
ECS World
├─ Entity #1
│  ├─ Player                ← link component (holds CharacterBody2D node)
│  └─ CollisionShape2D      ← node component
└─ Entity #2
   ├─ SpawnPoint            ← marker component
   └─ Marker2D              ← node component
```

One big difference between `Marker` and `Link`, is that `Marker` components are added alongside the normal component import chain. `Link` components
will always be added **instead** of the component it is linked to.

With the above structure, one could query the components through Garnet
```fs
let mySystem (c: Container) =
  c.On<Ready> 
    <| fun e ->
        for r in c.Query<SpawnPoint, Marker2D>() do 
          let marker = r.Value2
          // do something

        for r in c.Query<Player, CollisionShape2D>() do
          let characterBody2d, collisionShape2d = r.Value1.Node, r.Value2
          // do something
```

## Quickstart example
An implementation of Godot's *Dodge the Creeps* can be found in [this repo](https://github.com/vmenge/dodge_the_creeps_fsharp). 
Though one should be able to get started with something as simple as the following:

On the F# project:
```fs
// Main.fs
module Main

open Godot
open Godot.Garnet

[<Struct>]
type Ready = { Root: Node }

[<Struct>]
type Process = { Root: Node; Dt: float }

type FWorld =
    { Container: Container
      Disposable: IDisposable }

    static member Make node =
        let c = Container()
        c.ImportScene node

        {
            Container = c
            Disposable =
                Disposable.Create [
                    c.On<Ready> 
                    <| fun e -> GD.Print "root scene is ready!"

                    c.On<Process>
                    <| fun e ->
                        GD.Print $"hello from process! deltatime: {e.Dt}"
                ]
        }
```

On the Godot C# project, a single `World.cs` or equivalent is all that is needed, attached to the root node of a scene:
```cs
using Godot;
using static Main;

public partial class World : Node2D
{
    FWorld world = null!;

    public override void _Ready()
    {
        world = FWorld.Make(this);
        world.Container.Run(new Ready(this));
    }

    public override void _Process(double delta)
    {
        world.Container.Run(new Process(this, delta));
    }
}
```

From there, using the `Marker` and `Link` attributes to define structs that correspond to Nodes that belong to **or** are children of a `"entity"` group Node and their
equivalent groups should be more than enough to run `Garnet` queries as per its documentation.
