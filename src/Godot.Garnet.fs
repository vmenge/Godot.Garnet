module Godot.Garnet

open System
open System.Reflection
open System.Linq.Expressions
open System.Collections.Generic
open Godot
open Garnet.Composition

/// <summary>
/// Tags a <c>struct</c> that represents a **pure marker component**.
/// </summary>
/// <remarks>
/// • The attribute’s <c>name</c> maps 1‑to‑1 to a Godot group placed on a
///   scene node.
/// • The tagged struct **must** be value‑type (<c>struct</c>) and contain
///   **no fields**.
/// • At import time the converter detects the group, creates the entity,
///   and calls <c>Entity.With&lt;T&gt;()</c> with
///   <c>Unchecked.defaultof&lt;T&gt;</c>; no allocations or boxing occur.
/// <example>
/// <code>
/// [&lt;Struct; Marker("enemy_spawn")&gt;]
/// type EnemySpawn = struct end
/// </code>
/// Place the group <c>enemy_spawn</c> on any node; every such node becomes an
/// entity marked with <c>EnemySpawn</c>.
/// </example>
/// </remarks>
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Struct)>]
type MarkerAttribute(name: string) =
    inherit System.Attribute()
    member _.Name = name


/// <summary>
/// Tags a <c>struct</c> that links **exactly one** Godot node instance to its entity.
/// </summary>
/// <remarks>
/// • The struct must be a value type and contain **one field** whose type derives from <c>Godot.Node</c>.
/// • The attribute’s <c>name</c> must match a Godot group placed on nodes to be linked.
/// • On import, the converter instantiates the struct, assigns the node to the field, and adds it via <c>Entity.With&lt;T&gt;</c> — zero boxing, zero heap allocations.
/// • When this is done, instead of the <c>Node</c> being added as a component, the <c>struct</c> with the <c>Link</c> attribute is added as a component instead.
///
/// **Example**
/// <code>
/// [&lt;Struct; Link("plane")&gt;]
/// type Plane = {
///   Node: CharacterBody2D
/// }
/// </code>
/// Any node with group <c>plane</c> becomes an entity containing
/// <c>Plane</c>; the <c>Node</c> field stores that <c>CharacterBody2D</c>.
/// </remarks>
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Struct, AllowMultiple = false)>]
type LinkAttribute(name: string) =
    inherit Attribute()
    member _.Name = name

module private Helpers =
    let withMethod = typeof<Entity>.GetMethod "With"

    let inline addComponent (e: Entity) (n: obj) =
        withMethod.MakeGenericMethod(n.GetType()).Invoke(e, [| n |]) |> ignore

    let isEntity (str: StringName) = (string str) = "entity"

    let markerAdders: System.Collections.Generic.IDictionary<string, Entity -> unit> =
        Assembly.GetExecutingAssembly().GetTypes()
        |> Seq.choose (fun t ->
            t.GetCustomAttribute<MarkerAttribute>()
            |> Option.ofObj
            |> Option.map (fun attr ->
                if not t.IsValueType then
                    invalidOp $"{t.FullName} must be struct"

                if t.GetFields(BindingFlags.Instance ||| BindingFlags.Public).Length <> 0 then
                    invalidOp $"{t.FullName} must have no instance fields"

                let entP = Expression.Parameter(typeof<Entity>, "e")
                let defVal = Expression.Default t

                let call =
                    System.Linq.Expressions.Expression.Call(entP, withMethod.MakeGenericMethod t, defVal)

                let lam = Expression.Lambda<Action<Entity>>(call, entP).Compile()

                attr.Name, fun e -> lam.Invoke(e)))
        |> dict


    let tryAddMarkers (e: Entity) (node: Node) =
        for g in node.GetGroups() do
            match markerAdders.TryGetValue(string g) with
            | true, add -> add e
            | _ -> ()

    let linkAdders: IDictionary<string, Entity -> Node -> unit> =
        Assembly.GetExecutingAssembly().GetTypes()
        |> Seq.choose (fun t ->
            t.GetCustomAttribute<LinkAttribute>()
            |> Option.ofObj
            |> Option.map (fun attr ->
                let entP = Expression.Parameter(typeof<Entity>, "e")
                let nodeP = Expression.Parameter(typeof<Node>, "n")
                let var = Expression.Variable(t, "v")

                let field =
                    t.GetFields(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                    |> Array.head

                let assign =
                    Expression.Assign(Expression.Field(var, field), Expression.Convert(nodeP, field.FieldType))

                let withCall =
                    System.Linq.Expressions.Expression.Call(entP, withMethod.MakeGenericMethod t, var)

                let lam =
                    Expression
                        .Lambda<Action<Entity, Node>>(Expression.Block([ var ], assign, withCall), entP, nodeP)
                        .Compile()

                attr.Name, fun e n -> lam.Invoke(e, n)))
        |> dict


    let tryAddLink (e: Entity) (n: Node) =
        let links =
            n.GetGroups()
            |> Seq.filter (fun g ->
                match linkAdders.TryGetValue(string g) with
                | true, f ->
                    f e n
                    true
                | _ -> false)

        match Seq.length links with
        | 0 -> false
        | 1 -> true
        | _ -> invalidOp "only a single Link per Node is allowed"

open Helpers

type Container with
    /// <summary>
    /// Imports a Godot scene subtree into the Garnet ECS world that this
    /// <see cref="Container"/> manages.
    /// </summary>
    /// <remarks>
    /// • Every node in the **entity** group becomes a new ECS entity.
    /// • All non‑entity descendants are added to that entity:
    ///     • Groups backed by **MarkerAttribute** structs add their marker
    ///       components.
    ///     • Groups backed by **LinkAttribute** structs add a link component whose
    ///       single field receives the node reference.
    ///     • Any other node is stored directly as a component via
    ///       <c>Entity.With(node)</c>.
    /// • Nested entity nodes are illegal; the function throws if it encounters
    ///   one.
    /// • The entire conversion is allocation‑free at run time:
    ///   reflection and delegate generation happen once at startup.
    /// </remarks>
    member c.ImportScene(sceneRoot: Node) =
        let rec buildEntity (n: Node) =
            let e = c.Create()
            tryAddMarkers e n

            if not <| tryAddLink e n then
                addComponent e n

            let rec addDesc (parent: Node) =
                for c in parent.GetChildren() do
                    if c.IsInGroup "entity" then
                        failwith $"Nested entity '{c.Name}' inside '{parent.Name}'"

                    tryAddMarkers e c

                    if not <| tryAddLink e c then
                        addComponent e c

                    addDesc c

            addDesc n

        if sceneRoot.GetGroups() |> Seq.exists isEntity then
            buildEntity sceneRoot
        else
            sceneRoot.GetChildren()
            |> Seq.filter (fun c -> c.GetGroups() |> Seq.exists isEntity)
            |> Seq.iter buildEntity
