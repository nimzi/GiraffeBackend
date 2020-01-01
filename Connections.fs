module Connections

open System.Collections.Generic

type Agent<'T> = MailboxProcessor<'T>

type RegistrationRequest =
    | Check of user: string * password: string * channel: AsyncReplyChannel<bool>
    | Register of user: string * connection: string
    | DeregisterSingleConnection of string
    | DeregisterAllConnectionsOfUser of string


let userDb =
    Map.ofList
        [ ("dylan", "canberra")
          ("bob", "ottawa")
          ("lola", "moscow") ]

type UserId = User of string

type ConnId = Conn of string

type private ForwardMap = Map<UserId, Set<ConnId>>

type private BackMap = Map<ConnId, UserId>

type private State = ForwardMap * BackMap


let connsOfUser user (forward: ForwardMap) =
    forward
    |> Map.tryFind user
    |> Option.defaultValue Set.empty

let add user conn (state: State): State =
    let (forward, back) = state

    let connections =
        forward
        |> connsOfUser user
        |> Set.add conn
    forward |> Map.add user connections, back |> Map.add conn user

let remove conn (state: State): State =
    let (forward, back) = state
    match back |> Map.tryFind conn with
    | Some(user) ->
        let connections =
            forward
            |> connsOfUser user
            |> Set.remove conn

        let forward' =
            if connections.IsEmpty then forward |> Map.remove user else forward |> Map.add user connections

        forward', back |> Map.remove conn
    | _ -> state

let removeAll user (state: State): State =
    let (forward, back) = state
    let mutable back' = back
    for con in connsOfUser user forward do
        back' <- back' |> Map.remove con

    forward |> Map.remove user, back'


let agent =
    Agent.Start <| fun inbox ->
        let rec messageLoop (registry: State) =
            async {
                match! inbox.Receive() with
                | Register(uid, conn) -> return! messageLoop (add (User uid) (Conn conn) registry)
                | DeregisterSingleConnection conn -> return! messageLoop (remove (Conn conn) registry)
                | DeregisterAllConnectionsOfUser uid -> return! messageLoop (removeAll (User uid) registry)
                | Check(uid, pwd, channel) ->
                    userDb
                    |> Map.tryFind uid
                    |> Option.map (fun lookup -> lookup = pwd)
                    |> Option.defaultValue false
                    |> channel.Reply
                    return! messageLoop (registry)
            }

        let zero: State = Map.empty, Map.empty
        messageLoop zero


let tryAuthenticate user pwd = agent.PostAndAsyncReply <| fun channel -> Check(user, pwd, channel)

let register user conn = Register(user, conn) |> agent.Post
let deregister conn = DeregisterSingleConnection conn |> agent.Post
let deregisterAll user = DeregisterAllConnectionsOfUser user |> agent.Post
