module App

open System
open System.Security.Claims
open System.Threading
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Features
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.SignalR
open Giraffe
open Giraffe.GiraffeViewEngine
open Microsoft.AspNetCore.Cors.Infrastructure
open System.IO
open Microsoft.Extensions.Hosting
open System.Threading.Tasks
open Connections

type IClientApi =
    abstract Alive : unit -> Task
    abstract LockBoard : bool -> Task
    abstract LoginResponse : bool * string -> Task
    abstract Message : string -> Task
    abstract GameState : string -> Task

type GameHub() =
    inherit Hub<IClientApi>()
    
    /// Accept client logins  
    member this.Login(userId : string, pwd: string) =
        let connectionId = this.Context.ConnectionId

        task {
            // Maybe Async.StartImmediateAsTask is the right thing to do instead of Async.StartAsTask
            match! tryAuthenticate userId pwd |> Async.StartAsTask with
            | true -> 
                register userId connectionId
                this.Clients.Client(connectionId).LoginResponse(true, userId) |> ignore
                this.Clients.All.Message(sprintf "New Player: %s (%s)" userId connectionId) |> ignore
            | false -> 
                this.Clients.Client(connectionId).LoginResponse(false, userId) |> ignore
        }
        
        
       
        
    /// Handle client logout
    member this.Logout(userId : string) =
        this.Clients.All.Message(sprintf "Player left: %s" userId) |> ignore
        deregisterAll userId
        
    
    /// Handle player changing direction
    //   member this.Turn (playerId :string, direction :string) = 
    //     task { }
    //     updatePlayerDirection playerId direction
    /// Pass along message from one client to all clients
    member this.Send(message : string) =
        this.Clients.All.Message(message) |> ignore

type GameService(hubContext : IHubContext<GameHub, IClientApi>) =
    inherit BackgroundService()
    override this.ExecuteAsync(stoppingToken : CancellationToken) =
        let pingTimer = new System.Timers.Timer(1000.0)
        pingTimer.Elapsed.Add (fun _ -> 
            printfn "Timer elapsed"
            hubContext.Clients.All.Message("stateSerialized") 
            |> ignore)
        //this.HubContext.Clients.All.LoginResponse(true, "stateSerialized") |> ignore)
        pingTimer.Start()
        Task.CompletedTask

[<CLIMutable>]
type Person =
    { Name : string }

let layout (content : XmlNode list) =
    html [] [ head [] [ title [] [ str "Giraffe" ] ]
              body [] content ]

let partial() = p [] [ str "Some partial text." ]

let personView (model : Person) =
    [ div [ _class "container" ] 
          [ h3 [ _title "Some title attribute" ] 
                [ sprintf "Hello, %s" model.Name |> str ]
            
            a [ _href "https://github.com/giraffe-fsharp/Giraffe" ] 
                [ str "Github" ] ]
      div [] [ partial() ] ]
    |> layout

// ---------------------------------
// Error handler
// ---------------------------------
let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError
        (EventId(), ex, 
         "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Web app
// ---------------------------------
let authScheme = CookieAuthenticationDefaults.AuthenticationScheme
let accessDenied = setStatusCode 401 >=> text "Access Denied"
let mustBeUser = requiresAuthentication accessDenied
let mustBeAdmin =
    requiresAuthentication accessDenied >=> requiresRole "Admin" accessDenied
let mustBeJohn =
    requiresAuthentication accessDenied 
    >=> authorizeUser (fun u -> u.HasClaim(ClaimTypes.Name, "John")) 
            accessDenied

let loginHandler =
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        task { 
            let issuer = "http://localhost:5000"
            
            let claims =
                [ Claim(ClaimTypes.Name, "John", ClaimValueTypes.String, issuer)
                  
                  Claim
                      (ClaimTypes.Surname, "Doe", ClaimValueTypes.String, issuer)
                  
                  Claim
                      (ClaimTypes.Role, "Admin", ClaimValueTypes.String, issuer) ]
            
            let identity = ClaimsIdentity(claims, authScheme)
            let user = ClaimsPrincipal(identity)
            do! ctx.SignInAsync(authScheme, user)
            return! text "Successfully logged in" next ctx
        }

let userHandler =
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        text ctx.User.Identity.Name next ctx
let showUserHandler id = mustBeAdmin >=> text (sprintf "User ID: %i" id)

let configuredHandler =
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        let configuration = ctx.GetService<IConfiguration>()
        text configuration.["HelloMessage"] next ctx

let fileUploadHandler =
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        task { 
            return! (match ctx.Request.HasFormContentType with
                     | false -> RequestErrors.BAD_REQUEST "Bad request"
                     | true -> 
                         ctx.Request.Form.Files
                         |> Seq.fold 
                                (fun acc file -> 
                                sprintf "%s\n%s" acc file.FileName) ""
                         |> text) next ctx
        }

let fileUploadHandler2 =
    fun (next : HttpFunc) (ctx : HttpContext) -> 
        task { 
            let formFeature = ctx.Features.Get<IFormFeature>()
            let! form = formFeature.ReadFormAsync CancellationToken.None
            return! (form.Files
                     |> Seq.fold 
                            (fun acc file -> sprintf "%s\n%s" acc file.FileName) 
                            ""
                     |> text) next ctx
        }

let cacheHandler1 : HttpHandler =
    publicResponseCaching 30 None 
    >=> warbler (fun _ -> text (Guid.NewGuid().ToString()))
let cacheHandler2 : HttpHandler =
    responseCaching (Public(TimeSpan.FromSeconds(float 30))) None 
        (Some [| "key1"; "key2" |]) 
    >=> warbler (fun _ -> text (Guid.NewGuid().ToString()))
let cacheHandler3 : HttpHandler =
    noResponseCaching >=> warbler (fun _ -> text (Guid.NewGuid().ToString()))
let time() = System.DateTime.Now.ToString()

[<CLIMutable>]
type Car =
    { Name : string
      Make : string
      Wheels : int
      Built : DateTime }
    interface IModelValidation<Car> with
        member this.Validate() =
            if this.Wheels > 1 && this.Wheels <= 6 then Ok this
            else 
                Error
                    (RequestErrors.BAD_REQUEST 
                         "Wheels must be a value between 2 and 6.")

let parsingErrorHandler err = RequestErrors.BAD_REQUEST err

let webApp =
    choose [ GET >=> choose [ route "/" >=> text "index"
                              route "/ping" >=> text "pong"
                              
                              route "/error" 
                              >=> (fun _ _ -> failwith "Something went wrong!")
                              route "/login" >=> loginHandler
                              
                              route "/logout" >=> signOut authScheme 
                              >=> text "Successfully logged out."
                              route "/user" >=> mustBeUser >=> userHandler
                              route "/john-only" >=> mustBeJohn >=> userHandler
                              routef "/user/%i" showUserHandler
                              
                              route "/person" 
                              >=> (personView { Name = "Html Node" } |> htmlView)
                              route "/once" >=> (time() |> text)
                              
                              route "/everytime" 
                              >=> warbler (fun _ -> (time() |> text))
                              route "/configured" >=> configuredHandler
                              route "/upload" >=> fileUploadHandler
                              route "/upload2" >=> fileUploadHandler2
                              route "/cache/1" >=> cacheHandler1
                              route "/cache/2" >=> cacheHandler2
                              route "/cache/3" >=> cacheHandler3 ]
             route "/car" >=> bindModel<Car> None json
             
             route "/car2" 
             >=> tryBindQuery<Car> parsingErrorHandler None (validateModel xml)
             RequestErrors.notFound (text "Not Found") ]

// ---------------------------------
// Main
// ---------------------------------
let cookieAuth (o : CookieAuthenticationOptions) =
    do o.Cookie.HttpOnly <- true
       o.Cookie.SecurePolicy <- CookieSecurePolicy.SameAsRequest
       o.SlidingExpiration <- true
       o.ExpireTimeSpan <- TimeSpan.FromDays 7.0

let configureApp (app : IApplicationBuilder) =
    app.UseGiraffeErrorHandler(errorHandler) |> ignore
    //.UseEndpoints(fun endpoints -> endpoints.MapHub<GameHub>("/gameHub") |> ignore)
    app.UseStaticFiles() |> ignore
    app.UseAuthentication() |> ignore
    app.UseResponseCaching() |> ignore
    app.UseCors("CorsPolicy") |> ignore
    // In the older version of the framework we used to use the following:
    //    app.UseSignalR(fun routes -> routes.MapHub<GameHub>(PathString "/gameHub")) |> ignore
    // Still works but deprecated. It is replaced by the following TWO lines
    app.UseRouting() |> ignore
    app.UseEndpoints
        (fun endpoints -> endpoints.MapHub<GameHub>("/gameHub") |> ignore) 
    |> ignore
    app.UseGiraffe webApp |> ignore

let configureServices (services : IServiceCollection) =
    services.AddResponseCaching()
            .AddGiraffe()
            .AddHostedService<GameService>()
            .AddAuthentication(authScheme)
            .AddCookie(cookieAuth) |> ignore

    services.Configure(fun (options : CookiePolicyOptions) -> 
        options.CheckConsentNeeded <- fun context -> true
        options.MinimumSameSitePolicy <- SameSiteMode.None)
    |> ignore
    
    services.AddCors
        (fun options -> 
        options.AddPolicy
            ("CorsPolicy", 
             (fun builder -> 
             builder.AllowAnyMethod().AllowAnyHeader()
                    .WithOrigins("http://localhost:55830").AllowCredentials() 
             |> ignore))) |> ignore
    services.AddSignalR() |> ignore
    services.AddDataProtection() |> ignore

let configureLogging (loggerBuilder : ILoggingBuilder) =
    loggerBuilder.AddFilter(fun lvl -> lvl.Equals LogLevel.Error).AddConsole()
                 .AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHost.CreateDefaultBuilder()
           .Configure(Action<IApplicationBuilder> configureApp)
           .ConfigureServices(configureServices)
           .ConfigureLogging(configureLogging).UseContentRoot(contentRoot)
           .UseIISIntegration().UseWebRoot(webRoot)
           .UseUrls([| "http://10.0.1.200:8080" |]).Build().Run()
    0
