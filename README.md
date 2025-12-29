# Loom

A Rails-like web framework for Lean 4, weaving together Citadel (HTTP), Scribe (HTML), and Ledger (database) into a cohesive full-stack experience.

## Features

- **Sessions**: Cookie-based sessions with cryptographic signing
- **Flash Messages**: One-time messages that persist across redirects
- **Form Handling**: URL-encoded form parsing with CSRF protection
- **Named Routes**: Route registration with URL generation helpers
- **Middleware**: Composable request/response middleware
- **Static Files**: Static file serving with MIME type detection

## Installation

Add to your `lakefile.lean`:

```lean
require loom from git "https://github.com/nathanial/loom" @ "v0.0.1"
```

## Quick Start

```lean
import Loom

open Loom

def homeAction : Action := fun ctx => do
  Action.html "<h1>Welcome to Loom!</h1>"

def createUserAction : Action := fun ctx => do
  let name := ctx.param "name" |>.getD "Anonymous"
  -- Create user...
  let ctx := ctx.withFlash (·.success s!"User {name} created!")
  Action.redirect "/users"

def main : IO Unit := do
  let app := App.withSecret "my-secret-key-change-in-production"
    |>.get "/" "home" homeAction
    |>.get "/users" "users_index" usersAction
    |>.post "/users" "users_create" createUserAction
    |>.use Middleware.logging
    |>.use Middleware.securityHeaders

  app.run "0.0.0.0" 3000
```

## Core Concepts

### Application

The `App` type is the main container for your application:

```lean
let app := App.withSecret "secret-key"
  |>.get "/path" "route_name" action
  |>.post "/path" "route_name" action
  |>.use middleware
```

### Actions

Actions handle requests and return responses:

```lean
abbrev Action := Context -> IO Herald.Core.Response
```

Helper functions for common responses:
- `Action.text` - Plain text response
- `Action.html` - HTML response
- `Action.json` - JSON response
- `Action.redirect` - Redirect response
- `Action.notFound` - 404 response

### Context

The `Context` provides access to request data:

```lean
ctx.param "name"           -- Get a parameter
ctx.header "Content-Type"  -- Get a header
ctx.session.get "user_id"  -- Get session data
ctx.flash.get "error"      -- Get flash message
ctx.csrfToken              -- CSRF token for forms
```

### Sessions

Sessions are stored in signed cookies:

```lean
-- Read session
let userId := ctx.session.get "user_id"

-- Write session
let ctx := ctx.withSession (·.set "user_id" "42")
```

### Flash Messages

Flash messages persist for one request:

```lean
-- Set flash
let ctx := ctx.withFlash (·.success "Operation completed!")

-- Read flash (in next request)
let msg := ctx.flash.get "success"
```

### Named Routes

Generate URLs for named routes:

```lean
let routes := Routes.empty
  |>.get "/users/:id" "user_show" action

routes.pathFor "user_show" [("id", "42")]
-- => Some "/users/42"
```

### CSRF Protection

CSRF tokens are automatically generated. Include in forms:

```lean
open Scribe in
form [action_ "/submit", method_ "post"] do
  csrfField ctx.csrfToken
  input [type_ "text", name_ "name"]
  button "Submit"
```

### Middleware

Built-in middleware:

- `Middleware.logging` - Request logging
- `Middleware.securityHeaders` - Security headers
- `Middleware.cors` - CORS support
- `Middleware.basicAuth` - Basic authentication
- `Middleware.errorRecovery` - Error handling

### Static Files

Configure static file serving:

```lean
let config : AppConfig := {
  secretKey := "...".toUTF8
  staticPath := some "public"  -- Serve files from ./public
}
```

## Project Structure

```
loom/
├── Loom.lean           # Root module
├── Loom/
│   ├── App.lean        # Application container
│   ├── Controller.lean # Context and action types
│   ├── Cookie.lean     # Cookie parsing/building
│   ├── Flash.lean      # Flash messages
│   ├── Form.lean       # Form parsing, CSRF
│   ├── Middleware.lean # Built-in middleware
│   ├── Router.lean     # Named routes
│   ├── Session.lean    # Cookie sessions
│   └── Static.lean     # Static file serving
└── Tests/
    └── Main.lean       # Test suite
```

## Running Tests

```bash
lake test
```

## Dependencies

- **citadel**: HTTP server
- **scribe**: HTML builder
- **herald**: HTTP types
- **crucible**: Test framework

## License

MIT License
